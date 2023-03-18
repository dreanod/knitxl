#' Knit a Document to an XLSX file
#'
#' This function takes an input file, extracts the R code in it according to a
#' list of patterns, evaluates the code and writes the output in an XLSX
#' spreadsheet file.
#'
#' @param input Path to the input file.
#' @param output Path to the output file for \code{knitxl()}. If \code{NULL}, this
#'   function will try to guess a default, which will be under the current
#'   working directory.
#' @param quiet Boolean; suppress the progress bar and messages?
#' @param text A character vector. This is an alternative way to provide the
#' input file.
#' @param envir Environment in which code chunks are to be evaluated, for
#'   example, \code{\link{parent.frame}()}, \code{\link{new.env}()}, or
#'   \code{\link{globalenv}()}).
#' @param encoding Encoding of the input file; always assumed to be UTF-8 (i.e.,
#'   this argument is effectively ignored).
#' @return The compiled document is written into the output file, and the path
#'   of the output file is returned invisibly.
#' @export
#'
#' @examples
#'   library(knitxl)
#'   path_to_input <- system.file("examples", "knitxl-minimal.Rmd", package = "knitxl")
#'   path_to_output <- "knitxl-minimal.xlsx"
#'   knitxl(path_to_input, output = path_to_output)  # will generate knitxl-minimal.xlsx
#'   unlink(path_to_output)
#'
#' @importFrom magrittr %<>% %>%
#' @import knitr
knitxl <- function(input,
                   output = NULL,
                   text = NULL,
                   quiet = FALSE,
                   envir = parent.frame(),
                   encoding = "UTF-8") {

  stopifnot(!(missing(input) & is.null(text)))

  if (!missing(input)) {
    stopifnot(tools::file_ext(input) == "Rmd",
              file.exists(input))
    text <- readr::read_file(input)
  }

  text <- stringr::str_replace_all(text, "\r\n", "\n") %>%
    stringr::str_split_1("\n")

  front_matter <- parse_yaml_front_matter(text)
  text <- partition_yaml_front_matter(text)$body %>% paste(collapse = "\n")


  old_hooks <- knitr::knit_hooks$get()
  on.exit(knitr::knit_hooks$restore(old_hooks), add = TRUE)
  set_xl_hooks()

  old_opt_chunk <- knitr::opts_chunk$get()
  on.exit(knitr::opts_chunk$restore(old_opt_chunk), add = TRUE)
  set_opt_chunk()

  xl_obj$reset()
  on.exit(xl_obj$reset(), add = TRUE)

  out <- knitr::knit(input = NULL,
                     output = NULL,
                     text = text,
                     quiet = quiet,
                     envir = envir,
                     encoding = encoding)

  on.exit(unlink("figure/", recursive = TRUE), add = TRUE)

  if (xl_obj$is_empty()) {
    message("No code to execute: knitxl is just transcribing text to output file.")
    if (is.null(text))
      text <- readr::read_file(input)
    insert_text(text, type = "text")
  }

  if (is.null(output)) {
    if (missing(input))
      output <- "output.xlsx"
    else
      output <- out_fn_from_in_fn(input)
  }
  xl_obj$set_fn(output)
  xl_obj$write()

  invisible(output)
}

out_fn_from_in_fn <- function(in_fn) {
  xl_fn <- paste0(tools::file_path_sans_ext(in_fn), ".xlsx")
}

set_xl_hooks <- function() {

  set_xl_hooks_with_options()
  set_xl_hooks_without_option()
}

set_xl_hooks_with_options <- function() {
  hook_list <- c(source = source_hook,
                 output = output_hook,
                 message = message_hook,
                 warning = warning_hook,
                 error = error_hook,
                 plot = plot_hook,
                 chunk = chunk_hook)

  set_hook <- function(xl_hook, hook_name) {
    old_hook <- knitr::knit_hooks$get(hook_name)
    new_hook <- function(x, options) {
      xl_hook(x, options)
      old_hook(x, options)
    }
    args <- stats::setNames(list(new_hook), hook_name)
    do.call(knitr::knit_hooks$set, args)
  }

  purrr::iwalk(hook_list, set_hook)
}

set_xl_hooks_without_option <- function() {
  hook_list <- c(text = text_hook,
                 document = document_hook,
                 inline = inline_hook)

  set_hook <- function(xl_hook, hook_name) {
    old_hook <- knitr::knit_hooks$get(hook_name)
    new_hook <- function(x) {
      xl_hook(x)
      old_hook(x)
    }
    args <- stats::setNames(list(new_hook), hook_name)
    do.call(knitr::knit_hooks$set, args)
  }

  purrr::iwalk(hook_list, set_hook)
}

set_opt_chunk <- function() {
  knitr::opts_chunk$set(render = render_xl,
                        fig.show = "asis")
  kxl_set_default_theme()
}
