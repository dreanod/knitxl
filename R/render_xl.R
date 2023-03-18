
render_xl <- function(x, options, ...) {
  xl_renderer(x)
}

#' Represents an R object into a format that can be printed into an XLSX file
#'
#' This is a generic function that is intended for developers who want
#' to extend `knitxl` to print new classes R objects. It transforms an object
#' into a `knitxl_output_*` class (either `text`, `vector` or `data_frame`)
#' that can be printed in an XLSX file.
#'
#' @param x the object to be rendered in the XLSX file.
#' @param options the `knitr` and `knitxl` options used to cutomize the
#' rendering of `x`
#'
#' @return A character singleton, a data vector, or a data frame with class
#' `knitxl_output_*` (either `text`, `vector` or `data_frame`, respectively).
#'
#' @examples
#'    # Writes the summary of linear model fits a print output:
#'    xl_renderer.lm <- function(x, options) {
#'      res <- capture.output(summary(x))
#'      res <- paste0(res, collapse = "\n")
#'      class(res) <- "knit_xl_output_vector"
#'      res
#'    }
#'    registerS3method("xl_renderer", "lm", xl_renderer.lm)
#'    # knitxl will now print the summary of `lm` object in the generated
#'    # .xlsx file.
#'
#'    # This will instead write the summary information about the coefficients
#'    # in a table:
#'    xl_renderer.lm <- function(x, options) {
#'      summary(x)$coefficients %>%
#'      as.data.frame() %>%
#'      new_knitxl_output_data_frame()
#'    }
#'   registerS3method("xl_renderer", "lm", xl_renderer.lm)
#'
#' @export
xl_renderer <- function(x, options) {
  UseMethod("xl_renderer")
}

#' @export
#' @rdname xl_renderer
xl_renderer.default <- function(x, options) {
  knitr::knit_print(x)
}

new_knitxl_output_text <- function(text) {
  class(text) <- c("knitxl_output_text", class(text))
  text
}

#' @export
#' @rdname xl_renderer
xl_renderer.data.frame <- function(x, options) {
  new_knitxl_output_data_frame(x)
}

new_knitxl_output_data_frame <- function(df) {
  class(df) <- c("knitxl_output_data_frame", class(df))
  df
}

#' @export
#' @rdname xl_renderer
xl_renderer.numeric <- function(x, options) {
  xl_renderer_vector(x)
}

#' @export
#' @rdname xl_renderer
xl_renderer.logical <- function(x, options) {
  xl_renderer_vector(x)
}

#' @export
#' @rdname xl_renderer
xl_renderer.list <- function(x, options) {
  xl_renderer_vector(x)
}

#' @export
#' @rdname xl_renderer
xl_renderer.character <- function(x, options) {
  if (length(x) == 1)
    new_knitxl_output_text(x)
  else
    xl_renderer_vector(x)
}

xl_renderer_vector <- function(x, options) {
  new_knitxl_output_vector(x)
}

new_knitxl_output_vector <- function(x) {
  class(x) <- c("knitxl_output_vector", class(x))
  x
}

