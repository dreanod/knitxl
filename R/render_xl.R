
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
#' @export
xl_renderer <- function(x, options) {
  UseMethod("xl_renderer")
}

#' @export
xl_renderer.default <- function(x, options) {
  print_output <- paste0(utils::capture.output(
    print(x)
  ), collapse = "\n")
  new_knitxl_output_text(print_output)
}

new_knitxl_output_text <- function(text) {
  class(x) <- c("knitxl_output_text", class(x))
  x
}

#' @export
xl_renderer.data.frame <- function(x, options) {
  new_knitxl_output_data_frame(x)
}

new_knitxl_output_data_frame <- function(df) {
  class(df) <- c("knitxl_output_data_frame", class(df))
  df
}

#' @export
xl_renderer.numeric <- function(x, options) {
  xl_renderer_vector(x)
}

#' @export
xl_renderer.logical <- function(x, options) {
  xl_renderer_vector(x)
}

#' @export
xl_renderer.list <- function(x, options) {
  xl_renderer_vector(x)
}

#' @export
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

