
render_xl <- function(x, ...) {
  res <- xl_renderer(x)
  knitr::knit_print(x, ...)
}

#' @export
xl_renderer <- function(x) {
  UseMethod("xl_renderer")
}

#' @export
xl_renderer.default <- function(x) {
  text <- paste0(capture.output(
    print(x)
  ), collapse = "\n")
  insert_text(text, style = NULL)
}

#' @export
xl_renderer.data.frame <- function(x) {
  insert_data_frame(x)
}

#' @export
xl_renderer.numeric <- function(x) {
  xl_renderer_vector(x)
}

#' @export
xl_renderer.logical <- function(x) {
  xl_renderer_vector(x)
}

#' @export
xl_renderer.list <- function(x) {
  xl_renderer_vector(x)
}

#' @export
xl_renderer.character <- function(x) {
  if (length(x) == 1)
    insert_text(x)
  else
    xl_renderer_vector(x)
}

#' @export
xl_renderer_vector <- function(x) {
  insert_vector(x)
}

