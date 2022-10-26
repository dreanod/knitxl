
render_xl <- function(x, ...) {
  res <- xl_renderer(x)
  xl_obj$insert_data_frame(res[["table"]], res[["style"]])
  knitr::knit_print(x, ...)
}

#' @export
xl_renderer <- function(x) {
  UseMethod("xl_renderer")
}

#' @export
xl_renderer.default <- function(x) {
  tab <- as.data.frame(x)
  list(table = tab, style = NULL)
}

#' @export
xl_renderer.data.frame <- function(x) {
  list(table = x, style = NULL)
}

# xl_renderer.numeric <- function(x) {
#
# }
