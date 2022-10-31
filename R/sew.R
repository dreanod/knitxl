
#' @export
sew.knitxl_output_text <- function(x, ...) {
  insert_text(unclass(x), style = attr(x, "style"))
}

#' @export
sew.knitxl_output_vector <- function(x, ...) {
  insert_vector(unclass(x), style = attr(x, "style"))
}

#' @export
sew.knitxl_output_data_frame <- function(x, ...) {
  insert_data_frame(x, style = attr(x, "style"))
}
