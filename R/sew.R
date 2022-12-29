
#' @export
sew.knitxl_output_text <- function(x, options, ...) {
  insert_text(unclass(x))
}

#' @export
sew.knitxl_output_vector <- function(x, options, ...) {
  style <- kxl_style_get(options)
  insert_vector(unclass(x), style = style)
}

#' @export
sew.knitxl_output_data_frame <- function(x, options, ...) {
  style <- kxl_style_get(options)
  insert_data_frame(x, style = style)
}
