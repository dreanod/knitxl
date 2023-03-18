
#' Extend [knitr::sew()] generic to write text, vectors and data table to the
#' output .xlsx file.
#'
#' These functions are called within [knitr::knit()] to write either text
#' vector or a table into the output .xlsx file produced by [knitxl()].
#'
#' @param x	Output from evaluate::evaluate().
#' @param options A list of chunk options used to control output.
#' @param ... Other arguments to pass to methods.
#'
#' @return Invisibly returns `NULL`.
#' @keywords internal
#' @export
sew.knitxl_output_text <- function(x, options, ...) {
  insert_text(unclass(x))
}

#' @export
#' @rdname sew.knitxl_output_text
sew.knitxl_output_vector <- function(x, options, ...) {
  style <- kxl_style_get(options)
  insert_vector(unclass(x), style = style)
}

#' @export
#' @rdname sew.knitxl_output_text
sew.knitxl_output_data_frame <- function(x, options, ...) {
  style <- kxl_style_get(options)
  insert_data_frame(x, style = style)
}
