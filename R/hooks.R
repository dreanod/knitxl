source_hook <- function(x, options) {
  style <- kxl_style_get(options)
  insert_text(x, style, type = "text.source")
  invisible(x)
}

output_hook <- function(x, options) {
  invisible(x)
}

error_hook <- function(x, options) {
  style <- kxl_style_get(options)
  insert_text(x, style, type = "text.error")
  invisible(x)
}

warning_hook <- function(x, options) {
  style <- kxl_style_get(options)
  insert_text(x, style, type = "text.warning")
  invisible(x)
}

message_hook <- function(x, options) {
  style <- kxl_style_get(options)
  insert_text(x, style, type = "text.message")
  invisible(x)
}

plot_hook <- function(x, options) {
  insert_image(x, width = options$fig.width, height = options$fig.height,
               units = "in", dpi = options$dpi)
  invisible(x)
}

inline_hook <- function(x) {
  invisible(x)
}

text_hook <- function(x) {
  style <- kxl_style_get(knitr::opts_chunk$get())
  insert_text(stringr::str_trim(x), style = style, type = "text")
  invisible(x)
}

document_hook <- function(x) {
  invisible(x)
}

chunk_hook <- function(x, options) {
  invisible(x)
}
