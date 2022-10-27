source_hook <- function(x, options) {
  x
}

output_hook <- function(x, options) {
  invisible(x)
}

error_hook <- function(x, options) {
  style <- openxlsx::createStyle(fontColour = "red", textDecoration = "bold")
  xl_obj$insert_text(x, style)
  invisible(x)
}

warning_hook <- function(x, options) {
  style <- openxlsx::createStyle(fontColour = "orange", textDecoration = "bold")
  xl_obj$insert_text(x, style)
  invisible(x)
}

message_hook <- function(x, options) {
  style <- openxlsx::createStyle(fontColour = "blue", textDecoration = "italic")
  xl_obj$insert_text(x, style)
  invisible(x)
}

plot_hook <- function(x, options) {
  xl_obj$insert_image(x)
  invisible(x)
}

inline_hook <- function(x) {
  x
}

text_hook <- function(x) {
  xl_obj$insert_text(x)
  invisible(x)
}

document_hook <- function(x) {
  invisible(x)
}

chunk_hook <- function(x, options) x
