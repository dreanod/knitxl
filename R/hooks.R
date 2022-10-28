source_hook <- function(x, options) {
  style <- openxlsx::createStyle(fontName = "Courier New", bgFill = "grey80")
  insert_text(x, style)
  invisible(x)
}

output_hook <- function(x, options) {
  invisible(x)
}

error_hook <- function(x, options) {
  style <- openxlsx::createStyle(fontColour = "red", textDecoration = "bold")
  insert_text(x, style)
  invisible(x)
}

warning_hook <- function(x, options) {
  style <- openxlsx::createStyle(fontColour = "orange", textDecoration = "bold")
  insert_text(x, style)
  invisible(x)
}

message_hook <- function(x, options) {
  style <- openxlsx::createStyle(fontColour = "blue", textDecoration = "italic")
  insert_text(x, style)
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
  insert_text(x)
  invisible(x)
}

document_hook <- function(x) {
  invisible(x)
}

chunk_hook <- function(x, options) {
  invisible(x)
}
