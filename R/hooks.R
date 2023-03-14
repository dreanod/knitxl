source_hook <- function(x, options) {
  insert_text(x, type = "text.source")
  invisible(x)
}

output_hook <- function(x, options) {
  insert_text(x, type = "text.output")
  invisible(x)
}

error_hook <- function(x, options) {
  insert_text(x, type = "text.error")
  invisible(x)
}

warning_hook <- function(x, options) {
  insert_text(x, type = "text.warning")
  invisible(x)
}

message_hook <- function(x, options) {
  insert_text(x, type = "text.message")
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
  insert_text(stringr::str_trim(x), type = "text")
  invisible(x)
}

document_hook <- function(x) {
  invisible(x)
}

chunk_hook <- function(x, options) {
  invisible(x)
}
