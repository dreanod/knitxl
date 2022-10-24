

source_hook <- function(x, options) {
  x
}

output_hook <- function(x, options) {
  x
}

warning_hook <- function(x, options) {
  x
}

error_hook <- function(x, options) x



plot_hook <- function(x, options) x



inline_hook <- function(x) {
  x
}






text_hook <- function(x) {
  xl_obj$insert_text(x)
  invisible(x)
}

document_hook <- function(x) x


chunk_hook <- function(x, options) x
