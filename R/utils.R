
split_string_by_line <- function(string) {
  string <- stringr::str_split(string, "\n")
  string <- purrr::flatten_chr(string)
  string
}
