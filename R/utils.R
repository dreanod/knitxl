
split_string_by_line <- function(string) {
  string <- stringr::str_trim(string)
  string <- stringr::str_split(string, "\n")
  string <- purrr::flatten_chr(string)
  string
}

"%||%" <- function(a, b) {
  if (!is.null(a)) a else b
}

convert2inch <- function(x, from) {
  switch (from,
    "in" = x,
    "cm" = 0.3937007874 * x,
    "px" = 0.0104166667 * x
  )
}

get_img_dims <- function(path) {
  img <- readbitmap::read.bitmap(path)
  height <- dim(img)[1]
  width <- dim(img)[2]
  c(height = height, width = width)
}
