
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

read_xml_lit <- function(text) {
  if (stringr::str_length(text) > 0) {
    xml2::read_xml(text)
  } else {
    xml2::xml_new_root("p")
  }
}

# Copied from https://github.com/rstudio/rmarkdown
parse_yaml_front_matter <- function(input_lines) {

  partitions <- partition_yaml_front_matter(input_lines)
  if (!is.null(partitions$front_matter)) {
    front_matter <- partitions$front_matter
    if (length(front_matter) > 2) {
      front_matter <- front_matter[2:(length(front_matter) - 1)]
      front_matter <- one_string(front_matter)
      validate_front_matter(front_matter)
      parsed_yaml <- yaml::yaml.load(front_matter, eval.expr = TRUE)
      if (is.list(parsed_yaml))
        parsed_yaml
      else
        list()
    }
    else
      list()
  }
  else
    list()
}

# Copied from https://github.com/rstudio/rmarkdown
validate_front_matter <- function(front_matter) {
  front_matter <- trim_trailing_ws(front_matter)
  if (grepl(":$", front_matter))
    stop("Invalid YAML front matter (ends with ':')")
}


# Copied from https://github.com/rstudio/rmarkdown
partition_yaml_front_matter <- function(input_lines) {

  validate_front_matter <- function(delimiters) {
    if (length(delimiters) >= 2 &&
        (delimiters[2] - delimiters[1] > 1) &&
        grepl("^---\\s*$", input_lines[delimiters[1]])) {
      # verify that it's truly front matter, not preceded by
      # other content except blank lines or special comments
      # in html_notebook's intermediate .knit.md
      if (delimiters[1] == 1) {
        TRUE
      } else all(grepl(
        "^\\s*(<!-- rnb-\\w*-(begin|end) -->)?\\s*$",
        input_lines[1:delimiters[1] - 1]
      ))
    } else {
      FALSE
    }
  }

  # is there yaml front matter?
  delimiters <- grep("^(---|\\.\\.\\.)\\s*$", input_lines)
  if (validate_front_matter(delimiters)) {

    front_matter <- input_lines[(delimiters[1]):(delimiters[2])]

    input_body <- c()

    if (delimiters[1] > 1)
      input_body <- c(input_body,
                      input_lines[1:delimiters[1] - 1])

    if (delimiters[2] < length(input_lines))
      input_body <- c(input_body,
                      input_lines[-(1:delimiters[2])])

    list(front_matter = front_matter,
         body = input_body)
  }
  else {
    list(front_matter = NULL,
         body = input_lines)
  }
}



# Copied from https://github.com/rstudio/rmarkdown
trim_trailing_ws <- function(x) {
  sub("\\s+$", "", x)
}

one_string = function(x, ...) paste(x, ..., collapse = '\n')
