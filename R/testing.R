save_xl <- function(input_text) {
  path <- tempfile(fileext = ".xlsx")
  x <- suppressMessages(knitxl(text = input_text, output = path, quiet = TRUE))
  path
}

expect_snapshot_xl <- function(name, input_text) {
  name <- paste0(name, ".xlsx")

  testthat::announce_snapshot_file(name = name)

  path <- save_xl(input_text)
  testthat::expect_snapshot_file(path, name, compare = compare_file_xl)
}

compare_file_xl <- function(old, new) {
  old <- openxlsx::loadWorkbook(old)
  new <- openxlsx::loadWorkbook(new)

  old <- trim_wb_object(old)
  new <- trim_wb_object(new)

  comp <- waldo::compare(old, new)

  if (length(comp) > 0)
    print(comp)
  length(comp) == 0
}

trim_wb_object <- function(wb) {
  wb@.xData$core <- ""
  wb@.xData$media <- list()
  wb
}

build_chunk <- function(...) {
  chunks <- list(...)

  chunks <- purrr::map(chunks, function(code) {
    code <- paste0(code, collapse = "\n")
    paste0(c("```{r}",
             code,
             "```"), collapse = "\n")
  })

  paste0(chunks, collapse = "\n\n")
}

compare_xl_dev <- function() {
  compare_file_xl("dev/Book1.xlsx", "dev/Book2.xlsx")
}

set_local_context <- function() {
  tmp <- paste0(tempdir(), "/")
  dev_old <- opts_chunk$get("dev"); opts_chunk$set(dev = "png")
  fig.path_old <- opts_chunk$get("fig.path"); opts_chunk$set(fig.path = tmp)
  list(dev_old = dev_old,
       fig.path_old = fig.path_old,
       tempdir = tmp)
}

unset_local_context <- function(params) {
  unlink(params$tempdir, recursive = TRUE)
  opts_chunk$set(dev = params$dev_old)
  opts_chunk$set(fig.path = params$fig.path_old)
}

