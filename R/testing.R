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

expect_wb_objects_equal <- function(old, new, old_path = NULL, new_path = NULL) {
  comp <- waldo::compare(old, new)
  test_result <- length(comp) == 0

  if (test_result == TRUE) {
    error_message <- "Workbook objects are identical:\n"
  }
  else {
    error_message <- "Workbook objects are different:\n"
    if (!is.null(old_path) && !is.null(new_path)) {
      paths_message <- sprintf("Old path: %s\nNew path: %s\n", old_path, new_path)
      error_message <- paste(error_message, paths_message, sep = "")
    }
    error_message <- paste(error_message, comp, sep = "\n")
  }

  testthat::expect(test_result, error_message)

  test_result
}

compare_file_xl <- function(old_path, new_path) {
  old <- openxlsx::loadWorkbook(old_path)
  new <- openxlsx::loadWorkbook(new_path)

  old <- trim_wb_object(old)
  new <- trim_wb_object(new)

  expect_wb_objects_equal(old, new, old_path, new_path)
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
  tmp <- file.path(tempdir(), "fig-folder")
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

