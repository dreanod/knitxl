save_xl <- function(input_text) {
  path <- tempfile(fileext = ".xlsx")
  x <- knitxl(text = input_text, output = path)
  path
}

expect_snapshot_xl <- function(name, input_text) {
  name <- paste0(name, ".xlsx")

  announce_snapshot_file(name = name)

  path <- save_xl(input_text)
  expect_snapshot_file(path, name, compare = compare_file_xl)
}

compare_file_xl <- function(old, new) {
  old <- openxlsx::read.xlsx(old)
  new <- openxlsx::read.xlsx(new)
  comp <- waldo::compare(old, new)
  length(comp) == 0
}

test_that("knitxl parses text", {
  expect_snapshot_xl("file1", "Some text")
  expect_snapshot_xl("file2", "Some text `r 1+1`")
  expect_snapshot_xl("file3", "Some text `r 1+1` more text")
})

