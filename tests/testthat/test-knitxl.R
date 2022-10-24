expect_snapshot_xl <- function(input_text, name) {
  path <- paste0(tempdir(), "/", name)
  print(path)
  x <- knitxl(text = input_text,
         output = path)
  print(x)
  expect_snapshot_file(path, name)
}

# test_that("knitxl parses text", {
#   # expect_snapshot_xl("Some text", "test1.xlsx")
#   # expect_snapshot_xl("Some text `r 1+1`", "test2.xlsx")
#   # expect_snapshot_xl("Some text `r 1+1`. more text", "test3.xlsx")
# })


save_xl <- function(input_text) {
  path <- tempfile(fileext = ".xlsx")
  x <- knitxl(text = input_text, output = path)
  path
}

expect_snapshot_xl <- function(name, input_text) {
  name <- paste0(name, ".xlsx")

  announce_snapshot_file(name = name)

  path <- save_xl(input_text)
  expect_snapshot_file(path, name, compare = compare_file_text)
}


test_that("test2", {
  expect_snapshot_xl("file1", "Some text")
})


