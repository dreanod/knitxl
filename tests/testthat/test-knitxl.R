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
  old <- openxlsx::loadWorkbook(old)
  new <- openxlsx::loadWorkbook(new)

  old@.xData$core <- ""
  new@.xData$core <- ""

  comp <- waldo::compare(old, new)
  length(comp) == 0
}

build_chunk <- function(code) {
  code <- paste0(code, collapse = "\n")
  paste0(c("```{r}",
           code,
           "```"), collapse = "\n")
}

test_that("knitxl parses text", {
  expect_snapshot_xl("file1", "Some text")
  expect_snapshot_xl("file2", "Some text `r 1+1`")
  expect_snapshot_xl("file3", "Some text `r 1+1` more text")
  expect_snapshot_xl("file4", "* Item 1\n* Item 2\n* Item 3")
})

test_that("errors, warnings and messages rendered correctly", {
  expect_snapshot_xl("error", build_chunk("log('a')"))
  expect_snapshot_xl("warning", build_chunk("log(-1)"))
  expect_snapshot_xl("message", build_chunk("message('foo')"))
})

test_that("renders data.frames", {
  expect_snapshot_xl("data.frame1", build_chunk("mtcars"))
})

test_that("renders vectors", {
  expect_snapshot_xl("vector1", build_chunk("1:4"))
  expect_snapshot_xl("vector2", build_chunk("setNames(1:3, c('a', 'b', 'c'))"))
  expect_snapshot_xl("vector3", build_chunk("setNames(LETTERS[1:3], c('a', 'b', 'c'))"))
})

test_that("renders plots", {
  dev_old <- opts_chunk$get("dev"); opts_chunk$set(dev = "png")
  fig.path_old <- opts_chunk$get("fig.path"); opts_chunk$set(fig.path = tempdir())

  expect_snapshot_xl("plot1", build_chunk("plot(1:3, 1:3)"))

  opts_chunk$set(dev = dev_old)
  opts_chunk$set(fig.path = fig.path_old)
})

