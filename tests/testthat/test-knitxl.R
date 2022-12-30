
test_that("knitxl parses text and inline code", {
  content <- paste("Some text",
                   "Some text `r 1+1`",
                   "Some text `r 1+1` more text",
                   "* Item 1\n* Item 2\n* Item 3",
                   sep = "\n\n")
  expect_snapshot_xl("text", content)
})

test_that("errors, warnings and messages rendered correctly", {
  content <- paste(build_chunk("log('a')"),
                   build_chunk("log(-1)"),
                   build_chunk("message('foo')"),
                   sep = "\n\n")
  expect_snapshot_xl("conditions", content)
})

test_that("renders data.frames", {
  content <- paste(build_chunk("mtcars"),
                   build_chunk(c("mtcars",
                                 "iris")),
                   sep = "\n\n")
  expect_snapshot_xl("data_frame", content)
})

test_that("renders vectors", {
  content <- build_chunk(
    "1:4*10000",
    "setNames(1:3, c('a', 'b', 'c'))",
    "setNames(LETTERS[1:3], c('a', 'b', 'c'))",
    "1:3; 4:6",
    "setNames(1:3, 1:3); setNames(4:6, 4:6)"
  )
  expect_snapshot_xl("vector", content)
})

test_that("renders plots", {
  dev_old <- opts_chunk$get("dev"); opts_chunk$set(dev = "png")
  fig.path_old <- opts_chunk$get("fig.path"); opts_chunk$set(fig.path = tempdir())

  content <- build_chunk(
    "plot(1:3, 1:3)",
    "plot(1:3, 1:3); setNames(letters, LETTERS)",
    "plot(1:3, 1:3)", "setNames(letters, LETTERS)"
  )
  expect_snapshot_xl("plot", content)

  opts_chunk$set(dev = dev_old)
  opts_chunk$set(fig.path = fig.path_old)
})


test_that("parses headers", {
  expect_snapshot_xl("headers1", glue::glue(
    "# Header 1",
    "text",
    "## Header 2",
    "text",
    "### Header 3",
    "text",
    "#### Header 4",
    "text",
    "##### Header 5",
    "text",
    "###### Header 6",
    "text", .sep = "\n"
  ))
})
