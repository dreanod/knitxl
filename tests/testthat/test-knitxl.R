
test_that("knitxl parses text", {
  expect_snapshot_xl("text1", "Some text")
  expect_snapshot_xl("text2", "Some text `r 1+1`")
  expect_snapshot_xl("text3", "Some text `r 1+1` more text")
  expect_snapshot_xl("text4", "* Item 1\n* Item 2\n* Item 3")
})

test_that("errors, warnings and messages rendered correctly", {
  expect_snapshot_xl("error", build_chunk("log('a')"))
  expect_snapshot_xl("warning", build_chunk("log(-1)"))
  expect_snapshot_xl("message", build_chunk("message('foo')"))
})

test_that("renders data.frames", {
  expect_snapshot_xl("data_frame1", build_chunk("mtcars"))
  expect_snapshot_xl("data_frame2", build_chunk("mtcars", "iris"))
})

test_that("renders vectors", {
  expect_snapshot_xl("vector1", build_chunk("1:4*10000"))
  expect_snapshot_xl("vector2", build_chunk("setNames(1:3, c('a', 'b', 'c'))"))
  expect_snapshot_xl("vector3", build_chunk("setNames(LETTERS[1:3], c('a', 'b', 'c'))"))
  expect_snapshot_xl("vector4", build_chunk("1:3; 4:6"))
  expect_snapshot_xl("vector5", build_chunk("setNames(1:3, 1:3); setNames(4:6, 4:6)"))
})

test_that("renders plots", {
  dev_old <- opts_chunk$get("dev"); opts_chunk$set(dev = "png")
  fig.path_old <- opts_chunk$get("fig.path"); opts_chunk$set(fig.path = tempdir())

  expect_snapshot_xl("plot1", build_chunk("plot(1:3, 1:3)"))
  expect_snapshot_xl("plot2", build_chunk("plot(1:3, 1:3); setNames(letters, LETTERS)"))
  expect_snapshot_xl("plot3", build_chunk("plot(1:3, 1:3)", "setNames(letters, LETTERS)"))

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
