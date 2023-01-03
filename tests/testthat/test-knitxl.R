
test_that("knitxl parses text and inline code", {
  content <- paste("Some text",
                   "Some text `r 1+1`",
                   "Some text `r 1+1` more text",
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
  expect_snapshot_xl("headers", glue::glue(
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

test_that("parses markdown inline formatting", {
  expect_snapshot_xl("inline_md", paste(
    "A string with *italic* word",
    "A string with **bold** word",
    "A string with ***bold and italic***",
    "A string with *italic and **bold and italic** inside*",
    "A string with **bold and *bold and italic* inside**",
    "A string with ~~strikethrough~~",
    sep = "\n"
  ))
})

test_that("bullet list works", {
  expect_snapshot_xl("bullet_list", paste(
    "Bullet list 1:",
    "* Simple item",
    "* **Bold item**",
    "",
    "Bullet list 2",
    "- Simple item",
    "- **bold** and *italic*",
    sep = "\n"
  ))
})


test_that("hyperlinks work", {
  expect_snapshot_xl("hyperlinks", paste(
  "A string with an [hyperlink](https://en.wikipedia.org)",
  "A string with an [hyperlink](https://en.wikipedia.org) and another [hyperlink](https://fr.wikipedia.org)",
  "**A bold string with an [hyperlink](https://en.wikipedia.org)**",
  "* A list with an [hyperlink](https://en.wikipedia.org)",
  sep = "\n"
  ))
})

test_that("adding new worksheet works", {
  expect_snapshot_xl("new_ws", paste(
    "# This is Worksheet 1 {ws_name=myws1}",
    "Text in Worksheet 1",
    "## This is Worksheet 2 {ws_name=myws2}",
    build_chunk("mtcars"),
    "### This is still in Worksheet2",
    "Text in Worksheet 2",
    "## This is Worksheet 3 {ws_name=myws3}",
    "Text is Worksheet 3",
    sep = "\n"
  ))
})

test_that("horizontal rules work", {
  expect_snapshot_xl("hrules", paste(
    "# Testing Hrules",
    "",
    "Text before hrule",
    "******",
    "Text between hrules",
    "-----",
    "Text after hrules",
    sep = "\n"
  ))
})

test_that("insert images works", {})

test_that("inline R code works", {
  expect_snapshot_xl("inline_code", paste(
    "# Testing inline code",
    "A string with `code` inside.",
    "Another string with `more code` and `more and more code`.",
    sep = "\n"
  ))
})

test_that("plain code blocks works", {
  expect_snapshot_xl("code_block", paste(
    "# Testing plain code blocks",
    "Some text",
    "```",
    "code example",
    "```",
    "Some text",
    "```",
    "first line of code",
    "second line of code",
    "# A line with a comment",
    "```",
    "more text",
    sep = "\n"
  ))
})
