
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

test_that("parses source code", {
  ctx <- set_local_context()

  expect_snapshot_xl("source_code", build_chunk(c(
    "with(mtcars, {",
    "  plot(mpg~hp, pch=20, col='darkgray')",
    "  lines(lowess(hp, mpg))",
    "})"
  )))

  unset_local_context(ctx)
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
  ctx <- set_local_context()

  content <- build_chunk(
    "plot(1:3, 1:3)",
    "plot(1:3, 1:3); setNames(letters, LETTERS)",
    "plot(1:3, 1:3)", "setNames(letters, LETTERS)"
  )
  expect_snapshot_xl("plot", content)

  unset_local_context(ctx)
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

test_that("numbered lists work", {
  expect_snapshot_xl("numbered_list", paste(
    "Numbered list 1:",
    "1. simple item",
    "2. **bold** item",
    "",
    "Numbered list 2:",
    "a. simple item",
    "b. **bold** and *italic*",
    sep = "\n"
  ))
})


test_that("hyperlinks work", {
  expect_snapshot_xl("hyperlinks", paste(
  "A string with an [hyperlink](https://en.wikipedia.org)",
  "A string with an [hyperlink](https://en.wikipedia.org) and another [hyperlink](https://fr.wikipedia.org)",
  "**A bold string with an [hyperlink](https://en.wikipedia.org)**",
  "* A list with an [hyperlink](https://en.wikipedia.org)",
  "normal text **bold text** [hyperlink](https://yihui.org/knitr/)",
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
#
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

test_that("insert images works", {
  path_to_img <- system.file("extdata", "einstein.jpg", package = "openxlsx")
  expect_snapshot_xl("image", paste(
    paste0("Some text with an image ![alt text](", path_to_img, ") followed by more text"),
    "",
    paste0("Text with image ![img1](", path_to_img, ") followed by image ![img2](", path_to_img, ")"),
    sep = "\n"
  ))
})

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

test_that("blockquotes work", {
  expect_snapshot_xl("blockquote", paste(
    "Some normal text",
    "> A blockquote",
    "Some other normal text",
    "",
    "> blockquote 1",
    "> blockquote 2",
    sep = "\n"
  ))
})

test_that("renders r source code", {
  expect_snapshot_xl("r_source", paste(
    "Some text",
    "",
    build_chunk(c(
      "x <- 1 + 1",
      "y <- 2 * x"
    )),
    sep = "\n"
  ))
})

test_that("knit minimum example works", {
  ctx <- set_local_context()

  content <- readr::read_file(system.file("examples", "knitxl-minimal.Rmd", package = "knitxl"))
  expect_snapshot_xl("minimum_example", content)

  unset_local_context(ctx)
})

test_that("knit minimum example works from file", {
  ctx <- set_local_context()

  input_fn <- system.file("examples", "knitxl-minimal.Rmd", package = "knitxl")
  output_fn <- "minimum_example_from_file.xlsx"
  testthat::announce_snapshot_file(name = output_fn)
  path <- tempfile(fileext = ".xlsx")
  x <- suppressMessages(knitxl(input_fn, output = path, quiet = TRUE))
  testthat::expect_snapshot_file(path, output_fn, compare = compare_file_xl)

  unset_local_context(ctx)
})


test_that("knit minimum example works from file with header", {
  ctx <- set_local_context()

  input_fn <- system.file("examples", "knitxl-header.Rmd", package = "knitxl")
  output_fn <- "minimum_example_from_file_header.xlsx"
  testthat::announce_snapshot_file(name = output_fn)
  path <- tempfile(fileext = ".xlsx")
  x <- suppressMessages(knitxl(input_fn, output = path, quiet = TRUE))
  testthat::expect_snapshot_file(path, output_fn, compare = compare_file_xl)

  unset_local_context(ctx)
})


test_that("printing output works", {
  ctx <- set_local_context()

  expect_snapshot_xl("output", paste0(
    "# Printing outputs: \n",
    "\n",
    build_chunk(c(
      "x <- 1 + 1",
      "print(x)",
      "fit <- lm(mpg ~ cyl, data = mtcars)",
      "summary(fit)"
    ))
  ))

  unset_local_context(ctx)
})

test_that("xl_renderer methods can be implemented with print output", {

   xl_renderer.lm <- function(x, options) {
     res <- summary(x)
     glue::glue(
       "Goodness of fit: R^2 = {res$r.squared}", "\n",
       "Estimated coefficients are:", "\n",
       res$coefficients %>%
         as.data.frame %>%
         glue::glue_data("* {Estimate} for variable {rownames(.)}") %>%
         paste(collapse = "\n")
     )
   }

   registerS3method("xl_renderer", "lm", xl_renderer.lm)

   expect_snapshot_xl("xl_renderer_print_output", paste(
     "Results of a linear fit:",
     "",
     build_chunk("lm(mpg ~ cyl, data = mtcars)"),
     sep = "\n"
   ))
})

test_that("xl_renderer methods can be implemented with data.frame output", {

   xl_renderer.lm <- function(x, options) {
     summary(x)$coefficients %>%
       as.data.frame() %>%
       new_knitxl_output_data_frame()
   }

   registerS3method("xl_renderer", "lm", xl_renderer.lm)

   expect_snapshot_xl("xl_renderer_data_frame", paste(
     "Coefficients of a linear fit:",
     "",
     build_chunk("lm(mpg ~ cyl, data = mtcars)"),
     sep = "\n"
   ))
})

