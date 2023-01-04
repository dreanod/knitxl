test_that("md to openxml works", {

  t2xml <- function(s) {
    paste0("<si><t xml:space=\"preserve\">", s, "</t></si>")
  }

  ppxml <- function(s) {
    s %>% xml2::read_xml() %>% as.character() %>% stringr::str_remove("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n")
  }

  md <- "A simple string"
  oxml <- glue::glue("<si>",
                         "<t xml:space=\"preserve\">A simple string</t>",
                     "</si>")
  expect_equal(md %>% t2xml %>% md2oxml %>% ppxml, oxml %>% ppxml)

  md <- "A string with *italic* word"
  oxml <- glue::glue("<si>",
                       "<r>",
                         "<t xml:space=\"preserve\">A string with </t>",
                       "</r>",
                       "<r>",
                         "<rPr>",
                           "<i/>",
                           "<sz val=\"12\"/>",
                           "<color theme=\"1\"/>",
                           "<rFont val=\"Calibri\"/>",
                         "</rPr>",
                         "<t xml:space=\"preserve\">italic</t>",
                       "</r>",
                       "<r>",
                         "<t xml:space=\"preserve\"> word</t>",
                       "</r>",
                     "</si>")
  expect_equal(md %>% t2xml %>% md2oxml %>% ppxml, oxml %>% ppxml)

  md <- "A string with **bold** word"
  oxml <- glue::glue("<si>",
                       "<r>",
                         "<t xml:space=\"preserve\">A string with </t>",
                       "</r>",
                       "<r>",
                         "<rPr>",
                           "<b/>",
                           "<sz val=\"12\"/>",
                           "<color theme=\"1\"/>",
                           "<rFont val=\"Calibri\"/>",
                         "</rPr>",
                         "<t xml:space=\"preserve\">bold</t>",
                       "</r>",
                       "<r>",
                         "<t xml:space=\"preserve\"> word</t>",
                       "</r>",
                     "</si>")
  expect_equal(md %>% t2xml %>% md2oxml %>% ppxml, oxml %>% ppxml)

  md <- "A string with ***bold and italic***"
  oxml <- glue::glue("<si>",
                       "<r>",
                         "<t xml:space=\"preserve\">A string with </t>",
                       "</r>",
                       "<r>",
                         "<rPr>",
                           "<i/><b/>",
                           "<sz val=\"12\"/>",
                           "<color theme=\"1\"/>",
                           "<rFont val=\"Calibri\"/>",
                         "</rPr>",
                         "<t xml:space=\"preserve\">bold and italic</t>",
                       "</r>",
                     "</si>")
  expect_equal(md %>% t2xml %>% md2oxml %>% ppxml, oxml %>% ppxml)

  md <- "A string with *italic and **bold and italic** inside*"
  oxml <- glue::glue("<si>",
                       "<r>",
                         "<t xml:space=\"preserve\">A string with </t>",
                       "</r>",
                       "<r>",
                         "<rPr>",
                           "<i/>",
                           "<sz val=\"12\"/>",
                           "<color theme=\"1\"/>",
                           "<rFont val=\"Calibri\"/>",
                         "</rPr>",
                         "<t xml:space=\"preserve\">italic and </t>",
                       "</r>",
                       "<r>",
                         "<rPr>",
                           "<i/><b/>",
                           "<sz val=\"12\"/>",
                           "<color theme=\"1\"/>",
                           "<rFont val=\"Calibri\"/>",
                         "</rPr>",
                         "<t xml:space=\"preserve\">bold and italic</t>",
                       "</r>",
                       "<r>",
                         "<rPr>",
                           "<i/>",
                           "<sz val=\"12\"/>",
                           "<color theme=\"1\"/>",
                           "<rFont val=\"Calibri\"/>",
                         "</rPr>",
                         "<t xml:space=\"preserve\"> inside</t>",
                       "</r>",
                     "</si>")
  expect_equal(md %>% t2xml %>% md2oxml %>% ppxml, oxml %>% ppxml)

  md <- "A string with **bold and *bold and italic* inside**"
  oxml <- glue::glue("<si>",
                       "<r>",
                         "<t xml:space=\"preserve\">A string with </t>",
                       "</r>",
                       "<r>",
                         "<rPr>",
                           "<b/>",
                           "<sz val=\"12\"/>",
                           "<color theme=\"1\"/>",
                           "<rFont val=\"Calibri\"/>",
                         "</rPr>",
                         "<t xml:space=\"preserve\">bold and </t>",
                       "</r>",
                       "<r>",
                         "<rPr>",
                           "<i/><b/>",
                           "<sz val=\"12\"/>",
                           "<color theme=\"1\"/>",
                           "<rFont val=\"Calibri\"/>",
                         "</rPr>",
                         "<t xml:space=\"preserve\">bold and italic</t>",
                       "</r>",
                       "<r>",
                         "<rPr>",
                           "<b/>",
                           "<sz val=\"12\"/>",
                           "<color theme=\"1\"/>",
                           "<rFont val=\"Calibri\"/>",
                         "</rPr>",
                         "<t xml:space=\"preserve\"> inside</t>",
                       "</r>",
                     "</si>")
  expect_equal(md %>% t2xml %>% md2oxml %>% ppxml, oxml %>% ppxml)

  md <- "A string with ~~strikethrough~~"
  oxml <- glue::glue("<si>",
                       "<r>",
                         "<t xml:space=\"preserve\">A string with </t>",
                       "</r>",
                       "<r>",
                         "<rPr>",
                           "<strike/>",
                           "<sz val=\"12\"/>",
                           "<color theme=\"1\"/>",
                           "<rFont val=\"Calibri\"/>",
                         "</rPr>",
                         "<t xml:space=\"preserve\">strikethrough</t>",
                       "</r>",
                     "</si>")
  expect_equal(md %>% t2xml %>% md2oxml %>% ppxml, oxml %>% ppxml)

  md <- "A string with an [hyperlink](url)"
  oxml <- glue::glue("<si>",
                       "<r>",
                         "<t xml:space=\"preserve\">A string with an </t>",
                       "</r>",
                       "<r>",
                         "<rPr>",
                           "<u/>",
                           "<sz val=\"12\"/>",
                           "<color rgb =\"FF2A61BB\"/>",
                           "<rFont val=\"Calibri\"/>",
                         "</rPr>",
                         "<t xml:space=\"preserve\">hyperlink</t>",
                       "</r>",
                     "</si>")
  expect_equal(md %>% t2xml %>% md2oxml %>% ppxml, oxml %>% ppxml)

  md <- "A string with `code`"
  oxml <- glue::glue("<si>",
                       "<r>",
                         "<t xml:space=\"preserve\">A string with </t>",
                       "</r>",
                       "<r>",
                         "<rPr>",
                           "<b/>",
                           "<sz val=\"12\"/>",
                           "<color rgb =\"FF395392\"/>",
                           "<rFont val=\"Courier New\"/>",
                         "</rPr>",
                         "<t xml:space=\"preserve\">code</t>",
                       "</r>",
                     "</si>")
  expect_equal(md %>% t2xml %>% md2oxml %>% ppxml, oxml %>% ppxml)



})

test_that("extract ws name from header line", {
  expect_equal(extract_ws_name_option("# Header"), NULL)
  expect_equal(extract_ws_name_option("# Header {ws_name=myws}"), "myws")
  expect_equal(extract_ws_name_option("## Header {ws_name=myws}"), "myws")
  expect_equal(extract_ws_name_option("## Header {other_opt=myws}"), NULL)
  expect_equal(extract_ws_name_option("## Header {ws_name=myws, other_opt=opt1}"), "myws")
  expect_equal(extract_ws_name_option("## Header {other_opt=opt1, ws_name=myws}"), "myws")
  expect_equal(extract_ws_name_option("## Header {other_opt1=opt1, ws_name=myws, other_opt2=opt2}"), "myws")
  expect_equal(extract_ws_name_option("## Header {ws_name=myws1, ws_name=myws2}"), "myws1")

  expect_equal(remove_option_string("# Header"), "# Header")
  expect_equal(remove_option_string("# Header {ws_name=myws}"), "# Header")
  expect_equal(remove_option_string("## Header {ws_name=myws}"), "## Header")
  expect_equal(remove_option_string("## Header {other_opt=myws}"), "## Header")
  expect_equal(remove_option_string("## Header {ws_name=myws, other_opt=opt1}"), "## Header")
  expect_equal(remove_option_string("## Header {other_opt=opt1, ws_name=myws}"), "## Header")
  expect_equal(remove_option_string("## Header {other_opt1=opt1, ws_name=myws, other_opt2=opt2}"), "## Header")
  expect_equal(remove_option_string("## Header {ws_name=myws1, ws_name=myws2}"), "## Header")
})

test_that("detect hrules works", {
  expect_equal(detect_hrule("******"), TRUE)
  expect_equal(detect_hrule("  ******"), FALSE)
  expect_equal(detect_hrule("*****  "), TRUE)
  expect_equal(detect_hrule("***"), TRUE)
  expect_equal(detect_hrule("**"), FALSE)
  expect_equal(detect_hrule("*** Bla"), FALSE)
  expect_equal(detect_hrule("bla***"), FALSE)
  expect_equal(detect_hrule("***"), TRUE)

  expect_equal(detect_hrule("------"), TRUE)
  expect_equal(detect_hrule("  ------"), FALSE)
  expect_equal(detect_hrule("-----  "), TRUE)
  expect_equal(detect_hrule("---"), TRUE)
  expect_equal(detect_hrule("--"), FALSE)
  expect_equal(detect_hrule("--- Bla"), FALSE)
  expect_equal(detect_hrule("bla---"), FALSE)
  expect_equal(detect_hrule("---"), TRUE)

  expect_equal(detect_hrule("-*-"), FALSE)
})

test_that("parse images works", {
  expect_equal(detect_images("Some text"), FALSE)
  expect_equal(detect_images("Some text with image ![img](path_to_img)"), TRUE)
  expect_equal(detect_images("Some text with image ![img1](path_to_img1) and other image ![img2](path_to_img2)"), TRUE)

  expect_equal(get_path_to_images("Some text") %>% length(), 0)
  expect_equal(get_path_to_images("Some text with image ![img](path_to_img)"), "path_to_img")
  expect_equal(get_path_to_images("Some text with image ![img1](path_to_img1) and other image ![img2](path_to_img2)"), c("path_to_img1", "path_to_img2"))
})

test_that("parse blockquote", {
  expect_equal(detect_blockquote("Some text"), FALSE)
  expect_equal(detect_blockquote("Some text >"), FALSE)
  expect_equal(detect_blockquote("> Some text"), TRUE)
  expect_equal(detect_blockquote(">Some text"), TRUE)

  expect_equal(remove_blockquote("Some text"), "Some text")
  expect_equal(remove_blockquote("> Some text"), "Some text")
  expect_equal(remove_blockquote(" > Some text"), "Some text")
  expect_equal(remove_blockquote(">Some text"), "Some text")
  expect_equal(remove_blockquote(">>Some text"), ">Some text")
  expect_equal(remove_blockquote("> >Some text"), ">Some text")
  expect_equal(remove_blockquote(">> Some text"), "> Some text")
})
