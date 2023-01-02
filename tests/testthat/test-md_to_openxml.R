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
                           "<family val=\"2\"/>",
                           "<scheme val=\"minor\"/>",
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
                           "<family val=\"2\"/>",
                           "<scheme val=\"minor\"/>",
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
                           "<family val=\"2\"/>",
                           "<scheme val=\"minor\"/>",
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
                           "<family val=\"2\"/>",
                           "<scheme val=\"minor\"/>",
                         "</rPr>",
                         "<t xml:space=\"preserve\">italic and </t>",
                       "</r>",
                       "<r>",
                         "<rPr>",
                           "<i/><b/>",
                           "<sz val=\"12\"/>",
                           "<color theme=\"1\"/>",
                           "<rFont val=\"Calibri\"/>",
                           "<family val=\"2\"/>",
                           "<scheme val=\"minor\"/>",
                         "</rPr>",
                         "<t xml:space=\"preserve\">bold and italic</t>",
                       "</r>",
                       "<r>",
                         "<rPr>",
                           "<i/>",
                           "<sz val=\"12\"/>",
                           "<color theme=\"1\"/>",
                           "<rFont val=\"Calibri\"/>",
                           "<family val=\"2\"/>",
                           "<scheme val=\"minor\"/>",
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
                           "<family val=\"2\"/>",
                           "<scheme val=\"minor\"/>",
                         "</rPr>",
                         "<t xml:space=\"preserve\">bold and </t>",
                       "</r>",
                       "<r>",
                         "<rPr>",
                           "<i/><b/>",
                           "<sz val=\"12\"/>",
                           "<color theme=\"1\"/>",
                           "<rFont val=\"Calibri\"/>",
                           "<family val=\"2\"/>",
                           "<scheme val=\"minor\"/>",
                         "</rPr>",
                         "<t xml:space=\"preserve\">bold and italic</t>",
                       "</r>",
                       "<r>",
                         "<rPr>",
                           "<b/>",
                           "<sz val=\"12\"/>",
                           "<color theme=\"1\"/>",
                           "<rFont val=\"Calibri\"/>",
                           "<family val=\"2\"/>",
                           "<scheme val=\"minor\"/>",
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
                           "<family val=\"2\"/>",
                           "<scheme val=\"minor\"/>",
                         "</rPr>",
                         "<t xml:space=\"preserve\">strikethrough</t>",
                       "</r>",
                     "</si>")
  expect_equal(md %>% t2xml %>% md2oxml %>% ppxml, oxml %>% ppxml)
})
