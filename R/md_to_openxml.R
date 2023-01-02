
postprocess_shared_strings <- function(shared_sst) {
  purrr::modify(shared_sst, postprocess_shared_string_item)
}

postprocess_shared_string_item <- function(oxml_string_item) {
  if (stringr::str_detect(oxml_string_item, get_md_string_flag())) {
    oxml_string_item %>%
      stringr::str_remove(get_md_string_flag()) %>%
      md2oxml
  }
  else
    oxml_string_item
}

md2oxml <- function(oxml_string_item) {
  xml_item <- xml2::read_xml(oxml_string_item)

  stopifnot("Shared string item must be an xml with root element <si> and only one child <t> which contains all the text" =
              xml_item %>% xml2::xml_name() == "si" &
              xml_item %>% xml2::xml_children() %>% length() == 1 &
              xml_item %>% xml2::xml_child() %>% xml2::xml_name() == "t" &
              xml_item %>% xml2::xml_child() %>% xml2::xml_children() %>% length() == 0 &
              xml_item %>% xml2::xml_text() %>% length() == 1)

  md_text <- xml_item %>% xml2::xml_text()
  html_text <- commonmark::markdown_html(md_text, extensions = "strikethrough")

  if (is_simple_string(html_text))
    return(oxml_string_item)

  new_oxml <- xml2::xml_new_root("si")
  html_text_nodes <- html_text %>% xml2::read_xml() %>% xml2::xml_find_all("//text()")

  oxml_text_nodes <- purrr::map(html_text_nodes, html2oxml_for_text_nodes)
  purrr::walk(oxml_text_nodes, function(new_node) {
    new_oxml %>% xml2::xml_add_child(new_node)
  })

  new_oxml %>%
    as.character() %>%
    stringr::str_remove(stringr::coll("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n")) %>%
    stringr::str_replace_all(">[:space:]*<", "><")
}

is_simple_string <- function(html_text) {
  html_text %>% xml2::read_xml() %>% xml2::xml_children() %>% length() == 0
}

html2oxml_for_text_nodes <- function(html_text_node) {
  parents <- html_text_node %>% xml2::xml_parents() %>% xml2::xml_name()
  is_italic <- "em" %in% parents
  is_bold <- "strong" %in% parents
  is_strike <- "del" %in% parents
  is_hlink <- "a" %in% parents

  text <- html_text_node %>% xml2::xml_text()

  fontcolor <- NULL
  if (is_hlink)
    fontcolor <- "FF2A61BB"

  build_oxml_text_node(text, italic = is_italic, bold = is_bold,
                       strike = is_strike, underline = is_hlink,
                       color = fontcolor)
}

build_oxml_text_node <- function(text, italic, bold, strike, underline, color) {
  if (any(italic, bold, strike, underline, !is.null(color)))
    build_complex_oxml_text_node(text, italic, bold, strike, underline, color)
  else
    build_simple_oxml_text_node(text)
}

build_simple_oxml_text_node <- function(text) {
  glue::glue(
    "<r><t xml:space=\"preserve\">{text}</t></r>"
  ) %>%
    xml2::read_xml()
}

build_complex_oxml_text_node <- function(text, italic, bold, strike, underline,
                                         color) {
  mod_tags <- if (italic) "<i/>" else ""
  mod_tags <- paste0(mod_tags, if (bold) "<b/>" else "")
  mod_tags <- paste0(mod_tags, if (strike) "<strike/>" else "")
  mod_tags <- paste0(mod_tags, if (underline) "<u/>" else "")

  if (is.null(color)) {
    color_attr <- "theme=\"1\""
  } else {
    color_attr <- paste0("rgb=\"", color, "\"")
  }

  glue::glue(
    "<r>",
      "<rPr>",
        "{mod_tags}",
        "<sz val=\"12\"/>",
        "<color {color_attr}/>",
        "<rFont val=\"Calibri\"/>",
        "<family val=\"2\"/>",
        "<scheme val=\"minor\"/>",
      "</rPr>",
      "<t xml:space=\"preserve\">{text}</t>",
    "</r>"
  ) %>%
    xml2::read_xml()
}
