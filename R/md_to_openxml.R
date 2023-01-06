
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
  xml_item <- read_xml_lit(oxml_string_item)

  stopifnot("Shared string item must be an xml with root element <si> and only one child <t> which contains all the text" =
              xml_item %>% xml2::xml_name() == "si" &
              xml_item %>% xml2::xml_children() %>% length() == 1 &
              xml_item %>% xml2::xml_child() %>% xml2::xml_name() == "t" &
              xml_item %>% xml2::xml_child() %>% xml2::xml_children() %>% length() == 0 &
              xml_item %>% xml2::xml_text() %>% length() == 1)

  md_text <- xml_item %>% xml2::xml_text()

  list_prefix <- NULL
  if (detect_md_list_item(md_text)) {
    md_text <- protect_md_list_prefix(md_text)
  }

  html_text <- commonmark::markdown_html(md_text, extensions = "strikethrough")

  if (is_simple_string(html_text))
    return(oxml_string_item)

  new_oxml <- xml2::xml_new_root("si")
  html_text_nodes <- html_text %>% read_xml_lit() %>% xml2::xml_find_all("//text()")

  oxml_text_nodes <- purrr::map(html_text_nodes, html2oxml_for_text_nodes)
  purrr::walk(oxml_text_nodes, function(new_node) {
    new_oxml %>% xml2::xml_add_child(new_node)
  })

  new_oxml %>%
    as.character() %>%
    stringr::str_remove(stringr::coll("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n")) %>%
    stringr::str_replace_all(">\n *<", "><")
}

is_simple_string <- function(html_text) {
  html_text %>% read_xml_lit() %>% xml2::xml_children() %>% length() == 0
}

html2oxml_for_text_nodes <- function(html_text_node) {
  parents <- html_text_node %>% xml2::xml_parents() %>% xml2::xml_name()
  is_italic <- "em" %in% parents
  is_bold <- "strong" %in% parents
  is_strike <- "del" %in% parents
  is_hlink <- "a" %in% parents
  is_code <- "code" %in% parents
  is_underline <- FALSE
  font <- NULL
  fontcolor <- NULL

  text <- html_text_node %>% xml2::xml_text()

  if (is_code) {
    font <- "Courier New"
    fontcolor <- "FF395392"
    is_bold <- TRUE
  }

  if (is_hlink) {
    fontcolor <- "FF2A61BB"
    is_underline = TRUE
  }

  build_oxml_text_node(text, italic = is_italic, bold = is_bold,
                       strike = is_strike, underline = is_underline,
                       color = fontcolor, font = font)
}

build_oxml_text_node <- function(text, italic, bold, strike, underline, color, font) {
  if (any(italic, bold, strike, underline, !is.null(color), !is.null(font)))
    build_complex_oxml_text_node(text, italic, bold, strike, underline, color, font)
  else
    build_simple_oxml_text_node(text)
}

build_simple_oxml_text_node <- function(text) {
  glue::glue(
    "<r><t xml:space=\"preserve\">{text}</t></r>"
  ) %>%
    read_xml_lit()
}

build_complex_oxml_text_node <- function(text, italic, bold, strike, underline,
                                         color, font) {
  mod_tags <- if (italic) "<i/>" else ""
  mod_tags <- paste0(mod_tags, if (bold) "<b/>" else "")
  mod_tags <- paste0(mod_tags, if (strike) "<strike/>" else "")
  mod_tags <- paste0(mod_tags, if (underline) "<u/>" else "")

  if (is.null(color)) {
    color_attr <- "theme=\"1\""
  } else {
    color_attr <- paste0("rgb=\"", color, "\"")
  }

  font_attr <- font %||% "Calibri"

  glue::glue(
    "<r>",
      "<rPr>",
        "{mod_tags}",
        "<sz val=\"12\"/>",
        "<color {color_attr}/>",
        "<rFont val=\"{font_attr}\"/>",
      "</rPr>",
      "<t xml:space=\"preserve\">{text}</t>",
    "</r>"
  ) %>%
    read_xml_lit()
}

extract_ws_name_option <- function(header_md_text) {
  all_opts <- stringr::str_extract(header_md_text, "(?<=\\{).*(?=\\} *$)")
  if (is.na(all_opts)) return(NULL)

  opt <- stringr::str_extract(all_opts, "(?<=ws_name=)[[:alnum:]_]*")
  if(is.na(opt)) return(NULL)
  opt
}

remove_option_string <- function(header_md_text) {
  stringr::str_remove(header_md_text, " *\\{.*\\} *$")
}

detect_hrule <- function(md_line) {
  stringr::str_detect(md_line, "^(\\*{3,}|\\-{3,}) *$")
}

detect_code_fence <- function(text) {
  stringr::str_detect(text, "^``` *$")
}

detect_images <- function(text) {
  commonmark::markdown_html(text) %>%
    read_xml_lit() %>%
    xml2::xml_find_all("img") %>%
    length() > 0
}

get_path_to_images <- function(text) {
  commonmark::markdown_html(text) %>%
    read_xml_lit() %>%
    xml2::xml_find_all("img") %>%
    purrr::map_chr(~ xml2::xml_attr(.x, "src"))
}

detect_blockquote <- function(text) {
  stringr::str_detect(text, "^ *>")
}

remove_blockquote <- function(text) {
  stringr::str_remove(text, "^ *> *")
}

detect_md_list_item <- function(text) {
  stringr::str_detect(text, "^ *((\\*|\\-)|[:alnum:]\\.) ")
}

remove_md_list_prefix <- function(text) {
  stringr::str_remove(text, "^ *((\\*|\\-)|[:alnum:]\\.)  *")
}

get_md_list_prefix <- function(text) {
  prefix <- stringr::str_extract(text, "^ *((\\*|\\-)|[:alnum:]\\.) ")

  if (is.na(prefix)) {
    character(0)
  } else {
    prefix %>%
      stringr::str_trim() %>%
      stringr::str_replace("\\*|\\-", "\u2022") %>%
      paste0(" ")
  }
}

protect_md_list_prefix <- function(text) {
  prefix <- get_md_list_prefix(text)
  text <- remove_md_list_prefix(text)
  paste0("<span>", prefix, "</span>", text)
}

cell_has_hyperlink <- function(text) {
  text %>%
    commonmark::markdown_html() %>%
    read_xml_lit() %>%
    xml2::xml_find_all("//a") %>%
    length() > 0
}

get_cell_hyperlink <- function(text) {
  text %>%
    commonmark::markdown_html() %>%
    read_xml_lit() %>%
    xml2::xml_find_first("//a") %>%
    xml2::xml_attr("href")
}
