get_base_cell_opts <- function() {
  c("fontName",
    "fontSize",
    "fontColour",
    "numFmt",
    "border",
    "borderColour",
    "borderStyle",
    "bgFill",
    "fgFill",
    "halign",
    "valign",
    "textDecoration",
    "wrapText",
    "textRotation",
    "indent",
    "rowHeight")
}

get_cell_types <- function() {
  c("text",
    "text.h1",
    "text.h2",
    "text.h3",
    "text.h4",
    "text.h5",
    "text.h6",
    "text.hrule",
    "text.blockquote",
    "text.error",
    "text.warning",
    "text.message",
    "text.source",
    "text.output",
    "vector",
    "vector.names",
    "table",
    "table.header",
    "table.rownames"
    )
}

get_md_string_flag <- function() {
  "!!! knitxl: markdown line to parse !!!"
}
