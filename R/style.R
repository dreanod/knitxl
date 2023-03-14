


kxl_style <- local({

  body <- quote({
    style <- as.list(environment())
    class(style) <- "knitxl_style"
    style
  })

  types <- c("xl", paste("xl", get_cell_types(), sep = "."))
  arg_nms <- outer(types, get_base_cell_opts(), paste, sep = ".") %>%
    as.vector()
  special_args <- c("xl.gridlines",
                    "xl.vector.direction",
                    "xl.table.direction",
                    "xl.table.maxrows",
                    "xl.table.colNames",
                    "xl.table.rowNames")
  arg_nms <- c(arg_nms, special_args)
  arg_nms <- sort(arg_nms)
  args <- stats::setNames(rep(list(NULL), length(arg_nms)), arg_nms)

  rlang::new_function(args, body)
})

kxl_style_default <- function() {
  kxl_style(xl.gridlines = FALSE,
            xl.fontName = "Calibri",
            xl.fontSize = 11,
            xl.rowHeight = 12.75,

            xl.text.h1.fontName = "Calibri Light (Headings)",
            xl.text.h1.fontSize = 18,
            xl.text.h1.fontColour = "#475368",
            xl.text.h2.fontName = "Calibri (Body)",
            xl.text.h2.fontSize = 15,
            xl.text.h2.fontColour = "#475368",
            xl.text.h2.textDecoration = "bold",
            xl.text.h2.border = "bottom",
            xl.text.h2.borderStyle = "thick",
            xl.text.h2.borderColour = "#4F71BE",

            xl.text.h3.fontName = "Calibri (Body)",
            xl.text.h3.fontSize = 13,
            xl.text.h3.fontColour = "#475368",
            xl.text.h3.textDecoration = "bold",
            xl.text.h3.border = "bottom",
            xl.text.h3.borderStyle = "thick",
            xl.text.h3.borderColour = "#A6B7DE",

            xl.text.h4.fontName = "Calibri (Body)",
            xl.text.h4.fontSize = 11,
            xl.text.h4.fontColour = "#475368",
            xl.text.h4.textDecoration = "bold",
            xl.text.h4.border = "bottom",
            xl.text.h4.borderStyle = "medium",
            xl.text.h4.borderColour = "#A6B7DE",

            xl.text.h5.fontName = "Calibri (Body)",
            xl.text.h5.fontSize = 11,
            xl.text.h5.fontColour = "#475368",
            xl.text.h5.textDecoration = "bold",

            xl.text.h6.fontName = "Calibri (Body)",
            xl.text.h6.fontSize = 11,
            xl.text.h6.fontColour = "#475368",
            xl.text.h6.textDecoration = "italic",

            xl.text.hrule.border = "top",
            xl.text.hrule.borderStyle = "thick",
            xl.text.hrule.borderColour = "black",

            xl.text.blockquote.textDecoration = "italic",
            xl.text.blockquote.indent = 1,
            xl.text.blockquote.fontColour = "#7F7F7F",

            xl.text.error.fontColour = "red",
            xl.text.error.textDecoration = "bold",

            xl.text.warning.fontColour = "orange",
            xl.text.warning.textDecoration = "bold",

            xl.text.message.fontColour = "blue",
            xl.text.message.textDecoration = "italic",

            xl.text.output.fontColour = "grey",
            xl.text.output.fontName = "Courier New",
            xl.text.output.textDecoration = "italic",

            xl.text.source.fontName = "Courier New",

            xl.vector.numFmt = "GENERAL",
            xl.vector.direction = "vertical",
            xl.vector.names.textDecoration = "bold",
            xl.vector.names.border = "right",
            xl.vector.names.borderStyle = "thin",
            xl.vector.names.borderColour = "black",

            xl.table.numFmt = "GENERAL",
            xl.table.direction = "vertical",
            xl.table.maxrows = 50,
            xl.table.colNames = TRUE,
            xl.table.rowNames = TRUE,
            xl.table.header.border = "bottom",
            xl.table.header.textDecoration = "bold",
            xl.table.header.halign = "center",
            xl.table.rownames.fontColour = "grey",
            xl.table.rownames.border = "right")
}

kxl_set_default_theme <- function() {
  knitr::opts_chunk$set(kxl_style_default())
}

kxl_style_get <- function(options) {
  purrr::keep(options, stringr::str_starts(names(options), "xl."))
}

kxl_style_get_value <- function(style, keys) {
  get_value <- function(v) {
    if (is.null(style[[v]])) {
      split_value <- strsplit(v, ".", fixed = TRUE)[[1]]
      if (length(split_value) > 2) {
        split_value <- split_value[-(length(split_value) - 1)]
        return(get_value(paste(split_value, collapse = ".")))
      } else {
        return(NULL)
      }
    }
    style[[v]]
  }

  values <- purrr::map(keys, get_value)
  if (length(values) == 1) {
    return(values[[1]])
  }
  stats::setNames(values, keys)
}
