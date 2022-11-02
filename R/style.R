

kxl_style <- function(xl.fontName = NULL,
                      xl.fontSize = NULL,
                      xl.fontColour = NULL,
                      xl.numFmt = NULL,
                      xl.border = NULL,
                      xl.borderColour = NULL,
                      xl.borderStyle = NULL,
                      xl.bgFill = NULL,
                      xl.fgFill = NULL,
                      xl.halign = NULL,
                      xl.valign = NULL,
                      xl.textDecoration = NULL,
                      xl.wrapText = NULL,
                      xl.textRotation = NULL,
                      xl.indent = NULL,
                      xl.rowHeight = NULL,

                      xl.text.fontName = NULL,
                      xl.text.fontSize = NULL,
                      xl.text.fontColour = NULL,
                      xl.text.numFmt = NULL,
                      xl.text.border = NULL,
                      xl.text.borderColour = NULL,
                      xl.text.borderStyle = NULL,
                      xl.text.bgFill = NULL,
                      xl.text.fgFill = NULL,
                      xl.text.halign = NULL,
                      xl.text.valign = NULL,
                      xl.text.textDecoration = NULL,
                      xl.text.wrapText = NULL,
                      xl.text.textRotation = NULL,
                      xl.text.indent = NULL,
                      xl.text.rowHeight = NULL,

                      xl.text.error.fontName = NULL,
                      xl.text.error.fontSize = NULL,
                      xl.text.error.fontColour = NULL,
                      xl.text.error.numFmt = NULL,
                      xl.text.error.border = NULL,
                      xl.text.error.borderColour = NULL,
                      xl.text.error.borderStyle = NULL,
                      xl.text.error.bgFill = NULL,
                      xl.text.error.fgFill = NULL,
                      xl.text.error.halign = NULL,
                      xl.text.error.valign = NULL,
                      xl.text.error.textDecoration = NULL,
                      xl.text.error.wrapText = NULL,
                      xl.text.error.textRotation = NULL,
                      xl.text.error.indent = NULL,
                      xl.text.error.rowHeight = NULL,

                      xl.text.warning.fontName = NULL,
                      xl.text.warning.fontSize = NULL,
                      xl.text.warning.fontColour = NULL,
                      xl.text.warning.numFmt = NULL,
                      xl.text.warning.border = NULL,
                      xl.text.warning.borderColour = NULL,
                      xl.text.warning.borderStyle = NULL,
                      xl.text.warning.bgFill = NULL,
                      xl.text.warning.fgFill = NULL,
                      xl.text.warning.halign = NULL,
                      xl.text.warning.valign = NULL,
                      xl.text.warning.textDecoration = NULL,
                      xl.text.warning.wrapText = NULL,
                      xl.text.warning.textRotation = NULL,
                      xl.text.warning.indent = NULL,
                      xl.text.warning.rowHeight = NULL,

                      xl.text.message.fontName = NULL,
                      xl.text.message.fontSize = NULL,
                      xl.text.message.fontColour = NULL,
                      xl.text.message.numFmt = NULL,
                      xl.text.message.border = NULL,
                      xl.text.message.borderColour = NULL,
                      xl.text.message.borderStyle = NULL,
                      xl.text.message.bgFill = NULL,
                      xl.text.message.fgFill = NULL,
                      xl.text.message.halign = NULL,
                      xl.text.message.valign = NULL,
                      xl.text.message.textDecoration = NULL,
                      xl.text.message.wrapText = NULL,
                      xl.text.message.textRotation = NULL,
                      xl.text.message.indent = NULL,
                      xl.text.message.rowHeight = NULL,

                      xl.text.source.fontName = NULL,
                      xl.text.source.fontSize = NULL,
                      xl.text.source.fontColour = NULL,
                      xl.text.source.numFmt = NULL,
                      xl.text.source.border = NULL,
                      xl.text.source.borderColour = NULL,
                      xl.text.source.borderStyle = NULL,
                      xl.text.source.bgFill = NULL,
                      xl.text.source.fgFill = NULL,
                      xl.text.source.halign = NULL,
                      xl.text.source.valign = NULL,
                      xl.text.source.textDecoration = NULL,
                      xl.text.source.wrapText = NULL,
                      xl.text.source.textRotation = NULL,
                      xl.text.source.indent = NULL,
                      xl.text.source.rowHeight = NULL,

                      xl.vector.fontName = NULL,
                      xl.vector.fontSize = NULL,
                      xl.vector.fontColour = NULL,
                      xl.vector.numFmt = NULL,
                      xl.vector.border = NULL,
                      xl.vector.borderColour = NULL,
                      xl.vector.borderStyle = NULL,
                      xl.vector.bgFill = NULL,
                      xl.vector.fgFill = NULL,
                      xl.vector.halign = NULL,
                      xl.vector.valign = NULL,
                      xl.vector.textDecoration = NULL,
                      xl.vector.wrapText = NULL,
                      xl.vector.textRotation = NULL,
                      xl.vector.indent = NULL,
                      xl.vector.rowHeight = NULL,
                      xl.vector.direction = NULL,

                      xl.vector.names.fontName = NULL,
                      xl.vector.names.fontSize = NULL,
                      xl.vector.names.fontColour = NULL,
                      xl.vector.names.numFmt = NULL,
                      xl.vector.names.border = NULL,
                      xl.vector.names.borderColour = NULL,
                      xl.vector.names.borderStyle = NULL,
                      xl.vector.names.bgFill = NULL,
                      xl.vector.names.fgFill = NULL,
                      xl.vector.names.halign = NULL,
                      xl.vector.names.valign = NULL,
                      xl.vector.names.textDecoration = NULL,
                      xl.vector.names.wrapText = NULL,
                      xl.vector.names.textRotation = NULL,
                      xl.vector.names.indent = NULL,
                      xl.vector.names.rowHeight = NULL,
                      ...) {
  style <- find_args(...)
  class(style) <- "knitxl_style"
  style
}

kxl_style_default <- function() {
  kxl_style(xl.fontName = "Calibri",
            xl.fontSize = 11,
            xl.rowHeight = 12.75,
            xl.text.error.fontColour = "red",
            xl.text.error.textDecoration = "bold",
            xl.text.warning.fontColour = "orange",
            xl.text.warning.textDecoration = "bold",
            xl.text.message.fontColour = "blue",
            xl.text.message.textDecoration = "italic",
            xl.text.source.fontName = "Courier New",
            xl.vector.numFmt = "COMMA",
            xl.vector.direction = "vertical",
            xl.vector.names.textDecoration = "bold",
            xl.vector.names.border = "right",
            xl.vector.names.borderStyle = "thin",
            xl.vector.names.borderColour = "black")
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
    return(values)
  }
  setNames(values, keys)
}


# Get all arguments in a function as a list. Will fail if an ellipsis argument
# named .ignore
# @param ... passed on in case enclosing function uses ellipsis in argument list
# @source ggplot2
find_args <- function(...) {
  env <- parent.frame()
  args <- names(formals(sys.function(sys.parent(1))))

  vals <- mget(args, envir = env)
  vals <- vals[!vapply(vals, is_missing_arg, logical(1))]

  modify_list(vals, list(..., `...` = NULL))
}

# @source ggplot2
is_missing_arg <- function(x) identical(x, quote(expr = ))

# More performant modifyList without recursion
# @source ggplot2
modify_list <- function(old, new) {
  for (i in names(new)) old[[i]] <- new[[i]]
  old
}
