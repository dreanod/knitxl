


XlObj <- R6::R6Class("XlObj", list(
  fn = NA,
  wb = NA,
  current_ws = NA,
  current_row = NA,
  empty = NA,

  reset = function() {
    self$fn <- ""
    self$wb <- openxlsx::createWorkbook()
    openxlsx::addWorksheet(self$wb, "Sheet 1")
    self$current_ws = 1
    self$current_row = 1
    self$empty <- TRUE
    invisible(self)
  },

  set_fn = function(fn) {
    stopifnot(is.character(fn))
    self$fn <- fn
    invisible(self)
  },

  get_wb = function() {
    self$wb
  },

  is_empty = function() {
    self$empty
  },

  insert_text = function(text, style, type) {
    stopifnot("`text` must be a character" = is.character(text),
              "`text` must be of length 1" = length(text) == 1)

    if (text != "") {
      text <- split_string_by_line(text)
      n_text_rows <- length(text)
      new_rows <- self$current_row:(self$current_row + n_text_rows - 1)

      openxlsx::writeData(self$wb, self$current_ws, text,
                          startRow = self$current_row)

      self$style_text(rows = new_rows, style, type)

      self$increment_current_row(n_text_rows + 1)

      self$empty <- FALSE
      invisible(self)
    }
  },

  style_text = function(rows, style, type) {
    args <- get_oxl_style_args(style, type)
    oxl_style <- do.call(openxlsx::createStyle, args)
    openxlsx::addStyle(self$wb, self$current_ws, oxl_style, rows = rows,
                       cols = 1, stack = FALSE)
    invisible(self)
  },

  insert_vector = function(x, style) {
    stopifnot("`x` must be a list or an atomic vector" = is.atomic(x) | is.list(x),
              "`x` must be non-NULL" = !is.null(x))

    direction <- kxl_style_get_value(style, "xl.vector.direction")
    stopifnot("`direction` should be 'vertical' or 'horizontal'" = direction %in% c('vertical', 'horizontal'))

    if (direction == "vertical") {
      if (is.null(names(x))) {
        openxlsx::writeData(self$wb, self$current_ws, x, startRow = self$current_row)
        n_rows_inserted <- length(x)
        vec_cols <- 1
        name_cols <- NULL
      } else {
        x <- as.data.frame(x)
        openxlsx::writeData(self$wb, self$current_ws, x, startRow = self$current_row,
                  rowNames = TRUE, colNames = FALSE)
        n_rows_inserted <- nrow(x)
        vec_cols <- 2
        name_cols <- 1
      }
      vec_rows <- self$current_row:(self$current_row + n_rows_inserted - 1)
      name_rows <- vec_rows
    } else {
      x <- t(x)
      if (is.null(names(x))) {
        openxlsx::writeData(self$wb, self$current_ws, x, startRow = self$current_row,
                  colNames = FALSE)
        n_rows_inserted <- 1
        vec_rows <- self$current_row
        name_rows <- NULL
      } else {
        openxlsx::writeData(self$wb, self$current_ws, x, startRow = self$current_row,
                  colNames = TRUE)
        n_rows_inserted <- 2
        vec_rows <- self$current_row + 1
        name_rows <- self$current_row
      }
      vec_cols <- seq_along(x)
      name_cols <- vec_cols
    }

    self$style_vector(vec_rows, vec_cols, name_rows, name_cols, style)
    self$increment_current_row(n_rows_inserted + 1)

    self$empty <- FALSE
    invisible(self)
  },

  style_vector = function(vec_rows, vec_cols, name_rows, name_cols, style) {
    self$style_cells(style, "vector", vec_rows, vec_cols)
    if (!is.null(name_rows) & !is.null(name_cols))
      self$style_cells(style, "vector.names", name_rows, name_cols)
    invisible(self)
  },

  insert_data_frame = function(df, style) {
    stopifnot("`df` must be a data.frame" = is.data.frame(df))

    max_rows <- kxl_style_get_value(style, "xl.table.maxrows")
    colNames <- kxl_style_get_value(style, "xl.table.colNames")
    rowNames <- kxl_style_get_value(style, "xl.table.rowNames")

    if (nrow(df) > max_rows) {
      original_nrow_df <- nrow(df)
      df <- df[1:max_rows, ]
      cropping_df <- TRUE
    } else {
      cropping_df <- FALSE
    }

    openxlsx::writeData(self$wb, self$current_ws, df, startRow = self$current_row,
                        rowNames = rowNames, colNames = colNames)

    n_rows_inserted <- nrow(df) + colNames

    data_rows <- if (colNames) (self$current_row + 1):(self$current_row + n_rows_inserted - 1) else self$current_row:(self$current_row + n_rows_inserted)
    header_rows <- if (colNames) self$current_row else NULL
    name_rows <- if (rowNames) data_rows else NULL

    data_cols <- if (rowNames) 2:(ncol(df) + 1) else 1:ncol(df)
    header_cols <- if(colNames) data_cols else NULL
    name_cols <- if(rowNames) 1 else NULL

    self$style_data_frame(style, data_rows, data_cols, header_rows, header_cols,
                          name_rows, name_cols)

    self$increment_current_row(n_rows_inserted + 1)

    if (cropping_df) {
      message(glue::glue("...",
                         "Printing {max_rows} rows out of {original_nrow_df}.",
                         .sep = "/n"))
    }

    self$empty <- FALSE
    invisible(self)
  },

  style_data_frame = function(style, data_rows, data_cols, header_rows, header_cols,
                              name_rows, name_cols) {
    ind <- expand.grid(rows = data_rows, cols = data_cols)
    self$style_cells(style, "table", ind$rows, ind$cols)

    if (!is.null(header_rows) & !is.null(header_cols))
      self$style_cells(style, "table.header", header_rows, header_cols)

    if (!is.null(name_rows) & !is.null(name_cols))
      self$style_cells(style, "table.rownames", name_rows, name_cols)

    invisible(self)
  },

  style_cells = function(style, key, rows, cols) {
    args <- get_oxl_style_args(style, key)
    oxl_style <- do.call(openxlsx::createStyle, args)
    openxlsx::addStyle(self$wb, self$current_ws, oxl_style, rows = rows,
                       cols = cols, stack = FALSE)
    invisible(self)
  },

  insert_image = function(fn, width, height, units, dpi) {
    openxlsx::insertImage(wb = self$wb,
                          sheet = self$current_ws,
                          file = fn,
                          width = width,
                          height = height,
                          startRow = self$current_row,
                          startCol = 1,
                          units = units,
                          dpi = dpi)
    n_rows_inserted <- ceiling(height * 71.4 / 15)
    self$increment_current_row(n_rows_inserted + 1)

    self$empty <- FALSE
    invisible(self)
  },

  increment_current_row = function(n) {
    self$current_row <- self$current_row + n
    invisible(self)
  },

  view = function() {
    openxlsx::openXL(self$wb)
    invisible(self)
  },

  write = function() {
    flag <- openxlsx::saveWorkbook(self$wb, self$fn, overwrite = TRUE,
                                   returnValue = TRUE)
    invisible(self)
  }
))

xl_obj <- XlObj$new()


### public interface to write on xl_obj

#' @export
insert_text <- function(text, style, type) {
  xl_obj$insert_text(text, style = style, type = type)
}

#' @export
insert_vector <- function(x, style) {
  xl_obj$insert_vector(x, style = style)
}

#' @export
insert_data_frame <- function(df, style) {
  xl_obj$insert_data_frame(df, style = style)
}

insert_image <- function(fn, width, height, units, dpi) {
  xl_obj$insert_image(fn, width = width, height = height,
                      units = units, dpi = dpi)
}


### Styles

get_oxl_style_args <- function(style, type) {
  arg_names <- c("fontName",
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
                 "indent")
  style_opt_names <- paste("xl", type, arg_names, sep = ".")
  args <- setNames(kxl_style_get_value(style, style_opt_names), arg_names)
  purrr::discard(args, is.null)
}
