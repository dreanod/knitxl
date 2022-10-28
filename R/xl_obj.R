


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

  insert_text = function(text, style = NULL) {
    stopifnot("`text` must be a character" = is.character(text),
              "`text` must be of length 1" = length(text) == 1)

    text <- split_string_by_line(text)
    n_text_rows <- length(text)

    openxlsx::writeData(self$wb, self$current_ws, text,
                        startRow = self$current_row)

    if (!is.null(style)) {
      stopifnot("`style` must be of class `Style`" = inherits(style, "Style"))
      style_rows <- self$current_row:(self$current_row + n_text_rows - 1)
      openxlsx::addStyle(self$wb, self$current_ws, style,
                         rows = self$current_row, cols = 1, stack = TRUE)
    }

    self$increment_current_row(n_text_rows + 1)
    self$empty <- FALSE
    invisible(self)
  },

  insert_vector = function(x, style = NULL, h_style = NULL,
                           direction = "vertical") {
    stopifnot("`x` must be a list or an atomic vector" = is.atomic(x) | is.list(x),
              "`x` must be non-NULL" = !is.null(x),
              "`direction` should be 'vertical' or 'horizontal'" = direction %in% c('vertical', 'horizontal'))

    if (direction == "vertical") {
      if (is.null(names(x))) {
        openxlsx::writeData(self$wb, self$current_ws, x, startRow = self$current_row)
      } else {
        x <- as.data.frame(x)
        openxlsx::writeData(self$wb, self$current_ws, x, startRow = self$current_row,
                  rowNames = TRUE, colNames = FALSE)
      }
      n_rows_inserted <- length(x)
    } else {
      x <- t(x)
      if (is.null(names(x))) {
        openxlsx::writeData(self$wb, self$current_ws, x, startRow = self$current_row,
                  colNames = FALSE)
        n_rows_inserted <- 1
      } else {
        openxlsx::writeData(self$wb, self$current_ws, x, startRow = self$current_row,
                  colNames = TRUE)
        n_rows_inserted <- 2
      }
    }

    self$increment_current_row(n_rows_inserted + 1)
    self$empty <- FALSE
    invisible(self)
  },

  insert_data_frame = function(df, style = NULL, h_style = NULL, r_style = NULL,
                               colNames = TRUE, rowNames = TRUE, max_rows = 50) {
    stopifnot("`df` must be a data.frame" = is.data.frame(df))

    if (nrow(df) > max_rows) {
      original_nrow_df <- nrow(df)
      df <- df[1:max_rows, ]
      cropping_df <- TRUE
    } else {
      croppind_df <- FALSE
    }

    openxlsx::writeData(self$wb, self$current_ws, df, startRow = self$current_row,
                        rowNames = rowNames, colNames = colNames)

    n_rows_inserted <- nrow(df) + colNames
    self$increment_current_row(n_rows_inserted + 1)

    if (cropping_df) {
      message(glue::glue("...",
                         "Printing {max_rows} rows out of {original_nrow_df}.",
                         .sep = "/n"))
    }

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
  },

  increment_current_row = function(n) {
    self$current_row <- self$current_row + n
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
insert_text <- function(text, style = NULL) {
  xl_obj$insert_text(text, style = style)
}

#' @export
insert_vector <- function(x, style = NULL, h_style = NULL, direction = "vertical") {
  xl_obj$insert_vector(x, style = style, h_style = h_style, direction = direction)
}

#' @export
insert_data_frame <- function(df, style = NULL, h_style = NULL, r_style = NULL,
                              colNames = TRUE, rowNames = TRUE, max_rows = 50) {
  xl_obj$insert_data_frame(df, style = style, h_style = h_style,
                           r_style = r_style, colNames = colNames,
                           rowNames = rowNames, max_rows = max_rows)
}
