


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
        writeData(self$wb, self$current_ws, x, startRow = self$currentRow)
      } else {
        x <- as.data.frame(x)
        writeData(self$wb, self$current_ws, x, startRow = self$currentRow,
                  rowNames = TRUE, colNames = FALSE)
      }
      n_rows_inserted <- length(x)
    } else {
      x <- t(x)
      if (is.null(names(x))) {
        writeData(self$wb, self$current_ws, x, startRow = self$currentRow,
                  colNames = FALSE)
        n_rows_inserted <- 1
      } else {
        writeData(self$wb, self$current_ws, x, startRow = self$currentRow,
                  colNames = TRUE)
        n_rows_inserted <- 2
      }
    }

    self$increment_current_row(n_rows_inserted + 1)
    self$empty <- FALSE
    invisible(self)
  },

  insert_data_frame = function(df, style = NULL, h_style = NULL,
                               direction = "down") {

  }

  insert_text = function(text, style = NULL) {
    stopifnot(is.character(text))
  },

  insert_data_frame = function(tab, sty) {
    stopifnot(is.data.frame(tab))
    openxlsx::writeData(self$wb, self$current_ws, tab,
                        startRow = self$current_row,
                        rowNames = TRUE)
    self$increment_current_row(nrow(tab) + 1)
    self$empty <- FALSE
    invisible(self)
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

