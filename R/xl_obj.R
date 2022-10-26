


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
    stopifnot(is.character(text))
    text <- split_string_by_line(text)
    openxlsx::writeData(self$wb, self$current_ws, text,
                        startRow = self$current_row)
    if (!is.null(style)) {
      openxlsx::addStyle(self$wb, self$current_ws, style,
                         rows = self$current_row, cols = 1, stack = TRUE)
    }
    self$increment_current_row(length(text) + 1)
    self$empty <- FALSE
    invisible(self)
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

