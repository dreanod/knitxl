


XlObj <- R6::R6Class("XlObj", list(
  fn = NA,
  wb = NA,
  current_ws = NA,
  current_row = NA,

  reset = function() {
    self$fn <- ""
    self$wb <- openxlsx::createWorkbook()
    openxlsx::addWorksheet(self$wb, "Sheet 1")
    self$current_ws = 1
    self$current_row = 1
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

  insert_text = function(text) {
    stopifnot(is.character(text))
    text <- split_string_by_line(text)
    openxlsx::writeData(self$wb, self$current_ws, text,
                        startRow = self$current_row)
    self$increment_current_row(length(text) + 1)
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
    openxlsx::write.xlsx(self$content, file = self$fn)
    invisible(self)
  }
))

xl_obj <- XlObj$new()

