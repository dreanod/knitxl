

#' @importFrom magrittr %<>%
XlObj <- R6::R6Class("XlObj", list(
  fn = NA,
  wb = NA,
  current_ws = NA,
  current_row = NA,
  empty = NA,
  cell_styles = NA,

  reset = function() {
    self$fn <- ""
    self$wb <- openxlsx::createWorkbook()
    gridlines <- kxl_style_get_value(knitr::opts_chunk$get(), "xl.gridlines")
    self$cell_styles <- initialize_style()
    openxlsx::addWorksheet(self$wb, "Sheet 1", gridLines = gridlines)
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

  get_cell_style = function(cell_type) {
    stopifnot("no style for requested cell type" = cell_type %in% names(self$cell_styles))
    self$cell_styles[[cell_type]]
  },

  insert_text = function(text, type) {
    stopifnot("`text` must be a character" = is.character(text),
              "`text` must be of length 1" = length(text) == 1)

    if (text != "") {
      text <- split_string_by_line(text)
      n_text_rows <- length(text)
      new_rows <- self$current_row:(self$current_row + n_text_rows - 1)

      purrr::walk(1:n_text_rows, ~ self$write_line(text = text[.x],
                                                   row = new_rows[.x],
                                                   type = type))

      self$increment_current_row(n_text_rows + 1)

      self$empty <- FALSE
      invisible(self)
    }
  },

  write_line = function(text, row, type) {
    if (type == "text")
      self$parse_and_write_md_line(text, row)
    else
      self$write_line_in_cell(text, row, cell_type = type)
  },

  write_line_in_cell = function(text, row, cell_type) {
    if (stringr::str_length(text) > 0)
      text <- paste0(get_md_string_flag(), text)
    openxlsx::writeData(self$wb, self$current_ws, text, startRow = row)
    self$style_cells(cell_type, row, 1)
  },

  parse_and_write_md_line = function(text, row) {
    if (stringr::str_starts(text, "^#")) {
      self$write_header(text, row)
    } else if (stringr::str_starts(text, "^[*-] ")) {
      self$write_list(text, row)
    } else {
      self$write_line_in_cell(text, row, "text")
    }
  },

  write_header = function(text, row) {
    header_level <- stringr::str_extract(text, "^#*") %>%
      stringr::str_count("#")
    text <- stringr::str_remove(text, "^#* *")
    self$write_line_in_cell(text, row, paste0("text.h", header_level))
  },

  write_list = function(text, row) {
    text <- stringr::str_replace(text, "^[*-] ", "• ")
    self$write_line_in_cell(text, row, "text")
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
    self$style_cells("vector", vec_rows, vec_cols)
    if (!is.null(name_rows) & !is.null(name_cols))
      self$style_cells("vector.names", name_rows, name_cols)
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
    self$style_cells("table", ind$rows, ind$cols)

    if (!is.null(header_rows) & !is.null(header_cols))
      self$style_cells("table.header", header_rows, header_cols)

    if (!is.null(name_rows) & !is.null(name_cols))
      self$style_cells("table.rownames", name_rows, name_cols)

    invisible(self)
  },

  style_cells = function(cell_type, rows, cols) {
    oxl_style <- self$get_cell_style(cell_type)
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
    self$postprocess()
    flag <- openxlsx::saveWorkbook(self$wb, self$fn, overwrite = TRUE,
                                   returnValue = TRUE)
    invisible(self)
  },

  postprocess = function() {
    self$wb@.xData$sharedStrings %<>% postprocess_shared_strings()
  }
))

xl_obj <- XlObj$new()


### public interface to write on xl_obj

#' @export
insert_text <- function(text, type) {
  xl_obj$insert_text(text, type = type)
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
  arg_names <- setdiff(get_base_cell_opts(), "rowHeight")
  style_opt_names <- paste("xl", type, arg_names, sep = ".")
  args <- setNames(kxl_style_get_value(style, style_opt_names), arg_names)
  purrr::discard(args, is.null)
}

initialize_style <- function() {
  style <- kxl_style_get(knitr::opts_chunk$get())

  cell_types <- c("text",
                  paste0("text.", c("h1", "h2", "h3", "h4", "h5", "h6",
                                  "error", "warning", "message",
                                  "source")),
                  "vector", "vector.names",
                  "table", "table.header", "table.rownames")
  setNames(purrr::map(cell_types, ~ create_cell_style(style, cell_type = .x)), cell_types)
}

create_cell_style <- function(style, cell_type) {
  args <- get_oxl_style_args(style, type = cell_type)
  do.call(openxlsx::createStyle, args)
}
