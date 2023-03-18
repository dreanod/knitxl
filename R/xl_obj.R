#' @import R6
NULL

XlObj <- R6::R6Class("XlObj", list(
  fn = NA,
  wb = NA,
  current_ws = NA,
  current_row = NA,
  empty = NA,
  cell_styles = NA,
  is_in_code_block = NA,

  reset = function() {
    self$fn <- ""
    self$wb <- openxlsx::createWorkbook()
    gridlines <- kxl_style_get_value(knitr::opts_chunk$get(), "xl.gridlines")
    self$cell_styles <- initialize_style()
    openxlsx::addWorksheet(self$wb, "Sheet 1", gridLines = gridlines)
    self$current_ws = 1
    self$current_row = 1
    self$empty <- TRUE
    self$is_in_code_block <- FALSE
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
    stopifnot("`text` must be a character" = is.character(text))

    text <- split_string_by_line(text)
    if (all(text == "")) {
      return(invisible(self))
    }

    n_text_rows <- length(text)
    new_rows <- self$current_row:(self$current_row + n_text_rows - 1)

    purrr::walk(1:n_text_rows, ~ self$write_line(text = text[.x],
                                                 type = type))
    self$newline()
    invisible(self)
  },

  write_line = function(text, type) {

    if (type == "text.output") {
      self$write_line_in_cell(text, cell_type = type, parse_md = FALSE)
    } else if (detect_code_fence(text)) {
      self$is_in_code_block <- !self$is_in_code_block
      self$newline()
    } else if (type == "text") {
      if (self$is_in_code_block) {
        self$write_line_in_cell(text, cell_type = "text.source", parse_md = FALSE)
      } else if (stringr::str_length(text) > 0) {
        self$parse_and_write_md_line(text)
      } else {
        self$newline()
      }
    } else {
      self$write_line_in_cell(text, cell_type = type, parse_md = FALSE)
    }

    invisible(self)
  },

  write_line_in_cell = function(text, cell_type, parse_md = TRUE) {

    if (parse_md) {
      text <- paste0(get_md_string_flag(), text)

      if (cell_type == "text" & cell_has_hyperlink(text)) {
        link <- get_cell_hyperlink(text)
        text <- stats::setNames(link, text)
        class(text) <- "hyperlink"
      }
    }

    openxlsx::writeData(self$wb, self$current_ws, text, startRow = self$current_row)
    self$empty <- FALSE
    self$style_cells(cell_type, self$current_row, 1)
    self$newline()
    invisible(self)
  },

  parse_and_write_md_line = function(text) {
    if (stringr::str_starts(text, "^#")) {
      self$write_header(text)
    } else if (detect_hrule(text)) {
      self$insert_hrule()
    } else if (detect_blockquote(text)) {
      self$write_line_in_cell(text %>% remove_blockquote(), "text.blockquote")
    } else {
      self$write_line_in_cell(text, "text")
    }

    if (detect_images(text)) {
      self$insert_images_from_md(text)
    }

    invisible(self)
  },

  insert_images_from_md = function(text) {
    images <- get_path_to_images(text)

    purrr::walk(images, function(path) {
      img_dims <- get_img_dims(path)
      self$insert_image(fn = path, width = img_dims[["width"]],
                        height = img_dims[["height"]],
                        units = "px", dpi = 72)
    })

    invisible(self)
  },

  insert_hrule = function() {
    self$newline()
    openxlsx::writeData(self$wb, self$current_ws, "", startRow = self$current_row)
    self$style_cells("text.hrule", self$current_row, 1:10)
    self$empty <- FALSE
    self$newline()
    invisible(self)
  },

  write_header = function(text) {
    header_level <- stringr::str_extract(text, "^#*") %>%
      stringr::str_count("#")
    text <- stringr::str_remove(text, "^#* *")

    ws_name <- extract_ws_name_option(text)
    if (!is.null(ws_name)) {
      text <- remove_option_string(text)
      self$new_worksheet(ws_name)
    }

    self$write_line_in_cell(text, paste0("text.h", header_level))
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
                          startCol = 2,
                          units = units,
                          dpi = dpi)
    height <- convert2inch(height, units)
    width <- convert2inch(width, units)
    n_rows_inserted <- ceiling(height * 71.4 / 15)
    self$increment_current_row(n_rows_inserted + 1)

    self$empty <- FALSE
    invisible(self)
  },

  increment_current_row = function(n) {
    self$current_row <- self$current_row + n
    invisible(self)
  },

  newline = function() {
    self$increment_current_row(1)
    invisible(self)
  },

  new_worksheet = function(ws_name) {
    if (self$is_empty()) {
      openxlsx::renameWorksheet(self$wb, 1, ws_name)
    } else {
      gridlines <- kxl_style_get_value(knitr::opts_chunk$get(), "xl.gridlines")
      openxlsx::addWorksheet(self$wb, ws_name, gridLines = gridlines)
      self$is_in_code_block <- FALSE
      self$current_ws <- self$current_ws + 1
      self$current_row <- 1
    }
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


### interface to write on xl_obj

insert_text <- function(text, type) {
  xl_obj$insert_text(text, type = type)
  return(NULL)
}

insert_vector <- function(x, style) {
  xl_obj$insert_vector(x, style = style)
  return(NULL)
}

insert_data_frame <- function(df, style) {
  xl_obj$insert_data_frame(df, style = style)
  return(NULL)
}

insert_image <- function(fn, width, height, units, dpi) {
  xl_obj$insert_image(fn, width = width, height = height,
                      units = units, dpi = dpi)
  return(NULL)
}

### Styles

get_oxl_style_args <- function(style, type) {
  arg_names <- setdiff(get_base_cell_opts(), "rowHeight")
  style_opt_names <- paste("xl", type, arg_names, sep = ".")
  args <- stats::setNames(kxl_style_get_value(style, style_opt_names), arg_names)
  purrr::discard(args, is.null)
}

initialize_style <- function() {
  style <- kxl_style_get(knitr::opts_chunk$get())

  cell_types <- get_cell_types()
  stats::setNames(purrr::map(cell_types, ~ create_cell_style(style, cell_type = .x)), cell_types)
}

create_cell_style <- function(style, cell_type) {
  args <- get_oxl_style_args(style, type = cell_type)
  do.call(openxlsx::createStyle, args)
}

