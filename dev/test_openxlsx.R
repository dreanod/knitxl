library(openxlsx)
library(stringr)

wb <- createWorkbook()

addWorksheet(wb, "Sheet 1")

text <- c("* fruits
  + apples
    - macintosh
    - red delicious
  + pears
  + peaches", "* vegetables
  + broccoli
  + chard")

text <- str_split(text, "\n")
text <- purrr::flatten_chr(text)


writeData(wb, 1, text)

openXL(wb)
