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
style <- createStyle(fontColour = "red", textDecoration = "bold")
addStyle(wb, 1, style, 1, 1)

writeData(wb, 1, 1:4, 7, 1)

openXL(wb)
