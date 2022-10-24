require(ggplot2)
library(openxlsx)

wb <- createWorkbook()

## read historical prices from yahoo finance
ticker <- "CBA.AX"
csv.url <- paste0("https://query1.finance.yahoo.com/v7/finance/download/",
                  ticker, "?period1=1597597610&period2=1629133610&interval=1d&events=history&includeAdjustedClose=true")
prices <- read.csv(url(csv.url), as.is = TRUE)
prices$Date <- as.Date(prices$Date)
close <- prices$Close
prices$logReturns = c(0, log(close[2:length(close)]/close[1:(length(close)-1)]))

## Create plot of price series and add to worksheet
ggplot(data = prices, aes(as.Date(Date), as.numeric(Close))) +
  geom_line(colour="royalblue2") +
  labs(x = "Date", y = "Price", title = ticker) +
  geom_area(fill = "royalblue1",alpha = 0.3) +
  coord_cartesian(ylim=c(min(prices$Close)-1.5, max(prices$Close)+1.5))

## Add worksheet and write plot to sheet
addWorksheet(wb, sheetName = "CBA")
insertPlot(wb, sheet = 1, xy = c("J", 3))

## Histogram of log returns
ggplot(data = prices, aes(x = logReturns)) + geom_bar(binwidth=0.0025) +
  labs(title = "Histogram of log returns")

## currency
class(prices$Close) <- "currency" ## styles as currency in workbook

## write historical data and  histogram of returns
writeDataTable(wb, sheet = "CBA", x = prices)
insertPlot(wb, sheet = 1, startRow=25, startCol = "J")

## Add conditional formatting to show where logReturn > 0.01 using default style
conditionalFormat(wb, sheet = 1, cols = 1:ncol(prices), rows = 2:(nrow(prices)+1),
                  rule = "$H2 > 0.01")

## style log return col as a percentage
logRetStyle <- createStyle(numFmt = "percentage")

addStyle(wb, 1, style = logRetStyle, rows = 2:(nrow(prices) + 1),
         cols = "H", gridExpand = TRUE)

setColWidths(wb, sheet=1, cols = c("A", "F", "G", "H"), widths = 15)

## save workbook to working directory
saveWorkbook(wb, "stockPrice.xlsx", overwrite = TRUE)
openXL("stockPrice.xlsx")
