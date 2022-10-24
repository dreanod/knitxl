## inspired by xtable gallery
#https://CRAN.R-project.org/package=xtable/vignettes/xtableGallery.pdf

## Create a new workbook
library(openxlsx)

wb <- createWorkbook()
data(tli, package = "xtable")

## data.frame
test.n <- "data.frame"
my.df <- tli[1:10, ]
addWorksheet(wb = wb, sheetName = test.n)
writeData(wb = wb, sheet = test.n, x = my.df, borders = "n")

## matrix
test.n <- "matrix"
design.matrix <- model.matrix(~ sex * grade, data = my.df)
addWorksheet(wb = wb, sheetName = test.n)
writeData(wb = wb, sheet = test.n, x = design.matrix)

## aov
test.n <- "aov"
fm1 <- aov(tlimth ~ sex + ethnicty + grade + disadvg, data = tli)
addWorksheet(wb = wb, sheetName = test.n)
writeData(wb = wb, sheet = test.n, x = fm1)

## lm
test.n <- "lm"
fm2 <- lm(tlimth ~ sex*ethnicty, data = tli)
addWorksheet(wb = wb, sheetName = test.n)
writeData(wb = wb, sheet = test.n, x = fm2)

## anova 1
test.n <- "anova"
my.anova <- anova(fm2)
addWorksheet(wb = wb, sheetName = test.n)
writeData(wb = wb, sheet = test.n, x = my.anova)

## anova 2
test.n <- "anova2"
fm2b <- lm(tlimth ~ ethnicty, data = tli)
my.anova2 <- anova(fm2b, fm2)
addWorksheet(wb = wb, sheetName = test.n)
writeData(wb = wb, sheet = test.n, x = my.anova2)

## glm
test.n <- "glm"
fm3 <- glm(disadvg ~ ethnicty*grade, data = tli, family = binomial())
addWorksheet(wb = wb, sheetName = test.n)
writeData(wb = wb, sheet = test.n, x = fm3)

## prcomp
test.n <- "prcomp"
pr1 <- prcomp(USArrests)
addWorksheet(wb = wb, sheetName = test.n)
writeData(wb = wb, sheet = test.n, x = pr1)

## summary.prcomp
test.n <- "summary.prcomp"
addWorksheet(wb = wb, sheetName = test.n)
writeData(wb = wb, sheet = test.n, x = summary(pr1))

## simple table
test.n <- "table"
data(airquality)
airquality$OzoneG80 <- factor(airquality$Ozone > 80,
                              levels = c(FALSE, TRUE),
                              labels = c("Oz <= 80", "Oz > 80"))
airquality$Month <- factor(airquality$Month,
                           levels = 5:9,
                           labels = month.abb[5:9])
my.table <- with(airquality, table(OzoneG80,Month) )
addWorksheet(wb = wb, sheetName = test.n)
writeData(wb = wb, sheet = test.n, x = my.table)

## survdiff 1
library(survival)
test.n <- "survdiff1"
addWorksheet(wb = wb, sheetName = test.n)
x <- survdiff(Surv(futime, fustat) ~ rx, data = ovarian)
writeData(wb = wb, sheet = test.n, x = x)

## survdiff 2
test.n <- "survdiff2"
addWorksheet(wb = wb, sheetName = test.n)
expect <- survexp(futime ~ ratetable(age=(accept.dt - birth.dt),
                                     sex=1,year=accept.dt,race="white"), jasa, cohort=FALSE,
                  ratetable=survexp.usr)
x <- survdiff(Surv(jasa$futime, jasa$fustat) ~ offset(expect))
writeData(wb = wb, sheet = test.n, x = x)

## coxph 1
test.n <- "coxph1"
addWorksheet(wb = wb, sheetName = test.n)
bladder$rx <- factor(bladder$rx, labels = c("Pla","Thi"))
x <- coxph(Surv(stop,event) ~ rx, data = bladder)
writeData(wb = wb, sheet = test.n, x = x)

## coxph 2
test.n <- "coxph2"
addWorksheet(wb = wb, sheetName = test.n)
x <- coxph(Surv(stop,event) ~ rx + cluster(id), data = bladder)
writeData(wb = wb, sheet = test.n, x = x)

## cox.zph
test.n <- "cox.zph"
addWorksheet(wb = wb, sheetName = test.n)
x <- cox.zph(coxph(Surv(futime, fustat) ~ age + ecog.ps,  data=ovarian))
writeData(wb = wb, sheet = test.n, x = x)

## summary.coxph 1
test.n <- "summary.coxph1"
addWorksheet(wb = wb, sheetName = test.n)
x <- summary(coxph(Surv(stop,event) ~ rx, data = bladder))
writeData(wb = wb, sheet = test.n, x = x)

## summary.coxph 2
test.n <- "summary.coxph2"
addWorksheet(wb = wb, sheetName = test.n)
x <- summary(coxph(Surv(stop,event) ~ rx + cluster(id), data = bladder))
writeData(wb = wb, sheet = test.n, x = x)

## view without saving
openXL(wb)
