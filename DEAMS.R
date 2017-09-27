library(xlsx)
library(ggplot2)
library(survMisc)
library(survival)
library(survminer)
library(data.table)

source("misc.R")

# Getting the data
AllData <- read.xlsx(filePath, 1, startRow = 6, colIndex = c(1, 4, 5, 6, 7, 9))
AllData$Type <- gsub('DEAMS - ', '', AllData$Type)
AllData$Severity <- gsub('Severity ', '', AllData$Severity)
AllData <- AllData[order(AllData$Submit.Date), ]
# Regex to get the locations and customer (after this should drop description)
AllData$Location <- trimws(sapply(regmatches(AllData$Description, gregexpr("(?<=Base:).*?(?=\\w).*?(?=(\\s{2,3})|((I|i)ssue:))", AllData$Description, perl = TRUE)), '[',1))
AllData$Customer <- trimws(sapply(regmatches(AllData$Description, gregexpr("(?<=((C|c)ustomer:)).*?(?=\\w).*?(?=(\\s{2,3})|((E|e)mail:))", AllData$Description, perl = TRUE)), '[',1))
#drop all the description column after getting what we want
drops <- "Description"
AllData <- AllData[,!(names(AllData) %in% drops)]

#Make Time to Failure Column
AllData$Time.to.Fail <- difftime(AllData$Submit.Date, AllData$Submit.Date[1], units = "hours")
#Make Times Between Failure column
AllData$Time.Between.Failure <- make.interFailures(AllData$Time.to.Fail)
write(AllData$Time.to.Fail, "data/Failure_Times.txt", sep = "\n")
write(AllData$Time.Between.Failure, "data/Time_Between_Failure.txt", sep = "\n")

#Using survival packages to plot Kaplan Maier
#Failure Times
Time.to.Fail.Surv <- Surv(AllData$Time.to.Fail)
Time.to.Fail.km <- survfit(Time.to.Fail.Surv ~ 1, conf.int = .95, conf.type = "plain")
ggsurvplot(Time.to.Fail.km, title = "Time to Failure", ggtheme = theme_gray(base_size = 15), conf.int = TRUE, color = "black", conf.int.fill = "black") + labs(x = "HOURS", y = "RELIABILITY")+coord_cartesian(expand = FALSE)

#Inter-Failure Times
Time.Between.Fails.Surv <- Surv(AllData$Time.Between.Failure)
Time.Between.Fails.km <- survfit(Time.Between.Fails.Surv ~ 1, conf.int = .95, conf.type = "plain")
ggsurvplot(Time.Between.Fails.km, title = "Times Between Failures", ggtheme = theme_gray(base_size = 15), conf.int = TRUE, color = "black", conf.int.fill = "black") + labs(x = "HOURS", y = "RELIABILITY")+coord_cartesian(expand = FALSE)
