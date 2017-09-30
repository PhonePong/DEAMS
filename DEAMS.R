library(xlsx)
library(ggplot2)
library(ggpubr)
library(survMisc)
library(survival)
library(survminer)
library(data.table)
library(grid)
library(gridExtra)

source("misc.R")

# Getting the data
AllData <- read.xlsx("data/DEAMS_SERENA.xlsx", 1, startRow = 6, colIndex = c(1, 4, 5, 6, 7, 9))
AllData$Type <- gsub('DEAMS - ', '', AllData$Type)
AllData$Severity <- gsub('Severity ', '', AllData$Severity)
AllData <- AllData[order(AllData$Submit.Date), ]

#===================================================================================
# Regex to get the locations and customer (after this should drop description)
# Who they are
AllData$Customer <- trimws(sapply(regmatches(AllData$Description, gregexpr("(?<=((C|c)ustomer:)).*?(?=\\w).*?(?=(\\s{2,3})|((E|e)mail:))", AllData$Description, perl = TRUE)), '[',1))
# Where they are
AllData$Location <- trimws(sapply(regmatches(AllData$Description, gregexpr("(?<=Base:).*?(?=\\w).*?(?=(\\s{2,3})|((I|i)ssue:))", AllData$Description, perl = TRUE)), '[',1))
# tell them to stop complaining!
AllData$Phone <- trimws(sapply(regmatches(AllData$Description, gregexpr("(?<=((P|p)hone:)).*?(?=\\w).*?(?=(\\s{2,3})|((B|b)ase:))", AllData$Description, perl = TRUE)), '[',1))
#==================================================================================

#output to xlsx file (i need to look at the data again)
#write.xlsx(AllData, file = "data/output.xlsx", sheetName = "output2")
#drop all the description column after getting what we want
drops <- "Description"
AllData <- AllData[,!(names(AllData) %in% drops)]

#========================== grep commands to clean locations ====================
clean <- list(c("wp|wright", "trans|scott", "socom|mac", "san a|lack|jbsa", "trav", "minot", "mcgui|MDL", "mcconn", "dill", "scott", "max", "fair", "\\bgf\\b|forks", "ells", "dov", "dfas", "little|lra", "^$", "deams", "pope"), c("Wright P. AFB", "Scott AFB", "MacDill AFB", "JB SA", "Travis AFB", "Minot AFB", "JB MDL", "McConnell AFB", "MacDill AFB", "Scott AFB", "Maxwell AFB", "Fairchild AFB", "Grand F. AFB", "Ellsworth AFB", "Dover AFB", "DFAS", "Little R. AFB", "EMPTY", "DEAMS", "Pope AFB"))
# use custom function to clean the location column
AllData$Location <- clean.column(AllData$Location, clean)
#get all limestone, but not little rock
# but limestone is a town in OHIO!! alltogether need dfas
#grep("(\\bli\\b)|lime", AllData$Location, ignore.case = TRUE, value = TRUE)
#get little rock
#================================================================================

#=========== Create New Data frame ====================================================================================================
# subset data by selecting only rows where there are more than 3 occurences of each location 
t <- table(AllData$Location)
NewData <- AllData[AllData$Location %in% names(t[t >= 3]), ]

# sort by location, and then by date
NewData <- NewData[order(NewData$Location, NewData$Submit.Date), ]
#=======================================================================================================================================

#======================================================= Time operations by group ======================================================
# Inter-failures using data.table package
# need to coerce for the package features to work
setDT(NewData)
setkey(NewData, Location)
NewData[ , Time.Between.Fails := c(0, difftime(Submit.Date[-1L], Submit.Date[-.N], units = "hours")), by = Location]
# coerce back to dataframe for normal operations
setDF(NewData)

# cumulative time-to-fail
NewData$Time.To.Fail <- unlist(by(NewData, NewData$Location, function(x) difftime(x$Submit.Date, x$Submit.Date[1], units= "hours")))
#=======================================================================================================================================

#========================= Not using these ========================================================================================
# #Make Time to Failure Column
# AllData$Time.to.Fail <- difftime(AllData$Submit.Date, AllData$Submit.Date[1], units = "hours")
# #Make Times Between Failure column
# AllData$Time.Between.Failure <- make.interFailures(AllData$Time.to.Fail)
#write(AllData$Time.to.Fail, "data/Failure_Times.txt", sep = "\n")
#write(AllData$Time.Between.Failure, "data/Time_Between_Failure.txt", sep = "\n")
#==================================================================================================================================


#Using survival packages to plot Kaplan Meier
#Failure Times
Time.to.Fail.Surv <- Surv(AllData$Time.to.Fail)
Time.to.Fail.km <- survfit(Time.to.Fail.Surv ~ 1, conf.int = .95, conf.type = "plain")
#UNRELIABILITY
p1 <- ggsurvplot(Time.to.Fail.km, ggtheme = theme_gray(base_size = 20), fun = "event", conf.int = TRUE, color = "black", conf.int.fill = "black") + ggtitle("Kaplan Meier: Time to Failure") + labs(x = "HOURS", y = "UNRELIABILITY (CDF)")+coord_cartesian(expand = FALSE) 
#RELIABILITY
p2 <- ggsurvplot(Time.to.Fail.km, ggtheme = theme_gray(base_size = 20), conf.int = TRUE, color = "black", conf.int.fill = "black")+ggtitle("Kaplan Meier: Time to Failure") + labs(x = "HOURS", y = "RELIABILITY")+coord_cartesian(expand = FALSE)


#Inter-Failure Times
p3 <- ggplot(data=AllData, aes(Time.to.Fail,Time.Between.Failure))+ggtitle("Time Between Failures")+ labs(x ="HOURS", y = "TIME BETWEEN FAILURES") + theme(axis.title = element_text(size=22), axis.text=element_text(size=16)) + geom_point()+geom_line()+ theme(plot.title = element_text(size = 24))
                                                                                                                                    
# Time.Between.Fails.Surv <- Surv(AllData$Time.Between.Failure)
# Time.Between.Fails.km <- survfit(Time.Between.Fails.Surv ~ 1, conf.int = .95, conf.type = "plain")
# ggsurvplot(Time.Between.Fails.km, title = "Kaplan Meier: Times Between Failures", ggtheme = theme_gray(base_size = 15), conf.int = TRUE, color = "black", conf.int.fill = "black") + labs(x = "HOURS", y = "RELIABILITY")+coord_cartesian(expand = FALSE)

#==========================================================
#Multiple ggplots on 1 page 
#from ggpubr package
ggarrange(p1$plot, p2$plot, p3, ncol = 2, nrow = 2) %>% ggexport(filename = "data/test.png", width = 1080, height = 900)
#OR 
#from gridExtra package
# grid.arrange(p1$plot,p2$plot,p3, ncol = 2) 
#==========================================================