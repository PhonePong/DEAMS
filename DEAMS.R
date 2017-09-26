library(xlsx)

# Getting the data
AllData <- read.xlsx(filePath, 1, startRow = 6, colIndex = c(1, 4, 5, 6, 7, 9))
AllData$Type <- gsub('DEAMS - ', '', AllData$Type)
AllData$Severity <- gsub('Severity ', '', AllData$Severity)
AllData <- AllData[order(AllData$Submit.Date), ]
# Regex to get the locations and customer (after this should drop description)
AllData$Location <- trimws(sapply(regmatches(AllData$Description, gregexpr("(?<=Base:).*?(?=\\w).*?(?=(\\s{2,3})|((I|i)ssue:))", AllData$Description, perl = TRUE)), '[',1))
AllData$Customer <- trimws(sapply(regmatches(AllData$Description, gregexpr("(?<=((C|c)ustomer:)).*?(?=\\w).*?(?=(\\s{2,3})|((E|e)mail:))", AllData$Description, perl = TRUE)), '[',1))
#drop all the description data after getting what we want
drops <- "Description"
AllData <- AllData[,!(names(AllData) %in% drops)]