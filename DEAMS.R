library(xlsx)

# Cleaning the data
AllData <- read.xlsx(filePath, 1, startRow = 6, colIndex = c(1, 4, 5, 6, 7, 9))
AllData$Type <- gsub('DEAMS - ', '', AllData$Type)
AllData$Severity <- gsub('Severity ', '', AllData$Severity)
AllData <- AllData[order(AllData$Submit.Date), ]
# Regex to get the locations
AllData$Location <- trimws(sapply(regmatches(AllData$Description, gregexpr("(?<=Base:).*?(?=\\w).*?(?=(\\s{2,3})|((I|i)ssue:))", AllData$Description, perl = TRUE)), '[',1))