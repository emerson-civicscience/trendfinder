TFformat <- function(inputFormat){

inputFormat <- unique(inputFormat)

RCsuffix <- 'response count'
TRsuffix <- 'total responses'
sigSuffix <- ' significance'

if(is.null(time_period)){
  time_period <- names(inputFormat)[grep(RCsuffix, names(inputFormat))] %>%
    gsub(' - response count', '', .)
}

numberOfPeriods <- length(time_period)

responseColNumbers <- grep(RCsuffix, names(inputFormat))
totalColNumbers <- grep(TRsuffix, names(inputFormat))
# sigColNumbers <- grep(sigSuffix, names(inputFormat))

responseCols <- names(inputFormat)[responseColNumbers]
totalCols <- names(inputFormat)[totalColNumbers]
# sigCols <- names(inputFormat)[sigColNumbers]

percentTable <- subset(inputFormat, select = c("unique", names(inputFormat)[responseColNumbers]))
percentTable[, 2:dim(percentTable)[2]] <- 0
names(percentTable) <- c("unique", time_period)

percentDiffTable <- percentTable[, 1:(ncol(percentTable)-1)]

for(percentLoop in 1:numberOfPeriods){
  numerator <- inputFormat[,responseColNumbers[percentLoop]]
  denominator <- inputFormat[,totalColNumbers[percentLoop]]
  percent <- round(numerator/denominator, digits = 3)
  percentTable[, 1+percentLoop] <- percent
}

inputFormat <- merge(inputFormat, percentTable, by="unique")

if(numberOfPeriods == 1){
  percentDiffTable <- NULL
  percentDiffCols <- NULL
} else {

  percentDiffLoopVector <- seq.int(2, (numberOfPeriods))
  percentDiffCols <- paste0("Period ", percentDiffLoopVector-1, " - Period ", percentDiffLoopVector)

  for(percentDiffLoop in percentDiffLoopVector){

    names(percentDiffTable) <- c("unique", percentDiffCols)
    secondPeriod <- percentTable[,percentDiffLoop+1]
    firstPeriod <- percentTable[,percentDiffLoop]
    percentDiff <- round((secondPeriod - firstPeriod)/firstPeriod, digits = 3)
    percentDiffTable[, percentDiffLoop] <- percentDiff
  }

  inputFormat <- merge(inputFormat, percentDiffTable, by="unique")

}

outputStemID <- subset(inputFormat, select = stem)
outputStemID <- inputFormat$stem %>%
  as.data.frame()

names(outputStemID) <- 'Answer ID'

outputBannerID <- inputFormat$banner %>%
  as.data.frame()
names(outputBannerID) <- 'Answer ID'


outputStemTable <- left_join(outputStemID, dataKey, by='Answer ID')
outputBannerTable <- left_join(outputBannerID, dataKey, by='Answer ID')


baseCols <- c("Unique Row ID", "Unique Crosstab ID",
              "Stem Client Q Flag", "Stem Q Account ID",
              "Banner Client Q Flag", "Banner Q Account ID",
              "Answer Flag", "Stats Flag", "Chart",  "Manual Flag",
              "Stem QID", "Stem QText", "Stem ID (answers)", "Stem Name",
              "Banner QID", "Banner QText", "Banner ID (answers)", "Banner Name")

baseTable <- matrix(0L, nrow=nrow(inputFormat), ncol=length(baseCols))
colnames(baseTable) <- baseCols
baseTable <- as_tibble(baseTable)

baseTable$`Unique Row ID` <- paste0(outputStemTable$`Answer ID`,";",outputBannerTable$`Answer ID`)
baseTable$`Unique Crosstab ID` <- paste0(outputStemTable$`Question ID`,";",outputBannerTable$`Question ID`)
baseTable$`Stem Client Q Flag` <- outputStemTable$Sponsored
baseTable$`Stem Q Account ID` <- outputStemTable$Account
baseTable$`Banner Client Q Flag` <- outputBannerTable$Sponsored
baseTable$`Banner Q Account ID` <- outputBannerTable$Account
baseTable$`Answer Flag` <- as.numeric(outputStemTable$`Answer Flag`) + as.numeric(outputBannerTable$`Answer Flag`)
baseTable$`Stem QID` <- outputStemTable$`Question ID`
baseTable$`Stem QText` <- outputStemTable$`Question Text`
baseTable$`Stem ID (answers)` <- outputStemTable$`Answer ID`
baseTable$`Stem Name` <- outputStemTable$`Answer Text`
baseTable$`Banner QID` <- outputBannerTable$`Question ID`
baseTable$`Banner QText` <- outputBannerTable$`Question Text`
baseTable$`Banner ID (answers)` <- outputBannerTable$`Answer ID`
baseTable$`Banner Name` <- outputBannerTable$`Answer Text`

inputFormat <- inputFormat[,c(1,7:ncol(inputFormat))] %>%
  merge(baseTable, ., by.x="Unique Row ID", by.y=c("unique"))

columnOrder <- c(baseCols, time_period, percentDiffCols, responseCols, totalCols)

outputFormatted <- inputFormat[, columnOrder]
outputFormatted$`Stem QText` <- as.character(outputFormatted$`Stem QText`) %>%
  utf8_normalize(., map_quote = TRUE)
outputFormatted$`Banner QText` <- as.character(outputFormatted$`Banner QText`) %>%
  utf8_normalize(., map_quote = TRUE)
outputFormatted$`Stem Name` <- as.character(outputFormatted$`Stem Name`, map_quote = TRUE) %>%
  utf8_normalize(., map_quote = TRUE)
outputFormatted$`Banner Name` <- as.character(outputFormatted$`Banner Name`, map_quote = TRUE) %>%
  utf8_normalize(., map_quote = TRUE)

outputFormattedName <- outputName("Output - Responses - Formatted", batchTime = batchTime)
saveRDS(outputFormatted, file = outputFormattedName)

write.table(outputFormatted, file=paste0(outputFormattedName,'.tsv'), quote=TRUE, sep='\t', row.names=FALSE)

return(outputFormatted)

}
