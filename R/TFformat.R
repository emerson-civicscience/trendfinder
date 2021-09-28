TFformat <- function(inputFormat){

  # inputFormat <- outputWider

  inputFormat <- unique(inputFormat)

  inputFormatNumberOfCols1 <- ncol(inputFormat)

  RCsuffix <- 'response count'
  TRsuffix <- 'total responses'

  if(is.null(time_period)){
    time_period <- names(inputFormat)[grep(RCsuffix, names(inputFormat))] %>%
      gsub(' - response count', '', .)
  }

  numberOfPeriods <- length(time_period)

  responseColNumbers <- grep(RCsuffix, names(inputFormat))
  totalColNumbers <- grep(TRsuffix, names(inputFormat))

  responseCols <- names(inputFormat)[responseColNumbers]
  totalCols <- names(inputFormat)[totalColNumbers]

  inputFormatNumberOfMetadataCols <- inputFormatNumberOfCols1 - length(responseColNumbers) - length(totalColNumbers)

  percentTableCols <- lapply(c("unique$", names(inputFormat)[responseColNumbers]), grep, x=colnames(inputFormat)) %>%
    unlist()

  percentTable <- inputFormat[, percentTableCols]
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

  inputFormatNumberOfCols2 <- ncol(inputFormat)

  dataKeyColsWanted <- c('Answer ID', 'Weighting Scheme', 'Answer Text', 'Answer Flag', 'Question Text', 'Sponsored', 'Account')

  inputFormat <- left_join(inputFormat, dataKey[, dataKeyColsWanted], by = c("stem" = "Answer ID", "weights" = "Weighting Scheme"))

  inputFormatNumberOfCols3 <- ncol(inputFormat)

  colnames(inputFormat)[(inputFormatNumberOfCols2+1):inputFormatNumberOfCols3] <- c("Stem Name", "Stem Answer Flag", "Stem QText",
                                                                                    "Stem Client Q Flag", "Stem Q Account ID")

  dataKeyUSadultsColsWanted <- c('Answer ID', 'Answer Text', 'Answer Flag', 'Question Text', 'Sponsored', 'Account')

  dataKeyUSadults <- dataKey[which(dataKey$`Weighting Scheme` == "USadultWeighting"), ] %>%
    .[, dataKeyUSadultsColsWanted]

  inputFormat <- left_join(inputFormat, dataKeyUSadults, by = c('banner' = 'Answer ID')) # Use the 'basic' version of the dataKey to populate banner Qs

  inputFormatNumberOfCols4 <- ncol(inputFormat)

  colnames(inputFormat)[(inputFormatNumberOfCols3+1):inputFormatNumberOfCols4] <- c("Banner Name", "Banner Answer Flag", "Banner QText",
                                                                                    "Banner Client Q Flag", "Banner Q Account ID")

  colnames(inputFormat)[1:inputFormatNumberOfMetadataCols] <- c("Unique Row ID", "Weights", "Stem ID (answers)", "Banner ID (answers)", "Unique Crosstab ID", "Stem QID", "Banner QID")

  inputFormat$`Answer Flag` <- as.numeric(inputFormat$`Stem Answer Flag`) + as.numeric(inputFormat$`Banner Answer Flag`)
  inputFormat$`Stats Flag` <- NA
  inputFormat$`Chart` <- NA
  inputFormat$`Manual Flag` <- NA

  inputFormatData <- inputFormat[, c(1, (inputFormatNumberOfMetadataCols+1):(inputFormatNumberOfCols2))]

  baseCols <- c("Unique Row ID", "Weights", "Unique Crosstab ID",
                "Stem Client Q Flag", "Stem Q Account ID",
                "Banner Client Q Flag", "Banner Q Account ID",
                "Answer Flag", "Stats Flag", "Chart",  "Manual Flag",
                "Stem QID", "Stem QText", "Stem ID (answers)", "Stem Name",
                "Banner QID", "Banner QText", "Banner ID (answers)", "Banner Name")



  baseTable <- inputFormat[, baseCols]

  inputFormat <- merge(baseTable, inputFormatData)

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
