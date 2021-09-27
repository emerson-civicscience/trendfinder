TFwider <- function(inputWider){
  # inputWider <- outputResults

  # weights, stem, and banne are ID columns
  # batch & total.responses will be dropped and the start/end dates will be combined with response.count when widened
  number_of_ID_columns <- 3

  inputWiderSubset <- inputWider[,2:7] %>%
    setorder(., "startDate", "endDate")

  inputWiderSubset <- unique(inputWiderSubset)
  inputWiderSubset <- inputWiderSubset[!duplicated(inputWiderSubset[,1:4]),]
  # If you've managed to compute the same values more than once, this keeps just the first occurence

  outputStatsName <- outputName("Output - Stats", batchTime = batchTime)

  inputWiderSubset[is.na(inputWiderSubset)] <- 0
  inputWiderSubset <- unique(inputWiderSubset)

  inputWiderSubset <- pivot_wider(inputWiderSubset,
                                 names_from = c(startDate, endDate),
                                 values_from = response.count)

  numberOfPeriods <- ncol(inputWiderSubset) - number_of_ID_columns

  inputWiderSubset[is.na(inputWiderSubset)] <- 0

  inputColNames <- names(inputWiderSubset)

  inputDataColNames <- inputColNames[4:length(inputWiderSubset)] %>%
    gsub("_", " - ", .) %>%
    paste0(., ' - response count')

  names(inputWiderSubset)[(number_of_ID_columns+1):length(inputWiderSubset)] <- inputDataColNames

  totalColNames <- gsub('response count', 'total responses', inputDataColNames)

  baseColNames <- c('unique', 'weights', 'stem', 'banner', 'uniqueCrosstab', 'stemQ', 'bannerQ')

  baseColsEnd <- length(baseColNames)
  dataColsEnd <- baseColsEnd+length(inputDataColNames)
  totalColsEnd <- dataColsEnd+length(totalColNames)

  inputWiderSubset$unique <- paste(inputWiderSubset$stem, inputWiderSubset$banner, inputWiderSubset$weights, sep=";")
  inputWiderSubset$stem <- as.character(inputWiderSubset$stem)
  inputWiderSubset$banner <- as.character(inputWiderSubset$banner)

  dataKeySubset <- dataKey[, c('Answer ID', 'Question ID')]

  inputWiderSubset <- left_join(inputWiderSubset, dataKeySubset, by = c('stem' = 'Answer ID'))
  names(inputWiderSubset)[names(inputWiderSubset)=='Question ID'] <- 'stemQ'

  inputWiderSubset <- left_join(inputWiderSubset, dataKeySubset, by = c('banner' = 'Answer ID'))
  names(inputWiderSubset)[names(inputWiderSubset)=='Question ID'] <- 'bannerQ'

  inputWiderSubset$uniqueCrosstab <- paste(inputWiderSubset$stemQ, inputWiderSubset$bannerQ, sep=";")

  inputWiderSubset <- inputWiderSubset[, c(baseColNames, inputDataColNames)]
  totalsTable <- matrix(0L, nrow=nrow(inputWiderSubset), ncol=numberOfPeriods)

  inputWiderSubset <- cbind(inputWiderSubset, totalsTable) %>%
    as_tibble(.)

  names(inputWiderSubset) <- c(baseColNames, inputDataColNames, totalColNames)

  uniqueStemAndBannerQTable <- inputWiderSubset[, c('stem', 'bannerQ')] %>%
    unique(.)


  for(totalLoop in seq_len(nrow(uniqueStemAndBannerQTable))){
    totalTable <- inputWiderSubset[which(inputWiderSubset$stem == as.character(uniqueStemAndBannerQTable[totalLoop, 1])), ] %>%
      .[which(.$bannerQ == as.numeric(uniqueStemAndBannerQTable[totalLoop,2])), ]

    totalTable[(dataColsEnd+1):ncol(totalTable)] <- lapply(totalTable[(baseColsEnd+1):dataColsEnd], sum)

    if(totalLoop==1){
      outputWider <- totalTable
    } else{
      outputWider <- rbind(outputWider, totalTable)
    }

  }

  outputWider$stemQ[which(is.na(outputWider$stemQ))] <- 1

  return(outputWider)

}
