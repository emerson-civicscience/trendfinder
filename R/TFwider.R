TFwider <- function(inputWider){
  # inputWider <- outputResults

  ### NEED TO ADD ALL NA Qs to dataKey

  # weights, stem, and banne are ID columns
  # batch & total.responses will be dropped and the start/end dates will be combined with response.count when widened
  number_of_ID_columns <- 3

  inputWiderSubset <- inputWider[,2:7] %>%
    setorder(., "startDate", "endDate")

  inputWiderSubset <- unique(inputWiderSubset)
  inputWiderSubset <- inputWiderSubset[!duplicated(inputWiderSubset[,1:5]),]
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


  inputWiderSubset$stem <- as.character(inputWiderSubset$stem)
  inputWiderSubset$banner <- as.character(inputWiderSubset$banner)
  inputWiderSubset$weights <- as.character(inputWiderSubset$weights)
  inputWiderSubset$unique <- paste(inputWiderSubset$stem, inputWiderSubset$banner, inputWiderSubset$weights, sep=";")

  dataKeySubset <- dataKey[, c('Answer ID', 'Weighting Scheme', 'Question ID')]

  inputWiderSubset <- left_join(inputWiderSubset, dataKeySubset, by = c('stem' = 'Answer ID', 'weights' = 'Weighting Scheme'))
  names(inputWiderSubset)[names(inputWiderSubset)=='Question ID'] <- 'stemQ'

  dataKeyUSadults <- dataKey[which(dataKey$`Weighting Scheme` == "USadultWeighting"), ] %>%
    .[, c('Answer ID', 'Question ID')]

  inputWiderSubset <- left_join(inputWiderSubset, dataKeyUSadults, by = c('banner' = 'Answer ID')) # Use the 'basic' version of the dataKey to populate banner Qs
  # This will cause issues with questions segmented by themselves with funky weightings
  # Could adjust the crosstab handler so that only stem questions are modified by groupIDlist (which needs to be done away with anyway)
  names(inputWiderSubset)[names(inputWiderSubset)=='Question ID'] <- 'bannerQ'

  inputWiderSubset$uniqueCrosstab <- paste(inputWiderSubset$stemQ, inputWiderSubset$bannerQ, inputWiderSubset$weights, sep=";")

  inputWiderSubset <- inputWiderSubset[, c(baseColNames, inputDataColNames)]
  # totalsTable <- matrix(0L, nrow=nrow(inputWiderSubset), ncol=numberOfPeriods)
  #
  # inputWiderSubset <- cbind(inputWiderSubset, totalsTable) %>%
  #   as_tibble(.)



  # uniqueWeightAndStemAndBannerQTable <- inputWiderSubset[, c('weights', 'stem', 'bannerQ')] %>%
  #   unique(.)
  #
  #   uniqueCrosstabs <- inputWiderSubset$uniqueCrosstab %>%
  #     unique()
  #
  #   toTotalTable <- inputWiderSubset[, c('uniqueCrosstab', inputDataColNames)]

  totalTable <- aggregate(. ~ uniqueCrosstab, inputWiderSubset[, c('uniqueCrosstab', inputDataColNames)], sum)

  outputWider <- left_join(inputWiderSubset, totalTable, by = 'uniqueCrosstab')

  names(outputWider) <- c(baseColNames, inputDataColNames, totalColNames)

  # outputWider$stemQ[which(is.na(outputWider$stemQ))] <- 1



  return(outputWider)

}
