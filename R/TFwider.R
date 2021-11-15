TFwider <- function(inputWider){
  # inputWider <- outputResults

  # weights, stem, and banne are ID columns
  # batch & total.responses will be dropped and the start/end dates will be combined with response.count when widened
  number_of_ID_columns <- 3

  inputWiderSubset <- inputWider[, -which(colnames(inputWider) == 'batch')] %>%
    setorder(., "start_date", "end_date")

  inputWiderSubset <- unique(inputWiderSubset)
  inputWiderSubset <- inputWiderSubset[!duplicated(inputWiderSubset[,1:5]),]
  # If you've managed to compute the same values more than once, this keeps just the first occurence

  outputStatsName <- outputName("Output - Stats", batch_time = batch_time)

  inputWiderSubset[is.na(inputWiderSubset)] <- 0
  inputWiderSubset <- unique(inputWiderSubset)

  inputWiderSubset <- pivot_wider(inputWiderSubset,
                                  names_from = c(start_date, end_date),
                                  values_from = response_count)

  numberOfPeriods <- ncol(inputWiderSubset) - number_of_ID_columns

  inputWiderSubset[is.na(inputWiderSubset)] <- 0

  inputColNames <- names(inputWiderSubset)

  inputDataColNames <- inputColNames[4:length(inputWiderSubset)] %>%
    gsub("_", " - ", .) %>%
    paste0(., ' - response count')

  names(inputWiderSubset)[(number_of_ID_columns+1):length(inputWiderSubset)] <- inputDataColNames

  # totalColNames <- gsub('response count', 'total responses', inputDataColNames)

  baseColNames <- c('unique', 'weighting_scheme', 'stem', 'banner', 'uniqueCrosstab', 'stemQ', 'bannerQ')

  baseColsEnd <- length(baseColNames)
  dataColsEnd <- baseColsEnd+length(inputDataColNames)
  # totalColsEnd <- dataColsEnd+length(totalColNames)


  inputWiderSubset$stem <- as.character(inputWiderSubset$stem)
  inputWiderSubset$banner <- as.character(inputWiderSubset$banner)
  inputWiderSubset$weights <- as.character(inputWiderSubset$weighting_scheme)
  # inputWiderSubset$unique <- paste(inputWiderSubset$stem, inputWiderSubset$banner, inputWiderSubset$weighting_scheme, sep=";")

  dataKeySubset <- dataKey[, c('Answer ID', 'Weighting Scheme', 'Question ID')]

  inputWiderSubset <- left_join(inputWiderSubset, dataKeySubset, by = c('stem' = 'Answer ID', 'weighting_scheme' = 'Weighting Scheme'))
  names(inputWiderSubset)[names(inputWiderSubset)=='Question ID'] <- 'stemQ'

  # This will cause issues with questions segmented by themselves with funky weightings
  # Could adjust the crosstab handler so that only stem questions are modified by groupIDlist (which needs to be done away with anyway)
  inputWiderSubset <- left_join(inputWiderSubset, dataKeySubset, by = c('banner' = 'Answer ID', 'weighting_scheme' = 'Weighting Scheme'))
  names(inputWiderSubset)[names(inputWiderSubset)=='Question ID'] <- 'bannerQ'

  inputWiderSubset$stemQ[which(is.na(inputWiderSubset$stemQ))] <- inputWiderSubset$stem[which(is.na(inputWiderSubset$stemQ))] # This is for named segments

  inputWiderSubset$unique <- paste(inputWiderSubset$stem, inputWiderSubset$banner, inputWiderSubset$weighting_scheme, sep=";")
  inputWiderSubset$uniqueCrosstab <- paste(inputWiderSubset$stemQ, inputWiderSubset$bannerQ, inputWiderSubset$weighting_scheme, sep=";")

  outputWider <- inputWiderSubset[, c(baseColNames, inputDataColNames)]
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

  # inputWiderSubset$weightAndStemAndBannerQ <- paste0(inputWiderSubset$weighting_scheme, ';', inputWiderSubset$stem, ';', inputWiderSubset$bannerQ)

  # totalTable <- aggregate(. ~ weightAndStemAndBannerQ, inputWiderSubset[, c('weightAndStemAndBannerQ', inputDataColNames)], sum)

  # colnames(totalTable)[2] <- gsub('response count', 'total responses', colnames(totalTable)[2])

  # outputWider <- left_join(inputWiderSubset, totalTable, by = 'weightAndStemAndBannerQ')

  # outputWider <- outputWider[ , -which(colnames(outputWider)=='weightAndStemAndBannerQ')]

  # names(outputWider) <- c(baseColNames, inputDataColNames, totalColNames)
  names(outputWider) <- c(baseColNames, inputDataColNames)

  # outputWider$stemQ[which(is.na(outputWider$stemQ))] <- 1

  return(outputWider)

}
