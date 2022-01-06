TFwider <- function(inputWider){
  # inputWider <- all_results
  # inputWider <- readRDS('~/TrendFinder/Outputs/2021-12-17/All Results - Batch Time 2021-12-17 10_56_54 EST - groups removed.rds')


  # weights, stem, and banner are ID columns
  # batch & total.responses will be dropped and the start/end dates will be combined with response.count when widened
  number_of_ID_columns <- 3

  input_colnames_wanted <- c("start_date", "end_date", "stem", "banner", "weighting_scheme", "response_count")

  inputWiderSubset <- inputWider[, input_colnames_wanted] %>%
    setorder(., "start_date", "end_date")

  # If you've managed to compute the same values more than once, this keeps just the first occurenc
  inputWiderSubset <- inputWiderSubset[!duplicated(inputWiderSubset[,1:5]),]


  inputWiderSubset$response_count[which(is.na(inputWiderSubset$response_count))] <- 0

  inputWiderSubset <- pivot_wider(inputWiderSubset,
                                  names_from = c(start_date, end_date),
                                  values_from = response_count)

  numberOfPeriods <- ncol(inputWiderSubset) - number_of_ID_columns

  inputColNames <- names(inputWiderSubset)

  inputDataColNames <- inputColNames[(number_of_ID_columns+1):ncol(inputWiderSubset)] %>%
    gsub("_", " - ", .) %>%
    paste0(., ' - response count')

  names(inputWiderSubset)[(number_of_ID_columns+1):length(inputWiderSubset)] <- inputDataColNames

  baseColNames <- c('weighting_scheme', 'stemQ', 'stem', 'bannerQ', 'banner')

  inputWiderSubset$stem <- as.character(inputWiderSubset$stem)
  inputWiderSubset$banner <- as.character(inputWiderSubset$banner)
  inputWiderSubset$weights <- as.character(inputWiderSubset$weighting_scheme)

  # dataKey used to have groupings in `Answer ID` and `Question ID`
  # dataKeySubset <- dataKey[-grep(',', dataKey$`Answer ID`), ]
  # dataKeySubset <- dataKeySubset[-grep(',', dataKeySubset$`Question ID`), ]
  dataKeySubset <- dataKey[, c('Answer ID', 'Question ID')]
  dataKeySubset <- dataKeySubset[!duplicated(dataKeySubset), ]

  inputWiderSubset <- left_join(inputWiderSubset, dataKeySubset, by = c('stem' = 'Answer ID'))
  names(inputWiderSubset)[names(inputWiderSubset)=='Question ID'] <- 'stemQ'

  # This may cause issues with questions segmented by themselves with funky weightings
  inputWiderSubset <- left_join(inputWiderSubset, dataKeySubset, by = c('banner' = 'Answer ID'))
  names(inputWiderSubset)[names(inputWiderSubset)=='Question ID'] <- 'bannerQ'

  inputWiderSubset$stemQ[which(is.na(inputWiderSubset$stemQ))] <- inputWiderSubset$stem[which(is.na(inputWiderSubset$stemQ))] # This is for named segments

  # Originally made a key (unique row identifier) plus a unique crosstab identifier. Abandoned later
  # inputWiderSubset$unique <- paste(inputWiderSubset$stem, inputWiderSubset$banner, inputWiderSubset$weighting_scheme, sep=";")
  # inputWiderSubset$uniqueCrosstab <- paste(inputWiderSubset$stemQ, inputWiderSubset$bannerQ, inputWiderSubset$weighting_scheme, sep=";")

  outputWider <- inputWiderSubset[, c(baseColNames, inputDataColNames)]

  names(outputWider) <- c(baseColNames, inputDataColNames)

  return(outputWider)

}
