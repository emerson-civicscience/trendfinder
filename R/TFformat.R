TFformat <- function(inputFormat){

  # inputFormat <- TFwider(readRDS('~/TrendFinder/Outputs/2021-11-22/Output Results - Batch Time 2021-11-22 20:02:15 EST.rds'))
  # inputFormat <- outputWider
  # inputFormat <- readRDS('~/TrendFinder/Outputs/2021-12-02/All Results - Batch Time 2021-12-02 11_24_07 EST EDT.rds') %>%
    # TFwider()

  inputFormat <- inputFormat[!duplicated(inputFormat), ]

  RCsuffix = 'response count'
  TRsuffix = 'total responses'


  if(is.null(time_period)){
    time_period <- names(inputFormat)[grep(RCsuffix, names(inputFormat))] %>%
      gsub(' - response count', '', .)
  }
  numberOfPeriods <- length(time_period)

  outputFormatted <- inputFormat
  # outputFormatted$`Stem Answer Group ID` <- NA
  outputFormatted$`Stem Answer Group Label` <- NA
  # outputFormatted$`Stem Answer Label` <- NA
  # outputFormatted$`Banner Answer Group ID` <- NA
  outputFormatted$`Banner Answer Group Label` <- NA
  # outputFormatted$`Banner Answer Label` <- NA

  answer_grouping_table <- TFanswerGroupingHandler(inputFormat,
                                                   RCsuffix = RCsuffix,
                                                   numberOfPeriods = numberOfPeriods)

  outputFormatted <- rbind(outputFormatted, answer_grouping_table[, colnames(outputFormatted)])

  # groupsWanted <- c(10857, 11405, 11238, 11246, 15222, 10769)
  # stemGroups <- which(outputFormatted$`Stem Answer Group ID` %in% groupsWanted)
  # bannerGroups <- which(outputFormatted$`Banner Answer Group ID` %in% groupsWanted)
  # groups <- unique(c(stemGroups, bannerGroups))
  # outputFormatted <- outputFormatted[groups, ]

  outputFormatted$weighting_stem_bannerQ_and_answer_groups <- paste(outputFormatted$weighting_scheme,
                                                                    outputFormatted$stem,
                                                                    outputFormatted$bannerQ,
                                                                    # These come from TFanswerGroupingHandler pre-concatenated now
                                                                    # outputFormatted$`Stem Answer Group ID`,
                                                                    # outputFormatted$`Banner Answer Group ID`,
                                                                    sep=";")



  responseColNumbers <- grep(RCsuffix, colnames(outputFormatted))
  responseCols <- colnames(outputFormatted)[responseColNumbers]
  totalCols <- gsub(RCsuffix, TRsuffix, responseCols)


  response_aggregates <- aggregate(. ~unique,
                                   outputFormatted[, c('unique', responseCols)], sum)


  ### This does the equivalent of "overall percent" on an InsightStore crosstab
  # answer_grouping_overall_total_aggregates <- aggregate(. ~uniqueCrosstab,
  #                                                       answer_grouping_table[, c('uniqueCrosstab', responseCols)], sum)
  # colnames(answer_grouping_overall_total_aggregates) <- gsub("response count", "overall total responses",
  #                                                            colnames(total_aggregates))

  total_aggregates <- aggregate(. ~weighting_stem_bannerQ_and_answer_groups,
                                                outputFormatted[, c('weighting_stem_bannerQ_and_answer_groups', responseCols)], sum)
  colnames(total_aggregates) <- gsub(RCsuffix, TRsuffix,
                                                     colnames(total_aggregates))




  outputFormatted <- left_join(response_aggregates,
                               outputFormatted[, -which(colnames(outputFormatted) %in% responseCols)],
                               by = 'unique')

  outputFormatted <- left_join(outputFormatted,
                               total_aggregates,
                               by = 'weighting_stem_bannerQ_and_answer_groups')

  outputFormatted <- outputFormatted[, -which(colnames(outputFormatted) == "weighting_stem_bannerQ_and_answer_groups")]
  outputFormatted <- outputFormatted[!duplicated(outputFormatted$unique), ]



  percentTableCols <- lapply(c("unique$", responseCols), grep, x=colnames(outputFormatted)) %>%
    unlist()

  percentTable <- outputFormatted[, percentTableCols]
  percentTable[, 2:dim(percentTable)[2]] <- NA # This was '0', changed to NA. Not sure if that'll be a problem
  colnames(percentTable) <- c("unique", time_period)

  percentDiffTable <- percentTable[, 1:(ncol(percentTable)-1)]

  for(percentLoop in 1:numberOfPeriods){
    numerator <- outputFormatted[, which(colnames(outputFormatted) %in% responseCols)[percentLoop]]
    denominator <- outputFormatted[, which(colnames(outputFormatted) %in% totalCols)[percentLoop]]
    percent <- round(numerator/denominator, digits = 3)
    percentTable[, 1+percentLoop] <- percent
  }

  outputFormatted <- merge(outputFormatted, percentTable, by="unique")

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

    outputFormatted <- merge(outputFormatted, percentDiffTable, by="unique")

  }

  dataKeySubset <- dataKey[, c('Answer ID', 'Weighting Scheme', 'Answer Text', 'Answer Flag', 'Question Text', 'Sponsored', 'Account')]

  outputFormatted <- left_join(outputFormatted, dataKeySubset, by = c('stem' = 'Answer ID',
                                                                       'weighting_scheme' = 'Weighting Scheme'))

  colnames(outputFormatted)[which(colnames(outputFormatted) %in% c("Answer Text",
                                                                   "Answer Flag",
                                                                   "Question Text",
                                                                   "Sponsored",
                                                                   "Account"))] <- c("Stem Name",
                                                                                     "Stem Answer Flag",
                                                                                     "Stem QText",
                                                                                     "Stem Client Q Flag",
                                                                                     "Stem Q Account ID")




  outputFormatted <- left_join(outputFormatted, dataKeySubset, by = c('banner' = 'Answer ID',
                                                                       'weighting_scheme' = 'Weighting Scheme'))

  colnames(outputFormatted)[which(colnames(outputFormatted) %in% c("Answer Text",
                                                                   "Answer Flag",
                                                                   "Question Text",
                                                                   "Sponsored",
                                                                   "Account"))] <- c("Banner Name",
                                                                                     "Banner Answer Flag",
                                                                                     "Banner QText",
                                                                                     "Banner Client Q Flag",
                                                                                     "Banner Q Account ID")

  colnames(outputFormatted)[which(colnames(outputFormatted) %in% c("unique", "weighting_scheme",
                                                                   "stem",
                                                                   "banner",
                                                                   "uniqueCrosstab",
                                                                   "stemQ",
                                                                   "bannerQ"))] <- c("Unique Row ID",
                                                                                     "Weights",
                                                                                     "Stem ID (answers)",
                                                                                     "Banner ID (answers)",
                                                                                     "Unique Crosstab ID",
                                                                                     "Stem QID",
                                                                                     "Banner QID")

  outputFormatted$`Answer Flag` <- as.numeric(outputFormatted$`Stem Answer Flag`) + as.numeric(outputFormatted$`Banner Answer Flag`)

  outputFormatted$`Stats Flag` <- NA
  outputFormatted$`Chart` <- NA
  outputFormatted$`Manual Flag` <- NA

  # baseCols <- c("Unique Row ID", "Weights", "Unique Crosstab ID",
  #               "Stem Client Q Flag", "Stem Q Account ID",
  #               "Banner Client Q Flag", "Banner Q Account ID",
  #               "Answer Flag", "Stats Flag", "Chart",  "Manual Flag",
  #               "Stem QID", "Stem Answer Group ID", "Stem Answer Group Label", "Stem QText", "Stem ID (answers)", "Stem Name",
  #               "Banner QID", "Banner Answer Group ID", "Banner Answer Group Label", "Banner QText", "Banner ID (answers)", "Banner Name")

  stem_group_rows <- which(!is.na(outputFormatted$`Stem Answer Group Label`))
  outputFormatted$`Stem QText`[stem_group_rows] <- paste0(outputFormatted$`Stem QText`[stem_group_rows],
                                                          " - ",
                                                          outputFormatted$`Stem Answer Group Label`[stem_group_rows])

  banner_group_rows <- which(!is.na(outputFormatted$`Banner Answer Group Label`))
  outputFormatted$`Banner QText`[banner_group_rows] <- paste0(outputFormatted$`Banner QText`[banner_group_rows],
                                                          " - ",
                                                          outputFormatted$`Banner Answer Group Label`[banner_group_rows])

  baseCols <- c("Unique Row ID", "Weights", "Unique Crosstab ID",
                "Stem Client Q Flag", "Stem Q Account ID",
                "Banner Client Q Flag", "Banner Q Account ID",
                "Answer Flag", "Stats Flag", "Chart",  "Manual Flag",
                "Stem QID", "Stem QText", "Stem ID (answers)", "Stem Name",
                "Banner QID", "Banner QText", "Banner ID (answers)", "Banner Name")

  outputFormatted <- outputFormatted[, c(baseCols, time_period, percentDiffCols, responseCols, totalCols)]


  outputFormatted$`Stem QText` <- as.character(outputFormatted$`Stem QText`) %>%
    utf8_normalize(., map_quote = TRUE)
  outputFormatted$`Banner QText` <- as.character(outputFormatted$`Banner QText`) %>%
    utf8_normalize(., map_quote = TRUE)
  outputFormatted$`Stem Name` <- as.character(outputFormatted$`Stem Name`, map_quote = TRUE) %>%
    utf8_normalize(., map_quote = TRUE)
  outputFormatted$`Banner Name` <- as.character(outputFormatted$`Banner Name`, map_quote = TRUE) %>%
    utf8_normalize(., map_quote = TRUE)


  outputFormatted <- outputFormatted[!duplicated(outputFormatted[, c('Unique Row ID')]),]

  outputFormattedName <- outputName("Output - Responses - Formatted", batch_time = batch_time)
  saveRDS(outputFormatted, file = outputFormattedName)

  write.table(outputFormatted, file=paste0(outputFormattedName,'.tsv'), quote=TRUE, sep='\t', row.names=FALSE)

  return(outputFormatted)

}

