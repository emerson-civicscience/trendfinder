TFformat <- function(inputFormat, 
                     time_period = NULL, 
                     segment_names = segment_names, 
                     use_default_answer_flag = FALSE){

  # inputFormat <- outputWider

  inputFormat <- inputFormat[!duplicated(inputFormat), ]

  RCsuffix = 'response count'
  TRsuffix = 'total responses'


  if(is.null(time_period)){
    time_period <- names(inputFormat)[grep(RCsuffix, names(inputFormat))] %>%
      gsub(' - response count', '', .)
  }
  numberOfPeriods <- length(time_period)

  # TFanswerGroupingRefs takes the answer_groupings table and concatenates answer groups into single rows based on comma-separated answer choice IDs
  unique_answer_groupings <- TFanswerGroupingRefs()



  answer_grouping_table <- TFanswerGroupingHandler(inputFormat,
                                                   RCsuffix = RCsuffix,
                                                   numberOfPeriods = numberOfPeriods,
                                                   unique_answer_groupings = unique_answer_groupings)

  answer_groupings_and_dataKey <- left_join(answer_groupings, dataKey, by = "Answer ID")

  answer_groupings_and_dataKey$`Answer Text` <- answer_groupings_and_dataKey$`Answer Label`
  answer_groupings_and_dataKey$`Question Text` <- paste0(answer_groupings_and_dataKey$`Question Text`, " - ", answer_groupings_and_dataKey$`Answer Group Label`)
  answer_groupings_and_dataKey$`Group ID` <- answer_groupings_and_dataKey$`Answer Group ID`
  answer_groupings_and_dataKey <- answer_groupings_and_dataKey[, -which(colnames(answer_groupings_and_dataKey) %in% c("Answer Group ID", "Answer Group Label", "Answer Label"))]

  answer_groupings_and_dataKey <- left_join(answer_groupings_and_dataKey, unique_answer_groupings[, c("Answer ID", "Answer Group ID", "Answer Label")], by = c("Group ID" = "Answer Group ID", "Answer Text" = "Answer Label"))
  answer_groupings_and_dataKey$`Answer ID.x` <- answer_groupings_and_dataKey$`Answer ID.y`
  answer_groupings_and_dataKey <- answer_groupings_and_dataKey[, -which(colnames(answer_groupings_and_dataKey) == "Answer ID.y")]
  colnames(answer_groupings_and_dataKey) <- gsub("Answer ID.x", "Answer ID", colnames(answer_groupings_and_dataKey))

  answer_groupings_and_dataKey$Tag <- NA
  answer_groupings_and_dataKey$`Tag Order` <- NA
  answer_groupings_and_dataKey$Label <- NA

  answer_flag_aggregates <- aggregate(. ~`Answer ID`,
                                      answer_groupings_and_dataKey[, c('Answer ID', 'Answer Flag')], sum)

  answer_groupings_and_dataKey <- left_join(answer_groupings_and_dataKey[, -which(colnames(answer_groupings_and_dataKey) == "Answer Flag")], answer_flag_aggregates, by = "Answer ID") %>%
    .[!duplicated(.), ]


  answer_groupings_and_dataKey$`Answer Flag`[which(answer_groupings_and_dataKey$`Answer Flag` > 1)] <- 1

  answer_groupings_and_dataKey <- rbind(answer_groupings_and_dataKey, dataKey)



  outputFormatted <- inputFormat
  colnames(outputFormatted)[which(colnames(outputFormatted) == 'stem')] <- 'Stem Answer ID'
  outputFormatted$`Stem Answer Group ID` <- NA
  outputFormatted$`Stem Answer Group Label` <- NA
  outputFormatted$`Stem Answer Label` <- NA
  colnames(outputFormatted)[which(colnames(outputFormatted) == 'banner')] <- 'Banner Answer ID'
  outputFormatted$`Banner Answer Group ID` <- NA
  outputFormatted$`Banner Answer Group Label` <- NA
  outputFormatted$`Banner Answer Label` <- NA

  outputFormatted <- rbind(outputFormatted, answer_grouping_table)


  outputFormatted$weighting_stem_bannerQ_and_answer_groups <- paste(outputFormatted$weighting_scheme,
                                                                    outputFormatted$`Stem Answer ID`,
                                                                    outputFormatted$`Stem Answer Group ID`,
                                                                    outputFormatted$bannerQ,
                                                                    outputFormatted$`Banner Answer Group ID`,
                                                                    sep=";")



  responseColNumbers <- grep(RCsuffix, colnames(outputFormatted))
  responseCols <- colnames(outputFormatted)[responseColNumbers]
  totalCols <- gsub(RCsuffix, TRsuffix, responseCols)

  outputFormatted <- as.data.frame(outputFormatted)

  outputFormatted$unique <- paste(outputFormatted$weighting_scheme,
                                  outputFormatted$`Stem Answer ID`,
                                  outputFormatted$`Stem Answer Group ID`,
                                  outputFormatted$`Banner Answer ID`,
                                  outputFormatted$`Banner Answer Group ID`,
                                  sep=";")

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

  # outputFormatted <- outputFormatted[, -which(colnames(outputFormatted) == "weighting_stem_bannerQ_and_answer_groups")]
  # outputFormatted <- outputFormatted[, -which(colnames(outputFormatted) == "unique")]
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


  answer_groupings_and_dataKey_subset <- answer_groupings_and_dataKey[, c('Answer ID', 'Group ID', 'Answer Text', 'Answer Flag', 'Question Text', 'Sponsored', 'Account')]

  outputFormatted <- left_join(outputFormatted, answer_groupings_and_dataKey_subset, by = c('Stem Answer ID' = 'Answer ID', 'Stem Answer Group ID' = 'Group ID'))

  colnames(outputFormatted)[which(colnames(outputFormatted) %in% c("Answer Text",
                                                                   "Answer Flag",
                                                                   "Question Text",
                                                                   "Sponsored",
                                                                   "Account"))] <- c("Stem Name",
                                                                                     "Stem Answer Flag",
                                                                                     "Stem QText",
                                                                                     "Stem Client Q Flag",
                                                                                     "Stem Q Account ID")


  outputFormatted <- left_join(outputFormatted, answer_groupings_and_dataKey_subset, by = c('Banner Answer ID' = 'Answer ID', 'Banner Answer Group ID' = 'Group ID'))

  colnames(outputFormatted)[which(colnames(outputFormatted) %in% c("Answer Text",
                                                                   "Answer Flag",
                                                                   "Question Text",
                                                                   "Sponsored",
                                                                   "Account"))] <- c("Banner Name",
                                                                                     "Banner Answer Flag",
                                                                                     "Banner QText",
                                                                                     "Banner Client Q Flag",
                                                                                     "Banner Q Account ID")

  colnames(outputFormatted)[which(colnames(outputFormatted) %in% c("weighting_scheme",
                                                                   "stemQ",
                                                                   "bannerQ"))] <- c("Weighting Scheme",
                                                                                     "Stem QID",
                                                                                     "Banner QID")


  outputFormatted$`Answer Flag` <- as.numeric(outputFormatted$`Stem Answer Flag`) + as.numeric(outputFormatted$`Banner Answer Flag`)
  
  if(use_default_answer_flag){
    outputFormatted$`Answer Flag`[which(is.na(outputFormatted$`Answer Flag`))] <- 2
  }

  outputFormatted$`Stats Flag` <- NA
  outputFormatted$`Chart` <- NA
  outputFormatted$`Manual Flag` <- NA

  colnames(outputFormatted)[grep('Answer Group ID', colnames(outputFormatted))] <- gsub('Answer ', '', colnames(outputFormatted)[grep('Answer Group ID', colnames(outputFormatted))])

  baseCols <- c("Weighting Scheme",
                "Stem Client Q Flag", "Stem Q Account ID",
                "Banner Client Q Flag", "Banner Q Account ID",
                "Answer Flag", "Stats Flag", "Chart",  "Manual Flag",
                "Stem QID", "Stem Group ID", "Stem QText", "Stem Answer ID", "Stem Name",
                "Banner QID", "Banner Group ID","Banner QText", "Banner Answer ID", "Banner Name")

  outputFormatted <- outputFormatted[, c(baseCols, time_period, percentDiffCols, responseCols, totalCols)]
  
  if(!is.null(segment_names)){
    outputFormatted$`Stem QText`[which(outputFormatted$`Stem QID` %in% segment_names)] <- outputFormatted$`Stem QID`[which(outputFormatted$`Stem QID` %in% segment_names)]
    outputFormatted$`Stem Name`[which(outputFormatted$`Stem QID` %in% segment_names)] <- outputFormatted$`Stem QID`[which(outputFormatted$`Stem QID` %in% segment_names)]
  }
  

  outputFormatted$`Stem QText` <- as.character(outputFormatted$`Stem QText`) %>%
    utf8_normalize(., map_quote = TRUE)
  outputFormatted$`Banner QText` <- as.character(outputFormatted$`Banner QText`) %>%
    utf8_normalize(., map_quote = TRUE)
  outputFormatted$`Stem Name` <- as.character(outputFormatted$`Stem Name`, map_quote = TRUE) %>%
    utf8_normalize(., map_quote = TRUE)
  outputFormatted$`Banner Name` <- as.character(outputFormatted$`Banner Name`, map_quote = TRUE) %>%
    utf8_normalize(., map_quote = TRUE)


  outputFormatted <- outputFormatted[!duplicated(outputFormatted), ] # Just in case

  outputFormattedName <- outputName("Output - Responses - Formatted", batch_time = batch_time)
  saveRDS(outputFormatted, file = outputFormattedName)

  write.table(outputFormatted, file=paste0(outputFormattedName,'.tsv'), quote=TRUE, sep='\t', row.names=FALSE)

  return(outputFormatted)

}

