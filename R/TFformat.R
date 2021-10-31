TFformat <- function(inputFormat){

  # inputFormat <- outputWider

  inputFormat <- unique(inputFormat)
  inputFormatNumberOfCols1 <- ncol(inputFormat)

  answer_groupings <- arrange(answer_groupings, `Answer ID`)
  unique_answer_groups <- unique(answer_groupings$`Answer Group ID`)

  unique_answer_groupings <- NULL


  for(i in unique_answer_groups){
    answer_labels <- answer_groupings$`Answer Label`[which(answer_groupings$`Answer Group ID` == i)] %>%
      unique()
    unique_answer_groupings_subset <- answer_groupings[which(answer_groupings$`Answer Group ID` == i), ]

    for(j in answer_labels){

      unique_grouped_IDs <- paste(unique_answer_groupings_subset$`Answer ID`[which(unique_answer_groupings_subset$`Answer Label` == j)], collapse = ", ")
      unique_answer_groupings_subset$`Answer ID`[which(unique_answer_groupings_subset$`Answer Label` == j)] <- unique_grouped_IDs

    }

    unique_answer_groupings_subset <- unique(unique_answer_groupings_subset)
    unique_answer_groupings <- rbind(unique_answer_groupings, unique_answer_groupings_subset)

  }


  RCsuffix <- 'response count'
  # TRsuffix <- 'total responses'

  if(is.null(time_period)){
    time_period <- names(inputFormat)[grep(RCsuffix, names(inputFormat))] %>%
      gsub(' - response count', '', .)
  }

  numberOfPeriods <- length(time_period)

  responseColNumbers <- grep(RCsuffix, names(inputFormat))
  totalColNumbers <- (responseColNumbers[length(responseColNumbers)] + 1):(responseColNumbers[length(responseColNumbers)] + length(responseColNumbers))

  responseCols <- names(inputFormat)[responseColNumbers]
  totalCols <- gsub("response count", "total responses", responseCols)

  # inputFormatNumberOfMetadataCols <- inputFormatNumberOfCols1 - length(responseColNumbers) - length(totalColNumbers)
  inputFormatNumberOfMetadataCols <- inputFormatNumberOfCols1 - length(responseColNumbers)

  ungrouped_aggregates <- aggregate(. ~ uniqueCrosstab, inputFormat[, c('uniqueCrosstab', responseCols)], sum)
  colnames(ungrouped_aggregates) <- gsub("response count", "total responses", colnames(ungrouped_aggregates))
  outputFormatted <- left_join(inputFormat, ungrouped_aggregates, by = "uniqueCrosstab")

  ### answer_grouping_table_banner
  answer_grouping_table_banner <- left_join(answer_groupings, inputFormat, by = c("Answer ID" = "banner"))
  colnames(answer_grouping_table_banner)[1:ncol(answer_groupings)] <- paste0("Banner ", colnames(answer_grouping_table_banner)[1:ncol(answer_groupings)])
  answer_grouping_table_banner <- left_join(answer_grouping_table_banner,
                                            unique_answer_groupings,
                                            by = c('Banner Answer Group ID' = 'Answer Group ID', 'Banner Answer Label' = 'Answer Label'))
  answer_grouping_table_banner$`Banner Answer ID` <- answer_grouping_table_banner$`Answer ID`
  answer_grouping_table_banner$unique <- paste(
    answer_grouping_table_banner$stem,
    paste0(answer_grouping_table_banner$`Banner Answer ID`, ', Group ', answer_grouping_table_banner$`Banner Answer Group ID`),
    answer_grouping_table_banner$weighting_scheme,
    sep = ";"
  )
  answer_grouping_table_banner$uniqueCrosstab <- paste(
    answer_grouping_table_banner$stemQ,
    paste0(answer_grouping_table_banner$bannerQ, ', Group ', answer_grouping_table_banner$`Banner Answer Group ID`),
    answer_grouping_table_banner$weighting_scheme,
    sep = ";"
  )


  ### answer_grouping_table_stem
  answer_grouping_table_stem <- left_join(answer_groupings, inputFormat, by = c("Answer ID" = "stem"))
  colnames(answer_grouping_table_stem)[1:ncol(answer_groupings)] <- paste0("Stem ", colnames(answer_grouping_table_stem)[1:ncol(answer_groupings)])
  answer_grouping_table_stem <- left_join(answer_grouping_table_stem,
                                          unique_answer_groupings,
                                          by = c('Stem Answer Group ID' = 'Answer Group ID', 'Stem Answer Label' = 'Answer Label'))
  answer_grouping_table_stem$`Stem Answer ID` <- answer_grouping_table_stem$`Answer ID`
  answer_grouping_table_stem$unique <- paste(
    paste0(answer_grouping_table_stem$`Stem Answer ID`, ', Group ', answer_grouping_table_stem$`Stem Answer Group ID`),
    answer_grouping_table_stem$banner,
    answer_grouping_table_stem$weighting_scheme,
    sep = ";"
  )
  answer_grouping_table_stem$uniqueCrosstab <- paste(
    paste0(answer_grouping_table_stem$stemQ, ', Group ', answer_grouping_table_stem$`Stem Answer Group ID`),
    answer_grouping_table_stem$bannerQ,
    answer_grouping_table_stem$weighting_scheme,
    sep = ";"
  )
  answer_grouping_table_stem$`Banner Answer Group ID` <- answer_grouping_table_stem$bannerQ
  colnames(answer_grouping_table_stem)[which(colnames(answer_grouping_table_stem) == "banner")] <- 'Banner Answer ID'
  answer_grouping_table_stem$`Banner Answer Label` <- NA

  ### answer_grouping_table
  answer_grouping_table <- left_join(answer_groupings, answer_grouping_table_banner[, -which(colnames(answer_grouping_table_banner) == 'Answer ID')], by = c("Answer ID" = "stem"))
  colnames(answer_grouping_table)[1:ncol(answer_groupings)] <- paste0("Stem ", colnames(answer_grouping_table)[1:ncol(answer_groupings)])
  answer_grouping_table <- left_join(answer_grouping_table,
                                     unique_answer_groupings,
                                     by = c('Stem Answer Group ID' = 'Answer Group ID', 'Stem Answer Label' = 'Answer Label'))
  answer_grouping_table$`Stem Answer ID` <- answer_grouping_table$`Answer ID`
  answer_grouping_table$unique <- paste(
    paste0(answer_grouping_table$`Stem Answer ID`, ', Group ', answer_grouping_table$`Stem Answer Group ID`),
    paste0(answer_grouping_table$`Banner Answer ID`, ', Group ', answer_grouping_table$`Banner Answer Group ID`),
    answer_grouping_table$weighting_scheme,
    sep = ";"
  )
  answer_grouping_table$uniqueCrosstab <- paste(
    paste0(answer_grouping_table$stemQ, ', Group ', answer_grouping_table$`Stem Answer Group ID`),
    paste0(answer_grouping_table$bannerQ, ', Group ', answer_grouping_table$`Stem Answer Group ID`),
    answer_grouping_table$weighting_scheme,
    sep = ";"
  )

  ##answer_grouping_table_banner final cleanup
  answer_grouping_table_banner$`Stem Answer Group ID` <- answer_grouping_table_banner$stemQ
  colnames(answer_grouping_table_banner)[which(colnames(answer_grouping_table_banner) == "stem")] <- 'Stem Answer ID'
  answer_grouping_table_banner$`Stem Answer Label` <- NA
  answer_grouping_table_banner <- answer_grouping_table_banner[-which(answer_grouping_table_banner$unique %in% answer_grouping_table$unique), ]


  answer_grouping_table <- rbind(answer_grouping_table_stem[, colnames(answer_grouping_table)],
                                 answer_grouping_table_banner[, colnames(answer_grouping_table)],
                                 answer_grouping_table)

  answer_grouping_table <- answer_grouping_table[, -which(colnames(answer_grouping_table) == 'Answer ID')]
  answer_grouping_table <- unique(answer_grouping_table)

  unique_uniques <- unique(answer_grouping_table$unique)

  if(length(unique_uniques) != 1 || !is.na(unique_uniques[1])){

    answer_grouping_response_aggregates <- aggregate(. ~unique,
                                                     answer_grouping_table[, c('unique', responseCols)], sum)

    answer_grouping_total_aggregates <- aggregate(. ~uniqueCrosstab,
                                                  answer_grouping_table[, c('uniqueCrosstab', responseCols)], sum)
    colnames(answer_grouping_total_aggregates) <- gsub("response count", "total responses", colnames(answer_grouping_total_aggregates))

    answer_grouping_table <- left_join(answer_grouping_response_aggregates,
                                       answer_grouping_table[, -which(colnames(answer_grouping_table) %in% responseCols)],
                                       by = 'unique')

    answer_grouping_table <- left_join(answer_grouping_table,
                                       answer_grouping_total_aggregates, by = 'uniqueCrosstab')

    answer_grouping_table <- unique(answer_grouping_table)

    colnames(answer_grouping_table)[which(colnames(answer_grouping_table) == 'Stem Answer ID')] <- 'stem'
    colnames(answer_grouping_table)[which(colnames(answer_grouping_table) == 'Banner Answer ID')] <- 'banner'

    answer_grouping_table_cols_wanted <- which(colnames(answer_grouping_table) %in% colnames(outputFormatted))

    answer_grouping_table <- answer_grouping_table[, answer_grouping_table_cols_wanted]
    answer_grouping_table <- answer_grouping_table[, colnames(outputFormatted)]

    outputFormatted <- rbind(outputFormatted, answer_grouping_table)

  }

  percentTableCols <- lapply(c("unique$", names(outputFormatted)[responseColNumbers]), grep, x=colnames(outputFormatted)) %>%
    unlist()

  percentTable <- outputFormatted[, percentTableCols]
  percentTable[, 2:dim(percentTable)[2]] <- 0
  names(percentTable) <- c("unique", time_period)

  percentDiffTable <- percentTable[, 1:(ncol(percentTable)-1)]

  for(percentLoop in 1:numberOfPeriods){
    numerator <- outputFormatted[,responseColNumbers[percentLoop]]
    denominator <- outputFormatted[,totalColNumbers[percentLoop]]
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

  outputFormattedNumberOfCols <- ncol(outputFormatted)

  dataKeyColsWanted <- c('Answer ID', 'Weighting Scheme', 'Answer Text', 'Answer Flag', 'Question Text', 'Sponsored', 'Account')

  outputFormatted <- left_join(outputFormatted, dataKey[, dataKeyColsWanted], by = c("stem" = "Answer ID", "weighting_scheme" = "Weighting Scheme"))

  NA_rows <- which(is.na(outputFormatted$`Answer Text`))

  ### Note: This should really be done via dataKey but I'm just fixing it for now
  ### This could be left in in the event that a new segment is made but not added to dataKey right away
  # outputFormatted[NA_rows, "Answer Text"] <- outputFormatted$stem[NA_rows]
  # outputFormatted[NA_rows, "Answer Flag"] <- 1
  # outputFormatted[NA_rows, "Question Text"] <- "Custom Segment"

  outputFormattedNumberOfCols2 <- ncol(outputFormatted)

  colnames(outputFormatted)[(outputFormattedNumberOfCols+1):outputFormattedNumberOfCols2] <- c("Stem Name", "Stem Answer Flag", "Stem QText",
                                                                                    "Stem Client Q Flag", "Stem Q Account ID")

  dataKeyUSadultsColsWanted <- c('Answer ID', 'Answer Text', 'Answer Flag', 'Question Text', 'Sponsored', 'Account')

  dataKeyUSadults <- dataKey[which(dataKey$`Weighting Scheme` == "USadultWeighting"), ] %>%
    .[, dataKeyUSadultsColsWanted]

  outputFormatted <- left_join(outputFormatted, dataKeyUSadults, by = c('banner' = 'Answer ID')) # Use the 'basic' version of the dataKey to populate banner Qs

  outputFormattedNumberOfCols3 <- ncol(outputFormatted)

  colnames(outputFormatted)[(outputFormattedNumberOfCols2+1):outputFormattedNumberOfCols3] <- c("Banner Name", "Banner Answer Flag", "Banner QText",
                                                                                    "Banner Client Q Flag", "Banner Q Account ID")

  colnames(outputFormatted)[1:inputFormatNumberOfMetadataCols] <- c("Unique Row ID", "Weights", "Stem ID (answers)", "Banner ID (answers)", "Unique Crosstab ID", "Stem QID", "Banner QID")

  outputFormatted$`Answer Flag` <- as.numeric(outputFormatted$`Stem Answer Flag`) + as.numeric(outputFormatted$`Banner Answer Flag`)
  outputFormatted$`Stats Flag` <- NA
  outputFormatted$`Chart` <- NA
  outputFormatted$`Manual Flag` <- NA

  outputFormattedData <- outputFormatted[, c(1, (inputFormatNumberOfMetadataCols+1):(outputFormattedNumberOfCols))]

  baseCols <- c("Unique Row ID", "Weights", "Unique Crosstab ID",
                "Stem Client Q Flag", "Stem Q Account ID",
                "Banner Client Q Flag", "Banner Q Account ID",
                "Answer Flag", "Stats Flag", "Chart",  "Manual Flag",
                "Stem QID", "Stem QText", "Stem ID (answers)", "Stem Name",
                "Banner QID", "Banner QText", "Banner ID (answers)", "Banner Name")



  baseTable <- outputFormatted[, baseCols]

  outputFormatted <- merge(baseTable, outputFormattedData)

  columnOrder <- c(baseCols, time_period, percentDiffCols, responseCols, totalCols)

  outputFormatted <- outputFormatted[, columnOrder]
  outputFormatted$`Stem QText` <- as.character(outputFormatted$`Stem QText`) %>%
    utf8_normalize(., map_quote = TRUE)
  outputFormatted$`Banner QText` <- as.character(outputFormatted$`Banner QText`) %>%
    utf8_normalize(., map_quote = TRUE)
  outputFormatted$`Stem Name` <- as.character(outputFormatted$`Stem Name`, map_quote = TRUE) %>%
    utf8_normalize(., map_quote = TRUE)
  outputFormatted$`Banner Name` <- as.character(outputFormatted$`Banner Name`, map_quote = TRUE) %>%
    utf8_normalize(., map_quote = TRUE)

  outputFormattedName <- outputName("Output - Responses - Formatted", batch_time = batch_time)
  saveRDS(outputFormatted, file = outputFormattedName)

  write.table(outputFormatted, file=paste0(outputFormattedName,'.tsv'), quote=TRUE, sep='\t', row.names=FALSE)

  return(outputFormatted)

}

