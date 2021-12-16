TFstats <- function(inputStats, cutoff_stats_flags){
  # inputStats <- outputFormatted

  ### Preliminary estimate is TFstats can do 35,000 topline results and 4200 segment/cross results per hour

  uniqueBannerQlist <- unique(inputStats$`Banner QID`)

  baseColsEnd <- grep("Banner Name", colnames(inputStats))
  first_data_column <- baseColsEnd + 1

  all_period_columns <- grep("Period", colnames(inputStats))
  first_period_column <- all_period_columns[1]

  last_period_column <- all_period_columns[length(all_period_columns)]

  all_response_count_columns <- grep("- response", colnames(inputStats))
  first_response_count_column <- all_response_count_columns[1]
  last_response_count_column <- all_response_count_columns[length(all_response_count_columns)]

  first_total_response_count_column <- last_response_count_column
  last_total_response_count_column <- ncol(inputStats)

  if(is.na(first_period_column)){
    dataColsEnd <- all_response_count_columns[1] - 1
  } else{
    dataColsEnd <- first_period_column - 1
  }

  numberOfPeriods <- dataColsEnd - baseColsEnd



  sigColsTable <- matrix(0L, nrow=nrow(inputStats), ncol=(numberOfPeriods*2-1))

  # if(!is.na(first_period_column)){
  #   dateSigColNames <- colnames(inputStats)[all_response_count_columns] %>%
  #     gsub('response count', 'date significance', .)
  # }
  dateSigColNames <- colnames(inputStats)[all_response_count_columns[1:(length(all_response_count_columns) - 1)]] %>%
    gsub('response count', 'date significance', .)
  segSigColNames <- colnames(inputStats)[all_response_count_columns] %>%
    gsub('response count', 'segment significance', .)

  dateSigColsEnd <- ncol(inputStats)+length(dateSigColNames)

  outputStats <- cbind(inputStats, sigColsTable) %>%
    as_tibble()


  number_columns_outputStats <- ncol(outputStats)

  dateSigCols <- (last_total_response_count_column + 1):dateSigColsEnd
  segSigCols <- (dateSigColsEnd + 1):number_columns_outputStats

  names(outputStats) <- c(colnames(inputStats), dateSigColNames, segSigColNames)

  if(numberOfPeriods > 1){
    for(dateSigLoop in 1:nrow(outputStats)){
      for(dateLoop in 1:(numberOfPeriods-1)){

        # dateLoop <- 1
        againstCount <- as.numeric(outputStats[dateSigLoop, last_response_count_column])
        toCount <- as.numeric(outputStats[dateSigLoop, first_response_count_column - 1 + dateLoop])
        againstSize <- as.numeric(outputStats[dateSigLoop, last_total_response_count_column])
        toSize <- as.numeric(outputStats[dateSigLoop, last_response_count_column + dateLoop])

        ## Not sure why I commented out these if(is.na()) statements; leaving them here for now
        # if(is.na(againstCount) == FALSE){
        #   if(is.na(toCount) == FALSE){

        if(againstCount > 0 && againstCount != againstSize){
          if(toCount > 0 && toCount != toSize){

            prop <- prop.test(c(againstCount,toCount), c(againstSize,toSize))

            p_value <- prop$p.value

            if (p_value <= .05){
              # Report the value of the end time period subtracted from each time period
              # e.g. if April has a higher proportion than March, March will have a negative value
              # indicating it is smaller than the most recent time period
              percent_difference <- (prop$estimate[1]-prop$estimate[2])/prop$estimate[2] %>%
                as.numeric(.)
              outputStats[dateSigLoop, (ncol(inputStats)+dateLoop)] <- percent_difference
            }
          }
        }
        #   }
        # }
      }
    }
  }



  for(segmentLoop in 1:length(uniqueBannerQlist)){

    outputSubset <- subset(outputStats, outputStats$`Banner QID`==uniqueBannerQlist[segmentLoop])
    outputSubsetTopline <- subset(outputSubset, outputSubset$`Stem ID (answers)`==0)
    outputSubsetSegmentQuestions <- subset(outputSubset, outputSubset$`Stem ID (answers)`!=0)

    if(nrow(outputSubsetSegmentQuestions)!=0){
      for(rowLoop in 1:nrow(outputSubsetTopline)){

        outputSubsetSegmentIDs <- subset(outputSubsetSegmentQuestions, outputSubsetSegmentQuestions$`Banner QID`==outputSubsetTopline$`Banner QID`[rowLoop])

        for(everyPeriodLoop in 1:numberOfPeriods){

          againstCount <- as.numeric(outputSubsetTopline[rowLoop, first_response_count_column - 1 + everyPeriodLoop])
          againstSize <- as.numeric(outputSubsetTopline[rowLoop, last_response_count_column + everyPeriodLoop])

          for(everyStemLoop in 1:nrow(outputSubsetSegmentIDs)){

            toCount <- as.numeric(outputSubsetSegmentIDs[everyStemLoop, first_response_count_column - 1 + everyPeriodLoop])
            toSize <- as.numeric(outputSubsetSegmentIDs[everyStemLoop, last_response_count_column + everyPeriodLoop])

            if(is.na(againstCount) == FALSE){
              if(is.na(toCount) == FALSE){
                if(againstCount > 0 && againstCount != againstSize){
                  if(toCount > 0 && toCount != toSize){

                    prop <- prop.test(c(againstCount,toCount), c(againstSize,toSize))
                    p_value <- prop$p.value

                    if (p_value <= .05){
                      # Report the value of the topline proportion subtracted from the segment proportion
                      # e.g. if the segment has a bigger proportion than the topline, it is positive
                      # indicating it is larger than the topline
                      percent_difference <- (prop$estimate[1]-prop$estimate[2])/prop$estimate[2] %>%
                        as.numeric(.)
                      outputStats[grep(outputSubsetSegmentIDs$`Unique Row ID`[everyStemLoop], outputStats$`Unique Row ID`), (dateSigColsEnd+everyPeriodLoop)] <- percent_difference
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }




  # Create identifier for how many times a row has recently had statistical significance either in time or relative to topline

  if(numberOfPeriods <= 3){
    outputStats$`Stats Flag` <- rowSums(outputStats[, (last_total_response_count_column+1):ncol(outputStats)] != 0)
  } else{
    outputStats$`Stats Flag` <- rowSums(outputStats[, c((number_columns_outputStats-numberOfPeriods-1):(number_columns_outputStats-numberOfPeriods),
                                                        (number_columns_outputStats-2):number_columns_outputStats)] != 0)
  }

  for(i in 1:nrow(outputStats)){
    if( outputStats$`Stem QID`[i] == 0){
       outputStats$`Stats Flag`[i] <- outputStats$`Stats Flag`[i]*2 + 1 # Adjustment for topline results which do not have segment significance
    }
  }

  max_stats_flags <- max(outputStats$`Stats Flag`)
  cutoff_stats_flags_value <- round(max_stats_flags*cutoff_stats_flags)

  max_date_percent_diff <- outputStats[, dateSigCols] %>%
    abs() %>%
    max()

  if(max_date_percent_diff >= 2){
    cutoff_date_flags_value <- cutoff_date_flags
  } else{
    cutoff_date_flags_value <- max_date_percent_diff*cutoff_date_flags
  }


  max_seg_percent_diff <-  outputStats[, segSigCols] %>%
    abs() %>%
    max()
  cutoff_seg_flags_value <- max_seg_percent_diff*cutoff_seg_flags

  if(max_seg_percent_diff >= 2){
    cutoff_seg_flags_value <- cutoff_seg_flags
  } else{
    cutoff_seg_flags_value <- max_date_percent_diff*cutoff_seg_flags
  }

  for(chart_loop in 1:nrow(outputStats)){
    if(!is.na(outputStats$`Answer Flag`[chart_loop])){
      if(outputStats$`Answer Flag`[chart_loop] == 2){
        if(!is.na(outputStats$`Stats Flag`[chart_loop])){
          if(outputStats$`Stats Flag`[chart_loop] >= cutoff_stats_flags_value){
            if(abs(rowMeans(outputStats[chart_loop, dateSigCols])) >= cutoff_date_flags_value){
              outputStats$Chart[chart_loop] = 1
            }
            if(abs(rowMeans(outputStats[chart_loop, segSigCols])) >= cutoff_seg_flags_value){
              outputStats$Chart[chart_loop] = 1
            }
          }
        }
      }
    }
  }

  if(exists('batch_time')){
    outputStatsName <- outputName("Output - Responses - Formatted with Stats", batch_time = batch_time)

  } else{
    outputStatsName <- outputName("Output - Responses - Formatted with Stats")
  }

  saveRDS(outputStats, file = outputStatsName)

  # write.table(outputStats, file=paste0(outputStatsName,'.tsv'), quote=TRUE, sep='\t', row.names=FALSE)

  return(outputStats)

}
