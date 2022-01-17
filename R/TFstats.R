TFstats <- function(input_stats,
                    cutoff_stats_flags,
                    max_chart_return,
                    max_chart_iterate,
                    number_of_unique_topline_charts = 1000,
                    number_of_unique_seg_and_crosstab_charts = 1000){
  # input_stats <- outputFormatted

  input_stats$`Stem Group ID`[which(is.na(input_stats$`Stem Group ID`))] <- 0
  input_stats$`Banner Group ID`[which(is.na(input_stats$`Banner Group ID`))] <- 0

  input_cols <- colnames(input_stats)

  id_colnames <- c("Weighting Scheme", "Stem Answer ID", "Stem Group ID", "Banner Answer ID", "Banner Group ID")
  unique_input_rows <- input_stats[, id_colnames] %>% transpose() %>% as.list()

  id_cols <- which(input_cols %in% id_colnames)
  response_cols <- c(grep('response count', input_cols))
  total_cols <- c(grep('total responses', input_cols))


  df_response <- input_stats[, c(id_cols, response_cols)]
  colnames(df_response) <- gsub(' - response count', '', colnames(df_response))

  df_total <- input_stats[, c(id_cols, total_cols)]
  colnames(df_total) <- gsub(' - total responses', '', colnames(df_total))

  names_to_def <- c("start_date", "end_date")

  df_response_longer <- pivot_longer(df_response, cols = contains(" - "),
                                     names_to = names_to_def,
                                     names_sep = " - ",
                                     values_to = "response count")

  df_total_longer <- pivot_longer(df_total, cols = contains(" - "),
                                  names_to = names_to_def,
                                  names_sep = " - ",
                                  values_to = "total responses")

  df <- merge(df_response_longer, df_total_longer, by = c(id_colnames, names_to_def))

  stems <-  df[, c('Weighting Scheme', 'Stem Answer ID', 'Stem Group ID')] %>%
    .[!duplicated(.), ]
  banners <-  df[, c('Weighting Scheme', 'Banner Answer ID', 'Banner Group ID')] %>%
    .[!duplicated(.), ]
  start_and_end_dates <- df[, c('start_date', 'end_date')] %>%
    .[!duplicated(.), ] %>%
    setorder(., -"start_date")

  row.names(start_and_end_dates) <- NULL

  number_of_time_periods <- nrow(start_and_end_dates)

  if(number_of_time_periods < 3){
    total_date_periods <- number_of_time_periods
    total_seg_periods <- number_of_time_periods
  } else {
    total_date_periods <- 3
    total_seg_periods <- 2
  }

  # Test the last period with (up to) the two periods directly before it for significance
  if(total_date_periods > 1){
    date_sig_table <- mclapply(unique_input_rows, TFdatePropTest,
                               total_date_periods = total_date_periods,
                               df = df,
                               start_and_end_dates = start_and_end_dates,
                               id_colnames = id_colnames,
                               mc.cores = detectCores()) %>%
      rbindlist()
  } else{
    date_sign_table <- NULL
  }


  unique_banners <- input_stats[, c("Weighting Scheme", "Banner Answer ID", "Banner Group ID")] %>%
    .[!duplicated(.), ] %>%
    transpose() %>%
    as.list()


  for(i in total_seg_periods:1){

    seg_sig_table_interim <- mclapply(unique_banners, TFsegPropTest,
                                      start_date = start_and_end_dates$start_date[i],
                                      end_date = start_and_end_dates$end_date[i],
                                      id_colnames = id_colnames,
                                      df = df,
                                      mc.cores = detectCores()) %>%
      rbindlist(., fill = TRUE)

    remove_date_columns <- -grep('date', colnames(seg_sig_table_interim))
    remove_count_columns <- -grep('response', colnames(seg_sig_table_interim))
    remove_columns <- c(remove_date_columns, remove_count_columns)

    seg_sig_table_interim <- seg_sig_table_interim[, ..remove_columns]

    if(i == total_seg_periods){
      seg_sig_table <- seg_sig_table_interim
    } else{
      seg_sig_table <- merge(seg_sig_table, seg_sig_table_interim, by = id_colnames)
    }

  }

  output_stats <- left_join(input_stats, date_sig_table, by = id_colnames) %>%
    left_join(., seg_sig_table, by = id_colnames)

  sig_colnames <- grep('significance', colnames(output_stats))


  # Create identifier for how many times a row has recently had statistical significance either in time or relative to topline

  output_stats$`Stats Flag` <- rowSums(as.data.frame(output_stats[, sig_colnames]), na.rm = TRUE) %>%
    abs()

  output_stats_topline <- output_stats[which(output_stats$`Stem QID` == 0), ] %>%
  	.[which(.$`Answer Flag` == 2), ]
  output_stats_seg_and_crosstab <- output_stats[which(output_stats$`Stem QID` != 0), ] %>%
  	.[which(.$`Answer Flag` == 2), ]

  # Adjust for topline by using cutoff values among toplines and crosstabs separately
  cutoff_stats_flags_topline <- cutoff_stats_flags


  while(number_of_unique_topline_charts > max_chart_return){
    output_stats_topline$decile <- ntile(output_stats_topline$`Stats Flag`, cutoff_stats_flags_topline)
    number_of_unique_topline_charts <- output_stats_topline$`Banner QID`[which(output_stats_topline$decile == cutoff_stats_flags_topline)] %>%
      length()

    cutoff_stats_flags_topline <- cutoff_stats_flags_topline + max_chart_iterate
  }

  cutoff_stats_flags_seg_and_crosstab <- cutoff_stats_flags

  while(number_of_unique_seg_and_crosstab_charts > max_chart_return){
  	output_stats_seg_and_crosstab$decile <- ntile(output_stats_seg_and_crosstab$`Stats Flag`, cutoff_stats_flags_seg_and_crosstab)
    number_of_unique_seg_and_crosstab_charts <- output_stats_seg_and_crosstab$`Banner QID`[which(output_stats_seg_and_crosstab$decile == cutoff_stats_flags_seg_and_crosstab)] %>%
      length()

    cutoff_stats_flags_seg_and_crosstab <- cutoff_stats_flags_seg_and_crosstab + max_chart_iterate
  }

  if(length(output_stats_topline$decile) != 0){
    output_stats_topline <- output_stats_topline[which(output_stats_topline$decile == max(output_stats_topline$decile)), ]
  }

  if(length(output_stats_seg_and_crosstab$decile) != 0){
    output_stats_seg_and_crosstab <- output_stats_seg_and_crosstab[which(output_stats_seg_and_crosstab$decile == max(output_stats_seg_and_crosstab$decile)), ]
  }

  output_stats_deciles <- rbind(output_stats_topline, output_stats_seg_and_crosstab)

  output_stats <- left_join(output_stats, output_stats_deciles, by = colnames(output_stats))
  output_stats$Chart[which(!is.na(output_stats$decile))] <- 1
  output_stats <- output_stats[ , -which(colnames(output_stats) == 'decile')]


  if(exists('batch_time')){
    output_statsName <- outputName("Output - Responses - Formatted with Stats", batch_time = batch_time)

  } else{
    output_statsName <- outputName("Output - Responses - Formatted with Stats")
  }

  saveRDS(output_stats, file = output_statsName)

  # write.table(output_stats, file=paste0(output_statsName,'.tsv'), quote=TRUE, sep='\t', row.names=FALSE)

  return(output_stats)

  # write.table(outputStats, file=paste0(outputStatsName,'.tsv'), quote=TRUE, sep='\t', row.names=FALSE)

}
