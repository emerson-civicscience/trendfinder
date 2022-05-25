TFancestryOutputFormat <- function(outputFormatted){
  
  ancestry_brand_dash_historical <- readRDS("~/TrendFinder/Outputs/ancestry_brand_dash_historical.rds")
  ancestry_brand_dash_historical_notes <- readRDS("~/TrendFinder/Outputs/ancestry_brand_dash_historical_notes.rds")
  ancestry_bd_history <- left_join(ancestry_brand_dash_historical, ancestry_brand_dash_historical_notes)
  
  output_subset <- outputFormatted[, c(1, 8, 10:ncol(outputFormatted))]
  output_subset$`Stem Group ID`[is.na(output_subset$`Stem Group ID`)] <- 0
  output_subset$`Banner Group ID`[is.na(output_subset$`Banner Group ID`)] <- 0
  
  output_subset$`Stem QID`[output_subset$`Stem QID`==0] <- "US Adults"
  ancestry_data <- left_join(ancestry_bd_history, output_subset, by = c("Stem QID", "Banner Answer ID"))
  new_rc_cols <- grep(' - response count', colnames(ancestry_data))
  new_tr_cols <- grep(' - total responses', colnames(ancestry_data))
  
  for(i in 1:length(time_period)){
    replace_dates <- paste0(data_start_dates[i], " - ", data_end_dates[i])
    replacement_text <- time_period[i]
    colnames(ancestry_data) <- gsub(replace_dates, replacement_text, colnames(ancestry_data))
    
  }
  
  colnames(ancestry_data) <- gsub(" - Percent", "", colnames(ancestry_data))
  colnames(ancestry_data) <- gsub("response count", "Response Count", colnames(ancestry_data))
  colnames(ancestry_data) <- gsub("total responses", "Total Response Count", colnames(ancestry_data))
  
  metadata_cols <- which(colnames(ancestry_data) %in% c("Weighting Scheme", "Chart", "Stem QID", "Stem Group ID", "Stem QText", "Stem Answer ID", "Stem Name", "Banner QID", "Banner Group ID", "Banner QText", "Banner Answer ID", "Banner Name"))
  response_count_cols <- grep(" - Response Count", colnames(ancestry_data))
  total_response_cols <- grep(" - Total Response Count", colnames(ancestry_data))
  note_cols <- grep(" - Notes", colnames(ancestry_data))
  not_percent_cols <- which(1:ncol(ancestry_data) %in% c(metadata_cols, response_count_cols, total_response_cols, note_cols))
  percent_cols <- (1:ncol(ancestry_data))[-not_percent_cols]
  
  ancestry_percents <- ancestry_data[, c(metadata_cols, percent_cols)]
  ancestry_responses <- ancestry_data[, c(metadata_cols, response_count_cols)]
  ancestry_totals <- ancestry_data[, c(metadata_cols, total_response_cols)]
  ancestry_notes <- ancestry_data[, c(metadata_cols, note_cols)]
  
  ancestry_data <- merge(ancestry_percents, ancestry_responses)
  ancestry_data <- merge(ancestry_data, ancestry_totals)
  ancestry_data <- merge(ancestry_data, ancestry_notes)
  ancestry_data$`Apr 2022 - Notes` <- ""
  
  ancestry_data <- ancestry_data[, c(3, 4, 1, 5:11, 2, 12:ncol(ancestry_data))]
  
  ancestry_data$`Stem Group ID`[is.na(ancestry_data$`Stem Group ID`)] <- 0
  ancestry_data$`Banner Group ID`[is.na(ancestry_data$`Banner Group ID`)] <- 0
  
  return(ancestry_data)
}
  

