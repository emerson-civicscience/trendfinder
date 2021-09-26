TFdateHandler <- function(data_start_dates, data_end_dates){

  # Create vector of exclusive end dates by removing the first entry from data_start_dates and adding a day to DataEnd
  if(is.null(data_end_dates)){
    data_end_dates <- data_start_dates %>%
      .[-1] %>%
      c(., as.character(ymd(data_start_dates)+1))
  }

  return(data_end_dates)

}
