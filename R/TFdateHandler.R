TFdateHandler <- function(data_start_dates, data_end_dates = NULL, time_period_length = NULL){

	# Create vector of exclusive end dates by removing the first entry from data_start_dates, then
	# add one more entry that is the same length of time between the last two entries of data_start_dates (unless it exceeds today)

	data_start_dates <- ymd(data_start_dates)

	if(is.null(data_end_dates)){
		data_end_dates <- data_start_dates %>%
			.[-1]

		if(is.null(time_period_length)){
			time_period_length <- as.numeric(difftime(data_start_dates[length(data_start_dates)], data_start_dates[length(data_start_dates) - 1]))
		}

		final_date <- data_end_dates[length(data_end_dates)] + time_period_length

		data_end_dates <- c(data_end_dates, final_date)

	} else{
		final_date <- ymd(data_end_dates[length(data_end_dates)])
	}

	if(final_date > today()){
		data_end_dates[length(data_end_dates)] <- today()+1
	}

	data_end_dates <- (data_end_dates - 1) %>%
		as.character()

	return(data_end_dates)

}
