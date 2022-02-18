TFdatePropTest <- function(unique_input_row,
													 total_date_periods,
													 df,
													 start_and_end_dates,
													 id_colnames,
													 date_sig_table = NULL){

	date_rows <- df[df$`Weighting Scheme` == unique_input_row[1], ] %>%
		.[.$`Stem Answer ID` == unique_input_row[2], ] %>%
		.[.$`Stem Group ID` == unique_input_row[3], ] %>%
		.[.$`Banner Answer ID` == unique_input_row[4], ] %>%
		.[.$`Banner Group ID` == unique_input_row[5], ]
	
	remove_rows <- c(which(is.na(date_rows$`Weighting Scheme`)), 
	                 which(is.na(date_rows$`Stem Answer ID`)),
	                 which(is.na(date_rows$`Banner Answer ID`)),
	                 which(is.na(date_rows$`response count`)),
	                 which(is.na(date_rows$`total responses`)))
	
	if(length(remove_rows) > 0){
	  date_rows <- date_rows[-remove_rows, ]
	}
	
	compare_against <- date_rows[date_rows$start_date == start_and_end_dates$start_date[1], ] %>%
		.[.$end_date == start_and_end_dates$end_date[1]]

	for(j in 2:total_date_periods){
		# Match the "to" and "against" values to each other here and run through prop test function
		compare_to <- date_rows[date_rows$start_date == start_and_end_dates$start_date[j], ] %>%
			.[.$end_date == start_and_end_dates$end_date[j]]

		compare_to_colname <- paste0(compare_to$start_date, " - ", compare_to$end_date, " - date significance")

		against_count <- compare_against$`response count`
		to_count <- compare_to$`response count`
		against_size <- compare_against$`total responses`
		to_size <- compare_to$`total responses`

		if(against_count > 0 && against_count != against_size){
			if(to_count > 0 && to_count != to_size){

				prop <- prop.test(c(against_count,to_count), c(against_size,to_size))

				p_value <- prop$p.value

				if (p_value <= .05){
					# Report the value of the end time period subtracted from each time period
					# e.g. if April has a higher proportion than March, March will have a negative value
					# indicating it is smaller than the most recent time period
					percent_difference <- (prop$estimate[1]-prop$estimate[2])/prop$estimate[2] %>%
						as.numeric(.)
					compare_to[1, compare_to_colname] <- percent_difference
				}
			}
		}

		if(is.null(compare_to[1, compare_to_colname])){
			compare_to[1, compare_to_colname] <- NA
		}

		compare_to <- compare_to[, c(id_colnames, compare_to_colname)]

		if (j == 2){
			date_sig_table <- compare_to
		} else{
			date_sig_table <- merge(date_sig_table, compare_to, by = id_colnames)
		}

	}
}
