TFsegPropTest <- function(unique_banner,
													start_date,
													end_date,
													id_colnames,
													df,
													seg_sig_table = NULL){

	seg_rows_colname <- paste0(start_date, " - ", end_date, " - segment significance")

	seg_rows <- df[df$`Weighting Scheme` == unique_banner[1], ] %>%
		.[.$`Banner Answer ID` == unique_banner[2], ] %>%
		.[.$`Banner Group ID` == unique_banner[3], ]

	all_rows_period <- seg_rows[seg_rows$start_date == start_date, ] %>%
		.[.$end_date == end_date, ]

	topline_row_period <- all_rows_period[all_rows_period$`Stem Answer ID` == 0, ]

	against_count <- topline_row_period$`response count`
	against_size <- topline_row_period$`total responses`

	seg_rows_period <- all_rows_period[all_rows_period$`Stem Answer ID` != 0, ]

	if(nrow(topline_row_period) != 0 && nrow(seg_rows_period) != 0){
		for(i in 1:nrow(seg_rows_period)){
			to_count <- all_rows_period$`response count`[i]
			to_size <- all_rows_period$`total responses`[i]

			if(!is.na(against_count) && !is.na(against_size) && !is.na(to_count) && !is.na(to_size) && against_count > 0 && against_count != against_size && to_count > 0 && to_count != to_size){

				prop <- prop.test(c(against_count, to_count), c(against_size, to_size))
				p_value <- prop$p.value

				if (p_value <= .05){
					# Report the value of the topline proportion subtracted from the segment proportion
					# e.g. if the segment has a bigger proportion than the topline, it is positive
					# indicating it is larger than the topline
					percent_difference <- (prop$estimate[1]-prop$estimate[2])/prop$estimate[2] %>%
						as.numeric(.)

					seg_rows_period[i, seg_rows_colname] <- percent_difference

				} else{
					seg_rows_period[i, seg_rows_colname] <- NA
				}
			} else{
				seg_rows_period[i, seg_rows_colname] <- NA
			}
		}
	}

	seg_sig_table <- rbind(seg_sig_table, seg_rows_period)

	return(seg_sig_table)
}
