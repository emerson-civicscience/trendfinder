TFcrosstabPreconditions <- function(crosstabRow,
																		data_start_dates = data_start_dates,
																		data_end_dates = data_end_dates,
																		geography = USgeography,
																		useStemAndBannerPrecondition = NULL,
																		useStemPrecondition = NULL){

	stemQuestion <- as.character(crosstabRow[1])
	bannerQuestion <- as.character(crosstabRow[2])
	scheme_name <- as.character(crosstabRow[3])


	# It's default to use the banner precondition because it's generally more useful, but in some cases it might be better
	# to date the crosstab by the stem or both. This code and the crosstabTable function do not currently support different
	# dates for the stem and banner. They could, but the use cases seem limited at this time.
	if(is.null(useStemAndBannerPrecondition)){
		if(is.null(useStemPrecondition)){
			crosstabPreconditions <- tibble(start_date = data_start_dates,
																			end_date = data_end_dates,
																			stem = stemQuestion,
																			banner = bannerQuestion,
																			precondition = paste0(bannerQuestion,
																														":day>=",data_start_dates, "^",
																														bannerQuestion,
																														":day<=",data_end_dates, "^",
																														geography),
																			weighting_scheme = scheme_name)

		} else{

			crosstabPreconditions <- tibble(start_date = data_start_dates,
																			end_date = data_end_dates,
																			stem = stemQuestion,
																			banner = bannerQuestion,
																			precondition = paste0(stemQuestion,
																														":day>=",data_start_dates, "^",
																														stemQuestion,
																														":day<=",data_end_dates, "^",
																														geography),
																			weighting_scheme = scheme_name)

		}
	} else{

		crosstabPreconditions <- tibble(start_date = data_start_dates,
																		end_date = data_end_dates,
																		stem = stemQuestion,
																		banner = bannerQuestion,
																		precondition = paste0(stemQuestion,
																													":day>=",data_start_dates, "^",
																													stemQuestion,
																													":day<=",data_end_dates, "^",
																													bannerQuestion,
																													":day>=",data_start_dates, "^",
																													bannerQuestion,
																													":day<=",data_end_dates, "^",
																													geography),
																		weighting_scheme = scheme_name)
	}



	return(crosstabPreconditions)

}
