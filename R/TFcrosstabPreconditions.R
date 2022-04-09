TFcrosstabPreconditions <- function(crosstabRow,
                                    segment_list = NULL,
                                    stem_start_dates = stem_start_dates,
                                    stem_end_dates = stem_end_dates,
                                    data_start_dates = data_start_dates,
																		data_end_dates = data_end_dates,
																		geography = USgeography,
																		date_stem_and_banner = FALSE){
  
  # It's default to use the banner precondition because it's generally more useful, but in some cases it might be better
  # to date the crosstab by the stem or both.

	stemQuestion <- as.character(crosstabRow[1])
	bannerQuestion <- as.character(crosstabRow[2])
	scheme_name <- as.character(crosstabRow[3])

	if(is.null(stem_start_dates)){
	  stem_start_dates <- NA
	  stem_end_dates <- NA
	  
	  precondition <- paste0(bannerQuestion,
	                         ":day>=",data_start_dates, "^",
	                         bannerQuestion,
	                         ":day<=",data_end_dates, "^",
	                         geography)
	} else{
		if(date_stem_and_banner){
		  precondition <- paste0(stemQuestion,
		                         ":day>=",stem_start_dates, "^",
		                         stemQuestion,
		                         ":day<=",stem_end_dates, "^",
		                         bannerQuestion,
		                         ":day>=",data_start_dates, "^",
		                         bannerQuestion,
		                         ":day<=",data_end_dates, "^",
		                         geography)

		} else{
		  
		  precondition <- paste0(stemQuestion,
		                         ":day>=",stem_start_dates, "^",
		                         stemQuestion,
		                         ":day<=",stem_end_dates, "^",
		                         geography)
		}
	}
	
	crosstabPreconditions <- tibble(stem_start_date = stem_start_dates,
	                                stem_end_date = stem_end_dates,
	                                start_date = data_start_dates,
	                                end_date = data_end_dates,
	                                stem = stemQuestion,
	                                banner = bannerQuestion,
	                                precondition = precondition,
	                                weighting_scheme = scheme_name)

  return(crosstabPreconditions)

}
