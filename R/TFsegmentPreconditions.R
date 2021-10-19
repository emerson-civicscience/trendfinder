TFsegmentPreconditions <- function(bannerQuestion,
																	 segmentList,
																	 data_start_dates = data_start_dates,
																	 data_end_dates = data_end_dates,
																	 geography = USgeography,
																	 segmentPreconditions = NULL){

	bannerQuestion <- as.character(bannerQuestion)

	for(segmentLoop in segmentList){

		segment_and_scheme_name <- segmentLoop$segmentName

		segmentPreconditions <- rbind(segmentPreconditions,
																	tibble(start_date = data_start_dates,
																				 end_date = data_end_dates,
																				 stem = segment_and_scheme_name,
																				 banner = bannerQuestion,
																				 precondition = paste0(bannerQuestion,
																				 											":day>=",data_start_dates,"^",
																				 											bannerQuestion,
																				 											":day<=",data_end_dates,
																				 											"^",geography,"^",
																				 											segmentLoop$segmentDefinition),
																				 weighting_scheme = segment_and_scheme_name,
																				 weights = list(list(segmentLoop$segmentGender, segmentLoop$segmentAge))
																				 )
		)



	}

	return(segmentPreconditions)
}
