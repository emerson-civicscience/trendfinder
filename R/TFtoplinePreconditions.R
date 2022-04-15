TFtoplinePreconditions <- function(questionRowTopline,
																	 data_start_dates = data_start_dates,
																	 data_end_dates = data_end_dates,
																	 geography = USgeography){

	questionID <- questionRowTopline[1]
	scheme_name <- questionRowTopline[2]

	# The way the API works, age/gender must be part of the precondition and not just the weighting scheme if the
	# weighting scheme contains zero values, otherwise it will give the correct proportions but overcount the respondents
	toplinePreconditions <- tibble(stem_start_date = NA,
	                               stem_end_date = NA,
	                               start_date = data_start_dates,
																 end_date = data_end_dates,
																 stem = "0",
																 banner = questionID,
																 precondition = paste0(as.character(questionID),
																 											":day>=",data_start_dates,"^",
																 											as.character(questionID),
																 											":day<=",data_end_dates,
																 											"^",geography),
																 weighting_scheme = scheme_name)


	return(toplinePreconditions)

}
