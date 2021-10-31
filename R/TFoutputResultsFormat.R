TFoutputResultsFormat <- function(outputResults,
																	batch_time = NULL){

	names(outputResults) <- gsub("data.", "", names(outputResults))
	names(outputResults) <- gsub("\\.", "_", names(outputResults))

	outputResults$batch <- batch_time

	setcolorder(outputResults, c("batch", "start_date", "end_date", "weighting_scheme",
																 "stem", "banner",
																 "response_count", "total_responses"))

	outputResults$stem <- gsub('c\\(', '', outputResults$stem) %>%
		gsub('\\)', '', .)
	outputResults$banner <- gsub('c\\(', '', outputResults$banner) %>%
		gsub('\\)', '', .)

	return(outputResults)

}
