TFoutputResultsFormat <- function(outputResults,
																	batch_time = NULL){

	names(outputResults) <- gsub("data.", "", names(outputResults))
	names(outputResults) <- gsub("\\.", "_", names(outputResults))

	setcolorder(outputResults, c("start_date", "end_date", "weighting_scheme",
																 "stem", "banner",
																 "response_count"))

	outputResults$stem <- gsub('c\\(', '', outputResults$stem) %>%
		gsub('\\)', '', .)
	outputResults$banner <- gsub('c\\(', '', outputResults$banner) %>%
		gsub('\\)', '', .)

	outputResults$batch <- batch_time

	outputColumnOrder <- c(ncol(outputResults), 1:(ncol(outputResults)-1))

	outputResults <- outputResults[, outputColumnOrder]

	return(outputResults)

}
