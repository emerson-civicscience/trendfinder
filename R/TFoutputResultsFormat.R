TFoutputResultsFormat <- function(outputResults,
                                  stem_start_dates,
                                  stem_end_dates,
                                  data_start_dates,
                                  data_end_dates,
																	batch_time = NULL){

	names(outputResults) <- gsub("data.", "", names(outputResults))
	names(outputResults) <- gsub("\\.", "_", names(outputResults))

	outputResults$batch <- batch_time
	
	outputResults$stem_start_date <- "NA"
	outputResults$stem_end_date <- "NA"
	
	if(!is.null(stem_start_dates)){
	  for(i in 1:nrow(outputResults)){
	    if(outputResults$stem != 0){
	      outputResults$stem_start_date[i] <- stem_start_dates[which(data_start_dates == outputResults$start_date[i])]
	      outputResults$stem_end_date[i] <- stem_end_dates[which(data_start_dates == outputResults$start_date[i])]
	      
	    }
	  }
	} 

	setcolorder(outputResults, c("batch", 
	                             "stem_start_date", "stem_end_date", 
	                             "start_date", "end_date", 
	                             "weighting_scheme", "stem", "banner",
	                             "response_count"#, "total_responses"
	                             ))

	outputResults$stem <- gsub('c\\(', '', outputResults$stem) %>%
		gsub('\\)', '', .)
	outputResults$banner <- gsub('c\\(', '', outputResults$banner) %>%
		gsub('\\)', '', .)

	return(outputResults)

}
