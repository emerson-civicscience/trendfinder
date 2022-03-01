TFsegment <- function(segmentCondition, weightingDictSegments){

	bannerQuestion <- segmentCondition[1] %>%
		as.numeric()


	bannerIDs <- cs_get_question_metadata(bannerQuestion)$data %>%
		rownames(.) %>%
		as.numeric(.) %>%
		as.list(.)


	bannerNames <- as.character(bannerIDs)# Use answer choice IDs as group names

	segmentPrecondition <- segmentCondition[2] %>%
		as.character()

	scheme_name <- segmentCondition[3] %>%
		as.character()

	weights <- weightingDictSegments[which(weightingDictSegments$weighting_scheme == scheme_name),2][[1]][[1]]

	segmentResults <- lapply(segmentPrecondition, questionTable,
													 questionID = bannerQuestion,
													 answer.group.ids = bannerIDs,
													 answer.group.names = bannerNames,
													 weights = weights,
													 segmentText = scheme_name)

	segmentResults <- lapply(segmentResults, as.data.table) %>%
		do.call(rbind, .)

	segmentResults$weighting_scheme <- scheme_name
	
	current_time <- Sys.time()
	if(minute(current_time) == 0){
	  if(second(current_time) < 5){
	    print(paste0("TFsegment: question ", bannerQuestion, ", segment: ", scheme_name))
	  }
	}

	return(segmentResults)
}
