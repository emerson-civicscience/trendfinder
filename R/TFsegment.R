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
	
	#' Function which prints a message using shell echo; useful for printing messages from inside mclapply when running in Rstudio
	system(sprintf('echo "\n%s\n"', paste0(segmentCondition, collapse=" - ")))

	return(segmentResults)
}
