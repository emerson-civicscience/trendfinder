TFsegment <- function(segmentCondition, weighting_dict){

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

	weights <- weighting_dict[which(weighting_dict$weighting_scheme == scheme_name),2][[1]][[1]]

	segmentResults <- lapply(segmentPrecondition, questionTable,
													 questionID = bannerQuestion,
													 answer.group.ids = bannerIDs,
													 answer.group.names = bannerNames,
													 weights = weights,
													 segmentText = scheme_name)

	segmentResults <- lapply(segmentResults, as.data.table) %>%
		do.call(rbind, .)

	segmentResults$weighting_scheme <- scheme_name

	return(segmentResults)
}
