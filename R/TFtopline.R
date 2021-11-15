TFtopline <- function(toplineCondition){

  toplineQuestion <- toplineCondition[1] %>%
    as.numeric()

  # Assigning IDs to rows of the results will not be necessary once the ability to put answer groupings into
  # the TrendFinder questionTable and crosstabTable makers are removed in favor of grouping the results
  # after pulling the weighted results

  scheme_name <- toplineCondition[3] %>%
    as.character()

  weights <- weightingDict[which(weightingDict$scheme_name == scheme_name),2][[1]][[1]]

  age_gender_precondition <- TFageGenderPrecondition(weights)

  toplinePrecondition <- paste(c(age_gender_precondition, toplineCondition[2]), collapse="^")

  if(is.na(toplinePrecondition)){
    toplinePrecondition <- NULL
  }


  toplineIDs <- cs_get_question_metadata(toplineQuestion)$data %>%
    rownames(.) %>%
    as.numeric(.) %>%
    as.list(.)


  toplineNames <- as.character(toplineIDs)# Use answer choice IDs as group names


  toplineResults <- lapply(toplinePrecondition, questionTable,
                                     questionID = toplineQuestion,
                                     answer.group.ids = toplineIDs,
                                     answer.group.names = toplineNames,
                                     weights = weights,
                                     segmentText = "0")

  toplineResults <- lapply(toplineResults, as.data.table) %>%
    do.call(rbind, .)

  toplineResults$weighting_scheme <- scheme_name

  return(toplineResults)
}
