TFtopline <- function(toplineCondition){

  toplineQuestion <- toplineCondition[1] %>%
    as.numeric()

  # Assigning IDs to rows of the results will not be necessary once the ability to put answer groupings into
  # the TrendFinder questionTable and crosstabTable makers are removed in favor of grouping the results
  # after pulling the weighted results

  scheme_name <- toplineCondition[3] %>%
    as.character()

  weights <- weighting_dict[which(weighting_dict$scheme_name == scheme_name),2][[1]][[1]]

  age_gender_precondition <- TFageGenderPrecondition(weights)

  toplinePrecondition <- paste(c(age_gender_precondition, toplineCondition[2]), collapse="^")

  if(is.na(toplinePrecondition)){
    toplinePrecondition <- NULL
  }


  # Initially wanted the ability to put answer group names into the API, but since answer groupings
  # are now handled ad hoc after computation of ungrouped results, they are not needed
  # toplineIDs <- cs_get_question_metadata(toplineQuestion)$data %>%
  #   rownames(.) %>%
  #   as.numeric(.) %>%
  #   as.list(.)
  #
  #
  # toplineNames <- as.character(toplineIDs)# Use answer choice IDs as group names


  toplineResults <- lapply(toplinePrecondition, questionTable,
                           questionID = toplineQuestion,
                           # answer.group.ids = toplineIDs,
                           # answer.group.names = toplineNames,
                           weights = weights,
                           segmentText = "0")

  toplineResults <- lapply(toplineResults, as.data.table) %>%
    do.call(rbind, .)

  toplineResults$weighting_scheme <- scheme_name
  
  current_time <- Sys.time()
  if(minute(current_time) == 0){
    if(second(current_time) < 5){
      print("TFtopline: question ", toplineQuestion)
    }
  }

  return(toplineResults)
}
