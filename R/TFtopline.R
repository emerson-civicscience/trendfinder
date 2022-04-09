TFtopline <- function(toplineCondition){

  toplineQuestion <- toplineCondition[1] %>%
    as.numeric()

    scheme_name <- toplineCondition[3] %>%
    as.character()

  weights <- weighting_dict[which(weighting_dict$scheme_name == scheme_name),2][[1]][[1]]

  age_gender_precondition <- TFageGenderPrecondition(weights)

  toplinePrecondition <- paste(c(age_gender_precondition, toplineCondition[2]), collapse="^")

  if(is.na(toplinePrecondition)){
    toplinePrecondition <- NULL
  }

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
      print(paste0("TFtopline: question ", toplineQuestion))
    }
  }

  return(toplineResults)
}
