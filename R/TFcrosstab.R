TFcrosstab <- function(crosstabCondition,
                       # data_start_dates,
                       # data_end_dates,
                       weighting_dict = weighting_dict){

  stemQuestion <- as.numeric(crosstabCondition[1])
  bannerQuestion <- as.numeric(crosstabCondition[2])

  # Initially wanted the ability to put answer group names into the API, but since answer groupings
  # are now handled ad hoc after computation of ungrouped results, they are only needed for error handling
  # (can (and probably should) be moved to crosstabTable()).
  stemIDs <- cs_get_question_metadata(stemQuestion,
                                      access.key = CS_ACCESS_KEY,
                                      secret.key = CS_SECRET_KEY)$data %>%
    rownames(.) %>%
    as.numeric(.) %>%
    as.list(.)

  stemNames <- as.character(stemIDs)# Use answer choice IDs as group names


  bannerIDs <- cs_get_question_metadata(bannerQuestion)$data %>%
    rownames(.) %>%
    as.numeric(.) %>%
    as.list(.)

  bannerNames <- as.character(bannerIDs)# Use answer choice IDs as group names

  crosstabPrecondition <- crosstabCondition[3]

  scheme_name <- as.character(crosstabCondition[4])
  
  weights <- weighting_dict[which(weighting_dict$scheme_name == scheme_name),2][[1]][[1]]

  age_gender_precondition <- TFageGenderPrecondition(weights)

  if(!is.null(age_gender_precondition)){
    crosstabPrecondition <- paste0(age_gender_precondition, "^", crosstabPrecondition)
  }

  crosstabResults <- lapply(crosstabPrecondition, crosstabTable,
                            question.ids = c(stemQuestion, bannerQuestion, 484, 7078),
                            row.answer.group.ids = stemIDs,
                            row.answer.group.names = stemNames,
                            col.answer.group.ids = bannerIDs,
                            col.answer.group.names = bannerNames,
                            weights = weights)

  crosstabResults <- lapply(crosstabResults, as.data.table) %>%
    do.call(rbind, .)

  crosstabResults$data.stem <- as.character(crosstabResults$data.stem) # Because TFtopline and TFsegment both drop in a character
  # for their 'stem' value, convert this to a character before returning

  crosstabResults$weighting_scheme <- scheme_name
  
  # Print occasionally if still working?
  
  current_time <- Sys.time()
  if(minute(current_time) == 0){
    if(second(current_time) < 5){
      print("TFcrosstab: questions ", stemQuestion, " & ", bannerQuestion, ", weighting_scheme ", scheme_name)
    }
  }
  
  return(crosstabResults)

}
