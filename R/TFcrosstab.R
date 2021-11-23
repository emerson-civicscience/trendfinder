TFcrosstab <- function(crosstabCondition,
                       data_start_dates,
                       data_end_dates){

  stemQuestion <- as.numeric(crosstabCondition[1])

  stemIDs <- cs_get_question_metadata(stemQuestion)$data %>%
      rownames(.) %>%
      as.numeric(.) %>%
      as.list(.)


  stemNames <- as.character(stemIDs)# Use answer choice IDs as group names

  bannerQuestion <- as.numeric(crosstabCondition[2])

  bannerIDs <- cs_get_question_metadata(bannerQuestion)$data %>%
      rownames(.) %>%
      as.numeric(.) %>%
      as.list(.)

  bannerNames <- as.character(bannerIDs)# Use answer choice IDs as group names

  crosstabPrecondition <- crosstabCondition[3]

  scheme_name <- as.character(crosstabCondition[4])
  weights <- weightingDict[which(weightingDict$scheme_name == scheme_name),2][[1]][[1]]

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

  # crosstabResults$data.stem <- as.numeric(crosstabResults$data.stem)
  # crosstabResults$data.banner <- as.numeric(crosstabResults$data.banner)
  crosstabResults$weighting_scheme <- scheme_name

  # crosstabResults <- as.list(crosstabResults)

  # crosstabResults$end_date <- as.character(crosstabResults$end_date)

  return(crosstabResults)

}
