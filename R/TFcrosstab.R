TFcrosstab <- function(crosstabRow,
                       data_start_dates,
                       data_end_dates,
                       batchTime,
                       crosstabResults = NULL,
                       fullCrosstabResults = NULL,
                       geography = USgeography,
                       useStemAndBannerPrecondition = NULL,
                       useStemPrecondition = NULL){

  crosstabRow <- unlist(crosstabRow)

  stemQuestion <- as.numeric(crosstabRow[1])
  stemIDs <- groupIDlist[[stemQuestion]] # Grab answer choice IDs from groupIDlist

  if(is.null(stemIDs)){
    stemIDs <- cs_get_question_metadata(stemQuestion)$data %>%
      rownames(.) %>%
      as.numeric(.) %>%
      as.list(.)
  }

  stemNames <- as.character(stemIDs)# Use answer choice IDs as group names

  bannerQuestion <- as.numeric(crosstabRow[2])
  bannerIDs <- groupIDlist[[bannerQuestion]] # Grab answer choice IDs from groupIDlist

  if(is.null(bannerIDs)){
    bannerIDs <- cs_get_question_metadata(bannerQuestion)$data %>%
      rownames(.) %>%
      as.numeric(.) %>%
      as.list(.)
  }

  bannerNames <- as.character(bannerIDs)# Use answer choice IDs as group names

  # Its default to use the banner precondition because it's generally more useful, but in some cases it might be better
  # to date the crosstab by the stem or both. This code and the crosstabTable function do not currently support different
  # dates for the stem and banner. They could, but the use cases seem limited at this time.
  if(is.null(useStemAndBannerPrecondition)){
    if(is.null(useStemPrecondition)){
      crosstabPrecondition <- paste0(as.character(bannerQuestion),
                                     ":day>=",data_start_dates, "^",
                                     as.character(bannerQuestion),
                                     ":day<",data_end_dates, "^",
                                     geography)

    } else{
      crosstabPrecondition <- paste0(as.character(stemQuestion),
                                     ":day>=",data_start_dates, "^",
                                     as.character(stemQuestion),
                                     ":day<",data_end_dates,"^",
                                     geography)
    }
  } else{
    crosstabPrecondition <- paste0(as.character(stemQuestion),
                                   ":day>=",data_start_dates, "^",
                                   as.character(stemQuestion),
                                   ":day<",data_end_dates,"^",
                                   as.character(bannerQuestion),
                                   ":day>=",data_start_dates,"^",
                                   as.character(bannerQuestion),
                                   ":day<",data_end_dates, "^",
                                   geography)
  }

  scheme_name <- as.character(crosstabRow[3])
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
  names(crosstabResults) <- gsub("data.", "", names(crosstabResults))

  crosstabResults$weights <- scheme_name

  setcolorder(crosstabResults, c("startDate", "endDate", "weights",
                                 "stem", "banner",
                                 "response.count", "total.responses"))

  return(crosstabResults)

}
