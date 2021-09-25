TFcrosstabAlly <- function(crosstabRow,
                       data_start_dates,
                       data_end_dates,
                       batchTime,
                       crosstabResults = NULL,
                       fulLCrosstabResults = NULL,
                       geography = USgeography,
                       weights = NULL,
                       useStemAndBannerPrecondition = NULL,
                       useStemPrecondition = NULL,
                       multiple_weightings = NULL){

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
  if (stemQuestion == 138436){
    crosstabPrecondition <- paste0(as.character(stemQuestion),
                                   ":day>=",data_start_dates, "^",
                                   "7078!=29516^",
                                   as.character(stemQuestion),
                                   ":day<",data_end_dates,"^",
                                   as.character(bannerQuestion),
                                   ":day>=",data_start_dates,"^",
                                   as.character(bannerQuestion),
                                   ":day<",data_end_dates, "^",
                                   geography)

    weights = USadultWeighting
    # if (bannerQuestion == 42887){
    #   weights = US25orOlderWeighting
    # }
    # if (bannerQuestion == 42892){
    #   weights = US25orOlderWeighting
    # }
  }

  if (stemQuestion == 2793){
    crosstabPrecondition <- paste0("7078:day>=",data_start_dates, "^",
                                   "7078:day<",data_end_dates,"^",
                                   "7078=30024;30025;29520^",
                                   as.character(bannerQuestion),
                                   ":day>=",data_start_dates,"^",
                                   as.character(bannerQuestion),
                                   ":day<",data_end_dates, "^",
                                   geography)

    weights = US25to44weighting
  }


  if (stemQuestion == 135934){
    crosstabPrecondition <- paste0("7078:day>=",data_start_dates, "^",
                                   "7078:day<",data_end_dates,"^",
                                   "7078=30025;29520;29521^",
                                   as.character(bannerQuestion),
                                   ":day>=",data_start_dates,"^",
                                   as.character(bannerQuestion),
                                   ":day<",data_end_dates, "^",
                                   geography)

    multiple_weightings <- 2

    multiple_weightings_ref <- list(US25to44weighting, US25to34weighting)

  }

  if(is.null(multiple_weightings)){

    crosstabResults <- lapply(crosstabPrecondition, crosstabTable,
                              question.ids = c(stemQuestion, bannerQuestion, 484, 7078),
                              row.answer.group.ids = stemIDs,
                              row.answer.group.names = stemNames,
                              col.answer.group.ids = bannerIDs,
                              col.answer.group.names = bannerNames,
                              weights = weights)

  } else{
    for(i in 1:multiple_weightings){

      weights = multiple_weightings_ref[[i]]

      crosstabResults <- lapply(crosstabPrecondition, crosstabTable,
                                question.ids = c(stemQuestion, bannerQuestion, 484, 7078),
                                row.answer.group.ids = stemIDs,
                                row.answer.group.names = stemNames,
                                col.answer.group.ids = bannerIDs,
                                col.answer.group.names = bannerNames,
                                weights = weights)
    }

    # Need to add weighting scheme info to results!

  }



  crosstabResults <- lapply(crosstabResults, as.data.table) %>%
    do.call(rbind, .) %>%
    setcolorder(., c("startDate", "endDate",
                     "data.stem", "data.banner",
                     "data.response.count", "total.responses"))
  names(crosstabResults) <- gsub("data.", "", names(crosstabResults))


  return(crosstabResults)

}
