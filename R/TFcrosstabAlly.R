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

  scheme_name <- crosstabRow[3]

  weights <- weightingDict[which(weightingDict$scheme_name == scheme_name),2][[1]][[1]]

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

  }


  if (stemQuestion == 135934){
    if(scheme_name == 'US25to34weighting'){
        crosstabPrecondition <- paste0("7078:day>=",data_start_dates, "^",
                                       "7078:day<",data_end_dates,"^",
                                       "7078=30024;30025^",
                                       as.character(bannerQuestion),
                                       ":day>=",data_start_dates,"^",
                                       as.character(bannerQuestion),
                                       ":day<",data_end_dates, "^",
                                       geography)
    }

    if(scheme_name == 'US30to54weighting'){
      crosstabPrecondition <- paste0("7078:day>=",data_start_dates, "^",
                                     "7078:day<",data_end_dates,"^",
                                     "7078=30025;29520;29521^",
                                     as.character(bannerQuestion),
                                     ":day>=",data_start_dates,"^",
                                     as.character(bannerQuestion),
                                     ":day<",data_end_dates, "^",
                                     geography)
    }





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
