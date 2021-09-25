TFtoplineAndSegment <- function(toplineQuestion,
                                runTopline = runTopline,
                                segmentList = NULL,
                                data_start_dates,
                                data_end_dates,
                                batchTime,
                                toplineResults = NULL,
                                fullToplineResults = NULL,
                                segmentResults = NULL,
                                fullSegmentResults = NULL,
                                geography = USgeography,
                                weights = NULL){


        toplineIDs <- groupIDlist[[toplineQuestion]] # Grab answer choice IDs from groupIDlist

        if(is.null(toplineIDs)){
          bannerIDs <- cs_get_question_metadata(bannerQuestion)$data %>%
            rownames(.) %>%
            as.numeric(.) %>%
            as.list(.)
        }

        toplineNames <- as.character(toplineIDs)# Use answer choice IDs as group names

        if(!is.null(runTopline)){

          # There needs to be a way to check if any of the preconditions have been previously run for a question.
          # Check it against the all-time data set and remove that precondition if necessary.
          toplinePrecondition <- paste0(as.character(toplineQuestion),
                                        ":day>=",data_start_dates,"^",
                                        as.character(toplineQuestion),
                                        ":day<",data_end_dates,
                                        "^",geography)

          toplineResults <- lapply(toplinePrecondition, questionTable,
                                   toplineQuestion = toplineQuestion,
                                   answer.group.ids = toplineIDs,
                                   answer.group.names = toplineNames,
                                   weights = weights,
                                   segmentText = 0)
        }

        if(!is.null(segmentList)){
          for(segmentLoop in 1:length(segmentList)){

            # There needs to be a way to check if any of the preconditions have been previously run for a question.
            # Check it against the all-time data set and remove that precondition if necessary.
            segmentPrecondition <- paste0(as.character(toplineQuestion),
                                          ":day>=",data_start_dates,"^",
                                          as.character(toplineQuestion),
                                          ":day<",data_end_dates,
                                          "^",geography,"^",
                                          segmentList[[segmentLoop]]$segmentDefinition)

            segmentWeights = list(segmentList[[segmentLoop]]$segmentGender, segmentList[[segmentLoop]]$segmentAge)
            segmentText = segmentList[[segmentLoop]]$segmentName

            segmentResults <- lapply(segmentPrecondition, questionTable,
                                     toplineQuestion = toplineQuestion,
                                     answer.group.ids = toplineIDs,
                                     answer.group.names = toplineNames,
                                     weights = segmentWeights,
                                     segmentText = segmentText)

            if(is.null(fullSegmentResults)){
              fullSegmentResults <- segmentResults
            } else{
              fullSegmentResults <- c(fullSegmentResults, segmentResults)
            }
          }
        }

  toplineAndSegmentResults <- c(toplineResults, fullSegmentResults)

  toplineAndSegmentResults <- lapply(toplineAndSegmentResults, as.data.table) %>%
    do.call(rbind, .) %>%
    setcolorder(., c("startDate", "endDate",
                     "data.stem", "data.banner",
                     "data.response.count", "total.responses"))
  names(toplineAndSegmentResults) <- gsub("data.", "", names(toplineAndSegmentResults))

    return(toplineAndSegmentResults)
}
