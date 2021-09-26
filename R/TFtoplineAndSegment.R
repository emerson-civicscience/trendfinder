TFtoplineAndSegment <- function(questionRow,
                                runTopline = runTopline,
                                segmentList = NULL,
                                data_start_dates,
                                data_end_dates,
                                batchTime,
                                toplineResults = NULL,
                                fullToplineResults = NULL,
                                segmentResults = NULL,
                                fullSegmentResults = NULL,
                                geography = USgeography){

        questionID <- questionRow[1] %>%
          as.numeric()
        toplineIDs <- groupIDlist[[questionID]] # Grab answer choice IDs from groupIDlist

        scheme_name <- questionRow[2]

        weights <- weightingDict[which(weightingDict$scheme_name == scheme_name),2][[1]][[1]]

        if(is.null(toplineIDs)){
          bannerIDs <- cs_get_question_metadata(bannerQuestion)$data %>%
            rownames(.) %>%
            as.numeric(.) %>%
            as.list(.)
        }

        toplineNames <- as.character(toplineIDs)# Use answer choice IDs as group names

        if(!is.null(runTopline)){

          # The way the API works, age/gender must be part of the precondition and not just the weighting scheme if the
          # weighting scheme contains zero values, otherwise it will give the correct proportions but overcount the respondents

          age_gender_precondition <- TFageGenderPrecondition(weights)


          toplinePrecondition <- paste0(as.character(questionID),
                                        ":day>=",data_start_dates,"^",
                                        as.character(questionID),
                                        ":day<",data_end_dates,
                                        "^",geography)

          if(!is.null(age_gender_precondition)){
            toplinePrecondition <- paste0(age_gender_precondition, "^", toplinePrecondition)
          }

          toplineResults <- lapply(toplinePrecondition, questionTable,
                                   questionRow = questionID,
                                   answer.group.ids = toplineIDs,
                                   answer.group.names = toplineNames,
                                   weights = weights,
                                   segmentText = 0)
        }

        if(!is.null(segmentList)){
          for(segmentLoop in 1:length(segmentList)){

            # There needs to be a way to check if any of the preconditions have been previously run for a question.
            # Check it against the all-time data set and remove that precondition if necessary.
            segmentPrecondition <- paste0(as.character(questionID),
                                          ":day>=",data_start_dates,"^",
                                          as.character(questionID),
                                          ":day<",data_end_dates,
                                          "^",geography,"^",
                                          segmentList[[segmentLoop]]$segmentDefinition)

            segmentWeights = list(segmentList[[segmentLoop]]$segmentGender, segmentList[[segmentLoop]]$segmentAge)
            segmentText = segmentList[[segmentLoop]]$segmentName

            segmentResults <- lapply(segmentPrecondition, questionTable,
                                     questionRow = questionID,
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
    do.call(rbind, .)
  names(toplineAndSegmentResults) <- gsub("data.", "", names(toplineAndSegmentResults))

  toplineAndSegmentResults$weights <- scheme_name

  setcolorder(toplineAndSegmentResults, c("startDate", "endDate", "weights",
                                          "stem", "banner",
                                          "response.count", "total.responses"))

  return(toplineAndSegmentResults)
}
