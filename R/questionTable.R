questionTable <- function (precondition,
                           questionRow,
                           answer.group.ids = NULL,
                           answer.group.names = NULL,
                           weights = NULL,
                           segmentText = NULL,
                           access.key = CS_ACCESS_KEY,
                           secret.key = CS_SECRET_KEY)

  ### Mostly stolen from cs_create_question_barchart

{
  library(ggplot2)
  library(grid)
  library(gridExtra)
  library(lubridate)

  question.id <- c(questionRow[1], 484, 7078)

  columnOrder <- c("stem", "banner", "response.count")



  if(grepl(":day>=", precondition)){
    startDate <- sub('.*:day>=', '', precondition) %>%
      substr(1, 10)
  } else{
    startDate <- sub('.*:day>', '', precondition) %>%
      substr(1, 10) %>%
      as.Date(.) + 1
  }

  if(grepl(":day<=", precondition)){
    endDate <-   sub('.*:day<=', '', precondition) %>%
      substr(1, 10)
  } else{
    endDate <-   sub('.*:day<', '', precondition) %>%
      substr(1, 10) %>%
      as.Date(.) - 1
  }



  tryCatch( {
    result <- cs_get_crosstab(question.id, precondition)$data

    if(!is.null(weights)){
      data <- cs_weight_crosstab_data(result, weights=weights, cross = FALSE)
    } else {
      data <- NULL
      return(total_responses = as.numeric(aggregate(result$response.count, by = list(result$V3), FUN = sum)[2])) # This is just something for the question compendium
    }

      if(!is.null(data)){
        if (is.null(answer.group.ids)){

          names(data)[names(data) == "text.1"] <- "stem"
          names(data)[names(data) == "Row.names"] <- "banner"

          data$stem <- segmentText

          data <- data[, columnOrder]


        } else {
          data <- cs_cqb_ag(data,
                            answer.group.ids = answer.group.ids,
                            answer.group.names = answer.group.names)
          data$stem <- segmentText
          names(data)[names(data) == "text.1"] <- "banner"
          data <- data[, columnOrder]
        }

        total.responses <- sum(data$response.count, na.rm = T)

        return(list(startDate = startDate,
                    endDate = endDate,
                    data = data,
                    total.responses = total.responses))
      }
    }, error = function(e) {

    if (is.null(answer.group.ids)) {
      data <- cs_get_question_metadata(question.id[1])$data
      names(data)[names(data) == "response.text"] <- "stem"

      data$stem <- segmentText
      data$banner <- rownames(data)
      rownames(data) <- NULL
      data$response.count <- NA


    } else {
      data <- data.frame(matrix(ncol = 3, nrow = length(answer.group.names)))
      colnames(data) <- c("stem", "banner", "response.count")
      data$stem <- segmentText
      data$banner <- answer.group.names

    }

    return(list(startDate = startDate,
                endDate = endDate,
                data = data,
                total.responses = NA))

  } )

}
