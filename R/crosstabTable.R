crosstabTable <- function (precondition = NULL,
                           question.ids = NULL,
                           row.answer.group.ids = NULL,
                           row.answer.group.names = NULL,
                           col.answer.group.ids = NULL,
                           col.answer.group.names = NULL,
                           weights = NULL,
                           access.key = CS_ACCESS_KEY,
                           secret.key = CS_SECRET_KEY)

  ### Mostly stolen from cs_create_crosstab_barchart

{
  library(dplyr)
  library(ggplot2)
  library(gridExtra)
  library(lubridate)

  if(grepl(":day>=", precondition)){
    startDate <- sub('.*:day>=', '', precondition) %>%
      substr(1, 10)
  } else{
    startDate <- sub('.*:day>', '', precondition) %>%
      substr(1, 10)
  }

  if(grepl(":day<=", precondition)){
    endDate <-   sub('.*:day<=', '', precondition) %>%
      substr(1, 10)
  } else{
    endDate <-   sub('.*:day<', '', precondition) %>%
      substr(1, 10)
  }

  endDate <- as.Date(endDate) - 1

  tryCatch( {

    CGX <- cs_get_crosstab(question.ids, precondition, access.key = access.key,
                           secret.key = secret.key)

    if(is.null(weights)){

      data <- CGX$data %>%
        dplyr::group_by(V1, V2, text.1, text.2) %>%
        dplyr::rename(answer.choice.id.1 = V1,
                      answer.choice.id.2 = V2) %>%
        dplyr::summarise(response.count = round(sum(response.count), 0)) %>%
        dplyr::ungroup() %>%
        dplyr::arrange(text.1,
                       text.2)

      data <- cs_ccb_ag(data, row.answer.group.ids, row.answer.group.names,
                        col.answer.group.ids, col.answer.group.names)




    } else {
      data <- cs_weight_crosstab_data(CGX$data, weights, cross = TRUE)

      # Check here for what is passed when there's no answer choice IDs
      data <- cs_ccb_ag(data, row.answer.group.ids, row.answer.group.names,
                          col.answer.group.ids, col.answer.group.names)


      # If there are 0 responses for a row, the API helpfully* doesn't return a row    *Helpfully is sarcasm
      # May have to check against the all-time version from the error function and rbind rows that don't exist to this one...
      # although, I think for all intents and purposes it won't matter as long as your zero out any NA cells after "widening"
      # the response table
    }

    names(data)[names(data) == "text.1"] <- "stem"
    names(data)[names(data) == "text.2"] <- "banner"

    total_responses <- sum(data$response.count, na.rm = T)

    return(list(startDate = startDate,
                endDate= endDate,
                data = data,
                total.responses = total_responses))

  }, error = function(e) {

    CGX <- cs_get_crosstab(question.ids, access.key = access.key,
                           secret.key = secret.key)
    data <- data.frame(answer.choice.id.1 = CGX$data$V1,
                       answer.choice.id.2 = CGX$data$V2, text.1 = CGX$data$text.1,
                       text.2 = CGX$data$text.2, response.count = 0)

    data <- cs_ccb_ag(data, row.answer.group.ids, row.answer.group.names,
                      col.answer.group.ids, col.answer.group.names)


    names(data)[names(data) == "text.1"] <- "stem"
    names(data)[names(data) == "text.2"] <- "banner"

    return(list(startDate = startDate,
                endDate= endDate,
                data = data,
                total.responses = NA))
  } )
}
