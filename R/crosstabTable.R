crosstabTable <- function (precondition = NULL,
                           question.ids = NULL,
                           start_date = start_date,
                           end_date = end_date,
                           row.answer.group.ids = NULL,
                           row.answer.group.names = NULL,
                           col.answer.group.ids = NULL,
                           col.answer.group.names = NULL,
                           weights = NULL,
                           access.key = CS_ACCESS_KEY,
                           secret.key = CS_SECRET_KEY)

  ### Mostly stolen from cs_create_crosstab_barchart

{

  # if(grepl(":day>=", precondition)){
  #   start_date <- sub('.*:day>=', '', precondition) %>%
  #     substr(1, 10)
  # } else{
  #   start_date <- sub('.*:day>=', '', precondition) %>%
  #     substr(1, 10)
  # }
  # 
  # if(grepl(":day<=", precondition)){
  #   end_date <-   sub('.*:day<=', '', precondition) %>%
  #     substr(1, 10)
  # } else{
  #   end_date <-   sub('.*:day<=', '', precondition) %>%
  #     substr(1, 10)
  # }
  
  tryCatch( {

    
    if(question.ids[1] == question.ids[2]){
      'y'+'z' # Force error to send to `error = function(e)`
    }
    
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
      
      data <- cs_ccb_ag(data,
                        row.answer.group.ids,
                        row.answer.group.names,
                        col.answer.group.ids,
                        col.answer.group.names)
      
    } else {
      data <- cs_weight_crosstab_data(CGX$data, weights, cross = TRUE)

      # Check here for what is passed when there's no answer choice IDs
      # data <- cs_ccb_ag(data,
      #                   row.answer.group.ids,
      #                   row.answer.group.names,
      #                   col.answer.group.ids,
      #                   col.answer.group.names)


      # If there are 0 responses for a row, the API helpfully* doesn't return a row    *Helpfully is sarcasm
      # May have to check against the all-time version from the error function and rbind rows that don't exist to this one...
      # although, I think for all intents and purposes it won't matter as long as your zero out any NA cells after "widening"
      # the response table
    }

    names(data)[names(data) == "answer.choice.id.1"] <- "stem"
    names(data)[names(data) == "answer.choice.id.2"] <- "banner"

    data <- data[, c("stem", "banner", "response.count")]

    # total_responses <- sum(data$response.count, na.rm = T)

    return(list(start_date = start_date,
                end_date= end_date,
                data = data#,
                # total.responses = total_responses
           ))

  }, error = function(e) {

    data <- expand.grid(row.answer.group.ids, col.answer.group.ids)
    colnames(data) <- c("stem", "banner")
    data$response.count <- NA


    return(list(start_date = start_date,
                end_date= end_date,
                data = data#,
                # total.responses = NA
           ))
  } )
}
