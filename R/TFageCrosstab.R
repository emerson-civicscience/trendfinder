TFageCrosstab <- function(input_row){

  banner_question <- input_row[1]
  start_date <- input_row[2]
  end_date <- input_row[3]
  # start_date <- as.Date(input_row[2])
  # end_date <- as.Date(input_row[3])

  crosstabPrecondition <- paste0(banner_question, ":day>=", start_date,"^",
                                 banner_question,":day<", end_date,
                                 "^484=",c("1536", "1537"))

  crosstab_age <- lapply(crosstabPrecondition, crosstabAgeTable,
                         question.ids = c(7078, as.numeric(banner_question)))

  crosstab_age[[1]] <- crosstab_age[[1]][,c(1, 2, 4, 5, 6, 3)]
  crosstab_age[[1]][,1] <- banner_question
  crosstab_age[[1]][,2] <- start_date
  crosstab_age[[1]][,3] <- end_date
  crosstab_age[[1]][,4] <- "Male"
  crosstab_age[[2]] <- crosstab_age[[2]][,c(1, 2, 4, 5, 6, 3)]
  crosstab_age[[2]][,1] <- banner_question
  crosstab_age[[2]][,2] <- start_date
  crosstab_age[[2]][,3] <- end_date
  crosstab_age[[2]][,4] <- "Female"

  tryCatch({

    crosstabResults <- rbind(crosstab_age[[1]], crosstab_age[[2]]) %>%
      data.table()

    crosstabResults <- aggregate(crosstabResults$response.count, by = list(Gender = crosstabResults$response.text.2, Age = crosstabResults$text.1), FUN = sum)

    colnames(crosstabResults)[which(colnames(crosstabResults) == 'x')] <- "Fulfilled"

    # crosstabResults$Fulfilled <- as.numeric(crosstabResults$Fulfilled)

  }, error = function(e) {

    crosstabResults <- crosstab_age


  })

  crosstabResults$`Question ID` <- banner_question
  crosstabResults$`Start Date` <- start_date
  crosstabResults$`End Date` <- end_date


  column_order_crosstab <- c("Question ID", "Start Date", "End Date", "Gender", "Age", "Fulfilled")

  crosstabResults <- crosstabResults[, column_order_crosstab]


  return(crosstabResults)

}
