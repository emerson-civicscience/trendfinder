crosstabAgeTable <- function (precondition = NULL,
                           question.ids = NULL,
                           row.answer.group.ids = NULL,
                           row.answer.group.names = NULL,
                           col.answer.group.ids = NULL,
                           col.answer.group.names = NULL,
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

  if(grepl(":day<", precondition)){
    endDate <-   sub('.*:day<=', '', precondition) %>%
      substr(1, 10)
  } else{
    endDate <-   sub('.*:day<', '', precondition) %>%
      substr(1, 10)
      endDate <- as.Date(endDate) - 1
  }

  CGX <- error_crosstab

  tryCatch( {

    CGX <- cs_get_crosstab(question.ids, precondition, access.key = access.key,
                           secret.key = secret.key)

    CGX <- CGX$data

  }, error = function(e) {


  } )

    return(CGX)

}
