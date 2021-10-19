# Reusable file name maker
outputName <- function(outputType = NULL,
                       batch_time = NULL) {

  batch_time_file_name <- batchTime %>%
    as.character() %>%
    str_replace_all(':', '_') %>%
    paste0(., " EDT")

  if(is.null(batch_time)){
    return(file.path(outputFilePath,
                     paste0(outputType, " - No Batch Time")))
  } else{
  return(file.path(outputFilePath,
                   paste0(outputType, " - Batch Time ", batch_time_file_name)))
  }
}
