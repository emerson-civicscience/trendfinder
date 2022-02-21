# Reusable file name maker
outputName <- function(outputType = NULL,
                       batch_time = NULL) {

  outputFilePath <- outputFilePathMaker()

  batch_time_file_name <- batch_time %>%
    format(.,usetz = TRUE) %>%
    str_replace_all(':', '_')

  if(is.null(batch_time)){
    return(file.path(outputFilePath,
                     paste0(outputType, " - No Batch Time")))
  } else{
  return(file.path(outputFilePath,
                   paste0(outputType, " - Batch Time ", batch_time_file_name)))
  }
}
