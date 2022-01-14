# Reusable file path maker
outputFilePathMaker <- function(file_location = NULL,
                           output_date = NULL){
  if(is.null(output_date)){
    output_date <- today()
  }

  if(is.null(file_location)){
    outputFilePath <- file.path("~/TrendFinder/Outputs", output_date)
  } else{
    outputFilePath <- file.path(file_location, "Outputs", output_date)
  }

  if(!dir.exists(outputFilePath)){
    dir.create(outputFilePath)
  }

  return(outputFilePath)
}
