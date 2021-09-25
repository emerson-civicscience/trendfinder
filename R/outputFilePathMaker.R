# Reusable file path maker
outputFilePathMaker <- function(fileLocation = NULL,
                           outputDate = NULL){
  if(is.null(outputDate)){
    outputDate <- today()
  }

  if(is.null(fileLocation)){
    outputFilePath <- file.path("~/TrendFinder/Outputs", outputDate)
  } else{
    outputFilePath <- file.path(fileLocation, "Outputs", outputDate)
  }

  if(!dir.exists(outputFilePath)){
    dir.create(outputFilePath)
  }

  return(outputFilePath)
}


