TFcrosstabQuestionHandler <- function(stem_questions,
                                     banner_questions,
                                     crosstab_input){

  if(is.null(crosstab_input)){
    if(!is.null(stem_questions) && !is.null(banner_questions)){

      crosstab_input <- expand.grid(stem_questions, banner_questions)
      colnames(crosstab_input) <- c('seg', 'com')

    }
  }

  return(crosstab_input)

}
