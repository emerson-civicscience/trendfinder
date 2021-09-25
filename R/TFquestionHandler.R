TFtoplineQuestionHandler <- function(stem_questions,
                          banner_questions,
                          crosstab_input){

  question_list <- c(stem_questions, banner_questions) %>%
    unique() %>%
    sort()

  if(is.null(crosstab_input)){
    if(!is.null(stem_questions) && !is.null(banner_questions)){

      crosstab_input <- expand.grid(stem_questions, banner_questions)
      colnames(crosstab_input) <- c('seg', 'com')

    }
  }

  questionHandlerOutput <- list(questionList = question_list, crosstabInput = crosstab_input)

  return(questionHandlerOutput)

}
