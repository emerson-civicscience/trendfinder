TFtoplineQuestionHandler <- function(stem_questions,
                          banner_questions,
                          crosstab_input){

  question_list <- c(stem_questions, banner_questions) %>%
    unique() %>%
    sort()

  return(question_list)

}
