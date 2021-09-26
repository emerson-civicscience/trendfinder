TFcrosstabQuestionHandler <- function(stem_questions,
                                     banner_questions,
                                     crosstab_input,
                                     weighting_schemes = NULL){

  if(is.null(crosstab_input)){
    if(!is.null(stem_questions) && !is.null(banner_questions)){

      crosstab_input <- expand.grid(stem_questions, banner_questions)
      colnames(crosstab_input) <- c('seg', 'com')

      crosstab_input$weights <- "USadultWeighting"

      if(length(weighting_schemes) > 0){

        crosstab_input_copy <- crosstab_input
        for(scheme_loop in weighting_schemes){
          crosstab_input_copy$weights <- scheme_loop

          crosstab_input <- rbind(crosstab_input, crosstab_input_copy)
        }
      }
    }
  }

  return(crosstab_input)

}
