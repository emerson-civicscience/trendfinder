TFcrosstabQuestionHandler <- function(crosstab_input = NULL,
																			stem_questions = NULL,
																			banner_questions = NULL,
																			weighting_schemes = NULL){

    if(!is.null(stem_questions) && !is.null(banner_questions)){

      stem_banner_cross <- expand.grid(stem_questions, banner_questions)
      colnames(stem_banner_cross) <- c('seg', 'com')

      crosstab_input <- rbind(stem_banner_cross, crosstab_input)
    }

	crosstab_input$weights <- "USadultWeighting"

    if(length(weighting_schemes) > 0){

      crosstab_input_copy <- crosstab_input
      for(scheme_loop in weighting_schemes){
        crosstab_input_copy$weights <- scheme_loop

        crosstab_input <- rbind(crosstab_input, crosstab_input_copy)
      }
    }

	crosstab_input <- unique(crosstab_input)

  return(crosstab_input)

}
