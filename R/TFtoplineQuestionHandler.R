TFtoplineQuestionHandler <- function(stem_questions,
																		 banner_questions,
																		 weighting_schemes = NULL){

	question_list <- c(stem_questions, banner_questions) %>%
    unique() %>%
    sort()

	questionRows <- data.frame(banner = question_list, weight = "USadultWeighting")

	if(length(weighting_schemes) > 0){

		questionRowsCopy <- questionRows
		for(scheme_loop in weighting_schemes){
			questionRowsCopy$weight <- scheme_loop

			questionRows <- rbind(questionRows, questionRowsCopy)
		}

	}

  return(questionRows)

}
