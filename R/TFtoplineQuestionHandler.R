TFtoplineQuestionHandler <- function(stem_questions,
																		 banner_questions,
																		 weighting_schemes = "USadultWeighting"){

	question_list <- c(stem_questions, banner_questions) %>%
    unique() %>%
		rep(., length(weighting_schemes)) %>%
    sort()

	rep(question_list, length(weighting_schemes))

	questionRows <- data.frame(banner = question_list, weighting_scheme = weighting_schemes)

  return(questionRows)

}
