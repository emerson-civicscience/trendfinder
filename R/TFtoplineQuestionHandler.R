TFtoplineQuestionHandler <- function(stem_questions,
																		 banner_questions,
																		 weighting_schemes = "us_adults"){

	question_list <- c(stem_questions, banner_questions) %>%
    unique() %>%
		rep(., length(weighting_schemes)) %>%
    sort()

	questionRows <- data.frame(banner = question_list, weight = weighting_schemes)
	
	questionRows$banner <- as.numeric(questionRows$banner)
	questionRows$weight <- as.character(questionRows$weight)

  return(questionRows)

}
