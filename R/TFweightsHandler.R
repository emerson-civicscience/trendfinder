TFweightsHandler <- function(scheme_name){

	unlisted_weight <- weighting_dict$value[which(weighting_dict$scheme_name == scheme_name)] %>%
		unlist()

	weights = list(
		c(unlisted_weight['Male'],
			unlisted_weight['Female']),
		c(unlisted_weight['Under 18'],
			unlisted_weight['18 - 24'],
			unlisted_weight['25 - 29'],
			unlisted_weight['30 - 34'],
			unlisted_weight['35 - 44'],
			unlisted_weight['45 - 54'],
			unlisted_weight['55 - 64'],
			unlisted_weight['65 or older'])
	)

	return(weights)

}
