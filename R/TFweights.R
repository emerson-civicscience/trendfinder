TFweights <- function(weighting_schemes,
											bi.user = NULL,
											bi.password = NULL){

	if(any(weighting_schemes %in% weighting_dict$scheme_name)){
		new_schemes <- weighting_schemes[-which(weighting_schemes %in% weighting_dict$scheme_name)]
	} else{
		new_schemes <- weighting_schemes
	}

	if(length(new_schemes) > 0){

		weighting_dict_addition <- lapply(weighting_schemes, weightsTable, bi.user = bi.user, bi.password = bi.password) %>%
			rbindlist()

	} else{
		weighting_dict_addition <- NULL
	}

	if(!is.null(weighting_dict_addition)){
		weighting_dict <- rbind(weighting_dict, weighting_dict_addition) %>%
			as_tibble()
	}
  
  return(weighting_dict)
}
