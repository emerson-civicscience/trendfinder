TFcrosstabQuestionHandler <- function(crosstab_input = NULL,
																			stem_questions = NULL,
																			banner_questions = NULL,
																			weighting_schemes = NULL){

  if(!is.null(stem_questions) && !is.null(banner_questions)){

    stem_banner_cross <- expand.grid(stem_questions, banner_questions)
    colnames(stem_banner_cross) <- c('seg', 'com')

    if(is.null(weighting_schemes)){
    	stem_banner_cross$weight <- NA
    } else{

      for(i in weighting_schemes){

      	if(ncol(stem_banner_cross == 2)){
      		stem_banner_cross$weight <- i
      		if(length(weighting_schemes > 1)){
      			stem_banner_cross_weight <- stem_banner_cross
      		}
      	} else{
      		stem_banner_cross_weight$weight <- i
      		stem_banner_cross <- rbind(stem_banner_cross, stem_banner_cross_weight)
      	}
      }
    }

    crosstab_input <- rbind(stem_banner_cross, crosstab_input)
  }

	crosstab_input <- unique(crosstab_input)

  return(crosstab_input)

}
