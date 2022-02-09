TFcrosstabQuestionHandler <- function(manual_crosstab_input = NULL,
																			use_manual_crosstab_only = TRUE,
																			stem_questions = NULL,
																			banner_questions = NULL,
																			weighting_schemes = NULL){

	# Should create error handler here to throw an error about number of columns if not 2 or 3
	if(!is.null(manual_crosstab_input)){
		if(ncol(manual_crosstab_input) == 2){
			colnames(manual_crosstab_input) <- c('stem', 'banner')
			if(is.null(weighting_schemes)){
			  manual_crosstab_input$weight <- NA
			} else{
			  temp_crosstab <- manual_crosstab_input
			  for(i in seq_along(weighting_schemes)){
			    if(i == 1){
			      manual_crosstab_input$weight <- weighting_schemes[i]
			    } else{
			      temp_crosstab$weight <- weighting_schemes[i]
			      manual_crosstab_input <- rbind(manual_crosstab_input, temp_crosstab)
			    }
			  }
			}
			manual_crosstab_input$weight <- weighting_schemes
		} else if(ncol(manual_crosstab_input) == 3){
			colnames(manual_crosstab_input) <- c('stem', 'banner', 'weight')
		}
	}

  if(!is.null(stem_questions) && !is.null(banner_questions) && !use_manual_crosstab_only){
    stem_banner_cross <- expand.grid(stem_questions, banner_questions)
    colnames(stem_banner_cross) <- c('stem', 'banner')

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

    crosstab_input <- rbind(stem_banner_cross, manual_crosstab_input)
  } else{
  	crosstab_input <- manual_crosstab_input
  }

  crosstab_input <- crosstab_input[!duplicated(crosstab_input), ]
  
  same_stem_and_banner <- which(crosstab_input$stem == crosstab_input$banner)
  
  if(length(same_stem_and_banner) > 0){
    crosstab_input <- crosstab_input[-same_stem_and_banner, ]
  }
  
  row.names(crosstab_input) <- NULL
	
  return(crosstab_input)

}
