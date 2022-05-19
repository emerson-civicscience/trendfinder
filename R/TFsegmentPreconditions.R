TFsegmentPreconditions <- function(bannerQuestion,
																	 segment_list,
																	 stem_start_dates = stem_start_dates,
																	 stem_end_dates = stem_end_dates,
																	 data_start_dates = data_start_dates,
																	 data_end_dates = data_end_dates,
																	 date_stem_and_banner = FALSE,
																	 geography = USgeography,
																	 segmentPreconditions = NULL){

	bannerQuestion <- as.character(bannerQuestion)
	
	if(is.null(stem_start_dates)){
	  stem_start_dates <- NA
	}
	
	if(is.null(stem_end_dates)){
	  stem_end_dates <- NA
	}

	for(segment_loop in segment_list){

		segment_and_scheme_name <- segment_loop$segmentName
		
				# if(is.null(stem_start_dates)){ # This if/else statement doesn't actually work--the dates would have to be assigned to the QIDs in the definition
		#   stem_start_dates <- NA
		#   stem_end_dates <- NA
		  precondition <- paste0(bannerQuestion,
		                         ":day>=",data_start_dates,"^",
		                         bannerQuestion,
		                         ":day<=",data_end_dates,
		                         "^",geography,"^",
		                         segment_loop$segmentDefinition)
		# } else{
		#   if(date_stem_and_banner){
		#     precondition <- paste0(segment_loop$segmentDefinition,
		#                            ":day>=",stem_start_dates,"^",
		#                            segment_loop$segmentDefinition,
		#                            ":day<=",stem_end_dates,"^",
		#                            bannerQuestion,
		#                            ":day>=",data_start_dates,"^",
		#                            bannerQuestion,
		#                            ":day<=",data_end_dates,"^",
		#                            geography,
		#                            )
		#   } else{
		#     precondition <- paste0(geography,"^",
		#                            segment_loop$segmentDefinition,
		#                            ":day>=",stem_start_dates,"^",
		#                            segment_loop$segmentDefinition,
		#                            ":day<=",stem_end_dates)
		#   }
		# }
		
		
		  segmentPreconditions <- rbind(segmentPreconditions,
		                                data.table(stem_start_date = stem_start_dates,
		                                           stem_end_date = stem_end_dates,
		                                           start_date = data_start_dates,
		                                           end_date = data_end_dates,
		                                           stem = segment_and_scheme_name,
		                                           banner = bannerQuestion,
		                                           precondition = precondition,
		                                           weighting_scheme = segment_and_scheme_name,
		                                           weights = list(list(segment_loop$segmentGender, segment_loop$segmentAge))
		  ))
		
	}

	return(segmentPreconditions)
}
