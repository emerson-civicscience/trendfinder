#' @export

engage <- function(bi.user = NULL,
									 bi.password = NULL,
									 stem_questions = NULL,
									 banner_questions = NULL,
									 segment_list = NULL,
									 segment_names = NULL,
									 crosstab_input = NULL,
									 data_start_dates = "2021-01-01",
									 data_end_dates = NULL,
									 time_period = NULL,
									 weighting_schemes = NULL,
									 run_topline = TRUE,
									 run_crosstabs = TRUE,
									 manual_crosstab_input = NULL,
									 use_manual_crosstab_only = TRUE,
									 run_stats = FALSE,
									 use_default_answer_flag = FALSE,
									 cutoff_stats_flags = 10,
									 max_chart_return = 50,
									 max_chart_iterate = 5,
									 use_tags = TRUE,
									 must_plot = NULL,
									 plot_all = FALSE,
									 python_loc = NULL){

	stem_questions <- unique(stem_questions)
	banner_questions <- unique(banner_questions)
	crosstab_input <- crosstab_input[!duplicated(crosstab_input), ]

	weighting_dict <- TFweights(weighting_schemes,
	                            bi.user = bi.user,
	                            bi.password = bi.password)

	if(!exists('data_end_dates') | is.null(data_end_dates)){
		data_end_dates <- TFdateHandler(data_start_dates)
	} else{
		data_end_dates <- TFdateHandler(data_start_dates, data_end_dates = data_end_dates)
	}

	if(length(time_period) != length(data_start_dates)){
		time_period <- data_start_dates
	}

	questionList <- TFtoplineQuestionHandler(stem_questions = stem_questions,
																					 banner_questions = banner_questions,
																					 weighting_schemes = weighting_schemes)


	# TFcrosstabQuestionHandler() outputs a data frame with three columns: stem, banner, weights
	crosstab_input <- TFcrosstabQuestionHandler(manual_crosstab_input = manual_crosstab_input,
																							use_manual_crosstab_only = use_manual_crosstab_only,
																							stem_questions = stem_questions,
																							banner_questions = banner_questions,
																							weighting_schemes = weighting_schemes)


	### The function outputFilePathMaker generates the filepath for the output. Specifying fileLocation
	### will create a subfolder called "Outputs" in the fileLocation directory
	# outputDate <- today() - 1 # The default date in outputFilePathMaker is today()
	outputFilePath <- outputFilePathMaker()

	if(!is.null(crosstab_input)){
	  
	  crosstabStemList <- crosstab_input[, c("stem", "weight")]
	  colnames(crosstabStemList) <- colnames(questionList)
	  crosstabBannerList <- crosstab_input[, c("banner", "weight")]
	  crosstabQuestionList <- rbind(crosstabStemList, crosstabBannerList) %>%
	    .[!duplicated(.), ] 
		
	}

	# If no questions are specified for topline results but topline results are requested, create topline list of all
	# unique questions in crosstab input.
	if(is.null(questionList)){
		if(!is.null(crosstab_input)){
			questionList <- crosstabQuestionList
		} else{
			print("You don\'t have a topline list or a crosstab table/tibble to input.")
			stop()
		}
	} else{
	  questionList <- rbind(questionList, crosstabQuestionList) %>%
	    .[!duplicated(.), ] %>%
	    arrange(., banner)
	}
	
	



	trendfinder_history <- readRDS('~/TrendFinder/Outputs/trendfinder_history.rds')

	anti_join_columns <- c("start_date", "end_date", "stem", "banner", "weighting_scheme") # Used for matching history with current preconditions
	condition_columns <- c("banner", "precondition", "weighting_scheme") # Used to remove unwanted columns from precondition tables

	batch_time <- Sys.time()  # This iteration of TrendFinder relies on the concept of batches to organize/audit data
	batch_time_char <- format(batch_time, "%Y-%m-%d %H:%M:%S %Z")
	batch_metadata_update <- tibble(batch = batch_time_char,
																	weighting_schemes = list(weighting_schemes),
																	stem_questions = list(stem_questions),
																	banner_questions = list(banner_questions),
																	crosstab_input = list(crosstab_input),
																	notes = notes)



	if(run_topline){

		questionListTopline <- transpose(questionList) %>%
			as.list()

		toplineConditions <- mclapply(questionListTopline, TFtoplinePreconditions,
																	data_start_dates = data_start_dates,
																	data_end_dates = data_end_dates,
																	mc.cores = detectCores()) %>%
			rbindlist()


		### Each time new data is computed for TrendFinder, it will be pushed to two files (essentially database tables):
		##### '~/TrendFinder/Outputs/trendfinder_results.rds'
		##### '~/TrendFinder/Outputs/trendfinder_history.rds'
		### trendfinder_history is a much smaller file as it is just metadat about when/how the data was computed and the
		### the unique topline quesetion ID/crosstab question IDs rather than the answer IDs and actual data
		### If the table trendfinder_history has a matching row for start_date, end_date, weights, stem, and banner,
		### it is excluded with the anti_join() function

		toplineConditionsDeduped <- anti_join(toplineConditions, trendfinder_history, by=all_of(anti_join_columns))

		if(nrow(toplineConditionsDeduped) == 0){
			outputResults <- NULL
			trendfinder_history_update <- NULL
			allConditions <- toplineConditions
		} else{

			trendfinder_history_update <- toplineConditionsDeduped

			toplineConditionsDeduped <- as_tibble(toplineConditionsDeduped)

			toplineConditionsDeduped <- toplineConditionsDeduped[, condition_columns]

			toplineConditionsList <- transpose(toplineConditionsDeduped) %>%
				as.list()


			toplineResults <- mclapply(toplineConditionsList, TFtopline,
																 mc.cores = detectCores()) %>%
				rbindlist()

			# fileName <- outputName("Topline Results", batch_time = batch_time_char)
			# saveRDS(toplineResults, paste0(fileName, ".rds"))

			outputResults <- toplineResults
		}

		if(!is.null(outputResults)){
			outputResults$data.banner <- as.character(outputResults$data.banner)
		}

		allConditions <- toplineConditions %>% select(anti_join_columns)


	} else{

		toplineResults <- NULL
		outputResults <- NULL
		trendfinder_history_update <- NULL
		allConditions <- NULL

	}

	if(!is.null(segment_list)){

		questionListUnique <- unique(questionList$banner)

		segmentConditions <- mclapply(questionListUnique, TFsegmentPreconditions,
																	segment_list = segment_list,
																	data_start_dates = data_start_dates,
																	data_end_dates = data_end_dates,
																	mc.cores = detectCores()) %>%
			rbindlist()
		
		segment_names <- unique(segmentConditions$stem)

		weightingDictSegments <- tibble(weighting_scheme = segmentConditions$weighting_scheme, value = segmentConditions$weights) %>%
			unique()
		# packageVersion('tibble') # Currently using 2.1.3 and get https://github.com/tidyverse/tibble/issues/798

		segmentConditionsDeduped <- anti_join(segmentConditions, trendfinder_history, by = all_of(anti_join_columns))

		if(nrow(segmentConditionsDeduped) == 0){
			segmentResults <- NULL
		} else{
		  
		  keep_columns <- colnames(trendfinder_history_update)
		  trendfinder_history_update <- rbind(trendfinder_history_update, segmentConditionsDeduped[, ..keep_columns])

			segmentConditionsDedupedSubset <- segmentConditionsDeduped %>% select(., all_of(condition_columns))

			### Remove rows based on prior results run by TrendFinder
			### See comment just after toplineConditions (above) for more info

			segmentConditionsList <- transpose(segmentConditionsDedupedSubset) %>%
				as.list()

			segmentResults <- mclapply(segmentConditionsList, TFsegment,
																 weightingDictSegments = weightingDictSegments,
																 mc.cores = detectCores()) %>%
				rbindlist()

			# segmentResults <- TFoutputResultsFormat(segmentResults, batch_time = batch_time_char)
			
			segmentConditionsDeduped <- as.data.frame(segmentConditionsDeduped)

			segmentConditionsDeduped <- segmentConditionsDeduped[, which(colnames(segmentConditionsDeduped) %in% anti_join_columns)]
			
		}

		# fileName <- outputName("Segment Results", batch_time = batch_time_char)
		# saveRDS(segmentResults, paste0(fileName, ".rds"))

		segmentResultsChar <- segmentResults
		segmentResultsChar$data.banner <- as.character(segmentResults$data.banner)
		# segmentResultsChar$batch <- batch_time_char
		# colnames(segmentResultsChar)[grep('data.', colnames(segmentResultsChar))] <- gsub('data.', '', colnames(segmentResultsChar)[grep('data', colnames(segmentResultsChar))])
		# colnames(segmentResultsChar)[grep('response.count', colnames(segmentResultsChar))] <- gsub('response.count', 'response_count', colnames(segmentResultsChar)[grep('response.count', colnames(segmentResultsChar))])

		outputResults <- rbind(outputResults, segmentResultsChar)
		segmentConditions <- segmentConditions %>% select(anti_join_columns)
		allConditions <- rbind(allConditions, segmentConditions)

	}


	if(run_crosstabs){
		if(is.null(crosstab_input)){
			if(length(questionList) < 3){
				crosstab_input <- data.table(seg = stem_questions, com = banner_questions, weighting_scheme = weighting_schemes)
			} else{
				crosstab_input <- permutations(length(questionList), 2, questionList) %>%
					as.data.table() # This can get quite big quite fast!
			}

		}

		crosstabRows <- transpose(crosstab_input) %>%
			as.list()

		crosstabConditions <- mclapply(crosstabRows, TFcrosstabPreconditions,
																	 data_start_dates = data_start_dates,
																	 data_end_dates = data_end_dates,
																	 mc.cores = detectCores()) %>%
			rbindlist()


		### Remove rows based on prior results run by TrendFinder
		### See comment just after toplineConditions (above) for more info

		crosstabConditionsDeduped <- anti_join(crosstabConditions, trendfinder_history, by=all_of(anti_join_columns))

		if(nrow(crosstabConditionsDeduped) == 0){
			crosstabResults <- NULL
		} else{

			trendfinder_history_update <- rbind(trendfinder_history_update, crosstabConditionsDeduped)

			crosstabConditionsDeduped <- as_tibble(crosstabConditionsDeduped)

			crosstabConditionsDeduped <- crosstabConditionsDeduped[, c("stem", condition_columns)]

			crosstabConditionsList <- transpose(crosstabConditionsDeduped) %>%
				as.list()

			crosstabResults <- mclapply(crosstabConditionsList, TFcrosstab,
																	# data_start_dates = data_start_dates,
																	# data_end_dates = data_end_dates,
																	weighting_dict = weighting_dict,
																	mc.cores = detectCores()) %>%
				rbindlist()
		}

		# fileName <- outputName("Crosstab Results", batch_time = batch_time_char)
		# saveRDS(crosstabResults, paste0(fileName, ".rds"))

		crosstabResultsChar <- crosstabResults

		if(!is.null(crosstabResultsChar)){
			crosstabResultsChar$data.stem <- as.character(crosstabResultsChar$data.stem)
		}

		# crosstabResultsChar$data.banner <- as.character(crosstabResultsChar$data.banner)

		outputResults <- rbind(outputResults, crosstabResultsChar)
		crosstabConditions <- crosstabConditions %>% select(anti_join_columns)
		allConditions <- rbind(allConditions, crosstabConditions)



	} else{
		crosstabResults <- NULL
	}

	output_end_time <- Sys.time()
	elapsed_time <- output_end_time - batch_time
	elapsed_time # For informational/diagnostic use
	# fileName <- outputName("Output Results", batch_time = batch_time_char)
	

	if(!is.null(outputResults)){
	  outputResults <- as_tibble(outputResults)

		outputResults <- TFoutputResultsFormat(outputResults, batch_time = batch_time_char)
		# write.table(outputResults, file=paste0(fileName,'.tsv'), quote=TRUE, sep='\t', row.names=FALSE)
		# saveRDS(outputResults, paste0(fileName, ".rds"))
	}

	trendfinder_results <- readRDS('~/TrendFinder/Outputs/trendfinder_results.rds')

	if(!is.null(trendfinder_history_update)){

		trendfinder_history_update$batch <- batch_time_char
		trendfinder_history_update <- as.data.frame(trendfinder_history_update)
		trendfinder_history_update <- trendfinder_history_update[, c("batch", "start_date", "end_date", "weighting_scheme", "stem", "banner")]
		trendfinder_history <- readRDS('~/TrendFinder/Outputs/trendfinder_history.rds')
		trendfinder_history <- rbind(trendfinder_history, trendfinder_history_update)
		saveRDS(trendfinder_history, '~/TrendFinder/Outputs/trendfinder_history.rds')

		trendfinder_results <- rbind(trendfinder_results, outputResults)
		trendfinder_results <- trendfinder_results[!duplicated(trendfinder_results), ] # Just in case
		saveRDS(trendfinder_results, '~/TrendFinder/Outputs/trendfinder_results.rds')
	}


	# now have to mash up the already computed rows here with join instead of anti-join

	allConditionsAnswers <- left_join(allConditions, dataKey, by = c("stem" = "Question ID"))
	allConditionsAnswers$`Answer ID`[which(is.na(allConditionsAnswers$`Answer ID`))] <- allConditionsAnswers$stem[which(is.na(allConditionsAnswers$`Answer ID`))]
	allConditionsAnswers$stem <- allConditionsAnswers$`Answer ID`
	allConditionsAnswers <- allConditionsAnswers[, ..anti_join_columns]
	allConditionsAnswers <- left_join(allConditionsAnswers, dataKey, by = c("banner" = "Question ID"))
	allConditionsAnswers$banner <- allConditionsAnswers$`Answer ID`
	allConditionsAnswers <- allConditionsAnswers[, ..anti_join_columns]

	all_results <- left_join(allConditionsAnswers, trendfinder_results, by = all_of(anti_join_columns))

	# This is here to remove joins from dataKey that were not actually calculated
	# When answer groupings in the calculation phase are done away with, this won't be necessary
	# outputResults <- outputResults[-which(is.na(outputResults$response_count)), ]

	all_results <- TFoutputResultsFormat(all_results, batch_time = batch_time_char)
	fileName <- outputName("All Results", batch_time = batch_time_char)
	# saveRDS(all_results, paste0(fileName, ".rds"))

	batch_metadata <- rbind(readRDS('~/TrendFinder/Outputs/batch_metadata.rds'), batch_metadata_update)
	saveRDS(batch_metadata, '~/TrendFinder/Outputs/batch_metadata.rds')


	# if (!is.null(format_output)) {
	#   if (!is.null(cutoff_stats_flags)) {
	#     outputFormatted <- TFwider(outputResults) %>%
	#       TFformat(.) %>%
	#       TFstats(., cutoff_stats_flags)
	#   } else{
	#     outputFormatted <- TFwider(outputResults) %>%
	#       TFformat(.)
	#   }
	# }

	# outputWider <- TFwider(all_results)
	outputFormatted <- TFwider(all_results) %>%
	  TFformat(., time_period = time_period, segment_names = segment_names, use_default_answer_flag = use_default_answer_flag, batch_time = batch_time_char)
	
	if(run_stats){
	  outputStats <- TFstats(outputFormatted, 
	                         cutoff_stats_flags = cutoff_stats_flags,
	                         max_chart_return = max_chart_return,
	                         max_chart_iterate = max_chart_iterate)
	  
	  input_TFmakeCharts <- outputStats
	} else{
	  input_TFmakeCharts <- outputFormatted
	}
	
	# View(outputFormatted)
	
	# write.table(outputFormatted, file=paste0(fileName,'.tsv'), quote=TRUE, sep='\t', row.names=FALSE)
	# write.table(outputResults, file=paste0('~/TrendFinder/Outputs/2021-11-09/outputResults.tsv'), quote=TRUE, sep='\t', row.names=FALSE)
	
	TFmakeCharts(input_TFmakeCharts,
	             segment_names = segment_names,
	             use_tags = use_tags,
	             must_plot = must_plot,
	             plot_all = plot_all,
	             python_loc = python_loc)

	}
