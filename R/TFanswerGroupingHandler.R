TFanswerGroupingHandler <- function(inputAnswerGroupingHandler,
																		RCsuffix = RCsuffix,
																		numberOfPeriods = numberOfPeriods,
																		unique_answer_groupings = unique_answer_groupings){

	# inputAnswerGroupingHandler <- inputFormat

	answers_with_groups <- unique(answer_groupings$`Answer ID`)

	answers_with_groups_stem_table <- inputAnswerGroupingHandler[which(inputAnswerGroupingHandler$stem %in% answers_with_groups), ]
	answers_with_groups_banner_table <- inputAnswerGroupingHandler[which(inputAnswerGroupingHandler$banner %in% answers_with_groups), ]
	answers_with_groups_both_table <- answers_with_groups_stem_table[which(answers_with_groups_stem_table$banner %in% answers_with_groups), ]
	
	if(nrow(answers_with_groups_stem_table) > 0){
	  answer_table_stem <- left_join(answers_with_groups_stem_table, answer_groupings, by = c("stem" = "Answer ID"))
	  colnames(answer_table_stem)[grep('Answer', colnames(answer_table_stem))] <- paste0("Stem ", colnames(answer_table_stem)[grep('Answer', colnames(answer_table_stem))])
	  answer_table_stem <- left_join(answer_table_stem, unique_answer_groupings, by = c("Stem Answer Group ID" = "Answer Group ID", "Stem Answer Group Label" = "Answer Group Label", "Stem Answer Label" = "Answer Label"))
	  answer_table_stem$`Stem Answer ID` <- answer_table_stem$`Answer ID`
	  drop_stem_col <- -which(startsWith(colnames(answer_table_stem), 'Answer ID'))
	  answer_table_stem <- as.data.frame(answer_table_stem)
	  answer_table_stem <- answer_table_stem[, drop_stem_col]
	} else{
	  answer_table_stem <- NULL
	}

	if(nrow(answers_with_groups_banner_table) > 0){
	  answer_table_banner <- left_join(answers_with_groups_banner_table, answer_groupings, by = c("banner" = "Answer ID"))
  	colnames(answer_table_banner)[grep('Answer', colnames(answer_table_banner))] <- paste0("Banner ", colnames(answer_table_banner)[grep('Answer', colnames(answer_table_banner))])
  	answer_table_banner <- left_join(answer_table_banner, unique_answer_groupings, by = c("Banner Answer Group ID" = "Answer Group ID", "Banner Answer Group Label" = "Answer Group Label", "Banner Answer Label" = "Answer Label"))
  	answer_table_banner$`Banner Answer ID` <- answer_table_banner$`Answer ID`
  	drop_banner_col <- -which(startsWith(colnames(answer_table_banner), 'Answer ID'))
  	answer_table_banner <- as.data.frame(answer_table_banner)
  	answer_table_banner <- answer_table_banner[, drop_banner_col]
	} else{
	  answer_table_banner <- NULL
	}
	
	if(nrow(answers_with_groups_both_table) > 0){
	  answer_table_both <- left_join(answers_with_groups_both_table, answer_groupings, by = c("stem" = "Answer ID"))
	  answer_table_both <- left_join(answer_table_both, unique_answer_groupings, by = c("Answer Group ID", "Answer Group Label", "Answer Label"))
	  colnames(answer_table_both)[startsWith(colnames(answer_table_both), "Answer")] <- paste0("Stem ", colnames(answer_table_both)[startsWith(colnames(answer_table_both), "Answer")])
	  
	  answer_table_both <- left_join(answer_table_both, answer_groupings, by = c("banner" = "Answer ID"))
	  answer_table_both <- left_join(answer_table_both, unique_answer_groupings, by = c("Answer Group ID", "Answer Group Label", "Answer Label"))
	  colnames(answer_table_both)[startsWith(colnames(answer_table_both), "Answer")] <- paste0("Banner ", colnames(answer_table_both)[startsWith(colnames(answer_table_both), "Answer")])
	  drop_both_col <- -which(colnames(answer_table_both) %in% c("stem", "banner"))
	  
	  answer_table_both <- as.data.frame(answer_table_both)
	  answer_table_both <- answer_table_both[, drop_both_col]
	} else{
	  answer_table_both <- NULL
	}
	
	if(!is.null(nrow(answer_table_stem)) && nrow(answer_table_stem) > 0){
		answer_table_stem$`Banner Answer ID` <- NA
		answer_table_stem$`Banner Answer Group ID` <- NA
		answer_table_stem$`Banner Answer Group Label` <- NA
		answer_table_stem$`Banner Answer Label` <- NA
		answer_table_stem$`Banner Answer ID` <- answer_table_stem$banner
		drop_stem_col_2 <- -which(colnames(answer_table_stem) %in% c("stem", "banner"))
		answer_table_stem <- answer_table_stem[, drop_stem_col_2]
	} else{
	  answer_table_stem <- NULL
	}

	if(!is.null(nrow(answer_table_banner)) && nrow(answer_table_banner) > 0){
		answer_table_banner$`Stem Answer ID` <- NA
		answer_table_banner$`Stem Answer Group ID` <- NA
		answer_table_banner$`Stem Answer Group Label` <- NA
		answer_table_banner$`Stem Answer Label` <- NA
		answer_table_banner$`Stem Answer ID` <- answer_table_banner$stem
		drop_banner_col_2 <- -which(colnames(answer_table_banner) %in% c("stem", "banner"))
		answer_table_banner <- answer_table_banner[, drop_banner_col_2]
	} else{
		answer_table_banner <- NULL
	}

	outputAnswerGrouping <- rbind(answer_table_both, answer_table_stem, answer_table_banner)

	return(outputAnswerGrouping)
}
