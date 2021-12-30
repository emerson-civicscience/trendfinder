TFanswerGroupingHandler <- function(inputAnswerGroupingHandler,
																		RCsuffix = RCsuffix,
																		numberOfPeriods = numberOfPeriods,
																		unique_answer_groupings = unique_answer_groupings){

	answers_with_groups <- unique(answer_groupings$`Answer ID`)

	answers_with_groups_stem_table <- inputAnswerGroupingHandler[which(inputAnswerGroupingHandler$stem %in% answers_with_groups), ]
	answers_with_groups_banner_table <- inputAnswerGroupingHandler[which(inputAnswerGroupingHandler$banner %in% answers_with_groups), ]
	answers_with_groups_both_table <- answers_with_groups_stem_table[which(answers_with_groups_stem_table$banner %in% answers_with_groups), ]

	answer_table_stem <- left_join(answers_with_groups_stem_table, answer_groupings, by = c("stem" = "Answer ID"))
	colnames(answer_table_stem)[grep('Answer', colnames(answer_table_stem))] <- paste0("Stem ", colnames(answer_table_stem)[grep('Answer', colnames(answer_table_stem))])
	answer_table_stem <- left_join(answer_table_stem, unique_answer_groupings, by = c("Stem Answer Group ID" = "Answer Group ID", "Stem Answer Group Label" = "Answer Group Label", "Stem Answer Label" = "Answer Label"))
	answer_table_stem$`Stem Answer ID` <- answer_table_stem$`Answer ID`
	answer_table_stem <- answer_table_stem[, -which(startsWith(colnames(answer_table_stem), 'Answer ID'))]

	answer_table_banner <- left_join(answers_with_groups_banner_table, answer_groupings, by = c("banner" = "Answer ID"))
	colnames(answer_table_banner)[grep('Answer', colnames(answer_table_banner))] <- paste0("Banner ", colnames(answer_table_banner)[grep('Answer', colnames(answer_table_banner))])
	answer_table_banner <- left_join(answer_table_banner, unique_answer_groupings, by = c("Banner Answer Group ID" = "Answer Group ID", "Banner Answer Group Label" = "Answer Group Label", "Banner Answer Label" = "Answer Label"))
	answer_table_banner$`Banner Answer ID` <- answer_table_banner$`Answer ID`
	answer_table_banner <- answer_table_banner[, -which(startsWith(colnames(answer_table_banner), 'Answer ID'))]

	answer_table_both <- left_join(answers_with_groups_both_table, answer_groupings, by = c("stem" = "Answer ID"))
	answer_table_both <- left_join(answer_table_both, unique_answer_groupings, by = c("Answer Group ID", "Answer Group Label", "Answer Label"))
	colnames(answer_table_both)[startsWith(colnames(answer_table_both), "Answer")] <- paste0("Stem ", colnames(answer_table_both)[startsWith(colnames(answer_table_both), "Answer")])

	answer_table_both <- left_join(answer_table_both, answer_groupings, by = c("banner" = "Answer ID"))
	answer_table_both <- left_join(answer_table_both, unique_answer_groupings, by = c("Answer Group ID", "Answer Group Label", "Answer Label"))
	colnames(answer_table_both)[startsWith(colnames(answer_table_both), "Answer")] <- paste0("Banner ", colnames(answer_table_both)[startsWith(colnames(answer_table_both), "Answer")])

	answer_table_both <- answer_table_both[, -which(colnames(answer_table_both) %in% c("stem", "banner"))]

	if(nrow(answer_table_stem) > 0){
		answer_table_stem$`Banner Answer ID` <- NA
		answer_table_stem$`Banner Answer Group ID` <- NA
		answer_table_stem$`Banner Answer Group Label` <- NA
		answer_table_stem$`Banner Answer Label` <- NA
	} else{
		no_row_banner <- answer_table_stem[, grep('Answer', colnames(answer_table_stem))]
		colnames(no_row_banner) <- gsub('Stem', 'Banner', colnames(no_row_banner))
		answer_table_stem <- cbind(answer_table_stem, no_row_banner)
	}
	answer_table_stem$`Banner Answer ID` <- answer_table_stem$banner
	answer_table_stem <- answer_table_stem[, -which(colnames(answer_table_stem) %in% c("stem", "banner"))]

	if(nrow(answer_table_banner) > 0){
		answer_table_banner$`Stem Answer ID` <- NA
		answer_table_banner$`Stem Answer Group ID` <- NA
		answer_table_banner$`Stem Answer Group Label` <- NA
		answer_table_banner$`Stem Answer Label` <- NA
	} else{
		no_row_stem <- answer_table_banner[, grep('Answer', colnames(answer_table_banner))]
		colnames(no_row_stem) <- gsub('Banner', 'Stem', colnames(no_row_stem))
		answer_table_banner <- cbind(answer_table_banner, no_row_stem)
	}
	answer_table_banner$`Stem Answer ID` <- answer_table_banner$stem
	answer_table_banner <- answer_table_banner[, -which(colnames(answer_table_banner) %in% c("stem", "banner"))]

	outputAnswerGrouping <- rbind(answer_table_both, answer_table_stem, answer_table_banner)

	return(outputAnswerGrouping)
}
