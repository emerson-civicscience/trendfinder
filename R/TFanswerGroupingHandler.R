TFanswerGroupingHandler <- function(inputAnswerGroupingHandler,
																		RCsuffix = RCsuffix,
																		numberOfPeriods = numberOfPeriods){

	# inputAnswerGroupingHandler <- inputFormat

	unique_answer_groupings <- TFanswerGroupingRefs()

	### answer_grouping_table_banner
	answer_groupings_prepended <- answer_groupings
	colnames(answer_groupings_prepended) <- paste0("Banner ", colnames(answer_groupings_prepended))
	answer_grouping_table_banner <- left_join(inputAnswerGroupingHandler, answer_groupings_prepended, by = c("banner" = "Banner Answer ID"))
	answer_grouping_table_banner <- left_join(answer_grouping_table_banner,
																						unique_answer_groupings,
																						by = c('Banner Answer Group ID' = 'Answer Group ID', 'Banner Answer Label' = 'Answer Label'))
	answer_grouping_table_banner$`Banner Answer ID` <- answer_grouping_table_banner$`Answer ID`
	answer_grouping_table_banner$unique <- paste(
		answer_grouping_table_banner$stem,
		paste0(answer_grouping_table_banner$`Banner Answer ID`, ', Group ', answer_grouping_table_banner$`Banner Answer Group ID`),
		answer_grouping_table_banner$weighting_scheme,
		sep = ";"
	)
	answer_grouping_table_banner$uniqueCrosstab <- paste(
		answer_grouping_table_banner$stemQ,
		paste0(answer_grouping_table_banner$bannerQ, ', Group ', answer_grouping_table_banner$`Banner Answer Group ID`),
		answer_grouping_table_banner$weighting_scheme,
		sep = ";"
	)
	answer_grouping_table_banner$`Banner Answer ID` <- paste0(answer_grouping_table_banner$`Banner Answer ID`, ", Group ", answer_grouping_table_banner$`Banner Answer Group ID`)
	answer_grouping_table_banner$bannerQ <- paste0(answer_grouping_table_banner$bannerQ, ", Group ", answer_grouping_table_banner$`Banner Answer Group ID`)
	# this table is further cleaned up after it is joined with answer_grouping_table below after comment ### answer_grouping_table_banner final cleanup


	### answer_grouping_table_stem
	colnames(answer_groupings_prepended) <- gsub("Banner", "Stem", colnames(answer_groupings_prepended))
	answer_grouping_table_stem <- left_join(inputAnswerGroupingHandler, answer_groupings_prepended, by = c("stem" = "Stem Answer ID"))
	answer_grouping_table_stem <- left_join(answer_grouping_table_stem,
																					unique_answer_groupings,
																					by = c('Stem Answer Group ID' = 'Answer Group ID', 'Stem Answer Label' = 'Answer Label'))
	answer_grouping_table_stem$`Stem Answer ID` <- answer_grouping_table_stem$`Answer ID`
	answer_grouping_table_stem$unique <- paste(
		paste0(answer_grouping_table_stem$`Stem Answer ID`, ', Group ', answer_grouping_table_stem$`Stem Answer Group ID`),
		answer_grouping_table_stem$banner,
		answer_grouping_table_stem$weighting_scheme,
		sep = ";"
	)
	answer_grouping_table_stem$uniqueCrosstab <- paste(
		paste0(answer_grouping_table_stem$stemQ, ', Group ', answer_grouping_table_stem$`Stem Answer Group ID`),
		answer_grouping_table_stem$bannerQ,
		answer_grouping_table_stem$weighting_scheme,
		sep = ";"
	)
	answer_grouping_table_stem$`Stem Answer ID` <- paste0(answer_grouping_table_stem$`Stem Answer ID`, ", Group ", answer_grouping_table_stem$`Stem Answer Group ID`)
	answer_grouping_table_stem$stemQ <- paste0(answer_grouping_table_stem$stemQ, ", Group ", answer_grouping_table_stem$`Stem Answer Group ID`)
	answer_grouping_table_stem$`Banner Answer Group ID` <- NA
	answer_grouping_table_stem$`Banner Answer Group Label` <- NA
	colnames(answer_grouping_table_stem)[which(colnames(answer_grouping_table_stem) == "banner")] <- 'Banner Answer ID'
	answer_grouping_table_stem$`Banner Answer Label` <- NA

	### answer_grouping_table
	answer_grouping_table <- left_join(answer_groupings, answer_grouping_table_banner[, -which(colnames(answer_grouping_table_banner) %in% c('banner', 'Answer Group Label', 'Answer ID'))], by = c("Answer ID" = "stem"))
	colnames(answer_grouping_table)[1:ncol(answer_groupings)] <- paste0("Stem ", colnames(answer_grouping_table)[1:ncol(answer_groupings)])
	answer_grouping_table <- left_join(answer_grouping_table,
																		 unique_answer_groupings,
																		 by = c('Stem Answer Group ID' = 'Answer Group ID', 'Stem Answer Label' = 'Answer Label'))
	answer_grouping_table$`Stem Answer ID` <- answer_grouping_table$`Answer ID`
	answer_grouping_table$unique <- paste(
		paste0(answer_grouping_table$`Stem Answer ID`, ', Group ', answer_grouping_table$`Stem Answer Group ID`),
		paste0(answer_grouping_table$`Banner Answer ID`, ', Group ', answer_grouping_table$`Banner Answer Group ID`),
		answer_grouping_table$weighting_scheme,
		sep = ";"
	)
	answer_grouping_table$uniqueCrosstab <- paste(
		paste0(answer_grouping_table$stemQ, ', Group ', answer_grouping_table$`Stem Answer Group ID`),
		paste0(answer_grouping_table$bannerQ, ', Group ', answer_grouping_table$`Banner Answer Group ID`),
		answer_grouping_table$weighting_scheme,
		sep = ";"
	)
	answer_grouping_table$`Banner Answer ID` <- paste0(answer_grouping_table$`Banner Answer ID`, ", Group ", answer_grouping_table$`Banner Answer Group ID`)
	answer_grouping_table$bannerQ <- paste0(answer_grouping_table$bannerQ, ", Group ", answer_grouping_table$`Banner Answer Group ID`)
	answer_grouping_table$`Stem Answer ID` <- paste0(answer_grouping_table$`Stem Answer ID`, ", Group ", answer_grouping_table$`Stem Answer Group ID`)
	answer_grouping_table$stemQ <- paste0(answer_grouping_table$stemQ, ", Group ", answer_grouping_table$`Stem Answer Group ID`)

	### answer_grouping_table_banner final cleanup
	answer_grouping_table_banner$`Stem Answer Group ID` <- NA
	answer_grouping_table_banner$`Stem Answer Group Label` <- NA
	colnames(answer_grouping_table_banner)[which(colnames(answer_grouping_table_banner) == "stem")] <- 'Stem Answer ID'
	answer_grouping_table_banner$`Stem Answer Label` <- NA

	answer_grouping_table <- answer_grouping_table[, -which(colnames(answer_grouping_table) == 'Answer ID')]
	answer_grouping_table <- answer_grouping_table[, -grep('^Answer Group Label', colnames(answer_grouping_table))]

	outputAnswerGrouping <- rbind(answer_grouping_table_stem[, colnames(answer_grouping_table)],
																answer_grouping_table_banner[, colnames(answer_grouping_table)],
																answer_grouping_table)


	colnames(outputAnswerGrouping)[which(colnames(outputAnswerGrouping) == 'Stem Answer ID')] <- 'stem'
	colnames(outputAnswerGrouping)[which(colnames(outputAnswerGrouping) == 'Banner Answer ID')] <- 'banner'

	rownames(outputAnswerGrouping) <- NULL

	outputAnswerGroupingResponses <- outputAnswerGrouping[, grep(RCsuffix, colnames(outputAnswerGrouping))]

	if(numberOfPeriods == 1){
		if(is.na(unique(outputAnswerGroupingResponses))){
			dataRows <- NULL
		}
	} else{
		dataRows <- outputAnswerGroupingResponses[rowSums(is.na(outputAnswerGroupingResponses)) != ncol(outputAnswerGroupingResponses), ] %>%
			rownames() %>%
			as.numeric()

	}


	if(length(dataRows) > 0){
		outputAnswerGrouping <- outputAnswerGrouping[dataRows,]
	} else{
		outputAnswerGrouping <- NULL
	}

	return(outputAnswerGrouping)
}
