TFanswerGroupingRefs <- function(unique_answer_groupings = NULL){

	answer_groupings <- arrange(answer_groupings, `Answer ID`)
	unique_answer_groups <- unique(answer_groupings$`Answer Group ID`)


	for(i in unique_answer_groups){
		answer_labels <- answer_groupings$`Answer Label`[which(answer_groupings$`Answer Group ID` == i)] %>%
			unique()
		unique_answer_groupings_subset <- answer_groupings[which(answer_groupings$`Answer Group ID` == i), ]

		for(j in answer_labels){

			unique_grouped_IDs <- paste(unique_answer_groupings_subset$`Answer ID`[which(unique_answer_groupings_subset$`Answer Label` == j)], collapse = ", ")
			unique_answer_groupings_subset$`Answer ID`[which(unique_answer_groupings_subset$`Answer Label` == j)] <- unique_grouped_IDs
		}
		unique_answer_groupings_subset <- unique(unique_answer_groupings_subset)
		unique_answer_groupings <- rbind(unique_answer_groupings, unique_answer_groupings_subset)
	}

	return(unique_answer_groupings)
}
