weightsTable <- function(weighting_scheme,
												 weighting_scheme_goal = NULL,
												 bi.user = NULL,
												 bi.password = NULL){

	if(is.null(bi.user)){
		bi.user <- readRDS('~/TrendFinder/BI Info/bi.user.rds')
		bi.password = readRDS('~/TrendFinder/BI Info/bi.password.rds')
	}

	########################
	# CONNECT TO DATABASES #
	########################

	# establish connections to our databases
	cs.con <- dbConnect(RMySQL::MySQL(),
											username = bi.user,
											password = bi.password,
											host = "cs-nonwriter.civicscience.com",
											port = 3306,
											dbname = "civicscience")

	query <- paste0('SELECT * FROM weighting_scheme_goal WHERE `name` LIKE \'', weighting_scheme, '\'')

	suppressWarnings( # R doesn't like importing the float from p_goal, so I just supressed warnings
		query_table <- dbGetQuery(query, conn = cs.con))

	unique_attributes <- unique(query_table$attribute_uri) %>%
		gsub('optionQuestionResponse/', '', .)

	if(length(unique_attributes == 2)){
		age_gender_check <- c('484', '7078') %in% unique_attributes %>%
			length()
		if(age_gender_check == 2){

			weighting_scheme_goal <- left_join(dataKey[which(dataKey$`Question ID` %in% c(484, 7078)), c("Answer ID", "Answer Text")],
																				 query_table[, c('name', 'value_uri', 'p_goal')],
																				 by = c("Answer ID" = "value_uri"))

			# weighting_scheme_goal <- weighting_scheme_goal[-grep(',', weighting_scheme_goal$`Answer ID`), ]
			weighting_scheme_goal$p_goal[which(is.na(weighting_scheme_goal$name))] <- 0
			weighting_scheme_goal$p_goal <- weighting_scheme_goal$p_goal/100

			gender <- c(Male = weighting_scheme_goal$p_goal[which(weighting_scheme_goal$`Answer Text` == 'Male')],
									Female = weighting_scheme_goal$p_goal[which(weighting_scheme_goal$`Answer Text` == 'Female')])
			age <- c('Under 18' = weighting_scheme_goal$p_goal[which(weighting_scheme_goal$`Answer Text` == 'Under 18')],
							 '18 - 24' = weighting_scheme_goal$p_goal[which(weighting_scheme_goal$`Answer Text` == '18 - 24')],
							 '25 - 29' = weighting_scheme_goal$p_goal[which(weighting_scheme_goal$`Answer Text` == '25 - 29')],
							 '30 - 34' = weighting_scheme_goal$p_goal[which(weighting_scheme_goal$`Answer Text` == '30 - 34')],
							 '35 - 44' = weighting_scheme_goal$p_goal[which(weighting_scheme_goal$`Answer Text` == '35 - 44')],
							 '45 - 54' = weighting_scheme_goal$p_goal[which(weighting_scheme_goal$`Answer Text` == '45 - 54')],
							 '55 - 64' = weighting_scheme_goal$p_goal[which(weighting_scheme_goal$`Answer Text` == '55 - 64')],
							 '65 or older' = weighting_scheme_goal$p_goal[which(weighting_scheme_goal$`Answer Text` == '65 or older')])

			gender_age <- list(gender, age)

			weighting_dict_addition <- tibble(scheme_name = weighting_scheme, value = list(gender_age))
		}
	}
	dbDisconnect(cs.con)
	return(weighting_dict_addition)
}
