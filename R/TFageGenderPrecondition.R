TFageGenderPrecondition <- function(weights = NULL){

	if(!is.null(weights)){

		gender_zeros <- names(which(weights[[1]] == 0))
		age_zeros <- names(which(weights[[2]] == 0))

		# If a gender question with more than two genders is used, gender_precondition will have to be handled like age
		gender_precondition <- preconditionDict[which(preconditionDict$bucket_name == gender_zeros), 2] %>%
			unlist()

		age_precondition <- lapply(age_zeros, grep, x = preconditionDict$bucket_name) %>%
			unlist()

		if(!is.null(gender_precondition)){
			gender_precondition <- paste0("484!=", gender_precondition)
		}
		if(!is.null(age_precondition)){
			age_precondition <- paste0("7078!=", paste(preconditionDict$value[age_precondition], collapse=";"))
		}

		if(!is.null(gender_precondition) && !is.null(age_precondition)){
			and_operator <- "^"
		} else{
			and_operator <- NULL
		}

		age_gender_precondition <- paste(gender_precondition, and_operator, age_precondition) %>%
			gsub(" ", "", .)

	} else{
		age_gender_precondition <- NULL
	}

	return(age_gender_precondition)

}
