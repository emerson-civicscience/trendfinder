TFdatePropTestLetters <- function(input){
  
  # The input (outputFormatted) is already set up to perform prop testing across rows; just need to determine how many count/total columns there are
  
  input_cols <- colnames(input)
  
  count_cols <- grep(" - Response Count$", colnames(input))  # Dollar sign regex for 'ends with'
  # total_cols <- grep(" - total responses$", colnames(input)) # Dollar sign regex for 'ends with'
  prop_column_combinations <- combn(count_cols, 2) # This creates a table of all the possible pairwise combinations of columns for stat testing
  
  
  # metadata_cols <- c("Weighting Scheme", "Stem Client Q Flag", "Stem Q Account ID", "Banner Client Q Flag", "Banner Q Account ID", 
  #                   "Answer Flag", "Stats Flag", "Chart", "Manual Flag", "Stem QID", "Stem Group ID", "Stem QText", "Stem Answer ID", 
  #                   "Stem Name", "Banner QID", "Banner Group ID", "Banner QText", "Banner Answer ID", "Banner Name")
  
  # metadata_col_nums <- which(input_cols %in% metadata_cols)
  
  # Initialize final output. A better practice in R is probably to make a list and join it later to the initial input
  
  result <- input[, c(count_cols)]
  result[] <- ""
  colnames(result) <- gsub(" - Response Count$", " - stats test", colnames(result))
  result <- cbind(input, result)
  
  label_table <- result[1, grep(" - stats test", colnames(result))]
  label_table[1, ] <- LETTERSplex[1:ncol(label_table)]
  
  for(i in 1:nrow(result)){
    
    for(j in 1:ncol(prop_column_combinations)){
    # for(j in 1:6){
      
      # Each column in prop_column_combinations represents a prop test that needs done. For each value "f," there are two
      # values (the rows of prop_column_combinations). These are used to grab the values we want to test.
      
      count_1_col <- prop_column_combinations[1, j]
      count_2_col <- prop_column_combinations[2, j]
      
      count_1_col_text <- input_cols[count_1_col]
      count_2_col_text <- input_cols[count_2_col]
      total_1_text <- gsub(" - response count", " - total responses", count_1_col_text)
      total_2_text <- gsub(" - response count", " - total responses", count_2_col_text)
      total_1_col <- which(input_cols == total_1_text)
      total_2_col <- which(input_cols == total_2_text)
      
      count_1 <- result[i, count_1_col]
      count_2 <- result[i, count_2_col]
      total_1 <- result[i, total_1_col]
      total_2 <- result[i, total_2_col]
      
      result_col_1 <- total_1_col + length(count_cols)
      result_col_2 <- total_2_col + length(count_cols)
      
      if(is.na(count_1) == FALSE){
        if(is.na(count_2) == FALSE){
          if(count_1 > 0 && count_1 != total_1){
            if(count_2 > 0 && count_2 != total_2){
              
              prop <- prop.test(c(count_1, count_2), c(total_1, total_2))
              
              p_value <- prop$p.value
              difference <- (prop$estimate[1]-prop$estimate[2])*100
              
              if (p_value <= .05){
                
                if (difference > 0) { # If "difference" is positive, the column "k" is bigger than column "l" and that is put into the result
                  letter_identifier <- label_table[1, which(colnames(label_table) == colnames(result)[result_col_2])]
                  current_cell_result <- result[i, result_col_1]
                  if (current_cell_result == ""){
                    result[i, result_col_1] <- letter_identifier
                  } else{
                    result[i, result_col_1] <- paste(current_cell_result, letter_identifier, sep=", ")
                  }
                  
                } else{ # If "difference" is negative, the column "l" is bigger than column "k" and that is put into the result
                  letter_identifier <- label_table[1, which(colnames(label_table) == colnames(result)[result_col_1])]
                  current_cell_result <- result[i, result_col_2]
                  if (current_cell_result == ""){
                    result[i, result_col_2] <- letter_identifier
                  } else{
                    result[i, result_col_2] <- paste(current_cell_result, letter_identifier, sep=", ")
                  }
                }
              } # End of "if (p_value...).  Note, I'm not sure the logic works out so that the results are always put in alphabetical order, but it very well might
              # just based on how combn() works.
            }
          }
        }
      }
    } # End of "for(j...)
  } # End of "for(i...)
 
  # 
  # output <- merge(input, full_result, by = c("Question Identifier", "Answer Identifier"), all.x = TRUE) # Put back with original data structure
  # 
  # output_column_names <- colnames(output)
  # output_column_names <- str_replace_all(output_column_names, "\\.x", " - Percents and Counts") # Give friendly names
  # output_column_names <- str_replace_all(output_column_names, "\\.y", " - Stat Test") # Give friendly names
  # 
  # colnames(output) <- output_column_names
  
  # output[is.na(output)] <- " " # Remove NAs
  return(result)
}