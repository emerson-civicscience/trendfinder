TFmakeCharts <- function(input_TFmakeCharts,
                         segment_names = NULL,
												 use_tags = TRUE,
												 must_plot = NULL,
												 plot_all = FALSE,
												 python_loc = NULL,
												 ancestry_output = FALSE){

  # input_TFmakeCharts <- outputFormatted
  # input_TFmakeCharts <- ancestry_data

	input_TFmakeCharts$Chart[is.na(input_TFmakeCharts$Chart)] <- 0
	input_TFmakeCharts$`Stem Group ID`[is.na(input_TFmakeCharts$`Stem Group ID`)] <- 0
	input_TFmakeCharts$`Banner Group ID`[is.na(input_TFmakeCharts$`Banner Group ID`)] <- 0
	input_TFmakeCharts <- input_TFmakeCharts[!duplicated(input_TFmakeCharts), ]
	# input_TFmakeCharts <- input_TFmakeCharts[-which(input_TFmakeCharts$`Stem QID` == input_TFmakeCharts$`Banner QID`), ]
	columns_wanted <- colnames(input_TFmakeCharts)
	
	tag_table$`Answer Group ID`[is.na(tag_table$`Answer Group ID`)] <- 0
	
	# input_TFmakeCharts$`Stem Group ID` <- as.numeric(input_TFmakeCharts$`Stem Group ID`)
	# input_TFmakeCharts$`Banner Group ID` <- as.numeric(input_TFmakeCharts$`Banner Group ID`)

	if(use_tags){
		dt <- left_join(input_TFmakeCharts, tag_table, by = c("Stem Answer ID" = "Answer ID", "Stem Group ID" = "Answer Group ID"))
		colnames(dt) <- gsub("Tag", "Stem Tag", colnames(dt))
		dt <- left_join(dt, tag_table, by = c("Banner Answer ID" = "Answer ID", "Banner Group ID" = "Answer Group ID"))
		colnames(dt) <- gsub("^Tag", "Banner Tag", colnames(dt))
		
		dt_stem <- dt
		dt_stem$`Stem QID`[which(!is.na(dt_stem$`Stem Tag`))] <- dt_stem$`Stem Tag`[which(!is.na(dt_stem$`Stem Tag`))]
		dt_stem$`Stem Group ID`[which(!is.na(dt_stem$`Stem Tag`))] <- 0
		dt_stem$`Stem QText`[which(!is.na(dt_stem$`Stem Tag`))] <- dt_stem$`Stem Tag`[which(!is.na(dt_stem$`Stem Tag`))]
		dt_stem$`Stem Name`[which(!is.na(dt_stem$`Stem Tag`))] <- dt_stem$`Stem Tag Label`[which(!is.na(dt_stem$`Stem Tag`))]
		
		dt_banner <- dt
		dt_banner$`Banner QID`[which(!is.na(dt_banner$`Banner Tag`))] <- dt_banner$`Banner Tag`[which(!is.na(dt_banner$`Banner Tag`))]
		dt_banner$`Banner Group ID`[which(!is.na(dt_banner$`Banner Tag`))] <- 0
		dt_banner$`Banner QText`[which(!is.na(dt_banner$`Banner Tag`))] <- dt_banner$`Banner Tag`[which(!is.na(dt_banner$`Banner Tag`))]
		dt_banner$`Banner Name`[which(!is.na(dt_banner$`Banner Tag`))] <- dt_banner$`Banner Tag Label`[which(!is.na(dt_banner$`Banner Tag`))]

		dt$`Stem QID`[which(!is.na(dt$`Stem Tag`))] <- dt$`Stem Tag`[which(!is.na(dt$`Stem Tag`))]
		# dt$`Stem Group ID`[which(!is.na(dt$`Stem Tag`))] <- 0 # This was a copypasta error from copying dt_stem procedure
		dt$`Stem QText`[which(!is.na(dt$`Stem Tag`))] <- dt$`Stem Tag`[which(!is.na(dt$`Stem Tag`))]
		dt$`Stem Name`[which(!is.na(dt$`Stem Tag`))] <- dt$`Stem Tag Label`[which(!is.na(dt$`Stem Tag`))]
		dt$`Banner QID`[which(!is.na(dt$`Banner Tag`))] <- dt$`Banner Tag`[which(!is.na(dt$`Banner Tag`))]
		# dt$`Banner Group ID`[which(!is.na(dt$`Banner Tag`))] <- 0 # This was a copypasta error from copying dt_banner procedure
		dt$`Banner QText`[which(!is.na(dt$`Banner Tag`))] <- dt$`Banner Tag`[which(!is.na(dt$`Banner Tag`))]
		dt$`Banner Name`[which(!is.na(dt$`Banner Tag`))] <- dt$`Banner Tag Label`[which(!is.na(dt$`Banner Tag`))]
		
		dt <- rbind(dt_stem, dt_banner, dt)

		dt <- setorder(dt,
		               'Stem Group ID', 'Stem QID', 'Stem Tag Order',
		               'Banner QID', 'Banner Group ID','Banner Tag Order') %>%
		  .[, columns_wanted]
		
		dt <- rbind(input_TFmakeCharts, dt)
		
	} else{
		dt <- input_TFmakeCharts
	}

	dt <- dt[!duplicated(dt[c("Weighting Scheme", "Stem QID", "Stem Group ID", "Stem Answer ID", "Stem Name", "Banner QID", "Banner Group ID", "Banner Answer ID", "Banner Name")]), ]
	
	
	if(!ancestry_output){
	  remove_stems <- which(dt$`Stem QText` == "Ancestry Brand Dash Segments")
	  remove_banners <- which(dt$`Banner QText` == "Ancestry Brand Dash Segments")
	  remove_rows <- c(remove_stems, remove_banners)
	  if(length(remove_rows) > 0){
	    dt <- dt[-remove_rows, ]
	  } else{
	    fix_rows <- which(dt$`Banner QID` %in% c(122906, 123605))
	    fix_dt <- dt[fix_rows, ] # Kludgey fix to get report out 2022-06-10
	    dt <- dt[-fix_rows, ]
	    
	    fix_dt <- arrange(fix_dt, `Banner Answer ID`)
	    dt <- rbind(dt, fix_dt)
	  }
	  
	}
	
	if(plot_all){
		dt$Chart <- 1
	} else if(!is.null(must_plot)){
		for(i in must_plot){
			dt$Chart[which(dt$`Stem QID`== i[1] & dt$`Banner QID`== i[2])] <- 1
		}
	}


	ref_table <- dt[which(dt$Chart == 1), c("Stem QID", "Stem Group ID", "Banner QID", "Banner Group ID")] %>%
		.[!duplicated(.), ]

  if("Period 1 - Period 2" %in% colnames(dt)){
    data_columns_wanted <- (grep("Banner Name", colnames(dt))+1):(grep("Period 1 - ", colnames(dt))-1)
  } else if(ancestry_output){
    data_columns_wanted <- (grep("Banner Name", colnames(dt))+1):(grep(" - Response Count", colnames(dt))[1]-1)
  } else{
    data_columns_wanted <- (grep("Banner Name", colnames(dt))+1):(grep(" - response count", colnames(dt))[1]-1)
    }

  data_colnames_wanted <- colnames(dt)[data_columns_wanted] %>%
    as.list()


  file_name <- paste0("Excel from Python - ", Sys.time(), ".xlsx") %>%
      gsub(":", "_", .)

  
  data_colnames_wanted_py <- r_to_py(data_colnames_wanted)

  file_name_py <- r_to_py(file_name)
  segment_names_py <- r_to_py(segment_names)
  

  # write.xlsx(dt, file = '/home/emerson/TrendFinder/Outputs/2022-04-22/dt 2022-04-22.xlsx')
  # write.xlsx(ref_table, file = '/home/emerson/TrendFinder/Outputs/2022-04-22/ref_table 2022-04-22.xlsx')
  # data_colnames_wanted_py
  # file_name_py
  

  if(is.null(python_loc)){
  	python_loc <- file.path(.libPaths()[grep('home', .libPaths())], 'trendfinder', 'exec')

  }
  
  if(ancestry_output){
    
    dt <- dt[dt$`Stem QID` == "Ancestry Brand Dash Segments", ]
    
    pandas_df <- r_to_py(dt)
    
    ref_table <- readRDS('~/TrendFinder/Inputs/ancestry_ref_table.rds')
    chart_references_py <- r_to_py(ref_table)
    
    python_loc_and_file <- file.path(python_loc, "writeExcelAncestry.py")
    source_python(python_loc_and_file)
    
    writeExcelAncestry(pandas_df,
                       chart_references_py,
                       file_name_py)
    
  } else{
    python_loc_and_file <- file.path(python_loc, "writeExcel.py")
    source_python(python_loc_and_file)
    pandas_df <- r_to_py(dt)
    chart_references_py <- r_to_py(ref_table)
    # writeExcel(pandas_df,
    #            data_colnames_wanted_py,
    #            chart_references_py,
    #            segment_names_py,
    #            file_name_py)
    
    writeExcel(pandas_df,
               data_colnames_wanted_py,
               chart_references_py,
               file_name_py)
    
  }

  fileCopyStatus <- file.copy(from = file.path(getwd(), file_name),
                              to   = file.path(outputFilePathMaker(), file_name))

  if(fileCopyStatus == TRUE){
    file.remove(file.path(getwd(), file_name))
  }

}

