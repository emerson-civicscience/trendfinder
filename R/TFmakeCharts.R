TFmakeCharts <- function(input_TFmakeCharts,
												 use_tags = TRUE,
												 must_plot = NULL,
												 plot_all = FALSE,
												 python_loc = NULL){

  # input_TFmakeCharts <- outputFormatted

	input_TFmakeCharts$Chart[is.na(input_TFmakeCharts$Chart)] <- 0
	input_TFmakeCharts <- input_TFmakeCharts[!duplicated(input_TFmakeCharts), ]
	# input_TFmakeCharts <- input_TFmakeCharts[-which(input_TFmakeCharts$`Stem QID` == input_TFmakeCharts$`Banner QID`), ]
	columns_wanted <- colnames(input_TFmakeCharts)

	if(use_tags){
		dt <- left_join(input_TFmakeCharts, tag_table, by = c("Stem Answer ID" = "Answer ID"))
		colnames(dt) <- gsub("Tag", "Stem Tag", colnames(dt))
		dt <- left_join(dt, tag_table, by = c("Banner Answer ID" = "Answer ID"))
		colnames(dt) <- gsub("^Tag", "Banner Tag", colnames(dt))

		dt$`Stem QID`[which(!is.na(dt$`Stem Tag`))] <- dt$`Stem Tag`[which(!is.na(dt$`Stem Tag`))]
		dt$`Stem Group ID`[which(!is.na(dt$`Stem Tag`))] <- 0
		dt$`Stem QText`[which(!is.na(dt$`Stem Tag`))] <- dt$`Stem Tag`[which(!is.na(dt$`Stem Tag`))]
		dt$`Stem Name`[which(!is.na(dt$`Stem Tag`))] <- dt$`Stem Tag Label`[which(!is.na(dt$`Stem Tag`))]
		dt$`Banner QID`[which(!is.na(dt$`Banner Tag`))] <- dt$`Banner Tag`[which(!is.na(dt$`Banner Tag`))]
		dt$`Banner Group ID`[which(!is.na(dt$`Banner Tag`))] <- 0
		dt$`Banner QText`[which(!is.na(dt$`Banner Tag`))] <- dt$`Banner Tag`[which(!is.na(dt$`Banner Tag`))]
		dt$`Banner Name`[which(!is.na(dt$`Banner Tag`))] <- dt$`Banner Tag Label`[which(!is.na(dt$`Banner Tag`))]

		dt <- setorder(dt,
		               'Stem Group ID', 'Stem QID', 'Stem Tag Order',
		               'Banner QID', 'Banner Group ID','Banner Tag Order') %>%
		  .[, columns_wanted]
		
		dt <- rbind(input_TFmakeCharts, dt)
		
	} else{
		dt <- input_TFmakeCharts
	}

	dt <- dt[!duplicated(dt[c("Weighting Scheme", "Stem Group ID", "Stem Answer ID", "Banner Group ID", "Banner Answer ID")]), ]
	
	
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
  } else{
    data_columns_wanted <- (grep("Banner Name", colnames(dt))+1)
  }

  data_colnames_wanted <- colnames(dt)[data_columns_wanted] %>%
    as.list()


  file_name <- paste0("Excel from Python - ", Sys.time(), ".xlsx") %>%
      gsub(":", "_", .)

  pandas_df <- r_to_py(dt)
  data_colnames_wanted_py <- r_to_py(data_colnames_wanted)
  chart_references_py <- r_to_py(ref_table)
  file_name_py <- r_to_py(file_name)

  # write.table(dt, file=paste0('~/TrendFinder/Outputs/2022-01-05/dt 2022-01-05.tsv'), quote=TRUE, sep='\t', row.names=FALSE)
  # write.table(ref_table, file=paste0('~/TrendFinder/Outputs/2022-01-05/ref_table 2022-01-05.tsv'), quote=TRUE, sep='\t', row.names=FALSE)

  if(is.null(python_loc)){
  	python_loc <- file.path(.libPaths()[grep('home', .libPaths())], 'trendfinder', 'exec')

  }
  
  python_loc_and_file <- file.path(python_loc, "writeExcel.py")

  source_python(python_loc_and_file)

  writeExcel(pandas_df,
             data_colnames_wanted_py,
             chart_references_py,
             file_name_py)

  fileCopyStatus <- file.copy(from = file.path(getwd(), file_name),
                              to   = file.path(outputFilePathMaker(), file_name))

  if(fileCopyStatus == TRUE){
    file.remove(file.path(getwd(), file_name))
  }

}

