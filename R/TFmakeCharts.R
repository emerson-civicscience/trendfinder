TFmakeCharts <- function(input_TFmakeCharts){

  # input_TFmakeCharts <- outputFormatted

	tag_table_stem <- left_join(input_TFmakeCharts, tag_table, by = c("Stem Answer ID" = "Answer ID")) %>%
		.[which(!is.na(.$Tag)), ]
	tag_table_stem$`Stem QID` <- tag_table_stem$Tag
	tag_table_stem$`Stem Group ID` <- NA
	tag_table_stem$`Stem QText` <- tag_table_stem$Tag
	tag_table_stem$`Stem Answer ID` <- tag_table_stem$`Tag Order`
	tag_table_stem$`Stem Name` <- tag_table_stem$`Tag Label`
	tags_wanted_stem <- unique(tag_table_stem$Tag[which(tag_table_stem$Chart == 1)])
	tag_table_stem <- tag_table_stem[which(tag_table_stem$Tag %in% tags_wanted_stem), ]

	setorder(tag_table_stem, 'Stem QID', 'Stem Answer ID')

	tag_table_banner <- left_join(input_TFmakeCharts, tag_table, by = c("Banner Answer ID" = "Answer ID")) %>%
		.[which(!is.na(.$Tag)), ]
	tag_table_banner$`Banner QID` <- tag_table_banner$Tag
	tag_table_banner$`Banner Group ID` <- NA
	tag_table_banner$`Banner QText` <- tag_table_banner$Tag
	tag_table_banner$`Banner Answer ID` <- tag_table_banner$`Tag Order`
	tag_table_banner$`Banner Name` <- tag_table_banner$`Tag Label`
	tags_wanted_banner <- unique(tag_table_banner$Tag[which(tag_table_stem$Chart == 1)])
	tag_table_banner <- tag_table_banner[which(tag_table_banner$Tag %in% tags_wanted_banner), ]

	setorder(tag_table_banner, 'Banner QID', 'Banner Answer ID')

	tag_table_both <- tag_table_stem[, -grep('Tag', colnames(tag_table_stem))]
	tag_table_both <- left_join(tag_table_both, tag_table, by = c("Banner Answer ID" = "Answer ID")) %>%
	.[which(!is.na(.$Tag)), ]
	tag_table_both$`Banner QID` <- tag_table_both$Tag
	tag_table_both$`Banner Group ID` <- NA
	tag_table_both$`Banner QText` <- tag_table_both$Tag
	tag_table_both$`Banner Answer ID` <- tag_table_both$`Tag Order`
	tag_table_both$`Banner Name` <- tag_table_both$`Tag Label`
	tag_table_both <- tag_table_both[which(tag_table_both$Tag %in% tags_wanted_banner), ]

	setorder(tag_table_both, 'Banner QID', 'Banner Answer ID')

	tag_table_combined <- rbind(tag_table_stem, tag_table_banner, tag_table_both) %>%
		.[ , -grep('Tag', colnames(.))]

	tag_table_combined$Chart <- 1

	dt <- rbind(input_TFmakeCharts, tag_table_combined)

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

  # write.table(dt, file=paste0('~/TrendFinder/Outputs/2021-12-28/dt 2021-12-28.tsv'), quote=TRUE, sep='\t', row.names=FALSE)
  # write.table(ref_table, file=paste0('~/TrendFinder/Outputs/2021-12-28/ref_table 2021-12-28.tsv'), quote=TRUE, sep='\t', row.names=FALSE)

  source_python("~/TrendFinder/trendfinder/R/writeExcel.py")

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
