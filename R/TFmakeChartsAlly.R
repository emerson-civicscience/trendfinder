TFmakeChartsAlly <- function(input_TF_make_charts){

  # input_TF_make_charts <- readRDS('~/TrendFinder/Outputs/2021-09-27/Output - Responses - Formatted - Batch Time 2021-09-27 16_05_14 EDT')

  # input_TF_make_charts <- outputFormatted

  # Spacing has been thrown off by dataKey$weight

  dataKeyTagTable <- dataKey[which(!is.na(dataKey$Tag)), ] %>%
    .[, c('Answer ID',
          'Weighting Scheme',
          'Tag',
          'Tag Order',
          'Label')]

  dataKeyTagTable$Tag <- as.character(dataKeyTagTable$Tag)
  dataKeyTagTable$Label <- as.character(dataKeyTagTable$Label)

  dataKeyTagTable_stem <- dataKeyTagTable
  colnames(dataKeyTagTable_stem) <- paste0('Stem ', colnames(dataKeyTagTable_stem))
  colnames(dataKeyTagTable_stem)[1] <- 'Stem ID (answers)'

  dataKeyTagTable_banner <- dataKeyTagTable[which(dataKeyTagTable$`Weighting Scheme`=="USadultWeighting"), ]
  dataKeyTagTable_banner <- dataKeyTagTable_banner[ , -grep('Weighting Scheme', colnames(dataKeyTagTable))]
  colnames(dataKeyTagTable_banner) <- paste0('Banner ', colnames(dataKeyTagTable_banner))
  colnames(dataKeyTagTable_banner)[1] <- 'Banner ID (answers)'

  dt <- left_join(input_TF_make_charts, dataKeyTagTable_stem, by = c('Stem ID (answers)', "Weights" = "Stem Weighting Scheme"))
  dt <- left_join(dt, dataKeyTagTable_banner, by = 'Banner ID (answers)')

  # segment_rows <- which(is.na(dt$`Stem QText`))
  #
  # for(i in segment_rows){
  #   dt$`Stem QText`[i] <- "Life Stages"
  #   dt$`Unique Crosstab ID`[i] <- paste0(dt$`Stem QText`[i], ";", dt$`Banner QID`[i])
  #   dt$`Stem QID`[i] <- dt$`Stem ID (answers)`[i]
  #   dt$`Stem Name`[i] <- dt$`Stem ID (answers)`[i]
  #
  # }

  dt$`Stem Q Banner Tag` <- paste0(dt$`Stem QID`, ";", dt$`Banner Tag`)
  dt$`Stem Tag Banner Q` <- paste0(dt$`Stem Tag`, ";", dt$`Banner QID`)
  dt$`Tag Tag` <- paste0(dt$`Stem Tag`, ";", dt$`Banner Tag`)

  dt_subset <- dt[which(dt$Chart == 1), ]

  crosstab_stems <- unique(dt_subset$`Stem QID`) %>%
    .[-grep(0, .)]
  crosstab_banners <- unique(dt_subset$`Banner QID`)
  crosstab_toplines <- c(crosstab_stems, crosstab_banners) %>%
    paste0('0;', .)

  crosstab_references <- c(unique(dt_subset$`Unique Crosstab ID`), crosstab_toplines) %>%
    unique

  unique_stem_tags <- unique(dt_subset$`Stem Tag`) %>%
    .[which(!is.na(.))]
  unique_banner_tags <- unique(dt_subset$`Banner Tag`) %>%
    .[which(!is.na(.))]

  stem_q_banner_tag_references <- unique(dt$`Stem Q Banner Tag`) %>%
    .[-grep(';NA', .)]
  stem_tag_banner_q_references <- unique(dt$`Stem Tag Banner Q`) %>%
    .[-grep('NA;', .)]

  tag_tag_table <- dt_subset[, c('Stem Tag', 'Banner Tag')] %>%
    .[c(which(!is.na(.$`Stem Tag`))), ] %>%
    .[c(which(!is.na(.$`Banner Tag`))), ] %>%
    unique

  tag_tag_references <- NULL

  if(nrow(tag_tag_table) > 0){
    for(j in 1:nrow(tag_tag_table)){

      tag_tag_references <- c(tag_tag_references, paste0(tag_tag_table[j,1], ";", tag_tag_table[j,2]))
    }
  }



  potential_orphan_toplines <- dt_subset$`Banner QID`[which(dt_subset$`Stem QID` == 0)] %>%
    unique()

  orphan_topline_locations <- lapply(potential_orphan_toplines, grep, x=unique(c(crosstab_stems, crosstab_banners))) %>%
    unlist()

  orphan_topline_locations <- orphan_topline_locations[which(!is.na(orphan_topline_locations))]

  if(!is.null(orphan_topline_locations)){
    orphan_toplines <- potential_orphan_toplines[-orphan_topline_locations]
  } else {
    orphan_toplines <- NULL
  }


  colnames(dt) <- as.character(colnames(dt))
  meta_data_columns_wanted <- c("Unique Row ID", "Unique Crosstab ID", "Weights",
                                "Stem QID", "Stem QText", "Stem ID (answers)", "Stem Name", "Stem Tag Order", "Stem Label",
                                "Banner QID", "Banner QText", "Banner ID (answers)", "Banner Name", "Banner Tag Order", "Banner Label",
                                "Stem Q Banner Tag", "Stem Tag Banner Q", "Tag Tag")

  colnames(dt)[20] <- "2021 to date"

  data_columns_wanted <- 20
  data_colnames_wanted <- colnames(dt)[data_columns_wanted]

  dt_write_excel <- dt[, c(meta_data_columns_wanted, colnames(dt)[data_columns_wanted])]

  # Could use this in a pinch; would make all stem names a stem label if a label is available. Mucks up individual crosstabs, though.
  # dt_write_excel$`Stem Name`[which(!is.na(dt_write_excel$`Stem Label`))] <- dt_write_excel$`Stem Label`[which(!is.na(dt_write_excel$`Stem Label`))]


  last_meta_data_column <- length(meta_data_columns_wanted)

  if(length(orphan_toplines) > 0){
    topline_references <- paste0("0;", orphan_toplines)
  } else{
    topline_references <- NULL
  }

  file_name <- paste0("Excel from Python - ", Sys.time(), ".xlsx") %>%
    gsub(":", "_", .)

  chart_references <- c(topline_references, orphan_toplines,
                        crosstab_references,
                        stem_q_banner_tag_references, stem_tag_banner_q_references, tag_tag_references) %>%
    unique() %>%
    .[grep(";", .)]

  # write.table(dt_write_excel, file=paste0('tf 2021-09-07.tsv'), quote=TRUE, sep='\t', row.names=FALSE)

  # Convert tag order column back to numeric if it has been converted to factor
  dt_write_excel$`Banner Tag Order` <- as.numeric(dt_write_excel$`Banner Tag Order`)
  dt_write_excel$`Stem Tag Order` <- as.numeric(dt_write_excel$`Stem Tag Order`)

  # dt_write_excel_save <- dt_write_excel
  # dt_write_excel <- dt_write_excel_save

  dt_write_excel$`Stem Label`[which(dt_write_excel$`Stem Name` == 'Topline')] <- 'U.S. Adults'
  dt_write_excel <- dt_write_excel[which(!is.na(dt_write_excel$`Stem Label`)), ]

  dt_write_excel <- pivot_wider(dt_write_excel,
                                id_cols = c('Banner ID (answers)', 'Weights'),
                                names_from = 'Stem Label',
                                values_from = '2021 to date')

  colnames(dt_write_excel)[1] <- 'Answer ID'

  dt_write_excel <- left_join(dt_write_excel, dataKey, by = c("Answer ID", "Weights" = "Weighting Scheme"))

  colOrder <- c('Answer ID', 'Question ID', 'Answer Text', 'Answer Flag', 'Question Text', 'Sponsored', 'Account', 'Tag', 'Tag Order', 'Label', 'Affluent Millennials', 'Mass Middles', 'Young Mass Middles', 'U.S. Adults')

  dt_write_excel <- dt_write_excel[, colOrder]

  dt_write_excel$Tag <- as.numeric(dt_write_excel$Tag)
  dt_write_excel$`Tag Order` <- as.numeric(dt_write_excel$`Tag Order`)
  dt_write_excel$Label <- as.character(dt_write_excel$Label)

  pandas_df <- r_to_py(dt_write_excel)
  data_colnames_wanted_py <- r_to_py(data_colnames_wanted)
  chart_references_py <- r_to_py(chart_references)
  file_name_py <- r_to_py(file_name)

  # write.table(dt_write_excel, file=paste0("Ally Input - ", today(), ".tsv"), quote=TRUE, sep='\t', row.names=FALSE)

  source_python("~/TrendFinder/trendfinder/R/writeExcelAlly.py")

  writeExcelAlly(pandas_df,
                 data_colnames_wanted_py,
                 chart_references_py,
                 file_name_py)

  fileCopyStatus <- file.copy(from = file.path(getwd(), file_name),
                              to   = file.path(outputFilePathMaker(), file_name))

  if(fileCopyStatus == TRUE){
    file.remove(file.path(getwd(), file_name))
  }

}

