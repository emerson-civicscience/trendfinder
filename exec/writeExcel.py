# Resources: https://stackoverflow.com/questions/38888652/use-r-to-create-chart-in-excel-sheet
# https://xlsxwriter.readthedocs.io/example_pandas_chart.html#ex-pandas-chart
# pandas_df = r.pandas_df
# data_colnames_wanted_py = r.data_colnames_wanted_py
# chart_references_py = r.chart_references_py
# file_name_py = r.file_name_py
# pd.set_option("display.max_rows", None, "display.max_columns", None)

import pandas as pd
import numpy as np
import xlsxwriter as xw
import datetime
import operator

def writeExcel(pandas_df, data_colnames_wanted_py, chart_references_py, file_name_py):
  pandas_df['Stem Group ID'] = pandas_df['Stem Group ID'].fillna(0)
  pandas_df['Banner Group ID'] = pandas_df['Banner Group ID'].fillna(0)
  chart_references_py['Stem Group ID'] = chart_references_py['Stem Group ID'].fillna(0).astype(int)
  chart_references_py['Banner Group ID'] = chart_references_py['Banner Group ID'].fillna(0)
  
  first_table_row = 0
  first_data_row = first_table_row + 3 # May need adjusted depending on where you insert table into Excel
  first_table_col = 4
  first_data_col = first_table_col + 1 # May need adjusted depending on where you insert table into Excel
  first_category_name_row = str(first_table_row+2)
  second_category_name_row = str(first_table_row+3)
  
  chart_number = 0
  chart_dict = {}
  writer = pd.ExcelWriter(file_name_py, engine='xlsxwriter')
  workbook = writer.book
  
  title_cell_format = workbook.add_format({'font_color': 'white'})
  first_data_cell = xw.utility.xl_rowcol_to_cell(row=first_data_row, col=first_data_col)
  banner_qtext_location = 'D2'
  stem_qtext_location = 'D3'
  
  def makeChart(chart_title_formula, series, category_name_row):
    add_list = [first_data_col] * len(series)
    series = list(map(operator.add, series, add_list))
    series = list(map(xw.utility.xl_col_to_name, series))
    
    chart_name = 'chart' + str(chart_number)
    
    # Create a chart object.
    chart = workbook.add_chart({'type': 'column'}) # subtype???
    chart.set_size({'width': 1124, 'height': 450})
    chart.set_legend({'font': {'name': 'Segoe UI Light', 'size': 10}, 'position': 'bottom'})
    chart.set_x_axis({'line': {'color': '#D9D9D9'},
    'num_font': {'name': 'Segoe UI Light', 'size': 11, 'color': '#595959'}})
    chart.set_y_axis({'line': {'none': True},
    'num_font': {'name': 'Segoe UI Light', 'size': 11, 'color': '#595959'}, 
    'major_gridlines': {'visible': True, 'line': {'color' : '#D9D9D9'}},
    'max': 1, 'major_unit': .1})
    
    chart_dict[chart_name] = chart
    
    if '&" cut by "&' in chart_title_formula:
      if stem_qtext_location in chart_title_formula:
        cut_by_sub = series[0] + str(second_category_name_row) + '&" cut by "&'
        chart_title_formula = chart_title_formula.replace('&" cut by "&', cut_by_sub)
        chart_title_col = 'A'
      else:
        chart_title_formula = chart_title_formula + series[0] + str(first_category_name_row)
        chart_title_col = 'C'
      chart_title_cell = chart_title_col + str(chart_row+1)
      worksheet.write(chart_title_cell, chart_title_formula, title_cell_format)
      chart.set_title({'name': [truncated_sheet_name, chart_row, chart_col], 'name_font': {'name': 'Segoe UI Light', 'size': 13, 'color': '#595959'}})
    else:
      chart.set_title({'name': chart_title_formula, 'name_font': {'name': 'Segoe UI Light', 'size': 13, 'color': '#595959'}})
          
    for row_in_sheet in range(first_data_row+1, combined_df_excel_t_nrow+2):
      j = ','.join(['\'' + truncated_sheet_name + '\'!' + str(i) + str(row_in_sheet) for i in series])
      k = ','.join(['\'' + truncated_sheet_name + '\'!' + str(i) + category_name_row for i in series])
      
      formula_for_series = '=(' + j + ')'
      formula_for_categories = '=(' + k + ')'
      col_with_series_name = first_table_col
      name_of_series = '\'' + truncated_sheet_name + '\'!' + xw.utility.xl_rowcol_to_cell(row=row_in_sheet-1, col=col_with_series_name)
      
      chart.add_series({'values': formula_for_series, 
      'name': name_of_series, 
      # 'type': 'percentage',
      'overlap': -25,
      'categories': formula_for_categories
      })
    
    worksheet.insert_chart(chart_row, chart_col, chart_dict[chart_name])
    worksheet.set_zoom(80)
    
  sheet_number = 0
  # Uses the rows of chart_references_py (table passed from R) to iterate through
  for indices, row in chart_references_py.iterrows():   
    
    stem_q = row[0]
    stem_group = row[1]
    banner_q = row[2]
    banner_group = row[3]
    
    # Since the code handles topline results (which don't have a "stem" question), this 
    # skips finding the question text for the stem
    # Assign "Stem" to 'Stem QID'to parse the series info for the makeChart function
    
    if (stem_q != banner_q and stem_q != '0') or type(stem_q) == str:
      stem_df = pandas_df[pandas_df["Stem QText"] == "Topline"]
      stem_df = stem_df[stem_df["Banner QID"] == stem_q]
      stem_df = stem_df[stem_df["Banner Group ID"] == stem_group]
      plot_crosstab = True
      crosstab_df = pandas_df[pandas_df["Stem QID"] == stem_q]
      crosstab_df = crosstab_df[crosstab_df["Stem Group ID"] == stem_group]
      crosstab_df = crosstab_df[crosstab_df["Banner QID"] == banner_q]
      crosstab_df = crosstab_df[crosstab_df["Banner Group ID"] == banner_group]
      stem_qtext = crosstab_df['Stem QText'].iloc[0]
      first_stem_answer = crosstab_df['Stem Answer ID'].iloc[0]
      first_banner_answer = crosstab_df['Banner Answer ID'].iloc[0]
      
      if stem_df.shape[0] != '0':
        stem_df = stem_df.reset_index(drop=True, inplace=False)
        stem_df['Stem QID'] = "Stem"
        if stem_q != banner_q:
          plot_topline_2 = True
        else:
          plot_topline_2 = False
      else: 
        plot_topline_2 = False
    else:
      plot_topline_2 = False
      plot_crosstab = False
      stem_qtext = ''
      
    banner_df = pandas_df[pandas_df["Stem QText"] == "Topline"]
    banner_df = banner_df[banner_df["Banner QID"] == banner_q]
    banner_df = banner_df[banner_df["Banner Group ID"] == banner_group]
    
    if banner_df.shape[0] != 0:
      banner_df = banner_df.reset_index(drop=True, inplace=False)
      # Assign "Banner" to 'Stem QID'to parse the series info for the makeChart function
      banner_df['Stem QID'] = "Banner"
      banner_qtext = banner_df["Banner QText"].iloc[0]
    else:
      banner_qtext = crosstab_df["Banner QText"].iloc[0] # If there is a reference to a banner Q and no banner_df, it should mean there is a crosstab
      
    if plot_topline_2:
      combined_df = pd.concat([stem_df, banner_df, crosstab_df])
    elif plot_crosstab: 
      combined_df = pd.concat([banner_df, crosstab_df])
    else:
      combined_df = banner_df
      
    combined_df_cols = combined_df.columns.tolist()
    combined_df_excel_cols = ['Stem QID', 'Stem Answer ID', 
      'Banner QID', 'Banner Answer ID', 
      'Stem Name', 'Banner Name'] + data_colnames_wanted_py
      
    combined_df_excel = combined_df[combined_df_excel_cols]
    combined_df_excel.reset_index(drop=True, inplace=True)
    topline_1_series = combined_df_excel.index[combined_df_excel['Stem QID'] == 'Banner'].tolist()
    
    combined_df_excel_cols_t = ['Stem Name', 'Banner Name'] + data_colnames_wanted_py
    combined_df_excel_t = combined_df_excel[combined_df_excel_cols_t].T
    combined_df_excel_t_nrow, combined_df_excel_t_ncol = combined_df_excel_t.shape # .shape is a tuple
      
    sheet_number += 1 
      
    if stem_q == '0':
      stem_q_text = ''
    else:
      stem_q_text = str(stem_q)+';'
      
    sheet_number_text = str(sheet_number)
    truncated_sheet_name = sheet_number_text + "-" + (stem_q_text + str(row[2]))[0:(31-1-len(sheet_number_text))]
    combined_df_excel_t.to_excel(writer, sheet_name=truncated_sheet_name, index=True, startcol = first_table_col)        
    
    last_data_col = combined_df_excel_t_ncol + first_table_col
    
    last_data_cell = xw.utility.xl_rowcol_to_cell(row=combined_df_excel_t_nrow, col=last_data_col)
    
    worksheet = writer.sheets[truncated_sheet_name]
    worksheet.set_column(0, 0, 160)
    worksheet.set_column(2, 2, 160)
    percent_format = workbook.add_format({'num_format': '0%'})
    
    worksheet.conditional_format(first_data_cell + ':' + last_data_cell, 
    {'type': 'cell', 'criteria': '>', 'value': 0, 'format': percent_format})
    
    worksheet.write(banner_qtext_location, banner_qtext)
    worksheet.write(stem_qtext_location, stem_qtext)
    
    chart_row = 2
    
    if len(topline_1_series) != 0:
      chart_number += 1
      chart_col = 0
      category_name_row = str(first_table_row+3)
      chart_title_topline_1 = '\'' + truncated_sheet_name + '\'!' + banner_qtext_location
      
      makeChart(chart_title_topline_1, topline_1_series, category_name_row)
      
      if plot_topline_2:
        topline_2_series = combined_df_excel.index[combined_df_excel['Stem QID'] == 'Stem'].tolist()
        crosstab_ref_rows = combined_df_excel.index[~combined_df_excel.index.isin(topline_1_series + topline_2_series)].tolist()
        chart_number += 1
        chart_col = 2
        category_name_row = str(first_table_row+3)
        chart_title_topline_2 = '\'' + truncated_sheet_name + '\'!' + stem_qtext_location
        
        makeChart(chart_title_topline_2, topline_2_series, category_name_row)
        
    if plot_crosstab:
      
      crosstab_ref_rows = combined_df_excel.index[~combined_df_excel.index.isin(topline_1_series)].tolist()
      crosstab_subset = combined_df_excel.loc[crosstab_ref_rows, ]
      banner_answers = crosstab_subset[crosstab_subset['Stem Answer ID'] == first_stem_answer]['Banner Answer ID'].tolist()
      stem_answers = crosstab_subset[crosstab_subset['Banner Answer ID'] == first_banner_answer]['Stem Answer ID'].tolist()
      
      chart_col = 0
      
      for crosstab_1_loop in banner_answers:
        crosstab_1_series = combined_df_excel[combined_df_excel['Banner Answer ID'] == crosstab_1_loop].index.tolist()
        
        chart_number += 1
        chart_row += 25
        category_name_row = str(first_table_row+2)
        chart_title_crosstab_1_formula = '=&" cut by "&' + stem_qtext_location
        makeChart(chart_title_crosstab_1_formula, crosstab_1_series, category_name_row)
          
      chart_row = 2
      chart_col = 2	
          
      for crosstab_2_loop in stem_answers:
        crosstab_2_series = crosstab_subset[crosstab_subset['Stem Answer ID'] == crosstab_2_loop].index.tolist()
        chart_number += 1
        chart_row += 25
        category_name_row = str(first_table_row+3)
        chart_title_crosstab_2_formula = '=' + banner_qtext_location + '&" cut by "&'
        makeChart(chart_title_crosstab_2_formula, crosstab_2_series, category_name_row)
              
  writer.save()

