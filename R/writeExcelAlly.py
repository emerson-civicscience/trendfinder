# Resources: https://stackoverflow.com/questions/38888652/use-r-to-create-chart-in-excel-sheet
# https://xlsxwriter.readthedocs.io/example_pandas_chart.html#ex-pandas-chart


import pandas as pd
import numpy as np
import xlsxwriter as xw
import datetime
import operator

def writeExcelAlly(pandas_df,
           data_colnames_wanted_py,
           chart_references_py,
           file_name_py): # Defines function
           
  pandas_df.loc[np.isnan(pandas_df['Tag Order']), ['Banner Tag Order']] = 4

  pandas_df.sort_values(by=['Unique Row ID'], inplace = True)
  
  first_table_row = 0
  first_data_row = first_table_row + 3 # May need adjusted depending on where you insert table into Excel
  first_table_col = 4
  first_data_col = first_table_col + 1 # May need adjusted depending on where you insert table into Excel
  first_category_name_row = str(first_table_row+2)
  second_category_name_row = str(first_table_row+3)
  
  chart_number = 0
  chart_dict = {}
  
  truncated_counter = 0
  
  writer = pd.ExcelWriter(file_name_py, engine='xlsxwriter')
  workbook = writer.book
  
  title_cell_format = workbook.add_format({'font_color': 'white'})
  
  first_data_cell = xw.utility.xl_rowcol_to_cell(row=first_data_row, col=first_data_col)
  
  banner_qtext_location = 'D2'
  stem_qtext_location = 'D3'
  
  def makeChart(#crosstab_loop,
              #chart_number,
              chart_title_formula,
              series,
              category_name_row):
    
    add_list = [first_data_col] * len(series)
    series = list(map(operator.add, series, add_list))
    series = list(map(xw.utility.xl_col_to_name, series))
    
    chart_name = 'chart' + str(chart_number)
    
    # Create a chart object.
    chart = workbook.add_chart({'type': 'column'}) # subtype???
    chart.set_size({'width': 870, 'height': 421})
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

    for row_in_sheet in range(first_data_row+1, crosstab_df_excel_t_nrow+2):

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
    worksheet.set_zoom(70)
        

  if chart_references_py != None:
    for crosstab_loop in chart_references_py:
        
        crosstab_stem, crosstab_banner = crosstab_loop.split(';')

        crosstab_stem_ref = '0;' + crosstab_stem
        crosstab_banner_ref = '0;' + crosstab_banner
        
        stem_qtext = None
        banner_qtext = None

        try:
            crosstab_stem = np.int64(crosstab_stem)
        except ValueError:
            pass
        try:
            crosstab_banner = np.int64(crosstab_banner)
        except ValueError:
            pass

        if type(crosstab_stem) is str and type(crosstab_banner) is str:
            crosstab_df = pandas_df.loc[pandas_df['Stem Q Banner Tag'].isin([crosstab_stem_ref,
                                                                              crosstab_banner_ref]) | 
                                        pandas_df['Tag Tag'].isin([crosstab_loop])].copy()
            crosstab_df.loc[crosstab_df['Stem Q Banner Tag'] == crosstab_stem_ref, 'Unique Crosstab ID'] = crosstab_stem_ref
            crosstab_df.loc[crosstab_df['Stem Q Banner Tag'] == crosstab_banner_ref, 'Unique Crosstab ID'] = crosstab_banner_ref
            crosstab_df.loc[crosstab_df['Tag Tag'] == crosstab_loop, 'Unique Crosstab ID'] = crosstab_loop
            
            crosstab_df.loc[crosstab_df['Stem Q Banner Tag'] == crosstab_stem_ref, 'Banner Name'] = crosstab_df['Banner Label']
            crosstab_df.loc[crosstab_df['Stem Q Banner Tag'] == crosstab_banner_ref, 'Banner Name'] = crosstab_df['Banner Label']
            crosstab_df.loc[crosstab_df['Tag Tag'] == crosstab_loop, 'Stem Name'] = crosstab_df['Stem Label']
            crosstab_df.loc[crosstab_df['Tag Tag'] == crosstab_loop, 'Banner Name'] = crosstab_df['Banner Label']
            
            crosstab_df = crosstab_df.sort_values(by=['Banner Tag Order', 'Stem Tag Order'])
            stem_qtext = crosstab_stem
            banner_qtext = crosstab_banner

        elif type(crosstab_stem) is str:
            crosstab_df = pandas_df.loc[pandas_df['Unique Crosstab ID'].isin([crosstab_banner_ref]) |
                                        pandas_df['Stem Q Banner Tag'].isin([crosstab_stem_ref]) |
                                        pandas_df['Stem Tag Banner Q'].isin([crosstab_loop])].copy()
            crosstab_df.loc[crosstab_df['Stem Q Banner Tag'] == crosstab_stem_ref, 'Unique Crosstab ID'] = crosstab_stem_ref
            crosstab_df.loc[crosstab_df['Stem Tag Banner Q'] == crosstab_loop, 'Unique Crosstab ID'] = crosstab_loop
            
            crosstab_df.loc[crosstab_df['Stem Q Banner Tag'] == crosstab_stem_ref, 'Banner Name'] = crosstab_df['Banner Label']
            crosstab_df.loc[crosstab_df['Stem Tag Banner Q'] == crosstab_loop, 'Stem Name'] = crosstab_df['Stem Label']
            
            
            crosstab_df = crosstab_df.sort_values(by=['Stem Tag Order', 'Unique Row ID'])
            stem_qtext = crosstab_stem

        elif type(crosstab_banner) is str:
            crosstab_df = pandas_df.loc[pandas_df['Unique Crosstab ID'].isin([crosstab_stem_ref]) | 
                                        pandas_df['Stem Q Banner Tag'].isin([crosstab_banner_ref]) |
                                        pandas_df['Stem Q Banner Tag'].isin([crosstab_loop])].copy()
            crosstab_df.loc[crosstab_df['Stem Q Banner Tag'] == crosstab_banner_ref, 'Unique Crosstab ID'] = crosstab_banner_ref
            crosstab_df.loc[crosstab_df['Stem Q Banner Tag'] == crosstab_loop, 'Unique Crosstab ID'] = crosstab_loop
            
            crosstab_df.loc[crosstab_df['Stem Q Banner Tag'] == crosstab_banner_ref, 'Banner Name'] = crosstab_df['Banner Label']
            crosstab_df.loc[crosstab_df['Stem Q Banner Tag'] == crosstab_loop, 'Banner Name'] = crosstab_df['Banner Label']
            
            crosstab_df = crosstab_df.sort_values(by=['Banner Tag Order', 'Unique Row ID'])
            banner_qtext = crosstab_banner

        else:
            crosstab_df = pandas_df.loc[pandas_df['Unique Crosstab ID'].isin([crosstab_stem_ref, 
                                                                                  crosstab_banner_ref, 
                                                                                  crosstab_loop])]
            crosstab_df = crosstab_df.sort_values(by=['Unique Row ID'])

        
        crosstab_df_cols = crosstab_df.columns.tolist()
         
        crosstab_df_excel_cols = ['Unique Row ID', 'Unique Crosstab ID', 
                                  'Stem QID', 'Stem ID (answers)', 
                                  'Banner QID', 'Banner ID (answers)', 
                                  'Stem Name', 'Banner Name', '2021 to date']
        # This implies no other columns other than the percentage columns have " - " in their name
        # As the % diff, response count, and statistical significance testing isn't currently passed
        # here, it's fine for now, but may need a more robust solution
        
        crosstab_df_excel = crosstab_df[crosstab_df_excel_cols]
        crosstab_df_excel.reset_index(drop=True, inplace=True)        

        topline_1_series = crosstab_df_excel.index[crosstab_df_excel['Unique Crosstab ID'] == crosstab_banner_ref].tolist()
        topline_2_series = crosstab_df_excel.index[crosstab_df_excel['Unique Crosstab ID'] == crosstab_stem_ref].tolist()
        crosstab_ref_rows =  crosstab_df_excel.index[crosstab_df_excel['Unique Crosstab ID'] == crosstab_loop].tolist()
        
        

        crosstab_subset = crosstab_df_excel[(crosstab_df_excel['Unique Crosstab ID'] == crosstab_loop)] 
                                           
        
        first_row_by_index = crosstab_subset.index.tolist()[0]
        first_stem_answer = crosstab_subset['Stem ID (answers)'][first_row_by_index]
        first_banner_answer = crosstab_subset['Banner ID (answers)'][first_row_by_index]
        
        if stem_qtext == None:    
            stem_qtext_df = crosstab_df.loc[crosstab_df['Unique Row ID'] == crosstab_subset['Unique Row ID'][first_row_by_index], ['Stem QText']]
            stem_qtext = stem_qtext_df.iloc[0,0]
        
        if banner_qtext == None:
            banner_qtext_df = crosstab_df.loc[crosstab_df['Unique Row ID'] == crosstab_subset['Unique Row ID'][first_row_by_index], ['Banner QText']]
            banner_qtext = banner_qtext_df.iloc[0,0]
        
        banner_answers = crosstab_subset[crosstab_subset['Stem ID (answers)'] == first_stem_answer]['Banner ID (answers)'].tolist()
        stem_answers = crosstab_subset[crosstab_subset['Banner ID (answers)'] == first_banner_answer]['Stem ID (answers)'].tolist()
        # Would've used np.unique instead of the subset nonsense, but want to preserve order from tag order
        # when I make the ID set loop
        
        crosstab_df_excel_cols_t = ['Stem Name', 'Banner Name', '2021 to date']
        
        crosstab_df_excel_t = crosstab_df_excel[crosstab_df_excel_cols_t].T
        
        crosstab_df_excel_t_nrow, crosstab_df_excel_t_ncol = crosstab_df_excel_t.shape # .shape is a tuple
                
        truncated_sheet_name = crosstab_loop[0:27]    
            
        crosstab_df_excel_t.to_excel(writer, sheet_name=truncated_sheet_name, index=True, startcol = first_table_col)        
        
        last_data_col = crosstab_df_excel_t_ncol + first_table_col
        
        last_data_cell = xw.utility.xl_rowcol_to_cell(row=crosstab_df_excel_t_nrow, col=last_data_col)
        
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
            
        if len(topline_2_series) != 0:
            chart_number += 1
            chart_col = 2
            category_name_row = str(first_table_row+3)
            chart_title_topline_2 = '\'' + truncated_sheet_name + '\'!' + stem_qtext_location
            
            makeChart(chart_title_topline_2, topline_2_series, category_name_row)
              
        for crosstab_1_loop in banner_answers:
            # Finds rows wanted for crosstab 1
            crosstab_1_series = crosstab_df_excel[crosstab_df_excel['Banner ID (answers)'] == crosstab_1_loop].index.tolist()
            
            if len(crosstab_df_excel.index) != len(topline_1_series):
                
                chart_number += 1
                chart_col = 0
                chart_row += 25
            
                category_name_row = str(first_table_row+2)
                chart_title_crosstab_1_formula = '=&" cut by "&' + stem_qtext_location
            
                makeChart(chart_title_crosstab_1_formula, crosstab_1_series, category_name_row)
            
        chart_row = 2
                
        for crosstab_2_loop in stem_answers:
            crosstab_2_series = crosstab_subset[crosstab_subset['Stem ID (answers)'] == crosstab_2_loop].index.tolist()
            
            if topline_1_series != crosstab_2_series:
                chart_number += 1
                chart_col = 2
                chart_row += 25
            
            # Finds rows wanted for crosstab 2
            # With crosstab 2, I did not include the topline answer this time around. I found it of limited
            # usefulness and it might get confusing on the same chart
            # I will put it in its own chart if I want to revive it
            # This is accomplished by retrieving index values from crosstab_subset instead of crosstab_df_excel
            
                category_name_row = str(first_table_row+3)
                chart_title_crosstab_2_formula = '=' + banner_qtext_location + '&" cut by "&' 
                makeChart(chart_title_crosstab_2_formula, crosstab_2_series, category_name_row)
				
  writer.save()
