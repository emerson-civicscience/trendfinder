# Resources: https://stackoverflow.com/questions/38888652/use-r-to-create-chart-in-excel-sheet
# https://xlsxwriter.readthedocs.io/example_pandas_chart.html#ex-pandas-chart
# df = r.pandas_df
# chart_references_py = r.chart_references_py
# pd.set_option("display.max_rows", None, "display.max_columns", None)

import pandas as pd
import numpy as np
import xlsxwriter as xw
import datetime
import operator

from pathlib import Path

def writeExcelAncestry(df, chart_references_py, file_name_py):
    now = datetime.datetime.now()
    now_str = now.strftime("%Y-%m-%d %H:%M:%S").replace(":", "_")
    # output_file_name = 'Excel Output - ' + now_str + '.xlsx'
    
    first_percent_col = [i for i, s in enumerate(df.columns) if 'Banner Name' == s][0] + 1
    all_count_cols = [i for i, s in enumerate(df.columns) if ' - Response Count' in s]
    first_count_col = all_count_cols[0]
    all_total_cols = [i for i, s in enumerate(df.columns) if ' - Total Response Count' in s]
    first_total_col = all_total_cols[0]
    all_notes_cols = [i for i, s in enumerate(df.columns) if ' - Notes' in s]
    first_notes_col = all_notes_cols[0]
    
    all_row_sig_cols = [i for i, s in enumerate(df.columns) if ' - row significance' in s]
    
    
    number_row_sig_cols = len(all_row_sig_cols)
    if number_row_sig_cols > 0:
        first_row_sig_col = all_row_sig_cols[0]
        last_row_sig_col = number_row_sig_cols[number_row_sig_cols]
    if len(all_count_cols) == 1:
        last_percent_col = first_percent_col
    else:
        last_percent_col = first_count_col - 1
    last_count_col = all_count_cols[(len(all_count_cols) - 1)]
    last_total_col = all_total_cols[(len(all_total_cols) - 1)]
    last_notes_col = all_notes_cols[(len(all_notes_cols) - 1)]
    
    stem_name_col = df['Stem Name']
    df = df.drop(columns=['Stem Name'])
    df.insert(loc=first_percent_col - 2, column = 'Stem Name', value = stem_name_col)
    
    segment_names = ["US Adults", "US Adults 18-44", "US Adults 45+", "FH Users", "FH Non-users"]
    
    next_chart_number = 0
    chart_dict = {}
    writer = pd.ExcelWriter(file_name_py, engine='xlsxwriter')
    workbook = writer.book
    
    title_cell_format = workbook.add_format({'font_color': 'white'})
    
    
    def makeChart(table, totals_table, stats_table, notes_table, sheet_name, table_loc, chart_number, write_stats_arg, write_notes_arg):
        
        table.to_excel(writer, sheet_name=sheet_name, index = False, startrow = table_loc[0], startcol = table_loc[1])
        
        table_number = 1
        location_spacing = table.shape[0] * table_number + 2 * table_number
        totals_table.to_excel(writer, sheet_name=sheet_name, index = False, startrow = table_loc[0] + location_spacing, startcol = table_loc[1])
        
        table_number += 1
        location_spacing = table.shape[0] * table_number + 2 * table_number
        stats_table.to_excel(writer, sheet_name=sheet_name, index = False, startrow = table_loc[0] + location_spacing, startcol = table_loc[1])
        
        table_number += 1
        location_spacing = table.shape[0] * table_number + 2 * table_number
        notes_table.to_excel(writer, sheet_name=sheet_name, index = False, startrow = table_loc[0] + location_spacing, startcol = table_loc[1])
        
        sheet_reference = '\'' + sheet_name + '\'!'
        
        chart_name = 'chart' + str(chart_number) # This is the name the program references via a dictionary, not what appears in Excel
        chart = workbook.add_chart({'type': 'column'}) # subtype???
        chart_dict[chart_name] = chart
        
        chart.set_size({'width': 1124, 'height': 450})
        chart.set_legend({'font': {'name': 'Segoe UI Light', 'size': 10}, 'position': 'bottom'})
        chart.set_x_axis({'line': {'color': '#D9D9D9'},
                          'num_font': {'name': 'Segoe UI Light', 'size': 11, 'color': '#595959'}})
        chart.set_y_axis({'line': {'none': True},
                          'num_font': {'name': 'Segoe UI Light', 'size': 11, 'color': '#595959'}, 
                          'major_gridlines': {'visible': True, 'line': {'color' : '#D9D9D9'}},
                          'max': 1, 'major_unit': .1})
        
        category_string_start = xw.utility.xl_rowcol_to_cell(row = table_loc[0] + 1, col = data_range_df_subset[0] + table_loc[1] - 1)
        category_string_end = xw.utility.xl_rowcol_to_cell(row = table_loc[0] + table.shape[0], col = data_range_df_subset[0] + table_loc[1] - 1)
        category_formula = sheet_reference + category_string_start + ':' + category_string_end
        
        for table_col in data_range_df_subset:
            series_string_start = xw.utility.xl_rowcol_to_cell(row = table_loc[0] + 1, col = table_loc[1] + table_col)
            series_string_end = xw.utility.xl_rowcol_to_cell(row = table_loc[0] + table.shape[0], col = table_loc[1] + table_col)
            series_formula = sheet_reference + series_string_start + ':' + series_string_end
            series_name_cell = sheet_reference + xw.utility.xl_rowcol_to_cell(row = table_loc[0], col = table_loc[1] + table_col)
            
            chart.add_series({'values': series_formula,
                             'name': series_name_cell,
                             # 'type': 'percentage',
                             'overlap': -25,
                             'categories': category_formula})
        
        chart_row = table_loc[0]
        chart_col = table_loc[1] - 2
        
        banner_qtext_col = [i for i, s in enumerate(table.columns) if 'Banner QText' == s][0] + table_loc[1] # In the OG TrendFinder, Banner Name is used instead of QText
        stem_qtext_col = [i for i, s in enumerate(table.columns) if 'Stem Name' == s][0] + table_loc[1]
        banner_qtext_cell = xw.utility.xl_rowcol_to_cell(row=table_loc[0]+1, col = banner_qtext_col) # This dictates first half of chart title in Excel (i.e. the qtext) 
            
        if table['Stem QText'].iloc[0] == 'Topline':
            chart_title_formula = '=' + sheet_reference + banner_qtext_cell + '&" - US Adults"'
        else:
            
            stem_qtext_cell = xw.utility.xl_rowcol_to_cell(row=table_loc[0]+1, col = stem_qtext_col)
            chart_title_formula = '=' + sheet_reference + banner_qtext_cell + '&" - "&' + sheet_reference + stem_qtext_cell  
        
        chart_title_cell = xw.utility.xl_rowcol_to_cell(row = chart_row, col = chart_col)
        
        worksheet = writer.sheets[sheet_name]
        worksheet.write(chart_title_cell, chart_title_formula, title_cell_format)
        
        chart.set_title({'name': '=' + sheet_reference + chart_title_cell, 'name_font': {'name': 'Segoe UI Light', 'size': 13, 'color': '#595959'}})         
        worksheet.insert_chart(chart_row, chart_col, chart_dict[chart_name])
        worksheet.set_zoom(80)
        
    
    sheet_number = 0
     # sheet_name_list not part of normal TrendFinder
    sheet_name_list = ["Ancestry is a brand for me", "Brings families together", "Explore FH in the next month", "Have used a FH sub", 
    "Have used a DNA Kit", "DNA kit popularity", "Privacy - Ancestry", "Privacy - 23andMe", "US Economy in 6 months", "Personal finance in 6 months", 
    "Concern public spaces", "Length of self isolation", "Comfortable shopping in stores", "Comfortable eating @ restaurant", "Comfortable traveling", 
    "Comfortable going to work", "Comfortable going public events", "Job impact COVID", "Hours of TV watched", "Thought about family", "Connected with family", 
    "Relflective Introspective"]
    
    # I don't believe there's a good reason to keep the answer IDs in the metadata information after this step, at least from the perspective of making this program function; it could help with troubleshooting or investigations later
    metadata_cols = ['Stem QID', 'Stem Group ID', 'Stem QText',
                        'Banner QID', 'Banner Group ID', 'Banner QText',
                        'Stem Name', 'Banner Name']
    
    data_cols_df = list(df.columns[first_percent_col:last_percent_col+1])
    df_subset = df.loc[:, metadata_cols + data_cols_df]
    
    ncol_df_subset = df_subset.shape[1]
    
    data_range_df_subset = range(len(metadata_cols), ncol_df_subset)
    
    df_totals_subset = df.loc[:, df.columns.isin(metadata_cols) | df.columns.str.endswith(' - Total Response Count')]
    df_stats_subset = df.loc[:, df.columns.isin(metadata_cols) | df.columns.str.endswith(' - stats test')]
    df_notes_subset = df.loc[:, df.columns.isin(metadata_cols) | df.columns.str.endswith(' - Notes')]
    
    table_multiplier = 2
    
    if len(df_stats_subset.columns) == len(metadata_cols):
        write_stats = False
    else:
        write_stats = True
        table_multiplier += 1
    
    if len(df_notes_subset.columns) == len(metadata_cols):
        write_notes = False
    else:
        write_notes = True
        table_multiplier += 1
    
    
    # Uses the rows of chart_references_py (table passed from R) to iterate through
    for indices, row in chart_references_py.iterrows():
        truncated_sheet_name = sheet_name_list[sheet_number] # Not part of normal TrendFinder
        sheet_number += 1 # Number of tab in Excel
        
        stem_q = row[0]
        stem_group = row[1]
        banner_q = row[2]
        banner_group = row[3]
        
        next_row = 0
        data_col = 2
        
        for segment in segment_names:
        
            banner_df = df_subset[df_subset["Banner QID"] == banner_q]
            banner_df = banner_df[banner_df["Banner Group ID"] == banner_group]
            banner_df = banner_df[banner_df["Stem Name"] == segment]
            
            banner_totals_df = df_totals_subset[df_totals_subset["Banner QID"] == banner_q]
            banner_totals_df = banner_totals_df[banner_totals_df["Banner Group ID"] == banner_group]
            banner_totals_df = banner_totals_df[banner_totals_df["Stem Name"] == segment]
            
            banner_stats_df = df_stats_subset[df_stats_subset["Banner QID"] == banner_q]
            banner_stats_df = banner_stats_df[banner_stats_df["Banner Group ID"] == banner_group]
            banner_stats_df = banner_stats_df[banner_stats_df["Stem Name"] == segment]
            
            banner_notes_df = df_notes_subset[df_notes_subset["Banner QID"] == banner_q]
            banner_notes_df = banner_notes_df[banner_notes_df["Banner Group ID"] == banner_group]
            banner_notes_df = banner_notes_df[banner_notes_df["Stem Name"] == segment]
            
            # banner_df.reset_index(drop=True, inplace=True)
            nrow_banner_df = banner_df.shape[0]
            # banner_qtext = banner_df['Banner QText'].iloc[0]
            
            ### Next two lines are used in normal TrendFinder
            # sheet_number_string = str(sheet_number)
            # truncated_sheet_name = sheet_number_string + "-" + (str(banner_q))[0:(31-1-len(sheet_number_string))]
            

            
            next_chart_number += 1
        
            if nrow_banner_df > 0:
                makeChart(table = banner_df,
                totals_table = banner_totals_df,
                stats_table = banner_stats_df,
                notes_table = banner_notes_df,
                sheet_name = truncated_sheet_name,
                table_loc = (next_row, data_col),
                chart_number = next_chart_number,
                write_stats_arg = write_stats,
                write_notes_arg = write_notes)
                      
                nrow_banner_df_with_details = (nrow_banner_df + 2) * table_multiplier
                    
                if nrow_banner_df_with_details < 24:
                    banner_row_spacing = 25
                else:
                    banner_row_spacing = nrow_banner_df_with_details + 2
                        
                next_row += banner_row_spacing
            
                worksheet = writer.sheets[truncated_sheet_name]
                worksheet.set_column(0, 0, 160)
                worksheet.set_column(3, 3, 15)
                worksheet.set_column(4, 5, 10)
                worksheet.set_column(6, 6, 15)
                worksheet.set_column(7, 8, 30)
                worksheet.set_column(9, 9, 15)
                worksheet.set_column(10, banner_df.shape[1]+3, 12)
        
    writer.save()
