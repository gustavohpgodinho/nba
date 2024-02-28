
import pandas as pd
import numpy as np
import re

def define_ind_variable(df):
    """
    Define an indicator variable 'ind' based on certain conditions in the DataFrame.

    Parameters:
    df (DataFrame): Input DataFrame containing the data.

    Returns:
    DataFrame: DataFrame with the 'ind' variable defined based on specified conditions.
    """

    # Initialize ind variable to 0
    df['ind'] = 0
    
    # Replace NA values in description column with empty string
    df['description'] = df['description'].fillna('')
    
    # Define 'ind' variable based on various conditions
    df.loc[df['description'].str.contains('BLOCK'), 'ind'] += 1
    df.loc[df['description'].str.contains('AST'), 'ind'] += 1
    df.loc[df['description'].str.contains("3PT"), 'ind'] += 10
    df.loc[(df['tp_event'] == 3) & (df['description'].str.contains("^MISS")), 'ind'] += 5
    df.loc[df['description'].str.contains('STEAL'), 'ind'] += 1
    df.loc[df['description'].str.contains(r'\(P[0-9]+\)'), 'ind'] += 10
    df.loc[df['description'].str.contains(r'P[0-9]+\.PN'), 'ind'] += 3
    df.loc[df['description'].str.contains(r'P[0-9]+\.T[0-9]+'), 'ind'] += 6
    df.loc[df['description'].str.contains('T#'), 'ind'] += 9
    df.loc[(df['clock'] == "12:00:00") & (df['period'] == 1) & (df['tp_event'] == 10), 'ind'] += 1
    df.loc[(df['clock'] == "05:00:00") & (df['period'] >= 5) & (df['tp_event'] == 10), 'ind'] += 2
    df.loc[df['tp_event'].isin([12, 13]), 'ind'] += df['period'] + 10

    # Cap ind variable at 15 for certain conditions
    df.loc[(df['tp_event'].isin([12, 13])) & (df['ind'] > 14), 'ind'] = 15

    return df

def pre_process_pbp_data(df):
    """
    Pre-process the play-by-play data DataFrame by defining additional variables.

    Parameters:
    df (DataFrame): Input DataFrame containing the play-by-play data.

    Returns:
    DataFrame: DataFrame with pre-processed data and additional variables.
    """

    # Replace '1' with 'r', '2' or '3' with 't', '4' or '5' with 'p', '6' or '7' with 'c' in the 'persons' column
    df['p'] = df['persons']\
        .replace({'1': 'r', '2': 't', '3': 't', '4': 'p', '5': 'p', '6': 'c', '7': 'c'}, regex = True)
    
    # Update 'p' values based on 'tp_event' values
    df.loc[df['tp_event'].isin([5, 7, 18]), 'p'] = df['p'].str[:2]
    
    # Initialize 'tp' variable
    df['tp'] = ""
    
    # Define 'tp' variable based on various conditions
    conditions = [
        (df['tp_event'] == 1) & (df['ind'] == 10) & (df['persons'].isin(['400', '500'])),
        (df['tp_event'] == 1) & (df['ind'] == 11) & (df['persons'].isin(['440', '550'])),
        (df['tp_event'] == 1) & (df['ind'] == 0) & (df['tp_subaction'].str.contains("Dunk")),
        (df['tp_event'] == 1) & (df['ind'] == 1) & (df['tp_subaction'].str.contains("Dunk")),
        (df['tp_event'] == 1) & (df['ind'] == 0) & (df['tp_subaction'].str.contains("Hook")),
        (df['tp_event'] == 1) & (df['ind'] == 1) & (df['tp_subaction'].str.contains("Hook")),
        (df['tp_event'] == 1) & (df['ind'] == 0) & (df['tp_subaction'].str.contains("Layup")),
        (df['tp_event'] == 1) & (df['ind'] == 1) & (df['tp_subaction'].str.contains("Layup")),
        (df['tp_event'] == 1) & (df['ind'] == 0) & (df['tp_subaction'].str.contains("Jump")) & (df['tp'] == ""),
        (df['tp_event'] == 1) & (df['ind'] == 1) & (df['tp_subaction'].str.contains("Jump")) & (df['tp'] == ""),
        (df['tp_event'] == 1) & (df['ind'] == 0) & (df['tp_subaction'].str.contains("Tip")) & (df['tp'] == ""),
        (df['tp_event'] == 1) & (df['ind'] == 1) & (df['tp_subaction'].str.contains("Tip")) & (df['tp'] == ""),
        (df['tp_event'] == 1) & (df['ind'] == 0) & (df['tp_subaction'].str.contains("Bank")) & (df['tp'] == ""),
        (df['tp_event'] == 1) & (df['ind'] == 1) & (df['tp_subaction'].str.contains("Bank")) & (df['tp'] == ""),
        (df['tp_event'] == 1) & (df['ind'] == 0) & (df['tp_subaction'].str.contains("Fadeaway")) & (df['tp'] == ""),
        (df['tp_event'] == 1) & (df['ind'] == 1) & (df['tp_subaction'].str.contains("Fadeaway")) & (df['tp'] == ""),
        (df['tp_event'] == 1) & (df['ind'] == 0) & (df['tp'] == ""),
        (df['tp_event'] == 1) & (df['ind'] == 1) & (df['tp'] == ""),
        (df['tp_event'] == 2) & (df['ind'] == 10) & (df['persons'].isin(['400', '500'])),
        (df['tp_event'] == 2) & (df['ind'] == 11) & (df['persons'].isin(['405', '504', '501'])),
        (df['tp_event'] == 2) & (df['ind'] == 0) & (df['tp_subaction'].str.contains("Dunk")),
        (df['tp_event'] == 2) & (df['ind'] == 1) & (df['tp_subaction'].str.contains("Dunk")),
        (df['tp_event'] == 2) & (df['ind'] == 0) & (df['tp_subaction'].str.contains("Hook")),
        (df['tp_event'] == 2) & (df['ind'] == 1) & (df['tp_subaction'].str.contains("Hook")),
        (df['tp_event'] == 2) & (df['ind'] == 0) & (df['tp_subaction'].str.contains("Layup")),
        (df['tp_event'] == 2) & (df['ind'] == 1) & (df['tp_subaction'].str.contains("Layup")),
        (df['tp_event'] == 2) & (df['ind'] == 0) & (df['tp_subaction'].str.contains("Jump")) & (df['tp'] == ""),
        (df['tp_event'] == 2) & (df['ind'] == 1) & (df['tp_subaction'].str.contains("Jump")) & (df['tp'] == ""),
        (df['tp_event'] == 2) & (df['ind'] == 0) & (df['tp_subaction'].str.contains("Tip")) & (df['tp'] == ""),
        (df['tp_event'] == 2) & (df['ind'] == 1) & (df['tp_subaction'].str.contains("Tip")) & (df['tp'] == ""),
        (df['tp_event'] == 2) & (df['ind'] == 0) & (df['tp_subaction'].str.contains("Bank")) & (df['tp'] == ""),
        (df['tp_event'] == 2) & (df['ind'] == 1) & (df['tp_subaction'].str.contains("Bank")) & (df['tp'] == ""),
        (df['tp_event'] == 2) & (df['ind'] == 0) & (df['tp_subaction'].str.contains("Fadeaway")) & (df['tp'] == ""),
        (df['tp_event'] == 2) & (df['ind'] == 1) & (df['tp_subaction'].str.contains("Fadeaway")) & (df['tp'] == ""),
        (df['tp_event'] == 2) & (df['ind'] == 0) & (df['tp'] == ""),
        (df['tp_event'] == 2) & (df['ind'] == 1) & (df['tp'] == "")]

    choices = ['5', '51', '1', '11', '2', '21', '3', '31', '4', '41', '3', '31', '4', '41', '4', '41', 
               '4', '41', '5', '52', '1', '12', '2', '22', '3', '32', '4', '42', '3', '32', '4', '42', 
               '4', '42', '4', '42']
    
    df['tp'] = np.select(conditions, choices, default = df['tp'])
    df.loc[df['tp'].in, 'tp'] = df['tp_subevent']
    return df

def define_plays_pcod(df):
    df['pcod'] = df['persons'] + df['cod']
    
    df.loc[df['pcod'] == "p450_10(0)", 'pcod'] = "p454_10(0)"
    df.loc[df['pcod'] == "p452_10(0)", 'pcod'] = "p454_10(0)"
    df.loc[df['pcod'] == "p453_10(0)", 'pcod'] = "p455_10(0)"
    df.loc[df['pcod'] == "p542_10(0)", 'pcod'] = "p454_10(0)"
    df.loc[df['pcod'] == "p544_10(0)", 'pcod'] = "p454_10(0)"
    df.loc[df['pcod'] == "p543_10(0)", 'pcod'] = "p455_10(0)"
    df.loc[df['pcod'] == "p545_10(0)", 'pcod'] = "p455_10(0)"
    
    df.loc[df['pcod'] == "p450_10(1)", 'pcod'] = "p454_10(1)"
    df.loc[df['pcod'] == "p452_10(1)", 'pcod'] = "p454_10(1)"
    df.loc[df['pcod'] == "p453_10(1)", 'pcod'] = "p455_10(1)"
    df.loc[df['pcod'] == "p542_10(1)", 'pcod'] = "p454_10(1)"
    df.loc[df['pcod'] == "p544_10(1)", 'pcod'] = "p454_10(1)"
    df.loc[df['pcod'] == "p543_10(1)", 'pcod'] = "p455_10(1)"
    df.loc[df['pcod'] == "p545_10(1)", 'pcod'] = "p455_10(1)"
    df.loc[df['pcod'] == "p545_10(0)", 'pcod'] = "p455_10(0)"
    
    df.loc[df['pcod'] == "p450_10(2)", 'pcod'] = "p454_10(2)"
    df.loc[df['pcod'] == "p452_10(2)", 'pcod'] = "p454_10(2)"
    df.loc[df['pcod'] == "p453_10(2)", 'pcod'] = "p455_10(2)"
    df.loc[df['pcod'] == "p542_10(2)", 'pcod'] = "p454_10(2)"
    df.loc[df['pcod'] == "p544_10(2)", 'pcod'] = "p454_10(2)"
    df.loc[df['pcod'] == "p543_10(2)", 'pcod'] = "p455_10(2)"
    df.loc[df['pcod'] == "p545_10(2)", 'pcod'] = "p455_10(2)"
    
    df.loc[df['cod'].str.contains('_18', regex = True), 'pcod'] = df['pcod'].str.replace(r"(p[0-9]{3})(_.+?)", "p001\\2", regex = True)
    
    df.loc[df['cod'].str.contains('_1\\(.\\)', regex = True), 'pcod'] = df['pcod'].str.replace(r"(p[24][0-9]{2})(_.+?)", "p400\\2", regex = True)
    df.loc[df['cod'].str.contains('_1\\(.a\\)', regex = True), 'pcod'] = df['pcod'].str.replace(r"(p[24][0-9]{2})(_.+?)", "p440\\2", regex = True)
    df.loc[df['cod'].str.contains('_1\\(.\\)', regex = True), 'pcod'] = df['pcod'].str.replace(r"(p[35][0-9]{2})(_.+?)", "p500\\2", regex = True)
    df.loc[df['cod'].str.contains('_1\\(.a\\)', regex = True), 'pcod'] = df['pcod'].str.replace(r"(p[35][0-9]{2})(_.+?)", "p550\\2", regex = True)

    df.loc[df['cod'].str.contains('_2\\(.\\)', regex = True), 'pcod'] = df['pcod'].str.replace(r"(p[24][0-9]{2})(_.+?)", "p400\\2", regex = True)
    df.loc[df['cod'].str.contains('_2\\(.b\\)', regex = True), 'pcod'] = df['pcod'].str.replace(r"(p[24][0-9]{2})(_.+?)", "p405\\2", regex = True)
    df.loc[df['cod'].str.contains('_2\\(.\\)', regex = True), 'pcod'] = df['pcod'].str.replace(r"(p[35][0-9]{2})(_.+?)", "p500\\2", regex = True)
    df.loc[df['cod'].str.contains('_2\\(.b\\)', regex = True), 'pcod'] = df['pcod'].str.replace(r"(p[35][0-9]{2})(_.+?)", "p504\\2", regex = True)

    df.loc[df['cod'].str.contains('_3\\(.\\)', regex = True), 'pcod'] = df['pcod'].str.replace(r"(p[24][0-9]{2})(_.+?)", "p400\\2", regex = True)
    df.loc[df['cod'].str.contains('_3\\(.\\)', regex = True), 'pcod'] = df['pcod'].str.replace(r"(p[35][0-9]{2})(_.+?)", "p500\\2", regex = True)

    df.loc[df['cod'].str.contains('_4\\(0[up]', regex = True), 'pcod'] = df['pcod'].str.replace(r"(p[24][0-9]{2})(_.+?)", "p400\\2", regex = True)
    df.loc[df['cod'].str.contains('_4\\(0[up]', regex = True), 'pcod'] = df['pcod'].str.replace(r"(p[35][0-9]{2})(_.+?)", "p500\\2", regex = True)
    df.loc[df['cod'].str.contains('_4\\([01]t\\)', regex = True), 'pcod'] = df['pcod'].str.replace(r"(p[24][0-9]{2})(_.+?)", "p200\\2", regex = True)
    df.loc[df['cod'].str.contains('_4\\([01]t\\)', regex = True), 'pcod'] = df['pcod'].str.replace(r"(p[35][0-9]{2})(_.+?)", "p300\\2", regex = True)

    df.loc[df['cod'].str.contains('_5\\((25|36|37|38|39|40)', regex = True), 'pcod'] = df['pcod'].str.replace(r"(p[024][0-9]{2})(_.+?)", "p200\\2", regex = True)
    df.loc[df['cod'].str.contains('_5\\((25|36|37|38|39|40)', regex = True), 'pcod'] = df['pcod'].str.replace(r"(p[35][0-9]{2})(_.+?)", "p300\\2", regex = True)
    df.loc[df['cod'].str.contains('_5\\((00|12|13|14|15|16|17|18|19|20|21|22|23|24|26|27|28|29|30|31|32|33|34|41|43)', regex = True), 'pcod'] = df['pcod'].str.replace(r"(p[024][0-9]{2})(_.+?)", "p400\\2", regex = True)
    df.loc[df['cod'].str.contains('_5\\((00|12|13|14|15|16|17|18|19|20|21|22|23|24|26|27|28|29|30|31|32|33|34|41|43)', regex = True), 'pcod'] = df['pcod'].str.replace(r"(p[35][0-9]{2})(_.+?)", "p500\\2", regex = True)
    df.loc[df['cod'].str.contains('_5\\((01|11|35)', regex = True), 'pcod'] = df['pcod'].str.replace(r"(p[024][0-9]{2})(_.+?)", "p450\\2", regex = True)
    df.loc[df['cod'].str.contains('_5\\((01|11|35)', regex = True), 'pcod'] = df['pcod'].str.replace(r"(p[35][0-9]{2})(_.+?)", "p540\\2", regex = True)
    
    df.loc[df['cod'].str.contains('_6\\(01c', regex = True), 'pcod'] = df['pcod'].str.replace(r"(p[0246][0-9]{2})(_.+?)", "p601\\2", regex = True)
    df.loc[df['cod'].str.contains('_6\\(01c', regex = True), 'pcod'] = df['pcod'].str.replace(r"(p[357][0-9]{2})(_.+?)", "p701\\2", regex = True)
    df.loc[df['cod'].str.contains('_6\\((01|05)p', regex = True), 'pcod'] = df['pcod'].str.replace(r"(p[0246][0-9]{2})(_.+?)", "p401\\2", regex = True)
    df.loc[df['cod'].str.contains('_6\\((01|05)p', regex = True), 'pcod'] = df['pcod'].str.replace(r"(p[357][0-9]{2})(_.+?)", "p501\\2", regex = True)
    df.loc[df['cod'].str.contains('_6\\((01|06|08|09)t', regex = True), 'pcod'] = df['pcod'].str.replace(r"(p[0246][0-9]{2})(_.+?)", "p201\\2", regex = True)
    df.loc[df['cod'].str.contains('_6\\((01|06|08|09)t', regex = True), 'pcod'] = df['pcod'].str.replace(r"(p[357][0-9]{2})(_.+?)", "p301\\2", regex = True)
    df.loc[df['cod'].str.contains('_6\\((10|11|12|13|14|15|16|17|19|20|21|22|18)', regex = True), 'pcod'] = df['pcod'].str.replace(r"(p[246][0-9]{2})(_.+?)", "p451\\2", regex = True)
    df.loc[df['cod'].str.contains('_6\\((10|11|12|13|14|15|16|17|19|20|21|22)', regex = True), 'pcod'] = df['pcod'].str.replace(r"(p[357][0-9]{2})(_.+?)", "p541\\2", regex = True)
    df.loc[df['cod'].str.contains('_6\\(4cc', regex = True), 'pcod'] = df['pcod'].str.replace(r"(p[234567][0-9]{2})(_.+?)", "p671\\2", regex = True)
    df.loc[df['cod'].str.contains('_6\\(4pc', regex = True), 'pcod'] = df['pcod'].str.replace(r"(p4[0-9]{2})(_.+?)", "p471\\2", regex = True)
    df.loc[df['cod'].str.contains('_6\\(4pc', regex = True), 'pcod'] = df['pcod'].str.replace(r"(p5[0-9]{2})(_.+?)", "p561\\2", regex = True)
    df.loc[df['cod'].str.contains('_6\\(4pc', regex = True), 'pcod'] = df['pcod'].str.replace(r"(p6[0-9]{2})(_.+?)", "p651\\2", regex = True)
    df.loc[df['cod'].str.contains('_6\\(4pc', regex = True), 'pcod'] = df['pcod'].str.replace(r"(p6[0-9]{2})(_.+?)", "p651\\2", regex = True)
    df.loc[df['cod'].str.contains('_6\\(4pc', regex = True), 'pcod'] = df['pcod'].str.replace(r"(p7[0-9]{2})(_.+?)", "p741\\2", regex = True)
    df.loc[df['cod'].str.contains('_6\\(4pp', regex = True), 'pcod'] = df['pcod'].str.replace(r"(p[0-9]{3})(_.+?)", "p451\\2", regex = True)
    
    df.loc[df['cod'].str.contains('_7\\((0|2|3|4|5|6)', regex = True), 'pcod'] = df['pcod'].str.replace(r"(p[0246][0-9]{2})(_.+?)", "p401\\2", regex = True)
    df.loc[df['cod'].str.contains('_7\\((0|2|3|4|5|6)', regex = True), 'pcod'] = df['pcod'].str.replace(r"(p[357][0-9]{2})(_.+?)", "p501\\2", regex = True)
    df.loc[df['cod'].str.contains('_7\\(1', regex = True), 'pcod'] = df['pcod'].str.replace(r"(p[0246][0-9]{2})(_.+?)", "p201\\2", regex = True)
    df.loc[df['cod'].str.contains('_7\\(1', regex = True), 'pcod'] = df['pcod'].str.replace(r"(p[357][0-9]{2})(_.+?)", "p301\\2", regex = True)

    df.loc[df['cod'].str.contains('_8\\(0', regex = True), 'pcod'] = df['pcod'].str.replace(r"(p[0246][0-9]{2})(_.+?)", "p440\\2", regex = True)
    df.loc[df['cod'].str.contains('_8\\(0', regex = True), 'pcod'] = df['pcod'].str.replace(r"(p[357][0-9]{2})(_.+?)", "p550\\2", regex = True)
    
    df.loc[df['cod'].str.contains('_9\\(0', regex = True), 'pcod'] = df['pcod'].str.replace(r"(p[01][0-9]{2})(_.+?)", "p100\\2", regex = True)
    df.loc[df['cod'].str.contains('_9\\(0', regex = True), 'pcod'] = df['pcod'].str.replace(r"(p[246][0-9]{2})(_.+?)", "p200\\2", regex = True)
    df.loc[df['cod'].str.contains('_9\\(0', regex = True), 'pcod'] = df['pcod'].str.replace(r"(p[357][0-9]{2})(_.+?)", "p300\\2", regex = True)
    df.loc[df['cod'].str.contains('_9\\(1', regex = True), 'pcod'] = df['pcod'].str.replace(r"(p[0246][0-9]{2})(_.+?)", "p200\\2", regex = True)
    df.loc[df['cod'].str.contains('_9\\(1', regex = True), 'pcod'] = df['pcod'].str.replace(r"(p[357][0-9]{2})(_.+?)", "p300\\2", regex = True)
    df.loc[df['cod'].str.contains('_9\\(2', regex = True), 'pcod'] = df['pcod'].str.replace(r"(p[01][0-9]{2})(_.+?)", "p100\\2", regex = True)
    df.loc[df['cod'].str.contains('_9\\(2', regex = True), 'pcod'] = df['pcod'].str.replace(r"(p[246][0-9]{2})(_.+?)", "p200\\2", regex = True)
    df.loc[df['cod'].str.contains('_9\\(2', regex = True), 'pcod'] = df['pcod'].str.replace(r"(p[357][0-9]{2})(_.+?)", "p300\\2", regex = True)
    df.loc[df['cod'].str.contains('_9\\(3', regex = True), 'pcod'] = df['pcod'].str.replace(r"(p[01][0-9]{2})(_.+?)", "p100\\2", regex = True)
    df.loc[df['cod'].str.contains('_9\\(3', regex = True), 'pcod'] = df['pcod'].str.replace(r"(p[246][0-9]{2})(_.+?)", "p200\\2", regex = True)
    df.loc[df['cod'].str.contains('_9\\(3', regex = True), 'pcod'] = df['pcod'].str.replace(r"(p[357][0-9]{2})(_.+?)", "p300\\2", regex = True)
                  
    return df

def define_plays_cod(df):
    # Initialize 'cod' variable
    df['cod'] = ''
    
    # Define 'tp' variable based on various conditions
    conditions = [
        (df['tp_event'] == 1) & (df['tp'] == '1'),
        (df['tp_event'] == 1) & (df['tp'] == '11'),
        (df['tp_event'] == 1) & (df['tp'] == '2'),
        (df['tp_event'] == 1) & (df['tp'] == '21'),
        (df['tp_event'] == 1) & (df['tp'] == '3'),
        (df['tp_event'] == 1) & (df['tp'] == '31'),
        (df['tp_event'] == 1) & (df['tp'] == '4'),
        (df['tp_event'] == 1) & (df['tp'] == '41'),
        (df['tp_event'] == 1) & (df['tp'] == '5'),
        (df['tp_event'] == 1) & (df['tp'] == '51'),        
        (df['tp_event'] == 1) & (df['tp'].isna()) & (df['ind'] == 0),
        (df['tp_event'] == 1) & (df['tp'].isna()) & (df['ind'] == 1),
        
        (df['tp_event'] == 2) & (df['tp'] == '1'),
        (df['tp_event'] == 2) & (df['tp'] == '12'),
        (df['tp_event'] == 2) & (df['tp'] == '2'),
        (df['tp_event'] == 2) & (df['tp'] == '22'),
        (df['tp_event'] == 2) & (df['tp'] == '3'),
        (df['tp_event'] == 2) & (df['tp'] == '32'),
        (df['tp_event'] == 2) & (df['tp'] == '4'),
        (df['tp_event'] == 2) & (df['tp'] == '42'),
        (df['tp_event'] == 2) & (df['tp'] == '5'),
        (df['tp_event'] == 2) & (df['tp'] == '52'),        
        (df['tp_event'] == 2) & (df['tp'].isna()) & (df['ind'] == 0),
        (df['tp_event'] == 2) & (df['tp'].isna()) & (df['ind'] == 1),
        
        (df['tp_event'] == 3) & (df['tp'] == '10') & (df['ind'] == 0),
        (df['tp_event'] == 3) & (df['tp'] == '10') & (df['ind'] == 5),
        (df['tp_event'] == 3) & (df['tp'] == '11') & (df['ind'] == 0),
        (df['tp_event'] == 3) & (df['tp'] == '11') & (df['ind'] == 5),
        (df['tp_event'] == 3) & (df['tp'] == '12') & (df['ind'] == 0),
        (df['tp_event'] == 3) & (df['tp'] == '12') & (df['ind'] == 5),
        (df['tp_event'] == 3) & (df['tp'] == '13') & (df['ind'] == 0),
        (df['tp_event'] == 3) & (df['tp'] == '13') & (df['ind'] == 5),
        (df['tp_event'] == 3) & (df['tp'] == '14') & (df['ind'] == 0),
        (df['tp_event'] == 3) & (df['tp'] == '14') & (df['ind'] == 5),
        (df['tp_event'] == 3) & (df['tp'] == '15') & (df['ind'] == 0),
        (df['tp_event'] == 3) & (df['tp'] == '15') & (df['ind'] == 5),
        (df['tp_event'] == 3) & (df['tp'] == '16') & (df['ind'] == 0),
        (df['tp_event'] == 3) & (df['tp'] == '16') & (df['ind'] == 5),
        (df['tp_event'] == 3) & (df['tp'] == '21') & (df['ind'] == 0),
        (df['tp_event'] == 3) & (df['tp'] == '21') & (df['ind'] == 5),
        (df['tp_event'] == 3) & (df['tp'] == '22') & (df['ind'] == 0),
        (df['tp_event'] == 3) & (df['tp'] == '22') & (df['ind'] == 5),
        (df['tp_event'] == 3) & (df['tp'] == '18') & (df['ind'] == 0),
        (df['tp_event'] == 3) & (df['tp'] == '18') & (df['ind'] == 5),
        (df['tp_event'] == 3) & (df['tp'] == '19') & (df['ind'] == 0),
        (df['tp_event'] == 3) & (df['tp'] == '19') & (df['ind'] == 5),
        (df['tp_event'] == 3) & (df['tp'] == '20') & (df['ind'] == 0),
        (df['tp_event'] == 3) & (df['tp'] == '20') & (df['ind'] == 5),
        (df['tp_event'] == 3) & (df['tp'] == '25') & (df['ind'] == 0),
        (df['tp_event'] == 3) & (df['tp'] == '25') & (df['ind'] == 5),
        (df['tp_event'] == 3) & (df['tp'] == '26') & (df['ind'] == 0),
        (df['tp_event'] == 3) & (df['tp'] == '26') & (df['ind'] == 5),
        (df['tp_event'] == 3) & (df['tp'] == '27') & (df['ind'] == 0),
        (df['tp_event'] == 3) & (df['tp'] == '27') & (df['ind'] == 5),
        (df['tp_event'] == 3) & (df['tp'] == '28') & (df['ind'] == 0),
        (df['tp_event'] == 3) & (df['tp'] == '28') & (df['ind'] == 5),
        (df['tp_event'] == 3) & (df['tp'] == '29') & (df['ind'] == 0),
        (df['tp_event'] == 3) & (df['tp'] == '29') & (df['ind'] == 5),

        (df['tp_event'] == 4) & (df['tp'] == '0') & (df['p'] == 't00'),
        (df['tp_event'] == 4) & (df['tp'] == '0') & (df['p'] == 'p00'),
        (df['tp_event'] == 4) & (df['tp'] == '1') & (df['p'] == 't00'),
        
        (df['tp_event'] == 5) & (df['tp'] == '42') & (df['p'] == '00') & (df['ind'] == 0),
        (df['tp_event'] == 5) & (df['tp'] == '42') & (df['p'] == 't0') & (df['ind'] == 0),
        (df['tp_event'] == 5) & (df['tp'] == '44') & (df['p'] == '00') & (df['ind'] == 0),
        (df['tp_event'] == 5) & (df['tp'] == '37') & (df['p'] == 'p0') & (df['ind'] == 6),
        (df['tp_event'] == 5) & (df['tp'] == '45') & (df['p'] == 'p0') & (df['ind'] == 6),
        (df['tp_event'] == 5) & (df['tp'] == '4') & (df['p'] == 'p0') & (df['ind'] == 6),
        (df['tp_event'] == 5) & (df['tp'] == '40') & (df['p'] == 'p0') & (df['ind'] == 6),
        (df['tp_event'] == 5) & (df['tp'] == '39') & (df['p'] == 'p0') & (df['ind'] == 6),
        (df['tp_event'] == 5) & (df['tp'] == '8') & (df['p'] == 'p0') & (df['ind'] == 6),
        (df['tp_event'] == 5) & (df['tp'] == '13') & (df['p'] == 'p0') & (df['ind'] == 6),
        (df['tp_event'] == 5) & (df['tp'] == '15') & (df['p'] == 'p0') & (df['ind'] == 6),
        (df['tp_event'] == 5) & (df['tp'] == '1') & (df['p'] == 'p0') & (df['ind'] == 6),
        (df['tp_event'] == 5) & (df['tp'] == '2') & (df['p'] == 'p0') & (df['ind'] == 6),
        (df['tp_event'] == 5) & (df['tp'] == '21') & (df['p'] == 'p0') & (df['ind'] == 6),
        (df['tp_event'] == 5) & (df['tp'] == '6') & (df['p'] == 'p0') & (df['ind'] == 6),
        (df['tp_event'] == 5) & (df['tp'] == '7') & (df['p'] == 'p0') & (df['ind'] == 6),
        (df['tp_event'] == 5) & (df['tp'] == '0') & (df['p'] == 'p0') & (df['ind'] == 6),
        (df['tp_event'] == 5) & (df['tp'] == '12') & (df['p'] == 'p0') & (df['ind'] == 6),
        (df['tp_event'] == 5) & (df['tp'] == '19') & (df['p'] == 'p0') & (df['ind'] == 6),
        (df['tp_event'] == 5) & (df['tp'] == '17') & (df['p'] == 'p0') & (df['ind'] == 6),
        (df['tp_event'] == 5) & (df['tp'] == '20') & (df['p'] == 'p0') & (df['ind'] == 6),
        (df['tp_event'] == 5) & (df['tp'] == '18') & (df['p'] == 'p0') & (df['ind'] == 6),
        (df['tp_event'] == 5) & (df['tp'] == '36') & (df['p'] == 'p0') & (df['ind'] == 6),
        (df['tp_event'] == 5) & (df['tp'] == '35') & (df['p'] == 'p0') & (df['ind'] == 6),
        (df['tp_event'] == 5) & (df['tp'] == '9') & (df['p'] == 'p0') & (df['ind'] == 6),
        (df['tp_event'] == 5) & (df['tp'] == '33') & (df['p'] == 'p0') & (df['ind'] == 6),
        (df['tp_event'] == 5) & (df['tp'] == '34') & (df['p'] == 'p0') & (df['ind'] == 6),
        (df['tp_event'] == 5) & (df['tp'] == '1') & (df['p'] == 'pp') & (df['ind'] == 7),
        (df['tp_event'] == 5) & (df['tp'] == '2') & (df['p'] == 'pp') & (df['ind'] == 7),
        (df['tp_event'] == 5) & (df['tp'] == '11') & (df['p'] == 't0') & (df['ind'] == 9),
        (df['tp_event'] == 5) & (df['tp'] == '9') & (df['p'] == 't0') & (df['ind'] == 9),
        (df['tp_event'] == 5) & (df['tp'] == '10') & (df['p'] == 't0') & (df['ind'] == 9),
        (df['tp_event'] == 5) & (df['tp'] == '24') & (df['p'] == 'p0') & (df['ind'] == 6),
        (df['tp_event'] == 5) & (df['tp'] == '44') & (df['p'] == 't0') & (df['ind'] == 9),

        (df['tp_event'] == 6) & (df['tp'] == '17') & (df['p'] == 'p0r') & (df['ind'] == 0),
        (df['tp_event'] == 6) & (df['tp'] == '11') & (df['p'] == 'c0r') & (df['ind'] == 0),
        (df['tp_event'] == 6) & (df['tp'] == '16') & (df['p'] == 'ppr') & (df['ind'] == 0),
        (df['tp_event'] == 6) & (df['tp'] == '18') & (df['p'] == 't0r') & (df['ind'] == 0),
        (df['tp_event'] == 6) & (df['tp'] == '10') & (df['p'] == 'ppr') & (df['ind'] == 0),
        (df['tp_event'] == 6) & (df['tp'] == '11') & (df['p'] == 't0r') & (df['ind'] == 0),
        (df['tp_event'] == 6) & (df['tp'] == '18') & (df['p'] == '00r') & (df['ind'] == 0),
        (df['tp_event'] == 6) & (df['tp'] == '25') & (df['p'] == 't0r') & (df['ind'] == 0),
        (df['tp_event'] == 6) & (df['tp'] == '30') & (df['p'] == 't0r') & (df['ind'] == 0),
        (df['tp_event'] == 6) & (df['tp'] == '11') & (df['p'] == '00r') & (df['ind'] == 0),
        (df['tp_event'] == 6) & (df['tp'] == '16') & (df['p'] == 'cpr') & (df['ind'] == 0),
        (df['tp_event'] == 6) & (df['tp'] == '12') & (df['p'] == 'c0r') & (df['ind'] == 0),
        (df['tp_event'] == 6) & (df['tp'] == '16') & (df['p'] == 'ccr') & (df['ind'] == 0),
        (df['tp_event'] == 6) & (df['tp'] == '25') & (df['p'] == '00r') & (df['ind'] == 0),
        (df['tp_event'] == 6) & (df['tp'] == '10') & (df['p'] == 'p0r') & (df['ind'] == 0),
        (df['tp_event'] == 6) & (df['tp'] == '16') & (df['p'] == 'p0r') & (df['ind'] == 0),
        (df['tp_event'] == 6) & (df['tp'] == '16') & (df['p'] == 'pcr') & (df['ind'] == 0),
        (df['tp_event'] == 6) & (df['tp'] == '18') & (df['p'] == 'tcr') & (df['ind'] == 0),
        (df['tp_event'] == 6) & (df['tp'] == '18') & (df['p'] == 'tpr') & (df['ind'] == 0),
        (df['tp_event'] == 6) & (df['tp'] == '2') & (df['p'] == 'ppr') & (df['ind'] == 3),
        (df['tp_event'] == 6) & (df['tp'] == '1') & (df['p'] == 'ppr') & (df['ind'] == 3),
        (df['tp_event'] == 6) & (df['tp'] == '28') & (df['p'] == 'ppr') & (df['ind'] == 3),
        (df['tp_event'] == 6) & (df['tp'] == '3') & (df['p'] == 'ppr') & (df['ind'] == 3),
        (df['tp_event'] == 6) & (df['tp'] == '26') & (df['p'] == 'ppr') & (df['ind'] == 3),
        (df['tp_event'] == 6) & (df['tp'] == '11') & (df['p'] == 'p0r') & (df['ind'] == 3),
        (df['tp_event'] == 6) & (df['tp'] == '14') & (df['p'] == 'ppr') & (df['ind'] == 3),
        (df['tp_event'] == 6) & (df['tp'] == '6') & (df['p'] == 'ppr') & (df['ind'] == 3),
        (df['tp_event'] == 6) & (df['tp'] == '9') & (df['p'] == 'ppr') & (df['ind'] == 3),
        (df['tp_event'] == 6) & (df['tp'] == '31') & (df['p'] == 'ppr') & (df['ind'] == 3),
        (df['tp_event'] == 6) & (df['tp'] == '15') & (df['p'] == 'ppr') & (df['ind'] == 3),
        (df['tp_event'] == 6) & (df['tp'] == '12') & (df['p'] == 'ppr') & (df['ind'] == 3),
        (df['tp_event'] == 6) & (df['tp'] == '12') & (df['p'] == 'p0r') & (df['ind'] == 3),
        (df['tp_event'] == 6) & (df['tp'] == '13') & (df['p'] == 'ppr') & (df['ind'] == 3),
        (df['tp_event'] == 6) & (df['tp'] == '13') & (df['p'] == 'p0r') & (df['ind'] == 3),
        (df['tp_event'] == 6) & (df['tp'] == '2') & (df['p'] == 'ppr') & (df['ind'] == 6),
        (df['tp_event'] == 6) & (df['tp'] == '1') & (df['p'] == 'ppr') & (df['ind'] == 6),
        (df['tp_event'] == 6) & (df['tp'] == '3') & (df['p'] == 'ppr') & (df['ind'] == 6),
        (df['tp_event'] == 6) & (df['tp'] == '28') & (df['p'] == 'ppr') & (df['ind'] == 6),
        (df['tp_event'] == 6) & (df['tp'] == '11') & (df['p'] == 'p0r') & (df['ind'] == 6),
        (df['tp_event'] == 6) & (df['tp'] == '26') & (df['p'] == 'ppr') & (df['ind'] == 6),
        (df['tp_event'] == 6) & (df['tp'] == '14') & (df['p'] == 'ppr') & (df['ind'] == 6),
        (df['tp_event'] == 6) & (df['tp'] == '6') & (df['p'] == 'ppr') & (df['ind'] == 6),
        (df['tp_event'] == 6) & (df['tp'] == '9') & (df['p'] == 'ppr') & (df['ind'] == 6),
        (df['tp_event'] == 6) & (df['tp'] == '31') & (df['p'] == 'ppr') & (df['ind'] == 6),
        (df['tp_event'] == 6) & (df['tp'] == '15') & (df['p'] == 'ppr') & (df['ind'] == 6),
        (df['tp_event'] == 6) & (df['tp'] == '13') & (df['p'] == 'p0r') & (df['ind'] == 6),
        (df['tp_event'] == 6) & (df['tp'] == '12') & (df['p'] == 'p0r') & (df['ind'] == 6),
        (df['tp_event'] == 6) & (df['tp'] == '1') & (df['p'] == 'p0r') & (df['ind'] == 6),
        (df['tp_event'] == 6) & (df['tp'] == '13') & (df['p'] == 'ppr') & (df['ind'] == 6),
        (df['tp_event'] == 6) & (df['tp'] == '14') & (df['p'] == 'p0r') & (df['ind'] == 6),
        (df['tp_event'] == 6) & (df['tp'] == '8') & (df['p'] == 'ppr') & (df['ind'] == 6),
        (df['tp_event'] == 6) & (df['tp'] == '4') & (df['p'] == 'ppr') & (df['ind'] == 10),
        (df['tp_event'] == 6) & (df['tp'] == '26') & (df['p'] == 'ppr') & (df['ind'] == 10),
        
        (df['tp_event'] == 7) & (df['tp'] == '1') & (df['p'] == '00'),
        (df['tp_event'] == 7) & (df['tp'] == '1') & (df['p'] == 't0'),
        (df['tp_event'] == 7) & (df['tp'] == '2') & (df['p'] == 'p0'),
        (df['tp_event'] == 7) & (df['tp'] == '3') & (df['p'] == 'p0'),
        (df['tp_event'] == 7) & (df['tp'] == '4') & (df['p'] == 'p0'),
        (df['tp_event'] == 7) & (df['tp'] == '5') & (df['p'] == 'p0'),
        (df['tp_event'] == 7) & (df['tp'] == '6') & (df['p'] == 'p0'),
        
        (df['tp_event'] == 8),

        (df['tp_event'] == 9) & (df['tp'] == '1'),
        (df['tp_event'] == 9) & (df['tp'] == '7'),

        (df['tp_event'] == 10) & (df['tp'] == '0') & (df['ind'].isin([1, 2])),
        (df['tp_event'] == 10) & (df['tp'] == '0') & (df['ind'] == 0),
        (df['tp_event'] == 10) & (df['tp'] == '1'),
        (df['tp_event'] == 10) & (df['tp'] == '0') & (df['ind'].isna()),

        (df['tp_event'] == 11) & (df['p'] == 'c0r'),
        (df['tp_event'] == 11) & (df['p'] == 'p0r'),

        (df['tp_event'] == 12) & (df['ind'] == 11),
        (df['tp_event'] == 12) & (df['ind'] == 12),
        (df['tp_event'] == 12) & (df['ind'] == 13),
        (df['tp_event'] == 12) & (df['ind'] == 14),
        (df['tp_event'] == 12) & (df['ind'] == 15),

        (df['tp_event'] == 13) & (df['ind'] == 11),
        (df['tp_event'] == 13) & (df['ind'] == 12),
        (df['tp_event'] == 13) & (df['ind'] == 13),
        (df['tp_event'] == 13) & (df['ind'] == 14),
        (df['tp_event'] == 13) & (df['ind'] == 15),

        (df['tp_event'] == 18) & (df['tp'] == '0'),
        (df['tp_event'] == 18) & (df['tp'] == '1'),
        (df['tp_event'] == 18) & (df['tp'] == '2'),
        (df['tp_event'] == 18) & (df['tp'] == '3'),
        (df['tp_event'] == 18) & (df['tp'] == '4'),
        (df['tp_event'] == 18) & (df['tp'] == '5'),
        (df['tp_event'] == 18) & (df['tp'] == '6'),
        (df['tp_event'] == 18) & (df['tp'] == '7')]
        
    choices = ["_1(1)", "_1(1a)", "_1(2)", "_1(2a)", "_1(3)", "_1(3a)", "_1(4)", "_1(4a)", "_1(5)", "_1(5a)", "_1(4)", "_1(4a)",
               "_2(1)", "_2(1b)", "_2(2)", "_2(2b)", "_2(3)", "_2(3b)", "_2(4)", "_2(4b)", "_2(5)", "_2(5b)", "_2(4)", "_2(4b)",
               "_3(11c)", "_3(11w)", "_3(12c)", "_3(12w)", "_3(22c)", "_3(22w)", "_3(13c)", "_3(13w)", "_3(23c)", "_3(23w)", 
               "_3(33c)", "_3(33w)", "_3(40c)", "_3(40w)", "_3(40c)", "_3(40w)", "_3(40c)", "_3(40w)", "_3(51c)", "_3(51w)", 
               "_3(52c)", "_3(52w)", "_3(61c)", "_3(61w)", "_3(71c)", "_3(71w)", "_3(72c)", "_3(72w)", "_3(81c)", "_3(81w)", 
               "_3(82c)", "_3(82w)", "_3(83c)", "_3(83w)", "_4(0t)", "_4(0p)", "_4(1t)", "_5(39u)", "_5(39t)", "_5(40u)", 
               "_5(13p)", "_5(34p)", "_5(12p)", "_5(32p)", "_5(31p)", "_5(16p)", "_5(18p)", "_5(19p)", "_5(34p)", "_5(33p)", 
               "_5(24p)", "_5(14p)", "_5(15p)", "_5(00p)", "_5(17p)", "_5(22p)", "_5(20p)", "_5(23p)", "_5(21p)", "_5(30p)", 
               "_5(29p)", "_5(41p)", "_5(27p)", "_5(28p)", "_5(11p)", "_5(35p)", "_5(37t)", "_5(38t)", "_5(36t)", "_5(36t)", 
               "_5(40t)", "_6(05p)", "_6(01c)", "_6(4pp)", "_6(06t)", "_6(18a)", "_6(01t)", "_6(06u)", "_6(08t)", "_6(09t)", 
               "_6(01u)", "_6(4pc)", "_6(01c)", "_6(4cc)", "_6(08u)", "_6(18a)", "_6(4pp)", "_6(4pc)", "_6(06t)", "_6(06t)",
               "_6(11p)", "_6(10p)", "_6(21p)", "_6(12p)", "_6(13a)", "_6(01p)", "_6(19p)", "_6(15p)", "_6(17p)", "_6(22p)", 
               "_6(20p)", "_6(01p)", "_6(01p)", "_6(01p)", "_6(01p)", "_6(11a)", "_6(10a)", "_6(12a)", "_6(21a)", "_6(01p)", 
               "_6(13a)", "_6(19a)", "_6(15a)", "_6(17a)", "_6(22a)", "_6(20a)", "_6(01p)", "_6(01p)", "_6(10a)", "_6(01p)", 
               "_6(19a)", "_6(16a)", "_6(13a)", "_6(13a)", "_7(1u)", "_7(1t)", "_7(2p)", "_7(3p)", "_7(4p)", "_7(5p)", 
               "_7(6p)", "_8(0)", "_9(0)", "_9(1)", "_10(0)", "_10(1)", "_10(2)", "_10(0)", "_11(1)", "_11(0)", "_12(1)", 
               "_12(2)", "_12(3)", "_12(4)", "_12(0)", "_13(1)", "_13(2)", "_13(3)", "_13(4)", "_13(0)", "_18(0)", "_18(1)", 
               "_18(2)", "_18(3)", "_18(4)", "_18(5)", "_18(6)", "_18(7)"]
    
    df['cod'] = np.select(conditions, choices, default = df['cod'])
    
    conditions = [
        (df['tp_event'] == 3) & (df['tp'] == '24') & (df['ind'] == 0),
        (df['tp_event'] == 3) & (df['tp'] == '23') & (df['ind'] == 5),
        (df['tp_event'] == 4) & (df['p'] == '000') & (df['cod'] == ""),
        (df['tp_event'] == 4) & (df['p'] == 'p00') & (df['cod'] == ""),
        (df['tp_event'] == 4) & (df['p'] == 't00') & (df['cod'] == ""),
        (df['tp_event'] == 5) & (df['tp'] == '0') & (df['cod'] == ""),
        (df['tp_event'] == 5) & (df['tp'] == '1') & (df['cod'] == ""),
        (df['tp_event'] == 5) & (df['tp'] == '2') & (df['cod'] == ""),
        (df['tp_event'] == 5) & (df['tp'] == '3') & (df['cod'] == ""),
        (df['tp_event'] == 5) & (df['tp'] == '4') & (df['cod'] == ""),
        (df['tp_event'] == 5) & (df['tp'] == '5') & (df['cod'] == ""),
        (df['tp_event'] == 5) & (df['tp'] == '8') & (df['cod'] == ""),
        (df['tp_event'] == 5) & (df['tp'] == '11') & (df['cod'] == ""),
        (df['tp_event'] == 5) & (df['tp'] == '12') & (df['cod'] == ""),
        (df['tp_event'] == 5) & (df['tp'] == '13') & (df['cod'] == ""),
        (df['tp_event'] == 5) & (df['tp'] == '15') & (df['cod'] == ""),
        (df['tp_event'] == 5) & (df['tp'] == '17') & (df['cod'] == ""),
        (df['tp_event'] == 5) & (df['tp'] == '18') & (df['cod'] == ""),
        (df['tp_event'] == 5) & (df['tp'] == '19') & (df['cod'] == ""),
        (df['tp_event'] == 5) & (df['tp'] == '20') & (df['cod'] == ""),
        (df['tp_event'] == 5) & (df['tp'] == '23') & (df['cod'] == ""),
        (df['tp_event'] == 5) & (df['tp'] == '32') & (df['cod'] == ""),
        (df['tp_event'] == 5) & (df['tp'] == '37') & (df['cod'] == ""),
        (df['tp_event'] == 5) & (df['tp'] == '38') & (df['cod'] == ""),
        (df['tp_event'] == 5) & (df['tp'] == '40') & (df['cod'] == ""),
        (df['tp_event'] == 5) & (df['tp'] == '41') & (df['ind'] == 6) & (df['cod'] == ""),
        (df['tp_event'] == 5) & (df['tp'] == '41') & (df['ind'] == 9) & (df['cod'] == ""),
        (df['tp_event'] == 5) & (df['tp'] == '41') & (df['ind'] == 7) & (df['cod'] == ""),
        (df['tp_event'] == 5) & (df['tp'] == '42') & (df['cod'] == ""),
        (df['tp_event'] == 5) & (df['tp'] == '43') & (df['cod'] == ""),
        (df['tp_event'] == 5) & (df['tp'] == '52') & (df['cod'] == ""),
        (df['tp_event'] == 6) & (df['tp'] == '16') & (df['cod'] == ""),
        (df['tp_event'] == 6) & (df['tp'] == '1') & (df['ind'] == 0) & (df['cod'] == ""),
        (df['tp_event'] == 6) & (df['tp'] == '2') & (df['ind'] == 0) & (df['cod'] == ""),
        (df['tp_event'] == 6) & (df['tp'] == '2') & (df['ind'] == 3) & (df['cod'] == ""),
        (df['tp_event'] == 6) & (df['tp'] == '5') & (df['ind'] == 3) & (df['cod'] == ""),
        (df['tp_event'] == 6) & (df['tp'] == '5') & (df['ind'] == 6) & (df['cod'] == ""),
        (df['tp_event'] == 6) & (df['tp'] == '19') & (df['ind'] == 3) & (df['cod'] == ""),
        (df['tp_event'] == 6) & (df['tp'] == '19') & (df['ind'] == 6) & (df['cod'] == ""),
        (df['tp_event'] == 6) & (df['tp'] == '19') & (df['cod'] == ""),
        (df['tp_event'] == 6) & (df['tp'] == '27') & (df['ind'] == 3) & (df['cod'] == ""),
        (df['tp_event'] == 6) & (df['tp'] == '27') & (df['ind'] == 6) & (df['cod'] == ""),
        (df['tp_event'] == 6) & (df['tp'] == '29') & (df['ind'] == 3) & (df['cod'] == ""),
        (df['tp_event'] == 6) & (df['tp'] == '29') & (df['ind'] == 6) & (df['cod'] == ""),
        (df['tp_event'] == 6) & (df['tp'] == '32') & (df['cod'] == ""),
        (df['tp_event'] == 7) & (df['tp'] == '0') & (df['cod'] == ""),
        (df['tp_event'] == 7) & (df['tp'] == '6') & (df['cod'] == ""),
        (df['tp_event'] == 7) & (df['tp'] == '18') & (df['cod'] == ""),
        (df['tp_event'] == 9) & (df['tp'] == '2') & (df['cod'] == ""),
        (df['tp_event'] == 9) & (df['tp'] == '4') & (df['cod'] == ""),
        (df['tp_event'] == 9) & (df['tp'] == '0') & (df['cod'] == "")]
    
    choices = ['_3(00c)', '_3(00w)', '_4(0u)', '_4(0p)', '_4(1t)', '_5(01p)', '_5(34p)', '_5(33p)', '_5(31p)', '_5(12p)',
               '_5(13p)', '_5(16p)', '_5(37t)', '_5(25t)', '_5(18p)', '_5(19p)', '_5(20p)', '_5(21p)', '_5(22p)', '_5(23p)',
               '_5(26p)', '_5(43p)', '_5(13p)', '_5(38t)', '_5(35p)', '_5(33p)', '_5(33p)', '_5(35p)', '_5(39t)', '_5(31p)',
               '_5(01p)', '_6(4cc)', '_6(10a)', '_6(11a)', '_6(11p)', '_6(14p)', '_6(14a)', '_6(01p)', '_6(01p)', '_6(01p)',
               '_6(10p)', '_6(10a)', '_6(11p)', '_6(11a)', '_6(01p)', '_7(0p)', '_7(6p)', '_7(0p)', '_9(0)', '_9(2)', '_9(3)']

    df['cod'] = np.select(conditions, choices, default = df['cod'])

    df = define_plays_pcod(df)

    return df

def execute_pre_process_data(df):

    df = define_ind_variable(df)
    df['tp_subaction'] = df['tp_subaction'].str.strip()
    df['tp_subaction'] = np.where(df['tp_subaction'] == '', np.nan, df['tp_subaction'])
    df = pre_process_pbp_data(df)
    df = define_plays_cod(df)
    return df

def fix_plays_without_team(df):

    df.loc[(df['game_id'] == '0021900620') & (df['action_id'] == 81) & (df['cod'] == '_6(01u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == '0021900691') & (df['action_id'] == 76) & (df['cod'] == '_6(01u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == '0021900703') & (df['action_id'] == 290) & (df['cod'] == '_6(01u)'), 'location'] = 'h'
    df.loc[df['cod'] == '_6(01u)', 'cod'] = '_6(01t)'

    df.loc[(df['game_id'] == '0021900291') & (df['action_id'] == 65) & (df['cod'] == '_6(06u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == '0021900324') & (df['action_id'] == 192) & (df['cod'] == '_6(06u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == '0021900326') & (df['action_id'] == 489) & (df['cod'] == '_6(06u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == '0021900341') & (df['action_id'] == 247) & (df['cod'] == '_6(06u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == '0021900361') & (df['action_id'] == 249) & (df['cod'] == '_6(06u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == '0021900361') & (df['action_id'] == 296) & (df['cod'] == '_6(06u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == '0021900379') & (df['action_id'] == 282) & (df['cod'] == '_6(06u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == '0021900395') & (df['action_id'] == 363) & (df['cod'] == '_6(06u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == '0021900404') & (df['action_id'] == 541) & (df['cod'] == '_6(06u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == '0021900487') & (df['action_id'] == 255) & (df['cod'] == '_6(06u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == '0021900524') & (df['action_id'] == 399) & (df['cod'] == '_6(06u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == '0021900535') & (df['action_id'] == 159) & (df['cod'] == '_6(06u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == '0021900844') & (df['action_id'] == 318) & (df['cod'] == '_6(06u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == '0021901237') & (df['action_id'] == 295) & (df['cod'] == '_6(06u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == '0021901313') & (df['action_id'] == 123) & (df['cod'] == '_6(06u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == '0022000317') & (df['action_id'] == 258) & (df['cod'] == '_6(06u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == '0021900291') & (df['action_id'] == 375) & (df['cod'] == '_6(06u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == '0021900342') & (df['action_id'] == 316) & (df['cod'] == '_6(06u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == '0021900387') & (df['action_id'] == 273) & (df['cod'] == '_6(06u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == '0021900460') & (df['action_id'] == 477) & (df['cod'] == '_6(06u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == '0021900682') & (df['action_id'] == 113) & (df['cod'] == '_6(06u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == '0021900757') & (df['action_id'] == 245) & (df['cod'] == '_6(06u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == '0021900883') & (df['action_id'] == 405) & (df['cod'] == '_6(06u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == '0041900177') & (df['action_id'] == 207) & (df['cod'] == '_6(06u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == '0022000042') & (df['action_id'] == 331) & (df['cod'] == '_6(06u)'), 'location'] = 'h'
    df.loc[df['cod'] == '_6(06u)', 'cod'] = '_6(06t)'

    df.loc[(df['game_id'] == '0041800406') & (df['action_id'] == 482) & (df['cod'] == '_6(08u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == '0022000285') & (df['action_id'] == 462) & (df['cod'] == '_6(08u)'), 'location'] = 'v'
    df.loc[df['cod'] == '_6(08u)', 'cod'] = '_6(08t)'

    df.loc[(df['game_id'] == '0021800001') & (df['action_id'] == 344) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == '0021800002') & (df['action_id'] == 502) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == '0021800020') & (df['action_id'] == 116) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == '0021800031') & (df['action_id'] == 384) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == '0021800035') & (df['action_id'] == 364) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == '0021800037') & (df['action_id'] == 427) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == '0021800038') & (df['action_id'] == 147) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == '0021800045') & (df['action_id'] == 248) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == '0021800058') & (df['action_id'] == 173) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == '0021800060') & (df['action_id'] == 206) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == '0021800073') & (df['action_id'] == 233) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == '0021800077') & (df['action_id'] == 191) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == '0021800079') & (df['action_id'] == 118) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == '0021800082') & (df['action_id'] == 259) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == '0021800085') & (df['action_id'] == 292) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == '0021800087') & (df['action_id'] == 457) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == '0021800088') & (df['action_id'] == 460) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == '0021800092') & (df['action_id'] == 342) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == '0021800103') & (df['action_id'] == 43) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == '0021800108') & (df['action_id'] == 201) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == '0021800108') & (df['action_id'] == 316) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == '0021800109') & (df['action_id'] == 88) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == '0021800112') & (df['action_id'] == 366) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == '0021800119') & (df['action_id'] == 171) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == '0021800123') & (df['action_id'] == 274) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == '0021800128') & (df['action_id'] == 2) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == '0021800129') & (df['action_id'] == 178) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == '0021800136') & (df['action_id'] == 427) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == '0021800144') & (df['action_id'] == 345) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == '0021800146') & (df['action_id'] == 319) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == '0021800148') & (df['action_id'] == 151) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == '0021800151') & (df['action_id'] == 41) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == '0021800152') & (df['action_id'] == 188) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == '0021800154') & (df['action_id'] == 36) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == '0021800161') & (df['action_id'] == 388) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == '0021800163') & (df['action_id'] == 243) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == '0021800168') & (df['action_id'] == 90) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == '0021800170') & (df['action_id'] == 123) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == '0021800176') & (df['action_id'] == 301) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == '0021800179') & (df['action_id'] == 298) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == '0021800181') & (df['action_id'] == 380) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == '0021800192') & (df['action_id'] == 51) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == '0021800196') & (df['action_id'] == 390) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == '0021800197') & (df['action_id'] == 363) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == '0021800198') & (df['action_id'] == 516) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == '0021800200') & (df['action_id'] == 216) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == '0021800210') & (df['action_id'] == 195) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == '0021800212') & (df['action_id'] == 6) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == '0021800216') & (df['action_id'] == 202) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == '0021800223') & (df['action_id'] == 76) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == '0021800235') & (df['action_id'] == 491) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == '0021800236') & (df['action_id'] == 87) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == '0021800237') & (df['action_id'] == 195) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == '0021800243') & (df['action_id'] == 107) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == '0021800247') & (df['action_id'] == 507) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == '0021800254') & (df['action_id'] == 222) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == '0021800255') & (df['action_id'] == 300) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == '0021800258') & (df['action_id'] == 35) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == '0021800261') & (df['action_id'] == 220) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == '0021800271') & (df['action_id'] == 329) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == '0021800277') & (df['action_id'] == 313) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == '0021800278') & (df['action_id'] == 156) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == '0021800279') & (df['action_id'] == 90) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == '0021800282') & (df['action_id'] == 180) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == '0021800306') & (df['action_id'] == 535) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == '0021800313') & (df['action_id'] == 53) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == '0021800321') & (df['action_id'] == 225) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == '0021800323') & (df['action_id'] == 30) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == '0021800329') & (df['action_id'] == 383) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == '0021800337') & (df['action_id'] == 403) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == '0021800338') & (df['action_id'] == 355) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == '0021800343') & (df['action_id'] == 267) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == '0021800344') & (df['action_id'] == 333) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == '0021800346') & (df['action_id'] == 157) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == '0021800347') & (df['action_id'] == 72) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == '0021800357') & (df['action_id'] == 246) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == '0021800363') & (df['action_id'] == 366) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == '0021800371') & (df['action_id'] == 365) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == '0021800372') & (df['action_id'] == 236) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == '0021800387') & (df['action_id'] == 244) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == '0021800396') & (df['action_id'] == 192) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == '0021800404') & (df['action_id'] == 270) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == '0021800408') & (df['action_id'] == 357) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == '0021800412') & (df['action_id'] == 163) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == '0021800420') & (df['action_id'] == 538) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == '0021800423') & (df['action_id'] == 379) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == '0021800426') & (df['action_id'] == 25) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == '0021800429') & (df['action_id'] == 259) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == '0021800450') & (df['action_id'] == 186) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == '0021800452') & (df['action_id'] == 148) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == '0021800462') & (df['action_id'] == 207) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == '0021800463') & (df['action_id'] == 363) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == '0021800497') & (df['action_id'] == 86) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == '0021800502') & (df['action_id'] == 433) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == '0021800508') & (df['action_id'] == 61) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == '0021800509') & (df['action_id'] == 289) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == '0021800513') & (df['action_id'] == 313) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == '0021800515') & (df['action_id'] == 10) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == '0021800522') & (df['action_id'] == 451) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == '0021800524') & (df['action_id'] == 307) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == '0021800527') & (df['action_id'] == 168) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == '0021800554') & (df['action_id'] == 328) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0021800558") & (df['action_id'] ==  63) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0021800559") & (df['action_id'] == 189) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0021800560") & (df['action_id'] == 108) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0021800561") & (df['action_id'] == 278) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0021800568") & (df['action_id'] == 219) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0021800573") & (df['action_id'] == 295) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0021800576") & (df['action_id'] == 153) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0021800583") & (df['action_id'] ==  78) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0021800586") & (df['action_id'] == 482) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0021800588") & (df['action_id'] == 223) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0021800594") & (df['action_id'] ==  45) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0021800606") & (df['action_id'] == 435) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0021800611") & (df['action_id'] == 213) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0021800618") & (df['action_id'] == 375) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0021800622") & (df['action_id'] == 416) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0021800626") & (df['action_id'] ==  52) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0021800629") & (df['action_id'] == 411) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0021800633") & (df['action_id'] == 267) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0021800649") & (df['action_id'] == 437) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0021800656") & (df['action_id'] == 131) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0021800659") & (df['action_id'] == 453) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0021800660") & (df['action_id'] == 230) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0021800661") & (df['action_id'] == 192) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0021800674") & (df['action_id'] == 264) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0021800680") & (df['action_id'] == 315) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0021800693") & (df['action_id'] ==  69) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0021800696") & (df['action_id'] ==  81) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0021800698") & (df['action_id'] == 120) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0021800702") & (df['action_id'] == 221) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0021800707") & (df['action_id'] == 133) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0021800714") & (df['action_id'] == 214) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0021800715") & (df['action_id'] == 304) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0021800733") & (df['action_id'] == 337) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0021800738") & (df['action_id'] == 283) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0021800739") & (df['action_id'] == 350) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0021800743") & (df['action_id'] == 127) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0021800747") & (df['action_id'] == 393) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0021800750") & (df['action_id'] == 399) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0021800751") & (df['action_id'] == 255) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0021800763") & (df['action_id'] ==  95) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0021800773") & (df['action_id'] == 216) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0021800775") & (df['action_id'] == 403) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0021800780") & (df['action_id'] == 410) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0021800782") & (df['action_id'] == 266) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0021800783") & (df['action_id'] == 302) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0021800784") & (df['action_id'] ==  73) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0021800785") & (df['action_id'] == 260) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0021800806") & (df['action_id'] == 220) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0021800814") & (df['action_id'] ==  46) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0021800815") & (df['action_id'] == 332) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0021800818") & (df['action_id'] == 166) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0021800819") & (df['action_id'] == 381) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0021800830") & (df['action_id'] == 332) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0021800832") & (df['action_id'] == 492) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0021800838") & (df['action_id'] == 331) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0021800840") & (df['action_id'] ==  55) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0021800841") & (df['action_id'] == 407) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0021800845") & (df['action_id'] == 232) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0021800848") & (df['action_id'] == 182) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0021800854") & (df['action_id'] == 480) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0021800868") & (df['action_id'] == 320) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0021800869") & (df['action_id'] == 145) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0021800878") & (df['action_id'] == 332) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0021800885") & (df['action_id'] == 374) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0021800892") & (df['action_id'] == 310) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0021800900") & (df['action_id'] == 452) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0021800912") & (df['action_id'] == 331) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0021800915") & (df['action_id'] == 368) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0021800921") & (df['action_id'] == 240) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0021800924") & (df['action_id'] == 248) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0021800925") & (df['action_id'] == 386) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0021800927") & (df['action_id'] ==  54) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0021800935") & (df['action_id'] == 175) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0021800937") & (df['action_id'] == 435) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0021800939") & (df['action_id'] == 123) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0021800938") & (df['action_id'] == 226) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0021800943") & (df['action_id'] == 479) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0021800963") & (df['action_id'] == 132) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0021800967") & (df['action_id'] == 414) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0021800970") & (df['action_id'] == 432) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0021800973") & (df['action_id'] == 194) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0021800983") & (df['action_id'] ==  14) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0021800987") & (df['action_id'] == 279) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0021800994") & (df['action_id'] == 178) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0021801001") & (df['action_id'] == 223) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0021801001") & (df['action_id'] == 402) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0021801004") & (df['action_id'] == 299) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0021801009") & (df['action_id'] == 321) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0021801012") & (df['action_id'] == 202) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0021801015") & (df['action_id'] == 393) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0021801035") & (df['action_id'] ==  44) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0021801037") & (df['action_id'] == 404) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0021801045") & (df['action_id'] == 237) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0021801044") & (df['action_id'] == 420) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0021801061") & (df['action_id'] == 156) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0021801063") & (df['action_id'] == 344) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0021801074") & (df['action_id'] == 214) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0021801079") & (df['action_id'] == 337) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0021801111") & (df['action_id'] == 161) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0021801119") & (df['action_id'] == 256) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0021801123") & (df['action_id'] ==  11) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0021801130") & (df['action_id'] == 275) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0021801133") & (df['action_id'] ==  70) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0021801140") & (df['action_id'] == 244) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0021801173") & (df['action_id'] == 299) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0021801184") & (df['action_id'] == 136) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0021801191") & (df['action_id'] ==   2) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0021801193") & (df['action_id'] == 379) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0021801207") & (df['action_id'] ==  10) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0021801217") & (df['action_id'] ==   2) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0021801220") & (df['action_id'] == 483) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0021801221") & (df['action_id'] == 122) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0021801227") & (df['action_id'] == 287) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0021801230") & (df['action_id'] == 322) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0041800142") & (df['action_id'] == 271) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0041800223") & (df['action_id'] ==  75) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0041800225") & (df['action_id'] == 367) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0041800301") & (df['action_id'] ==  23) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0041800303") & (df['action_id'] == 288) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0041800305") & (df['action_id'] == 206) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0041800404") & (df['action_id'] == 404) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0041800405") & (df['action_id'] ==  67) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0021900025") & (df['action_id'] ==  37) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0021900017") & (df['action_id'] == 237) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0021900031") & (df['action_id'] ==   2) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0021900037") & (df['action_id'] == 208) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0021900045") & (df['action_id'] == 291) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0021900048") & (df['action_id'] == 100) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0021900055") & (df['action_id'] == 251) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0021900060") & (df['action_id'] == 416) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0021900062") & (df['action_id'] == 253) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0021900065") & (df['action_id'] == 160) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0021900070") & (df['action_id'] == 144) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0021900081") & (df['action_id'] == 276) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0021900082") & (df['action_id'] == 134) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0021900087") & (df['action_id'] == 295) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0021900100") & (df['action_id'] == 233) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0021900105") & (df['action_id'] == 127) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0021900121") & (df['action_id'] == 294) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0021900128") & (df['action_id'] == 194) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0021900134") & (df['action_id'] == 334) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0021900136") & (df['action_id'] == 279) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0021900140") & (df['action_id'] == 177) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0021900152") & (df['action_id'] == 492) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0021900154") & (df['action_id'] == 389) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0021900157") & (df['action_id'] == 180) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0021900159") & (df['action_id'] == 392) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0021900164") & (df['action_id'] == 415) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0021900177") & (df['action_id'] == 317) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0021900178") & (df['action_id'] == 339) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0021900188") & (df['action_id'] == 313) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0021900193") & (df['action_id'] == 381) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0021900213") & (df['action_id'] == 303) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0021900219") & (df['action_id'] == 454) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0021900227") & (df['action_id'] == 188) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0021900230") & (df['action_id'] == 147) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0021900260") & (df['action_id'] == 239) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0021900262") & (df['action_id'] ==  76) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0021900271") & (df['action_id'] == 372) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0021900273") & (df['action_id'] == 252) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0021900276") & (df['action_id'] == 171) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0021900283") & (df['action_id'] == 334) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0021900291") & (df['action_id'] == 244) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0021900296") & (df['action_id'] == 419) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0021900301") & (df['action_id'] == 257) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0021900306") & (df['action_id'] ==  63) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0021900307") & (df['action_id'] == 241) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0021900309") & (df['action_id'] == 317) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0021900313") & (df['action_id'] == 105) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0021900317") & (df['action_id'] ==  67) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0021900319") & (df['action_id'] ==  29) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0021900330") & (df['action_id'] ==  99) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0021900332") & (df['action_id'] ==  11) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0021900339") & (df['action_id'] == 467) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0021900340") & (df['action_id'] == 153) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0021900342") & (df['action_id'] ==  67) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0021900344") & (df['action_id'] == 261) & (df['cod'] == '_7(1u)'), 'location'] = 'h'
    df.loc[(df['game_id'] == "0021900345") & (df['action_id'] == 431) & (df['cod'] == '_7(1u)'), 'location'] = 'h'

    df.loc[(df['game_id'] == "0021800008") & (df['action_id'] == 522) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021800014") & (df['action_id'] == 314) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021800019") & (df['action_id'] ==  23) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021800048") & (df['action_id'] == 387) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021800058") & (df['action_id'] == 438) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021800065") & (df['action_id'] == 247) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021800069") & (df['action_id'] == 161) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021800073") & (df['action_id'] == 360) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021800075") & (df['action_id'] ==  66) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021800076") & (df['action_id'] == 217) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021800092") & (df['action_id'] == 248) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021800101") & (df['action_id'] == 283) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021800114") & (df['action_id'] == 213) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021800132") & (df['action_id'] == 404) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021800134") & (df['action_id'] == 306) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021800140") & (df['action_id'] == 345) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021800145") & (df['action_id'] ==  73) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021800146") & (df['action_id'] == 257) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021800155") & (df['action_id'] ==  94) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021800157") & (df['action_id'] == 399) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021800158") & (df['action_id'] == 255) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021800160") & (df['action_id'] == 417) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021800163") & (df['action_id'] == 372) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021800164") & (df['action_id'] == 246) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021800168") & (df['action_id'] == 137) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021800173") & (df['action_id'] == 348) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021800176") & (df['action_id'] == 126) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021800178") & (df['action_id'] == 237) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021800181") & (df['action_id'] == 365) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021800185") & (df['action_id'] == 340) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021800188") & (df['action_id'] == 361) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021800194") & (df['action_id'] == 416) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021800209") & (df['action_id'] == 114) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021800213") & (df['action_id'] == 338) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021800215") & (df['action_id'] == 222) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021800219") & (df['action_id'] ==  56) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021800232") & (df['action_id'] ==  92) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021800232") & (df['action_id'] ==  92) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021800238") & (df['action_id'] == 119) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021800239") & (df['action_id'] ==  99) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021800246") & (df['action_id'] == 371) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021800254") & (df['action_id'] == 239) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021800259") & (df['action_id'] == 316) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021800263") & (df['action_id'] == 499) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021800268") & (df['action_id'] == 297) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021800273") & (df['action_id'] == 314) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021800275") & (df['action_id'] == 231) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021800283") & (df['action_id'] == 342) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021800293") & (df['action_id'] == 355) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021800301") & (df['action_id'] == 160) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021800302") & (df['action_id'] == 129) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021800305") & (df['action_id'] ==  59) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021800313") & (df['action_id'] == 213) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021800315") & (df['action_id'] == 113) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021800317") & (df['action_id'] == 317) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021800322") & (df['action_id'] ==  12) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021800325") & (df['action_id'] == 220) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021800328") & (df['action_id'] == 504) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021800331") & (df['action_id'] == 175) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021800336") & (df['action_id'] ==  97) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021800340") & (df['action_id'] == 153) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021800341") & (df['action_id'] ==  80) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021800349") & (df['action_id'] == 307) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021800370") & (df['action_id'] == 242) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021800379") & (df['action_id'] == 321) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021800381") & (df['action_id'] ==  22) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021800383") & (df['action_id'] == 418) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021800386") & (df['action_id'] == 471) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021800396") & (df['action_id'] == 420) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021800398") & (df['action_id'] == 363) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021800400") & (df['action_id'] == 282) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021800402") & (df['action_id'] == 280) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021800420") & (df['action_id'] ==  63) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021800428") & (df['action_id'] == 248) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021800446") & (df['action_id'] == 245) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021800447") & (df['action_id'] == 451) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021800450") & (df['action_id'] == 292) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021800477") & (df['action_id'] ==  47) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021800488") & (df['action_id'] == 150) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021800496") & (df['action_id'] == 267) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021800497") & (df['action_id'] == 347) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021800500") & (df['action_id'] == 417) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021800513") & (df['action_id'] == 229) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021800521") & (df['action_id'] == 396) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021800538") & (df['action_id'] == 225) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021800541") & (df['action_id'] == 356) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021800551") & (df['action_id'] ==  44) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021800557") & (df['action_id'] == 235) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021800558") & (df['action_id'] == 159) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021800559") & (df['action_id'] ==  82) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021800571") & (df['action_id'] == 472) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021800582") & (df['action_id'] == 415) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021800583") & (df['action_id'] == 130) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021800591") & (df['action_id'] ==  89) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021800599") & (df['action_id'] == 190) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021800603") & (df['action_id'] == 280) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021800605") & (df['action_id'] == 136) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021800608") & (df['action_id'] ==  43) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021800609") & (df['action_id'] == 560) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021800613") & (df['action_id'] == 220) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021800620") & (df['action_id'] == 221) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021800629") & (df['action_id'] == 273) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021800630") & (df['action_id'] ==  65) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021800630") & (df['action_id'] == 309) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021800631") & (df['action_id'] == 108) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021800632") & (df['action_id'] ==  26) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021800638") & (df['action_id'] == 267) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021800653") & (df['action_id'] == 479) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021800678") & (df['action_id'] == 496) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021800680") & (df['action_id'] == 229) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021800690") & (df['action_id'] == 260) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021800692") & (df['action_id'] == 437) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021800693") & (df['action_id'] ==  90) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021800697") & (df['action_id'] == 212) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021800707") & (df['action_id'] == 192) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021800708") & (df['action_id'] == 476) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021800727") & (df['action_id'] == 379) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021800728") & (df['action_id'] ==  64) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021800730") & (df['action_id'] == 387) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021800731") & (df['action_id'] == 165) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021800733") & (df['action_id'] == 499) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021800734") & (df['action_id'] == 496) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021800741") & (df['action_id'] == 201) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021800769") & (df['action_id'] == 390) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021800773") & (df['action_id'] == 288) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021800775") & (df['action_id'] == 379) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021800781") & (df['action_id'] == 335) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021800782") & (df['action_id'] == 124) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021800785") & (df['action_id'] == 234) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021800795") & (df['action_id'] == 419) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021800806") & (df['action_id'] ==  78) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021800809") & (df['action_id'] == 312) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021800812") & (df['action_id'] == 353) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021800819") & (df['action_id'] == 217) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021800820") & (df['action_id'] == 108) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021800826") & (df['action_id'] == 139) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021800836") & (df['action_id'] ==  92) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021800841") & (df['action_id'] == 113) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021800843") & (df['action_id'] == 298) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021800866") & (df['action_id'] == 223) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021800868") & (df['action_id'] == 308) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021800881") & (df['action_id'] == 293) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021800883") & (df['action_id'] == 357) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021800886") & (df['action_id'] == 361) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021800894") & (df['action_id'] == 210) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021800896") & (df['action_id'] == 191) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021800904") & (df['action_id'] == 513) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021800905") & (df['action_id'] == 281) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021800928") & (df['action_id'] == 500) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021800948") & (df['action_id'] == 171) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021800970") & (df['action_id'] == 296) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021800976") & (df['action_id'] == 516) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021800977") & (df['action_id'] == 199) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021800979") & (df['action_id'] == 214) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021800984") & (df['action_id'] == 118) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021800999") & (df['action_id'] == 324) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021801000") & (df['action_id'] == 365) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021801002") & (df['action_id'] == 464) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021801009") & (df['action_id'] == 107) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021801013") & (df['action_id'] == 334) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021801014") & (df['action_id'] == 278) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021801021") & (df['action_id'] == 372) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021801033") & (df['action_id'] == 192) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021801037") & (df['action_id'] ==  45) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021801046") & (df['action_id'] == 173) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021801066") & (df['action_id'] ==  75) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021801067") & (df['action_id'] == 268) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021801072") & (df['action_id'] == 429) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021801073") & (df['action_id'] ==  67) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021801079") & (df['action_id'] ==  96) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021801090") & (df['action_id'] == 341) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021801093") & (df['action_id'] == 339) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021801113") & (df['action_id'] == 278) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021801120") & (df['action_id'] == 243) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021801121") & (df['action_id'] ==  51) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021801129") & (df['action_id'] == 220) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021801138") & (df['action_id'] == 233) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021801147") & (df['action_id'] ==  46) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021801151") & (df['action_id'] == 349) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021801159") & (df['action_id'] == 150) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021801166") & (df['action_id'] == 277) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021801168") & (df['action_id'] == 165) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021801174") & (df['action_id'] == 274) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021801180") & (df['action_id'] == 247) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021801185") & (df['action_id'] == 135) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021801193") & (df['action_id'] == 330) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021801195") & (df['action_id'] == 412) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021801198") & (df['action_id'] == 194) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021801222") & (df['action_id'] == 354) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021801227") & (df['action_id'] == 329) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0041800142") & (df['action_id'] == 283) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0041800152") & (df['action_id'] ==  31) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0041800102") & (df['action_id'] == 221) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0041800133") & (df['action_id'] == 374) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0041800103") & (df['action_id'] == 112) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0041800174") & (df['action_id'] == 426) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0041800115") & (df['action_id'] ==  35) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0041800165") & (df['action_id'] == 269) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0041800213") & (df['action_id'] ==  79) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0041800226") & (df['action_id'] ==  25) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0041800306") & (df['action_id'] == 271) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0041800402") & (df['action_id'] == 244) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0041800404") & (df['action_id'] == 422) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021900001") & (df['action_id'] == 297) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021900002") & (df['action_id'] ==  71) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021900008") & (df['action_id'] == 344) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021900014") & (df['action_id'] == 194) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021900021") & (df['action_id'] == 209) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021900026") & (df['action_id'] ==  99) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021900027") & (df['action_id'] == 111) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021900031") & (df['action_id'] == 465) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021900040") & (df['action_id'] ==  97) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021900045") & (df['action_id'] == 190) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021900056") & (df['action_id'] == 310) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021900065") & (df['action_id'] == 260) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021900078") & (df['action_id'] == 240) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021900081") & (df['action_id'] ==  72) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021900082") & (df['action_id'] ==  63) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021900091") & (df['action_id'] == 427) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021900105") & (df['action_id'] == 532) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021900107") & (df['action_id'] == 192) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021900109") & (df['action_id'] == 142) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021900128") & (df['action_id'] == 130) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021900136") & (df['action_id'] == 509) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021900148") & (df['action_id'] == 283) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021900149") & (df['action_id'] == 161) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021900160") & (df['action_id'] == 458) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021900164") & (df['action_id'] == 259) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021900159") & (df['action_id'] == 259) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021900168") & (df['action_id'] == 369) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021900172") & (df['action_id'] == 351) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021900200") & (df['action_id'] == 269) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021900205") & (df['action_id'] == 268) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021900216") & (df['action_id'] == 277) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021900217") & (df['action_id'] == 234) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021900221") & (df['action_id'] == 186) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021900256") & (df['action_id'] == 194) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021900264") & (df['action_id'] == 320) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021900269") & (df['action_id'] ==  80) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021900271") & (df['action_id'] == 123) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021900273") & (df['action_id'] == 355) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021900285") & (df['action_id'] ==  18) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021900289") & (df['action_id'] ==  92) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021900291") & (df['action_id'] ==  27) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021900305") & (df['action_id'] ==  21) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021900313") & (df['action_id'] == 420) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021900318") & (df['action_id'] ==  67) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021900323") & (df['action_id'] == 271) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021900324") & (df['action_id'] ==  70) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021900333") & (df['action_id'] == 555) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021900341") & (df['action_id'] == 135) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021900344") & (df['action_id'] == 344) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0021900351") & (df['action_id'] == 307) & (df['cod'] == '_7(1u)'), 'location'] = 'v'
    df.loc[df['cod'] == '_7(1u)', 'cod'] = '_7(1t)'

    df.loc[(df['game_id'] == "0042100171") & (df['action_id'] == 364) & (df['cod'] == '_5(40u)'), 'location'] = 'v'
    df.loc[df['cod'] == '_5(40u)', 'cod'] = '_5(40t)'
    
    df.loc[(df['game_id'] == "0022000285") & (df['action_id'] == 463) & (df['cod'] == '_5(39u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0022000215") & (df['action_id'] == 512) & (df['cod'] == '_5(39u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0022101135") & (df['action_id'] == 465) & (df['cod'] == '_5(39u)'), 'location'] = 'v'
    df.loc[(df['game_id'] == "0022200323") & (df['action_id'] == 538) & (df['cod'] == '_5(39u)'), 'location'] = 'v'
    df.loc[df['cod'] == '_5(39u)', 'cod'] = '_5(39t)'
    
    return df

def build_df_pbp_plays(df):

    df = df.drop_duplicates()
    df = define_ind_variable(df)
    df['tp_action'] = df['tp_action'].str.strip()
    df['tp_subaction'] = df['tp_subaction'].str.strip()
    df = df[['season', 'game_id', 'num_event', 'action_id', 
             'period', 'clock', 'tp_event', 'tp_action', 
             'tp_subevent', 'tp_subaction', 'persons', 
             'location', 'ind', 'xlegacy', 'ylegacy', 
             'shot_distance', 'description']]
    df['action_id'] = df['action_id'].astype(int)
    df = df.sort_values(by=['season', 'game_id', 'period', 'clock', 'action_id'], 
                        ascending=[True, True, True, False, True])
    df = pre_process_pbp_data(df)
    df['p'] = np.where(df['tp_event'].isin([5, 7, 18]), df['p'].str[:2], df['p'])
    df['ind'] = np.where((df['tp_event'].isin([12, 13])) & (df['ind'] > 14), 15, df['ind'])
    df = define_plays_cod(df)
    df = fix_plays_without_team(df)
    return df

# Define a function to extract the shot based on the condition
def extract_shot(row):
    if condition_1(row) and not condition_2(row):
        return re.sub(r"(\[player\]\s+)(.+)(\s+\(cnt PTS\))", r"\2", row['gram'])
    elif condition_1(row) and condition_2(row):
        return re.sub(r"(\[player\]\s+\[dist\]\s+)(.+)(\s+\(cnt PTS\))", r"\2", row['gram'])
    else:
        return row['shot']
    
def put_grammar_in_pbp(df):
    df['ind_dist'] = df['description'].str.contains(r'\d+\'').astype(int)
    df['ind_no'] = df['description'].str.contains("No ").astype(int)
    df['ind_unk'] = df['description'].str.contains("Unknown").astype(int)
    df['ind_off'] = df['description'].str.contains("Offensive").astype(int)
    df['ind_viot'] = df['description'].str.contains("Violation Turnover").astype(int)
    df['ind_ba'] = df['description'].str.contains("Backcourt Turnover").astype(int)
    df['ind_step'] = df['description'].str.contains("Step Out").astype(int)
    df['ind_play'] = df['description'].str.contains("Player Out").astype(int)
    df['ind_out'] = df['description'].str.contains("Out [Oo]f").astype(int)
    df['ind_pos'] = df['description'].str.contains("Poss ").astype(int)
    df['ind_ste'] = df['description'].str.contains("STEAL ").astype(int)
    df['ind_sec'] = df['description'].str.contains("Second Violation").astype(int)
    df['ind_exc'] = df['description'].str.contains("^Excess").astype(int)
    df['ind_trace'] = df['description'].str.contains(r'\s+\-\s+').astype(int)
    df['ind_turn'] = ''.join([str(df[col]) for col in ['ind_no', 'ind_off', 'ind_viot', 'ind_ba', 'ind_step', 
                                                       'ind_play', 'ind_out', 'ind_pos', 'ind_ste', 'ind_sec', 
                                                       'ind_exc', 'ind_trace']])
    df['ind_tfoul'] = df['description'].str.contains("Foul:T.FOUL").astype(int)
    df['ind_tfoul2'] = df['description'].str.contains("T.FOUL").astype(int)
    df['ind_uns'] = df['description'].str.contains("Unsportsmanlike").astype(int)
    df['ind_pn'] = df['description'].str.contains("\.PN").astype(int)
    df['cnt_par'] = df['description'].str.count(r'\(')
    df['ind_hang'] = df['description'].str.contains("HANGING").astype(int)
    df['ind_tau'] = df['description'].str.contains("Taunting").astype(int)
    df['ind_delay'] = df['description'].str.contains("Delay").astype(int)
    df['ind_block'] = df['description'].str.contains("Block").astype(int)
    df['ind_dot'] = df['description'].str.contains(r'\(\.\)').astype(int)
    df['ind_charge'] = df['description'].str.contains("Charge").astype(int)
    df['ind_tcnt'] = df['description'].str.contains("\.T").astype(int)
    df['ind_double'] = df['description'].str.contains("DOUBLE").astype(int)
    df['ind_trace'] = df['description'].str.contains(r'\s+\-\s+').astype(int)
    df['ind_fouls'] = ''.join([str(df[col]) for col in ['ind_tfoul', 'ind_tfoul2', 'ind_uns', 'ind_pn', 'cnt_par', 
                                                        'ind_hang', 'ind_tau', 'ind_delay', 'ind_block', 'ind_dot', 
                                                        'ind_charge', 'ind_tcnt', 'ind_double', 'ind_trace']])
    df['ind_vio'] = df['description'].str.contains("Violation:").astype(int)
    df['ind_violation'] = ''.join([str(df[col]) for col in ['ind_no', 'ind_vio', 'cnt_par', 'ind_dot']])
    
    df['ind_full'] = df['description'].str.contains("Full ").astype(int)
    df['ind_reg'] = df['description'].str.contains("Reg\.").astype(int)
    df['ind_reg2'] = df['description'].str.contains("Regular").astype(int)
    df['ind_short'] = df['description'].str.contains("Short").astype(int)
    df['ind_short2'] = df['description'].str.contains("Short$").astype(int)
    df['ind_time'] = df['description'].str.contains("Timeout:\s+").astype(int)
    df['ind_timeout'] = ''.join([str(df[col]) for col in ['ind_full', 'ind_reg', 'ind_reg2', 'ind_short', 
                                                          'ind_short2', 'ind_time', 'ind_no']])
    df['ind_tipto_'] = df['description'].str.contains("Tip to .+?").astype(int)
    df['ind_CC'] = df['description'].str.contains("CC").astype(int)
    df['ind_jumpball'] = ''.join([str(df[col]) for col in ['ind_tipto_', 'ind_CC']])
    df['ind_other'] = df['description'].str.contains("Other").astype(int)
    df['ind_2ndtech'] = df['description'].str.contains("Second Technical").astype(int)
    df['ind_type1'] = df['description'].str.contains("Type 1").astype(int)
    df['ind_type2'] = df['description'].str.contains("Type 2").astype(int)
    df['ind_ejection'] = ''.join([str(df[col]) for col in ['ind_other', 'ind_2ndtech', 'ind_no', 
                                                           'ind_type1', 'ind_type2']])
    df['ind_1st'] = df['description'].str.contains("1st").astype(int)
    df['ind_2nd'] = df['description'].str.contains("2nd").astype(int)
    df['ind_3rd'] = df['description'].str.contains("3rd").astype(int)
    df['ind_4th'] = df['description'].str.contains("4th").astype(int)
    df['ind_OT'] = df['description'].str.contains("OT").astype(int)
    df['ind_period'] = ''.join([str(df[col]) for col in ['ind_1st', 'ind_2nd', 'ind_3rd', 
                                                         'ind_4th', 'ind_OT']])

    df['gram'] = ''
    df.loc[(df['cod'] == '_1(1)') & (df['tp_subaction'] == 'Alley Oop Dunk Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = '[player] Alley Oop Dunk (cnt PTS)'
    df.loc[(df['cod'] == '_1(1)') & (df['tp_subaction'] == 'Alley Oop Dunk Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = '[player] [dist] Alley Oop Dunk (cnt PTS)'
    df.loc[(df['cod'] == '_1(1)') & (df['tp_subaction'] == 'Cutting Dunk Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = '[player] Cutting Dunk Shot (cnt PTS)'
    df.loc[(df['cod'] == '_1(1)') & (df['tp_subaction'] == 'Cutting Dunk Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = '[player] [dist] Cutting Dunk Shot (cnt PTS)'
    df.loc[(df['cod'] == '_1(1)') & (df['tp_subaction'] == 'Driving Dunk Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = '[player] Driving Dunk (cnt PTS)'
    df.loc[(df['cod'] == '_1(1)') & (df['tp_subaction'] == 'Driving Dunk Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = '[player] [dist] Driving Dunk (cnt PTS)'
    df.loc[(df['cod'] == '_1(1)') & (df['tp_subaction'] == 'Driving Reverse Dunk Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = '[player] Driving Reverse Dunk Shot (cnt PTS)'
    df.loc[(df['cod'] == '_1(1)') & (df['tp_subaction'] == 'Driving Reverse Dunk Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = '[player] [dist] Driving Reverse Dunk Shot (cnt PTS)'
    df.loc[(df['cod'] == '_1(1)') & (df['tp_subaction'] == 'Driving Slam Dunk Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = '[player] Driving Slam Dunk (cnt PTS)'
    df.loc[(df['cod'] == '_1(1)') & (df['tp_subaction'] == 'Driving Slam Dunk Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = '[player] [dist] Driving Slam Dunk (cnt PTS)'
    df.loc[(df['cod'] == '_1(1)') & (df['tp_subaction'] == 'Dunk Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = '[player] Dunk (cnt PTS)'
    df.loc[(df['cod'] == '_1(1)') & (df['tp_subaction'] == 'Dunk Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = '[player] [dist] Dunk (cnt PTS)'
    df.loc[(df['cod'] == '_1(1)') & (df['tp_subaction'] == 'Putback Dunk Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = '[player] Putback Dunk (cnt PTS)'
    df.loc[(df['cod'] == '_1(1)') & (df['tp_subaction'] == 'Putback Dunk Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = '[player] [dist] Putback Dunk (cnt PTS)'
    df.loc[(df['cod'] == '_1(1)') & (df['tp_subaction'] == 'Putback Reverse Dunk Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = '[player] [dist] Putback Reverse Dunk (cnt PTS)'
    df.loc[(df['cod'] == '_1(1)') & (df['tp_subaction'] == 'Putback Slam Dunk Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = '[player] Putback Slam Dunk (cnt PTS)'
    df.loc[(df['cod'] == '_1(1)') & (df['tp_subaction'] == 'Putback Slam Dunk Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = '[player] [dist] Putback Slam Dunk (cnt PTS)'
    df.loc[(df['cod'] == '_1(1)') & (df['tp_subaction'] == 'Reverse Dunk Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = '[player] Reverse Dunk (cnt PTS)'
    df.loc[(df['cod'] == '_1(1)') & (df['tp_subaction'] == 'Reverse Dunk Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = '[player] [dist] Reverse Dunk (cnt PTS)'
    df.loc[(df['cod'] == '_1(1)') & (df['tp_subaction'] == 'Reverse Slam Dunk Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = '[player] Reverse Slam Dunk (cnt PTS)'
    df.loc[(df['cod'] == '_1(1)') & (df['tp_subaction'] == 'Reverse Slam Dunk Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = '[player] [dist] Reverse Slam Dunk (cnt PTS)'
    df.loc[(df['cod'] == '_1(1)') & (df['tp_subaction'] == 'Running Alley Oop Dunk Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = '[player] Running Alley Oop Dunk Shot (cnt PTS)'
    df.loc[(df['cod'] == '_1(1)') & (df['tp_subaction'] == 'Running Dunk Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = '[player] Running Dunk (cnt PTS)'
    df.loc[(df['cod'] == '_1(1)') & (df['tp_subaction'] == 'Running Dunk Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = '[player] [dist] Running Dunk (cnt PTS)'
    df.loc[(df['cod'] == '_1(1)') & (df['tp_subaction'] == 'Running Reverse Dunk Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = '[player] Running Reverse Dunk Shot (cnt PTS)'
    df.loc[(df['cod'] == '_1(1)') & (df['tp_subaction'] == 'Running Reverse Dunk Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = '[player] [dist] Running Reverse Dunk Shot (cnt PTS)'
    df.loc[(df['cod'] == '_1(1)') & (df['tp_subaction'] == 'Running Slam Dunk Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = '[player] Running Slam Dunk (cnt PTS)'
    df.loc[(df['cod'] == '_1(1)') & (df['tp_subaction'] == 'Running Slam Dunk Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = '[player] [dist] Running Slam Dunk (cnt PTS)'
    df.loc[(df['cod'] == '_1(1)') & (df['tp_subaction'] == 'Slam Dunk Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = '[player] Slam Dunk (cnt PTS)'
    df.loc[(df['cod'] == '_1(1)') & (df['tp_subaction'] == 'Slam Dunk Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = '[player] [dist] Slam Dunk (cnt PTS)'
    df.loc[(df['cod'] == '_1(1)') & (df['tp_subaction'] == 'Tip Dunk Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = '[player] Tip Dunk Shot (cnt PTS)'
    df.loc[(df['cod'] == '_1(1)') & (df['tp_subaction'] == 'Tip Dunk Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = '[player] [dist] Tip Dunk Shot (cnt PTS)'
    df.loc[(df['cod'] == '_1(1a)') & (df['tp_subaction'] == 'Alley Oop Dunk Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = '[player] Alley Oop Dunk (cnt PTS) ([player] cnt AST)'
    df.loc[(df['cod'] == '_1(1a)') & (df['tp_subaction'] == 'Alley Oop Dunk Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = '[player] [dist] Alley Oop Dunk (cnt PTS) ([player] cnt AST)'
    df.loc[(df['cod'] == '_1(1a)') & (df['tp_subaction'] == 'Cutting Dunk Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = '[player] Cutting Dunk Shot (cnt PTS) ([player] cnt AST)'
    df.loc[(df['cod'] == '_1(1a)') & (df['tp_subaction'] == 'Cutting Dunk Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = '[player] [dist] Cutting Dunk Shot (cnt PTS) ([player] cnt AST)'
    df.loc[(df['cod'] == '_1(1a)') & (df['tp_subaction'] == 'Driving Dunk Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = '[player] Driving Dunk (cnt PTS) ([player] cnt AST)'
    df.loc[(df['cod'] == '_1(1a)') & (df['tp_subaction'] == 'Driving Dunk Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = '[player] [dist] Driving Dunk (cnt PTS) ([player] cnt AST)'
    df.loc[(df['cod'] == '_1(1a)') & (df['tp_subaction'] == 'Driving Reverse Dunk Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = '[player] Driving Reverse Dunk Shot (cnt PTS) ([player] cnt AST)'
    df.loc[(df['cod'] == '_1(1a)') & (df['tp_subaction'] == 'Driving Reverse Dunk Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = '[player] [dist] Driving Reverse Dunk Shot (cnt PTS) ([player] cnt AST)'
    df.loc[(df['cod'] == '_1(1a)') & (df['tp_subaction'] == 'Driving Slam Dunk Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = '[player] Driving Slam Dunk (cnt PTS) ([player] cnt AST)'
    df.loc[(df['cod'] == '_1(1a)') & (df['tp_subaction'] == 'Driving Slam Dunk Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = '[player] [dist] Driving Slam Dunk (cnt PTS) ([player] cnt AST)'
    df.loc[(df['cod'] == '_1(1a)') & (df['tp_subaction'] == 'Dunk Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = '[player] Dunk (cnt PTS) ([player] cnt AST)'
    df.loc[(df['cod'] == '_1(1a)') & (df['tp_subaction'] == 'Dunk Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = '[player] [dist] Dunk (cnt PTS) ([player] cnt AST)'
    df.loc[(df['cod'] == '_1(1a)') & (df['tp_subaction'] == 'Putback Dunk Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = '[player] Putback Dunk (cnt PTS) ([player] cnt AST)'
    df.loc[(df['cod'] == '_1(1a)') & (df['tp_subaction'] == 'Putback Dunk Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = '[player] [dist] Putback Dunk (cnt PTS) ([player] cnt AST)'
    df.loc[(df['cod'] == '_1(1a)') & (df['tp_subaction'] == 'Putback Slam Dunk Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = '[player] Putback Slam Dunk (cnt PTS) ([player] cnt AST)'
    df.loc[(df['cod'] == '_1(1a)') & (df['tp_subaction'] == 'Putback Slam Dunk Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = '[player] [dist] Putback Slam Dunk (cnt PTS) ([player] cnt AST)'
    df.loc[(df['cod'] == '_1(1a)') & (df['tp_subaction'] == 'Reverse Dunk Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = '[player] Reverse Dunk (cnt PTS) ([player] cnt AST)'
    df.loc[(df['cod'] == '_1(1a)') & (df['tp_subaction'] == 'Reverse Dunk Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = '[player] [dist] Reverse Dunk (cnt PTS) ([player] cnt AST)'
    df.loc[(df['cod'] == '_1(1a)') & (df['tp_subaction'] == 'Reverse Slam Dunk Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = '[player] Reverse Slam Dunk (cnt PTS) ([player] cnt AST)'
    df.loc[(df['cod'] == '_1(1a)') & (df['tp_subaction'] == 'Reverse Slam Dunk Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = '[player] [dist] Reverse Slam Dunk (cnt PTS) ([player] cnt AST)'
    df.loc[(df['cod'] == '_1(1a)') & (df['tp_subaction'] == 'Running Alley Oop Dunk Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = '[player] Running Alley Oop Dunk Shot (cnt PTS) ([player] cnt AST)'
    df.loc[(df['cod'] == '_1(1a)') & (df['tp_subaction'] == 'Running Alley Oop Dunk Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = '[player] [dist] Running Alley Oop Dunk Shot (cnt PTS) ([player] cnt AST)'
    df.loc[(df['cod'] == '_1(1a)') & (df['tp_subaction'] == 'Running Dunk Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = '[player] Running Dunk (cnt PTS) ([player] cnt AST)'
    df.loc[(df['cod'] == '_1(1a)') & (df['tp_subaction'] == 'Running Dunk Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = '[player] [dist] Running Dunk (cnt PTS) ([player] cnt AST)'
    df.loc[(df['cod'] == '_1(1a)') & (df['tp_subaction'] == 'Running Reverse Dunk Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = '[player] Running Reverse Dunk Shot (cnt PTS) ([player] cnt AST)'
    df.loc[(df['cod'] == '_1(1a)') & (df['tp_subaction'] == 'Running Reverse Dunk Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = '[player] [dist] Running Reverse Dunk Shot (cnt PTS) ([player] cnt AST)'
    df.loc[(df['cod'] == '_1(1a)') & (df['tp_subaction'] == 'Running Slam Dunk Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = '[player] Running Slam Dunk (cnt PTS) ([player] cnt AST)'
    df.loc[(df['cod'] == '_1(1a)') & (df['tp_subaction'] == 'Running Slam Dunk Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = '[player] [dist] Running Slam Dunk (cnt PTS) ([player] cnt AST)'
    df.loc[(df['cod'] == '_1(1a)') & (df['tp_subaction'] == 'Slam Dunk Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = '[player] Slam Dunk (cnt PTS) ([player] cnt AST)'
    df.loc[(df['cod'] == '_1(1a)') & (df['tp_subaction'] == 'Slam Dunk Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = '[player] [dist] Slam Dunk (cnt PTS) ([player] cnt AST)'
    df.loc[(df['cod'] == '_1(1a)') & (df['tp_subaction'] == 'Tip Dunk Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = '[player] Tip Dunk Shot (cnt PTS) ([player] cnt AST)'
    df.loc[(df['cod'] == '_1(1a)') & (df['tp_subaction'] == 'Tip Dunk Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = '[player] [dist] Tip Dunk Shot (cnt PTS) ([player] cnt AST)'
    df.loc[(df['cod'] == '_1(2)') & (df['tp_subaction'] == 'Driving Bank Hook Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = '[player] Driving Bank Hook Shot (cnt PTS)'
    df.loc[(df['cod'] == '_1(2)') & (df['tp_subaction'] == 'Driving Bank Hook Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = '[player] [dist] Driving Bank Hook Shot (cnt PTS)'
    df.loc[(df['cod'] == '_1(2)') & (df['tp_subaction'] == 'Driving Hook Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = '[player] Driving Hook Shot (cnt PTS)'
    df.loc[(df['cod'] == '_1(2)') & (df['tp_subaction'] == 'Driving Hook Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = '[player] [dist] Driving Hook Shot (cnt PTS)'
    df.loc[(df['cod'] == '_1(2)') & (df['tp_subaction'] == 'Hook Bank Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = '[player] Hook Bank Shot (cnt PTS)'
    df.loc[(df['cod'] == '_1(2)') & (df['tp_subaction'] == 'Hook Bank Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = '[player] [dist] Hook Bank Shot (cnt PTS)'
    df.loc[(df['cod'] == '_1(2)') & (df['tp_subaction'] == 'Hook Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = '[player] Hook Shot (cnt PTS)'
    df.loc[(df['cod'] == '_1(2)') & (df['tp_subaction'] == 'Hook Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = '[player] [dist] Hook Shot (cnt PTS)'
    df.loc[(df['cod'] == '_1(2)') & (df['tp_subaction'] == 'Jump Bank Hook Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = '[player] [dist] Jump Bank Hook Shot (cnt PTS)'
    df.loc[(df['cod'] == '_1(2)') & (df['tp_subaction'] == 'Jump Hook Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = '[player] Jump Hook Shot (cnt PTS)'
    df.loc[(df['cod'] == '_1(2)') & (df['tp_subaction'] == 'Jump Hook Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = '[player] [dist] Jump Hook Shot (cnt PTS)'
    df.loc[(df['cod'] == '_1(2)') & (df['tp_subaction'] == 'Running Bank Hook Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = '[player] Running Bank Hook Shot (cnt PTS)'
    df.loc[(df['cod'] == '_1(2)') & (df['tp_subaction'] == 'Running Bank Hook Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = '[player] [dist] Running Bank Hook Shot (cnt PTS)'
    df.loc[(df['cod'] == '_1(2)') & (df['tp_subaction'] == 'Running Hook Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = '[player] Running Hook Shot (cnt PTS)'
    df.loc[(df['cod'] == '_1(2)') & (df['tp_subaction'] == 'Running Hook Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = '[player] [dist] Running Hook Shot (cnt PTS)'
    df.loc[(df['cod'] == '_1(2)') & (df['tp_subaction'] == 'Turnaround Bank Hook Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = '[player] Turnaround Bank Hook Shot (cnt PTS)'
    df.loc[(df['cod'] == '_1(2)') & (df['tp_subaction'] == 'Turnaround Bank Hook Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = '[player] [dist] Turnaround Bank Hook Shot (cnt PTS)'
    df.loc[(df['cod'] == '_1(2)') & (df['tp_subaction'] == 'Turnaround Hook Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = '[player] Turnaround Hook Shot (cnt PTS)'
    df.loc[(df['cod'] == '_1(2)') & (df['tp_subaction'] == 'Turnaround Hook Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = '[player] [dist] Turnaround Hook Shot (cnt PTS)'
    df.loc[(df['cod'] == '_1(2a)') & (df['tp_subaction'] == 'Driving Bank Hook Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = '[player] Driving Bank Hook Shot (cnt PTS) ([player] cnt AST)'
    df.loc[(df['cod'] == '_1(2a)') & (df['tp_subaction'] == 'Driving Bank Hook Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = '[player] [dist] Driving Bank Hook Shot (cnt PTS) ([player] cnt AST)'
    df.loc[(df['cod'] == '_1(2a)') & (df['tp_subaction'] == 'Driving Hook Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = '[player] Driving Hook Shot (cnt PTS) ([player] cnt AST)'
    df.loc[(df['cod'] == '_1(2a)') & (df['tp_subaction'] == 'Driving Hook Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = '[player] [dist] Driving Hook Shot (cnt PTS) ([player] cnt AST)'
    df.loc[(df['cod'] == '_1(2a)') & (df['tp_subaction'] == 'Hook Bank Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = '[player] Hook Bank Shot (cnt PTS) ([player] cnt AST)'
    df.loc[(df['cod'] == '_1(2a)') & (df['tp_subaction'] == 'Hook Bank Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = '[player] [dist] Hook Bank Shot (cnt PTS) ([player] cnt AST)'
    df.loc[(df['cod'] == '_1(2a)') & (df['tp_subaction'] == 'Hook Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = '[player] Hook Shot (cnt PTS) ([player] cnt AST)'
    df.loc[(df['cod'] == '_1(2a)') & (df['tp_subaction'] == 'Hook Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = '[player] [dist] Hook Shot (cnt PTS) ([player] cnt AST)'
    df.loc[(df['cod'] == '_1(2a)') & (df['tp_subaction'] == 'Jump Bank Hook Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = '[player] [dist] Jump Bank Hook Shot (cnt PTS) ([player] cnt AST)'
    df.loc[(df['cod'] == '_1(2a)') & (df['tp_subaction'] == 'Jump Hook Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = '[player] Jump Hook Shot (cnt PTS) ([player] cnt AST)'
    df.loc[(df['cod'] == '_1(2a)') & (df['tp_subaction'] == 'Jump Hook Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = '[player] [dist] Jump Hook Shot (cnt PTS) ([player] cnt AST)'
    df.loc[(df['cod'] == '_1(2a)') & (df['tp_subaction'] == 'Running Bank Hook Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = '[player] [dist] Running Bank Hook Shot (cnt PTS) ([player] cnt AST)'
    df.loc[(df['cod'] == '_1(2a)') & (df['tp_subaction'] == 'Running Hook Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = '[player] Running Hook Shot (cnt PTS) ([player] cnt AST)'
    df.loc[(df['cod'] == '_1(2a)') & (df['tp_subaction'] == 'Running Hook Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = '[player] [dist] Running Hook Shot (cnt PTS) ([player] cnt AST)'
    df.loc[(df['cod'] == '_1(2a)') & (df['tp_subaction'] == 'Turnaround Bank Hook Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = '[player] Turnaround Bank Hook Shot (cnt PTS) ([player] cnt AST)'
    df.loc[(df['cod'] == '_1(2a)') & (df['tp_subaction'] == 'Turnaround Bank Hook Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = '[player] [dist] Turnaround Bank Hook Shot (cnt PTS) ([player] cnt AST)'
    df.loc[(df['cod'] == '_1(2a)') & (df['tp_subaction'] == 'Turnaround Hook Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = '[player] Turnaround Hook Shot (cnt PTS) ([player] cnt AST)'
    df.loc[(df['cod'] == '_1(2a)') & (df['tp_subaction'] == 'Turnaround Hook Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = '[player] [dist] Turnaround Hook Shot (cnt PTS) ([player] cnt AST)'
    df.loc[(df['cod'] == '_1(3)') & (df['tp_subaction'] == 'Alley Oop Layup shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = '[player] Alley Oop Layup (cnt PTS)'
    df.loc[(df['cod'] == '_1(3)') & (df['tp_subaction'] == 'Alley Oop Layup shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = '[player] [dist] Alley Oop Layup (cnt PTS)'
    df.loc[(df['cod'] == '_1(3)') & (df['tp_subaction'] == 'Cutting Finger Roll Layup Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = '[player] Cutting Finger Roll Layup Shot (cnt PTS)'
    df.loc[(df['cod'] == '_1(3)') & (df['tp_subaction'] == 'Cutting Finger Roll Layup Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = '[player] [dist] Cutting Finger Roll Layup Shot (cnt PTS)'
    df.loc[(df['cod'] == '_1(3)') & (df['tp_subaction'] == 'Cutting Layup Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = '[player] Cutting Layup Shot (cnt PTS)'
    df.loc[(df['cod'] == '_1(3)') & (df['tp_subaction'] == 'Cutting Layup Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = '[player] [dist] Cutting Layup Shot (cnt PTS)'
    df.loc[(df['cod'] == '_1(3)') & (df['tp_subaction'] == 'Driving Finger Roll Layup Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = '[player] Driving Finger Roll Layup (cnt PTS)'
    df.loc[(df['cod'] == '_1(3)') & (df['tp_subaction'] == 'Driving Finger Roll Layup Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = '[player] [dist] Driving Finger Roll Layup (cnt PTS)'
    df.loc[(df['cod'] == '_1(3)') & (df['tp_subaction'] == 'Driving Layup Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = '[player] Driving Layup (cnt PTS)'
    df.loc[(df['cod'] == '_1(3)') & (df['tp_subaction'] == 'Driving Layup Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = '[player] [dist] Driving Layup (cnt PTS)'
    df.loc[(df['cod'] == '_1(3)') & (df['tp_subaction'] == 'Driving Reverse Layup Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = '[player] Driving Reverse Layup (cnt PTS)'
    df.loc[(df['cod'] == '_1(3)') & (df['tp_subaction'] == 'Driving Reverse Layup Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = '[player] [dist] Driving Reverse Layup (cnt PTS)'
    df.loc[(df['cod'] == '_1(3)') & (df['tp_subaction'] == 'Finger Roll Layup Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = '[player] Finger Roll Layup (cnt PTS)'
    df.loc[(df['cod'] == '_1(3)') & (df['tp_subaction'] == 'Finger Roll Layup Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = '[player] [dist] Finger Roll Layup (cnt PTS)'
    df.loc[(df['cod'] == '_1(3)') & (df['tp_subaction'] == 'Layup Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = '[player] Layup (cnt PTS)'
    df.loc[(df['cod'] == '_1(3)') & (df['tp_subaction'] == 'Layup Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = '[player] [dist] Layup (cnt PTS)'
    df.loc[(df['cod'] == '_1(3)') & (df['tp_subaction'] == 'Putback Layup Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = '[player] Putback Layup (cnt PTS)'
    df.loc[(df['cod'] == '_1(3)') & (df['tp_subaction'] == 'Putback Layup Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = '[player] [dist] Putback Layup (cnt PTS)'
    df.loc[(df['cod'] == '_1(3)') & (df['tp_subaction'] == 'Reverse Layup Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = '[player] Reverse Layup (cnt PTS)'
    df.loc[(df['cod'] == '_1(3)') & (df['tp_subaction'] == 'Reverse Layup Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = '[player] [dist] Reverse Layup (cnt PTS)'
    df.loc[(df['cod'] == '_1(3)') & (df['tp_subaction'] == 'Running Alley Oop Layup Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = '[player] [dist] Running Alley Oop Layup Shot (cnt PTS)'
    df.loc[(df['cod'] == '_1(3)') & (df['tp_subaction'] == 'Running Finger Roll Layup Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = '[player] Running Finger Roll Layup (cnt PTS)'
    df.loc[(df['cod'] == '_1(3)') & (df['tp_subaction'] == 'Running Finger Roll Layup Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = '[player] [dist] Running Finger Roll Layup (cnt PTS)'
    df.loc[(df['cod'] == '_1(3)') & (df['tp_subaction'] == 'Running Layup Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = '[player] Running Layup (cnt PTS)'
    df.loc[(df['cod'] == '_1(3)') & (df['tp_subaction'] == 'Running Layup Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = '[player] [dist] Running Layup (cnt PTS)'
    df.loc[(df['cod'] == '_1(3)') & (df['tp_subaction'] == 'Running Reverse Layup Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = '[player] Running Reverse Layup (cnt PTS)'
    df.loc[(df['cod'] == '_1(3)') & (df['tp_subaction'] == 'Running Reverse Layup Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = '[player] [dist] Running Reverse Layup (cnt PTS)'
    df.loc[(df['cod'] == '_1(3)') & (df['tp_subaction'] == 'Running Tip Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = '[player] Running Tip Shot (cnt PTS)'
    df.loc[(df['cod'] == '_1(3)') & (df['tp_subaction'] == 'Running Tip Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = '[player] [dist] Running Tip Shot (cnt PTS)'
    df.loc[(df['cod'] == '_1(3)') & (df['tp_subaction'] == 'Tip Layup Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = '[player] Tip Layup Shot (cnt PTS)'
    df.loc[(df['cod'] == '_1(3)') & (df['tp_subaction'] == 'Tip Layup Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = '[player] [dist] Tip Layup Shot (cnt PTS)'
    df.loc[(df['cod'] == '_1(3)') & (df['tp_subaction'] == 'Tip Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = '[player] Tip Shot (cnt PTS)'
    df.loc[(df['cod'] == '_1(3)') & (df['tp_subaction'] == 'Tip Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = '[player] [dist] Tip Shot (cnt PTS)'
    df.loc[(df['cod'] == '_1(3a)') & (df['tp_subaction'] == 'Alley Oop Layup shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = '[player] Alley Oop Layup (cnt PTS) ([player] cnt AST)'
    df.loc[(df['cod'] == '_1(3a)') & (df['tp_subaction'] == 'Alley Oop Layup shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = '[player] [dist] Alley Oop Layup (cnt PTS) ([player] cnt AST)'
    df.loc[(df['cod'] == '_1(3a)') & (df['tp_subaction'] == 'Cutting Finger Roll Layup Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = '[player] Cutting Finger Roll Layup Shot (cnt PTS) ([player] cnt AST)'
    df.loc[(df['cod'] == '_1(3a)') & (df['tp_subaction'] == 'Cutting Finger Roll Layup Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = '[player] [dist] Cutting Finger Roll Layup Shot (cnt PTS) ([player] cnt AST)'
    df.loc[(df['cod'] == '_1(3a)') & (df['tp_subaction'] == 'Cutting Layup Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = '[player] Cutting Layup Shot (cnt PTS) ([player] cnt AST)'
    df.loc[(df['cod'] == '_1(3a)') & (df['tp_subaction'] == 'Cutting Layup Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = '[player] [dist] Cutting Layup Shot (cnt PTS) ([player] cnt AST)'
    df.loc[(df['cod'] == '_1(3a)') & (df['tp_subaction'] == 'Driving Finger Roll Layup Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = '[player] Driving Finger Roll Layup (cnt PTS) ([player] cnt AST)'
    df.loc[(df['cod'] == '_1(3a)') & (df['tp_subaction'] == 'Driving Finger Roll Layup Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = '[player] [dist] Driving Finger Roll Layup (cnt PTS) ([player] cnt AST)'
    df.loc[(df['cod'] == '_1(3a)') & (df['tp_subaction'] == 'Driving Layup Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = '[player] Driving Layup (cnt PTS) ([player] cnt AST)'
    df.loc[(df['cod'] == '_1(3a)') & (df['tp_subaction'] == 'Driving Layup Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = '[player] [dist] Driving Layup (cnt PTS) ([player] cnt AST)'
    df.loc[(df['cod'] == '_1(3a)') & (df['tp_subaction'] == 'Driving Reverse Layup Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = '[player] Driving Reverse Layup (cnt PTS) ([player] cnt AST)'
    df.loc[(df['cod'] == '_1(3a)') & (df['tp_subaction'] == 'Driving Reverse Layup Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = '[player] [dist] Driving Reverse Layup (cnt PTS) ([player] cnt AST)'
    df.loc[(df['cod'] == '_1(3a)') & (df['tp_subaction'] == 'Finger Roll Layup Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = '[player] Finger Roll Layup (cnt PTS) ([player] cnt AST)'
    df.loc[(df['cod'] == '_1(3a)') & (df['tp_subaction'] == 'Finger Roll Layup Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = '[player] [dist] Finger Roll Layup (cnt PTS) ([player] cnt AST)'
    df.loc[(df['cod'] == '_1(3a)') & (df['tp_subaction'] == 'Layup Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = '[player] Layup (cnt PTS) ([player] cnt AST)'
    df.loc[(df['cod'] == '_1(3a)') & (df['tp_subaction'] == 'Layup Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = '[player] [dist] Layup (cnt PTS) ([player] cnt AST)'
    df.loc[(df['cod'] == '_1(3a)') & (df['tp_subaction'] == 'Putback Layup Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = '[player] Putback Layup (cnt PTS) ([player] cnt AST)'
    df.loc[(df['cod'] == '_1(3a)') & (df['tp_subaction'] == 'Putback Layup Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = '[player] [dist] Putback Layup (cnt PTS) ([player] cnt AST)'
    df.loc[(df['cod'] == '_1(3a)') & (df['tp_subaction'] == 'Reverse Layup Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = '[player] Reverse Layup (cnt PTS) ([player] cnt AST)'
    df.loc[(df['cod'] == '_1(3a)') & (df['tp_subaction'] == 'Reverse Layup Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = '[player] [dist] Reverse Layup (cnt PTS) ([player] cnt AST)'
    df.loc[(df['cod'] == '_1(3a)') & (df['tp_subaction'] == 'Running Alley Oop Layup Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = '[player] [dist] Running Alley Oop Layup Shot (cnt PTS) ([player] cnt AST)'
    df.loc[(df['cod'] == '_1(3a)') & (df['tp_subaction'] == 'Running Alley Oop Layup Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = '[player] Running Alley Oop Layup Shot (cnt PTS) ([player] cnt AST)'
    df.loc[(df['cod'] == '_1(3a)') & (df['tp_subaction'] == 'Running Finger Roll Layup Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = '[player] Running Finger Roll Layup (cnt PTS) ([player] cnt AST)'
    df.loc[(df['cod'] == '_1(3a)') & (df['tp_subaction'] == 'Running Finger Roll Layup Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = '[player] [dist] Running Finger Roll Layup (cnt PTS) ([player] cnt AST)'
    df.loc[(df['cod'] == '_1(3a)') & (df['tp_subaction'] == 'Running Layup Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = '[player] Running Layup (cnt PTS) ([player] cnt AST)'
    df.loc[(df['cod'] == '_1(3a)') & (df['tp_subaction'] == 'Running Layup Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = '[player] [dist] Running Layup (cnt PTS) ([player] cnt AST)'
    df.loc[(df['cod'] == '_1(3a)') & (df['tp_subaction'] == 'Running Reverse Layup Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = '[player] Running Reverse Layup (cnt PTS) ([player] cnt AST)'
    df.loc[(df['cod'] == '_1(3a)') & (df['tp_subaction'] == 'Running Reverse Layup Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = '[player] [dist] Running Reverse Layup (cnt PTS) ([player] cnt AST)'
    df.loc[(df['cod'] == '_1(3a)') & (df['tp_subaction'] == 'Running Tip Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = '[player] [dist] Running Tip Shot (cnt PTS) ([player] cnt AST)'
    df.loc[(df['cod'] == '_1(3a)') & (df['tp_subaction'] == 'Tip Layup Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = '[player] [dist] Tip Layup Shot (cnt PTS) ([player] cnt AST)'
    df.loc[(df['cod'] == '_1(3a)') & (df['tp_subaction'] == 'Tip Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = '[player] Tip Shot (cnt PTS) ([player] cnt AST)'
    df.loc[(df['cod'] == '_1(3a)') & (df['tp_subaction'] == 'Tip Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = '[player] [dist] Tip Shot (cnt PTS) ([player] cnt AST)'
    df.loc[(df['cod'] == '_1(4)') & (df['tp_subaction'] == '') & (df['ind_dist'] == 0) & (df['ind_no'] == 1), 'gram'] = '[player] No Shot (cnt PTS)'
    df.loc[(df['cod'] == '_1(4)') & (df['tp_subaction'] == '') & (df['ind_dist'] == 1) & (df['ind_no'] == 1), 'gram'] = '[player] [dist] No Shot (cnt PTS)'
    df.loc[(df['cod'] == '_1(4)') & (df['tp_subaction'] == '') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = '[player] Shot (cnt PTS)'
    df.loc[(df['cod'] == '_1(4)') & (df['tp_subaction'] == '') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = '[player] [dist] Shot (cnt PTS)'
    df.loc[(df['cod'] == '_1(4)') & (df['tp_subaction'] == 'Driving Bank shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = '[player] [dist] Driving Bank Shot (cnt PTS)'
    df.loc[(df['cod'] == '_1(4)') & (df['tp_subaction'] == 'Driving Floating Bank Jump Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = '[player] Driving Floating Bank Jump Shot (cnt PTS)'
    df.loc[(df['cod'] == '_1(4)') & (df['tp_subaction'] == 'Driving Floating Bank Jump Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = '[player] [dist] Driving Floating Bank Jump Shot (cnt PTS)'
    df.loc[(df['cod'] == '_1(4)') & (df['tp_subaction'] == 'Driving Floating Jump Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = '[player] Driving Floating Jump Shot (cnt PTS)'
    df.loc[(df['cod'] == '_1(4)') & (df['tp_subaction'] == 'Driving Floating Jump Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = '[player] [dist] Driving Floating Jump Shot (cnt PTS)'
    df.loc[(df['cod'] == '_1(4)') & (df['tp_subaction'] == 'Driving Jump shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = '[player] Driving Jump Shot (cnt PTS)'
    df.loc[(df['cod'] == '_1(4)') & (df['tp_subaction'] == 'Driving Jump shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = '[player] [dist] Driving Jump Shot (cnt PTS)'
    df.loc[(df['cod'] == '_1(4)') & (df['tp_subaction'] == 'Fadeaway Bank shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = '[player] Fadeaway Bank Shot (cnt PTS)'
    df.loc[(df['cod'] == '_1(4)') & (df['tp_subaction'] == 'Fadeaway Bank shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = '[player] [dist] Fadeaway Bank Shot (cnt PTS)'
    df.loc[(df['cod'] == '_1(4)') & (df['tp_subaction'] == 'Fadeaway Jump Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = '[player] Fadeaway Jumper (cnt PTS)'
    df.loc[(df['cod'] == '_1(4)') & (df['tp_subaction'] == 'Fadeaway Jump Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = '[player] [dist] Fadeaway Jumper (cnt PTS)'
    df.loc[(df['cod'] == '_1(4)') & (df['tp_subaction'] == 'Floating Jump shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = '[player] Floating Jump Shot (cnt PTS)'
    df.loc[(df['cod'] == '_1(4)') & (df['tp_subaction'] == 'Floating Jump shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = '[player] [dist] Floating Jump Shot (cnt PTS)'
    df.loc[(df['cod'] == '_1(4)') & (df['tp_subaction'] == 'Jump Bank Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = '[player] Jump Bank Shot (cnt PTS)'
    df.loc[(df['cod'] == '_1(4)') & (df['tp_subaction'] == 'Jump Bank Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = '[player] [dist] Jump Bank Shot (cnt PTS)'
    df.loc[(df['cod'] == '_1(4)') & (df['tp_subaction'] == 'Jump Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = '[player] Jump Shot (cnt PTS)'
    df.loc[(df['cod'] == '_1(4)') & (df['tp_subaction'] == 'Jump Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = '[player] [dist] Jump Shot (cnt PTS)'
    df.loc[(df['cod'] == '_1(4)') & (df['tp_subaction'] == 'Pullup Bank shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = '[player] [dist] Pullup Bank Shot (cnt PTS)'
    df.loc[(df['cod'] == '_1(4)') & (df['tp_subaction'] == 'Pullup Jump shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = '[player] Pullup Jump Shot (cnt PTS)'
    df.loc[(df['cod'] == '_1(4)') & (df['tp_subaction'] == 'Pullup Jump shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = '[player] [dist] Pullup Jump Shot (cnt PTS)'
    df.loc[(df['cod'] == '_1(4)') & (df['tp_subaction'] == 'Running Bank shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = '[player] Running Bank Shot (cnt PTS)'
    df.loc[(df['cod'] == '_1(4)') & (df['tp_subaction'] == 'Running Bank shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = '[player] [dist] Running Bank Shot (cnt PTS)'
    df.loc[(df['cod'] == '_1(4)') & (df['tp_subaction'] == 'Running Jump Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = '[player] Running Jump Shot (cnt PTS)'
    df.loc[(df['cod'] == '_1(4)') & (df['tp_subaction'] == 'Running Jump Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = '[player] [dist] Running Jump Shot (cnt PTS)'
    df.loc[(df['cod'] == '_1(4)') & (df['tp_subaction'] == 'Running Pull-Up Jump Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = '[player] [dist] Running Pull-Up Jump Shot (cnt PTS)'
    df.loc[(df['cod'] == '_1(4)') & (df['tp_subaction'] == 'Step Back Bank Jump Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = '[player] [dist] Step Back Bank Jump Shot (cnt PTS)'
    df.loc[(df['cod'] == '_1(4)') & (df['tp_subaction'] == 'Step Back Jump shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = '[player] Step Back Jump Shot (cnt PTS)'
    df.loc[(df['cod'] == '_1(4)') & (df['tp_subaction'] == 'Step Back Jump shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = '[player] [dist] Step Back Jump Shot (cnt PTS)'
    df.loc[(df['cod'] == '_1(4)') & (df['tp_subaction'] == 'Turnaround Bank shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = '[player] Turnaround Bank Shot (cnt PTS)'
    df.loc[(df['cod'] == '_1(4)') & (df['tp_subaction'] == 'Turnaround Bank shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = '[player] [dist] Turnaround Bank Shot (cnt PTS)'
    df.loc[(df['cod'] == '_1(4)') & (df['tp_subaction'] == 'Turnaround Fadeaway Bank Jump Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = '[player] [dist] Turnaround Fadeaway Bank Jump Shot (cnt PTS)'
    df.loc[(df['cod'] == '_1(4)') & (df['tp_subaction'] == 'Turnaround Fadeaway shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = '[player] Turnaround Fadeaway (cnt PTS)'
    df.loc[(df['cod'] == '_1(4)') & (df['tp_subaction'] == 'Turnaround Fadeaway shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = '[player] [dist] Turnaround Fadeaway (cnt PTS)'
    df.loc[(df['cod'] == '_1(4)') & (df['tp_subaction'] == 'Turnaround Jump Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = '[player] Turnaround Jump Shot (cnt PTS)'
    df.loc[(df['cod'] == '_1(4)') & (df['tp_subaction'] == 'Turnaround Jump Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = '[player] [dist] Turnaround Jump Shot (cnt PTS)'
    df.loc[(df['cod'] == '_1(4a)') & (df['tp_subaction'] == '') & (df['ind_dist'] == 0) & (df['ind_no'] == 1), 'gram'] = '[player] No Shot (cnt PTS) ([player] cnt AST)'
    df.loc[(df['cod'] == '_1(4a)') & (df['tp_subaction'] == '') & (df['ind_dist'] == 1) & (df['ind_no'] == 1), 'gram'] = '[player] [dist] No Shot (cnt PTS) ([player] cnt AST)'
    df.loc[(df['cod'] == '_1(4a)') & (df['tp_subaction'] == '') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = '[player] Shot (cnt PTS) ([player] cnt AST)'
    df.loc[(df['cod'] == '_1(4a)') & (df['tp_subaction'] == '') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = '[player] [dist] Shot (cnt PTS) ([player] cnt AST)'
    df.loc[(df['cod'] == '_1(4a)') & (df['tp_subaction'] == 'Driving Bank shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = '[player] Driving Bank Shot (cnt PTS) ([player] cnt AST)'
    df.loc[(df['cod'] == '_1(4a)') & (df['tp_subaction'] == 'Driving Bank shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = '[player] [dist] Driving Bank Shot (cnt PTS) ([player] cnt AST)'
    df.loc[(df['cod'] == '_1(4a)') & (df['tp_subaction'] == 'Driving Floating Bank Jump Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = '[player] Driving Floating Bank Jump Shot (cnt PTS) ([player] cnt AST)'
    df.loc[(df['cod'] == '_1(4a)') & (df['tp_subaction'] == 'Driving Floating Bank Jump Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = '[player] [dist] Driving Floating Bank Jump Shot (cnt PTS) ([player] cnt AST)'
    df.loc[(df['cod'] == '_1(4a)') & (df['tp_subaction'] == 'Driving Floating Jump Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = '[player] Driving Floating Jump Shot (cnt PTS) ([player] cnt AST)'
    df.loc[(df['cod'] == '_1(4a)') & (df['tp_subaction'] == 'Driving Floating Jump Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = '[player] [dist] Driving Floating Jump Shot (cnt PTS) ([player] cnt AST)'
    df.loc[(df['cod'] == '_1(4a)') & (df['tp_subaction'] == 'Driving Jump shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = '[player] [dist] Driving Jump Shot (cnt PTS) ([player] cnt AST)'
    df.loc[(df['cod'] == '_1(4a)') & (df['tp_subaction'] == 'Fadeaway Bank shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = '[player] [dist] Fadeaway Bank Shot (cnt PTS) ([player] cnt AST)'
    df.loc[(df['cod'] == '_1(4a)') & (df['tp_subaction'] == 'Fadeaway Jump Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = '[player] [dist] Fadeaway Jumper (cnt PTS) ([player] cnt AST)'
    df.loc[(df['cod'] == '_1(4a)') & (df['tp_subaction'] == 'Floating Jump shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = '[player] Floating Jump Shot (cnt PTS) ([player] cnt AST)'
    df.loc[(df['cod'] == '_1(4a)') & (df['tp_subaction'] == 'Floating Jump shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = '[player] [dist] Floating Jump Shot (cnt PTS) ([player] cnt AST)'
    df.loc[(df['cod'] == '_1(4a)') & (df['tp_subaction'] == 'Jump Bank Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = '[player] [dist] Jump Bank Shot (cnt PTS) ([player] cnt AST)'
    df.loc[(df['cod'] == '_1(4a)') & (df['tp_subaction'] == 'Jump Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = '[player] Jump Shot (cnt PTS) ([player] cnt AST)'
    df.loc[(df['cod'] == '_1(4a)') & (df['tp_subaction'] == 'Jump Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = '[player] [dist] Jump Shot (cnt PTS) ([player] cnt AST)'
    df.loc[(df['cod'] == '_1(4a)') & (df['tp_subaction'] == 'Pullup Bank shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = '[player] [dist] Pullup Bank Shot (cnt PTS) ([player] cnt AST)'
    df.loc[(df['cod'] == '_1(4a)') & (df['tp_subaction'] == 'Pullup Jump shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = '[player] [dist] Pullup Jump Shot (cnt PTS) ([player] cnt AST)'
    df.loc[(df['cod'] == '_1(4a)') & (df['tp_subaction'] == 'Running Bank shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = '[player] [dist] Running Bank Shot (cnt PTS) ([player] cnt AST)'
    df.loc[(df['cod'] == '_1(4a)') & (df['tp_subaction'] == 'Running Jump Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = '[player] Running Jump Shot (cnt PTS) ([player] cnt AST)'
    df.loc[(df['cod'] == '_1(4a)') & (df['tp_subaction'] == 'Running Jump Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = '[player] [dist] Running Jump Shot (cnt PTS) ([player] cnt AST)'
    df.loc[(df['cod'] == '_1(4a)') & (df['tp_subaction'] == 'Running Pull-Up Jump Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = '[player] Running Pull-Up Jump Shot (cnt PTS) ([player] cnt AST)'
    df.loc[(df['cod'] == '_1(4a)') & (df['tp_subaction'] == 'Running Pull-Up Jump Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = '[player] [dist] Running Pull-Up Jump Shot (cnt PTS) ([player] cnt AST)'
    df.loc[(df['cod'] == '_1(4a)') & (df['tp_subaction'] == 'Step Back Bank Jump Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = '[player] [dist] Step Back Bank Jump Shot (cnt PTS) ([player] cnt AST)'
    df.loc[(df['cod'] == '_1(4a)') & (df['tp_subaction'] == 'Step Back Jump shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = '[player] [dist] Step Back Jump Shot (cnt PTS) ([player] cnt AST)'
    df.loc[(df['cod'] == '_1(4a)') & (df['tp_subaction'] == 'Turnaround Bank shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = '[player] [dist] Turnaround Bank Shot (cnt PTS) ([player] cnt AST)'
    df.loc[(df['cod'] == '_1(4a)') & (df['tp_subaction'] == 'Turnaround Fadeaway Bank Jump Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = '[player] [dist] Turnaround Fadeaway Bank Jump Shot (cnt PTS) ([player] cnt AST)'
    df.loc[(df['cod'] == '_1(4a)') & (df['tp_subaction'] == 'Turnaround Fadeaway shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = '[player] [dist] Turnaround Fadeaway (cnt PTS) ([player] cnt AST)'
    df.loc[(df['cod'] == '_1(4a)') & (df['tp_subaction'] == 'Turnaround Jump Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = '[player] Turnaround Jump Shot (cnt PTS) ([player] cnt AST)'
    df.loc[(df['cod'] == '_1(4a)') & (df['tp_subaction'] == 'Turnaround Jump Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = '[player] [dist] Turnaround Jump Shot (cnt PTS) ([player] cnt AST)'
    df.loc[(df['cod'] == '_1(5)') & (df['tp_subaction'] == '') & (df['ind_dist'] == 1) & (df['ind_no'] == 1), 'gram'] = '[player] [dist] 3PT No Shot (cnt PTS)'
    df.loc[(df['cod'] == '_1(5)') & (df['tp_subaction'] == 'Driving Bank shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = '[player] [dist] 3PT Driving Bank Shot (cnt PTS)'
    df.loc[(df['cod'] == '_1(5)') & (df['tp_subaction'] == 'Driving Floating Bank Jump Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = '[player] [dist] 3PT Driving Floating Bank Jump Shot (cnt PTS)'
    df.loc[(df['cod'] == '_1(5)') & (df['tp_subaction'] == 'Driving Floating Jump Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = '[player] [dist] 3PT Driving Floating Jump Shot (cnt PTS)'
    df.loc[(df['cod'] == '_1(5)') & (df['tp_subaction'] == 'Driving Jump shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = '[player] [dist] 3PT Driving Jump Shot (cnt PTS)'
    df.loc[(df['cod'] == '_1(5)') & (df['tp_subaction'] == 'Fadeaway Bank shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = '[player] [dist] 3PT Fadeaway Bank Shot (cnt PTS)'
    df.loc[(df['cod'] == '_1(5)') & (df['tp_subaction'] == 'Fadeaway Jump Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = '[player] 3PT Fadeaway Jumper (cnt PTS)'
    df.loc[(df['cod'] == '_1(5)') & (df['tp_subaction'] == 'Fadeaway Jump Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = '[player] [dist] 3PT Fadeaway Jumper (cnt PTS)'
    df.loc[(df['cod'] == '_1(5)') & (df['tp_subaction'] == 'Floating Jump shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = '[player] 3PT Floating Jump Shot (cnt PTS)'
    df.loc[(df['cod'] == '_1(5)') & (df['tp_subaction'] == 'Floating Jump shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = '[player] [dist] 3PT Floating Jump Shot (cnt PTS)'
    df.loc[(df['cod'] == '_1(5)') & (df['tp_subaction'] == 'Jump Bank Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = '[player] 3PT Jump Bank Shot (cnt PTS)'
    df.loc[(df['cod'] == '_1(5)') & (df['tp_subaction'] == 'Jump Bank Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = '[player] [dist] 3PT Jump Bank Shot (cnt PTS)'
    df.loc[(df['cod'] == '_1(5)') & (df['tp_subaction'] == 'Jump Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = '[player] 3PT Jump Shot (cnt PTS)'
    df.loc[(df['cod'] == '_1(5)') & (df['tp_subaction'] == 'Jump Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = '[player] [dist] 3PT Jump Shot (cnt PTS)'
    df.loc[(df['cod'] == '_1(5)') & (df['tp_subaction'] == 'Pullup Bank shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = '[player] [dist] 3PT Pullup Bank Shot (cnt PTS)'
    df.loc[(df['cod'] == '_1(5)') & (df['tp_subaction'] == 'Pullup Jump shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = '[player] 3PT Pullup Jump Shot (cnt PTS)'
    df.loc[(df['cod'] == '_1(5)') & (df['tp_subaction'] == 'Pullup Jump shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = '[player] [dist] 3PT Pullup Jump Shot (cnt PTS)'
    df.loc[(df['cod'] == '_1(5)') & (df['tp_subaction'] == 'Putback Layup Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = '[player] [dist] 3PT Putback Layup (cnt PTS)'
    df.loc[(df['cod'] == '_1(5)') & (df['tp_subaction'] == 'Running Bank shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = '[player] [dist] 3PT Running Bank Shot (cnt PTS)'
    df.loc[(df['cod'] == '_1(5)') & (df['tp_subaction'] == 'Running Jump Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = '[player] 3PT Running Jump Shot (cnt PTS)'
    df.loc[(df['cod'] == '_1(5)') & (df['tp_subaction'] == 'Running Jump Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = '[player] [dist] 3PT Running Jump Shot (cnt PTS)'
    df.loc[(df['cod'] == '_1(5)') & (df['tp_subaction'] == 'Running Pull-Up Jump Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = '[player] 3PT Running Pull-Up Jump Shot (cnt PTS)'
    df.loc[(df['cod'] == '_1(5)') & (df['tp_subaction'] == 'Running Pull-Up Jump Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = '[player] [dist] 3PT Running Pull-Up Jump Shot (cnt PTS)'
    df.loc[(df['cod'] == '_1(5)') & (df['tp_subaction'] == 'Step Back Bank Jump Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = '[player] [dist] 3PT Step Back Bank Jump Shot (cnt PTS)'
    df.loc[(df['cod'] == '_1(5)') & (df['tp_subaction'] == 'Step Back Jump shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = '[player] 3PT Step Back Jump Shot (cnt PTS)'
    df.loc[(df['cod'] == '_1(5)') & (df['tp_subaction'] == 'Step Back Jump shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = '[player] [dist] 3PT Step Back Jump Shot (cnt PTS)'
    df.loc[(df['cod'] == '_1(5)') & (df['tp_subaction'] == 'Turnaround Bank shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = '[player] [dist] 3PT Turnaround Bank Shot (cnt PTS)'
    df.loc[(df['cod'] == '_1(5)') & (df['tp_subaction'] == 'Turnaround Fadeaway Bank Jump Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = '[player] [dist] 3PT Turnaround Fadeaway Bank Jump Shot (cnt PTS)'
    df.loc[(df['cod'] == '_1(5)') & (df['tp_subaction'] == 'Turnaround Fadeaway shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = '[player] 3PT Turnaround Fadeaway (cnt PTS)'
    df.loc[(df['cod'] == '_1(5)') & (df['tp_subaction'] == 'Turnaround Fadeaway shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = '[player] [dist] 3PT Turnaround Fadeaway (cnt PTS)'
    df.loc[(df['cod'] == '_1(5)') & (df['tp_subaction'] == 'Turnaround Jump Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = '[player] 3PT Turnaround Jump Shot (cnt PTS)'
    df.loc[(df['cod'] == '_1(5)') & (df['tp_subaction'] == 'Turnaround Jump Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = '[player] [dist] 3PT Turnaround Jump Shot (cnt PTS)'
    df.loc[(df['cod'] == '_1(5a)') & (df['tp_subaction'] == '') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = '[player] [dist] 3PT Shot (cnt PTS) ([player] cnt AST)'
    df.loc[(df['cod'] == '_1(5a)') & (df['tp_subaction'] == 'Driving Floating Bank Jump Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = '[player] [dist] 3PT Driving Floating Bank Jump Shot (cnt PTS) ([player] cnt AST)'
    df.loc[(df['cod'] == '_1(5a)') & (df['tp_subaction'] == 'Driving Floating Jump Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = '[player] 3PT Driving Floating Jump Shot (cnt PTS) ([player] cnt AST)'
    df.loc[(df['cod'] == '_1(5a)') & (df['tp_subaction'] == 'Driving Floating Jump Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = '[player] [dist] 3PT Driving Floating Jump Shot (cnt PTS) ([player] cnt AST)'
    df.loc[(df['cod'] == '_1(5a)') & (df['tp_subaction'] == 'Driving Jump shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = '[player] [dist] 3PT Driving Jump Shot (cnt PTS) ([player] cnt AST)'
    df.loc[(df['cod'] == '_1(5a)') & (df['tp_subaction'] == 'Fadeaway Bank shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = '[player] [dist] 3PT Fadeaway Bank Shot (cnt PTS) ([player] cnt AST)'
    df.loc[(df['cod'] == '_1(5a)') & (df['tp_subaction'] == 'Fadeaway Jump Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = '[player] 3PT Fadeaway Jumper (cnt PTS) ([player] cnt AST)'
    df.loc[(df['cod'] == '_1(5a)') & (df['tp_subaction'] == 'Fadeaway Jump Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = '[player] [dist] 3PT Fadeaway Jumper (cnt PTS) ([player] cnt AST)'
    df.loc[(df['cod'] == '_1(5a)') & (df['tp_subaction'] == 'Floating Jump shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = '[player] 3PT Floating Jump Shot (cnt PTS) ([player] cnt AST)'
    df.loc[(df['cod'] == '_1(5a)') & (df['tp_subaction'] == 'Floating Jump shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = '[player] [dist] 3PT Floating Jump Shot (cnt PTS) ([player] cnt AST)'
    df.loc[(df['cod'] == '_1(5a)') & (df['tp_subaction'] == 'Jump Bank Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = '[player] [dist] 3PT Jump Bank Shot (cnt PTS) ([player] cnt AST)'
    df.loc[(df['cod'] == '_1(5a)') & (df['tp_subaction'] == 'Jump Bank Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = '[player] 3PT Jump Bank Shot (cnt PTS) ([player] cnt AST)'
    df.loc[(df['cod'] == '_1(5a)') & (df['tp_subaction'] == 'Jump Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = '[player] 3PT Jump Shot (cnt PTS) ([player] cnt AST)'
    df.loc[(df['cod'] == '_1(5a)') & (df['tp_subaction'] == 'Jump Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = '[player] [dist] 3PT Jump Shot (cnt PTS) ([player] cnt AST)'
    df.loc[(df['cod'] == '_1(5a)') & (df['tp_subaction'] == 'Pullup Bank shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = '[player] [dist] 3PT Pullup Bank Shot (cnt PTS) ([player] cnt AST)'
    df.loc[(df['cod'] == '_1(5a)') & (df['tp_subaction'] == 'Pullup Jump shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = '[player] 3PT Pullup Jump Shot (cnt PTS) ([player] cnt AST)'
    df.loc[(df['cod'] == '_1(5a)') & (df['tp_subaction'] == 'Pullup Jump shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = '[player] [dist] 3PT Pullup Jump Shot (cnt PTS) ([player] cnt AST)'
    df.loc[(df['cod'] == '_1(5a)') & (df['tp_subaction'] == 'Running Bank Hook Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = '[player] [dist] 3PT Running Bank Hook Shot (cnt PTS) ([player] cnt AST)'
    df.loc[(df['cod'] == '_1(5a)') & (df['tp_subaction'] == 'Running Bank shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = '[player] [dist] 3PT Running Bank Shot (cnt PTS) ([player] cnt AST)'
    df.loc[(df['cod'] == '_1(5a)') & (df['tp_subaction'] == 'Running Jump Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = '[player] 3PT Running Jump Shot (cnt PTS) ([player] cnt AST)'
    df.loc[(df['cod'] == '_1(5a)') & (df['tp_subaction'] == 'Running Jump Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = '[player] [dist] 3PT Running Jump Shot (cnt PTS) ([player] cnt AST)'
    df.loc[(df['cod'] == '_1(5a)') & (df['tp_subaction'] == 'Running Pull-Up Jump Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = '[player] 3PT Running Pull-Up Jump Shot (cnt PTS) ([player] cnt AST)'
    df.loc[(df['cod'] == '_1(5a)') & (df['tp_subaction'] == 'Running Pull-Up Jump Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = '[player] [dist] 3PT Running Pull-Up Jump Shot (cnt PTS) ([player] cnt AST)'
    df.loc[(df['cod'] == '_1(5a)') & (df['tp_subaction'] == 'Step Back Bank Jump Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = '[player] 3PT Step Back Bank Jump Shot (cnt PTS) ([player] cnt AST)'
    df.loc[(df['cod'] == '_1(5a)') & (df['tp_subaction'] == 'Step Back Bank Jump Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = '[player] [dist] 3PT Step Back Bank Jump Shot (cnt PTS) ([player] cnt AST)'
    df.loc[(df['cod'] == '_1(5a)') & (df['tp_subaction'] == 'Step Back Jump shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = '[player] 3PT Step Back Jump Shot (cnt PTS) ([player] cnt AST)'
    df.loc[(df['cod'] == '_1(5a)') & (df['tp_subaction'] == 'Step Back Jump shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = '[player] [dist] 3PT Step Back Jump Shot (cnt PTS) ([player] cnt AST)'
    df.loc[(df['cod'] == '_1(5a)') & (df['tp_subaction'] == 'Turnaround Bank Hook Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = '[player] [dist] 3PT Turnaround Bank Hook Shot (cnt PTS) ([player] cnt AST)'
    df.loc[(df['cod'] == '_1(5a)') & (df['tp_subaction'] == 'Turnaround Bank shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = '[player] [dist] 3PT Turnaround Bank Shot (cnt PTS) ([player] cnt AST)'
    df.loc[(df['cod'] == '_1(5a)') & (df['tp_subaction'] == 'Turnaround Fadeaway Bank Jump Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = '[player] [dist] 3PT Turnaround Fadeaway Bank Jump Shot (cnt PTS) ([player] cnt AST)'
    df.loc[(df['cod'] == '_1(5a)') & (df['tp_subaction'] == 'Turnaround Fadeaway shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = '[player] 3PT Turnaround Fadeaway (cnt PTS) ([player] cnt AST)'
    df.loc[(df['cod'] == '_1(5a)') & (df['tp_subaction'] == 'Turnaround Fadeaway shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = '[player] [dist] 3PT Turnaround Fadeaway (cnt PTS) ([player] cnt AST)'
    df.loc[(df['cod'] == '_1(5a)') & (df['tp_subaction'] == 'Turnaround Jump Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = '[player] 3PT Turnaround Jump Shot (cnt PTS) ([player] cnt AST)'
    df.loc[(df['cod'] == '_1(5a)') & (df['tp_subaction'] == 'Turnaround Jump Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = '[player] [dist] 3PT Turnaround Jump Shot (cnt PTS) ([player] cnt AST)'

    df.loc[(df['cod'] == '_2(1)') & (df['tp_subaction'] == 'Alley Oop Dunk Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] Alley Oop Dunk'
    df.loc[(df['cod'] == '_2(1)') & (df['tp_subaction'] == 'Alley Oop Dunk Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] Alley Oop Dunk'
    df.loc[(df['cod'] == '_2(1)') & (df['tp_subaction'] == 'Cutting Dunk Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] Cutting Dunk Shot'
    df.loc[(df['cod'] == '_2(1)') & (df['tp_subaction'] == 'Cutting Dunk Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] Cutting Dunk Shot'
    df.loc[(df['cod'] == '_2(1)') & (df['tp_subaction'] == 'Driving Dunk Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] Driving Dunk'
    df.loc[(df['cod'] == '_2(1)') & (df['tp_subaction'] == 'Driving Dunk Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] Driving Dunk'
    df.loc[(df['cod'] == '_2(1)') & (df['tp_subaction'] == 'Driving Reverse Dunk Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] Driving Reverse Dunk Shot'
    df.loc[(df['cod'] == '_2(1)') & (df['tp_subaction'] == 'Driving Reverse Dunk Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] Driving Reverse Dunk Shot'
    df.loc[(df['cod'] == '_2(1)') & (df['tp_subaction'] == 'Driving Slam Dunk Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] Driving Slam Dunk'
    df.loc[(df['cod'] == '_2(1)') & (df['tp_subaction'] == 'Driving Slam Dunk Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] Driving Slam Dunk'
    df.loc[(df['cod'] == '_2(1)') & (df['tp_subaction'] == 'Dunk Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] Dunk'
    df.loc[(df['cod'] == '_2(1)') & (df['tp_subaction'] == 'Dunk Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] Dunk'
    df.loc[(df['cod'] == '_2(1)') & (df['tp_subaction'] == 'Putback Dunk Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] Putback Dunk'
    df.loc[(df['cod'] == '_2(1)') & (df['tp_subaction'] == 'Putback Dunk Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] Putback Dunk'
    df.loc[(df['cod'] == '_2(1)') & (df['tp_subaction'] == 'Putback Slam Dunk Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] Putback Slam Dunk'
    df.loc[(df['cod'] == '_2(1)') & (df['tp_subaction'] == 'Putback Slam Dunk Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] Putback Slam Dunk'
    df.loc[(df['cod'] == '_2(1)') & (df['tp_subaction'] == 'Reverse Dunk Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] Reverse Dunk'
    df.loc[(df['cod'] == '_2(1)') & (df['tp_subaction'] == 'Reverse Dunk Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] Reverse Dunk'
    df.loc[(df['cod'] == '_2(1)') & (df['tp_subaction'] == 'Reverse Slam Dunk Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] Reverse Slam Dunk'
    df.loc[(df['cod'] == '_2(1)') & (df['tp_subaction'] == 'Reverse Slam Dunk Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] Reverse Slam Dunk'
    df.loc[(df['cod'] == '_2(1)') & (df['tp_subaction'] == 'Running Alley Oop Dunk Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] Running Alley Oop Dunk Shot'
    df.loc[(df['cod'] == '_2(1)') & (df['tp_subaction'] == 'Running Alley Oop Dunk Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] Running Alley Oop Dunk Shot'
    df.loc[(df['cod'] == '_2(1)') & (df['tp_subaction'] == 'Running Dunk Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] Running Dunk'
    df.loc[(df['cod'] == '_2(1)') & (df['tp_subaction'] == 'Running Dunk Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] Running Dunk'
    df.loc[(df['cod'] == '_2(1)') & (df['tp_subaction'] == 'Running Reverse Dunk Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] Running Reverse Dunk Shot'
    df.loc[(df['cod'] == '_2(1)') & (df['tp_subaction'] == 'Running Slam Dunk Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] Running Slam Dunk'
    df.loc[(df['cod'] == '_2(1)') & (df['tp_subaction'] == 'Running Slam Dunk Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] Running Slam Dunk'
    df.loc[(df['cod'] == '_2(1)') & (df['tp_subaction'] == 'Slam Dunk Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] Slam Dunk'
    df.loc[(df['cod'] == '_2(1)') & (df['tp_subaction'] == 'Slam Dunk Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] Slam Dunk'
    df.loc[(df['cod'] == '_2(1)') & (df['tp_subaction'] == 'Tip Dunk Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] Tip Dunk Shot'
    df.loc[(df['cod'] == '_2(1)') & (df['tp_subaction'] == 'Tip Dunk Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] Tip Dunk Shot'
    df.loc[(df['cod'] == '_2(1b)') & (df['tp_subaction'] == 'Alley Oop Dunk Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] Alley Oop Dunk [player] BLOCK (cnt BLK)'
    df.loc[(df['cod'] == '_2(1b)') & (df['tp_subaction'] == 'Alley Oop Dunk Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] Alley Oop Dunk [player] BLOCK (cnt BLK)'
    df.loc[(df['cod'] == '_2(1b)') & (df['tp_subaction'] == 'Cutting Dunk Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] Cutting Dunk Shot [player] BLOCK (cnt BLK)'
    df.loc[(df['cod'] == '_2(1b)') & (df['tp_subaction'] == 'Cutting Dunk Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] Cutting Dunk Shot [player] BLOCK (cnt BLK)'
    df.loc[(df['cod'] == '_2(1b)') & (df['tp_subaction'] == 'Driving Dunk Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] Driving Dunk [player] BLOCK (cnt BLK)'
    df.loc[(df['cod'] == '_2(1b)') & (df['tp_subaction'] == 'Driving Dunk Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] Driving Dunk [player] BLOCK (cnt BLK)'
    df.loc[(df['cod'] == '_2(1b)') & (df['tp_subaction'] == 'Driving Reverse Dunk Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] Driving Reverse Dunk Shot [player] BLOCK (cnt BLK)'
    df.loc[(df['cod'] == '_2(1b)') & (df['tp_subaction'] == 'Driving Slam Dunk Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] Driving Slam Dunk [player] BLOCK (cnt BLK)'
    df.loc[(df['cod'] == '_2(1b)') & (df['tp_subaction'] == 'Driving Slam Dunk Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] Driving Slam Dunk [player] BLOCK (cnt BLK)'
    df.loc[(df['cod'] == '_2(1b)') & (df['tp_subaction'] == 'Dunk Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] Dunk [player] BLOCK (cnt BLK)'
    df.loc[(df['cod'] == '_2(1b)') & (df['tp_subaction'] == 'Dunk Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] Dunk [player] BLOCK (cnt BLK)'
    df.loc[(df['cod'] == '_2(1b)') & (df['tp_subaction'] == 'Putback Dunk Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] Putback Dunk [player] BLOCK (cnt BLK)'
    df.loc[(df['cod'] == '_2(1b)') & (df['tp_subaction'] == 'Putback Dunk Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] Putback Dunk [player] BLOCK (cnt BLK)'
    df.loc[(df['cod'] == '_2(1b)') & (df['tp_subaction'] == 'Putback Reverse Dunk Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] Putback Reverse Dunk [player] BLOCK (cnt BLK)'
    df.loc[(df['cod'] == '_2(1b)') & (df['tp_subaction'] == 'Putback Slam Dunk Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] Putback Slam Dunk [player] BLOCK (cnt BLK)'
    df.loc[(df['cod'] == '_2(1b)') & (df['tp_subaction'] == 'Reverse Dunk Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] Reverse Dunk [player] BLOCK (cnt BLK)'
    df.loc[(df['cod'] == '_2(1b)') & (df['tp_subaction'] == 'Reverse Dunk Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] Reverse Dunk [player] BLOCK (cnt BLK)'
    df.loc[(df['cod'] == '_2(1b)') & (df['tp_subaction'] == 'Reverse Slam Dunk Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] Reverse Slam Dunk [player] BLOCK (cnt BLK)'
    df.loc[(df['cod'] == '_2(1b)') & (df['tp_subaction'] == 'Running Alley Oop Dunk Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] Running Alley Oop Dunk Shot [player] BLOCK (cnt BLK)'
    df.loc[(df['cod'] == '_2(1b)') & (df['tp_subaction'] == 'Running Dunk Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] Running Dunk [player] BLOCK (cnt BLK)'
    df.loc[(df['cod'] == '_2(1b)') & (df['tp_subaction'] == 'Running Dunk Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] Running Dunk [player] BLOCK (cnt BLK)'
    df.loc[(df['cod'] == '_2(1b)') & (df['tp_subaction'] == 'Running Reverse Dunk Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] Running Reverse Dunk Shot [player] BLOCK (cnt BLK)'
    df.loc[(df['cod'] == '_2(1b)') & (df['tp_subaction'] == 'Running Slam Dunk Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] Running Slam Dunk [player] BLOCK (cnt BLK)'
    df.loc[(df['cod'] == '_2(1b)') & (df['tp_subaction'] == 'Running Slam Dunk Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] Running Slam Dunk [player] BLOCK (cnt BLK)'
    df.loc[(df['cod'] == '_2(1b)') & (df['tp_subaction'] == 'Slam Dunk Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] Slam Dunk [player] BLOCK (cnt BLK)'
    df.loc[(df['cod'] == '_2(1b)') & (df['tp_subaction'] == 'Slam Dunk Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] Slam Dunk [player] BLOCK (cnt BLK)'
    df.loc[(df['cod'] == '_2(1b)') & (df['tp_subaction'] == 'Tip Dunk Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] Tip Dunk Shot [player] BLOCK (cnt BLK)'
    df.loc[(df['cod'] == '_2(1b)') & (df['tp_subaction'] == 'Tip Dunk Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] Tip Dunk Shot [player] BLOCK (cnt BLK)'
    df.loc[(df['cod'] == '_2(2)') & (df['tp_subaction'] == 'Driving Bank Hook Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] Driving Bank Hook Shot'
    df.loc[(df['cod'] == '_2(2)') & (df['tp_subaction'] == 'Driving Hook Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] Driving Hook Shot'
    df.loc[(df['cod'] == '_2(2)') & (df['tp_subaction'] == 'Driving Hook Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] Driving Hook Shot'
    df.loc[(df['cod'] == '_2(2)') & (df['tp_subaction'] == 'Hook Bank Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] Hook Bank Shot'
    df.loc[(df['cod'] == '_2(2)') & (df['tp_subaction'] == 'Hook Bank Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] Hook Bank Shot'
    df.loc[(df['cod'] == '_2(2)') & (df['tp_subaction'] == 'Hook Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] Hook Shot'
    df.loc[(df['cod'] == '_2(2)') & (df['tp_subaction'] == 'Hook Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] Hook Shot'
    df.loc[(df['cod'] == '_2(2)') & (df['tp_subaction'] == 'Jump Bank Hook Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] Jump Bank Hook Shot'
    df.loc[(df['cod'] == '_2(2)') & (df['tp_subaction'] == 'Jump Hook Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] Jump Hook Shot'
    df.loc[(df['cod'] == '_2(2)') & (df['tp_subaction'] == 'Jump Hook Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] Jump Hook Shot'
    df.loc[(df['cod'] == '_2(2)') & (df['tp_subaction'] == 'Running Bank Hook Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] Running Bank Hook Shot'
    df.loc[(df['cod'] == '_2(2)') & (df['tp_subaction'] == 'Running Hook Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] Running Hook Shot'
    df.loc[(df['cod'] == '_2(2)') & (df['tp_subaction'] == 'Running Hook Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] Running Hook Shot'
    df.loc[(df['cod'] == '_2(2)') & (df['tp_subaction'] == 'Turnaround Bank Hook Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] Turnaround Bank Hook Shot'
    df.loc[(df['cod'] == '_2(2)') & (df['tp_subaction'] == 'Turnaround Bank Hook Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] Turnaround Bank Hook Shot'
    df.loc[(df['cod'] == '_2(2)') & (df['tp_subaction'] == 'Turnaround Hook Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] Turnaround Hook Shot'
    df.loc[(df['cod'] == '_2(2)') & (df['tp_subaction'] == 'Turnaround Hook Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] Turnaround Hook Shot'
    df.loc[(df['cod'] == '_2(2b)') & (df['tp_subaction'] == 'Driving Bank Hook Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] Driving Bank Hook Shot [player] BLOCK (cnt BLK)'
    df.loc[(df['cod'] == '_2(2b)') & (df['tp_subaction'] == 'Driving Hook Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] Driving Hook Shot [player] BLOCK (cnt BLK)'
    df.loc[(df['cod'] == '_2(2b)') & (df['tp_subaction'] == 'Driving Hook Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] Driving Hook Shot [player] BLOCK (cnt BLK)'
    df.loc[(df['cod'] == '_2(2b)') & (df['tp_subaction'] == 'Hook Bank Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] Hook Bank Shot [player] BLOCK (cnt BLK)'
    df.loc[(df['cod'] == '_2(2b)') & (df['tp_subaction'] == 'Hook Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] Hook Shot [player] BLOCK (cnt BLK)'
    df.loc[(df['cod'] == '_2(2b)') & (df['tp_subaction'] == 'Hook Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] Hook Shot [player] BLOCK (cnt BLK)'
    df.loc[(df['cod'] == '_2(2b)') & (df['tp_subaction'] == 'Jump Bank Hook Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] Jump Bank Hook Shot [player] BLOCK (cnt BLK)'
    df.loc[(df['cod'] == '_2(2b)') & (df['tp_subaction'] == 'Jump Hook Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] Jump Hook Shot [player] BLOCK (cnt BLK)'
    df.loc[(df['cod'] == '_2(2b)') & (df['tp_subaction'] == 'Jump Hook Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] Jump Hook Shot [player] BLOCK (cnt BLK)'
    df.loc[(df['cod'] == '_2(2b)') & (df['tp_subaction'] == 'Running Hook Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] Running Hook Shot [player] BLOCK (cnt BLK)'
    df.loc[(df['cod'] == '_2(2b)') & (df['tp_subaction'] == 'Running Hook Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] Running Hook Shot [player] BLOCK (cnt BLK)'
    df.loc[(df['cod'] == '_2(2b)') & (df['tp_subaction'] == 'Turnaround Bank Hook Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] Turnaround Bank Hook Shot [player] BLOCK (cnt BLK)'
    df.loc[(df['cod'] == '_2(2b)') & (df['tp_subaction'] == 'Turnaround Hook Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] Turnaround Hook Shot [player] BLOCK (cnt BLK)'
    df.loc[(df['cod'] == '_2(2b)') & (df['tp_subaction'] == 'Turnaround Hook Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] Turnaround Hook Shot [player] BLOCK (cnt BLK)'
    df.loc[(df['cod'] == '_2(3)') & (df['tp_subaction'] == 'Alley Oop Layup shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] Alley Oop Layup'
    df.loc[(df['cod'] == '_2(3)') & (df['tp_subaction'] == 'Alley Oop Layup shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] Alley Oop Layup'
    df.loc[(df['cod'] == '_2(3)') & (df['tp_subaction'] == 'Cutting Finger Roll Layup Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] Cutting Finger Roll Layup Shot'
    df.loc[(df['cod'] == '_2(3)') & (df['tp_subaction'] == 'Cutting Finger Roll Layup Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] Cutting Finger Roll Layup Shot'
    df.loc[(df['cod'] == '_2(3)') & (df['tp_subaction'] == 'Cutting Layup Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] Cutting Layup Shot'
    df.loc[(df['cod'] == '_2(3)') & (df['tp_subaction'] == 'Cutting Layup Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] Cutting Layup Shot'
    df.loc[(df['cod'] == '_2(3)') & (df['tp_subaction'] == 'Driving Finger Roll Layup Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] Driving Finger Roll Layup'
    df.loc[(df['cod'] == '_2(3)') & (df['tp_subaction'] == 'Driving Finger Roll Layup Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] Driving Finger Roll Layup'
    df.loc[(df['cod'] == '_2(3)') & (df['tp_subaction'] == 'Driving Layup Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] Driving Layup'
    df.loc[(df['cod'] == '_2(3)') & (df['tp_subaction'] == 'Driving Layup Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] Driving Layup'
    df.loc[(df['cod'] == '_2(3)') & (df['tp_subaction'] == 'Driving Reverse Layup Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] Driving Reverse Layup'
    df.loc[(df['cod'] == '_2(3)') & (df['tp_subaction'] == 'Driving Reverse Layup Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] Driving Reverse Layup'
    df.loc[(df['cod'] == '_2(3)') & (df['tp_subaction'] == 'Finger Roll Layup Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] Finger Roll Layup'
    df.loc[(df['cod'] == '_2(3)') & (df['tp_subaction'] == 'Finger Roll Layup Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] Finger Roll Layup'
    df.loc[(df['cod'] == '_2(3)') & (df['tp_subaction'] == 'Layup Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] Layup'
    df.loc[(df['cod'] == '_2(3)') & (df['tp_subaction'] == 'Layup Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] Layup'
    df.loc[(df['cod'] == '_2(3)') & (df['tp_subaction'] == 'Putback Layup Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] Putback Layup'
    df.loc[(df['cod'] == '_2(3)') & (df['tp_subaction'] == 'Putback Layup Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] Putback Layup'
    df.loc[(df['cod'] == '_2(3)') & (df['tp_subaction'] == 'Reverse Layup Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] Reverse Layup'
    df.loc[(df['cod'] == '_2(3)') & (df['tp_subaction'] == 'Reverse Layup Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] Reverse Layup'
    df.loc[(df['cod'] == '_2(3)') & (df['tp_subaction'] == 'Running Alley Oop Layup Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] Running Alley Oop Layup Shot'
    df.loc[(df['cod'] == '_2(3)') & (df['tp_subaction'] == 'Running Alley Oop Layup Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] Running Alley Oop Layup Shot'
    df.loc[(df['cod'] == '_2(3)') & (df['tp_subaction'] == 'Running Finger Roll Layup Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] Running Finger Roll Layup'
    df.loc[(df['cod'] == '_2(3)') & (df['tp_subaction'] == 'Running Finger Roll Layup Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] Running Finger Roll Layup'
    df.loc[(df['cod'] == '_2(3)') & (df['tp_subaction'] == 'Running Layup Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] Running Layup'
    df.loc[(df['cod'] == '_2(3)') & (df['tp_subaction'] == 'Running Layup Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] Running Layup'
    df.loc[(df['cod'] == '_2(3)') & (df['tp_subaction'] == 'Running Reverse Layup Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] Running Reverse Layup'
    df.loc[(df['cod'] == '_2(3)') & (df['tp_subaction'] == 'Running Reverse Layup Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] Running Reverse Layup'
    df.loc[(df['cod'] == '_2(3)') & (df['tp_subaction'] == 'Running Tip Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] Running Tip Shot'
    df.loc[(df['cod'] == '_2(3)') & (df['tp_subaction'] == 'Running Tip Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] Running Tip Shot'
    df.loc[(df['cod'] == '_2(3)') & (df['tp_subaction'] == 'Tip Layup Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] Tip Layup Shot'
    df.loc[(df['cod'] == '_2(3)') & (df['tp_subaction'] == 'Tip Layup Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] Tip Layup Shot'
    df.loc[(df['cod'] == '_2(3)') & (df['tp_subaction'] == 'Tip Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] Tip Shot'
    df.loc[(df['cod'] == '_2(3)') & (df['tp_subaction'] == 'Tip Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] Tip Shot'
    df.loc[(df['cod'] == '_2(3b)') & (df['tp_subaction'] == 'Alley Oop Layup shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] Alley Oop Layup [player] BLOCK (cnt BLK)'
    df.loc[(df['cod'] == '_2(3b)') & (df['tp_subaction'] == 'Alley Oop Layup shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] Alley Oop Layup [player] BLOCK (cnt BLK)'
    df.loc[(df['cod'] == '_2(3b)') & (df['tp_subaction'] == 'Cutting Finger Roll Layup Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] Cutting Finger Roll Layup Shot [player] BLOCK (cnt BLK)'
    df.loc[(df['cod'] == '_2(3b)') & (df['tp_subaction'] == 'Cutting Finger Roll Layup Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] Cutting Finger Roll Layup Shot [player] BLOCK (cnt BLK)'
    df.loc[(df['cod'] == '_2(3b)') & (df['tp_subaction'] == 'Cutting Layup Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] Cutting Layup Shot [player] BLOCK (cnt BLK)'
    df.loc[(df['cod'] == '_2(3b)') & (df['tp_subaction'] == 'Cutting Layup Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] Cutting Layup Shot [player] BLOCK (cnt BLK)'
    df.loc[(df['cod'] == '_2(3b)') & (df['tp_subaction'] == 'Driving Finger Roll Layup Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] Driving Finger Roll Layup [player] BLOCK (cnt BLK)'
    df.loc[(df['cod'] == '_2(3b)') & (df['tp_subaction'] == 'Driving Finger Roll Layup Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] Driving Finger Roll Layup [player] BLOCK (cnt BLK)'
    df.loc[(df['cod'] == '_2(3b)') & (df['tp_subaction'] == 'Driving Layup Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] Driving Layup [player] BLOCK (cnt BLK)'
    df.loc[(df['cod'] == '_2(3b)') & (df['tp_subaction'] == 'Driving Layup Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] Driving Layup [player] BLOCK (cnt BLK)'
    df.loc[(df['cod'] == '_2(3b)') & (df['tp_subaction'] == 'Driving Reverse Layup Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] Driving Reverse Layup [player] BLOCK (cnt BLK)'
    df.loc[(df['cod'] == '_2(3b)') & (df['tp_subaction'] == 'Driving Reverse Layup Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] Driving Reverse Layup [player] BLOCK (cnt BLK)'
    df.loc[(df['cod'] == '_2(3b)') & (df['tp_subaction'] == 'Finger Roll Layup Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] Finger Roll Layup [player] BLOCK (cnt BLK)'
    df.loc[(df['cod'] == '_2(3b)') & (df['tp_subaction'] == 'Finger Roll Layup Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] Finger Roll Layup [player] BLOCK (cnt BLK)'
    df.loc[(df['cod'] == '_2(3b)') & (df['tp_subaction'] == 'Layup Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] Layup [player] BLOCK (cnt BLK)'
    df.loc[(df['cod'] == '_2(3b)') & (df['tp_subaction'] == 'Layup Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] Layup [player] BLOCK (cnt BLK)'
    df.loc[(df['cod'] == '_2(3b)') & (df['tp_subaction'] == 'Putback Layup Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] Putback Layup [player] BLOCK (cnt BLK)'
    df.loc[(df['cod'] == '_2(3b)') & (df['tp_subaction'] == 'Putback Layup Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] Putback Layup [player] BLOCK (cnt BLK)'
    df.loc[(df['cod'] == '_2(3b)') & (df['tp_subaction'] == 'Reverse Layup Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] Reverse Layup [player] BLOCK (cnt BLK)'
    df.loc[(df['cod'] == '_2(3b)') & (df['tp_subaction'] == 'Reverse Layup Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] Reverse Layup [player] BLOCK (cnt BLK)'
    df.loc[(df['cod'] == '_2(3b)') & (df['tp_subaction'] == 'Running Alley Oop Layup Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] Running Alley Oop Layup Shot [player] BLOCK (cnt BLK)'
    df.loc[(df['cod'] == '_2(3b)') & (df['tp_subaction'] == 'Running Finger Roll Layup Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] Running Finger Roll Layup [player] BLOCK (cnt BLK)'
    df.loc[(df['cod'] == '_2(3b)') & (df['tp_subaction'] == 'Running Finger Roll Layup Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] Running Finger Roll Layup [player] BLOCK (cnt BLK)'
    df.loc[(df['cod'] == '_2(3b)') & (df['tp_subaction'] == 'Running Layup Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] Running Layup [player] BLOCK (cnt BLK)'
    df.loc[(df['cod'] == '_2(3b)') & (df['tp_subaction'] == 'Running Layup Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] Running Layup [player] BLOCK (cnt BLK)'
    df.loc[(df['cod'] == '_2(3b)') & (df['tp_subaction'] == 'Running Reverse Layup Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] Running Reverse Layup [player] BLOCK (cnt BLK)'
    df.loc[(df['cod'] == '_2(3b)') & (df['tp_subaction'] == 'Running Reverse Layup Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] Running Reverse Layup [player] BLOCK (cnt BLK)'
    df.loc[(df['cod'] == '_2(3b)') & (df['tp_subaction'] == 'Tip Layup Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] Tip Layup Shot [player] BLOCK (cnt BLK)'
    df.loc[(df['cod'] == '_2(3b)') & (df['tp_subaction'] == 'Tip Layup Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] Tip Layup Shot [player] BLOCK (cnt BLK)'
    df.loc[(df['cod'] == '_2(3b)') & (df['tp_subaction'] == 'Tip Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] Tip Shot [player] BLOCK (cnt BLK)'
    df.loc[(df['cod'] == '_2(3b)') & (df['tp_subaction'] == 'Tip Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] Tip Shot [player] BLOCK (cnt BLK)'
    df.loc[(df['cod'] == '_2(4)') & (df['tp_subaction'] == '') & (df['ind_dist'] == 0) & (df['ind_no'] == 1), 'gram'] = 'MISS [player] No Shot'
    df.loc[(df['cod'] == '_2(4)') & (df['tp_subaction'] == '') & (df['ind_dist'] == 1) & (df['ind_no'] == 1), 'gram'] = 'MISS [player] [dist] No Shot'
    df.loc[(df['cod'] == '_2(4)') & (df['tp_subaction'] == '') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] Shot'
    df.loc[(df['cod'] == '_2(4)') & (df['tp_subaction'] == '') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] Shot'
    df.loc[(df['cod'] == '_2(4)') & (df['tp_subaction'] == 'Driving Bank shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] Driving Bank Shot'
    df.loc[(df['cod'] == '_2(4)') & (df['tp_subaction'] == 'Driving Floating Bank Jump Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] Driving Floating Bank Jump Shot'
    df.loc[(df['cod'] == '_2(4)') & (df['tp_subaction'] == 'Driving Floating Bank Jump Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] Driving Floating Bank Jump Shot'
    df.loc[(df['cod'] == '_2(4)') & (df['tp_subaction'] == 'Driving Floating Jump Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] Driving Floating Jump Shot'
    df.loc[(df['cod'] == '_2(4)') & (df['tp_subaction'] == 'Driving Floating Jump Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] Driving Floating Jump Shot'
    df.loc[(df['cod'] == '_2(4)') & (df['tp_subaction'] == 'Driving Jump shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] Driving Jump Shot'
    df.loc[(df['cod'] == '_2(4)') & (df['tp_subaction'] == 'Driving Jump shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] Driving Jump Shot'
    df.loc[(df['cod'] == '_2(4)') & (df['tp_subaction'] == 'Fadeaway Bank shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] Fadeaway Bank Shot'
    df.loc[(df['cod'] == '_2(4)') & (df['tp_subaction'] == 'Fadeaway Jump Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] Fadeaway Jumper'
    df.loc[(df['cod'] == '_2(4)') & (df['tp_subaction'] == 'Fadeaway Jump Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] Fadeaway Jumper'
    df.loc[(df['cod'] == '_2(4)') & (df['tp_subaction'] == 'Floating Jump shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] Floating Jump Shot'
    df.loc[(df['cod'] == '_2(4)') & (df['tp_subaction'] == 'Floating Jump shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] Floating Jump Shot'
    df.loc[(df['cod'] == '_2(4)') & (df['tp_subaction'] == 'Jump Bank Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] Jump Bank Shot'
    df.loc[(df['cod'] == '_2(4)') & (df['tp_subaction'] == 'Jump Bank Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] Jump Bank Shot'
    df.loc[(df['cod'] == '_2(4)') & (df['tp_subaction'] == 'Jump Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] Jump Shot'
    df.loc[(df['cod'] == '_2(4)') & (df['tp_subaction'] == 'Jump Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] Jump Shot'
    df.loc[(df['cod'] == '_2(4)') & (df['tp_subaction'] == 'Pullup Bank shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] Pullup Bank Shot'
    df.loc[(df['cod'] == '_2(4)') & (df['tp_subaction'] == 'Pullup Jump shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] Pullup Jump Shot'
    df.loc[(df['cod'] == '_2(4)') & (df['tp_subaction'] == 'Pullup Jump shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] Pullup Jump Shot'
    df.loc[(df['cod'] == '_2(4)') & (df['tp_subaction'] == 'Running Bank shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] Running Bank Shot'
    df.loc[(df['cod'] == '_2(4)') & (df['tp_subaction'] == 'Running Jump Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] Running Jump Shot'
    df.loc[(df['cod'] == '_2(4)') & (df['tp_subaction'] == 'Running Jump Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] Running Jump Shot'
    df.loc[(df['cod'] == '_2(4)') & (df['tp_subaction'] == 'Running Pull-Up Jump Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] Running Pull-Up Jump Shot'
    df.loc[(df['cod'] == '_2(4)') & (df['tp_subaction'] == 'Step Back Bank Jump Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] Step Back Bank Jump Shot'
    df.loc[(df['cod'] == '_2(4)') & (df['tp_subaction'] == 'Step Back Jump shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] Step Back Jump Shot'
    df.loc[(df['cod'] == '_2(4)') & (df['tp_subaction'] == 'Step Back Jump shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] Step Back Jump Shot'
    df.loc[(df['cod'] == '_2(4)') & (df['tp_subaction'] == 'Turnaround Bank shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] Turnaround Bank Shot'
    df.loc[(df['cod'] == '_2(4)') & (df['tp_subaction'] == 'Turnaround Fadeaway Bank Jump Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] Turnaround Fadeaway Bank Jump Shot'
    df.loc[(df['cod'] == '_2(4)') & (df['tp_subaction'] == 'Turnaround Fadeaway shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] Turnaround Fadeaway Shot'
    df.loc[(df['cod'] == '_2(4)') & (df['tp_subaction'] == 'Turnaround Fadeaway shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] Turnaround Fadeaway Shot'
    df.loc[(df['cod'] == '_2(4)') & (df['tp_subaction'] == 'Turnaround Jump Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] Turnaround Jump Shot'
    df.loc[(df['cod'] == '_2(4)') & (df['tp_subaction'] == 'Turnaround Jump Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] Turnaround Jump Shot'
    df.loc[(df['cod'] == '_2(4b)') & (df['tp_subaction'] == '') & (df['ind_dist'] == 0) & (df['ind_no'] == 1), 'gram'] = 'MISS [player] No Shot [player] BLOCK (cnt BLK)'
    df.loc[(df['cod'] == '_2(4b)') & (df['tp_subaction'] == '') & (df['ind_dist'] == 1) & (df['ind_no'] == 1), 'gram'] = 'MISS [player] [dist] No Shot [player] BLOCK (cnt BLK)'
    df.loc[(df['cod'] == '_2(4b)') & (df['tp_subaction'] == '') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] Shot [player] BLOCK (cnt BLK)'
    df.loc[(df['cod'] == '_2(4b)') & (df['tp_subaction'] == '') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] Shot [player] BLOCK (cnt BLK)'
    df.loc[(df['cod'] == '_2(4b)') & (df['tp_subaction'] == 'Driving Bank shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] Driving Bank Shot [player] BLOCK (cnt BLK)'
    df.loc[(df['cod'] == '_2(4b)') & (df['tp_subaction'] == 'Driving Floating Bank Jump Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] Driving Floating Bank Jump Shot [player] BLOCK (cnt BLK)'
    df.loc[(df['cod'] == '_2(4b)') & (df['tp_subaction'] == 'Driving Floating Bank Jump Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] Driving Floating Bank Jump Shot [player] BLOCK (cnt BLK)'
    df.loc[(df['cod'] == '_2(4b)') & (df['tp_subaction'] == 'Driving Floating Jump Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] Driving Floating Jump Shot [player] BLOCK (cnt BLK)'
    df.loc[(df['cod'] == '_2(4b)') & (df['tp_subaction'] == 'Driving Floating Jump Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] Driving Floating Jump Shot [player] BLOCK (cnt BLK)'
    df.loc[(df['cod'] == '_2(4b)') & (df['tp_subaction'] == 'Driving Jump shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] Driving Jump Shot [player] BLOCK (cnt BLK)'
    df.loc[(df['cod'] == '_2(4b)') & (df['tp_subaction'] == 'Fadeaway Bank shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] Fadeaway Bank Shot [player] BLOCK (cnt BLK)'
    df.loc[(df['cod'] == '_2(4b)') & (df['tp_subaction'] == 'Fadeaway Jump Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] Fadeaway Jumper [player] BLOCK (cnt BLK)'
    df.loc[(df['cod'] == '_2(4b)') & (df['tp_subaction'] == 'Floating Jump shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] Floating Jump Shot [player] BLOCK (cnt BLK)'
    df.loc[(df['cod'] == '_2(4b)') & (df['tp_subaction'] == 'Floating Jump shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] Floating Jump Shot [player] BLOCK (cnt BLK)'
    df.loc[(df['cod'] == '_2(4b)') & (df['tp_subaction'] == 'Jump Bank Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] Jump Bank Shot [player] BLOCK (cnt BLK)'
    df.loc[(df['cod'] == '_2(4b)') & (df['tp_subaction'] == 'Jump Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] Jump Shot [player] BLOCK (cnt BLK)'
    df.loc[(df['cod'] == '_2(4b)') & (df['tp_subaction'] == 'Jump Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] Jump Shot [player] BLOCK (cnt BLK)'
    df.loc[(df['cod'] == '_2(4b)') & (df['tp_subaction'] == 'Pullup Bank shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] Pullup Bank Shot [player] BLOCK (cnt BLK)'
    df.loc[(df['cod'] == '_2(4b)') & (df['tp_subaction'] == 'Pullup Jump shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] Pullup Jump Shot [player] BLOCK (cnt BLK)'
    df.loc[(df['cod'] == '_2(4b)') & (df['tp_subaction'] == 'Running Bank shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] Running Bank Shot [player] BLOCK (cnt BLK)'
    df.loc[(df['cod'] == '_2(4b)') & (df['tp_subaction'] == 'Running Jump Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] Running Jump Shot [player] BLOCK (cnt BLK)'
    df.loc[(df['cod'] == '_2(4b)') & (df['tp_subaction'] == 'Running Jump Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] Running Jump Shot [player] BLOCK (cnt BLK)'
    df.loc[(df['cod'] == '_2(4b)') & (df['tp_subaction'] == 'Running Pull-Up Jump Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] Running Pull-Up Jump Shot [player] BLOCK (cnt BLK)'
    df.loc[(df['cod'] == '_2(4b)') & (df['tp_subaction'] == 'Step Back Bank Jump Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] Step Back Bank Jump Shot [player] BLOCK (cnt BLK)'
    df.loc[(df['cod'] == '_2(4b)') & (df['tp_subaction'] == 'Step Back Jump shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] Step Back Jump Shot [player] BLOCK (cnt BLK)'
    df.loc[(df['cod'] == '_2(4b)') & (df['tp_subaction'] == 'Turnaround Bank shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] Turnaround Bank Shot [player] BLOCK (cnt BLK)'
    df.loc[(df['cod'] == '_2(4b)') & (df['tp_subaction'] == 'Turnaround Fadeaway Bank Jump Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] Turnaround Fadeaway Bank Jump Shot [player] BLOCK (cnt BLK)'
    df.loc[(df['cod'] == '_2(4b)') & (df['tp_subaction'] == 'Turnaround Fadeaway shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] Turnaround Fadeaway Shot [player] BLOCK (cnt BLK)'
    df.loc[(df['cod'] == '_2(4b)') & (df['tp_subaction'] == 'Turnaround Fadeaway shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] Turnaround Fadeaway Shot [player] BLOCK (cnt BLK)'
    df.loc[(df['cod'] == '_2(4b)') & (df['tp_subaction'] == 'Turnaround Jump Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] Turnaround Jump Shot [player] BLOCK (cnt BLK)'
    df.loc[(df['cod'] == '_2(4b)') & (df['tp_subaction'] == 'Turnaround Jump Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] Turnaround Jump Shot [player] BLOCK (cnt BLK)'
    df.loc[(df['cod'] == '_2(5)') & (df['tp_subaction'] == '') & (df['ind_dist'] == 1) & (df['ind_no'] == 1), 'gram'] = 'MISS [player] [dist] 3PT No Shot'
    df.loc[(df['cod'] == '_2(5)') & (df['tp_subaction'] == '') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] 3PT Shot'
    df.loc[(df['cod'] == '_2(5)') & (df['tp_subaction'] == 'Driving Bank Hook Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] 3PT Driving Bank Hook Shot'
    df.loc[(df['cod'] == '_2(5)') & (df['tp_subaction'] == 'Driving Bank shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] 3PT Driving Bank Shot'
    df.loc[(df['cod'] == '_2(5)') & (df['tp_subaction'] == 'Driving Floating Bank Jump Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] 3PT Driving Floating Bank Jump Shot'
    df.loc[(df['cod'] == '_2(5)') & (df['tp_subaction'] == 'Driving Floating Bank Jump Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] 3PT Driving Floating Bank Jump Shot'
    df.loc[(df['cod'] == '_2(5)') & (df['tp_subaction'] == 'Driving Floating Jump Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] 3PT Driving Floating Jump Shot'
    df.loc[(df['cod'] == '_2(5)') & (df['tp_subaction'] == 'Driving Hook Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] 3PT Driving Hook Shot'
    df.loc[(df['cod'] == '_2(5)') & (df['tp_subaction'] == 'Driving Jump shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] 3PT Driving Jump Shot'
    df.loc[(df['cod'] == '_2(5)') & (df['tp_subaction'] == 'Dunk Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] 3PT Dunk'
    df.loc[(df['cod'] == '_2(5)') & (df['tp_subaction'] == 'Fadeaway Bank shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] 3PT Fadeaway Bank Shot'
    df.loc[(df['cod'] == '_2(5)') & (df['tp_subaction'] == 'Fadeaway Jump Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] 3PT Fadeaway Jumper'
    df.loc[(df['cod'] == '_2(5)') & (df['tp_subaction'] == 'Fadeaway Jump Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] 3PT Fadeaway Jumper'
    df.loc[(df['cod'] == '_2(5)') & (df['tp_subaction'] == 'Floating Jump shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] 3PT Floating Jump Shot'
    df.loc[(df['cod'] == '_2(5)') & (df['tp_subaction'] == 'Floating Jump shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] 3PT Floating Jump Shot'
    df.loc[(df['cod'] == '_2(5)') & (df['tp_subaction'] == 'Hook Bank Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] 3PT Hook Bank Shot'
    df.loc[(df['cod'] == '_2(5)') & (df['tp_subaction'] == 'Hook Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] 3PT Hook Shot'
    df.loc[(df['cod'] == '_2(5)') & (df['tp_subaction'] == 'Jump Bank Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] 3PT Jump Bank Shot'
    df.loc[(df['cod'] == '_2(5)') & (df['tp_subaction'] == 'Jump Bank Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] 3PT Jump Bank Shot'
    df.loc[(df['cod'] == '_2(5)') & (df['tp_subaction'] == 'Jump Hook Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] 3PT Jump Hook Shot'
    df.loc[(df['cod'] == '_2(5)') & (df['tp_subaction'] == 'Jump Hook Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] 3PT Jump Hook Shot'
    df.loc[(df['cod'] == '_2(5)') & (df['tp_subaction'] == 'Jump Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] 3PT Jump Shot'
    df.loc[(df['cod'] == '_2(5)') & (df['tp_subaction'] == 'Jump Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] 3PT Jump Shot'
    df.loc[(df['cod'] == '_2(5)') & (df['tp_subaction'] == 'Layup Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] 3PT Layup'
    df.loc[(df['cod'] == '_2(5)') & (df['tp_subaction'] == 'Pullup Bank shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] 3PT Pullup Bank Shot'
    df.loc[(df['cod'] == '_2(5)') & (df['tp_subaction'] == 'Pullup Bank shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] 3PT Pullup Bank Shot'
    df.loc[(df['cod'] == '_2(5)') & (df['tp_subaction'] == 'Pullup Jump shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] 3PT Pullup Jump Shot'
    df.loc[(df['cod'] == '_2(5)') & (df['tp_subaction'] == 'Pullup Jump shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] 3PT Pullup Jump Shot'
    df.loc[(df['cod'] == '_2(5)') & (df['tp_subaction'] == 'Reverse Layup Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] 3PT Reverse Layup'
    df.loc[(df['cod'] == '_2(5)') & (df['tp_subaction'] == 'Running Bank shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] 3PT Running Bank Shot'
    df.loc[(df['cod'] == '_2(5)') & (df['tp_subaction'] == 'Running Hook Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] 3PT Running Hook Shot'
    df.loc[(df['cod'] == '_2(5)') & (df['tp_subaction'] == 'Running Jump Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] 3PT Running Jump Shot'
    df.loc[(df['cod'] == '_2(5)') & (df['tp_subaction'] == 'Running Jump Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] 3PT Running Jump Shot'
    df.loc[(df['cod'] == '_2(5)') & (df['tp_subaction'] == 'Running Layup Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] 3PT Running Layup'
    df.loc[(df['cod'] == '_2(5)') & (df['tp_subaction'] == 'Running Pull-Up Jump Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] 3PT Running Pull-Up Jump Shot'
    df.loc[(df['cod'] == '_2(5)') & (df['tp_subaction'] == 'Running Pull-Up Jump Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] 3PT Running Pull-Up Jump Shot'
    df.loc[(df['cod'] == '_2(5)') & (df['tp_subaction'] == 'Step Back Bank Jump Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] 3PT Step Back Bank Jump Shot'
    df.loc[(df['cod'] == '_2(5)') & (df['tp_subaction'] == 'Step Back Bank Jump Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] 3PT Step Back Bank Jump Shot'
    df.loc[(df['cod'] == '_2(5)') & (df['tp_subaction'] == 'Step Back Jump shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] 3PT Step Back Jump Shot'
    df.loc[(df['cod'] == '_2(5)') & (df['tp_subaction'] == 'Step Back Jump shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] 3PT Step Back Jump Shot'
    df.loc[(df['cod'] == '_2(5)') & (df['tp_subaction'] == 'Tip Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] 3PT Tip Shot'
    df.loc[(df['cod'] == '_2(5)') & (df['tp_subaction'] == 'Turnaround Bank shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] 3PT Turnaround Bank Shot'
    df.loc[(df['cod'] == '_2(5)') & (df['tp_subaction'] == 'Turnaround Fadeaway Bank Jump Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] 3PT Turnaround Fadeaway Bank Jump Shot'
    df.loc[(df['cod'] == '_2(5)') & (df['tp_subaction'] == 'Turnaround Fadeaway Bank Jump Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] 3PT Turnaround Fadeaway Bank Jump Shot'
    df.loc[(df['cod'] == '_2(5)') & (df['tp_subaction'] == 'Turnaround Fadeaway shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] 3PT Turnaround Fadeaway Shot'
    df.loc[(df['cod'] == '_2(5)') & (df['tp_subaction'] == 'Turnaround Fadeaway shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] 3PT Turnaround Fadeaway Shot'
    df.loc[(df['cod'] == '_2(5)') & (df['tp_subaction'] == 'Turnaround Hook Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] 3PT Turnaround Hook Shot'
    df.loc[(df['cod'] == '_2(5)') & (df['tp_subaction'] == 'Turnaround Jump Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] 3PT Turnaround Jump Shot'
    df.loc[(df['cod'] == '_2(5)') & (df['tp_subaction'] == 'Turnaround Jump Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] 3PT Turnaround Jump Shot'
    df.loc[(df['cod'] == '_2(5b)') & (df['tp_subaction'] == 'Driving Floating Bank Jump Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] 3PT Driving Floating Bank Jump Shot [player] BLOCK (cnt BLK)'
    df.loc[(df['cod'] == '_2(5b)') & (df['tp_subaction'] == 'Driving Floating Jump Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] 3PT Driving Floating Jump Shot [player] BLOCK (cnt BLK)'
    df.loc[(df['cod'] == '_2(5b)') & (df['tp_subaction'] == 'Driving Floating Jump Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] 3PT Driving Floating Jump Shot [player] BLOCK (cnt BLK)'
    df.loc[(df['cod'] == '_2(5b)') & (df['tp_subaction'] == 'Driving Jump shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] 3PT Driving Jump Shot [player] BLOCK (cnt BLK)'
    df.loc[(df['cod'] == '_2(5b)') & (df['tp_subaction'] == 'Driving Jump shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] 3PT Driving Jump Shot [player] BLOCK (cnt BLK)'
    df.loc[(df['cod'] == '_2(5b)') & (df['tp_subaction'] == 'Driving Layup Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] 3PT Driving Layup [player] BLOCK (cnt BLK)'
    df.loc[(df['cod'] == '_2(5b)') & (df['tp_subaction'] == 'Fadeaway Jump Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] 3PT Fadeaway Jumper [player] BLOCK (cnt BLK)'
    df.loc[(df['cod'] == '_2(5b)') & (df['tp_subaction'] == 'Fadeaway Jump Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] 3PT Fadeaway Jumper [player] BLOCK (cnt BLK)'
    df.loc[(df['cod'] == '_2(5b)') & (df['tp_subaction'] == 'Floating Jump shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] 3PT Floating Jump Shot [player] BLOCK (cnt BLK)'
    df.loc[(df['cod'] == '_2(5b)') & (df['tp_subaction'] == 'Hook Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] 3PT Hook Shot [player] BLOCK (cnt BLK)'
    df.loc[(df['cod'] == '_2(5b)') & (df['tp_subaction'] == 'Hook Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] 3PT Hook Shot [player] BLOCK (cnt BLK)'
    df.loc[(df['cod'] == '_2(5b)') & (df['tp_subaction'] == 'Jump Bank Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] 3PT Jump Bank Shot [player] BLOCK (cnt BLK)'
    df.loc[(df['cod'] == '_2(5b)') & (df['tp_subaction'] == 'Jump Bank Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] 3PT Jump Bank Shot [player] BLOCK (cnt BLK)'
    df.loc[(df['cod'] == '_2(5b)') & (df['tp_subaction'] == 'Jump Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] 3PT Jump Shot [player] BLOCK (cnt BLK)'
    df.loc[(df['cod'] == '_2(5b)') & (df['tp_subaction'] == 'Jump Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] 3PT Jump Shot [player] BLOCK (cnt BLK)'
    df.loc[(df['cod'] == '_2(5b)') & (df['tp_subaction'] == 'Pullup Jump shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] 3PT Pullup Jump Shot [player] BLOCK (cnt BLK)'
    df.loc[(df['cod'] == '_2(5b)') & (df['tp_subaction'] == 'Pullup Jump shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] 3PT Pullup Jump Shot [player] BLOCK (cnt BLK)'
    df.loc[(df['cod'] == '_2(5b)') & (df['tp_subaction'] == 'Running Jump Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] 3PT Running Jump Shot [player] BLOCK (cnt BLK)'
    df.loc[(df['cod'] == '_2(5b)') & (df['tp_subaction'] == 'Running Jump Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] 3PT Running Jump Shot [player] BLOCK (cnt BLK)'
    df.loc[(df['cod'] == '_2(5b)') & (df['tp_subaction'] == 'Running Pull-Up Jump Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] 3PT Running Pull-Up Jump Shot [player] BLOCK (cnt BLK)'
    df.loc[(df['cod'] == '_2(5b)') & (df['tp_subaction'] == 'Running Pull-Up Jump Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] 3PT Running Pull-Up Jump Shot [player] BLOCK (cnt BLK)'
    df.loc[(df['cod'] == '_2(5b)') & (df['tp_subaction'] == 'Step Back Bank Jump Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] 3PT Step Back Bank Jump Shot [player] BLOCK (cnt BLK)'
    df.loc[(df['cod'] == '_2(5b)') & (df['tp_subaction'] == 'Step Back Jump shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] 3PT Step Back Jump Shot [player] BLOCK (cnt BLK)'
    df.loc[(df['cod'] == '_2(5b)') & (df['tp_subaction'] == 'Step Back Jump shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] 3PT Step Back Jump Shot [player] BLOCK (cnt BLK)'
    df.loc[(df['cod'] == '_2(5b)') & (df['tp_subaction'] == 'Tip Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] 3PT Tip Shot [player] BLOCK (cnt BLK)'
    df.loc[(df['cod'] == '_2(5b)') & (df['tp_subaction'] == 'Turnaround Fadeaway Bank Jump Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] 3PT Turnaround Fadeaway Bank Jump Shot [player] BLOCK (cnt BLK)'
    df.loc[(df['cod'] == '_2(5b)') & (df['tp_subaction'] == 'Turnaround Fadeaway shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] 3PT Turnaround Fadeaway Shot [player] BLOCK (cnt BLK)'
    df.loc[(df['cod'] == '_2(5b)') & (df['tp_subaction'] == 'Turnaround Fadeaway shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] 3PT Turnaround Fadeaway Shot [player] BLOCK (cnt BLK)'
    df.loc[(df['cod'] == '_2(5b)') & (df['tp_subaction'] == 'Turnaround Jump Shot') & (df['ind_dist'] == 0) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] 3PT Turnaround Jump Shot [player] BLOCK (cnt BLK)'
    df.loc[(df['cod'] == '_2(5b)') & (df['tp_subaction'] == 'Turnaround Jump Shot') & (df['ind_dist'] == 1) & (df['ind_no'] == 0), 'gram'] = 'MISS [player] [dist] 3PT Turnaround Jump Shot [player] BLOCK (cnt BLK)'

    df.loc[(df['cod'] == '_3(00c)'), 'gram'] = '[player] (cnt PTS)'
    df.loc[(df['cod'] == '_3(00w)'), 'gram'] = 'MISS [player]'
    df.loc[(df['cod'] == '_3(11c)'), 'gram'] = '[player] Free Throw 1 of 1 (cnt PTS)'
    df.loc[(df['cod'] == '_3(11w)'), 'gram'] = 'MISS [player] Free Throw 1 of 1'
    df.loc[(df['cod'] == '_3(12c)'), 'gram'] = '[player] Free Throw 1 of 2 (cnt PTS)'
    df.loc[(df['cod'] == '_3(12w)'), 'gram'] = 'MISS [player] Free Throw 1 of 2'
    df.loc[(df['cod'] == '_3(13c)'), 'gram'] = '[player] Free Throw 1 of 3 (cnt PTS)'
    df.loc[(df['cod'] == '_3(13w)'), 'gram'] = 'MISS [player] Free Throw 1 of 3'
    df.loc[(df['cod'] == '_3(22c)'), 'gram'] = '[player] Free Throw 2 of 2 (cnt PTS)'
    df.loc[(df['cod'] == '_3(22w)'), 'gram'] = 'MISS [player] Free Throw 2 of 2'
    df.loc[(df['cod'] == '_3(23c)'), 'gram'] = '[player] Free Throw 2 of 3 (cnt PTS)'
    df.loc[(df['cod'] == '_3(23w)'), 'gram'] = 'MISS [player] Free Throw 2 of 3'
    df.loc[(df['cod'] == '_3(33c)'), 'gram'] = '[player] Free Throw 3 of 3 (cnt PTS)'
    df.loc[(df['cod'] == '_3(33w)'), 'gram'] = 'MISS [player] Free Throw 3 of 3'
    df.loc[(df['cod'] == '_3(40c)'), 'gram'] = '[player] Free Throw Technical (cnt PTS)'
    df.loc[(df['cod'] == '_3(40w)'), 'gram'] = 'MISS [player] Free Throw Technical'
    df.loc[(df['cod'] == '_3(51c)'), 'gram'] = '[player] Free Throw Flagrant 1 of 2 (cnt PTS)'
    df.loc[(df['cod'] == '_3(51w)'), 'gram'] = 'MISS [player] Free Throw Flagrant 1 of 2'
    df.loc[(df['cod'] == '_3(52c)'), 'gram'] = '[player] Free Throw Flagrant 2 of 2 (cnt PTS)'
    df.loc[(df['cod'] == '_3(52w)'), 'gram'] = 'MISS [player] Free Throw Flagrant 2 of 2'
    df.loc[(df['cod'] == '_3(61c)'), 'gram'] = '[player] Free Throw Flagrant 1 of 1 (cnt PTS)'
    df.loc[(df['cod'] == '_3(71c)'), 'gram'] = '[player] Free Throw Clear Path 1 of 2 (cnt PTS)'
    df.loc[(df['cod'] == '_3(71w)'), 'gram'] = 'MISS [player] Free Throw Clear Path 1 of 2'
    df.loc[(df['cod'] == '_3(72c)'), 'gram'] = '[player] Free Throw Clear Path 2 of 2 (cnt PTS)'
    df.loc[(df['cod'] == '_3(72w)'), 'gram'] = 'MISS [player] Free Throw Clear Path 2 of 2'
    df.loc[(df['cod'] == '_3(81c)'), 'gram'] = '[player] Free Throw Flagrant 1 of 3 (cnt PTS)'
    df.loc[(df['cod'] == '_3(81w)'), 'gram'] = 'MISS [player] Free Throw Flagrant 1 of 3'
    df.loc[(df['cod'] == '_3(82c)'), 'gram'] = '[player] Free Throw Flagrant 2 of 3 (cnt PTS)'
    df.loc[(df['cod'] == '_3(82w)'), 'gram'] = 'MISS [player] Free Throw Flagrant 2 of 3'
    df.loc[(df['cod'] == '_3(83c)'), 'gram'] = '[player] Free Throw Flagrant 3 of 3 (cnt PTS)'
    df.loc[(df['cod'] == '_3(83w)'), 'gram'] = 'MISS [player] Free Throw Flagrant 3 of 3'

    df.loc[(df['cod'] == '_4(0p)') & (df['ind_unk'] == 0), 'gram'] = '[player] REBOUND (Off:cnt Def:cnt)'
    df.loc[(df['cod'] == '_4(0t)') & (df['ind_unk'] == 0), 'gram'] = '[team] Rebound'
    df.loc[(df['cod'] == '_4(0u)') & (df['ind_unk'] == 1), 'gram'] = 'Unknown'
    df.loc[(df['cod'] == '_4(0u)') & (df['ind_unk'] == 0), 'gram'] = 'Normal Rebound'
    df.loc[(df['cod'] == '_4(1t)') & (df['ind_unk'] == 0), 'gram'] = '[team] Rebound'

    df.loc[(df['cod'] == '_5(00p)') & (df['ind_turn'] == '100000000000'), 'gram'] = '[player] No Turnover (Pcnt.Tcnt)'
    df.loc[(df['cod'] == '_5(00p)') & (df['ind_turn'] == '000000000000'), 'gram'] = '[player] Turnover Turnover (Pcnt.Tcnt)'
    df.loc[(df['cod'] == '_5(01p)') & (df['ind_turn'] == '100000001000'), 'gram'] = '[player] No Turnover (Pcnt.Tcnt) [player] STEAL (cnt STL)'
    df.loc[(df['cod'] == '_5(01p)') & (df['ind_turn'] == '000000001000'), 'gram'] = '[player] Turnover Turnover (Pcnt.Tcnt) [player] STEAL (cnt STL)'
    df.loc[(df['cod'] == '_5(11p)') & (df['ind_turn'] == '000000001000'), 'gram'] = '[player] Bad Pass Turnover (Pcnt.Tcnt) [player] STEAL (cnt STL)'
    df.loc[(df['cod'] == '_5(12p)') & (df['ind_turn'] == '000000000000'), 'gram'] = '[player] Traveling Turnover (Pcnt.Tcnt)'
    df.loc[(df['cod'] == '_5(13p)') & (df['ind_turn'] == '000000000000'), 'gram'] = '[player] Foul Turnover (Pcnt.Tcnt)'
    df.loc[(df['cod'] == '_5(13p)') & (df['ind_turn'] == '010000000000'), 'gram'] = '[player] Offensive Foul Turnover (Pcnt.Tcnt)'
    df.loc[(df['cod'] == '_5(14p)') & (df['ind_turn'] == '000000000000'), 'gram'] = '[player] Double Dribble Turnover (Pcnt.Tcnt)'
    df.loc[(df['cod'] == '_5(15p)') & (df['ind_turn'] == '000000000000'), 'gram'] = '[player] Discontinue Dribble Turnover (Pcnt.Tcnt)'
    df.loc[(df['cod'] == '_5(16p)') & (df['ind_turn'] == '001000000100'), 'gram'] = '[player] 3 Second Violation Turnover (Pcnt.Tcnt)'
    df.loc[(df['cod'] == '_5(16p)') & (df['ind_turn'] == '000000000100'), 'gram'] = '[team] Turnover: 3 Second Violation (T#cnt)'
    df.loc[(df['cod'] == '_5(17p)') & (df['ind_turn'] == '000000000000'), 'gram'] = '[player] Inbound Turnover (Pcnt.Tcnt)'
    df.loc[(df['cod'] == '_5(18p)') & (df['ind_turn'] == '000100000000'), 'gram'] = '[player] Backcourt Turnover (Pcnt.Tcnt)'
    df.loc[(df['cod'] == '_5(18p)') & (df['ind_turn'] == '000000000000'), 'gram'] = '[team] Turnover: Backcourt (T#cnt)'
    df.loc[(df['cod'] == '_5(19p)') & (df['ind_turn'] == '010000000000'), 'gram'] = '[player] Offensive Goaltending Turnover (Pcnt.Tcnt)'
    df.loc[(df['cod'] == '_5(20p)') & (df['ind_turn'] == '001000000000'), 'gram'] = '[player] Lane Violation Turnover (Pcnt.Tcnt)'
    df.loc[(df['cod'] == '_5(21p)') & (df['ind_turn'] == '001000000000'), 'gram'] = '[player] Jump Ball Violation Turnover (Pcnt.Tcnt)'
    df.loc[(df['cod'] == '_5(22p)') & (df['ind_turn'] == '001000000000'), 'gram'] = '[player] Kicked Ball Violation Turnover (Pcnt.Tcnt)'
    df.loc[(df['cod'] == '_5(23p)') & (df['ind_turn'] == '000000000000'), 'gram'] = '[player] Illegal Assist Turnover (Pcnt.Tcnt)'
    df.loc[(df['cod'] == '_5(24p)') & (df['ind_turn'] == '000000000000'), 'gram'] = '[player] Palming Turnover (Pcnt.Tcnt)'
    df.loc[(df['cod'] == '_5(25t)') & (df['ind_turn'] == '000000000000'), 'gram'] = '[team] Turnover: Inbound (T#cnt)'
    df.loc[(df['cod'] == '_5(26p)') & (df['ind_turn'] == '000000000000'), 'gram'] = '[player] Double Personal Turnover (Pcnt.Tcnt)'
    df.loc[(df['cod'] == '_5(27p)') & (df['ind_turn'] == '000000000000'), 'gram'] = '[player] Punched Ball Turnover (Pcnt.Tcnt)'
    df.loc[(df['cod'] == '_5(28p)') & (df['ind_turn'] == '000000000000'), 'gram'] = '[player] Swinging Elbows Turnover (Pcnt.Tcnt)'
    df.loc[(df['cod'] == '_5(29p)') & (df['ind_turn'] == '000000000000'), 'gram'] = '[player] Basket from Below Turnover (Pcnt.Tcnt)'
    df.loc[(df['cod'] == '_5(30p)') & (df['ind_turn'] == '000000000000'), 'gram'] = '[player] Illegal Screen Turnover (Pcnt.Tcnt)'
    df.loc[(df['cod'] == '_5(31p)') & (df['ind_turn'] == '000010100000'), 'gram'] = '[player] Step Out of Bounds Turnover (Pcnt.Tcnt)'
    df.loc[(df['cod'] == '_5(31p)') & (df['ind_turn'] == '001001100000'), 'gram'] = '[player] Player Out of Bounds Violation Turnover (Pcnt.Tcnt)'
    df.loc[(df['cod'] == '_5(31p)') & (df['ind_turn'] == '000000100000'), 'gram'] = '[player] Out Of Bounds Turnover (Pcnt.Tcnt)'
    df.loc[(df['cod'] == '_5(32p)') & (df['ind_turn'] == '000000100000'), 'gram'] = '[player] Out of Bounds Lost Ball Turnover (Pcnt.Tcnt)'
    df.loc[(df['cod'] == '_5(33p)') & (df['ind_turn'] == '000000000000'), 'gram'] = '[player] Lost Ball Turnover (Pcnt.Tcnt)'
    df.loc[(df['cod'] == '_5(34p)') & (df['ind_turn'] == '000000100001'), 'gram'] = '[player] Out of Bounds - Bad Pass Turnover Turnover (Pcnt.Tcnt)'
    df.loc[(df['cod'] == '_5(34p)') & (df['ind_turn'] == '000000000000'), 'gram'] = '[player] Bad Pass Turnover (Pcnt.Tcnt)'
    df.loc[(df['cod'] == '_5(35p)') & (df['ind_turn'] == '000000001000'), 'gram'] = '[player] Lost Ball Turnover (Pcnt.Tcnt) [player] STEAL (cnt STL)'
    df.loc[(df['cod'] == '_5(35p)') & (df['ind_turn'] == '000000011000'), 'gram'] = '[player] Poss Lost Ball Turnover (Pcnt.Tcnt) [player] STEAL (cnt STL)'
    df.loc[(df['cod'] == '_5(35p)') & (df['ind_turn'] == '000000101000'), 'gram'] = '[player] Out of Bounds Lost Ball Turnover (Pcnt.Tcnt) [player] STEAL (cnt STL)'
    df.loc[(df['cod'] == '_5(36t)') & (df['ind_turn'] == '000000000100'), 'gram'] = '[team] Turnover: 8 Second Violation (T#cnt)'
    df.loc[(df['cod'] == '_5(37t)') & (df['ind_turn'] == '000000000000'), 'gram'] = '[team] Turnover: Shot Clock (T#cnt)'
    df.loc[(df['cod'] == '_5(38t)') & (df['ind_turn'] == '000000000000'), 'gram'] = '[team] Turnover: 5 Second Inbound (T#cnt)'
    df.loc[(df['cod'] == '_5(38t)') & (df['ind_turn'] == '000000000100'), 'gram'] = '[team] Turnover: 5 Second Violation (T#cnt)'
    df.loc[(df['cod'] == '_5(39t)') & (df['ind_turn'] == '000000000010'), 'gram'] = 'Excess Timeout Turnover'
    df.loc[(df['cod'] == '_5(39t)') & (df['ind_turn'] == '000000000000'), 'gram'] = '[team] Turnover: (T#cnt)'
    df.loc[(df['cod'] == '_5(40t)') & (df['ind_turn'] == '000000000000'), 'gram'] = '[team] Turnover: Too Many Players (T#cnt)'
    df.loc[(df['cod'] == '_5(41p)') & (df['ind_turn'] == '001000000100'), 'gram'] = '[player] 5 Second Violation Turnover (Pcnt.Tcnt)'
    df.loc[(df['cod'] == '_5(43p)') & (df['ind_turn'] == '000000000000'), 'gram'] = '[player] Opposite Basket Turnover (Pcnt.Tcnt)'
    
    df.loc[(df['cod'] == '_6(01c)') & (df['ind_fouls '] == '11001000000000'), 'gram'] = '[coach] Foul:T.FOUL ([referee])'
    df.loc[(df['cod'] == '_6(01c)') & (df['ind_fouls '] == '11000000000000'), 'gram'] = '[coach] Foul:T.FOUL'
    df.loc[(df['cod'] == '_6(01c)') & (df['ind_fouls '] == '00100000000000'), 'gram'] = '[coach] Foul:Non-Unsportsmanlike'
    df.loc[(df['cod'] == '_6(01c)') & (df['ind_fouls '] == '00101000000000'), 'gram'] = '[coach] Foul:Non-Unsportsmanlike ([referee])'
    df.loc[(df['cod'] == '_6(01p)') & (df['ind_fouls '] == '01002000000100'), 'gram'] = '[player] T.FOUL (Pcnt.Tcnt) ([referee])'
    df.loc[(df['cod'] == '_6(01p)') & (df['ind_fouls '] == '01001000000100'), 'gram'] = '[player] T.FOUL (Pcnt.Tcnt)'
    df.loc[(df['cod'] == '_6(01p)') & (df['ind_fouls '] == '01012000000000'), 'gram'] = '[player] T.FOUL (Pcnt.PN) ([referee])'
    df.loc[(df['cod'] == '_6(01p)') & (df['ind_fouls '] == '01011000000000'), 'gram'] = '[player] T.FOUL (Pcnt.PN)'
    df.loc[(df['cod'] == '_6(01p)') & (df['ind_fouls '] == '00002100000100'), 'gram'] = '[player] HANGING.TECH.FOUL (Pcnt.Tcnt) ([referee])'
    df.loc[(df['cod'] == '_6(01p)') & (df['ind_fouls '] == '00001100000100'), 'gram'] = '[player] HANGING.TECH.FOUL (Pcnt.Tcnt)'
    df.loc[(df['cod'] == '_6(01p)') & (df['ind_fouls '] == '00011100000100'), 'gram'] = '[player] HANGING.TECH.FOUL (Pcnt.PN)'
    df.loc[(df['cod'] == '_6(01p)') & (df['ind_fouls '] == '00012100000100'), 'gram'] = '[player] HANGING.TECH.FOUL (Pcnt.PN) ([referee])'
    df.loc[(df['cod'] == '_6(01p)') & (df['ind_fouls '] == '00001010000100'), 'gram'] = '[player] Taunting (Pcnt.Tcnt)'
    df.loc[(df['cod'] == '_6(01p)') & (df['ind_fouls '] == '00002010000100'), 'gram'] = '[player] Taunting (Pcnt.Tcnt) ([referee])'
    df.loc[(df['cod'] == '_6(01p)') & (df['ind_fouls '] == '00011010000000'), 'gram'] = '[player] Taunting (Pcnt.PN)'
    df.loc[(df['cod'] == '_6(01p)') & (df['ind_fouls '] == '00012010000000'), 'gram'] = '[player] Taunting (Pcnt.PN) ([referee])'
    df.loc[(df['cod'] == '_6(01p)') & (df['ind_fouls '] == '00102000000100'), 'gram'] = '[player] Non-Unsportsmanlike (Pcnt.Tcnt) ([referee])'
    df.loc[(df['cod'] == '_6(01p)') & (df['ind_fouls '] == '00101000000100'), 'gram'] = '[player] Non-Unsportsmanlike (Pcnt.Tcnt)'
    df.loc[(df['cod'] == '_6(01p)') & (df['ind_fouls '] == '00112000000000'), 'gram'] = '[player] Non-Unsportsmanlike (Pcnt.PN) ([referee])'
    df.loc[(df['cod'] == '_6(01p)') & (df['ind_fouls '] == '00111000000000'), 'gram'] = '[player] Non-Unsportsmanlike (Pcnt.PN)'
    df.loc[(df['cod'] == '_6(01p)') & (df['ind_fouls '] == '00102000000101'), 'gram'] = '[player] Non-Unsportsmanlike Tech Foul - Flopping (Pcnt.Tcnt) ([referee])'
    df.loc[(df['cod'] == '_6(01p)') & (df['ind_fouls '] == '00112000000001'), 'gram'] = '[player] Non-Unsportsmanlike Tech Foul - Flopping (Pcnt.PN) ([referee])'
    df.loc[(df['cod'] == '_6(01t)') & (df['ind_fouls '] == '01000000000000'), 'gram'] = '[team] T.FOUL'
    df.loc[(df['cod'] == '_6(01t)') & (df['ind_fouls '] == '00000000000000'), 'gram'] = '[team] Foul'
    df.loc[(df['cod'] == '_6(01u)') & (df['ind_fouls '] == '00000000000000'), 'gram'] = 'Technical'
    df.loc[(df['cod'] == '_6(05p)') & (df['ind_fouls '] == '00001000000000'), 'gram'] = '[team] T.Foul (Def. 3 Sec [player] )'
    df.loc[(df['cod'] == '_6(05p)') & (df['ind_fouls '] == '00002000000000'), 'gram'] = '[team] T.Foul (Def. 3 Sec [player] ) ([referee])'
    df.loc[(df['cod'] == '_6(06t)') & (df['ind_fouls '] == '00000001000000'), 'gram'] = '[team] Delay'
    df.loc[(df['cod'] == '_6(06t)') & (df['ind_fouls '] == '00000000000000'), 'gram'] = '[team] Foul'
    df.loc[(df['cod'] == '_6(06u)') & (df['ind_fouls '] == '00000001000000'), 'gram'] = 'Delay Technical'
    df.loc[(df['cod'] == '_6(08t)') & (df['ind_fouls '] == '00000000000000'), 'gram'] = '[team] Excess Timeout Technical'
    df.loc[(df['cod'] == '_6(08u)') & (df['ind_fouls '] == '00000000000000'), 'gram'] = 'Excess Timeout Technical'
    df.loc[(df['cod'] == '_6(09t)') & (df['ind_fouls '] == '00000000000000'), 'gram'] = '[team] Too Many Players Tech Foul'
    df.loc[(df['cod'] == '_6(10a)') & (df['ind_fouls '] == '00002000000100'), 'gram'] = '[player] P.FOUL (Pcnt.Tcnt) ([referee])'
    df.loc[(df['cod'] == '_6(10a)') & (df['ind_fouls '] == '00001000000100'), 'gram'] = '[player] P.FOUL (Pcnt.Tcnt)'
    df.loc[(df['cod'] == '_6(10a)') & (df['ind_fouls '] == '00001000100100'), 'gram'] = '[player] Personal Block Foul (Pcnt.Tcnt)'
    df.loc[(df['cod'] == '_6(10a)') & (df['ind_fouls '] == '00002000100100'), 'gram'] = '[player] Personal Block Foul (Pcnt.Tcnt) ([referee])'
    df.loc[(df['cod'] == '_6(10a)') & (df['ind_fouls '] == '00000000000000'), 'gram'] = 'Personal'
    df.loc[(df['cod'] == '_6(10p)') & (df['ind_fouls '] == '00012000000000'), 'gram'] = '[player] P.FOUL (Pcnt.PN) ([referee])'
    df.loc[(df['cod'] == '_6(10p)') & (df['ind_fouls '] == '00011000000000'), 'gram'] = '[player] P.FOUL (Pcnt.PN)'
    df.loc[(df['cod'] == '_6(10p)') & (df['ind_fouls '] == '00011000100000'), 'gram'] = '[player] Personal Block Foul (Pcnt.PN)'
    df.loc[(df['cod'] == '_6(10p)') & (df['ind_fouls '] == '00012000100000'), 'gram'] = '[player] Personal Block Foul (Pcnt.PN) ([referee])'
    df.loc[(df['cod'] == '_6(11a)') & (df['ind_fouls '] == '00002000000100'), 'gram'] = '[player] S.FOUL (Pcnt.Tcnt) ([referee])'
    df.loc[(df['cod'] == '_6(11a)') & (df['ind_fouls '] == '00001000000100'), 'gram'] = '[player] S.FOUL (Pcnt.Tcnt)'
    df.loc[(df['cod'] == '_6(11a)') & (df['ind_fouls '] == '00001000100100'), 'gram'] = '[player] Shooting Block Foul (Pcnt.Tcnt)'
    df.loc[(df['cod'] == '_6(11a)') & (df['ind_fouls '] == '00002000100100'), 'gram'] = '[player] Shooting Block Foul (Pcnt.Tcnt) ([referee])'
    df.loc[(df['cod'] == '_6(11a)') & (df['ind_fouls '] == '00002000010100'), 'gram'] = '[player] S.FOUL (Pcnt.Tcnt) (.)'
    df.loc[(df['cod'] == '_6(11a)') & (df['ind_fouls '] == '00000000000000'), 'gram'] = 'Shooting'
    df.loc[(df['cod'] == '_6(11p)') & (df['ind_fouls '] == '00012000000000'), 'gram'] = '[player] S.FOUL (Pcnt.PN) ([referee])'
    df.loc[(df['cod'] == '_6(11p)') & (df['ind_fouls '] == '00011000000000'), 'gram'] = '[player] S.FOUL (Pcnt.PN)'
    df.loc[(df['cod'] == '_6(11p)') & (df['ind_fouls '] == '00011000100000'), 'gram'] = '[player] Shooting Block Foul (Pcnt.PN)'
    df.loc[(df['cod'] == '_6(11p)') & (df['ind_fouls '] == '00012000100000'), 'gram'] = '[player] Shooting Block Foul (Pcnt.PN) ([referee])'
    df.loc[(df['cod'] == '_6(11p)') & (df['ind_fouls '] == '00012000010000'), 'gram'] = '[player] S.FOUL (Pcnt.PN) (.)'
    df.loc[(df['cod'] == '_6(12a)') & (df['ind_fouls '] == '00002000000100'), 'gram'] = '[player] L.B.FOUL (Pcnt.Tcnt) ([referee])'
    df.loc[(df['cod'] == '_6(12a)') & (df['ind_fouls '] == '00001000000100'), 'gram'] = '[player] L.B.FOUL (Pcnt.Tcnt)'
    df.loc[(df['cod'] == '_6(12p)') & (df['ind_fouls '] == '00012000000000'), 'gram'] = '[player] L.B.FOUL (Pcnt.PN) ([referee])'
    df.loc[(df['cod'] == '_6(12p)') & (df['ind_fouls '] == '00011000000000'), 'gram'] = '[player] L.B.FOUL (Pcnt.PN)'
    df.loc[(df['cod'] == '_6(13a)') & (df['ind_fouls '] == '00002000000000'), 'gram'] = '[player] OFF.Foul (Pcnt) ([referee])'
    df.loc[(df['cod'] == '_6(13a)') & (df['ind_fouls '] == '00001000000000'), 'gram'] = '[player] OFF.Foul (Pcnt)'
    df.loc[(df['cod'] == '_6(13a)') & (df['ind_fouls '] == '00002000001100'), 'gram'] = '[player] Offensive Charge Foul (Pcnt.Tcnt) ([referee])'
    df.loc[(df['cod'] == '_6(13a)') & (df['ind_fouls '] == '00002000001000'), 'gram'] = '[player] Offensive Charge Foul (Pcnt) ([referee])'
    df.loc[(df['cod'] == '_6(13a)') & (df['ind_fouls '] == '00001000001100'), 'gram'] = '[player] Offensive Charge Foul (Pcnt.Tcnt)'
    df.loc[(df['cod'] == '_6(13a)') & (df['ind_fouls '] == '00012000001000'), 'gram'] = '[player] Offensive Charge Foul (Pcnt.PN) ([referee])'
    df.loc[(df['cod'] == '_6(13a)') & (df['ind_fouls '] == '00001000001000'), 'gram'] = '[player] Offensive Charge Foul (Pcnt)'
    df.loc[(df['cod'] == '_6(13a)') & (df['ind_fouls '] == '00011000001000'), 'gram'] = '[player] Offensive Charge Foul (Pcnt.PN)'
    df.loc[(df['cod'] == '_6(14a)') & (df['ind_fouls '] == '00001000000100'), 'gram'] = '[player] IN.FOUL (Pcnt.Tcnt)'
    df.loc[(df['cod'] == '_6(14a)') & (df['ind_fouls '] == '00002000000100'), 'gram'] = '[player] IN.FOUL (Pcnt.Tcnt) ([referee])'
    df.loc[(df['cod'] == '_6(14p)') & (df['ind_fouls '] == '00011000000000'), 'gram'] = '[player] IN.FOUL (Pcnt.PN)'
    df.loc[(df['cod'] == '_6(14p)') & (df['ind_fouls '] == '00012000000000'), 'gram'] = '[player] IN.FOUL (Pcnt.PN) ([referee])'
    df.loc[(df['cod'] == '_6(15a)') & (df['ind_fouls '] == '00002000000100'), 'gram'] = '[player] AWAY.FROM.PLAY.FOUL (Pcnt.Tcnt) ([referee])'
    df.loc[(df['cod'] == '_6(15a)') & (df['ind_fouls '] == '00001000000100'), 'gram'] = '[player] AWAY.FROM.PLAY.FOUL (Pcnt.Tcnt)'
    df.loc[(df['cod'] == '_6(15p)') & (df['ind_fouls '] == '00012000000000'), 'gram'] = '[player] AWAY.FROM.PLAY.FOUL (Pcnt.PN) ([referee])'
    df.loc[(df['cod'] == '_6(15p)') & (df['ind_fouls '] == '00011000000000'), 'gram'] = '[player] AWAY.FROM.PLAY.FOUL (Pcnt.PN)'
    df.loc[(df['cod'] == '_6(16a)') & (df['ind_fouls '] == '00001000000100'), 'gram'] = '[player] PUNCH.FOUL (Pcnt.Tcnt)'
    df.loc[(df['cod'] == '_6(16a)') & (df['ind_fouls '] == '00002000000100'), 'gram'] = '[player] PUNCH.FOUL (Pcnt.Tcnt) ([referee])'
    df.loc[(df['cod'] == '_6(17a)') & (df['ind_fouls '] == '00001000000100'), 'gram'] = '[player] C.P.FOUL (Pcnt.Tcnt)'
    df.loc[(df['cod'] == '_6(17a)') & (df['ind_fouls '] == '00002000000100'), 'gram'] = '[player] C.P.FOUL (Pcnt.Tcnt) ([referee])'
    df.loc[(df['cod'] == '_6(17p)') & (df['ind_fouls '] == '00011000000000'), 'gram'] = '[player] C.P.FOUL (Pcnt.PN)'
    df.loc[(df['cod'] == '_6(17p)') & (df['ind_fouls '] == '00012000000000'), 'gram'] = '[player] C.P.FOUL (Pcnt.PN) ([referee])'
    df.loc[(df['cod'] == '_6(18a)') & (df['ind_fouls '] == '00002000000001'), 'gram'] = 'Foul : Double Personal - [player] (cnt PF), [player] (cnt PF)'
    df.loc[(df['cod'] == '_6(18a)') & (df['ind_fouls '] == '00003000000001'), 'gram'] = 'Foul : Double Personal - [player] (cnt PF), [player] (cnt PF) ([referee])'
    df.loc[(df['cod'] == '_6(18a)') & (df['ind_fouls '] == '00000000000000'), 'gram'] = 'Double Personal'
    df.loc[(df['cod'] == '_6(19a)') & (df['ind_fouls '] == '01002000000100'), 'gram'] = '[player] FLAGRANT.FOUL.TYPE1 (Pcnt.Tcnt) ([referee])'
    df.loc[(df['cod'] == '_6(19a)') & (df['ind_fouls '] == '01001000000100'), 'gram'] = '[player] FLAGRANT.FOUL.TYPE1 (Pcnt.Tcnt)'
    df.loc[(df['cod'] == '_6(19p)') & (df['ind_fouls '] == '01012000000100'), 'gram'] = '[player] FLAGRANT.FOUL.TYPE1 (Pcnt.PN) ([referee])'
    df.loc[(df['cod'] == '_6(19p)') & (df['ind_fouls '] == '01011000000100'), 'gram'] = '[player] FLAGRANT.FOUL.TYPE1 (Pcnt.PN)'
    df.loc[(df['cod'] == '_6(20a)') & (df['ind_fouls '] == '01002000000100'), 'gram'] = '[player] FLAGRANT.FOUL.TYPE2 (Pcnt.Tcnt) ([referee])'
    df.loc[(df['cod'] == '_6(20a)') & (df['ind_fouls '] == '01001000000100'), 'gram'] = '[player] FLAGRANT.FOUL.TYPE2 (Pcnt.Tcnt)'
    df.loc[(df['cod'] == '_6(20p)') & (df['ind_fouls '] == '01011000000100'), 'gram'] = '[player] FLAGRANT.FOUL.TYPE2 (Pcnt.PN)'
    df.loc[(df['cod'] == '_6(20p)') & (df['ind_fouls '] == '01012000000100'), 'gram'] = '[player] FLAGRANT.FOUL.TYPE2 (Pcnt.PN) ([referee])'
    df.loc[(df['cod'] == '_6(21a)') & (df['ind_fouls '] == '00002000000100'), 'gram'] = '[player] Personal Take Foul (Pcnt.Tcnt) ([referee])'
    df.loc[(df['cod'] == '_6(21a)') & (df['ind_fouls '] == '00001000000100'), 'gram'] = '[player] Personal Take Foul (Pcnt.Tcnt)'
    df.loc[(df['cod'] == '_6(21p)') & (df['ind_fouls '] == '00012000000000'), 'gram'] = '[player] Personal Take Foul (Pcnt.PN) ([referee])'
    df.loc[(df['cod'] == '_6(21p)') & (df['ind_fouls '] == '00011000000000'), 'gram'] = '[player] Personal Take Foul (Pcnt.PN)'
    df.loc[(df['cod'] == '_6(22a)') & (df['ind_fouls '] == '00002000000100'), 'gram'] = '[player] Transition Take Foul (Pcnt.Tcnt) ([referee])'
    df.loc[(df['cod'] == '_6(22p)') & (df['ind_fouls '] == '00012000000000'), 'gram'] = '[player] Transition Take Foul (Pcnt.PN) ([referee])'
    df.loc[(df['cod'] == '_6(4cc)') & (df['ind_fouls '] == '00001000000110'), 'gram'] = '[coach] Foul:DOUBLE.TECHNICAL.FOUL ([referee])'
    df.loc[(df['cod'] == '_6(4pc)') & (df['ind_fouls '] == '00001000000001'), 'gram'] = 'Double Technical - [player], [coach] ([referee])'
    df.loc[(df['cod'] == '_6(4pc)') & (df['ind_fouls '] == '00000000000110'), 'gram'] = '[coach] Foul:DOUBLE.TECHNICAL.FOUL'
    df.loc[(df['cod'] == '_6(4pc)') & (df['ind_fouls '] == '00001000000110'), 'gram'] = '[coach] Foul:DOUBLE.TECHNICAL.FOUL ([referee])'
    df.loc[(df['cod'] == '_6(4pp)') & (df['ind_fouls '] == '00001000000001'), 'gram'] = 'Double Technical - [player], [player] ([referee])'
    df.loc[(df['cod'] == '_6(4pp)') & (df['ind_fouls '] == '00000000000001'), 'gram'] = 'Double Technical - [player], [player]'
    df.loc[(df['cod'] == '_6(4pp)') & (df['ind_fouls '] == '00000000000000'), 'gram'] = 'Double Technical'    

    df.loc[(df['cod'] == '_7(0p)') & (df['ind_violation'] == '1100'), 'gram'] = '[player] Violation:No Violation'
    df.loc[(df['cod'] == '_7(1t)') & (df['ind_violation'] == '0100'), 'gram'] = '[team] Violation: Delay of game Violation'
    df.loc[(df['cod'] == '_7(1t)') & (df['ind_violation'] == '0000'), 'gram'] = 'Delay Of Game'
    df.loc[(df['cod'] == '_7(2p)') & (df['ind_violation'] == '0110'), 'gram'] = '[player] Violation:Defensive Goaltending ([referee])'
    df.loc[(df['cod'] == '_7(2p)') & (df['ind_violation'] == '0100'), 'gram'] = '[player] Violation:Defensive Goaltending'
    df.loc[(df['cod'] == '_7(3p)') & (df['ind_violation'] == '0100'), 'gram'] = '[player] Violation:Lane'
    df.loc[(df['cod'] == '_7(3p)') & (df['ind_violation'] == '0000'), 'gram'] = 'Lane'
    df.loc[(df['cod'] == '_7(3p)') & (df['ind_violation'] == '0110'), 'gram'] = '[player] Violation:Lane ([referee])'
    df.loc[(df['cod'] == '_7(4p)') & (df['ind_violation'] == '0100'), 'gram'] = '[player] Violation:Jump Ball'
    df.loc[(df['cod'] == '_7(4p)') & (df['ind_violation'] == '0110'), 'gram'] = '[player] Violation:Jump Ball ([referee])'
    df.loc[(df['cod'] == '_7(5p)') & (df['ind_violation'] == '0110'), 'gram'] = '[player] Violation:Kicked Ball ([referee])'
    df.loc[(df['cod'] == '_7(5p)') & (df['ind_violation'] == '0100'), 'gram'] = '[player] Violation:Kicked Ball'
    df.loc[(df['cod'] == '_7(5p)') & (df['ind_violation'] == '0111'), 'gram'] = '[player] Violation:Kicked Ball (.)'
    df.loc[(df['cod'] == '_7(6p)') & (df['ind_violation'] == '0000'), 'gram'] = 'Double Lane'
    df.loc[(df['cod'] == '_7(6p)') & (df['ind_violation'] == '0100'), 'gram'] = '[player] Violation:Double Lane'
    df.loc[(df['cod'] == '_7(6p)') & (df['ind_violation'] == '0110'), 'gram'] = '[player] Violation:Double Lane ([referee])'
    df.loc[(df['cod'] == '_7(6p)') & (df['ind_violation'] == '0111'), 'gram'] = '[player] Violation:Double Lane (.)'

    df.loc[(df['cod'] == '_8(0)'), 'gram'] = 'SUB: [player] FOR [player]'

    df.loc[(df['cod'] == '_9(0)') & (df['ind_timeout'] == '1011010'), 'gram'] = '[team] Timeout: Regular (Full cnt Short cnt)'
    df.loc[(df['cod'] == '_9(0)') & (df['ind_timeout'] == '0111010'), 'gram'] = '[team] Timeout: Regular (Reg.cnt Short cnt)'
    df.loc[(df['cod'] == '_9(0)') & (df['ind_timeout'] == '0101010'), 'gram'] = '[team] Timeout: Short (Reg.cnt Short cnt)'
    df.loc[(df['cod'] == '_9(0)') & (df['ind_timeout'] == '1001010'), 'gram'] = '[team] Timeout: Short (Full cnt Short cnt)'
    df.loc[(df['cod'] == '_9(0)') & (df['ind_timeout'] == '0010010'), 'gram'] = 'Timeout: Regular'
    df.loc[(df['cod'] == '_9(0)') & (df['ind_timeout'] == '0010000'), 'gram'] = 'Regular'
    df.loc[(df['cod'] == '_9(0)') & (df['ind_timeout'] == '0001110'), 'gram'] = 'Timeout: Short'
    df.loc[(df['cod'] == '_9(1)') & (df['ind_timeout'] == '1001010'), 'gram'] = '[team] Timeout: Coach Challenge (Full cnt Short cnt)'
    df.loc[(df['cod'] == '_9(1)') & (df['ind_timeout'] == '0101010'), 'gram'] = '[team] Timeout: Coach Challenge (Reg.cnt Short cnt)'
    df.loc[(df['cod'] == '_9(2)') & (df['ind_timeout'] == '0000010'), 'gram'] = 'Timeout: Official'
    df.loc[(df['cod'] == '_9(2)') & (df['ind_timeout'] == '0101010'), 'gram'] = '[team] Timeout: Official (Reg.cnt Short cnt)'
    df.loc[(df['cod'] == '_9(2)') & (df['ind_timeout'] == '1001010'), 'gram'] = '[team] Timeout: Official (Full cnt Short cnt)'
    df.loc[(df['cod'] == '_9(3)') & (df['ind_timeout'] == '0000011'), 'gram'] = 'Timeout: No Timeout'
    df.loc[(df['cod'] == '_9(3)') & (df['ind_timeout'] == '1001011'), 'gram'] = '[team] Timeout: No Timeout (Full cnt Short cnt)'
    df.loc[(df['cod'] == '_9(3)') & (df['ind_timeout'] == '0101011'), 'gram'] = '[team] Timeout: No Timeout (Reg.cnt Short cnt)'
    df.loc[(df['cod'] == '_9(3)') & (df['ind_timeout'] == '1001010'), 'gram'] = '[team] Timeout: Timeout (Full cnt Short cnt)'
    df.loc[(df['cod'] == '_9(3)') & (df['ind_timeout'] == '0101010'), 'gram'] = '[team] Timeout: Timeout (Reg.cnt Short cnt)'
                  
    df.loc[(df['cod'] == '_10(0)') & (df['ind_jumpball'] == '10'), 'gram'] = 'Jump Ball [player] vs. [player]: Tip to [player]'
    df.loc[(df['cod'] == '_10(0)') & (df['ind_jumpball'] == '00'), 'gram'] = 'Jump Ball [player] vs. [player]: Tip to'
    df.loc[(df['cod'] == '_10(1)') & (df['ind_jumpball'] == '10'), 'gram'] = 'Jump Ball [player] vs. [player]: Tip to [player]'
    df.loc[(df['cod'] == '_10(1)') & (df['ind_jumpball'] == '00'), 'gram'] = 'Jump Ball [player] vs. [player]: Tip to'
    df.loc[(df['cod'] == '_10(2)') & (df['ind_jumpball'] == '11'), 'gram'] = 'Jump Ball (CC) [player] vs. [player]: Tip to [player]'
    df.loc[(df['cod'] == '_10(2)') & (df['ind_jumpball'] == '00'), 'gram'] = 'Coach Challenge'

    df.loc[(df['cod'] == '_11(0)') & (df['ind_ejection'] == '10000'), 'gram'] = '[player] Ejection:Other'
    df.loc[(df['cod'] == '_11(0)') & (df['ind_ejection'] == '01000'), 'gram'] = '[player] Ejection:Second Technical'
    df.loc[(df['cod'] == '_11(0)') & (df['ind_ejection'] == '00001'), 'gram'] = '[player] Ejection:First Flagrant Type 2'
    df.loc[(df['cod'] == '_11(0)') & (df['ind_ejection'] == '00100'), 'gram'] = '[player] Ejection:No Ejection'
    df.loc[(df['cod'] == '_11(0)') & (df['ind_ejection'] == '00010'), 'gram'] = '[player] Ejection:Second Flagrant Type 1'
    df.loc[(df['cod'] == '_11(0)') & (df['ind_ejection'] == '00000'), 'gram'] = '[player] Ejection:'
    df.loc[(df['cod'] == '_11(1)') & (df['ind_ejection'] == '01000'), 'gram'] = '[coach] Ejection:Second Technical'
    df.loc[(df['cod'] == '_11(1)') & (df['ind_ejection'] == '10000'), 'gram'] = '[coach] Ejection:Other'
    df.loc[(df['cod'] == '_11(1)') & (df['ind_ejection'] == '00100'), 'gram'] = '[coach] Ejection:No Ejection'

    df.loc[(df['cod'] == '_12(0)') & (df['ind_period'] == '10001'), 'gram'] = 'Start of 1st OT ([hour])'
    df.loc[(df['cod'] == '_12(0)') & (df['ind_period'] == '01001'), 'gram'] = 'Start of 2nd OT ([hour])'
    df.loc[(df['cod'] == '_12(0)') & (df['ind_period'] == '00101'), 'gram'] = 'Start of 3rd OT ([hour])'
    df.loc[(df['cod'] == '_12(0)') & (df['ind_period'] == '00011'), 'gram'] = 'Start of 4th OT ([hour])'
    df.loc[(df['cod'] == '_12(1)') & (df['ind_period'] == '10000'), 'gram'] = 'Start of 1st Period ([hour])'
    df.loc[(df['cod'] == '_12(2)') & (df['ind_period'] == '01000'), 'gram'] = 'Start of 2nd Period ([hour])'
    df.loc[(df['cod'] == '_12(3)') & (df['ind_period'] == '00100'), 'gram'] = 'Start of 3rd Period ([hour])'
    df.loc[(df['cod'] == '_12(4)') & (df['ind_period'] == '00010'), 'gram'] = 'Start of 4th Period ([hour])'

    df.loc[(df['cod'] == '_13(0)') & (df['ind_period'] == '10001'), 'gram'] = 'End of 1st OT ([hour])'
    df.loc[(df['cod'] == '_13(0)') & (df['ind_period'] == '01001'), 'gram'] = 'End of 2nd OT ([hour])'
    df.loc[(df['cod'] == '_13(0)') & (df['ind_period'] == '00101'), 'gram'] = 'End of 3rd OT ([hour])'
    df.loc[(df['cod'] == '_13(0)') & (df['ind_period'] == '00011'), 'gram'] = 'End of 4th OT ([hour])'
    df.loc[(df['cod'] == '_13(1)') & (df['ind_period'] == '10000'), 'gram'] = 'End of 1st Period ([hour])'
    df.loc[(df['cod'] == '_13(2)') & (df['ind_period'] == '01000'), 'gram'] = 'End of 2nd Period ([hour])'
    df.loc[(df['cod'] == '_13(3)') & (df['ind_period'] == '00100'), 'gram'] = 'End of 3rd Period ([hour])'
    df.loc[(df['cod'] == '_13(4)') & (df['ind_period'] == '00010'), 'gram'] = 'End of 4th Period ([hour])'

    df.loc[(df['cod'] == '_18(0)') & (df['ind_period'] == '00000'), 'gram'] = 'Support Ruling'
    df.loc[(df['cod'] == '_18(0)') & (df['ind_period'] == '00010'), 'gram'] = 'Instant Replay4th Period ([hour])'
    df.loc[(df['cod'] == '_18(0)') & (df['ind_period'] == '00100'), 'gram'] = 'Instant Replay3rd Period ([hour])'
    df.loc[(df['cod'] == '_18(0)') & (df['ind_period'] == '01000'), 'gram'] = 'Instant Replay2nd Period ([hour])'
    df.loc[(df['cod'] == '_18(0)') & (df['ind_period'] == '10000'), 'gram'] = 'Instant Replay1st Period ([hour])'
    df.loc[(df['cod'] == '_18(0)') & (df['ind_period'] == '10001'), 'gram'] = 'Instant Replay1st OT ([hour])'
    df.loc[(df['cod'] == '_18(0)') & (df['ind_period'] == '01001'), 'gram'] = 'Instant Replay2nd OT ([hour])'
    df.loc[(df['cod'] == '_18(0)') & (df['ind_period'] == '00101'), 'gram'] = 'Instant Replay3rd OT ([hour])'
    df.loc[(df['cod'] == '_18(1)') & (df['ind_period'] == '00000'), 'gram'] = 'Overturn Ruling'
    df.loc[(df['cod'] == '_18(1)') & (df['ind_period'] == '00010'), 'gram'] = 'Instant Replay4th Period ([hour])'
    df.loc[(df['cod'] == '_18(1)') & (df['ind_period'] == '00100'), 'gram'] = 'Instant Replay3rd Period ([hour])'
    df.loc[(df['cod'] == '_18(1)') & (df['ind_period'] == '01000'), 'gram'] = 'Instant Replay2nd Period ([hour])'
    df.loc[(df['cod'] == '_18(1)') & (df['ind_period'] == '10000'), 'gram'] = 'Instant Replay1st Period ([hour])'
    df.loc[(df['cod'] == '_18(1)') & (df['ind_period'] == '10001'), 'gram'] = 'Instant Replay1st OT ([hour])'
    df.loc[(df['cod'] == '_18(1)') & (df['ind_period'] == '01001'), 'gram'] = 'Instant Replay2nd OT ([hour])'
    df.loc[(df['cod'] == '_18(1)') & (df['ind_period'] == '00101'), 'gram'] = 'Instant Replay3rd OT ([hour])'
    df.loc[(df['cod'] == '_18(2)') & (df['ind_period'] == '00000'), 'gram'] = 'Altercation Ruling'
    df.loc[(df['cod'] == '_18(2)') & (df['ind_period'] == '10000'), 'gram'] = 'Instant Replay1st Period ([hour])'
    df.loc[(df['cod'] == '_18(2)') & (df['ind_period'] == '01000'), 'gram'] = 'Instant Replay2nd Period ([hour])'
    df.loc[(df['cod'] == '_18(2)') & (df['ind_period'] == '00010'), 'gram'] = 'Instant Replay4th Period ([hour])'
    df.loc[(df['cod'] == '_18(2)') & (df['ind_period'] == '00100'), 'gram'] = 'Instant Replay3rd Period ([hour])'
    df.loc[(df['cod'] == '_18(2)') & (df['ind_period'] == '10001'), 'gram'] = 'Instant Replay1st OT ([hour])'
    df.loc[(df['cod'] == '_18(2)') & (df['ind_period'] == '01001'), 'gram'] = 'Instant Replay2nd OT ([hour])'
    df.loc[(df['cod'] == '_18(3)') & (df['ind_period'] == '10000'), 'gram'] = 'Instant Replay1st Period ([hour])'
    df.loc[(df['cod'] == '_18(3)') & (df['ind_period'] == '01000'), 'gram'] = 'Instant Replay2nd Period ([hour])'
    df.loc[(df['cod'] == '_18(3)') & (df['ind_period'] == '00100'), 'gram'] = 'Instant Replay3rd Period ([hour])'
    df.loc[(df['cod'] == '_18(3)') & (df['ind_period'] == '00010'), 'gram'] = 'Instant Replay4th Period ([hour])'
    df.loc[(df['cod'] == '_18(3)') & (df['ind_period'] == '10001'), 'gram'] = 'Instant Replay1st OT ([hour])'
    df.loc[(df['cod'] == '_18(3)') & (df['ind_period'] == '01001'), 'gram'] = 'Instant Replay2nd OT ([hour])'
    df.loc[(df['cod'] == '_18(4)') & (df['ind_period'] == '10000'), 'gram'] = 'Instant Replay1st Period ([hour])'
    df.loc[(df['cod'] == '_18(4)') & (df['ind_period'] == '01000'), 'gram'] = 'Instant Replay2nd Period ([hour])'
    df.loc[(df['cod'] == '_18(4)') & (df['ind_period'] == '00100'), 'gram'] = 'Instant Replay3rd Period ([hour])'
    df.loc[(df['cod'] == '_18(4)') & (df['ind_period'] == '00010'), 'gram'] = 'Instant Replay4th Period ([hour])'
    df.loc[(df['cod'] == '_18(4)') & (df['ind_period'] == '10001'), 'gram'] = 'Instant Replay1st OT ([hour])'
    df.loc[(df['cod'] == '_18(5)') & (df['ind_period'] == '00000'), 'gram'] = 'Coach Challenge Overturn Ruling'
    df.loc[(df['cod'] == '_18(5)') & (df['ind_period'] == '10000'), 'gram'] = 'Instant Replay1st Period ([hour])'
    df.loc[(df['cod'] == '_18(5)') & (df['ind_period'] == '01000'), 'gram'] = 'Instant Replay2nd Period ([hour])'
    df.loc[(df['cod'] == '_18(5)') & (df['ind_period'] == '00100'), 'gram'] = 'Instant Replay3rd Period ([hour])'
    df.loc[(df['cod'] == '_18(5)') & (df['ind_period'] == '00010'), 'gram'] = 'Instant Replay4th Period ([hour])'
    df.loc[(df['cod'] == '_18(5)') & (df['ind_period'] == '10001'), 'gram'] = 'Instant Replay1st OT ([hour])'
    df.loc[(df['cod'] == '_18(5)') & (df['ind_period'] == '01001'), 'gram'] = 'Instant Replay2nd OT ([hour])'
    df.loc[(df['cod'] == '_18(6)') & (df['ind_period'] == '10000'), 'gram'] = 'Instant Replay1st Period ([hour])'
    df.loc[(df['cod'] == '_18(6)') & (df['ind_period'] == '01000'), 'gram'] = 'Instant Replay2nd Period ([hour])'
    df.loc[(df['cod'] == '_18(6)') & (df['ind_period'] == '00100'), 'gram'] = 'Instant Replay3rd Period ([hour])'
    df.loc[(df['cod'] == '_18(6)') & (df['ind_period'] == '00010'), 'gram'] = 'Instant Replay4th Period ([hour])'
    df.loc[(df['cod'] == '_18(6)') & (df['ind_period'] == '10001'), 'gram'] = 'Instant Replay1st OT ([hour])'
    df.loc[(df['cod'] == '_18(6)') & (df['ind_period'] == '01001'), 'gram'] = 'Instant Replay2nd OT ([hour])'
    df.loc[(df['cod'] == '_18(6)') & (df['ind_period'] == '00101'), 'gram'] = 'Instant Replay3rd OT ([hour])'
    df.loc[(df['cod'] == '_18(7)') & (df['ind_period'] == '10000'), 'gram'] = 'Instant Replay1st Period ([hour])'
    df.loc[(df['cod'] == '_18(7)') & (df['ind_period'] == '01000'), 'gram'] = 'Instant Replay2nd Period ([hour])'
    df.loc[(df['cod'] == '_18(7)') & (df['ind_period'] == '00100'), 'gram'] = 'Instant Replay3rd Period ([hour])'
    df.loc[(df['cod'] == '_18(7)') & (df['ind_period'] == '00010'), 'gram'] = 'Instant Replay4th Period ([hour])'
    df.loc[(df['cod'] == '_18(7)') & (df['ind_period'] == '10001'), 'gram'] = 'Instant Replay1st OT ([hour])'

    df.loc[(df['cod'] == "_5(01p)") & (df['gram'].isna()), 'gram'] = "[player] No Turnover (Pcnt.Tcnt) [player] STEAL (cnt STL)"
    df.loc[(df['cod'] == "_5(12p)") & (df['gram'].isna()), 'gram'] = "[player] Traveling Turnover (Pcnt.Tcnt)"
    df.loc[(df['cod'] == "_5(13p)") & (df['gram'].isna()), 'gram'] = "[player] Foul Turnover (Pcnt.Tcnt)"
    df.loc[(df['cod'] == "_5(20p)") & (df['gram'].isna()), 'gram'] = "[player] Lane Violation Turnover (Pcnt.Tcnt)"
    df.loc[(df['cod'] == "_5(21p)") & (df['gram'].isna()), 'gram'] = "[player] Jump Ball Violation Turnover (Pcnt.Tcnt)"
    df.loc[(df['cod'] == "_5(22p)") & (df['gram'].isna()), 'gram'] = "[player] Kicked Ball Violation Turnover (Pcnt.Tcnt)"
    df.loc[(df['cod'] == "_5(31p)") & (df['gram'].isna()), 'gram'] = "[player] Step Out of Bounds Turnover (Pcnt.Tcnt)"
    df.loc[(df['cod'] == "_5(36t)") & (df['gram'].isna()), 'gram'] = "[team] Turnover: 8 Second Violation (T#cnt)"
    df.loc[(df['cod'] == "_6(01c)") & (df['gram'].isna()), 'gram'] = "[coach] Foul:T.FOUL ([referee])"

    df.loc[(df['cod'] == "_6(01p)") & (df['gram'].isna()), 'gram'] = "[player] T.FOUL (Pcnt.PN) ([referee])"
    df.loc[(df['cod'] == "_6(05p)") & (df['gram'].isna()), 'gram'] = "[team] T.Foul (Def. 3 Sec [player] ) ([referee])"
    df.loc[(df['cod'] == "_6(10p)") & (df['gram'].isna()), 'gram'] = "[player] P.FOUL (Pcnt.PN) ([referee])"
    df.loc[(df['cod'] == "_6(11p)") & (df['gram'].isna()), 'gram'] = "[player] S.FOUL (Pcnt.PN) ([referee])"
    df.loc[(df['cod'] == "_6(12p)") & (df['gram'].isna()), 'gram'] = "[player] L.B.FOUL (Pcnt.PN) ([referee])"
    df.loc[(df['cod'] == "_6(13a)") & (df['gram'].isna()), 'gram'] = "[player] OFF.Foul (Pcnt) ([referee])"
    df.loc[(df['cod'] == "_6(14p)") & (df['gram'].isna()), 'gram'] = "[player] IN.FOUL (Pcnt.PN) ([referee])"
    df.loc[(df['cod'] == "_6(15p)") & (df['gram'].isna()), 'gram'] = "[player] AWAY.FROM.PLAY.FOUL (Pcnt.PN) ([referee])"
    df.loc[(df['cod'] == "_6(17p)") & (df['gram'].isna()), 'gram'] = "[player] C.P.FOUL (Pcnt.PN) ([referee])"
    df.loc[(df['cod'] == "_6(18a)") & (df['gram'].isna()), 'gram'] = "Foul : Double Personal - [player] (cnt PF), [player] (cnt PF) ([referee])"
    df.loc[(df['cod'] == "_6(21p)") & (df['gram'].isna()), 'gram'] = "[player] Personal Take Foul (Pcnt.PN) ([referee])"
    df.loc[(df['cod'] == "_6(22p)") & (df['gram'].isna()), 'gram'] = "[player] Transition Take Foul (Pcnt.PN) ([referee])"
    df.loc[(df['cod'] == "_6(4pp)") & (df['gram'].isna()), 'gram'] = "Double Technical - [player], [player] ([referee])"
    df.loc[(df['cod'] == "_7(0p)") & (df['gram'].isna()), 'gram'] = "[player] Violation:No Violation"
    df.loc[(df['cod'] == "_7(2p)") & (df['gram'].isna()), 'gram'] = "[player] Violation:Defensive Goaltending ([referee])"
    df.loc[(df['cod'] == "_7(4p)") & (df['gram'].isna()), 'gram'] = "[player] Violation:Jump Ball ([referee])"
    df.loc[(df['cod'] == "_7(5p)") & (df['gram'].isna()), 'gram'] = "[player] Violation:Kicked Ball ([referee])"
    df.loc[(df['cod'] == "_9(0)") & (df['gram'].isna()), 'gram'] = "Timeout: Short"
    df.loc[(df['cod'] == "_9(2)") & (df['gram'].isna()), 'gram'] = "Timeout: Official"
    df.loc[(df['cod'] == "_9(3)") & (df['gram'].isna()), 'gram'] = "Timeout: No Timeout"

    columns_to_drop = ['ind_dist', 'ind_no', 'ind_unk', 'ind_off', 'ind_viot', 'ind_ba', 'ind_step', 'ind_play',
                    'ind_out', 'ind_pos', 'ind_ste', 'ind_sec', 'ind_exc', 'ind_trace', 'ind_turn', 'ind_tfoul',
                    'ind_tfoul2', 'ind_uns', 'ind_pn', 'cnt_par', 'ind_hang', 'ind_tau', 'ind_delay', 'ind_block',
                    'ind_dot', 'ind_charge', 'ind_tcnt', 'ind_double', 'ind_trace', 'ind_fouls', 'ind_vio', 
                    'ind_violation', 'ind_full', 'ind_reg', 'ind_reg2', 'ind_short', 'ind_short2', 'ind_time',
                    'ind_timeout', 'ind_CC', 'ind_tipto_', 'ind_jumpball', 'ind_other', 'ind_2ndtech', 'ind_type1',
                    'ind_type2', 'ind_ejection', 'ind_1st', 'ind_2nd', 'ind_3rd', 'ind_4th', 'ind_OT', 'ind_period']

    # Dropping selected columns
    df = df.drop(columns=columns_to_drop)

    # Define the conditions
    condition_1 = df['cod'].str.contains(r"_[1-2]\([1-5]\)")
    condition_2 = df['gram'].str.contains(r"\[dist\]")

    # Apply the function to create the 'shot' column
    df['shot'] = df.apply(extract_shot, axis = 1)
    # Using loc to perform replacements
    df.loc[df['cod'].isin(["_1(1)", "_1(1a)", "_2(1)", "_2(1b)"]), 'gram'] = df.loc[df['cod'].isin(["_1(1)", "_1(1a)", "_2(1)", "_2(1b)"]), 'gram'].str.replace(df['shot'], "[DUNK SHOT]")
    df.loc[df['cod'].isin(["_1(2)", "_1(2a)", "_2(2)", "_2(2b)"]), 'gram'] = df.loc[df['cod'].isin(["_1(2)", "_1(2a)", "_2(2)", "_2(2b)"]), 'gram'].str.replace(df['shot'], "[HOOK SHOT]")
    df.loc[df['cod'].isin(["_1(3)", "_1(3a)", "_2(3)", "_2(3b)"]), 'gram'] = df.loc[df['cod'].isin(["_1(3)", "_1(3a)", "_2(3)", "_2(3b)"]), 'gram'].str.replace(df['shot'], "[LAYUP SHOT]")
    df.loc[df['cod'].isin(["_1(4)", "_1(4a)", "_2(4)", "_2(4b)"]), 'gram'] = df.loc[df['cod'].isin(["_1(4)", "_1(4a)", "_2(4)", "_2(4b)"]), 'gram'].str.replace(df['shot'], "[JUMP SHOT]")
    df.loc[df['cod'].isin(["_1(5)", "_1(5a)", "_2(5)", "_2(5b)"]), 'gram'] = df.loc[df['cod'].isin(["_1(5)", "_1(5a)", "_2(5)", "_2(5b)"]), 'gram'].str.replace(df['shot'], "[3PT JUMP SHOT]")
    
    return df


FOLDER_PROCESSED_DATA = "D:/Mestrado/NBA/nba/data/processed/"

pbptotal = pd.read_csv(FOLDER_PROCESSED_DATA + "pbp_python.csv", sep = ';', dtype = 'str')
numeric_cols = ['num_event','action_id','period','tp_event','tp_subevent','shot_distance','xlegacy','ylegacy']
pbptotal[numeric_cols] = pbptotal[numeric_cols].apply(pd.to_numeric, errors = 'coerce')

grouped = pbptotal.groupby('season')

# Apply build_df_pbp_plays to each group and put results in a list
df_list_1 = [build_df_pbp_plays(group) for _, group in grouped]

# Apply put_grammar_in_pbp to each DataFrame in the list and put results in another list
df_list_2 = [put_grammar_in_pbp(df) for df in df_list_1]

# Concatenate DataFrames in the list
result_df = pd.concat(df_list_2, ignore_index=True)

# Drop columns 'p', 'ind', 'tp'
result_df = result_df.drop(columns=['p', 'ind', 'tp'])

df = [group for _, group in grouped][0]
df = pbptotal
