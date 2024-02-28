import pandas as pd
import re

FOLDER_PROCESSED_DATA = "D:/Mestrado/NBA/nba/data/processed/"

def concatenate_pbp_sources(df_api, df_crawler):
    # Perform left join on specified columns
    merged_df = pd.merge(df_api, df_crawler, 
                         left_on = ["SEASON", "GAME_ID", "EVENTNUM", "PERIOD"], 
                         right_on = ["season", "game_id", "actionNumber", "period"], 
                         how = "left")
    
    # Select desired columns
    merged_df = merged_df[['SEASON', 'GAME_ID', 'EVENTNUM', 'actionId', 'EVENTMSGTYPE',
                           'actionType', 'EVENTMSGACTIONTYPE', 'subType', 'PERIOD',
                           'clock', 'DESCRIPTION', 'persons', 'location',
                           'shotDistance', 'xLegacy', 'yLegacy']]
    
    # Clean clock column
    merged_df['clock'] = merged_df['clock']\
        .str.replace("^PT", "", regex = True)\
        .str.replace("M", ":", regex = True)\
        .str.replace("S$", "", regex = True)\
        .str.replace(".", ":", regex = True)
    
    # Replace NaN values in location column with 'n'
    merged_df['location'] = merged_df['location'].fillna('n')
    
    # Rename columns
    merged_df = merged_df.rename(columns={'SEASON': 'season', 'GAME_ID': 'game_id', 
                                          'EVENTNUM': 'num_event', 'actionId': 'action_id', 
                                          'EVENTMSGTYPE': 'tp_event', 'actionType': 'tp_action', 
                                          'EVENTMSGACTIONTYPE': 'tp_subevent', 'subType': 'tp_subaction', 
                                          'PERIOD': 'period', 'shotDistance': 'shot_distance', 
                                          'DESCRIPTION': 'description', 'xLegacy': 'xlegacy', 
                                          'yLegacy': 'ylegacy'})
    
    return merged_df

pbp_api = pd.read_csv(FOLDER_PROCESSED_DATA + "pbpa_total_python.csv", sep = ';', dtype = 'str')
pbp_site = pd.read_csv(FOLDER_PROCESSED_DATA + "pbpc_total_python.csv", sep = ';', dtype = 'str')

df_ = concatenate_pbp_sources(pbp_api, pbp_site)

numeric_cols = ['num_event','action_id','period','tp_event','tp_subevent','shot_distance','xlegacy','ylegacy']
df_[numeric_cols] = df_[numeric_cols].apply(pd.to_numeric, errors = 'coerce')

df_.to_csv(path_or_buf = FOLDER_PROCESSED_DATA + "pbp_python.csv", sep = ';', index = False)

