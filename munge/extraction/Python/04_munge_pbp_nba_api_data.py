
import pandas as pd
import re
import os

FOLDER_API_DATA = "D:/Mestrado/NBA/nba/data/temp/api/"
FOLDER_SAVE_API_PBP_SEASON_FILES = "D:/Mestrado/NBA/nba/data/processed/pbp_season_files/api/"
FOLDER_PROCESSED_DATA = "D:/Mestrado/NBA/nba/data/processed/"

def pre_process_pbp_api_files(file):
    # Extract season from file name
    season = re.search(r'([0-9]{4}_[0-9]{2})', file).group(0).replace('_', '-')
    
    # Read data from file
    xdf = pd.read_csv(file, delimiter=';', dtype=str)
    
    # Create auxiliary column
    xdf['aux'] = 0
    xdf.loc[~xdf['HOMEDESCRIPTION'].isna() & ~xdf['VISITORDESCRIPTION'].isna(), 'aux'] = 1
    xdf.loc[~xdf['HOMEDESCRIPTION'].isna() & xdf['VISITORDESCRIPTION'].isna(), 'aux'] = 2
    xdf.loc[xdf['HOMEDESCRIPTION'].isna() & ~xdf['VISITORDESCRIPTION'].isna(), 'aux'] = 3
    xdf.loc[xdf['HOMEDESCRIPTION'].isna() & xdf['VISITORDESCRIPTION'].isna(), 'aux'] = 4
    
    # Create ind column
    xdf['ind'] = 0
    xdf.loc[(xdf['aux'] == 1) & (xdf['HOMEDESCRIPTION'].str.contains('BLOCK|STEAL')), 'ind'] = 1
    xdf.loc[(xdf['aux'] == 1) & (xdf['VISITORDESCRIPTION'].str.contains('BLOCK|STEAL')), 'ind'] = 2
    
    # Modify DESCRIPTION column
    xdf['DESCRIPTION'] = ''
    xdf.loc[xdf['ind'] == 1, 'DESCRIPTION'] = xdf['VISITORDESCRIPTION'] + ' ' + xdf['HOMEDESCRIPTION']
    xdf.loc[xdf['ind'] == 2, 'DESCRIPTION'] = xdf['HOMEDESCRIPTION'] + ' ' + xdf['VISITORDESCRIPTION']
    xdf.loc[xdf['aux'] == 2, 'DESCRIPTION'] = xdf['HOMEDESCRIPTION']
    xdf.loc[xdf['aux'] == 3, 'DESCRIPTION'] = xdf['VISITORDESCRIPTION']
    xdf.loc[xdf['aux'] == 4, 'DESCRIPTION'] = xdf['NEUTRALDESCRIPTION']
    
    # Drop auxiliary columns
    xdf.drop(columns=['aux', 'ind'], inplace=True)
    
    # Convert NaN values in PERSON1TYPE, PERSON2TYPE, PERSON3TYPE to 0
    xdf[['PERSON1TYPE', 'PERSON2TYPE', 'PERSON3TYPE']] = xdf[['PERSON1TYPE', 'PERSON2TYPE', 'PERSON3TYPE']].fillna(0)
    
    # Concatenate PERSON1TYPE, PERSON2TYPE, PERSON3TYPE into a single column
    xdf['PERSON1TYPE'] = [str(i) for i in xdf['PERSON1TYPE']]
    xdf['PERSON2TYPE'] = [str(i) for i in xdf['PERSON2TYPE']]
    xdf['PERSON3TYPE'] = [str(i) for i in xdf['PERSON3TYPE']]

    xdf['persons'] = xdf['PERSON1TYPE'] + xdf['PERSON2TYPE'] + xdf['PERSON3TYPE']
    
    # Add SEASON column
    xdf['SEASON'] = season
    
    # Select desired columns
    xdf = xdf[['SEASON', 'GAME_ID', 'EVENTNUM', 'EVENTMSGTYPE', 'EVENTMSGACTIONTYPE', 'PERIOD',
               'WCTIMESTRING', 'PCTIMESTRING', 'DESCRIPTION', 'persons', 'PERSON1TYPE',
               'PERSON2TYPE', 'PERSON3TYPE', 'PLAYER1_ID', 'PLAYER2_ID', 'PLAYER3_ID',
               'PLAYER1_NAME', 'PLAYER2_NAME', 'PLAYER3_NAME', 'PLAYER1_TEAM_ID',
               'PLAYER2_TEAM_ID', 'PLAYER3_TEAM_ID', 'PLAYER1_TEAM_NICKNAME',
               'PLAYER2_TEAM_NICKNAME', 'PLAYER3_TEAM_NICKNAME', 'PLAYER1_TEAM_CITY',
               'PLAYER2_TEAM_CITY', 'PLAYER3_TEAM_CITY', 'PLAYER1_TEAM_ABBREVIATION',
               'PLAYER2_TEAM_ABBREVIATION', 'PLAYER3_TEAM_ABBREVIATION', 'SCORE',
               'SCOREMARGIN', 'VIDEO_AVAILABLE_FLAG']]
    
    return xdf

set_files = [file for file in os.listdir(FOLDER_API_DATA) if re.search("[0-9]{4}_[0-9]{2}", file)]

list_pbp = []
for file in set_files:
    df_ = pre_process_pbp_api_files(FOLDER_API_DATA + file)
    list_pbp.append(df_)
    print(f"{file} processed.\n")

# Concatenate and save play-by-play data  
pd.concat(list_pbp, axis = 0)\
  .to_csv(path_or_buf = FOLDER_PROCESSED_DATA + "pbpa_total_python.csv", sep = ';', index = False)

# Read the combined play-by-play data
pbpa_total = pd.read_csv(filepath_or_buffer = FOLDER_PROCESSED_DATA + "pbpa_total_python.csv", sep = ';', dtype = 'str')
