
# Import libraries
import os
import re
import pandas as pd
import datetime

clean_actions = lambda a: a.replace(r"{", "").replace(r"}", "").replace(", ", "[;]").split(",")
break_actions = lambda a: [element.split(":", maxsplit = 1) for element in clean_actions(a)]
actions_to_dict = lambda a: {kv[0]: [kv[1]] for kv in break_actions(a)}
actions_to_dataframe = lambda a: pd.DataFrame.from_dict(actions_to_dict(a))

# Define file paths
FOLDER_PROCESSED_DATA = "D:/Mestrado/NBA/nba/data/processed/"
FILE_GAMES = FOLDER_PROCESSED_DATA + "games_nba.csv"
PATH_HTML_PAGES = 'D:/Mestrado/NBA/nba/data/raw/html_nba_site/'
PATH_CSV_FILES = "D:/Mestrado/NBA/nba/data/temp/pbp_nbasite/csv_files/"

# Read all filenames from the HTML pages folder
filenames = [filename for filename in os.listdir(PATH_HTML_PAGES)]
games_already_extracted = [files.replace(".csv", ".txt") for files in os.listdir(PATH_CSV_FILES)]
# Define functions for reading and saving game play-by-play data
def read_game_pbp(filename, path):
    game_id = filename.replace(".txt", "")
    with open(path + filename, 'r', encoding='latin-1') as file:
        content = file.read()  
    pattern = '\\{actionNumber\\:.+?\\}'
    actions = re.findall(pattern, content)
    df = pd.concat([actions_to_dataframe(row) for row in actions], axis = 0)\
        .assign(description = lambda x: [desc.replace("[;]", ",") for desc in x['description']])\
        .assign(GAME_ID = game_id)\
        .reset_index(drop = True)
    df.replace("", pd.NA, inplace = True)
    columns = ['GAME_ID', 'actionId', 'actionNumber', 'clock', 'period', 'actionType', 
               'subType', 'description', 'location', 'teamId', 'teamTricode', 'personId', 
               'playerName', 'playerNameI', 'xLegacy', 'yLegacy', 'shotDistance', 
               'shotResult', 'isFieldGoal', 'scoreHome', 'scoreAway', 'pointsTotal',
               'videoAvailable']
    return df[columns]

def save_game_pbp_as_csv_file(dataframe, path):
  game_id = dataframe['GAME_ID'][0]
  dataframe.to_csv(path_or_buf = path + game_id + ".csv", sep = ';', index = False)
  
def read_and_save_game_pbp(filename, html_pages_folder, csv_files_folder):
  df = read_game_pbp(filename, html_pages_folder)
  save_game_pbp_as_csv_file(df, csv_files_folder)

def build_pbp_dataframe_by_season(season, games, path_pbp_by_games_file):
  # Filter games for the specified season
  games_ = games.query(f"SEASON == '{season}'")[['GAME_ID', 'SEASON']]
  df_ = pd.DataFrame.from_dict({'file': os.listdir(path_pbp_by_games_file)})\
  .assign(GAME_ID = lambda x: [s[:10] for s in x['file']])\
  .merge(games_, how = 'inner')\
  .assign(file  = lambda x: x['GAME_ID'] + '.csv')

  season_pbp = [pd.read_csv(path_pbp_by_games_file + file, sep = ';', dtype = 'str') for file in df_['file'].to_list()]
  season_pbp = pd.concat(season_pbp, axis = 0)
  season_pbp.replace("", pd.NA, inplace = True)
  
  season_pbp['n'] = season_pbp\
  .groupby(['GAME_ID', 'actionNumber'])['actionId']\
  .transform('size')
  
  spbp1 = season_pbp\
  .query("n == 1")\
  .rename(columns = {'personId': 'personId1', 
                     'playerName': 'playerName1', 
                     'playerNameI': 'playerNameI1',
                     'teamId': 'teamId1',
                     'teamTricode': 'teamTricode1'})
           
  spbp2 = season_pbp\
  .query("n == 2")\
  .groupby(['GAME_ID', 'actionNumber'])\
  .apply(lambda x: pd.Series({
     'actionId': min(x['actionId']),
     'description': ' '.join(x['description'].fillna("")),
     'personId': ';'.join(x['personId'].fillna("")),
     'playerName': ';'.join(x['playerName'].fillna("")),
     'playerNameI': ';'.join(x['playerNameI'].fillna("")),
     'teamId': ';'.join(x['teamId'].fillna("")),
     'teamTricode': ';'.join(x['teamTricode'].fillna(""))
     }))\
  .reset_index()    
    
  spbp2 = season_pbp\
  .query("n == 2")\
  .drop(['description', 'personId', 'playerName', 
         'playerNameI', 'teamId', 'teamTricode', 'actionId'], axis = 1)\
  .merge(spbp2, how = 'inner')\
  .query("not actionType.isna()")
  
  # Separate columns
  spbp2[['personId1', 'personId2']] = spbp2['personId'].str.split(';', expand = True)
  spbp2[['playerName1', 'playerName2']] = spbp2['playerName'].str.split(';', expand = True)
  spbp2[['playerNameI1', 'playerNameI2']] = spbp2['playerNameI'].str.split(';', expand = True)
  spbp2[['teamId1', 'teamId2']] = spbp2['teamId'].str.split(';', expand = True)
  spbp2[['teamTricode1', 'teamTricode2']] = spbp2['teamTricode'].str.split(';', expand = True)

  spbp = pd.concat([spbp1, spbp2], axis = 0)\
  .drop(['personId', 'playerName', 'playerNameI', 'teamId', 'teamTricode'], axis = 1)
  
  # Replace 0 with NaN in selected columns
  replace_cols = ['personId1', 'personId2', 'teamId1', 'teamId2']
  spbp[replace_cols] = spbp[replace_cols].replace('0', pd.NA)

  # Replace values with NaN based on condition
  spbp['shotDistance'] = spbp.apply(lambda x: pd.NA if x['isFieldGoal'] == '0' else x['shotDistance'], axis = 1)
  spbp['xLegacy'] = spbp.apply(lambda x: pd.NA if x['isFieldGoal'] == '0' else x['xLegacy'], axis = 1)
  spbp['yLegacy'] = spbp.apply(lambda x: pd.NA if x['isFieldGoal'] == '0' else x['yLegacy'], axis = 1)

  # Convert selected columns to numeric
  numeric_cols = ['period', 'actionNumber', 'shotDistance', 'xLegacy', 
                  'yLegacy', 'scoreHome', 'scoreAway', 'pointsTotal', 
                  'videoAvailable']
  spbp[numeric_cols] = spbp[numeric_cols]\
  .apply(pd.to_numeric, errors = 'coerce')
  
  # Arrange DataFrame
  spbp = spbp.sort_values(by = ['GAME_ID', 'period', 'actionNumber'])\
  .rename({'GAME_ID': 'game_id'}, axis = 1)\
  .assign(season = season)
  
  spbp = spbp[['season', 'game_id', 'actionId', 'actionNumber', 'actionType', 
               'subType', 'clock', 'period', 'description', 'location', 
               'personId1', 'personId2', 'playerName1', 'playerName2', 
               'playerNameI1', 'playerNameI2', 'teamId1', 'teamId2', 
               'teamTricode1', 'teamTricode2', 'isFieldGoal', 'shotResult',
               'shotDistance', 'xLegacy', 'yLegacy', 'scoreHome', 'scoreAway', 
               'pointsTotal', 'videoAvailable']]
               
  return spbp

# Find missing files
missing_files = set(filenames).difference(set(games_already_extracted))

if len(missing_files) > 0:
  for iter, f in enumerate(missing_files):
    read_and_save_game_pbp(f, PATH_HTML_PAGES, PATH_CSV_FILES)
   
# Read games data   
games_nba = pd.read_csv(FILE_GAMES, sep = ';', dtype = 'str')  

# Build play-by-play DataFrame by season
season_files = []
for season_ in games_nba['SEASON'].drop_duplicates().to_list():
  df_ = build_pbp_dataframe_by_season(season = season_, games = games_nba, path_pbp_by_games_file = PATH_CSV_FILES)
  season_files.append(df_)
  print(f"Season:{season_} processed.\n")
  
# Concatenate and save play-by-play data  
pd.concat(season_files, axis = 0)\
  .to_csv(path_or_buf = FOLDER_PROCESSED_DATA + "pbpc_total_python.csv", sep = ';', index = False)

# Read the combined play-by-play data
pbpc_total = pd.read_csv(filepath_or_buffer = FOLDER_PROCESSED_DATA + "pbpc_total_python.csv", sep = ';')

