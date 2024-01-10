

## Extract data from NBA api

import pandas as pd
import requests
import time
import os
from nba_api.stats.endpoints import leaguegamefinder
from nba_api.stats.endpoints import playbyplayv2

headers  = {
  "Connection": "keep-alive",
  "Accept": "application/json, text/plain, */*",
  "x-nba-stats-token": "true",
  "User-Agent": "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_14_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/79.0.3945.130 Safari/537.36",
  "x-nba-stats-origin": "stats",
  "Sec-Fetch-Site": "same-origin",
  "Sec-Fetch-Mode": "cors",
  "Referer": "https://stats.nba.com/",
  "Accept-Encoding": "gzip, deflate, br",
  "Accept-Language": "en-US,en;q=0.9"} 
            

# Functions to execute the extraction of data

def select_columns_from_games_dataframe(df):
    
    """
    Select only useful columns from the dataframe extracted from the NBA API in a way that one line represents one game.
    
    Parameters
    df: dataframe extracted from the NBA API.
  
    Returns
    returned_df; dataframe with only useful columns from each extract game of NBA by GAME_ID.
    """
    
    columns = ["GAME_ID", "GAME_DATE", "SEASON_ID", "MATCHUP", "TEAM_ID", "TEAM_ABBREVIATION", "TEAM_NAME", "PTS"]
    df = df[columns]
    
    dict_home = {'TEAM_ID': 'HOME_TEAM_ID', 
                 'TEAM_ABBREVIATION': 'HOME_TEAM_ABBR', 
                 'TEAM_NAME': 'HOME_TEAM', 
                 'PTS': 'HOME_PTS'}
                 
    dict_away = {'TEAM_ID': 'AWAY_TEAM_ID', 
                 'TEAM_ABBREVIATION': 'AWAY_TEAM_ABBR', 
                 'TEAM_NAME': 'AWAY_TEAM', 
                 'PTS': 'AWAY_PTS'}

    df_home_team = df\
    .query("MATCHUP.str.contains(pat = 'vs\\.', regex = True, na = False)")\
    .rename(dict_home, axis = 1)\
    .reset_index(drop = True)\
    .drop(['GAME_ID', 'MATCHUP'], axis = 1)
    
    df_away_team = df\
    .query("MATCHUP.str.contains(pat = '@', regex = True, na = False)")\
    .rename(dict_away, axis = 1)\
    .reset_index(drop = True)\
    .drop(['GAME_ID', 'GAME_DATE', 'SEASON_ID'], axis = 1)    
    
    returned_df = pd.concat([df_home_team, df_away_team], axis = 1)
    
    returned_columns = ["SEASON_ID", "GAME_DATE", "MATCHUP", "HOME_TEAM_ID", "AWAY_TEAM_ID", 
                        "HOME_TEAM_ABBR", "AWAY_TEAM_ABBR", "HOME_TEAM", "HOME_PTS", "AWAY_PTS", "AWAY_TEAM"]
    
    returned_df = returned_df[returned_columns]
    
    return returned_df
  
def filter_nba_games_by_type(df, type_games = ['Regular Season', "Play-in", "Playoffs"]):
    
    """
    Choose only specific types of games to extract pbp data.
    
    Parameters
    df: dataframe extracted from the NBA API.
    type_games: list of strings with the types of games to extract. If empty, will return all NBA type of games. Options: ['Preseason', 'Regular Season', 'All star', 'Playoffs', 'Play-in']
  
    Returns
    df_return; dataframe with only the type of games passed to function.
    """
    
    df_return = df\
    .reset_index()\
    .assign(HOME_TEAM_ID = lambda x: [str(i).replace(".0", "") for i in x["HOME_TEAM_ID"]],
            AWAY_TEAM_ID = lambda x: [str(i).replace(".0", "") for i in x["AWAY_TEAM_ID"]],
            SEASON_TYPE = lambda x: [i[:3] for i in x["GAME_ID"]])\
    .assign(SEASON_TYPE = lambda x: ["Preseason" if i == "001" else i for i in x["SEASON_TYPE"]])\
    .assign(SEASON_TYPE = lambda x: ["Regular Season" if i == "002" else i for i in x["SEASON_TYPE"]])\
    .assign(SEASON_TYPE = lambda x: ["All star" if i == "003" else i for i in x["SEASON_TYPE"]])\
    .assign(SEASON_TYPE = lambda x: ["Playoffs" if i == "004" else i for i in x["SEASON_TYPE"]])\
    .assign(SEASON_TYPE = lambda x: ["Play-in" if i == "005" else i for i in x["SEASON_TYPE"]])
    
    if type_games != []:
      df_return = df_return\
      .query("SEASON_TYPE in " + str(type_games))
      

    df_return\
    .astype({"HOME_PTS": "Int64", "AWAY_PTS": "Int64"})\
    [["SEASON_ID", "SEASON", "SEASON_TYPE", "GAME_ID", "GAME_DATE", "MATCHUP", 
      "HOME_TEAM", "HOME_PTS", "AWAY_PTS", "AWAY_TEAM", "HOME_TEAM_ID", 
      "AWAY_TEAM_ID", "HOME_TEAM_ABBR", "AWAY_TEAM_ABBR"]]\
    .sort_values(["SEASON", "GAME_DATE"])\
    .reset_index(drop = True)
    
    return df_return

def create_seasons_list(start_year = 2008, end_year = 2021):
    
    """
    Create a list of seasons to be used in the download_games function.
    
    Parameters
    start_year: integer to set up the first season to download games data. Default: 2008
    end_year: integer to set up the last season to download games data. Default: 2021
  
    Returns
    seasons: list to set up the seasons to download games data. 
    """
    
    seasons = []
    for i in range(start_year, end_year + 1):
        seasons.append(str(i) + "-" + str(i + 1)[2:])
        
    return seasons

def download_games(seasons, show_steps, type_games):    
    
    """
    Connect to the NBA API and download games data.
    
    Parameters
    seasons: list to set up the seasons to download games data. 
    show_steps: boolean to print what season we are extracting games.
    type_games: list of strings with the types of games to extract. If empty, will return all NBA type of games. Options: ['Preseason', 'Regular Season', 'All star', 'Playoffs', 'Play-in']
  
    Returns
    df: dataframe containing all games data from the seasons passed to function.
    """

    games_data = []
    for s in seasons: 
        
        if show_steps:
            print("Extracting games of season: " + s)
        
        gamefinder = leaguegamefinder.LeagueGameFinder(season_nullable = s, league_id_nullable = '00', headers = headers)
        
        season_games = gamefinder\
        .get_data_frames()[0]\
        .groupby('GAME_ID')\
        .apply(select_columns_from_games_dataframe)
        
        season_games['SEASON'] = s
        
        games_data.append(season_games)
        
    df = filter_nba_games_by_type(pd.concat(games_data, axis = 0), type_games = type_games)
    
    return df
  
def download_play_by_play(df_games):
    
    """
    Connect to the NBA API and download the play by play selected games' data.
    
    Parameters
    df_games: dataframe containing the games that we want to extract the play by play data. This dataframe must contain the column "GAME_ID".
  
    Returns
    pbp_data: list of dataframes containing the play by play data of the games passed to function.
    """


    pbp_data = []
    
    # Just a counter to avoid to make many requests to the API in a short period of time
    i = 0
    
    for game_id in list(df_games['GAME_ID']):
        i += 1
        
        pbp = playbyplayv2.PlayByPlayV2(game_id, headers = headers)\
        .get_data_frames()[0]
        
        pbp_data.append(pbp)
        
        print("Get data from game > " + game_id + ": " + str(len(pbp)) + " rows.")
        
        if((i % 10) == 0):
            time.sleep(5)
    
    return pbp_data

def execute_download_data_nba_api(file_to_save_games_nba, path_to_save_pbp, seasons = [], show_steps = True, type_games = []):
    
    """
    Execute all of the process to extract the games and the play by play of NBA api.
    
    Parameters
    file_to_save_games_nba: string with the path to save the games data. It needs to be a csv extension.
    path_to_save_pbp: string with the path to save the play by play data.
    seasons: list to set up the seasons to download games data.
    show_steps: boolean to print what season we are extracting games.
    type_games: list of strings with the types of games to extract. If empty, will return extract only the following NBA type of games: ['Regular Season', 'Playoffs', 'Play-in']. Options: ['Preseason', 'Regular Season', 'All star', 'Playoffs', 'Play-in']
    
    Returns
    
    """
    
    if seasons == []:
        seasons = create_seasons_list(2008, 2023)
    
    if type_games == []:
        type_games = ['Regular Season', "Play-in", "Playoffs"]
    
    # Doing the download of the games.
    games_nba = download_games(seasons = seasons, show_steps = show_steps, type_games = type_games)
    
    # Saving the csv file of games nba details
    games_nba.to_csv(file_to_save_games_nba, index = False, sep = ';')
    
    # columns there will be in our pbp data file.
    pbp_data_columns = ['GAME_ID', 'EVENTNUM', 'EVENTMSGTYPE', 'EVENTMSGACTIONTYPE', 'PERIOD', 'WCTIMESTRING', 
                        'PCTIMESTRING', 'HOMEDESCRIPTION', 'NEUTRALDESCRIPTION', 'VISITORDESCRIPTION', 'SCORE', 
                        'SCOREMARGIN', 'PERSON1TYPE', 'PLAYER1_ID', 'PLAYER1_NAME', 'PLAYER1_TEAM_ID', 
                        'PLAYER1_TEAM_CITY', 'PLAYER1_TEAM_NICKNAME', 'PLAYER1_TEAM_ABBREVIATION', 'PERSON2TYPE', 
                        'PLAYER2_ID', 'PLAYER2_NAME', 'PLAYER2_TEAM_ID', 'PLAYER2_TEAM_CITY', 'PLAYER2_TEAM_NICKNAME', 
                        'PLAYER2_TEAM_ABBREVIATION', 'PERSON3TYPE', 'PLAYER3_ID', 'PLAYER3_NAME', 'PLAYER3_TEAM_ID', 
                        'PLAYER3_TEAM_CITY', 'PLAYER3_TEAM_NICKNAME', 'PLAYER3_TEAM_ABBREVIATION', 
                        'VIDEO_AVAILABLE_FLAG']
    
    # If there isn't any kind of downlaoded pbp data yet, so we download all the data from the games in games_nba.
    # If there are downloaded pbp data, so we will download only the games of games_nba the we haven't pbp data.
    if len(os.listdir(path = path_to_save_pbp)) == 0:
        
        if show_steps:
          print("\nDownloading play by play data of " + str(games_nba.shape[0]) + " games.")
          
        for s in seasons:
            pbp_season = games_nba.query("SEASON == " + s)\
            .pipe(download_play_by_play)
            
            path = path_to_save_pbp + "pbpa_" + str.replace(s, "-", "_") + ".csv"
            
            pbp_data = pd.concat(pbp_season, axis = 0)\
            .reset_index(drop = True)\
            .assign(GAME_ID = lambda x: ['00' + str(i) for i in x['GAME_ID']])\
            .astype({"PERSON1TYPE": "Int64", 
                     "PLAYER1_TEAM_ID": "Int64", 
                     "PLAYER2_TEAM_ID": "Int64", 
                     "PLAYER3_TEAM_ID": "Int64"})\
            [pbp_data_columns]
            
            pbp_data.to_csv(path_or_buf = path, sep = ';', index = False)
    
    else: 
        games_playbyplay_downloaded = list()
        
        for file in os.listdir(path_to_save_pbp):
            df_downloaded_pbp = pd.read_csv(path_to_save_pbp + file, sep = ';')[['GAME_ID']].drop_duplicates()
            games_playbyplay_downloaded.append(df_downloaded_pbp)
            
        games_playbyplay_downloaded = pd.concat(games_playbyplay_downloaded, axis = 0)\
        .assign(GAME_ID = lambda x: ['00' + str(i) for i in x['GAME_ID']])\
        .reset_index(drop = True)
        
        xdf_pbp = games_nba\
        .merge(games_playbyplay_downloaded.assign(aux = 1), how = 'left')\
        .query("aux != 1")
        
        print("\nDownloading play by play data of " + str(xdf_pbp.shape[0]) + " games.")
        
        xdf_pbp = xdf_pbp\
        .pipe(download_play_by_play)
        
        xdf_pbp = pd.concat(xdf_pbp, axis = 0).reset_index(drop = True)
        
        path = path_to_save_pbp + "pbpa_complement" + ".csv"
        
        xdf_pbp.to_csv(path_or_buf = path, sep = ';', index = False)
        
        print("\nExtracted pbp saved in pbpa_complement.csv")
        
        print("Reading all the pbp past data to save pbp season files:")        
        
        pbp_data = list()
        
        for file in os.listdir(path_to_save_pbp):
            
            print("Reading file: " + file)
            temp = pd.read_csv(path_to_save_pbp + file, sep = ';')
            pbp_data.append(temp)
            
        pbp_data = pd.concat(pbp_data, axis = 0)
        
        pbp_data = pbp_data\
        .assign(GAME_ID = lambda x: ['00' + str(i) for i in x['GAME_ID']])\
        .astype({"PERSON1TYPE": "Int64", 
                 "PLAYER1_TEAM_ID": "Int64", 
                 "PLAYER2_TEAM_ID": "Int64", 
                 "PLAYER3_TEAM_ID": "Int64"})\
        [pbp_data_columns]
        
        print("Saving the pbp data of each season in a separated file:")        
                
        for s in games_nba['SEASON'].drop_duplicates().to_list():
            
            print("Recording the data for the season " + s)
            
            games_nba\
            .query("SEASON == '" + s + "'")[['GAME_ID']]\
            .drop_duplicates()\
            .merge(pbp_data, how = 'inner')\
            .sort_values(['GAME_ID', 'PERIOD', 'EVENTNUM'], ascending = [True, True, True])\
            .reset_index(drop = True)\
            .to_csv(path_to_save_pbp + "pbpa_" + s.replace("-", "_") + ".csv", index = False, sep = ';')


# Execute the code

execute_download_data_nba_api(path_to_save_pbp = "D:/Mestrado/NBA/nba/data/api/", file_to_save_games_nba = "D:/Mestrado/NBA/nba/data/games_nba.csv")
                              
                              
