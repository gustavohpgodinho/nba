

import os
import numpy as np
import pandas as pd
import requests
from bs4 import BeautifulSoup
from concurrent.futures import ThreadPoolExecutor
from tqdm import tqdm

def extract_nba_game_page(df, path_save_files):
    """
    Extracts the HTML content from a given link in a DataFrame and saves it to a text file.

    Parameters:
    - df (pandas.DataFrame): DataFrame containing at least 'link' and 'GAME_ID' columns.
    - path_save_files (str): Path to the directory where the HTML content files will be saved.

    Example:
    >>> df_example = pd.DataFrame({'link': ['https://example.com'], 'GAME_ID': [123]})
    >>> path_save_files_example = '/path/to/save/files'
    >>> extract_nba_game_page(df_example, path_save_files_example)
    """

    try:
        # Extract link and game ID from DataFrame
        link = df['link']

        # Retrieve HTML content from the link
        response = requests.get(link)
        response.raise_for_status()  # Check for any request errors

        # Clean HTML content
        obj = response.text
        obj = obj.replace("\\", "").replace("\"", "")  # Remove backslashes and double quotes

        # Define the path to save the file
        path = os.path.join(path_save_files, df['GAME_ID'] + ".txt")

        # Save the HTML content to a text file
        with open(path, 'w', encoding='utf-8') as file:
            file.write(obj)

    except Exception as e:
        print("An error occurred")
        print(e)

# Example usage:
# df_example = pd.DataFrame({'link': ['https://example.com'], 'GAME_ID': [123]})
# path_save_files_example = '/path/to/save/files'
# extract_nba_game_page(df_example, path_save_files_example)


def extract_nba_play_by_play_pages(nba_games, files_path):
  
    """
    Extract NBA play-by-play pages for games that haven't been extracted yet.

    Parameters:
    - nba_games (pd.DataFrame): DataFrame with NBA game data, including 'GAME_ID' and 'SEASON'.
    - files_path (str): Path to save the generated text files.

    Returns:
    None
    """
    
    # Identify games that have already been crawled
    games_already_crawled = set([filename.replace(".txt", "") for filename in os.listdir(files_path)])

    # Identify games that are missing
    missing_games = nba_games[~nba_games["GAME_ID"].isin(games_already_crawled)]
    missing_games["row"] = np.random.rand(len(missing_games))
    missing_games = missing_games.sort_values(by="row", ascending=False)

    if not missing_games.empty:
      print(f"\nExtracting play-by-play pages for the " + str(missing_games.shape[0]) + " games:")
            
    # Loop until all missing games are extracted
    while not missing_games.empty:
        # Identify games that have already been crawled (again)
        games_already_crawled = set([filename.replace(".txt", "") for filename in os.listdir(files_path)])

        # Identify games that are missing (again)
        missing_games = nba_games[~nba_games["GAME_ID"].astype(str).isin(games_already_crawled)]
        missing_games["row"] = np.random.rand(len(missing_games))
        missing_games = missing_games.sort_values(by="row", ascending=False)
        missing_games['link'] = ["https://www.nba.com/game/cle-vs-bos-" + str(GAME_ID) + "/play-by-play?period=All" for GAME_ID in missing_games['GAME_ID']]
        
        for i in range(len(missing_games)):
          print("Extracting the page of the game: " + missing_games['GAME_ID'].iloc[i])
          extract_nba_game_page(missing_games.iloc[i], files_path)

# Exemplo de uso:
# nba_games_example = pd.DataFrame({"GAME_ID": [1, 2, 3], "SEASON": [2020, 2020, 2021]})
# files_path_example = "/path/to/save/files"
# extract_nba_play_by_play_pages(nba_games_example, files_path_example)


# Read NBA games data
dtypes = {'GAME_ID': str, 'HOME_TEAM_ID': str, 'AWAY_TEAM_ID': str, 'HOME_PTS': int, 'AWAY_PTS': int}

games_nba = pd.read_csv("D:/Mestrado/NBA/nba/data/games_nba.csv", delimiter = ';', dtype = dtypes)

# Extract play-by-play pages
extract_nba_play_by_play_pages(nba_games = games_nba, files_path = "D:/Mestrado/NBA/nba/data/crawler/html_nba_site/")

