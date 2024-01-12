
### Folder to save the data of the project

 > 1. The first part are the data from NBA API. We can get this data from other sources of data, but here, we can know the id of the games and it's essential to get the data in NBA site. The data from this source is in api folder distributed by season.

 > 2. The second part are the data from a crawler in NBA site. To do this, we use the GAME_ID (extracted from NBA api) and then access the website and download the html page. In R, we extract the data from those html text files and make season dataframes RData files. The data from this source is in crawled_pbp/crawled_pbp_nbasite folder distributed by season.

 > 3. We also get the games of Basketball Reference site and play by play data is available there too. I need TO DO a efficient way to extract data from there, but even if I extract, I'm not using that data.

