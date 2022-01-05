
import os
os.chdir('program')

import pandas as pd
#import function_model as md
import numpy as np


#df=pd.read_csv('../data/tracking2018.csv')
df=pd.read_csv('/mnt/01e93028-9cf3-4480-a1c8-8c693bc9b031/Downloads/nfl-databowl/tracking2018.csv')
df2=pd.read_csv('/mnt/01e93028-9cf3-4480-a1c8-8c693bc9b031/Downloads/nfl-databowl/tracking2019.csv')
df=pd.concat([df,df2],axis=0)
del df2

df['game_label']=df.apply(lambda x: get_gamelabel(x['gameId'],x['playId']),axis=1)
plays=pd.read_csv('../data/plays.csv')
games=pd.read_csv('../data/games.csv')

df_plays=games.merge(plays, on='gameId')
df_plays['defenseTeam']=df_plays.apply(lambda x: x['visitorTeamAbbr'] if x['possessionTeam']==x['homeTeamAbbr'] else x['homeTeamAbbr'], axis=1)
df_players=pd.read_csv('../data/players.csv')
df_players['player_label']=df_players.apply(lambda x: x['displayName']+'--'+x['Position'], axis=1)
players_dict=dict(zip(df_players.nflId.tolist(),df_players.player_label.tolist()))
df_plays['game_label']=df_plays.apply(lambda x: get_gamelabel(x['gameId'],x['playId']),axis=1)



ids_available=df['game_label'].drop_duplicates().tolist()
ids_returns=df_plays.query('kickReturnYardage==kickReturnYardage and specialTeamsPlayType=="kickoff"')['game_label'].drop_duplicates().tolist()

ids=list(set(ids_available) & set(ids_returns))



test_set=playSet(ids)
test_set.add_plays(df_plays)


test_set.add_tracking_data_set(df)
df_datamart=test_set.get_training_sample(5)

df_datamart.to_csv('../cache/df_datamart.csv',index=False)
df_datamart.to_pickle('../cache/df_datamart.pickle')


id_temp=['2019122908--2061']
test_set=playSet(id_temp)
test_set.add_plays(df_plays)
test_set.add_tracking_data_set(df)
test_set.plays[0].get_play_sample()
test_set.plays[0].football.x
test_set.plays[0].playResult