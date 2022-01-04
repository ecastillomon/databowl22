
import os
os.chdir('program')

import pandas as pd
#import function_model as md
import numpy as np


df=pd.read_csv('../data/tracking2018.csv')

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
ids_returns=df_plays.query('kickReturnYardage==kickReturnYardage')['game_label'].drop_duplicates().tolist()

ids=list(set(ids_available) & set(ids_returns))



test_set=playSet(ids)
test_set.add_plays(df_plays)


test_set.add_tracking_data_set(df)
df_datamart=test_set.get_training_sample(10)

df_datamart.to_csv('../cache/df_datamart.csv',index=False)
df_datamart.to_pickle('../cache/df_datamart.pickle')



