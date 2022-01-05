df=pd.read_csv('/mnt/01e93028-9cf3-4480-a1c8-8c693bc9b031/Downloads/nfl-databowl/tracking2020.csv')
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
df_sample=test_set.get_play_sample()
df_sample.to_csv('../cache/df_sample_2020.csv',index=False)

