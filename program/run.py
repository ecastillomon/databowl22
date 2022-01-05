df=pd.read_csv('../data/tracking2018.csv')
df['game_label']=df.apply(lambda x: get_gamelabel(x['gameId'],x['playId']),axis=1)
plays=pd.read_csv('../data/plays.csv')
games=pd.read_csv('../data/games.csv')

df_plays=games.merge(plays, on='gameId')
df_plays['defenseTeam']=df_plays.apply(lambda x: x['visitorTeamAbbr'] if x['possessionTeam']==x['homeTeamAbbr'] else x['homeTeamAbbr'], axis=1)
df_players=pd.read_csv('../data/players.csv')
df_players['player_label']=df_players.apply(lambda x: x['displayName']+'--'+x['Position'], axis=1)
players_dict=dict(zip(df_players.nflId.tolist(),df_players.player_label.tolist()))
x=df_plays.iloc[13770]
df_temp=df.query('gameId==2020091401')
temp_play=play(x['gameId'].astype(str)+'--'+x['playId'].astype(str),x['homeTeamAbbr'],x['visitorTeamAbbr'] ,x['playDescription'], x['gameClock'],x['yardlineNumber'], side=x['yardlineSide'] , possessionTeam=x['possessionTeam'], defenseTeam=x['defenseTeam'],type=x['specialTeamsPlayType'],result=x['specialTeamsResult'], yardage=x['kickReturnYardage'], penaltyYards=x['penaltyYards'], playResult=x['playResult'])

temp_play.add_tracking_data(df)


temp_play.get_training_sample(n=10)

temp_play.get_model(30)

temp_play.events

distance_temp=temp_play.distance_to_ball_carrier()

distance_temp[temp_play.possession_players,:].argsort(axis=0)

distance_mat_off=distance_temp[temp_play.possession_players,:]
closest_off=distance_mat_off.argsort(axis=0).argsort(axis=0)

distance_mat_def=distance_temp[temp_play.defense_players,:]
closest_def=distance_mat_def.argsort(axis=0).argsort(axis=0)

tframe=10
pd.DataFrame(np.sort(distance_mat_off[:,tframe])).transpose().add_prefix('off_distance_')

#.argmin(axis=0)




