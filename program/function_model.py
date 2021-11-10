import pandas as pd
import numpy as np
class game:
    def __init__(self, home=None,away=None,week=None):
        self.home=home
        self.away=away
        week=None

class play:
    def __init__(self, id=None,desc=None, time=None,yardline=None,side=None,possessionTeam=None):
        self.id=id
        self.desc=desc
        self.players=[]
        self.football=None
        self.time=time
        self.yardline=yardline
        self.side=side
        self.possessionTeam=possessionTeam
        self.defense=None
        self.ball_carrier=[]
        self.frames=None
        self.events=None
    def __repr__(self):
        return 'Play: {}, Possesion {} defending {}'.format(self.id, self.possessionTeam,self.defense)
    def __str__(self):
        return 'Play: {}, Possesion {} defending {}'.format(self.id, self.possessionTeam,self.defense)
    def add_tracking_data(self,df):
        id_temp=self.id
        df_temp=df.query('game_label==@id_temp').copy(deep=True)
        max_frame=df.frameId.max()
        self.frames=max_frame
        df_frames=df_temp[['frameId']].drop_duplicates().assign(dummy=True)
        df_temp=df_temp.assign(dummy=True).merge(df_frames, on=['frameId','dummy'],how='outer')
        df_temp['nflId']=df_temp['nflId'].apply(lambda x: 'football' if pd.isnull(x) else x)
        for pplayer in df_temp['nflId'].drop_duplicates().tolist():
            df_tempp=df_temp.query('nflId == @pplayer').sort_values('frameId')
            if pplayer=='football':
                self.football=player(id=pplayer,x=df_tempp['x'].to_numpy(),y=df_tempp['y'].to_numpy(),speed=df_tempp['s'].to_numpy(),direction=df_tempp['dir'].to_numpy(),player_label='placeholder',ttype='football')
            else:     
                self.players.append(player(id=pplayer,x=df_tempp['x'].to_numpy(),y=df_tempp['y'].to_numpy(),speed=df_tempp['s'].to_numpy(),direction=df_tempp['dir'].to_numpy(),player_label=players_dict[pplayer],ttype=df_tempp['team'].drop_duplicates().iloc[0]))
        self.get_ball_carrier()
        self.events=df_temp.query('event!="None"')[['event','frameId']].drop_duplicates()       
    def get_ball_carrier(self):
        res=[]
        for player in self.players:
            res.append(self.football.get_distance(player)) 
        self.ball_carrier=np.asarray(res).argmin(axis=0)
    def distance_to_ball_carrier(self):
        wstart=self.events.query('event in ["punt_received","kick_received"]')['frameId'].iloc[0]
        wend=self.events.query('event in ["tackle","fumble","out_of_bounds"]')['frameId'].iloc[0]
        if not len(wend)>0:
            wend=self.events['frameId'].iloc[-1]






        



def get_gamelabel(gameId,playId):
    return str(gameId)+'--'+str(playId)

class player:
    def __init__(self,id=None, x=None,y=None,speed=None, direction=None,player_label=None,ttype=None):
        self.id=id
        self.player_label=player_label
        self.x=x
        self.y=y
        self.speed=speed
        self.dir=direction
        self.type=ttype   
    def __repr__(self):
        return 'Player: {}, team {} '.format( self.player_label,self.type)
    def __str__(self):
        return 'Player: {}, team {} '.format( self.player_label,self.type)
    def get_distance(self,pplayer):
        a= np.asmatrix(np.array([self.x,self.y])) 
        b= np.asmatrix(np.array([pplayer.x,pplayer.y])) 
        return np.linalg.norm(a-b,axis=0)              

df=pd.read_csv('../data/tracking2018.csv')
df['game_label']=df.apply(lambda x: get_gamelabel(x['gameId'],x['playId']),axis=1)
plays=pd.read_csv('../data/plays.csv')
df_players=pd.read_csv('../data/players.csv')
df_players['player_label']=df_players.apply(lambda x: x['displayName']+'--'+x['Position'], axis=1)
players_dict=dict(zip(df_players.nflId.tolist(),df_players.player_label.tolist()))
x=plays.iloc[13770]

temp_play=play(x['gameId'].astype(str)+'--'+x['playId'].astype(str),x['playDescription'], x['gameClock'],x['yardlineNumber'], side=x['yardlineSide'] , possessionTeam=x['possessionTeam'])

temp_play.add_tracking_data(df)

temp_play.ball_carrier