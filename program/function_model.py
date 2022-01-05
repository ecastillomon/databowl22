import pandas as pd
import numpy as np
from joblib import load
import xgboost as xgb
mod_xgb=xgb.Booster({'nthread':1})
mod_xgb.load_model('../model/yp.model')
feats=load('../model/feats.pickle')
class game:
    def __init__(self, home=None,away=None,week=None):
        self.home=home
        self.away=away
        week=None


class play:
    def __init__(self, id=None,home=None,away=None,desc=None, time=None,yardline=None,possessionTeam=None,defenseTeam=None,type=None,result=None, yardage=None, penaltyYards=None) :
        self.id=id
        self.home=home
        self.away=away
        self.desc=desc
        self.players=[]
        self.football=None
        self.time=time
        self.yardline=yardline
        self.side=None
        self.possessionTeam=possessionTeam
        self.defense=defenseTeam
        self.ball_carrier=[]
        self.frames=None
        self.events=None
        self.type=type
        self.result=result
        self.yardage=yardage
        self.is_penalty= pd.isnull(penaltyYards)
        self.playResult=None
        self.possession_players=[]
        self.defense_players=[]
        self.yardsGained=[]
        #self.returnStart=None
        #self.returnEnd=None
    def __repr__(self):
        return 'Play: {}, Possesion {} defending {}'.format(self.id, self.possessionTeam,self.defense)
    def __str__(self):
        return 'Play: {}, Possesion {} defending {}'.format(self.id, self.possessionTeam,self.defense)
    def add_tracking_data(self,df):
        id_temp=self.id
        df_temp=df.query('game_label==@id_temp').copy(deep=True)
        max_frame=df.frameId.max()
        self.side=df.playDirection.iloc[0]
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
        self.set_players_index()
        self.set_playResult()
        self.get_yards_gained()   
    def set_players_index(self):
        if self.possessionTeam==self.home:
            pos_temp='home'
        else:
            pos_temp='away'
        i=0    
        for pplayer in self.players:
            if pplayer.type==pos_temp:
                self.possession_players.append(i)
            else:
                self.defense_players.append(i)
            i=i+1
    def set_playResult(self):
        wend=self.events.query('event in ["tackle","fumble","out_of_bounds","touchdown"]')['frameId'].iloc[0]
        self.playResult=self.football.x[wend-1]                
    def get_yards_gained(self):
        self.yardsGained=abs(self.football.x-self.playResult)
        #if self.type=='Kickoff':
        #    if self.side=='right':
        #        self.yardsGained=self.playResult-self.football.x
        #    else:
        #        self.yardsGained=self.football.x-self.playResult
        #elif self.type=='Punt':
        #    if self.side=='left':
        #        self.yardsGained=self.playResult-self.football.x
        #    else:
        #        self.yardsGained=self.football.x-self.playResult            

    def get_yards_gained_frame(self,frame):
        return self.yardsGained[frame-1]
    def get_ball_carrier(self):
        res=[]
        for player in self.players:
            res.append(self.football.get_distance(player)) 
        self.ball_carrier=np.asarray(res).argmin(axis=0) 
    def distance_to_ball_carrier(self):
        wstart=self.events.query('event in ["punt_received","kick_received"]')['frameId'].iloc[0]
        wend=self.events.query('event in ["tackle","fumble","out_of_bounds","touchdown"]')['frameId'].iloc[0]
        if wend.size>0:
            wend=self.events['frameId'].iloc[-1]
        ball_carrier=self.players[self.ball_carrier[wstart]]    
        res=[]
        for player in self.players:
            res.append(ball_carrier.get_distance(player)) 
        return  np.asarray(res)
    def speed_mat(self):
        res=[]        
        for player in self.players:
            res.append(player.get_speed())      
        #return  np.asarray(res)[:,wstart:wend] 
        return  np.asarray(res)
    def get_speed_byteam(self):
        speed_mat=self.speed_mat()
        speed_off=speed_mat[self.possession_players,:]
        speed_def=speed_mat[self.defense_players,:]
        return speed_off,speed_def
    def get_distance_byteam(self):
        distance_mat=self.distance_to_ball_carrier()
        distance_mat_off=distance_mat[self.possession_players,:]
        distance_mat_def=distance_mat[self.defense_players,:]
        return distance_mat_off,distance_mat_def       
    def closest_players(self):
        distance_mat_off,distance_mat_def=self.get_distance_byteam()
        closest_off=distance_mat_off.argsort(axis=0).argsort(axis=0)
        closest_def=distance_mat_def.argsort(axis=0).argsort(axis=0)
        return closest_off,closest_def
    def get_model(self,tframe=None):
        distance_mat_off,distance_mat_def=self.get_distance_byteam()
        speed_mat_off,speed_mat_def=self.get_speed_byteam()

        off_distance=pd.DataFrame(np.sort(distance_mat_off[:,tframe])).transpose().add_prefix('off_distance_')
        def_distance= pd.DataFrame(np.sort(distance_mat_def[:,tframe])).transpose().add_prefix('def_distance_')

        off_speed=pd.DataFrame(np.sort(speed_mat_off[:,tframe])).transpose().add_prefix('off_speed_')
        def_speed= pd.DataFrame(np.sort(speed_mat_def[:,tframe])).transpose().add_prefix('def_speed_')

        return pd.concat([off_distance,def_distance,off_speed,def_speed],axis=1).assign(id=self.id,target=self.get_yards_gained_frame(tframe),frame=tframe)
    def get_training_sample(self,n=10):
        wstart=self.events.query('event in ["punt_received","kick_received"]')['frameId'].iloc[0]
        wend=self.events.query('event in ["tackle","fumble","out_of_bounds","touchdown"]')['frameId'].iloc[0]
        if wend.size>0:
            wend=self.events['frameId'].iloc[-1]
        sample=np.random.choice(range(wstart,wend),n,replace=False)
        res=[]
        for iframe in sample:
            res.append(self.get_model(iframe))
        return pd.concat(res) 
    def get_play_sample(self):
        wstart=self.events.query('event in ["punt_received","kick_received"]')['frameId'].iloc[0]
        wend=self.events.query('event in ["tackle","fumble","out_of_bounds","touchdown"]')['frameId'].iloc[0]
        if wend.size>0:
            wend=self.events['frameId'].iloc[-1]
        res=[]
        for iframe in range(wstart,wend+1):
            res.append(self.get_model(iframe))
        return pd.concat(res)                
    def get_yp_model(self):
        df_temp=self.get_play_sample()
        dtemp = xgb.DMatrix(df_temp[feats])
        df_temp['predicted_yp']=mod_xgb.predict(dtemp)
        df_temp['predicted_yp']=df_temp['predicted_yp'].apply(lambda x: 0 if x<0 else x)
        return df_temp






#####------->
        
          

     



def get_gamelabel(gameId,playId):
    return str(gameId)+'--'+str(playId)

class player:
    def __init__(self,id=None, x=None,y=None,speed=None,acc=None ,direction=None,player_label=None,ttype=None):
        self.id=id
        self.player_label=player_label
        self.x=x
        self.y=y
        self.speed=speed
        self.acc=acc
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
    def get_acc(self):
        return self.acc
    def get_speed(self):
        return self.speed                      

class playSet:
    def __init__(self, game_labels=None):
        self.game_labels=game_labels
        self.plays=[]
        
    def add_plays(self,df_plays=None):
        #for id_temp in self.gameIds:
        #    pplays=df_plays.query('gameId==@id_temp and kickReturnYardage==kickReturnYardage')['playId'].drop_duplicates().tolist()
        for pplay in self.game_labels:
            y=df_plays.query('game_label==@pplay').iloc[0]
            ttemp_play=play(y['gameId'].astype(str)+'--'+y['playId'].astype(str),y['homeTeamAbbr'],y['visitorTeamAbbr'] ,y['playDescription'], y['gameClock'],y['yardlineNumber'],  possessionTeam=y['possessionTeam'], defenseTeam=y['defenseTeam'],type=y['specialTeamsPlayType'],result=y['specialTeamsResult'], yardage=y['kickReturnYardage'], penaltyYards=y['penaltyYards'])
            print(ttemp_play)
            self.plays.append(ttemp_play)

    def add_tracking_data_set(self,df=None):
       for pplay in self.plays:
           try:
            pplay.add_tracking_data(df)
            print('Success at ', pplay)
           except Exception as e:
            print('Error at ', pplay)
            print('Error: ',e)

    def get_training_sample(self,n=10):
        res=[]
        for pplay in self.plays:
            try:
                res.append(pplay.get_training_sample(n=n))
                print('Success at ', pplay)
            except Exception as e:
                print('Error at ', pplay)
                print('Error: ',e)

        return pd.concat(res,axis=0)
    def get_play_sample(self):
        res=[]
        for pplay in self.plays:
            try:
                res.append(pplay.get_yp_model())
                print('Success at ', pplay)
            except Exception as e:
                print('Error at ', pplay)
                print('Error: ',e)

        return pd.concat(res,axis=0)        
