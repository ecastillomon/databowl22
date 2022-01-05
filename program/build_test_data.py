df=pd.read_csv('/mnt/01e93028-9cf3-4480-a1c8-8c693bc9b031/Downloads/nfl-databowl/tracking2018.csv')
df['game_label']=df.apply(lambda x: get_gamelabel(x['gameId'],x['playId']),axis=1)

ids_available=df['game_label'].drop_duplicates().tolist()
ids_returns=df_plays.query('kickReturnYardage==kickReturnYardage')['game_label'].drop_duplicates().tolist()

ids=list(set(ids_available) & set(ids_returns))


play_id='2018090903--2956'
test_set=playSet([play_id])
test_set.add_plays(df_plays)
test_set.add_tracking_data_set(df)

test_set.plays[0].get_yp_model()

df_sample=test_set.plays[0].get_yp_model()

df_sample.to_csv('../cache/df_sample_{}.csv'.format(play_id),index=False)


test_set.plays[0].side

from joblib import load
import xgboost as xgb
mod_xgb=xgb.Booster({'nthread':1})
mod_xgb.load_model('../model/yp.model')
feats=load('../model/feats.pickle')
dtemp = xgb.DMatrix(df_sample[feats])
df_sample['predicted_yp']=mod_xgb.predict(dtemp)
df_sample['predicted_yp']=df_sample['predicted_yp'].apply(lambda x: 0 if x<0 else x)
df_sample.to_csv('../cache/df_sample_{}.csv'.format(play_id),index=False)
