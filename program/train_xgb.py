# To add a new cell, type '# %%'
# To add a new markdown cell, type '# %% [markdown]'

# %%
import importlib
import pandas as pd
import numpy as np
# %%
from xgboost import XGBRegressor
import xgboost as xgb
from sklearn.model_selection import cross_val_score


from sklearn.model_selection import train_test_split

# %%
#df_datamart=pd.read_csv('../cache/df_datamart.csv')

from sklearn import preprocessing
from sklearn.model_selection import KFold
#lbl = preprocessing.LabelEncoder()


feats=df_datamart.drop(columns=['target','id','frame']).columns


df_x, df_y =df_datamart[feats],df_datamart['target']


# %%
x_train, x_test, y_train, y_test = train_test_split(df_x, df_y,random_state=17,shuffle=True, test_size=0.10)


# %%
kfold = KFold(n_splits=5, random_state=17,shuffle=True)


# %%
dtrain = xgb.DMatrix(x_train, label=y_train)
dtest = xgb.DMatrix(x_test, label=y_test)


# %%
#params={'objective': "binary:logistic",'num_boost_round' : 1500, 'early_stopping_rounds' : 100, 'max_depth':3, 'learning_rate':.01 }
params={'objective': "reg:squarederror", 'early_stopping_rounds' : 100, 'max_depth':3, 'colsample_bytree':.3, 'learning_rate':.05 ,'subsample':1,'min_child_weight':1,'n_estimators':300}


# %%
cv_xgb=xgb.cv(dtrain=dtrain, folds=kfold,params=params,num_boost_round = 1500)


# %%
cv_xgb.iloc[358]


# %%
n_optim=np.argmin(cv_xgb['test-rmse-mean']) 


# %%
mod_xgb=xgb.train(dtrain=dtrain,  params=params,num_boost_round=n_optim)


# %%



# %%
mod_xgb.save_model('../model/yp.model')
dtrain.save_binary('../model/x_train.buffer')    
dtest.save_binary('../model/x_test.buffer')    
from joblib import dump,load
dump(feats,'../model/feats.pickle')

# %%
x_test['pred']=mod_xgb.predict(dtest)
x_test['pred']=x_test['pred'].apply(lambda x: 0 if x<0 else x)

# %%
df_datamart.groupby(['denied']).count()


# %%
x_test['predicted_prob'].min()


# %%
from sklearn.metrics import mean_squared_error, mean_absolute_error
mean_squared_error( y_test,x_test['pred'])
mean_absolute_error(y_test,x_test['pred'])

# %%
from sklearn.metrics import roc_auc_score
roc_auc_score( y_test,x_test['predicted_prob'])


# %%
from sklearn.metrics import roc_auc_score
roc_auc_score( y_test,x_test['predicted_prob'])


# %%