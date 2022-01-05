source('lib/helpers.R')
source('lib/load_db.R')
# source('lib/etl_functions.R')
ipak(c('dplyr','tidyr','ggplot2','gganimate','av','ggthemes','stringr','patchwork'))
df=read.csv('cache/df_sample_2020.csv',stringsAsFactors = FALSE)%>% 
  mutate(pyoe=target-predicted_yp, delta=predicted_yp-lag(predicted_yp,1), second_delta=delta-lag(delta,1)) 

df_index=plays_index %>%get_game_label() %>% collect()
df_players=players %>% collect()

df_sum_plays=df %>% group_by(id) %>% 
  summarise_at(c('pyoe','delta','second_delta'), list(max=function(x)max(x,na.rm = TRUE), 
                 min=function(x)min(x,na.rm = TRUE), mean=function(x)mean(x,na.rm = TRUE),
                 first=first)) %>% 
  inner_join(df_index %>% filter(!is.na(returnerId)),by=c('id'='game_label')) %>% 
  left_join(df_players %>% select(nflId,displayName), by=c('returnerId'='nflId')) %>% 
  rename(pyoe=pyoe_first)

write.csv(df_sum_plays,'cache/pyoe_summary_plays.csv',row.names = FALSE)
## All Returns
df_sum_players =df_sum_plays %>% group_by(returnerId, displayName) %>% 
  summarise_at(c('pyoe'),list(max=function(x)max(x,na.rm = TRUE), n=function(x)length(x),
                                    min=function(x)min(x,na.rm = TRUE), mean=function(x)mean(x,na.rm = TRUE),
                                    sum=function(x)sum(x,na.rm = TRUE)))

               
## By return Type
df_sum_players =df_sum_plays %>% 
  group_by(returnerId, displayName,specialTeamsPlayType) %>% 
  summarise_at(c('pyoe'),list(max=function(x)max(x,na.rm = TRUE), n=function(x)length(x),
                              min=function(x)min(x,na.rm = TRUE), mean=function(x)mean(x,na.rm = TRUE),sd=function(x)sd(x,na.rm = TRUE),
                              sum=function(x)sum(x,na.rm = TRUE)))

write.csv(df_sum_players,'cache/pyoe_summary_players.csv',row.names = FALSE)

# nflfastR::

df_sum_players %>% filter(specialTeamsPlayType=='Punt') %>% filter(n>=5) %>% 
  ggplot(aes(x=mean,y=max))+geom_point(aes(size=n,color=displayName))+geom_text(aes(label=stringr::str_wrap(displayName,width = 10) ), vjust=+1.5, alpha=.5)+
  geom_smooth(method='lm',se=FALSE,linetype=2,alpha=.5)+labs(
    #title = "Which Punt Returner is justifying his attempts?",
    title = "Punt Returners by Projected Yards Over Expected at moment of catch", 
     x="Average PYOE",y="Max PYOE",
     caption = "Source: NFL Data Bowl 2021, 2020 season plays \nAt least 5 Punt Returns to be considered \nPY Model \n @ecastillomon @andreacasiragh1")+
  guides(size='none',color='none')+
  theme_bw()+theme_patpaitriot()

df_sum_players %>% filter(specialTeamsPlayType=='Kickoff') %>% filter(n>=5) %>% 
  ggplot(aes(x=mean,y=max))+geom_point(aes(size=n,color=displayName))+geom_text(aes(label=stringr::str_wrap(displayName,width = 10) ), vjust=+1.5, alpha=.5)+
  geom_smooth(method='lm',se=FALSE,linetype=2,alpha=.5)+labs(
    #title = "Which Kick Returner is justifying his attempts?",
   title = "Kick Returners by Projected Yards Over Expected at moment of catch", 
   x="Average PYOE",y="Max PYOE",
   caption = "Source: NFL Data Bowl 2021, 2020 season plays \nAt least 5 Kick Returns to be considered \nPY Model \n @ecastillomon @andreacasiragh1")+
  guides(size='none',color='none')+
  theme_bw()+theme_patpaitriot()

df_acum=df_sum_plays %>% tidyr::separate(id,c('gameId','playId'),sep = '--') %>% 
  group_by(returnerId, displayName,specialTeamsPlayType) %>% arrange(gameId,playId) %>% 
  mutate(pyoe_acum=cumsum(pyoe),n=row_number()) %>% ungroup() %>% filter(n>=5)
write.csv(df_acum,'cache/pyoe_accumulated.csv',row.names = FALSE)  

{ ggplot()+geom_line(data= filter(df_acum,specialTeamsPlayType=='Kickoff'),aes(x=n,y=pyoe_acum,color=displayName))+
      geom_text(data= filter(df_acum,specialTeamsPlayType=='Kickoff') %>% group_by(displayName) %>% filter(n==max(n)),aes(x=n,y=pyoe_acum,label=stringr::str_wrap(displayName,width = 10)))+
    scale_x_continuous(n.breaks = 10)+
    guides(color='none')+theme_bw()+theme_patpaitriot()+labs(title="Kick Returner Leaderboard",y="Accumulated PYOE",x="Attempts")
  }/ {
    ggplot()+geom_line(data= filter(df_acum,specialTeamsPlayType=='Punt'),aes(x=n,y=pyoe_acum,color=displayName))+
      geom_text(data= filter(df_acum,specialTeamsPlayType=='Punt') %>% group_by(displayName) %>% filter(n==max(n)),aes(x=n,y=pyoe_acum,label=stringr::str_wrap(displayName,width = 10)))+
      scale_x_continuous(n.breaks = 10)+
      guides(color='none')+theme_bw()+theme_patpaitriot()+labs(title="Punt Returner Leaderboard",y="Accumulated PYOE",x="Attempts")
  }->p1;ggsave('output/leaderboard.png',height = 20,width = 20)


  # geom_abline(aes(intercept=0,slope=1))
