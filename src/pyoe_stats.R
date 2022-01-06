source('lib/helpers.R')
source('lib/load_db.R')
# source('lib/etl_functions.R')
ipak(c('dplyr','tidyr','ggplot2','gganimate','av','ggthemes','stringr','patchwork','ggimage'))
df=read.csv('cache/df_sample_2020.csv',stringsAsFactors = FALSE)%>% group_by(id) %>% arrange((frame)) %>% 
  mutate(pyoe=target-predicted_yp, delta=predicted_yp-lag(predicted_yp,1), second_delta=delta-lag(delta,1)) %>% ungroup()

df_index=plays_index %>%get_game_label() %>% collect()
df_players=players %>% collect()

df_team_poss=df %>% distinct(possesionTeam,id)
df_sum_plays=df %>% group_by(id,possesionTeam=defense) %>% 
  summarise_at(c('pyoe','delta','second_delta'), list(max=function(x)max(x,na.rm = TRUE), 
                 min=function(x)min(x,na.rm = TRUE), mean=function(x)mean(x,na.rm = TRUE),
                 first=first)) %>% 
  inner_join(df_index %>% filter(!is.na(returnerId)),by=c('id'='game_label')) %>% 
  left_join(df_players %>% select(nflId,displayName), by=c('returnerId'='nflId')) %>% 
  rename(pyoe=pyoe_first)

write.csv(df_sum_plays,'cache/pyoe_summary_plays.csv',row.names = FALSE)
## All Returns
df_sum_players =df_sum_plays %>% group_by(returnerId, displayName,possesionTeam) %>% 
  summarise_at(c('pyoe'),list(max=function(x)max(x,na.rm = TRUE), n=function(x)length(x),
                                    min=function(x)min(x,na.rm = TRUE), mean=function(x)mean(x,na.rm = TRUE),
                                    sum=function(x)sum(x,na.rm = TRUE))) 

               
## By return Type
df_sum_players =df_sum_plays %>% 
  group_by(returnerId, displayName,specialTeamsPlayType) %>% 
  summarise_at(c('pyoe'),list(max=function(x)max(x,na.rm = TRUE), n=function(x)length(x),
                              min=function(x)min(x,na.rm = TRUE), mean=function(x)mean(x,na.rm = TRUE),sd=function(x)sd(x,na.rm = TRUE),
                              sum=function(x)sum(x,na.rm = TRUE)))

## By return Type
df_sum_teams =df_sum_plays %>% 
  group_by(specialTeamsPlayType,possesionTeam) %>% 
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
  mutate(pyoe_acum=cumsum(pyoe),n=row_number()) %>% ungroup() %>% group_by(returnerId,specialTeamsPlayType) %>% filter(n()>=10) %>% ungroup() %>% 
  left_join(df_colors,by=c('possesionTeam'='team_abbr')) 
write.csv(df_acum,'cache/pyoe_accumulated.csv',row.names = FALSE)  


ggplot()+geom_line(data= filter(df_acum,specialTeamsPlayType=='Kickoff'),aes(x=n,y=pyoe_acum,color=displayName))+
  geom_text(data= filter(df_acum,specialTeamsPlayType=='Kickoff') %>% group_by(displayName) %>% filter(n==max(n)),aes(x=n,y=pyoe_acum,label=stringr::str_wrap(displayName,width = 10)))+
  scale_x_continuous(n.breaks = 10)+
  guides(color='none')+theme_bw()+theme_patpaitriot()+labs(title="Kick Returner Performance through the 2020 season",y="Accumulated PYOE",x="Attempts",caption="Players with at least 10 attempts in 2020 \n Esteban Castillo @patpAItriot Andrea Casiraghi")->p1;ggsave('output/kr_acum_leaderboard.png',height = 8,width = 12)

ggplot()+geom_line(data= filter(df_acum,specialTeamsPlayType=='Punt'),aes(x=n,y=pyoe_acum,color=displayName))+
  geom_text(data= filter(df_acum,specialTeamsPlayType=='Punt') %>% group_by(displayName) %>% filter(n==max(n)),aes(x=n,y=pyoe_acum,label=stringr::str_wrap(displayName,width = 10)))+
  scale_x_continuous(n.breaks = 10)+
  guides(color='none')+theme_bw()+theme_patpaitriot()+labs(title="Punt Returner Performance through the 2020 season",y="Accumulated PYOE",x="Attempts",caption="Players with at least 10 attempts in 2020 \n Esteban Castillo @patpAItriot Andrea Casiraghi")->p1;ggsave('output/pr_acum_leaderboard.png',height = 8,width = 12)

{ ggplot()+geom_line(data= filter(df_acum,specialTeamsPlayType=='Kickoff'),aes(x=n,y=pyoe_acum,color=team_color))+scale_color_identity()+
      geom_text(data= filter(df_acum,specialTeamsPlayType=='Kickoff') %>% group_by(displayName) %>% filter(n==max(n)),aes(x=n,y=pyoe_acum,label=stringr::str_wrap(displayName,width = 10)))+
    scale_x_continuous(n.breaks = 10)+
    guides(color='none')+theme_bw()+theme_patpaitriot()+labs(title="Kick Returner Leaderboard",y="Accumulated PYOE",x="Attempts")
  }/ {
    ggplot()+geom_line(data= filter(df_acum,specialTeamsPlayType=='Punt'),aes(x=n,y=pyoe_acum,color=team_color))+scale_color_identity()+
      geom_text(data= filter(df_acum,specialTeamsPlayType=='Punt') %>% group_by(displayName) %>% filter(n==max(n)),aes(x=n,y=pyoe_acum,label=stringr::str_wrap(displayName,width = 10)))+
      scale_x_continuous(n.breaks = 10)+
      guides(color='none')+theme_bw()+theme_patpaitriot()+labs(title="Punt Returner Leaderboard",y="Accumulated PYOE",x="Attempts")
  }->p1;ggsave('output/leaderboard.png',height = 20,width = 20)


  # geom_abline(aes(intercept=0,slope=1))

df_colors=nflfastR::teams_colors_logos %>% 
  mutate(team_color3=coalesce(team_color3,team_color),team_color4=coalesce(team_color4,team_color)) %>% 
  group_by(team_color) %>% mutate(rep_c1=n()) %>% ungroup() %>%
  group_by(team_color2) %>% mutate(rep_c2=n()) %>% ungroup() %>%
  group_by(team_color3) %>% mutate(rep_c3=n()) %>% ungroup() %>%
  mutate(team_color=case_when(rep_c1==1~ team_color,
                              rep_c2==1~team_color2,
                              rep_c3==1~team_color3,
                              TRUE~team_color4)) %>% mutate(team_color=ifelse(team_abbr=="GB","#eead1e", team_color))

df_acum_teams=df_sum_plays %>% tidyr::separate(id,c('gameId','playId'),sep = '--') %>% 
  group_by(possesionTeam,specialTeamsPlayType) %>% arrange(gameId,playId) %>% 
  mutate(pyoe_acum=cumsum(pyoe),n=row_number()) %>% ungroup() %>% 
  # filter(possesionTeam=="BUF"& specialTeamsPlayType=="Kickoff") %>% 
  left_join(df_colors,by=c('possesionTeam'='team_abbr')) 
ggplot()+geom_line(data= filter(df_acum_teams,specialTeamsPlayType=='Kickoff'),aes(x=n,y=pyoe_acum,color=team_color))+
  geom_image(data= filter(df_acum_teams,specialTeamsPlayType=='Kickoff') %>% group_by(possesionTeam) %>% filter(n==max(n)),aes(image=team_logo_espn,x=n,y=pyoe_acum ),size=.03, position = position_jitter(width = .4,seed = 18), alpha=.5)+
  # geom_text(data= filter(df_acum_teams,specialTeamsPlayType=='Kickoff') %>% group_by(possesionTeam) %>% filter(n==max(n)),aes(x=n,y=pyoe_acum,label=stringr::str_wrap(possesionTeam,width = 10)))+
  scale_x_continuous(n.breaks = 10)+scale_color_identity()+
  guides(color='none')+theme_bw()+theme_patpaitriot()+labs(title="Kick Returner Leaderboard",y="Accumulated PYOE",x="Attempts",caption="Attempts in 2020 \n Esteban Castillo @patpAItriot Andrea Casiraghi")->p1;ggsave('output/kr_team_leaderboard.png',width =12, height = 8,dpi="retina")


ggplot()+geom_line(data= filter(df_acum_teams,specialTeamsPlayType=='Punt'),aes(x=n,y=pyoe_acum,color=team_color))+
  geom_image(data= filter(df_acum_teams,specialTeamsPlayType=='Punt') %>% group_by(possesionTeam) %>% filter(n==max(n)),aes(image=team_logo_espn,x=n,y=pyoe_acum ),size=.03, position = position_jitter(width = .4,seed = 18), alpha=.5)+
  # geom_text(data= filter(df_acum_teams,specialTeamsPlayType=='Kickoff') %>% group_by(possesionTeam) %>% filter(n==max(n)),aes(x=n,y=pyoe_acum,label=stringr::str_wrap(possesionTeam,width = 10)))+
  scale_x_continuous(n.breaks = 10)+scale_color_identity()+
  guides(color='none')+theme_bw()+theme_patpaitriot()+labs(title="Punt Returner Leaderboard",y="Accumulated PYOE",x="Attempts",caption="Attempts in 2020 \n Esteban Castillo @patpAItriot Andrea Casiraghi")->p1;ggsave('output/pr_team_leaderboard.png',width =12, height = 8,dpi="retina")
