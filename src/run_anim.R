source('lib/helpers.R')
source('lib/load_db.R')
# source('lib/etl_functions.R')
ipak(c('dplyr','tidyr','ggplot2','gganimate','av','ggthemes','stringr'))
df=read.csv('cache/df_sample_2018090903--2956.csv',stringsAsFactors = FALSE)

df %>% 
  mutate(gap=target-predicted_yp, delta=predicted_yp-lag(predicted_yp,1), second_delta=delta-lag(delta,1)) %>%
  ggplot(aes(x=frame,y=predicted_yp)) +geom_point()+
  geom_point(aes(y=target),color='red')+
  geom_point(aes(y=gap),color='green')+
  geom_point(aes(y=delta),color='yellow')+
  # geom_point(aes(y=second_delta),color='brown')+
  geom_smooth()+scale_x_continuous(n.breaks = 30)



df_index=plays_index %>%get_game_label() %>% collect()
df_games=games  %>% collect()
df_players=players %>% collect()
nested_index=df_index %>%
  tidyr::nest(plays=c(everything(),-gameId)) %>%
  left_join(df_games,by=c("gameId"))

play_id='2018090903--2956'
# df_sample=get_team_plays_nested("ATL",play_type="%intercepted%", type="off")
# game_ids=df_index %>% filter(gameId==2018110406) %>% pull(gameId) %>% unique()
# sf_sample=get_play_sf(gameid = game_ids)

df_temp=df_plays %>% 
  # get_game_label() %>%filter(game_label==play_id) %>% 
  filter(gameId==2018090903 & playId==2956) %>%
  collect()

df_football=df_temp %>% filter(displayName=='football')

df_players_temp=df_temp %>% filter(!displayName=='football')
play_title=df_index %>%  filter(gameId==2018090903 & playId==2956) %>% pull(playDescription)
play_length=df_temp %>% summarise(frameId=max(frameId))%>% pull(frameId)
x_label_func=function(x){x-10}
play_frames=ggplot(data=df_players_temp)+
  geom_point(data=df_football,aes(x=x,y=y,group=frameId),shape=3)+
  geom_point(data=df_players_temp,aes(x=x,y=y,group=frameId,shape=team,color=team))+
  geom_vline(aes(xintercept=10),linetype=1,alpha=.85,color='#34eb43')+
  geom_vline(aes(xintercept=110),linetype=1,alpha=.85,color='#34eb43')+
  scale_color_brewer(type='qual',palette = 'Set1')+guides(color='none',shape='none')+
  theme_bw()+
  theme_patpaitriot()+scale_x_continuous(breaks = seq(10,110,by=10),labels = x_label_func)+
  transition_states(frameId,transition_length = 2, state_length = 1,
                    wrap = FALSE)+
  labs(
    title = stringr::str_wrap(play_title,width = 50) ,
    subtitle = paste0(" Frame: {closest_state}"),
    x="Yardline",y='',
    #subtitle = paste0(" Frame: {floor(frame*play_length/1000)}"),
    
    caption = "Source: NFL Data Bowl 2021 \nPY Model \n @ecastillomon @andreacasiragh1"
  ) 
play_anim= animate(
  play_frames,
  fps = 50,
  nframe = 1000,
  width = 1500,
  height =  720,
  width=1280,res = 144,
  renderer = av_renderer())
anim_save(glue::glue('output/{play_id}.mp4') , play_anim )
