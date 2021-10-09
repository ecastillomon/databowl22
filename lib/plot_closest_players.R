plot_closest_players=function(df_closest,  player_name="Gilmore"){
  df_dist=df_closest %>% filter(grepl(player_name,displayName))
  file_dir="output/distance_gifs/"
  play_id=df_dist$playId[1]
  game_id=df_dist$gameId[1]

  g1= df_dist%>%
    select(nflId,frameId, displayName, matches("closest.*-nflId")) %>%
    tidyr::pivot_longer(cols = matches("closest"), names_to=c("relation"), values_to="playerId") %>%
    mutate(playerId=as.numeric(playerId),relation=gsub("-nflId","",relation)) %>%
    left_join(df_players , by=c("playerId"="nflId")) %>%
    left_join({
      df_dist %>%
        select(frameId, matches("closest.*-value")) %>%
        tidyr::pivot_longer(cols = matches("closest"), names_to=c("relation"), values_to="distance") %>%
        mutate(relation=gsub("-value","",relation))
    }, by=c("frameId","relation")) %>%
    # group_by(frameId,nflId) %>%
    mutate(closest=grepl("closest_1$",relation)) %>%
    ggplot(aes(x=name,y=distance,fill=closest))+geom_col()+
    scale_fill_manual(values = c("TRUE"="green","FALSE"="red"))+
    # geom_label(aes(x=Inf,y=50,label=paste0("Frame: ",frameId)))+
    transition_time(frameId) +
    labs(title=paste0("Distance to each player on the field:",player_name),
         subtitle="Example playId: {play_id} , gameId: {game_id}  ,FrameId:  {frame}")+
    ease_aes('linear')
  # frameId
  play_length = length(unique(df_dist$frameId))
  play_anim = animate(g1,  renderer =  av_renderer(),
                      width=1280,height =  720, res = 144)
  anim_save(paste0(file_dir,play_id,"_",game_id,"_",player_name,".mp4") , play_anim )
  play_anim

}
