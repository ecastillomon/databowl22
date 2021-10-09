get_convex_plot=function(sf,file_dir="output/sf_plots/"){
  df_plot=sf %>% head(1) %>% pull(players_all) %>% .[[1]] %>%
    tidyr::unnest(tracking_data) %>% st_sf()%>% mutate(frameId=as.integer(frameId))
  # %>%
  #   left_join(df_players,by=c("nflId"))
  # %>% mutate(frameId=as.integer(frameId))
  home=sf %>% pull(homeTeamAbbr) %>% head(1)
  away=sf %>% pull(visitorTeamAbbr) %>% head(1)
  desc=sf %>% pull(playDescription) %>% head(1)

  df_football=df_plot %>% filter(nflId==9999)
  df_plot_off=sf %>% head(1) %>% pull(convex_hull_off)%>% .[[1]]%>% mutate(frameId=as.integer(frameId))
  df_plot_def=sf %>% head(1) %>% pull(convex_hull_def)%>% .[[1]]%>% mutate(frameId=as.integer(frameId))
  # key=sf$
  play_frames=ggplot()+
    geom_sf(aes(geometry=geometry),alpha=.2,data=df_plot_off)+
    geom_sf(aes(geometry=geometry),
            alpha=.2,data=df_plot_def)+
    geom_sf(shape=3,data=df_football)+
    geom_sf(data=df_plot,aes(color=position_type_1,shape=position_type_1),size=3)+
    geom_segment(aes(x=x,y=y,xend=x+v_x,yend=y+v_y),data=df_plot,arrow = arrow(type = "closed",
                                                                               length = unit(0.05, "inches")))+
    theme_classic()+
    labs(
      title = paste0(away," at ", home, ", ",desc),
      subtitle = paste0(" Frame: {frame}"),
      # subtitle = paste0("Outcome: ", outcome),
      # subtitle = paste0(format(ball_data$gameId %>% head(1) %>% substr(1,8) %>% as.Date(tryFormats=c("%Y%m%d")) ,"%B %d %y")),
      caption = "Source: NFL Data Bowl 2020"
    ) +
    transition_time(frameId)+
    ease_aes('linear')

  play_length = length(unique(df_plot$frameId))
  play_anim = animate(
  play_frames,
  fps = 10,
  nframe = play_length,
  # width = 850,
  # height = 500,
  width=1280,height =  720, res = 144,
  end_pause = 10,renderer =  av_renderer())
  anim_save(paste0(file_dir,"convex_hulls-",df_plot$nflId[1],".mp4") , play_anim )
  play_anim

}

get_vornoi_plot=function(sf,file_dir="output/sf_plots/"){

  df_plot_stats=sf %>% head(1) %>% pull(play_stats) %>% .[[1]]
  df_plot=sf %>% head(1) %>% pull(players_all) %>% .[[1]] %>%
    tidyr::unnest(tracking_data) %>% st_sf()%>%
    left_join(df_plot_stats , by=c("frameId")) %>% mutate(frameId=as.integer(frameId))
  # %>% filter(nflId!=9999)
  df_boundary=sf %>% head(1) %>% pull(play_boundary) %>% .[[1]]
  # %>%
  #   left_join(df_players,by=c("nflId"))
  # %>% mutate(frameId=as.integer(frameId))
  home=sf %>% pull(homeTeamAbbr) %>% head(1)
  away=sf %>% pull(visitorTeamAbbr) %>% head(1)
  desc=sf %>% pull(playDescription) %>% head(1)
  down=sf %>% pull(down) %>% head(1)
  togo=sf %>% pull(yardsToGo) %>% head(1)
  pass_result=sf %>% pull(passResult) %>% head(1)
  gm_id=sf %>% pull(gameId) %>% head(1)
  pl_id=sf %>% pull(playId) %>% head(1)
  play_length = length(unique(df_plot$frameId))
  play_title = paste0(away," at ", home, " ",down," & ",togo," [",pass_result,"]" , ", ",desc)
  file_dir=paste0(file_dir,gm_id,"/")
  system(paste0("mkdir -p ",file_dir ))

  df_football=df_plot %>% filter(nflId==9999)
  df_plot_vor=sf %>% head(1) %>% pull(vornoi_all)%>% .[[1]]%>% mutate(frameId=as.integer(frameId)) %>%
    left_join(df_plot%>% st_drop_geometry() %>% distinct(nflId, jerseyNumber,position_type_1,displayName,pass_start,pass_end,play_start) ,by=c("nflId"))
  play_area=df_plot_vor %>% filter(frameId==1) %>% summarise(area=sum(area)) %>% pull(area)
  ##Players
  df_plot_vor %>% mutate(displayName=paste0(jerseyNumber,"\n",displayName)) %>%
    # semi_join(df_plot_stats %>% filter(frameId>=pass_start & frameId<=pass_end), by=c("frameId")) %>%
    {ggplot()+
        geom_area(data=.,aes(x=frameId,y=area, fill=position_type_1))+
        geom_vline(data=distinct(.,displayName,play_start,pass_start,pass_end), aes(xintercept=play_start),linetype=1)+
        geom_vline(data=distinct(.,displayName,pass_start,pass_start,pass_end), aes(xintercept=pass_start),linetype=3)+
        geom_vline(data=distinct(.,displayName,pass_end,pass_start,pass_end), aes(xintercept=pass_end),linetype=3)+
        facet_grid(.~displayName)}+ggsave(paste0(file_dir,"VA-",pl_id,".png"),width = 15,height = 7 )

  ##Stacked
  df_plot_vor %>% mutate(displayName=paste0(jerseyNumber,"\n",displayName)) %>%
    # semi_join(df_plot_stats %>% filter(frameId>=pass_start & frameId<=pass_end), by=c("frameId")) %>%
    {ggplot()+
        geom_area(data=.,aes(x=frameId,y=area, fill=displayName), position = position_stack())+
        geom_text(data=distinct(.,position_type_1,play_start), aes(x=play_start,y=.9*play_area,label="Ball Snap"),angle=90,size=3, hjust=-.01,vjust=1.5)+
        geom_text(data=distinct(.,position_type_1,pass_start), aes(x=pass_start,y=.9*play_area,label="Pass Thrown"),angle=90,size=3, hjust=-.01,vjust=1.5)+
        geom_text(data=distinct(.,position_type_1,pass_end), aes(x=pass_end,y=.9*play_area,label="Pass Arrived"),angle=90,size=3, hjust=-.01,vjust=1.5)+
        geom_vline(data=distinct(.,position_type_1,pass_start,pass_start,pass_end), aes(xintercept=pass_start),linetype=3)+
        geom_vline(data=distinct(.,position_type_1,pass_end,pass_start,pass_end), aes(xintercept=pass_end),linetype=3)+
        geom_vline(data=distinct(.,position_type_1,play_start,pass_start,pass_end), aes(xintercept=play_start),linetype=1)+
        geom_vline(data=distinct(.,position_type_1,pass_start,pass_start,pass_end), aes(xintercept=pass_start),linetype=3)+
        geom_vline(data=distinct(.,position_type_1,pass_end,pass_start,pass_end), aes(xintercept=pass_end),linetype=3)+
        theme_bw()+ labs(
          title = play_title,
          caption = "Source: NFL Data Bowl 2020"
        ) +
        facet_grid(.~position_type_1)}+ggsave(paste0(file_dir,"VA_stacked-",pl_id,".png"),width = 15,height = 7 )
  df_plot_vor %>%group_by(position_type_1,frameId) %>% summarise(area=sum(area),play_start=first(play_start),pass_start=first(pass_start),
                                                                 pass_end=first(pass_end)) %>%ungroup() %>%
    {ggplot()+
        geom_area(data=.,aes(x=frameId,y=area, fill=position_type_1),position = position_stack())+
        geom_vline(data=., aes(xintercept=play_start),linetype=1)+
        geom_vline(data=., aes(xintercept=pass_start),linetype=3)+
        geom_vline(data=., aes(xintercept=pass_end),linetype=3)+
        geom_vline(data=distinct(.,position_type_1,pass_end,pass_start,pass_end), aes(xintercept=pass_end),linetype=3)+
        theme_bw()+ labs(
          title = play_title,
          caption = "Source: NFL Data Bowl 2020"
        ) }+ggsave(paste0(file_dir,"VA-team-",pl_id,".png"),width = 15,height = 7 )
  # df_plot_vor %>%group_by(position_type_1,frameId) %>% summarise(area=sum(area),play_start=first(play_start),pass_start=first(pass_start),
  #                                                                pass_end=first(pass_end)) %>%ungroup() %>%
  #   # semi_join(df_plot_stats %>% filter(frameId>=pass_start & frameId<=pass_end), by=c("frameId")) %>%
  #   {ggplot()+
  #       geom_area(data=.,aes(x=frameId,y=area, fill=position_type_1))+
  #       geom_vline(data=., aes(xintercept=play_start),linetype=1)+
  #       geom_vline(data=., aes(xintercept=pass_start),linetype=3)+
  #       geom_vline(data=., aes(xintercept=pass_end),linetype=3)+
  #       geom_vline(data=distinct(.,position_type_1,pass_end,pass_start,pass_end), aes(xintercept=pass_end),linetype=3)+
  #       theme_bw()+ labs(
  #         title = play_title,
  #         caption = "Source: NFL Data Bowl 2020"
  #       ) +
  #       facet_grid(.~position_type_1)}+ggsave(paste0(file_dir,"VA-team-",pl_id,".png"),width = 15,height = 7 )

tryCatch({
  df_plot_vor_sel=df_plot_vor %>% filter(frameId%in% c(pass_start,pass_end,play_start)) %>%mutate(frameId=factor(frameId,ordered = TRUE,labels = c("Play Start","Pass Start","Pass End")))
  df_plot_sel=df_plot %>% filter(frameId%in% c(pass_start,pass_end,play_start)) %>%mutate(frameId=factor(frameId,ordered = TRUE,labels = c("Play Start","Pass Start","Pass End")))
  df_football_sel=df_football%>% filter(frameId%in% c(pass_start,pass_end,play_start)) %>%mutate(frameId=factor(frameId,ordered = TRUE,labels = c("Play Start","Pass Start","Pass End")))

  ggplot()+
    geom_sf(aes(geometry=geometry,fill=position_type_1,group=frameId),alpha=.2,data=df_plot_vor_sel)+
    # geom_sf(aes(geometry=geometry),
    #         alpha=.2,data=df_plot)+
    geom_sf(aes(group=frameId),shape=3,data=df_football_sel)+
    geom_sf(data=df_plot_sel %>% filter(!is.na(position_type_1)),aes(color=position_type_1,shape=position_type_1,group=frameId),size=3, alpha=.5)+
    geom_text(aes(x=x,y=y,label=jerseyNumber), data=df_plot_sel,vjust=-1,size=3)+
    geom_segment(aes(x=x,y=y,xend=x+v_x,yend=y+v_y,color=position_type_1),data=df_plot_sel,arrow = arrow(type = "closed",
                                                                                                         length = unit(0.05, "inches")), alpha=.3,linetype=2)+
    facet_wrap(frameId~.)+
    theme_classic()+
    labs(
      title = play_title,
      caption = "Source: NFL Data Bowl 2020"
    )+ggsave(paste0(file_dir,"VOR-",pl_id,".png"),width = 15,height = 7 )
},error=function(e){
  print("No Pass")
})





  # key=sf$
  play_frames=ggplot()+
    geom_sf(aes(geometry=geometry,fill=position_type_1,group=frameId),alpha=.2,data=df_plot_vor)+
    # geom_sf(aes(geometry=geometry),
    #         alpha=.2,data=df_plot)+
    geom_sf(aes(group=frameId),shape=3,data=df_football)+
    geom_sf(data=df_plot %>% filter(!is.na(position_type_1)),aes(color=position_type_1,shape=position_type_1,group=frameId),size=3, alpha=.5)+
    geom_text(aes(x=x,y=y,label=jerseyNumber,group=frameId), data=df_plot%>% filter(!is.na(position_type_1)),vjust=-1,size=3)+
    # geom_sf_label(data=df_plot %>% filter(!is.na(position_type_1)) ,aes(label=displayName),size=3, vjust=-1,inherit.aes = FALSE)+
    geom_segment(aes(x=x,y=y,xend=x+v_x,yend=y+v_y,color=position_type_1,group=frameId),data=df_plot,arrow = arrow(type = "closed",
                                                                               length = unit(0.05, "inches")), alpha=.3,linetype=2)+
    theme_classic()+
    labs(
      title = play_title,
      subtitle = paste0(" Frame: {floor(frame*play_length/250)}"),

      caption = "Source: NFL Data Bowl 2020"
    ) +
    transition_states(frameId,transition_length = 2, state_length = 1,
                      wrap = FALSE)
  play_anim = tryCatch({
    animate(
      play_frames,
      fps = 50,
      nframe = 250,
      # width = 15,height = 7,
      width = 1500,
      height =  720,
      # width = 850,
      # height = 500,
      end_pause = 10,
      width=1280,res = 144,

      renderer =  av_renderer())
  },error=function(e){
    animate(
      play_frames,
      fps = 50,
      nframe = 250,
      # width = 15,height = 7,
      width = 1500,
      # height = 500,
      height =  720,
      res = 144,
      renderer =  av_renderer())
  })


  anim_save(paste0(file_dir,"vornoi-",pl_id,".mp4") , play_anim )
  play_anim

}

get_vornoi_plot_pred=function(sf,file_dir="output/sf_plots/"){
  df_plot=sf %>% head(1) %>% pull(players_all) %>% .[[1]] %>%
    tidyr::unnest(tracking_data) %>% st_sf()%>% mutate(frameId=as.integer(frameId))
  # %>%
  #   left_join(df_players,by=c("nflId"))
  # %>% mutate(frameId=as.integer(frameId))
  home=sf %>% pull(homeTeamAbbr) %>% head(1)
  away=sf %>% pull(visitorTeamAbbr) %>% head(1)
  desc=sf %>% pull(playDescription) %>% head(1)

  df_football=df_plot %>% filter(nflId==9999)
  df_plot_vor=sf %>% head(1) %>% pull(vornoi_pred)%>% .[[1]]%>% mutate(frameId=as.integer(frameId))

  # key=sf$
  play_frames=ggplot()+
    geom_sf(aes(geometry=geometry),alpha=.2,data=df_plot_vor)+
    # geom_sf(aes(geometry=geometry),
    #         alpha=.2,data=df_plot)+
    geom_sf(shape=3,data=df_football)+
    geom_sf(data=df_plot,aes(color=position_type_1,shape=position_type_1),size=3)+
    geom_segment(aes(x=x,y=y,xend=x+v_x,yend=y+v_y),data=df_plot,arrow = arrow(type = "closed",
                                                                               length = unit(0.05, "inches")))+
    theme_classic()+
    labs(
      title = paste0(away," at ", home, ", ",desc),
      subtitle = paste0(" Frame: {frame}"),
      # subtitle = paste0("Outcome: ", outcome),
      # subtitle = paste0(format(ball_data$gameId %>% head(1) %>% substr(1,8) %>% as.Date(tryFormats=c("%Y%m%d")) ,"%B %d %y")),
      caption = "Source: NFL Data Bowl 2020"
    ) +
    transition_time(frameId)+
    ease_aes('linear')

  play_length = length(unique(df_plot$frameId))
  play_anim = animate(
    play_frames,
    fps = 10,
    nframe = play_length,
    # width = 850,
    # height = 500,
    width=1280,height =  720, res = 144,
    end_pause = 10,renderer =  av_renderer())
  anim_save(paste0(file_dir,"vornoi-",df_plot$nflId[1],".mp4") , play_anim )
  play_anim

}
get_triangles_plot=function(sf,file_dir="output/sf_plots/"){
  df_plot=sf %>% head(1) %>% pull(players_all) %>% .[[1]] %>%
    tidyr::unnest(tracking_data) %>% st_sf()%>% mutate(frameId=as.integer(frameId))
  home=sf %>% pull(homeTeamAbbr) %>% head(1)
  away=sf %>% pull(visitorTeamAbbr) %>% head(1)
  desc=sf %>% pull(playDescription) %>% head(1)

  df_football=df_plot %>% filter(nflId==9999)
  df_plot_off=sf %>% head(1) %>% pull(triangles_off) %>% .[[1]]%>% mutate(frameId=as.integer(frameId))
  df_plot_def=sf %>% head(1) %>% pull(triangles_def) %>% .[[1]]%>% mutate(frameId=as.integer(frameId))
  # key=sf$
  play_frames=ggplot()+
    geom_sf(aes(geometry=geometry),alpha=.2,data=df_plot_off)+
    geom_sf(aes(geometry=geometry),
            alpha=.2,data=df_plot_def)+
    geom_sf(shape=3,data=df_football)+
    geom_sf(data=df_plot,aes(color=position_type_1,shape=position_type_1),size=3)+
    theme_classic()+
    labs(
      title = paste0(away," at ", home, ", ",desc),
      subtitle = paste0(" Frame: {frame}"),
      # subtitle = paste0("Outcome: ", outcome),
      # subtitle = paste0(format(ball_data$gameId %>% head(1) %>% substr(1,8) %>% as.Date(tryFormats=c("%Y%m%d")) ,"%B %d %y")),
      caption = "Source: NFL Data Bowl 2020"
    ) +
    transition_time(frameId)+
    ease_aes('linear')

  play_length = length(unique(df_plot$frameId))
  play_anim = animate(
    play_frames,
    fps = 10,
    nframe = play_length,
    # width = 850,
    # height = 500,
    width=1280,height =  720, res = 144,
    end_pause = 10,renderer =  av_renderer())
  anim_save(paste0(file_dir,"triangulation-",df_plot$nflId[1],".mp4") , play_anim )
  play_anim

}

get_delaunay_plot=function(sf,file_dir="output/sf_plots/"){

  df_plot=sf %>% head(1) %>% pull(players_all) %>% .[[1]] %>%
    tidyr::unnest(tracking_data) %>% st_sf()%>% mutate(frameId=as.integer(frameId))
  home=sf %>% pull(homeTeamAbbr) %>% head(1)
  away=sf %>% pull(visitorTeamAbbr) %>% head(1)
  desc=sf %>% pull(playDescription) %>% head(1)

  df_football=df_plot %>% filter(nflId==9999)
  df_plot_off=sf %>% head(1) %>% pull(delaunay_triangles_off) %>% .[[1]]%>% mutate(frameId=as.integer(frameId))
  df_plot_def=sf %>% head(1) %>% pull(delaunay_triangles_def) %>% .[[1]]%>% mutate(frameId=as.integer(frameId))
  # key=sf$
  play_frames=ggplot()+
    geom_sf(aes(geometry=geometry),alpha=.2,data=df_plot_off)+
    geom_sf(aes(geometry=geometry),
            alpha=.2,data=df_plot_def)+
    geom_sf(aes(geometry=geometry),shape=3,data=df_football)+
    geom_sf(data=df_plot,aes(geometry=geometry,color=position_type_1,shape=position_type_1),size=3)+
    theme_classic()+
    labs(
      title = paste0(away," at ", home, ", ",desc),
      subtitle = paste0(" Frame: {frame}"),
      # subtitle = paste0("Outcome: ", outcome),
      # subtitle = paste0(format(ball_data$gameId %>% head(1) %>% substr(1,8) %>% as.Date(tryFormats=c("%Y%m%d")) ,"%B %d %y")),
      caption = "Source: NFL Data Bowl 2020"
    ) +
    transition_time(frameId)+
    ease_aes('linear')

  play_length = length(unique(df_plot$frameId))
  play_anim = animate(
    play_frames,
    fps = 10,
    nframe = play_length,
    # width = 850,
    # height = 500,
    width=1280,height =  720, res = 144,
    end_pause = 10,renderer =  av_renderer())
  anim_save(paste0(file_dir,"delaunay-",df_plot$nflId[1],".mp4") , play_anim )
  play_anim

}

get_velocity_plot=function(sf,file_dir="output/sf_plots/"){
  df_plot=sf %>% head(1) %>% pull(players_all) %>% .[[1]] %>%
    tidyr::unnest(tracking_data) %>% st_sf()%>% mutate(frameId=as.integer(frameId))
  # %>%
  #   left_join(df_players,by=c("nflId"))
  # %>% mutate(frameId=as.integer(frameId))
  home=sf %>% pull(homeTeamAbbr) %>% head(1)
  away=sf %>% pull(visitorTeamAbbr) %>% head(1)
  desc=sf %>% pull(playDescription) %>% head(1)

  df_football=df_plot %>% filter(nflId==9999)
  # df_plot_vor=sf %>% head(1) %>% pull(vornoi_all)%>% .[[1]]%>% mutate(frameId=as.integer(frameId))

  # key=sf$
  play_frames=ggplot()+
    # geom_sf(aes(geometry=geometry),alpha=.2,data=df_plot_vor)+
    # geom_sf(aes(geometry=geometry),
    #         alpha=.2,data=df_plot)+
    geom_sf(shape=3,data=df_football)+
    geom_sf(data=df_plot,aes(color=position_type_1,shape=position_type_1),size=3)+
    geom_segment(aes(x=x,y=y,xend=x+v_x,yend=y+v_y),data=df_plot,arrow = arrow(type = "closed",
                                                                               length = unit(0.05, "inches")))+
    # geom_segment(aes(x=x,y=y,xend=x+.01,yend=y+.01),data=df_plot,arrow = arrow(type = "open",
    #                                                                            length = unit(0.05, "inches")))+
    theme_classic()+
    labs(
      title = paste0(away," at ", home, ", ",desc),
      subtitle = paste0(" Frame: {frame}"),
      # subtitle = paste0("Outcome: ", outcome),
      # subtitle = paste0(format(ball_data$gameId %>% head(1) %>% substr(1,8) %>% as.Date(tryFormats=c("%Y%m%d")) ,"%B %d %y")),
      caption = "Source: NFL Data Bowl 2020"
    ) +
    transition_time(frameId)+
    ease_aes('linear')

  play_length = length(unique(df_plot$frameId))
  play_anim = animate(
    play_frames,
    fps = 10,
    nframe = play_length,
    # width = 850,
    # height = 500,
    width=1280,height =  720, res = 144,
    end_pause = 10,renderer =  av_renderer())
  anim_save(paste0(file_dir,"velocity-",df_plot$nflId[1],".mp4") , play_anim )
  play_anim

}


get_football_plot=function(sf,file_dir="output/sf_plots/"){
  df_plot=sf %>% head(1) %>% pull(players_all) %>% .[[1]] %>%
    tidyr::unnest(tracking_data) %>% st_sf()%>% mutate(frameId=as.integer(frameId))
  # %>%
  #   left_join(df_players,by=c("nflId"))
  # %>% mutate(frameId=as.integer(frameId))
  home=sf %>% pull(homeTeamAbbr) %>% head(1)
  away=sf %>% pull(visitorTeamAbbr) %>% head(1)
  desc=sf %>% pull(playDescription) %>% head(1)
  df_football=df_plot %>% filter(nflId==9999) %>% mutate(event_2=ifelse(event=="None",event,"Other"))
  # df_plot_vor=sf %>% head(1) %>% pull(vornoi_all)%>% .[[1]]%>% mutate(frameId=as.integer(frameId))
  # key=sf$
  ggplot()+
    geom_sf(aes(alpha=event_2,color=event,size=2),shape=1,data=df_football)+
    geom_sf_text(aes(alpha=event_2,label=event),data=df_football %>% filter(event!="None") , vjust=-1)+
    # scale_color_manual(values = c("None"="black",""))+
    # geom_sf(data=df_plot,aes(color=position_type_1,shape=position_type_1),size=3)+
    # geom_segment(aes(x=x,y=y,xend=x+v_x,yend=y+v_y),data=df_plot,arrow = arrow(type = "closed",
    #                                                                            length = unit(0.05, "inches")))+
    theme_classic()+
    labs(
      title = paste0(away," at ", home, ", ",desc),
      # subtitle = paste0("Outcome: ", outcome),
      # subtitle = paste0(format(ball_data$gameId %>% head(1) %>% substr(1,8) %>% as.Date(tryFormats=c("%Y%m%d")) ,"%B %d %y")),
      caption = "Source: NFL Data Bowl 2020"
    )


}

get_plot_passingboxscore=function(sf,file_dir="output/passing_boxscore/"){
  df_detail=sf %>%select(playId,gameId, absoluteYardlineNumber) %>%
    inner_join(df_games,by=c("gameId"))
  home=sf %>% pull(homeTeamAbbr) %>% head(1)
  away=sf %>% pull(visitorTeamAbbr) %>% head(1)
  desc=sf %>% pull(playDescription) %>% head(1)
  gameDate=sf %>% pull(gameDate) %>% head(1) %>% as.Date(format="%m/%d/%Y")
  gameId=sf %>% pull(gameId) %>% head(1)
  df_football=sf %>%
    mutate(absoluteyardlineNumber=coalesce(absoluteYardlineNumber,yardlineNumber)) %>%
    filter(!is.na(absoluteYardlineNumber)) %>% ##Filter no plays
    select(playId,gameId,playDescription,playDirection, ball,possessionTeam,absoluteYardlineNumber) %>%
    tidyr::unnest(ball) %>%
    # head(1) %>% pull(ball) %>% .[[1]] %>%
    tidyr::unnest(tracking_data) %>% st_sf()%>% mutate(frameId=as.integer(frameId)) %>%
    mutate(event_2=ifelse(event=="None",event,"Other")) %>%
    mutate(x=x-absoluteYardlineNumber) %>%
    # group_by(playId,gameId,frameId) %>%
    mutate(x=case_when(grepl("left",playDirection)  ~x*-1,
                       TRUE~x )) %>%
    # ungroup() %>%
    group_by(playId,gameId) %>%
    mutate(pass_event=case_when(grepl("pass_forward",event)~"start",
                                grepl("interception",event,event,ignore.case = TRUE)~"interception",
                                grepl("touchdown",event,ignore.case = TRUE)~"touchdown",
                                # grepl("sack|fumbl",event,ignore.case = TRUE)~"sack",
                                 grepl("pass_(arrived|outcome)",event)~"end",

                                 TRUE~"other")  ,

           event=case_when( grepl("interception",event,event,ignore.case = TRUE)~"interception",
                            grepl("touchdown",event,ignore.case = TRUE)~"touchdown",TRUE~event),
           pass_start=min(frameId[grepl("start",pass_event)]),pass_end=min(frameId[grepl("end",pass_event)]),
           pass_event=ifelse(pass_event=="end" & frameId!=pass_end,"other",pass_event),
           complete=!any(grepl("\\wcomplete|interception|sack",event)),
           inair=frameId>pass_start & frameId<pass_end,
           pass_event=case_when(grepl("other",pass_event) & inair~"air",
                                grepl("other",pass_event) & !is.finite(pass_start) & grepl("Other",event_2)~"sack",
                                grepl("other",pass_event) & !inair & frameId>pass_end~"aftercatch",
                                TRUE~pass_event) ) %>%
    filter(any(grepl("ball_snap",event))) %>%
    filter(frameId>=frameId[grepl("ball_snap",event)]) %>%ungroup() %>%
    # filter(frameId>=frameId[event=="pass_forward"] & frameId<=frameId[event%in% c("pass_arrived")]) %>%


    st_set_geometry(NULL) %>%
    st_as_sf(coords = c("x", "y"), remove=FALSE)



     # %>%
  #   left_join(df_players,by=c("nflId"))
  # %>% mutate(frameId=as.integer(frameId))


  # df_plot_vor=sf %>% head(1) %>% pull(vornoi_all)%>% .[[1]]%>% mutate(frameId=as.integer(frameId))
  # key=sf$
  ggplot()+
    # geom_vline(aes(xintercept=0),linetype=2, alpha=.2)+
    geom_sf(aes(alpha=event_2,color=event, size=pass_event, shape=pass_event,fill=complete),data=df_football %>%
              filter(event_2=="Other" | frameId>pass_start))+
              # filter(inair|event_2=="Other"))+
    scale_shape_manual(values = c("start"=25,"end"=24,"other"=1,"interception"=22,"sack"=4,"touchdown"=23,"air"=21,"aftercatch"=3))+
    scale_size_manual(values =  c("start"=1,"end"=2.5,"other"=.5,"interception"=3.5,"sack"=3,"touchdown"=3.5,"air"=.5,"aftercatch"=.7))+
    # scale_alpha_manual(values = c("None"=.1,"Other"=.5))+
    scale_fill_manual(values = c("TRUE"="green","FALSE"="red"))+
    scale_color_viridis_d()+
    # scale_color_brewer(palette = "Spectral")+
    # scale_x_continuous(limits=c(-20,40))+
    # scale_color_brewer(type="div")+
    # geom_sf_text(aes(alpha=pass_event,label=event),data=df_football %>% filter(event!="None") , vjust=-1)+
    facet_col(.~possessionTeam)+
    # scale_color_manual(values = c("None"="black",""))+
    # geom_sf(data=df_plot,aes(color=position_type_1,shape=position_type_1),size=3)+
    # geom_segment(aes(x=x,y=y,xend=x+v_x,yend=y+v_y),data=df_plot,arrow = arrow(type = "closed",
    #                                                                            length = unit(0.05, "inches")))+
    theme_classic()+guides(alpha="none",size="none",fill="none")+labs(color="Event", shape="Pass Event")+
    labs(
      title = paste0(away," at ", home, ", ", gameDate %>% format("%b %d %Y") ),
      subtitle = paste0("Normalized Ball Position in Sample of Passing Plays "),
      # subtitle = paste0(format(ball_data$gameId %>% head(1) %>% substr(1,8) %>% as.Date(tryFormats=c("%Y%m%d")) ,"%B %d %y")),
      caption = "Source: NFL Data Bowl 2020"
    )+ggsave(paste0(file_dir,home,"-",away,"-",gameId,".png"),width = 10,height = 10)


}


#' get_plot_passingboxscore_team_off
#'
#' @param sf with selected teams games
#' @param file_dir
#'
#' @return
#' @export
#'
#' @examples
get_plot_passingboxscore_team_off=function(sf,file_dir="output/team_boxscore/"){
  df_detail=sf %>%select(playId,gameId, absoluteYardlineNumber) %>%
    inner_join(df_games,by=c("gameId"))
  team=sf %>%
    distinct(gameId, homeTeamAbbr) %>%
    group_by(homeTeamAbbr) %>%
    count() %>% filter(n>1) %>%
    pull(homeTeamAbbr)

  home=sf %>% pull(homeTeamAbbr) %>% head(1)
  away=sf %>% pull(visitorTeamAbbr) %>% head(1)
  # desc=sf %>% pull(playDescription) %>% head(1)
  # gameDate=sf %>% pull(gameDate) %>% head(1) %>% as.Date(format="%m/%d/%Y")
  gameId=sf %>% pull(gameId) %>% head(1)
  df_football=sf %>%
    mutate(absoluteyardlineNumber=coalesce(absoluteYardlineNumber,yardlineNumber),
           startingQB=purrr::map_chr(players_off, function(x){
             x %>% filter(position=="QB") %>%
               pull(displayName) %>%
               paste0(collapse = ",")

           }),
           gameDate=gameDate %>% as.Date(format="%m/%d/%Y")) %>%
    filter(!is.na(absoluteYardlineNumber)) %>% ##Filter no plays
    filter(possessionTeam==team) %>%
    mutate(opp=ifelse(grepl(team,homeTeamAbbr),visitorTeamAbbr,homeTeamAbbr)) %>%
    select(playId,gameId,playDescription,playDirection,gameDate, ball,possessionTeam,absoluteYardlineNumber,opp,startingQB) %>%
    tidyr::unnest(ball) %>%
    tidyr::unnest(tracking_data) %>% st_sf()%>%
    mutate(frameId=as.integer(frameId)) %>%
    mutate(event_2=ifelse(event=="None",event,"Other")) %>%
    mutate(x=x-absoluteYardlineNumber) %>%
    # group_by(playId,gameId,frameId) %>%
    mutate(x=case_when(grepl("left",playDirection)  ~x*-1,
                       TRUE~x )) %>%
    # ungroup() %>%
    group_by(playId,gameId) %>%
    mutate(pass_event=case_when(grepl("pass_forward",event)~"start",
                                grepl("interception",event,event,ignore.case = TRUE)~"interception",
                                grepl("touchdown",event,ignore.case = TRUE)~"touchdown",
                                # grepl("sack|fumbl",event,ignore.case = TRUE)~"sack",
                                grepl("pass_(arrived|outcome)",event)~"end",

                                TRUE~"other")  ,
           event=case_when( grepl("interception",event,event,ignore.case = TRUE)~"interception",
                            grepl("touchdown",event,ignore.case = TRUE)~"touchdown",TRUE~event),
           pass_start=min(frameId[grepl("start",pass_event)]),pass_end=min(frameId[grepl("end",pass_event)]),
           pass_event=ifelse(pass_event=="end" & frameId!=pass_end,"other",pass_event),
           complete=!any(grepl("\\wcomplete|interception|sack",event)),
           inair=frameId>pass_start & frameId<pass_end,
           pass_event=case_when(grepl("other",pass_event) & inair~"air",
                                grepl("other",pass_event) & !is.finite(pass_start) & grepl("Other",event_2)~"sack",
                                grepl("other",pass_event) & !inair & frameId>pass_end~"aftercatch",
                                TRUE~pass_event) ) %>%
    filter(any(grepl("ball_snap",event))) %>%
    filter(frameId>=frameId[grepl("ball_snap",event)]) %>%ungroup() %>%
    st_drop_geometry()
    # filter(frameId>=frameId[event=="pass_forward"] & frameId<=frameId[event%in% c("pass_arrived")]) %>%
    # st_set_geometry(NULL) %>%
    # st_as_sf(coords = c("x", "y"), remove=FALSE)



  # %>%
  #   left_join(df_players,by=c("nflId"))
  # %>% mutate(frameId=as.integer(frameId))


  # df_plot_vor=sf %>% head(1) %>% pull(vornoi_all)%>% .[[1]]%>% mutate(frameId=as.integer(frameId))
  # key=sf$
  ggplot(data=df_football %>%
           filter(event_2=="Other" | frameId>pass_start))+
    # geom_vline(aes(xintercept=0),linetype=2, alpha=.2)+
    geom_point(aes(x=x,y=y,alpha=event_2,color=event, size=pass_event, shape=pass_event,fill=complete))+
    geom_label(aes(x=-10,y=-10,label=startingQB), data=df_football %>%
                 group_by(opp,gameDate, startingQB) %>% count() %>%
                 group_by(opp,gameDate) %>%
                 arrange(desc(n)) %>% filter(row_number()==1) %>%
                 ungroup(),size=3)+
    # geom_sf(aes(alpha=event_2,color=event, size=pass_event, shape=pass_event,fill=complete),data=df_football %>%
    #           filter(event_2=="Other" | frameId>pass_start))+
    # filter(inair|event_2=="Other"))+
    scale_shape_manual(values = c("start"=25,"end"=24,"other"=1,"interception"=22,"sack"=4,"touchdown"=23,"air"=21,"aftercatch"=3))+
    scale_size_manual(values =  c("start"=1,"end"=2.5,"other"=.5,"interception"=3.5,"sack"=3,"touchdown"=3.5,"air"=.5,"aftercatch"=.7))+
    # scale_alpha_manual(values = c("None"=.1,"Other"=.5))+
    scale_fill_manual(values = c("TRUE"="green","FALSE"="red"))+
    scale_color_viridis_d()+
    scale_x_continuous(limits = c(-20,40))+
    geom_vline(aes(xintercept=0),linetype=3,alpha=.4)+
    # scale_color_brewer(palette = "Spectral")+
    # scale_x_continuous(limits=c(-20,40))+
    # scale_color_brewer(type="div")+
    # geom_sf_text(aes(alpha=pass_event,label=event),data=df_football %>% filter(event!="None") , vjust=-1)+
    facet_wrap(gameDate~opp)+

    # scale_color_manual(values = c("None"="black",""))+
    # geom_sf(data=df_plot,aes(color=position_type_1,shape=position_type_1),size=3)+
    # geom_segment(aes(x=x,y=y,xend=x+v_x,yend=y+v_y),data=df_plot,arrow = arrow(type = "closed",
    #                                                                            length = unit(0.05, "inches")))+
    theme_classic()+guides(alpha="none",size="none",fill="none")+labs(color="Event", shape="Pass Event")+
    labs(
      title = paste0("Passing Offense: ", team ),
      subtitle = paste0("Normalized Ball Position in Sample of Passing Plays "),
      # subtitle = paste0(format(ball_data$gameId %>% head(1) %>% substr(1,8) %>% as.Date(tryFormats=c("%Y%m%d")) ,"%B %d %y")),
      caption = "Source: NFL Data Bowl 2020"
    )  +ggsave(paste0(file_dir,"off-",team,".png"),width = 10,height = 10)


}
get_plot_passingboxscore_team_def=function(sf,file_dir="output/team_boxscore/"){
  df_detail=sf %>%select(playId,gameId, absoluteYardlineNumber) %>%
    inner_join(df_games,by=c("gameId"))
  team=sf %>%
    distinct(gameId, homeTeamAbbr) %>%
    group_by(homeTeamAbbr) %>%
    count() %>% filter(n>1) %>%
    pull(homeTeamAbbr)

  home=sf %>% pull(homeTeamAbbr) %>% head(1)
  away=sf %>% pull(visitorTeamAbbr) %>% head(1)
  # desc=sf %>% pull(playDescription) %>% head(1)
  gameDate=sf %>% pull(gameDate) %>% head(1) %>% as.Date(format="%m/%d/%Y")
  gameId=sf %>% pull(gameId) %>% head(1)
  df_football=sf %>%
    mutate(absoluteyardlineNumber=coalesce(absoluteYardlineNumber,yardlineNumber),
           startingQB=purrr::map_chr(players_off, function(x){
             x %>% filter(position=="QB") %>%
               pull(displayName) %>%
               paste0(collapse = ",")

           }),
           gameDate=gameDate %>% as.Date(format="%m/%d/%Y")) %>%
    filter(!is.na(absoluteYardlineNumber)) %>% ##Filter no plays
    filter(possessionTeam!=team) %>%
    mutate(opp=ifelse(grepl(team,homeTeamAbbr),visitorTeamAbbr,homeTeamAbbr)) %>%
    select(playId,gameId,playDescription,playDirection,gameDate, ball,possessionTeam,absoluteYardlineNumber,opp,startingQB) %>%
    tidyr::unnest(ball) %>%
    tidyr::unnest(tracking_data) %>% st_sf()%>%
    mutate(frameId=as.integer(frameId)) %>%
    mutate(event_2=ifelse(event=="None",event,"Other")) %>%
    mutate(x=x-absoluteYardlineNumber) %>%
    # group_by(playId,gameId,frameId) %>%
    mutate(x=case_when(grepl("left",playDirection)  ~x*-1,
                       TRUE~x )) %>%
    # ungroup() %>%
    group_by(playId,gameId) %>%
    mutate(pass_event=case_when(grepl("pass_forward",event)~"start",
                                grepl("interception",event,event,ignore.case = TRUE)~"interception",
                                grepl("touchdown",event,ignore.case = TRUE)~"touchdown",
                                # grepl("sack|fumbl",event,ignore.case = TRUE)~"sack",
                                grepl("pass_(arrived|outcome)",event)~"end",

                                TRUE~"other")  ,
           event=case_when( grepl("interception",event,event,ignore.case = TRUE)~"interception",
                            grepl("touchdown",event,ignore.case = TRUE)~"touchdown",TRUE~event),
           pass_start=min(frameId[grepl("start",pass_event)]),pass_end=min(frameId[grepl("end",pass_event)]),
           pass_event=ifelse(pass_event=="end" & frameId!=pass_end,"other",pass_event),
           complete=!any(grepl("\\wcomplete|interception|sack",event)),
           inair=frameId>pass_start & frameId<pass_end,
           pass_event=case_when(grepl("other",pass_event) & inair~"air",
                                grepl("other",pass_event) & !is.finite(pass_start) & grepl("Other",event_2)~"sack",
                                grepl("other",pass_event) & !inair & frameId>pass_end~"aftercatch",
                                TRUE~pass_event) ) %>%
    filter(any(grepl("ball_snap",event))) %>%
    filter(frameId>=frameId[grepl("ball_snap",event)]) %>%ungroup() %>%
    st_drop_geometry()
  # filter(frameId>=frameId[event=="pass_forward"] & frameId<=frameId[event%in% c("pass_arrived")]) %>%
  # st_set_geometry(NULL) %>%
  # st_as_sf(coords = c("x", "y"), remove=FALSE)



  # %>%
  #   left_join(df_players,by=c("nflId"))
  # %>% mutate(frameId=as.integer(frameId))


  # df_plot_vor=sf %>% head(1) %>% pull(vornoi_all)%>% .[[1]]%>% mutate(frameId=as.integer(frameId))
  # key=sf$
  ggplot(data=df_football %>%
           filter(event_2=="Other" | frameId>pass_start))+
    # geom_vline(aes(xintercept=0),linetype=2, alpha=.2)+
    geom_point(aes(x=x,y=y,alpha=event_2,color=event, size=pass_event, shape=pass_event,fill=complete))+
    geom_label(aes(x=-10,y=-10,label=startingQB), data=df_football %>%
                 group_by(opp,gameDate, startingQB) %>% count() %>%
                 group_by(opp,gameDate) %>%
                 arrange(desc(n)) %>% filter(row_number()==1) %>%
                 ungroup(),size=3)+
    # geom_sf(aes(alpha=event_2,color=event, size=pass_event, shape=pass_event,fill=complete),data=df_football %>%
    #           filter(event_2=="Other" | frameId>pass_start))+
    # filter(inair|event_2=="Other"))+
    scale_shape_manual(values = c("start"=25,"end"=24,"other"=1,"interception"=22,"sack"=4,"touchdown"=23,"air"=21,"aftercatch"=3))+
    scale_size_manual(values =  c("start"=1,"end"=2.5,"other"=.5,"interception"=3.5,"sack"=3,"touchdown"=3.5,"air"=.5,"aftercatch"=.7))+
    # scale_alpha_manual(values = c("None"=.1,"Other"=.5))+
    scale_fill_manual(values = c("TRUE"="green","FALSE"="red"))+
    scale_color_viridis_d()+
    scale_x_continuous(limits = c(-20,40))+
    geom_vline(aes(xintercept=0),linetype=3,alpha=.4)+
    # scale_color_brewer(palette = "Spectral")+
    # scale_x_continuous(limits=c(-20,40))+
    # scale_color_brewer(type="div")+
    # geom_sf_text(aes(alpha=pass_event,label=event),data=df_football %>% filter(event!="None") , vjust=-1)+
    facet_wrap(gameDate~opp)+

    # scale_color_manual(values = c("None"="black",""))+
    # geom_sf(data=df_plot,aes(color=position_type_1,shape=position_type_1),size=3)+
    # geom_segment(aes(x=x,y=y,xend=x+v_x,yend=y+v_y),data=df_plot,arrow = arrow(type = "closed",
    #                                                                            length = unit(0.05, "inches")))+
    theme_classic()+guides(alpha="none",size="none",fill="none")+labs(color="Event", shape="Pass Event")+
    labs(
      title = paste0("Passing Defense: ", team ),
      subtitle = paste0("Normalized Ball Position in Sample of Passing Plays "),
      # subtitle = paste0(format(ball_data$gameId %>% head(1) %>% substr(1,8) %>% as.Date(tryFormats=c("%Y%m%d")) ,"%B %d %y")),
      caption = "Source: NFL Data Bowl 2020"
    )   +ggsave(paste0(file_dir,"def-",team,".png"),width = 15,height = 10)


}


#' get_plot_passingboxscore_player
#'
#' @param sf with selected teams games
#' @param file_dir
#'
#' @return
#' @export
#'
#' @examples
get_plot_passingboxscore_player=function(sf,file_dir="output/team_boxscore/"){
  df_detail=sf %>%select(playId,gameId, absoluteYardlineNumber) %>%
    inner_join(df_games,by=c("gameId"))
  team=sf %>%
    distinct(gameId, homeTeamAbbr) %>%
    group_by(homeTeamAbbr) %>%
    count() %>% filter(n>1) %>%
    pull(homeTeamAbbr)

  home=sf %>% pull(homeTeamAbbr) %>% head(1)
  away=sf %>% pull(visitorTeamAbbr) %>% head(1)
  # desc=sf %>% pull(playDescription) %>% head(1)
  # gameDate=sf %>% pull(gameDate) %>% head(1) %>% as.Date(format="%m/%d/%Y")
  gameId=sf %>% pull(gameId) %>% head(1)
  df_football=sf %>%
    mutate(absoluteyardlineNumber=coalesce(absoluteYardlineNumber,yardlineNumber),
           startingQB=purrr::map_chr(players_off, function(x){
             x %>% filter(position=="QB") %>%
               pull(displayName) %>%
               paste0(collapse = ",")

           }),
           gameDate=gameDate %>% as.Date(format="%m/%d/%Y")) %>%
    filter(!is.na(absoluteYardlineNumber)) %>% ##Filter no plays
    filter(possessionTeam==team) %>%
    mutate(opp=ifelse(grepl(team,homeTeamAbbr),visitorTeamAbbr,homeTeamAbbr)) %>%
    select(playId,gameId,playDescription,playDirection,gameDate, ball,possessionTeam,absoluteYardlineNumber,opp,startingQB) %>%
    tidyr::unnest(ball) %>%
    tidyr::unnest(tracking_data) %>% st_sf()%>%
    mutate(frameId=as.integer(frameId)) %>%
    mutate(event_2=ifelse(event=="None",event,"Other")) %>%
    mutate(x=x-absoluteYardlineNumber) %>%
    # group_by(playId,gameId,frameId) %>%
    mutate(x=case_when(grepl("left",playDirection)  ~x*-1,
                       TRUE~x )) %>%
    # ungroup() %>%
    group_by(playId,gameId) %>%
    mutate(pass_event=case_when(grepl("pass_forward",event)~"start",
                                grepl("interception",event,event,ignore.case = TRUE)~"interception",
                                grepl("touchdown",event,ignore.case = TRUE)~"touchdown",
                                # grepl("sack|fumbl",event,ignore.case = TRUE)~"sack",
                                grepl("pass_(arrived|outcome)",event)~"end",

                                TRUE~"other")  ,
           event=case_when( grepl("interception",event,event,ignore.case = TRUE)~"interception",
                            grepl("touchdown",event,ignore.case = TRUE)~"touchdown",TRUE~event),
           pass_start=min(frameId[grepl("start",pass_event)]),pass_end=min(frameId[grepl("end",pass_event)]),
           pass_event=ifelse(pass_event=="end" & frameId!=pass_end,"other",pass_event),
           complete=!any(grepl("\\wcomplete|interception|sack",event)),
           inair=frameId>pass_start & frameId<pass_end,
           pass_event=case_when(grepl("other",pass_event) & inair~"air",
                                grepl("other",pass_event) & !is.finite(pass_start) & grepl("Other",event_2)~"sack",
                                grepl("other",pass_event) & !inair & frameId>pass_end~"aftercatch",
                                TRUE~pass_event) ) %>%
    filter(any(grepl("ball_snap",event))) %>%
    filter(frameId>=frameId[grepl("ball_snap",event)]) %>%ungroup() %>%
    st_drop_geometry()%>%
    ##Get one player per team
    group_by(gameId,startingQB) %>% mutate(n=n()) %>% ungroup() %>%
  group_by(startingQB) %>% mutate(n=unique(gameId) %>% length()) %>% ungroup() %>%
  filter(n==max(n))
  # filter(frameId>=frameId[event=="pass_forward"] & frameId<=frameId[event%in% c("pass_arrived")]) %>%
  # st_set_geometry(NULL) %>%
  # st_as_sf(coords = c("x", "y"), remove=FALSE)



  # %>%
  #   left_join(df_players,by=c("nflId"))
  # %>% mutate(frameId=as.integer(frameId))


  # df_plot_vor=sf %>% head(1) %>% pull(vornoi_all)%>% .[[1]]%>% mutate(frameId=as.integer(frameId))
  # key=sf$
  ggplot(data=df_football %>%
           filter(event_2=="Other" | frameId>pass_start))+
    # geom_vline(aes(xintercept=0),linetype=2, alpha=.2)+
    geom_point(aes(x=x,y=y,alpha=event_2,color=event, size=pass_event, shape=pass_event,fill=complete))+
    # geom_label(aes(x=-10,y=-10,label=startingQB), data=df_football %>%
    #              group_by(opp,gameDate, startingQB) %>% count() %>%
    #              group_by(opp,gameDate) %>%
    #              arrange(desc(n)) %>% filter(row_number()==1) %>%
    #              ungroup(),size=3)+
    # geom_sf(aes(alpha=event_2,color=event, size=pass_event, shape=pass_event,fill=complete),data=df_football %>%
    #           filter(event_2=="Other" | frameId>pass_start))+
    # filter(inair|event_2=="Other"))+
    scale_shape_manual(values = c("start"=25,"end"=24,"other"=1,"interception"=22,"sack"=4,"touchdown"=23,"air"=21,"aftercatch"=3))+
    scale_size_manual(values =  c("start"=1,"end"=2.5,"other"=.5,"interception"=3.5,"sack"=3,"touchdown"=3.5,"air"=.5,"aftercatch"=.7))+
    # scale_alpha_manual(values = c("None"=.1,"Other"=.5))+
    scale_fill_manual(values = c("TRUE"="green","FALSE"="red"))+
    scale_color_viridis_d()+
    scale_x_continuous(limits = c(-20,40))+
    geom_vline(aes(xintercept=0),linetype=3,alpha=.4)+
    # scale_color_brewer(palette = "Spectral")+
    # scale_x_continuous(limits=c(-20,40))+
    # scale_color_brewer(type="div")+
    # geom_sf_text(aes(alpha=pass_event,label=event),data=df_football %>% filter(event!="None") , vjust=-1)+
    facet_wrap(.~startingQB)+

    # scale_color_manual(values = c("None"="black",""))+
    # geom_sf(data=df_plot,aes(color=position_type_1,shape=position_type_1),size=3)+
    # geom_segment(aes(x=x,y=y,xend=x+v_x,yend=y+v_y),data=df_plot,arrow = arrow(type = "closed",
    #                                                                            length = unit(0.05, "inches")))+
    theme_classic()+guides(alpha="none",size="none",fill="none")+labs(color="Event", shape="Pass Event")+
    labs(
      title = paste0("Passing Offense: ", team ),
      subtitle = paste0("Normalized Ball Position in Sample of Passing Plays "),
      # subtitle = paste0(format(ball_data$gameId %>% head(1) %>% substr(1,8) %>% as.Date(tryFormats=c("%Y%m%d")) ,"%B %d %y")),
      caption = "Source: NFL Data Bowl 2020"
    )  +ggsave(paste0(file_dir,"player-",team,".png"),width = 10,height = 10)


}



get_vornoi_analytics_plot=function(sf,file_dir="output/sf_plots/"){


  home=sf %>% pull(homeTeamAbbr) %>% head(1)
  away=sf %>% pull(visitorTeamAbbr) %>% head(1)

  gm_id=sf %>% pull(gameId) %>% head(1)
  pl_id=sf %>% pull(playId) %>% head(1)

  play_title = paste0(away," at ", home)

  file_dir=paste0(file_dir,gm_id,"/")
  system(paste0("mkdir -p ",file_dir ))

  df_team=sf %>%mutate(defTeam=ifelse(possessionTeam==homeTeamAbbr,visitorTeamAbbr,homeTeamAbbr)) %>%
    # filter(possessionTeam=="BAL") %>%
    select(gameId,possessionTeam,personnelO,passResult,personnelD,defTeam,playId,epa, vornoi_stats_team, vornoi_stats_players) %>%
    # filter(!is.na(vornoi_stats_team)) %>%
    tidyr::unnest(cols = c(vornoi_stats_team),keep_empty=TRUE)

  df_players_vor=sf %>%mutate(defTeam=ifelse(possessionTeam==homeTeamAbbr,visitorTeamAbbr,homeTeamAbbr)) %>%
    # filter(possessionTeam=="BAL") %>%
    select(gameId,possessionTeam,defTeam,passResult,personnelO,personnelD,playId,epa, vornoi_stats_team, vornoi_stats_players) %>%
    # filter(!is.na(vornoi_stats_team)) %>%
    tidyr::unnest(cols = c(vornoi_stats_players),keep_empty=TRUE) %>%
    left_join({df_players}, by=c("nflId") )

  df_team %>%
    filter(position_type_1=="defense") %>%
    ggplot(aes(x=area_gained_perc,y=epa))+
    geom_point(aes(shape=passResult,color=passResult),alpha=1)+
    scale_color_brewer(type="qual",palette = "Set1")+theme_bw()+scale_x_continuous(labels = scales::percent_format())+
    geom_hline(aes(yintercept=0),linetype=3,alpha=.5)+
    geom_smooth(method = "lm",se=FALSE)+
    # guides(shape="Pass Result",color="Personnel")+
    # scale_shape_manual(values=c("C"=24,"I"=25,"S"=23,"IN"=23))+

    facet_wrap(.~defTeam,scales = "fixed")+labs(x="Change in Area Controlled (yd2)",y="EPA allowed",
                                                               title="Efectiveness of Giving up terrain",
                                                               subtitle = play_title,
                                                 shape="Pass Result",color="Personnel",
                                                               # caption=TeX("$\\frac{\\text{Area controlled at snap}}{\\text{Area controlled at beginning of pass or sack}}$"))+
                                                               # ylab()+
                                                               caption = "Source: NFL Data Bowl 2020 \n @ecastillomon \n Change in Area controlled is ratio between area controlled at snap, and at moment of pass or sack")+
    ggsave(paste0(file_dir,"area_controlled_epa.png"),width = 15,height = 7 )
  df_team %>%
    filter(position_type_1=="defense") %>%
    ggplot(aes(x=area_gained_perc,y=epa))+
    geom_point(aes(shape=passResult,color=passResult),alpha=.6)+
    scale_color_brewer(type="qual",palette = "Set1")+theme_bw()+scale_x_continuous(labels = scales::percent_format(),limits=c(0,1.1))+
    geom_hline(aes(yintercept=0),linetype=3,alpha=.5)+
    facet_wrap(defTeam~personnelD)+
    geom_smooth(method="lm",se=FALSE)+
    labs(x="Change in Area Controlled (yd2)",y="EPA allowed",
         title="Efectiveness of Giving up terrain",
         subtitle = play_title,
         shape="Pass Result",color="Personnel",
         caption = "Source: NFL Data Bowl 2020 \n @ecastillomon \n Change in Area controlled is ratio between area controlled at snap, and at moment of pass or sack")+
    ggsave(paste0(file_dir,"area_controlled_epa_personnel_all.png"),width = 15,height = 7 )


  df_team %>%
    filter(position_type_1=="defense") %>%
    ggplot(aes(x=area_gained_perc,y=defTeam,color=defTeam))+
    geom_boxplot(outlier.shape= NULL,outlier.alpha = 0)+geom_vline(aes(xintercept=1), alpha=.4)+
    geom_point()+
    scale_x_continuous(labels = scales::percent_format(), limits = c(.25,1))+labs(x="Change in Area Controlled (yd2)",y="Secondary",
                                                                                  caption = "Source: NFL Data Bowl 2020")+
    ggsave(paste0(file_dir,"area_controlled_team.png"),width = 15,height = 7 )


  df_team %>%
    filter(position_type_1=="defense") %>%
    ggplot(aes(x=area_gained_perc,y=personnelD,fill=defTeam))+
    geom_boxplot(outlier.shape= NULL,outlier.alpha = 0,alpha=.5)+geom_vline(aes(xintercept=1), alpha=.4)+
    geom_point(aes(color=epa), position = position_jitter(seed=17, height = .1))+scale_color_gradient2(low="green",mid = "light green",high = "red",midpoint = 0)+
    scale_x_continuous(labels = scales::percent_format(), limits = c(.25,1))+facet_wrap(defTeam~.)+labs(x="Change in Area Controlled (yd2)",y="Personnel",
                                                                                                        caption = "Source: NFL Data Bowl 2020")+
    ggsave(paste0(file_dir,"area_controlled_team_personnel.png"),width = 15,height = 7 )





  df_players_vor %>%
    filter(position_type_1!="defense") %>%
    ggplot(aes(x=area_gained_perc,y=displayName,color=possessionTeam))+
    geom_boxplot(outlier.shape= NULL,outlier.alpha = 0)+geom_vline(aes(xintercept=1), alpha=.4)+
    geom_point()+
    scale_x_continuous(labels = scales::percent_format(), limits = c(0,2))+facet_wrap(defTeam~., scales = "free_y")+
    labs(x="Change in Area Controlled (yd2)",y="Offensive Player",caption = "Source: NFL Data Bowl 2020")+
    ggsave(paste0(file_dir,"area_controlled_player_off.png"),width = 15,height = 7 )
  df_players_vor %>%
    filter(position_type_1=="defense") %>%
    ggplot(aes(x=area_gained_perc,y=displayName,color=position))+
    geom_boxplot(outlier.shape= NULL,outlier.alpha = 0)+geom_vline(aes(xintercept=1), alpha=.4)+
    geom_point()+
    scale_x_continuous(labels = scales::percent_format(), limits = c(0,2))+facet_wrap(defTeam~., scales = "free_y")+
    labs(x="Change in Area Controlled (yd2)",y="Defensive Player",
         caption = "Source: NFL Data Bowl 2020")+
    ggsave(paste0(file_dir,"area_controlled_player_def.png"),width = 15,height = 7 )



  df_players_vor %>%
    filter(position_type_1=="defense") %>%
    ggplot(aes(x=area_gained_perc,y=displayName,color=position))+
    geom_boxplot(outlier.shape= NULL,outlier.alpha = 0)+geom_vline(aes(xintercept=1), alpha=.4)+
    geom_point()+
    scale_x_continuous(labels = scales::percent_format(), limits = c(0,2))+facet_wrap(defTeam~personnelD, scales = "free_y")+
    labs(x="Change in Area Controlled (yd2)",y="Defensive Player",
         caption = "Source: NFL Data Bowl 2020")+
    ggsave(paste0(file_dir,"area_controlled_player_def_personnel.png"),width = 15,height = 7 )


  df_players_vor %>%
    filter(position_type_1=="defense") %>%
    ggplot(aes(x=area_gained_perc,y=epa))+
    geom_point(aes(color=personnelD),alpha=.8)+
    # geom_contour()+
    geom_density2d(alpha=.5,breaks=10)+
    geom_hline(aes(yintercept=0),linetype=3,alpha=.5)+
    scale_x_continuous(labels = scales::percent_format(), limits = c(0,2))+
    geom_smooth(method = "lm", se=FALSE)+
    facet_wrap(defTeam~displayName,scales = "free_x")+
    labs(x="Change in Area Controlled (yd2)",y="EPA",
         caption = "Source: NFL Data Bowl 2020")+
    ggsave(paste0(file_dir,"area_controlled_player_def_epa.png"),width = 15,height = 7 )

  df_players_vor %>%
    filter(position_type_1=="defense") %>%
    ggplot(aes(x=area_first,y=area_last,fill=personnelD,color=personnelD,group=personnelD))+
    geom_point(aes(shape=passResult),alpha=.8)+
    scale_shape_manual(values=c("C"=24,"I"=25,"S"=23,"IN"=23))+
    # geom_contour()+
    geom_density2d(alpha=.5,breaks=10)+
    geom_hline(aes(yintercept=0),linetype=3,alpha=.5)+
    geom_abline(aes(intercept=0,slope=1), alpha=.2)+
    # scale_x_continuous(labels = scales::percent_format(), limits = c(0,2))+
    # geom_smooth(method = "lm", se=FALSE)+
    facet_wrap(defTeam~displayName,scales = "fixed")+labs(x="Area Covered at Snap (yd2)", y="Area Covered at Time of Throw or Sack (yd2)",
                                                    caption = "Source: NFL Data Bowl 2020")+
    ggsave(paste0(file_dir,"area_difference_player_def_epa.png"),width = 15,height = 7 )

  df_players_vor %>%
    filter(position_type_1=="defense") %>%
    ggplot(aes(y=area_first,x=area_gained_perc,color=epa))+
    geom_point(aes(),alpha=.8)+
    # scale_shape_manual(values=c("C"=24,"I"=25,"S"=23,"IN"=23))+
    scale_x_continuous(labels=scales::percent_format())+
    # geom_contour()+
    geom_density2d(alpha=.5,breaks=10)+
    scale_color_gradient2(low="green",mid = "light green",high = "red",midpoint = 0)+
    geom_hline(aes(yintercept=0),linetype=3,alpha=.5)+
    geom_abline(aes(intercept=0,slope=1), alpha=.2)+
    facet_wrap(defTeam~displayName,scales = "free")+labs(x="Area Covered at Snap (yd2)", y="Area Covered at Time of Throw or Sack (yd2)",
                                                          caption = "Source: NFL Data Bowl 2020")
    # ggsave(paste0(file_dir,"area_difference_player_def_epa.png"),width = 15,height = 7 )


}



get_vornoi_analytics_team=function(sf,file_dir="output/sf_plots/",team){




  play_title = paste0(team," Secondary")

  file_dir=paste0(file_dir,team,"/")
  system(paste0("mkdir -p ",file_dir ))

  df_players_vor=df_sample %>%
    mutate(defTeam=ifelse(possessionTeam==homeTeamAbbr,visitorTeamAbbr,homeTeamAbbr)) %>%
    select(gameId,possessionTeam,defTeam,passResult,personnelO,personnelD,playId,epa, vornoi_stats_team, vornoi_stats_players) %>%
    tidyr::unnest(cols = c(vornoi_stats_players),keep_empty=TRUE) %>%
    group_by(gameId,playId) %>% mutate(area_relative_last=area_last/sum(area_last)) %>%
    left_join({df_players}, by=c("nflId") ) %>%
    filter(defTeam==!!team)
  df_players_vor_off=df_sample %>%
    mutate(defTeam=ifelse(possessionTeam==homeTeamAbbr,visitorTeamAbbr,homeTeamAbbr)) %>%
    select(gameId,possessionTeam,defTeam,passResult,personnelO,personnelD,playId,epa, vornoi_stats_team, vornoi_stats_players) %>%
    tidyr::unnest(cols = c(vornoi_stats_players),keep_empty=TRUE) %>%
    group_by(gameId,playId) %>% mutate(area_relative_last=area_last/sum(area_last)) %>%
    left_join({df_players}, by=c("nflId") ) %>%
    filter(possessionTeam==!!team)

  df_team=df_sample %>%
    mutate(defTeam=ifelse(possessionTeam==homeTeamAbbr,visitorTeamAbbr,homeTeamAbbr)) %>%
    select(gameId,possessionTeam,personnelO,passResult,personnelD,defTeam,playId,epa, vornoi_stats_team, vornoi_stats_players) %>%
    tidyr::unnest(cols = c(vornoi_stats_team),keep_empty=TRUE) %>%
    filter(defTeam==!!team)

  df_team %>%
    filter(position_type_1=="defense") %>%
    ggplot(aes(x=area_gained_perc,y=epa))+
    geom_point(aes(shape=passResult,fill=passResult),alpha=.6)+
    scale_fill_brewer(type="qual",palette = "Set1")+theme_bw()+scale_x_continuous(labels = scales::percent_format(),limits=c(0,1.1))+
    geom_hline(aes(yintercept=0),linetype=3,alpha=.5)+
    geom_smooth(method="lm",se=FALSE)+
    # guides(shape="Pass Result",color="Personnel")+
    scale_shape_manual(values=c("C"=24,"I"=25,"S"=23,"IN"=22))+
    # scale_x_continuous(limi)
    # facet_wrap(.~defTeam,scales = "free_x")+
    labs(x="Change in Area Controlled (yd2)",y="EPA allowed",
                                                 title="Efectiveness of Giving up terrain",
                                                 subtitle = play_title,
                                                 shape="Pass Result",color="Personnel",
                                                 # caption=TeX("$\\frac{\\text{Area controlled at snap}}{\\text{Area controlled at beginning of pass or sack}}$"))+
                                                 # ylab()+
                                                 caption = "Source: NFL Data Bowl 2020 \n @ecastillomon \n Change in Area controlled is ratio between area controlled at snap, and at moment of pass or sack")+
    ggsave(paste0(file_dir,"area_controlled_epa.png"),width = 15,height = 7 )
  df_team %>%
    filter(position_type_1=="defense") %>%
    ggplot(aes(x=area_gained_perc,y=epa,color=passResult))+
    geom_point(aes(shape=passResult),alpha=.6)+
    scale_color_brewer(type="qual",palette = "Set1")+theme_bw()+scale_x_continuous(labels = scales::percent_format(),limits=c(0,1.1))+
    geom_hline(aes(yintercept=0),linetype=3,alpha=.5)+
    facet_wrap(personnelD~.)+
    geom_smooth(method="lm",se=FALSE)+
    labs(x="Change in Area Controlled (yd2)",y="EPA allowed",
         title="Efectiveness of Giving up terrain",
         subtitle = play_title,
         shape="Pass Result",color="Personnel",
         caption = "Source: NFL Data Bowl 2020 \n @ecastillomon \n Change in Area controlled is ratio between area controlled at snap, and at moment of pass or sack")+
    ggsave(paste0(file_dir,"area_controlled_epa_personnel.png"),width = 15,height = 7 )
  df_team %>%
    filter(position_type_1=="defense") %>%
    ggplot(aes(x=area_gained_perc,y=epa))+
    geom_point(aes(shape=passResult,color=passResult),alpha=.6)+
    scale_color_brewer(type="qual",palette = "Set1")+theme_bw()+scale_x_continuous(labels = scales::percent_format(),limits=c(0,1.1))+
    geom_hline(aes(yintercept=0),linetype=3,alpha=.5)+
    facet_wrap(personnelD~.)+
    geom_smooth(method="lm",se=FALSE)+
    labs(x="Change in Area Controlled (yd2)",y="EPA allowed",
         title="Efectiveness of Giving up terrain",
         subtitle = play_title,
         shape="Pass Result",color="Personnel",
         caption = "Source: NFL Data Bowl 2020 \n @ecastillomon \n Change in Area controlled is ratio between area controlled at snap, and at moment of pass or sack")+
    ggsave(paste0(file_dir,"area_controlled_epa_personnel_all.png"),width = 15,height = 7 )

  df_team %>%
    filter(position_type_1=="defense") %>%
    ggplot(aes(x=area_gained_perc,y=possessionTeam,color=possessionTeam))+
    geom_boxplot(outlier.shape= NULL,outlier.alpha = 0)+geom_vline(aes(xintercept=1), alpha=.4)+
    geom_point(aes(shape=passResult),alpha=1)+
    scale_x_continuous(labels = scales::percent_format(), limits = c(.25,1))+labs(x="Change in Area Controlled (yd2)",y="Secondary",
                                                                                  caption = "Source: NFL Data Bowl 2020")+
    ggsave(paste0(file_dir,"area_controlled_team.png"),width = 15,height = 7 )


  df_team %>%
    filter(position_type_1=="defense") %>%
    ggplot(aes(x=area_gained_perc,y=personnelD,fill=personnelD))+
    geom_boxplot(outlier.shape= NULL,outlier.alpha = 0,alpha=.5)+geom_vline(aes(xintercept=1), alpha=.2)+theme_bw()+
    geom_point(aes(color=epa), position = position_jitter(seed=17, height = .1))+scale_color_gradient2(low="green",mid = "light green",high = "red",midpoint = 0)+
    scale_x_continuous(labels = scales::percent_format(), limits = c(.25,1))+facet_wrap(defTeam~.)+labs(x="Change in Area Controlled (yd2)",y="Personnel",
                                                                                                        caption = "Source: NFL Data Bowl 2020")+
    ggsave(paste0(file_dir,"area_controlled_team_personnel.png"),width = 15,height = 7 )

  df_team %>%
    filter(position_type_1=="defense") %>%
    ggplot(aes(x=area_gained_perc,y=possessionTeam,fill=passResult))+
    geom_density_ridges(alpha=.75)+
    scale_x_continuous(labels = scales::percent_format(), limits = c(.5,1))
    # geom_boxplot(outlier.shape= NULL,outlier.alpha = 0,alpha=.5)+geom_vline(aes(xintercept=1), alpha=.2)+theme_bw()+
    geom_point(aes(color=epa), position = position_jitter(seed=17, height = .1))+scale_color_gradient2(low="green",mid = "light green",high = "red",midpoint = 0)+
    scale_x_continuous(labels = scales::percent_format(), limits = c(.25,1))+facet_wrap(defTeam~.)+labs(x="Change in Area Controlled (yd2)",y="Personnel",
                                                                                                        caption = "Source: NFL Data Bowl 2020")+
    ggsave(paste0(file_dir,"area_controlled_correlation.png"),width = 15,height = 7 )

  df_players_vor_off %>%
    filter(position_type_1!="defense") %>%filter(displayName=="Julio Jones") %>%
    ggplot(aes(x=area_relative_last,y=defTeam,color=defTeam))+
    geom_boxplot(outlier.shape= NULL,outlier.alpha = 0)+
    # geom_vline(aes(xintercept=1), alpha=.4)+
    geom_point(aes(shape=passResult),alpha=.4)+
    scale_x_continuous(labels = scales::percent_format())+
    # facet_wrap(displayName~., scales = "free")+
    labs(x="Change in Area Controlled (yd2)",y="Offensive Player",
         caption = "Source: NFL Data Bowl 2020")

  df_players_vor_off %>%
    filter(position_type_1!="defense") %>%filter(displayName=="Julio Jones") %>%
    ggplot(aes(x=area_relative_last,y=epa,color=defTeam))+geom_point()+
    # geom_boxplot(outlier.shape= NULL,outlier.alpha = 0)+
    scale_x_continuous(labels = scales::percent_format())+facet_wrap(defTeam~.)+
    geom_smooth(method = "lm", se=FALSE)+
    labs(x="Change in Area Controlled (yd2)",y="Offensive Player",
         caption = "Source: NFL Data Bowl 2020")


  df_players_vor %>%
    filter(position_type_1!="defense") %>%
    ggplot(aes(x=area_gained_perc,y=displayName,color=possessionTeam))+
    geom_boxplot(outlier.shape= NULL,outlier.alpha = 0)+geom_vline(aes(xintercept=1), alpha=.4)+
    geom_point(aes(shape=passResult),alpha=.4)+
    scale_x_continuous(labels = scales::percent_format(), limits = c(0,2))+facet_wrap(possessionTeam~., scales = "free_y")+
    labs(x="Change in Area Controlled (yd2)",y="Offensive Player",
         caption = "Source: NFL Data Bowl 2020")+
    ggsave(paste0(file_dir,"area_controlled_player_off.png"),width = 15,height = 7 )
  df_players_vor %>%
    filter(position_type_1=="defense") %>%
    ggplot(aes(x=area_gained_perc,y=displayName,color=position))+
    geom_boxplot(outlier.shape= NULL,outlier.alpha = 0)+geom_vline(aes(xintercept=1), alpha=.4)+
    geom_point(alpha=.2, position = position_jitter(height = .2))+
    scale_x_continuous(labels = scales::percent_format(), limits = c(0,2))+facet_wrap(possessionTeam~., scales = "free_y")+
    labs(x="Change in Area Controlled (yd2)",y="Defensive Player",
         caption = "Source: NFL Data Bowl 2020")+
    ggsave(paste0(file_dir,"area_controlled_player_def.png"),width = 15,height = 7 )



  df_players_vor %>%
    filter(position_type_1=="defense") %>%
    ggplot(aes(x=area_gained_perc,y=displayName,color=position))+
    geom_boxplot(outlier.shape= NULL,outlier.alpha = 0)+geom_vline(aes(xintercept=1), alpha=.4)+
    geom_point(alpha=.5)+
    scale_x_continuous(labels = scales::percent_format(), limits = c(0,2))+facet_wrap(defTeam~personnelD, scales = "free_y")+
    labs(x="Change in Area Controlled (yd2)",y="Defensive Player",
         caption = "Source: NFL Data Bowl 2020")+
    ggsave(paste0(file_dir,"area_controlled_player_def_personnel.png"),width = 15,height = 7 )

  df_players_vor %>%
    filter(position_type_1=="offense") %>%filter(possessionTeam %in% c("PIT")) %>%
    ggplot(aes(x=area_relative_last,y=epa,color=possessionTeam,group=possessionTeam))+
    geom_point(alpha=.2)+
    # geom_contour()+
    geom_density2d(alpha=.5,breaks=10)+
    geom_hline(aes(yintercept=0),linetype=3,alpha=.5)+
    scale_x_continuous(labels = scales::percent_format(), limits = c(0,1))+
    # geom_smooth(method = "lm", se=FALSE)+
    facet_wrap(.~displayName,scales = "free_x")+
    labs(x="Change in Area Controlled (yd2)",y="EPA",
         caption = "Source: NFL Data Bowl 2020")
    # ggsave(paste0(file_dir,"area_controlled_player_def_epa.png"),width = 15,height = 7 )


  df_players_vor %>%
    filter(position_type_1=="defense") %>%
    ggplot(aes(x=area_gained_perc,y=epa,color=possessionTeam,group=possessionTeam))+
    geom_point(alpha=.2)+
    # geom_contour()+
    geom_density2d(alpha=.5,breaks=10)+
    geom_hline(aes(yintercept=0),linetype=3,alpha=.5)+
    scale_x_continuous(labels = scales::percent_format(), limits = c(0,2))+
    # geom_smooth(method = "lm", se=FALSE)+
    facet_wrap(.~displayName,scales = "free_x")+
    labs(x="Change in Area Controlled (yd2)",y="EPA",
         caption = "Source: NFL Data Bowl 2020")+
    ggsave(paste0(file_dir,"area_controlled_player_def_epa.png"),width = 15,height = 7 )

  df_players_vor %>%
    filter(position_type_1=="defense") %>%
    ggplot(aes(x=area_first,y=area_last,fill=personnelD,color=personnelD,group=personnelD))+
    geom_point(aes(shape=passResult),alpha=.2)+
    scale_shape_manual(values=c("C"=24,"I"=25,"S"=23,"IN"=23))+
    # geom_contour()+
    geom_density2d(alpha=.5,breaks=10)+
    geom_hline(aes(yintercept=0),linetype=3,alpha=.5)+
    geom_abline(aes(intercept=0,slope=1), alpha=.2)+
    # scale_x_continuous(labels = scales::percent_format(), limits = c(0,2))+
    # geom_smooth(method = "lm", se=FALSE)+
    facet_wrap(defTeam~displayName,scales = "fixed")+labs(x="Area Covered at Snap (yd2)", y="Area Covered at Time of Throw or Sack (yd2)",
                                                          caption = "Source: NFL Data Bowl 2020")+
    ggsave(paste0(file_dir,"area_difference_player_def_epa.png"),width = 15,height = 7 )

  df_players_vor %>%
    filter(position_type_1=="defense") %>%
    ggplot(aes(y=area_first,x=area_relative_last,color=epa))+
    geom_point(aes(),alpha=.8)+
    # scale_shape_manual(values=c("C"=24,"I"=25,"S"=23,"IN"=23))+
    scale_x_continuous(labels=scales::percent_format())+
    # geom_contour()+
    geom_density2d(alpha=.5,breaks=10)+
    scale_color_gradient2(low="green",mid = "light green",high = "red",midpoint = 0)+
    geom_hline(aes(yintercept=0),linetype=3,alpha=.5)+
    geom_abline(aes(intercept=0,slope=1), alpha=.2)+
    facet_wrap(defTeam~displayName,scales = "free")+labs(x="Area Covered at Snap (yd2)", y="Area Covered at Time of Throw or Sack (yd2)",
                                                         caption = "Source: NFL Data Bowl 2020")
  # ggsave(paste0(file_dir,"area_difference_player_def_epa.png"),width = 15,height = 7 )


}
