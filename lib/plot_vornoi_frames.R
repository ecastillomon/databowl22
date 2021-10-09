plot_vornoi_frames=function(key){
  df_vornoi %>% filter(playId==key) %>%
    group_by(frameId,playId,team) %>% summarise(area_covered=sum(vornoi)) %>%
    group_by(frameId,playId) %>%
    mutate(perc_possesion=area_covered/sum(area_covered)) %>%
    {
      ggplot(data=.,aes(x=frameId,y=perc_possesion,color=team))+geom_line()+scale_y_continuous(labels = scales::percent_format())+
        labs(x="Frame",y="Percent of Field Possesion" )}->p1
  p1
  df_plays %>% filter(playId==key) %>% filter(team=="football") %>%
    {ggplot(data=.,aes(x=frameId,y=x))+geom_line()+
        # geom_area()+
        scale_y_continuous(labels = scales::comma_format(accuracy = 1))+
        labs(x="Frame",y="Yard Line Possesion",
             # title = .$playDescription %>% head(1),
             # subtitle = paste0(h_team,"--",a_team," ",format(ball_data$gameId %>% head(1) %>% substr(1,8) %>% as.Date(tryFormats=c("%Y%m%d")) ,"%B %d %y")),
             caption = "Source: NFL Next Gen Stats")}->p2
    # labs(x="Frame",y="Yard Line Possesion")
  p1+p2
}


plot_vornoi_poss=function(key){
  df_vornoi %>% filter(playId==key) %>%
    group_by(frameId,playId,team) %>% summarise(area_covered=sum(vornoi)) %>%
    group_by(frameId,playId) %>%
    mutate(perc_possesion=area_covered/sum(area_covered)) %>%
    {
      ggplot(data=.,aes(x=frameId,y=perc_possesion,color=team))+geom_line()+scale_y_continuous(labels = scales::percent_format())+
        labs(x="Frame",y="Percent of Field Possesion",
             title = .$playDescription %>% head(1) )+
        ggsave(filename=paste0("output/plays/",key,"_poss.png"))}}


plot_vornoi_yard=function(key){
    df_plays %>% filter(playId==key) %>% filter(team=="football") %>%
    {ggplot(data=.,aes(x=frameId,y=x))+geom_area()+
        scale_y_continuous(labels = scales::comma_format(accuracy = 1))+
        labs(x="Frame",y="Yard Line Possesion",
             # title = .$playDescription %>% head(1),
             # subtitle = paste0(h_team,"--",a_team," ",format(ball_data$gameId %>% head(1) %>% substr(1,8) %>% as.Date(tryFormats=c("%Y%m%d")) ,"%B %d %y")),
             caption = "Source: NFL Next Gen Stats")+
        ggsave(filename=paste0("output/plays/",key,"_yard.png"))}}
