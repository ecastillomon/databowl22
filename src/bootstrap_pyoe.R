ipak(c("bayesboot",'forcats','nflfastR'))
nflfastR::teams_colors_logos
df_bootstrap=df_sum_plays%>%
  group_by(returnerId,specialTeamsPlayType,possesionTeam) %>% 
  filter(n()>=10) %>%
  summarise(pyoe=bayesboot(pyoe,mean,na.rm = TRUE, )$V1) %>% ungroup()


df_rank=df_bootstrap %>%
  filter(specialTeamsPlayType=='Punt') %>% 
  group_by(specialTeamsPlayType,returnerId) %>% summarise(mean_pyoe=median(pyoe)) %>% group_by(specialTeamsPlayType) %>% arrange((mean_pyoe)) %>% 
  mutate(rank=row_number()) %>% ungroup() %>% 
  left_join(df_players %>% select(nflId,displayName), by=c('returnerId'='nflId'))



df_bootstrap %>%  filter(specialTeamsPlayType=='Punt') %>% 
  left_join(df_rank, by=c('returnerId','specialTeamsPlayType')) %>% 
  mutate(displayName=factor(displayName) %>% forcats::fct_reorder(rank)) %>% 
  left_join(nflfastR::teams_colors_logos,by=c('possesionTeam'='team_abbr')) %>% 
  ggplot(aes(x=displayName,y=pyoe,color=team_color))+scale_color_identity()+
    scale_y_continuous(limits = c(-10,25))+
  geom_hline(aes(yintercept=0),linetype=3,alpha=.2)+
  geom_boxplot(outlier.alpha = 0.5)+
  theme_bw()+theme_patpaitriot()+
  theme(axis.text.x = element_text(angle=90), axis.title.y = element_text(colour='black',size=20), axis.title.x.bottom = element_text(colour='black',size=20))+
  labs(title = "Who was the best Punt Returner in 2020?",caption="Punt Returners with at least 10 attempts in 2020 \n Esteban Castillo @patpAItriot Andrea Casiraghi",
       y="Bootstrapped Mean PYOE",x="")->p1;ggsave('output/pr_leaderboard.png',width = 6, height = 4,dpi="retina")
# ->p1



df_rank=df_bootstrap %>%
  filter(specialTeamsPlayType=='Kickoff') %>% 
  group_by(specialTeamsPlayType,returnerId) %>% summarise(mean_pyoe=median(pyoe)) %>% group_by(specialTeamsPlayType) %>% arrange((mean_pyoe)) %>% 
  mutate(rank=row_number()) %>% ungroup() %>% 
  left_join(df_players %>% select(nflId,displayName), by=c('returnerId'='nflId'))



df_bootstrap %>%  filter(specialTeamsPlayType=='Kickoff') %>% 
  left_join(df_rank, by=c('returnerId','specialTeamsPlayType')) %>% 
  mutate(displayName=factor(displayName) %>% forcats::fct_reorder(rank)) %>% 
  left_join(nflfastR::teams_colors_logos,by=c('possesionTeam'='team_abbr')) %>% 
  ggplot(aes(x=displayName,y=pyoe,color=team_color))+scale_color_identity()+
  scale_y_continuous(limits = c(-10,25))+
  geom_hline(aes(yintercept=0),linetype=3,alpha=.2)+
  geom_boxplot(outlier.alpha = 0.5)+
  theme_bw()+theme_patpaitriot()+
  theme(axis.text.x = element_text(angle=90), axis.title.y = element_text(colour='black',size=20), axis.title.x.bottom = element_text(colour='black',size=20))+
  labs(title = "Who was the best Kick Returner in 2020?",caption="Kick Returners with at least 10 attempts in 2020 \n Esteban Castillo @patpAItriot Andrea Casiraghi",
       y="Bootstrapped Mean PYOE",x="")->p1;ggsave('output/kr_leaderboard.png',width = 6, height = 4,dpi="retina")