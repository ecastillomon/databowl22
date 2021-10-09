Sys.setlocale("LC_TIME", "en_US.UTF-8")
source("lib/plot_helpers.R")
library(ggplot2)
library(ggforce)
library(gganimate)
get_vornoi_plot=function(df_play,key,file_dir="output/vornoi_frames/"){
  # print(key)
  play_data = df_play %>% filter(playId==key)
  gameid=play_data %>% pull(gameId) %>% head(1)
  home=play_data %>% pull(homeTeamAbbr) %>% head(1)
  away=play_data %>% pull(visitorTeamAbbr) %>% head(1)
  desc=play_data %>% pull(playDescription) %>% head(1)
  player_data=play_data %>% filter(team!="football")
  ball_data=play_data %>% filter(team=="football")
  outcome=player_data %>%
    filter(!grepl("None",event)) %>%  filter(frameId==max(frameId)) %>% pull(event) %>% unique()
  play_frames = plot_field() +
  geom_voronoi_tile(
    data = player_data %>% filter(x >= 0, x <= 120, y >= 0, y <= 160/3),
    bound = c(0, 120, 0, 160/3),
    mapping = aes(x = x, y = y, fill = team, group = -1L),
    colour = "white",
    size = 0.5,
    alpha = 0.5
  ) +
    # scale_fill_manual(values = colors_df, name = "Team")+
    # away team locations
    geom_point(
      data = player_data %>% filter(team == "away"),
      mapping = aes(x = x, y = y,color = team),
      fill = "#ffffff",
      shape = 21, alpha = 1, size = 6
    ) +
    # away team jersey numbers
    geom_text(
      data = player_data %>% filter(team == "away"),
      mapping = aes(x = x, y = y, label = jerseyNumber,color = team),
      size = 3.5, #family = "mono"
    ) +
    # home team locations
    geom_point(
      data = player_data %>% filter(team == "home"),
      mapping = aes(x = x, y = y,color = team),
      fill = "#ffffff",
      shape = 21, alpha = 1, size = 6
    ) +
    # away team jersey numbers
    geom_text(
      data = player_data %>% filter(team == "home"),
      mapping = aes(x = x, y = y, label = jerseyNumber,color = team),
      size = 3.5, #family = "mono"
    ) +
    # ball location
    geom_point(
      data = ball_data,
      mapping = aes(x = x, y = y),
      fill = "#935e38", color = "#d9d9d9",
      shape = 21, alpha = 1, size = 4
    ) +
    labs(
      title = paste0(away," at ", home, ", ",desc),
      subtitle = paste0("Game Id:",gameid," \n Play Id: ", key," Frame: {frame}"),
      # subtitle = paste0("Outcome: ", outcome),
      # subtitle = paste0(format(ball_data$gameId %>% head(1) %>% substr(1,8) %>% as.Date(tryFormats=c("%Y%m%d")) ,"%B %d %y")),
      caption = "Source: NFL Data Bowl 2020"
    ) +
    # animation stuff
    transition_time(frameId) +
    ease_aes('linear') +
    NULL
  # ensure timing of play matches 10 frames-per-second (h/t NFL Football Ops)
  play_length = length(unique(player_data$frameId))
  play_anim = animate(
    play_frames,
    fps = 10,
    nframe = play_length,
    # width = 850,
    # height = 500,
    width=1280,height =  720, res = 144,
    end_pause = 0,renderer =  av_renderer()
  )
  anim_save(paste0(file_dir,"play_anim_vornoy_key-",key,".mp4") , play_anim )
  play_anim
}



get_delaunay_plot=function(df_play,key,file_dir="output/delaunay_frames/"){
  # print(key)
  play_data = df_play %>% filter(playId==key)
  gameid=play_data %>% pull(gameId) %>% head(1)
  home=play_data %>% pull(homeTeamAbbr) %>% head(1)
  away=play_data %>% pull(visitorTeamAbbr) %>% head(1)
  desc=play_data %>% pull(playDescription) %>% head(1)
  player_data=play_data %>% filter(team!="football")
  ball_data=play_data %>% filter(team=="football")
  outcome=player_data %>%
    filter(!grepl("None",event)) %>%  filter(frameId==max(frameId)) %>% pull(event) %>% unique()
  play_frames = plot_field() +
    geom_delaunay_tile(
      data = player_data %>% filter(x >= 0, x <= 120, y >= 0, y <= 160/3),
      bound = c(0, 120, 0, 160/3),
      mapping = aes(x = x, y = y, fill = team, group = -1L),
      colour = "white",
      size = 0.5,
      alpha = 0.5
    ) +
    # scale_fill_manual(values = colors_df, name = "Team")+
    # away team locations
    geom_point(
      data = player_data %>% filter(team == "away"),
      mapping = aes(x = x, y = y,color = team),
      fill = "#ffffff",
      shape = 21, alpha = 1, size = 6
    ) +
    # away team jersey numbers
    geom_text(
      data = player_data %>% filter(team == "away"),
      mapping = aes(x = x, y = y, label = jerseyNumber,color = team),
      size = 3.5, #family = "mono"
    ) +
    # home team locations
    geom_point(
      data = player_data %>% filter(team == "home"),
      mapping = aes(x = x, y = y,color = team),
      fill = "#ffffff",
      shape = 21, alpha = 1, size = 6
    ) +
    # away team jersey numbers
    geom_text(
      data = player_data %>% filter(team == "home"),
      mapping = aes(x = x, y = y, label = jerseyNumber,color = team),
      size = 3.5, #family = "mono"
    ) +
    # ball location
    geom_point(
      data = ball_data,
      mapping = aes(x = x, y = y),
      fill = "#935e38", color = "#d9d9d9",
      shape = 21, alpha = 1, size = 4
    ) +
    labs(
      title = paste0(away," at ", home, ", ",desc),
      subtitle = paste0("Game Id:",gameid," \n Play Id: ", key," Frame: {frame}"),
      # subtitle = paste0("Outcome: ", outcome),
      # subtitle = paste0(format(ball_data$gameId %>% head(1) %>% substr(1,8) %>% as.Date(tryFormats=c("%Y%m%d")) ,"%B %d %y")),
      caption = "Source: NFL Data Bowl 2020"
    ) +
    # animation stuff
    transition_time(frameId) +
    ease_aes('linear') +
    NULL
  # ensure timing of play matches 10 frames-per-second (h/t NFL Football Ops)
  play_length = length(unique(player_data$frameId))
  play_anim = animate(
    play_frames,
    fps = 10,
    nframe = play_length,
    # width = 850,
    # height = 500,
    width=1280,height =  720, res = 144,
    end_pause = 10,renderer =  av_renderer()
  )
  anim_save(paste0(file_dir,"play_anim_delaunay_key-",key,".mp4") , play_anim )
  play_anim
}

