ipak(c('RSQLite','vroom','dplyr','purrr'))
temp_folder="/mnt/01e93028-9cf3-4480-a1c8-8c693bc9b031/Downloads/nfl-databowl/"
df_all=list.files(temp_folder) %>%
  .[grepl("tracking",.)] %>%
  purrr::map_dfr(function(x){
    vroom::vroom(paste0(temp_folder,x))
  })
object.size(df_all) %>% format("Mb")


con <- dbConnect(RSQLite::SQLite(), "data/dataBowl.db")

dbWriteTable(con,"df_plays",df_all, overwrite=TRUE)


plays=vroom::vroom("data/plays.csv")
dbWriteTable(con,"plays_index",plays,overwrite=TRUE)

games=vroom::vroom("data/games.csv")
dbWriteTable(con,"games",games,overwrite=TRUE)

players=vroom::vroom("data/players.csv") %>% rename(position=Position) %>% 
  mutate(position_type_1=case_when(grepl("CB|SS|MLB|FS|OLB|DE|LB|ILB|DB|^S$|NT|DT",position)~"defense",
                                   grepl("WR|QB|TE|RB|FB|HB|LB|ILB|DB|^S$|NT|DT",position)~"offense",
                                   grepl("^K$|^P$|LS",position)~"special teams"))
dbWriteTable(con,"players",players,overwrite=TRUE)
