library(DBI)
library(dbplyr)
library(dplyr)
dir="/home/esteban/roboShutdown/"
con = dbConnect(RSQLite::SQLite(), paste0(dir,"data/dataBowl.db"))
df_plays= tbl(con, "df_plays")
games=tbl(con, "games")
plays_index=tbl(con,"plays_index")
players=tbl(con,"players")
