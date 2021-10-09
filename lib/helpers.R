options(scipen=999)

overThresh=function(x,thresh=1.1, limit=1){
  slider::slide_dbl(x, function(y){
    any( (tail(y,-1)/head(y,1))>thresh )
  }, .after = 21*limit)}

underThresh=function(x,thresh=1.1, limit=1){
  slider::slide_dbl(x, function(y){
    any( (tail(y,-1)/head(y,1))<=thresh )
  }, .after = 21*limit)}

returnThresh=function(x,thresh=1.1, limit=1){
  slider::slide_dbl(x, function(y){
    max( (tail(y,-1)/head(y,1)), na.rm = TRUE )
  }, .after = 21*limit)}

ma_n=function(x,n=4){
  zoo::rollapply(x, FUN=function(x){
    mean(x,na.rm = TRUE)
  }, width=n, align="right", partial=FALSE,fill=NA)
}
median_n= function(x,n=4){
  zoo::rollapply(x, FUN=function(x){
    median(x,na.rm = TRUE)
  }, width=n, align="right", partial=FALSE,fill=NA)
} 

sd_n= function(x,n=4){
  zoo::rollapply(x, FUN=function(x){
    sd(x,na.rm = TRUE)
  }, width=n, align="right", partial=FALSE,fill=NA)
} 
prob_moneyline=function(x){
  x=as.numeric(x)
  x=ifelse(x>.50,-100*x/(1-x),(100*(1-x))/x) }

moneyline_prob=function(x){
  x=as.numeric(x)
  x=ifelse(x<0,-x/(-x+100),100/(x+100))
}

kelly_criterion=function(prob,pay){
  pay=pay-1
  (prob*pay-(1-prob))/pay
}
kelly=function(prob,pay){
  # pay=pay-1
  (prob*pay-(1-prob))/pay
}
# bets %>% select(Home.Money.Line_probability, spread) %>% filter(!is.na(Home.Money.Line_probability)) %>%
#   lm(spread~Home.Money.Line_probability, data=. ) ->reg
# summary(reg)
spread_prob=function(x){
  x=as.numeric(x)
  x=(x*-31.36118)+ 16.24561
}

prob_spread=function(x){
  x=as.numeric(x)
  x=(x-16.24561)/-31.36118
}

pay_2_prob=function(x){
  1/x
}

# Copy data out of R
copy.table <- function(obj, size = 4096) {
  clip <- paste('clipboard-', size, sep = '')
  f <- file(description = clip, open = 'w')
  write.table(obj, f, row.names = FALSE, sep = '\t')
  close(f)  
}

# Paste data into R
paste.table <- function() {
  f <- file(description = 'clipboard', open = 'r')
  df <- read.table(f, sep = '\t', header = TRUE)
  close(f)
  return(df)
}

# multi_metric_numeric =  yardstick::metric_set(yardstick::accuracy, yardstick::kap, yardstick::f_meas)
# multi_metric_prob= yardstick::metric_set(yardstick::roc_auc, yardstick::pr_auc)

# write and read from clipboard - linux -----------------------------------
# wclip <- function(x, sep="\t", row.names=FALSE, col.names=TRUE){
#   con <- pipe("xclip -selection clipboard -i", open="w")
#   write.table(x, con, sep=sep, row.names=row.names, col.names=col.names)
#   close(con)
# }
# 
# rclip <- function(){
#   con <- pipe("xclip -selection clipboard -o")
#   d <- read.delim2(con, stringsAsFactors = FALSE)
#   return(d)
#   close(con)
# }


# getmode -------------------------------------------------------------------------------------------------------------------

# Create the function.
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}



# String de meses
mes.nombres <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio",
                 "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")

mes.df <- data.frame(mes_nombre = mes.nombres, mes_num = 1:12)

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    renv::install(new.pkg)
    # install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
# RoundUp -----------------------------------------------------------------

roundUpNice <- function(x, nice=c(1,2,4,5,6,8,10)) {
  if(length(x) != 1) stop("'x' must be of length 1")
  10^floor(log10(x)) * nice[[which(x <= 10^floor(log10(x)) * nice)[[1]]]]
}


# Find neasest
find_fr <- function(your.number, x) {
  
  which.min(abs(x-your.number))   
  
}

concat_dir_rds=function(x){
  dir=x
  list.files(dir) %>%
    purrr::map_dfr(function(y){
      readRDS(paste0(dir,y))
    })
}