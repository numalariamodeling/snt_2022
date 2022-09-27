library(dplyr)
library(stringr)
library(ggplot2)
source("input_processor/burkina/functions.R")

rootdir <- "C:\\Users\\kbt4040\\NU-malaria-team Dropbox\\data\\burkina_dhs"
rootdir2 <- file.path(rootdir, "data analysis")

mltaken <- function(df) {
  mlt1 <- df %>%
    dplyr::select(ml13a:ml13h) %>%
    rowSums(na.rm = T)
  mlt2 <- df %>%
    dplyr::select(ml13a:ml13h) %>%
    apply(1, \(x) all(is.na(x) | x == 9))
  mlt1[mlt2] <- NA
  mlt1 <- as.numeric(mlt1 >= 1)
  
  return(mlt1)
}

acttaken <- function(df) {
  actt <- df %>%
    dplyr::select(contains(act)) %>%
    apply(1, \(x) sum(x == 1, na.rm = T))
  
  return(actt)
}

non_act <- c("ml13a", "ml13b", "ml13c", "ml13d", "ml13da",
             "ml13g", "ml13h")
act <- c("ml13e", "ml13aa", "ml13ab", "ml13f")

resdf <- data.frame()
for (yr in c(2010, 2014, 2017)) {
  df <- get_kr(rootdir2,yr)
  tmp <- df[df$h22==1 & df$h32z==1,]
  tmp$mltaken <- mltaken(tmp)
  tmp$acttaken <- acttaken(tmp)
  
  res <- tmp %>%
    filter(mltaken == 1) %>%
    summarise(act_perc = mean(acttaken))
  res$year <- yr
  resdf <- bind_rows(resdf, res)
}

data.table::fwrite(resdf, 
                   file.path(rootdir, "intermediate_data", 
                             "act_perc.csv"))
