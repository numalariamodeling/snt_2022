library(dplyr)
library(stringr)
library(ggplot2)
source("input_processor/burkina/functions.R")

rootdir <- "C:\\Users\\kbt4040\\NU-malaria-team Dropbox\\data\\burkina_dhs"
rootdir2 <- file.path(rootdir, "data analysis")

subset_rename <- function (df) {
  df1 <- df %>%
    dplyr::select(hv005, hv024, hv104, hv105, hml12) %>%
    mutate(
      sex = case_when(
        hv104 == 1 ~ "Male",
        hv104 == 2 ~ "Female",
        hv104 == 9 ~ NA_character_
      ),
      net_use = case_when(
        hml12 == 0 ~ 0,
        hml12 == 1 ~ 1,
        hml12 == 2 ~ 1,
        T ~ 0),
      ageg = case_when(
        hv105 <= 5 ~ "U05",
        hv105 <= 10 ~ "U10",
        hv105 <= 20 ~ "U20",
        hv105 == 98 ~ NA_character_,
        T ~ "A20"
      )) %>%
  dplyr::select(weight = hv005, region = hv024, sex, ageg, net_use)
  return(df1)
}

collate_years <- function (years = c(2010, 2014, 2017)) {
  bigdf <- data.frame()
  for (yr in years) {
    df <- get_pr(rootdir2, yr)
    df <- subset_rename(df)
    df$year <- yr
    
    bigdf <- bind_rows(bigdf, df)
  }
  
  return(bigdf)
}

bigdf <- collate_years()
bigdf$ageg <- factor(bigdf$ageg, levels = c("U05", "U10", "U20", "A20"))
net_yr_age <- bigdf %>%
  group_by(year, ageg) %>%
  na.omit() %>%
  summarise(net_use = sum(net_use * weight) / sum(weight),
            n = n()) %>%
  group_by(year) %>%
  mutate(r = net_use/net_use[ageg=="U05"])

net_yr_age %>%
  ggplot() +
  geom_line(aes(x=year, y=net_use, colour=ageg))

net_yr_age %>%
  ggplot() +
  geom_line(aes(x=ageg, y=r, group=year, colour=as.factor(year)))

net_yr_age_region <- bigdf %>%
  group_by(year, ageg, region) %>%
  na.omit() %>%
  summarise(net_use = sum(net_use * weight) / sum(weight),
            n = n()) %>%
  group_by(year, region) %>%
  mutate(r = net_use/net_use[ageg=="U05"])

net_yr_age_region %>%
  ggplot() +
  geom_line(aes(x=ageg, y=r, group=as.factor(region))) +
  facet_wrap(~ year)

data.table::fwrite(net_yr_age, 
                   file.path(rootdir, "intermediate_data", 
                             "net_use_ratio.csv"))
