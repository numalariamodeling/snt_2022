library(dplyr)

rootdir <- "C:\\Users\\kbt4040\\NU-malaria-team Dropbox\\data\\burkina_dhs"
indir <- file.path(rootdir, "intermediate_data")
outdir <- "IO/simulation_priors"

calc_msd <- function (mean, lower, upper) {
  mlogit <- arm::logit(mean)
  tmp <- (mlogit - arm::logit(lower)) + (arm::logit(upper) - mlogit)
  sdlogit <- tmp/2 / 1.96
  
  return(data.frame(mlogit = mlogit, sdlogit = sdlogit))
}

load_cm_by_ds <- function (indir, year) {
  cm_fname <- paste0("cm_by_ds_", year, ".csv")
  cm_fname <- file.path(indir, cm_fname)
  cm_df <- data.table::fread(cm_fname)
  cm_df$year <- year
  
  return(cm_df)
}

make_cm_priors <- function (year) {
  cm_df <- load_cm_by_ds(indir, year) %>%
    left_join(act_use, by = "year") %>%
    mutate(across(starts_with("cm_cov"),
                  .fns = ~ .x * act_perc))
  cm_priors <- cm_df %>%
    mutate(calc_msd(cm_cov, cm_cov_lci, cm_cov_uci)) %>%
    mutate(Var_Name = paste0("CM_cov_", year),
           Distribution = "normal",
           Transform = "invlogit") %>%
    dplyr::select(DS_Name, Var_Name, Distribution, Transform, 
                  Param1 = mlogit, Param2 = sdlogit)
  
  return(cm_priors)
}

act_use <- data.table::fread(file.path(indir, "act_perc.csv"))

CM_priors <- data.frame()
for (yr in c(2010, 2014, 2017)) {
  CM_priors <- bind_rows(CM_priors, make_cm_priors(yr))
} 

outfile <- file.path(outdir, "burkina", "CM_priors.csv")
data.table::fwrite(CM_priors, outfile)
