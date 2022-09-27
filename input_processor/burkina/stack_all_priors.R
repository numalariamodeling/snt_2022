library(dplyr)

rootdir <- "C:\\Users\\kbt4040\\NU-malaria-team Dropbox\\"
habmult_minmax_fname <- file.path(rootdir, "projects\\hbhi_burkina\\simulation_output/prevalence/min_max_hab_to_sample_for_archetype_pfpr_v2022.csv")
habmult_df <- data.table::fread(habmult_minmax_fname) %>%
  rename(archetype = DS_Name)

ds_pop_fname <- file.path(rootdir, "projects\\hbhi_burkina\\simulation_inputs\\burkina_DS_pop.csv")
ds_pop <- data.table::fread(ds_pop_fname)
habmult_priors <- ds_pop %>%
  dplyr::select(DS_Name, archetype = seasonality_archetype_2) %>%
  left_join(habmult_df) %>%
  mutate(Param1 = log10(min_hab),
         Param2 = log10(max_hab),
         Var_Name = "Habitat_Multiplier",
         Distribution = "uniform",
         Transform = "exp10") %>%
  dplyr::select(DS_Name, Var_Name, Distribution, Transform, Param1, Param2)

ITN_priors_fname <- "IO/simulation_priors/burkina/ITN_priors.csv"
ITN_priors <- data.table::fread(ITN_priors_fname)
CM_priors_fname <- "IO/simulation_priors/burkina/CM_priors.csv"
CM_priors <- data.table::fread(CM_priors_fname)

priors_df <- bind_rows(habmult_priors, ITN_priors, CM_priors)
# Standardize cases for DS_Name
conv_table <- ds_pop %>%
  dplyr::select(DS = DS_Name) %>%
  mutate(DS_Name = toupper(DS))
priors_df <- priors_df %>%
  left_join(conv_table) %>%
  mutate(DS_Name = if_else(!is.na(DS), DS, DS_Name)) %>%
  left_join(ds_pop %>% 
              dplyr::select(DS_Name, archetype = seasonality_archetype_2)) %>%
  dplyr::select(-DS)

outfile <- file.path("IO/simulation_priors/burkina/all_priors.csv")
data.table::fwrite(priors_df, outfile)
table(priors_df$DS_Name)
