library(dplyr)
library(raster)
library(sf)
library(stringr)

rootdir <- "C:\\Users\\kbt4040\\NU-malaria-team Dropbox\\data\\burkina_dhs"
outdir <- "IO/simulation_priors"

# FUNCTIONS
calc_msd <- function (mean, lower, upper) {
  mlogit <- arm::logit(mean)
  tmp <- (mlogit - arm::logit(lower)) + (arm::logit(upper) - mlogit)
  sdlogit <- tmp/2 / 1.96
  
  return(data.frame(mlogit = mlogit, sdlogit = sdlogit))
}

w_avg <- function (x, w) sum(x * w, na.rm = T) / sum(w, na.rm = T)

load_itn_ras <- function (data_folder, year, crop_ras) {
  fnames <- paste("ITN", year, "use", 
                  c("mean", "lower", "upper"), sep="_") %>%
    paste0(".tif")
  
  ras_folder <- file.path(data_folder, "burkina_rasterfiles")
  
  itn_ras <- lapply(fnames, 
                    function (x) file.path(ras_folder, x) %>% raster)
  itn_ras <- lapply(itn_ras, function (x) crop(x, crop_ras))
  return(itn_ras)
}

calc_DS_itn_msd <- function(itn_ras, pop_ras) {
  ras_bricks <- stack(itn_ras[[1]], itn_ras[[2]], itn_ras[[3]], pop_ras)
  ras_df <- as.data.frame(ras_bricks, xy=T)
  ras_sp <- ras_df[,c("x", "y")] %>%
    SpatialPoints(proj4string = crs(ras_bricks))
  ras_df$DS_Name <- over(ras_sp, shp[,"NOMDEP"])$NOMDEP
  ras_df <- ras_df %>%
    filter(!is.na(DS_Name))
  
  # CALCULATE DISTRICT MSD
  col_tmp <- str_match(colnames(ras_df), "ITN_([0-9]+)_use_(.*)")
  cond <- !is.na(col_tmp[,1])
  colnames(ras_df)[cond] <- paste0("ITN_use_", col_tmp[cond,3])
  ras_df <- ras_df %>%
    group_by(DS_Name) %>%
    summarize(ITN_use_mean = w_avg(ITN_use_mean, ppp),
              ITN_use_lower = w_avg(ITN_use_lower, ppp),
              ITN_use_upper = w_avg(ITN_use_upper, ppp))
  
  DS_msd <- ras_df %>%
    group_by(DS_Name) %>%
    mutate(calc_msd(ITN_use_mean, ITN_use_lower, ITN_use_upper))
  DS_msd$year <- unique(col_tmp[cond,2]) %>% as.numeric
  return(DS_msd)
}

load_pop_ras <- function (data_folder, year) {
  ras_folder <- file.path(data_folder, "burkina_dhs", "covariates")
  fname <- paste0("bfa_ppp_", year, "_1km_Aggregated_UNadj.tif")
  r <- raster(file.path(ras_folder, fname))
  names(r) <- "ppp"
  
  return(r)
}

# INPUT
data_folder <- "C:\\Users\\kbt4040\\NU-malaria-team Dropbox\\data\\"
shp <- file.path(data_folder, "burkina_shapefiles",
                 "Health Districts Burkina",
                 "70ds_from_nmcp_2019_numbered.shp") %>%
  shapefile
shp <- spTransform(shp, CRS("+proj=longlat +datum=WGS84 +no_defs"))

ITN_priors <- data.frame()
years <- c(2010, 2013, 2016, 2019)
for (yr in years) {
  pop_ras <- load_pop_ras(data_folder, yr)
  itn_ras <- load_itn_ras(data_folder, yr, pop_ras)
  
  pop_ras <- pop_ras %>%
    resample(itn_ras[[1]])
  DS_msd_yr <- calc_DS_itn_msd(itn_ras, pop_ras)
  ITN_priors <- bind_rows(ITN_priors, DS_msd_yr)
}

# Combined
ITN_priors <- ITN_priors %>%
  mutate(Var_Name = paste0("ITN_", year),
         Distribution = "normal",
         Transform = "invlogit")
ITN_priors <- ITN_priors %>%
  dplyr::select(DS_Name, Var_Name, Distribution, Transform, Param1 = mlogit,
                Param2 = sdlogit)

outfile <- file.path(outdir, "burkina", "ITN_priors.csv")
data.table::fwrite(ITN_priors, outfile)
