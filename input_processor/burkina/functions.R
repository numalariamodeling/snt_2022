library(dplyr)

get_kr <- function (rootdir, year) {
  if (year == 2010) {
    dtafile <- file.path(rootdir, "data", "BF_2010_DHS_06192019", "BFKR62DT",
                         "BFKR62FL.DTA")
    return(haven::read_dta(dtafile))
  } else if (year == 2014) {
    dtafile <- file.path(rootdir, "data", "BF_2014_MIS_06192019", "BFKR71DT",
                         "BFKR71FL.DTA")
    return(haven::read_dta(dtafile))
  } else if (year == 2017) {
    dtafile <- file.path(rootdir, "data", "BF_2017-18_MIS_07252019_1531_86355", 
                         "BFKR7ADT", "BFKR7AFL.DTA")
    return(haven::read_dta(dtafile))
  } else if (year == 2003) {
    dtafile <- file.path(rootdir, "data", "BF_2003_DHS_06192019", "BFKR43DT",
                         "BFKR43FL.DTA")
    return(haven::read_dta(dtafile))
  }
  
  stop("Error: invalid year")
}

get_pr <- function (rootdir, year) {
  if (year == 2010) {
    dtafile <- file.path(rootdir, "data", "BF_2010_DHS_06192019", "BFPR62DT",
                         "BFPR62FL.DTA")
    return(haven::read_dta(dtafile))
  } else if (year == 2014) {
    dtafile <- file.path(rootdir, "data", "BF_2014_MIS_08052021", "BFPR71DT",
                         "BFPR71FL.DTA")
    return(haven::read_dta(dtafile))
  } else if (year == 2017) {
    dtafile <- file.path(rootdir, "data", "BF_2017-18_MIS_08052021", "BFPR7ADT",
                         "BFPR7AFL.DTA")
    return(haven::read_dta(dtafile))
  }
  
  stop("Error: invalid year")
}

get_ge <- function (rootdir, year) {
  if (year == 2010) {
    ge_fname <- file.path(rootdir, "data analysis", "data", "BF_2010_DHS_06192019", 
                          "BFGE61FL", "BFGE61FL.shp")
  } else if (year == 2014) {
    ge_fname <- file.path(rootdir, "data analysis", "data", "BF_2014_MIS_06192019", 
                          "BFGE71FL", "BFGE71FL.shp")
  } else if (year == 2017) {
    ge_fname <- file.path(rootdir, "data analysis", "data", 
                          "BF_2017-18_MIS_07252019_1531_86355", 
                          "BFGE7AFL", "BFGE7AFL.shp")
  } else {
    stop("Wrong year.")
  }
  
  return(raster::shapefile(ge_fname))
}

getMode <- function (v) {
  tab <- table(v)
  ind <- which.max(tab)[1]
  return(names(tab)[ind] %>% as.numeric)
}
