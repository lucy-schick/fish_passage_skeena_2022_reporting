pkgs_cran <- c(
  # 'raster', #load this dog before dplyr yo
  # 'readwritesqlite',
  'sf',
  'readxl',
  'janitor',
  'leafem',
  'leaflet',
  'kableExtra',
  'httr',
  'RPostgres',
  # 'RPostgreSQL',
  'DBI',
  'magick',
  'bcdata',
  'jpeg',
  'datapasta',
  'knitr',
  'data.table',
  'bookdown',
  'fasstr',
  'tidyhydat',
  'elevatr',
  'rayshader',
  'exifr',
  'english',
  'leaflet.extras',
  'ggdark',
  'geojsonio',
  'pdftools',
  'pagedown',
  'fishbc',
  'chron',
  'roxygen2',
  'devtools',
  'tidyverse'
  # 'analogsea',
  # 'here'
  # rgl,
  # geojsonsf,
  # bit64 ##to make integer column type for pg
  # gert  ##to track git moves
  ##leafpop I think
)

pkgs_gh <- c(#'Envirometrix/plotKML',  #plot kml needed to go ahead of other packages for some reason and wants to reinstall everytime.... not sure why. hash out for nowpoissonconsulting/fwapgr",
  'poissonconsulting/poisspatial',
  'poissonconsulting/fwapgr',
  "newgraphenvironment/fpr",
  "lucy-schick/fishbc@updated_data",
  "poissonconsulting/readwritesqlite" #https://github.com/poissonconsulting/readwritesqlite/issues/47
)

pkgs_all <- c(pkgs_cran,
              pkgs_gh)


# install or upgrade all the packages with pak
if(params$update_packages){
  lapply(pkgs_all,
         pak::pkg_install,
         ask = FALSE)
}

# load all the packages

pkgs_ld <- c(pkgs_cran,
             basename(pkgs_gh))

lapply(pkgs_ld,
       require,
       character.only = TRUE)
