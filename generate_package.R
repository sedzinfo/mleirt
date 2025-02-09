##########################################################################################
# DIRECTORIES
##########################################################################################
# R CMD check melirt
# R CMD Rd2pdf melirt
# R CMD build melirt --resave-data
library(devtools)
library(roxygen2)
directory<-paste0(gsub("generate_package.R","",rstudioapi::getActiveDocumentContext()$path))
setwd(directory)
# usethis::create_package("melirt")
# usethis::use_data_raw(name='data/mfi.rda')
# file.create("R/data.R")
rm(list=c("melirt"))
document()
install()
library(mleirt)
mleirt()


