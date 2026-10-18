##########################################################################################
# DIRECTORIES
##########################################################################################
# R CMD check mleirt
# R CMD Rd2pdf mleirt
# R CMD build mleirt --resave-data
library(devtools)
library(roxygen2)
rm(list = ls())
graphics.off()
cat("\014")
##########################################################################################
# FUNCTIONS PACKAGE
##########################################################################################
directory <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(directory)
# usethis::create_package("mleirt")
# usethis::use_data_raw(name='data/mfi.rda')
# file.create("R/data.R")
document()
if ("mleirt" %in% rownames(installed.packages())) remove.packages("mleirt")
install()
library(mleirt)

pkgdown::build_site(getwd())
system("R CMD Rd2pdf . --force --no-preview --output=mleirt.pdf")
if (.Platform$OS.type == "windows") shell.exec("mleirt.pdf") else system("evince mleirt.pdf")
system("R CMD build . --resave-data")

mleirt() # starts the app: the console stays busy until you close it
