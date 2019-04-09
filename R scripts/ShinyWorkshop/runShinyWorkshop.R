#Set up your computer to easily interact during "Intro to Shiny" Workshop (May 3, 2017).
#Code will install any missing R packages needed.
#Code will also open HTML page to allow you to follow along with code walkthrough during workshop.

#Install needed packages (if missing):
  needed.packages <- c("shiny","rmarkdown","png")
  install.packages <- needed.packages[!(needed.packages %in% installed.packages()[,"Package"])]
  if(length(install.packages) > 0) install.packages(install.packages, repos="http://cran.rstudio.com/")
  lapply(c(needed.packages,"knitr"), library, character.only = T)

#Run Wlakthrough Shiny example presentation (it will open an interactive HTML page)
  rmarkdown::run("./IntroToShinyWorkshop.Rmd")

