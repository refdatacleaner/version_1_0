##This should detect and install missing packages before loading them - hopefully!

list.of.packages <- c("shiny","shinydashboard","shinyjs","shinyalert","markdown","splus2R","DT","data.table","xlsx","jsonlite","readr","XML","rvest","tableHTML","kulife","expss","pracma","shinyBS")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages,function(x){library(x,character.only=TRUE)}) 
