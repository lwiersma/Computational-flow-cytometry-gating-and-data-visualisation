location <- paste('/Users/larswiersma/Documents/Academia/MSc Industrial Engineering and Management/Research Project/Experimental_data/') 
experiment <- "220224_BONCAT pure cultures (growth activity)"

selection <- "S2"
noise <- c()

source(paste(location,
             "Analysis/",
             "Data visualisation (raw plot, back-end).R",
             sep = ""))

source(paste(location,
             "Analysis/",
             "Data visualisation (tidy plot, back-end).R",
             sep = "")) 

rm(list=ls())

