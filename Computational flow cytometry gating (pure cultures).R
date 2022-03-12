location <- paste('/Users/larswiersma/Documents/Academia/MSc Industrial Engineering and Management/Research Project/Experimental_data/') 

source(paste(location,
             "Analysis/",
             "Flow cytometry triplicate scheme.R",
             sep = ""))

experiment <- "220224_BONCAT pure cultures (growth activity)"
sample <- c(S2)

CSV_file_of_sample <- paste(location, 
                            experiment, 
                            "/Cluster_files/",
                            experiment,
                            ".",
                            "S2",
                            "_clustered.CSV",
                            sep="")

library(readr)
library(dplyr)
library(stringr)

df <- read.csv(text=",")
  
for (well in sample) {
  CSV_file_of_well <- paste(location,
                            experiment,
                            "/CSV_files/",
                            experiment,
                            ".",
                            well,
                            ".CSV",
                            sep="")
  df.bind <- read_csv(CSV_file_of_well)
  names(df.bind) <- str_replace_all(names(df.bind), 
                                    c("-" = ""))
  
  df.bind <- df.bind[c("FSCHLog", 
                       "SSCHLog", 
                       "GRNHLog", 
                       #"YELHLog", 
                       #"REDHLog", 
                       #"NIRHLog", 
                       "RED2HLog" 
                       #"NIR2HLog"
  )] %>%
    filter(RED2HLog >= 0.0001) %>%
    na.omit()
  
  df <- rbind(df, df.bind)
  
}

library(flowClust)

df.flowClust <- flowClust(df, 
                          varNames = c('FSCHLog', 
                                       'SSCHLog', 
                                       'GRNHLog', 
                                       'RED2HLog'),
                          K = 1:12,
                          B = 100,
                          level = 1.00)

vector <- c()

if (max(criterion(df.flowClust, 
                  "BIC")) < 0.0) {
  for (i in criterion(df.flowClust, 
                      "BIC")) {
    fraction <- (max(criterion(df.flowClust, 
                               "BIC")) / i)
    vector <- append(vector, 
                     fraction)
  }
} else {
      for (i in criterion(df.flowClust, 
                          "BIC")) {
        fraction <- (i / max(criterion(df.flowClust, 
                                       "BIC")))
        vector <- append(vector, 
                         fraction)
      }
  }

n <- which(vector == vector[vector >= 0.95][1])

df.flowClust <- flowClust(df, 
                          varNames = c('FSCHLog', 
                                       'SSCHLog', 
                                       'GRNHLog', 
                                       'RED2HLog'),
                          K = n,
                          B = 1000,
                          level = 1.00)

for (entry in c(1:nrow(df))) {
    df$Cluster[entry] <- as.character(df.flowClust@label[entry])
}

write_csv(df, 
          file = CSV_file_of_sample)
