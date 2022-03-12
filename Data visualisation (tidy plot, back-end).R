library(readr)
library(dplyr)
library(stringr)

CSV_file_of_selection <- paste(location,
                          experiment,
                          "/Cluster_files/",
                          experiment,
                          ".",
                          selection,
                          "_clustered.CSV",
                          sep="")

df.tidy <- read_csv(CSV_file_of_selection)

i <- 1
while (i <= length(noise)) {
  df.tidy$Cluster[df.tidy$Cluster == noise[i]] <- paste("Noise", 
                                                        i)
  i <- i + 1
}

for (element in sort(unique(df.tidy$Cluster))[1:(length(sort(unique(df.tidy$Cluster)))-length(noise))]) {
  df.tidy$Cluster[df.tidy$Cluster == element] <- which(sort(unique(df.tidy$Cluster)) %in% element)
}

df.tidy <- df.tidy %>%
  filter(!str_detect(df.tidy$Cluster, "Noise"))

library(ggplot2)
library(scales)
library(factoextra)
library(ggsci)

visual <- ggplot(na.omit(df.tidy),
                 aes(GRNHLog, 
                     RED2HLog)) +
  geom_point(size=0.250,
             alpha=0.750, 
             colour = '#102033') +
  stat_density_2d(aes(fill = ..level..),
                  geom = "polygon",
                  colour = 'white',
                  size=0.250,
                  show.legend = FALSE,
                  alpha=0.500,
                  bins=8) +
    theme(legend.position = c(.97,.025),
          legend.direction = 'vertical',
          legend.justification = c("right",
                                   "bottom"),
          legend.box.just = "bottom",
          legend.margin = margin(6, 6, 6, 6)) +
    labs(x="Green Fluorescence (SYTO 9) Intensity",
         y=paste("Red Fluorescence", 
                 if ("RED2HLog" %in% colnames(df.tidy)) {
                   "(BONCAT)"
                 } else{
                   "(PI)"
                 }, 
                 "Intensity"),
         colour="Clusters") +
    scale_x_continuous(breaks = c(0, 1, 2, 3, 4),
                       labels = c(expression("10"^"0"), 
                                  expression("10"^"1"), 
                                  expression("10"^"2"), 
                                  expression("10"^"3"), 
                                  expression("10"^"4")), 
                       limits = c(0, 4)) +
    scale_y_continuous(breaks = c(0, 1, 2, 3, 4),
                       labels = c(expression("10"^"0"), 
                                  expression("10"^"1"), 
                                  expression("10"^"2"), 
                                  expression("10"^"3"), 
                                  expression("10"^"4")), 
                       limits = c(0, 4)) +
theme(axis.title.x = element_text(size = 19, family='Times'), 
      axis.title.y = element_text(size = 19, family='Times'), 
      axis.text.x = element_text(size = 19, family='Times'), 
      axis.text.y = element_text(size = 19, family='Times'))
visual
  
ggsave(file = paste(location,
                    experiment,
                    "/Cluster_files/",
                    "Tidy scatter plot of ",  
                    experiment, 
                    ".", 
                    selection, 
                    ".png", 
                    sep=""), 
       dpi=300, 
       width=10, 
       height=10)