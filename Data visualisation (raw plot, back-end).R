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

df.raw <- read_csv(CSV_file_of_selection)

i <- 1
while (i <= length(noise)) {
  df.raw$Cluster[df.raw$Cluster == noise[i]] <- paste("Noise", 
                                                      i)
  i <- i + 1
}

for (element in sort(unique(df.raw$Cluster))[1:(length(sort(unique(df.raw$Cluster)))-length(noise))]) {
  df.raw$Cluster[df.raw$Cluster == element] <- which(sort(unique(df.raw$Cluster)) %in% element)
}

library(ggplot2)
library(scales)
library(factoextra)
library(ggsci)

visual <- ggplot(na.omit(df.raw), 
                 aes(GRNHLog, 
                     RED2HLog, 
                     colour = as.factor(Cluster))) +
  geom_point(size=0.250, 
             alpha=0.750) +
  stat_density_2d(aes(fill = ..level..),
                  geom = "polygon",
                  colour = 'white',
                  size=0.250,
                  show.legend = FALSE,
                  alpha=0.225,
                  bins=4) +
  scale_color_manual(values = c('#E64B35FF',
                                '#4DBBD5FF',
                                '#00A087FF',
                                '#3C5488FF',
                                '#F39B7FFF',
                                '#8491B4FF',
                                '#91D1C2FF',
                                '#DC0000FF',
                                '#7E6148FF',
                                '#B09C85FF',
                                '#7A4988',
                                '#FEDE00'
                                )) +
    theme(legend.position = c(.97,.025),
          legend.direction = 'vertical',
          legend.justification = c("right",
                                   "bottom"),
          legend.box.just = "bottom",
          legend.margin = margin(6, 6, 6, 6)) +
    guides(colour = guide_legend(override.aes = list(size = 4))) +
    labs(x="Green Fluorescence (SYTO 9) Intensity",
         y=paste("Red Fluorescence", 
                 if ("RED2HLog" %in% colnames(df.raw)) {
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
      axis.text.x = element_text(size = 17, family='Times'), 
      axis.text.y = element_text(size = 17, family='Times'))
visual
  
ggsave(file = paste(location,
                    experiment,
                    "/Cluster_files/",
                    "Scatter plot of ",  
                    experiment, 
                    ".", 
                    selection, 
                    ".png", 
                    sep=""), 
       dpi=300, 
       width=10, 
       height=10)