library(dplyr)
library(ggplot2)

file <- "data/gifts.csv"
raw_data <- read.csv(file)
raw_data <- sample_n(raw_data, 10000)

northPole <- data.frame(Longitude=0, Latitude=90)
northPoleList <- as.list(northPole)

calcDist <- function(long, lat, reference=northPoleList) {
    # row <- lapply(as.list((row)), as.numeric)
    sqrt((reference$Longitude - long) ^ 2 + (reference$Latitude - lat) ^ 2)
}

data <- raw_data %>%
    mutate(dist = calcDist(Longitude, Latitude)) %>%
    arrange(-dist, -Weight)

plotMap <- function(data) {
    ggplot(data, aes(Longitude, Latitude, color=factor(cluster))) + 
        geom_point() +
        theme(legend.position='none',
              axis.text.x=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks=element_blank())
}

plotMap(data)