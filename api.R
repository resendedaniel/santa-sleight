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

g <- ggplot(data, aes(Longitude, Latitude)) +
    geom_point(aes(size=sqrt(Weight)), alpha=.5) +
    theme_bw() +
    theme(legend.position="none")
g