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
    ggplot(data, aes(x=Longitude, y=Latitude, color=factor(cluster))) + 
        geom_point() +
        theme(legend.position='none',
              axis.text.x=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks=element_blank())
}

plotCluster <- function(cluster) {
    rownames(cluster) <- NULL
    cluster <- cluster %>% dplyr::select(Longitude, Latitude)
    cluster <- do.call(rbind, list(northPole, cluster, northPole))
    n <- nrow(cluster)
    segments <- data.frame(x=cluster$Longitude[-n],
                           xend=cluster$Longitude[-1],
                           y=cluster$Latitude[-n],
                           yend=cluster$Latitude[-1])
    ggplot(segments, aes(x=x, xend=xend, y=y, yend=yend)) + geom_segment()
}