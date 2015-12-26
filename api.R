## Require and install libs
package_list <- c('dplyr',
                  'ggplot2',
                  'geosphere')
for(p in package_list) {
    if(!(p %in% rownames(installed.packages()))) install.packages(p, repos='http://cran.rstudio.com', lib='/usr/local/lib/R/site-library/')
    library(p, character.only = TRUE)
}

file <- "data/gifts.csv"
raw_data <- read.csv(file)
raw_data <- sample_n(raw_data, 10000)

northPole <- data.frame(Longitude=0, Latitude=90)
northPoleList <- as.list(northPole)

calcDist <- function(long, lat, reference=northPoleList) {
    distHaversine(c(long, lat), c(reference$Longitude, reference$Latitude))
}

data <- raw_data
data$dist <- sapply(seq(nrow(data)), function(i) calcDist(data$Longitude[i], data$Latitude[i]))
data <- data %>% arrange(-dist, -Weight)

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
    # cluster <- do.call(rbind, list(northPole, cluster, northPole))
    n <- nrow(cluster)
    segments <- data.frame(x=cluster$Longitude[-n],
                           xend=cluster$Longitude[-1],
                           y=cluster$Latitude[-n],
                           yend=cluster$Latitude[-1])
    ggplot(segments, aes(x=x, xend=xend, y=y, yend=yend)) +
        geom_segment(linetype="dashed") +
        geom_point(aes(x, y))
}

calcClusterDistance <- function(cluster) {
    cluster_ <- cluster %>% dplyr::select(Longitude, Latitude)
    cluster_ <- do.call(rbind, list(northPole, cluster_, northPole))
    
    sum(sapply(seq(nrow(cluster_) - 1), function(i) {
        calcDist(cluster_$Longitude[i], cluster_$Latitude[i], cluster_[i+1,])
    }))
}

optimizeCluster <- function(cluster) {
    cluster_ <- cluster # %>% dplyr::select(Longitude, Latitude)
    n <- nrow(cluster_)
    GiftIds <- cluster_$GiftId[which.min(cluster_$dist)]
    while(length(GiftIds) != n) {
        remain <- cluster_ %>% filter(!GiftId %in% GiftIds)
        current <- cluster_[tail(GiftIds, 1) == cluster_$GiftId, ]
        nextDist <- apply(remain, 1, function(row) {
            row <- as.list(row)
            row$Longitude <- as.numeric(row$Longitude)
            row$Latitude <- as.numeric(row$Latitude)
            calcDist(row$Longitude, row$Latitude, current)
        })
        GiftIds <- c(GiftIds, remain$GiftId[which.min(nextDist)])
    }
    
    cluster_ <- cluster_[match(GiftIds, cluster$GiftId), ]
    
    cluster_
}