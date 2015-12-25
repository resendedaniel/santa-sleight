source("api.R")

data <- data %>% mutate(picked = FALSE)

currCluster <- 1
while(any(!data$picked)) {
    cat("Cluster #", currCluster, "")
    # New center
    available <- data %>%
        filter(!picked)
    farthest <- as.list(available[1,])
    available <- available %>%
        mutate(farthestDist = calcDist(Longitude, Latitude, reference=farthest)) %>%
        arrange(farthestDist)
    
    currWeight <- 0
    i <- 1
    clusterIds <- integer(0)
    while(currWeight < 1000 & i <= nrow(available)) {
        # cat("currWeight: ", currWeight, " - ", "i: ", i, "\n")
        if(currWeight + available$Weight[i] <= 1000) {
            clusterIds <- c(clusterIds, available$GiftId[i])
            currWeight <- currWeight + available$Weight[i]
        }
        i <- i + 1
    }
    cat("#Gifts: ", length(clusterIds), " | ", "Weight: ", sum(data$Weight[data$GiftId %in% clusterIds]), "\n")
    
    data$picked[data$GiftId %in% clusterIds] <- TRUE
    data$cluster[data$GiftId %in% clusterIds] <- currCluster
    currCluster <- currCluster + 1
}

g <- ggplot(data, aes(Longitude, Latitude, color=factor(cluster))) + 
    geom_point() +
    theme(legend.position='none',
          axis.text.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks=element_blank())

g_facet <- g + facet_wrap(~cluster)

split_cluster <- split(data, data$cluster)

ggplot(data.frame(n=sapply(split_cluster, nrow)), aes(n)) +
    geom_histogram(binwidth=5) +
    ggtitle("Size ofeach cluster")

variance <- sapply(split_cluster, function(x) {
    center <- list(Longitude=mean(x$Longitude), Latitude=mean(x$Latitude))
    dists <- sapply(1:nrow(x), function(i) {
        calcDist(x$Longitude[i], x$Latitude[i], center)
    })
    var(dists)
})

ggplot(data.frame(variance), aes(variance)) +
    geom_histogram(binwidth=5) +
    ggtitle("Variance of each cluster's distance")


