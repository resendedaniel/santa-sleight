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

split_cluster <- split(data, data$cluster)
cluster_size <- sapply(split_cluster, nrow)
variance <- sapply(split_cluster, function(x) {
    center <- list(Longitude=mean(x$Longitude), Latitude=mean(x$Latitude))
    dists <- sapply(1:nrow(x), function(i) {
        calcDist(x$Longitude[i], x$Latitude[i], center)
    })
    var(dists)
})

cluster <- split_cluster[[sample(length(split_cluster), 1)]]

g <- plotMap(data)
g_facet <- g + facet_wrap(~cluster)
g_cluster_size <- ggplot(data.frame(cluster_size), aes(cluster_size)) +
    geom_histogram(binwidth=5) +
    ggtitle("Size of each cluster")
g_cluster_variance <- ggplot(data.frame(variance), aes(variance)) +
    geom_histogram(binwidth=5) +
    ggtitle("Variance of each cluster's distance")
g_cluster_example <- plotCluster(cluster)

t <- proc.time()
split_cluster <- sapply(seq(split_cluster), function(i) {
    output <- optimizeCluster(split_cluster[[i]])
    t <- proc.time() - t
    t <- round(t[3], 1)
    print(paste0(round(i / length(split_cluster), 1), "% ", t, " seconds"))
    output
}, simplify=FALSE)

# lapply(split_cluster, plotCluster)
