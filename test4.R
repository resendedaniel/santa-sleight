source("api.R")

data <- data %>% mutate(picked = FALSE)

print("Clustering")
t0 <- proc.time()
clusteringTime <- data.frame(x=numeric(0), t=numeric(0))
currCluster <- 1
while(any(!data$picked)) {
    t1 <- proc.time()
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
    
    data$picked[data$GiftId %in% clusterIds] <- TRUE
    data$cluster[data$GiftId %in% clusterIds] <- currCluster
    
    cat(round(proc.time() - t1, 2), " secs", "\n")
    t <- round(proc.time() - t0, 2)
    clusteringTime <- rbind(clusteringTime,
                            data.frame(x=1 - nrow(available) / nrow(data),
                                       t=t[3]))
    if(currCluster %% 10 == 0) {
        slope <- (clusteringTime$t[currCluster] - clusteringTime$t[currCluster - 5]) / (clusteringTime$x[currCluster] - clusteringTime$x[currCluster - 5])
        g_time<- ggplot(rbind(clusteringTime,
                              data.frame(x=1, t=clusteringTime$t[currCluster] + slope * (1 - clusteringTime$x[currCluster]))),
                        aes(x, y=t)) + geom_line()
        print(g_time)
    }
    
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

g <- plotMap(data)
g_cluster_size <- ggplot(data.frame(cluster_size), aes(cluster_size)) +
    geom_histogram(binwidth=5) +
    ggtitle("Size of each cluster")
g_cluster_variance <- ggplot(data.frame(variance), aes(variance)) +
    geom_histogram(binwidth=5) +
    ggtitle("Variance of each cluster's distance")

print("Optimizing clusters")
t0 <- proc.time()
split_cluster <- sapply(seq(split_cluster), function(i) {
    output <- optimizeCluster(split_cluster[[i]])
    t <- proc.time() - t0
    t <- round(t[3], 1)
    print(paste0(round(i / length(split_cluster) * 100, 2), "% ", t, " seconds"))
    output
}, simplify=FALSE)

deliveries <- do.call(rbind, split_cluster) %>%
    dplyr::rename(TripId = cluster) %>%
    dplyr::select(GiftId, TripId)

write.csv(deliveries, paste0("data/output-", as.numeric(Sys.time()), ".csv"), row.names=FALSE)
