source("api.R")

data <- readData(200)
data <- processData(data)

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
    available$farthestDist <- sapply(seq(nrow(available)), function(i) calcDist(available$Longitude[i], available$Latitude[i], farthest))
    available <- available %>%
        arrange(farthestDist)
    
    # Picking points
    currWeight <- 0
    i <- 1
    clusterIds <- integer(0)
    while(currWeight < 1000 & nrow(available)) {
        if(currWeight + available$Weight[1] <= 1000) {
            clusterIds <- c(clusterIds, available$GiftId[1])
            currWeight <- currWeight + available$Weight[1]
            available <-available[-1, ]
        }
        available <- available %>%
            filter(Weight <= 1000 - currWeight)
    }
    
    data$picked[data$GiftId %in% clusterIds] <- TRUE
    data$cluster[data$GiftId %in% clusterIds] <- currCluster
    
    # Statistical
    t <- round(proc.time() - t0, 2)[3]
    completed <- mean(data$picked)
    clusteringTime <- rbind(clusteringTime,
                            data.frame(x=completed,
                                       t=t))
    slope <- (clusteringTime$t[currCluster] - clusteringTime$t[currCluster - 5]) / (clusteringTime$x[currCluster] - clusteringTime$x[currCluster - 5])
    elt <- clusteringTime$t[currCluster]
    eta <- slope * (1 - clusteringTime$x[currCluster])
    cat(round(proc.time() - t1, 2)[3], "secs", "|", round(completed * 100, 1), "% completed", " | ",
        "Elapsed time:", round(elt) / 60, "m |\n",
        "Estimated Remaining time:", round(eta) / 60, "m |\n",
        "Estimated Total time:", round(elt + eta) / 60, "m |\n")

#     if(currCluster %% 10 == 0) {
#         slope <- (clusteringTime$t[currCluster] - clusteringTime$t[currCluster - 5]) / (clusteringTime$x[currCluster] - clusteringTime$x[currCluster - 5])
#         elt <- clusteringTime$t[currCluster]
#         eta <- slope * (1 - clusteringTime$x[currCluster])
#         g_time <- ggplot(rbind(clusteringTime,
#                               data.frame(x=1, t=elt + eta)),
#                         aes(x, y=t)) +
#             geom_line() +
#             geom_point() +
#             ggtitle(paste("Estimated time remaining:", round(eta), "secs", "\n",
#                           "Total time:", round(elt + eta)))
#         print(g_time)
#     }
    
    currCluster <- currCluster + 1
}
if(nrow(data) == 100000) {
    save(data, file="data/clusters.rda")
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

# plotData(data)

cat("Optimizing clusters", "\n")
t0 <- proc.time()
split_cluster <- sapply(seq(split_cluster), function(i) {
    cat("Otimizing cluser #", i, " / ", length(split_cluster), " = ", round(i / length(split_cluster), 1), "/n")
    output <- neuralOptimizeCluster(split_cluster[[i]])
    t <- proc.time() - t0
    t <- round(t[3], 2)
    cat("Cluster #", i, "optimized. ", t, " seconds. ETA: ", (length(split_cluster) - i) * t, "\n\n")
    output
}, simplify=FALSE)

deliveries <- do.call(rbind, split_cluster) %>%
    dplyr::rename(TripId = cluster) %>%
    dplyr::select(GiftId, TripId)

write.csv(deliveries, paste0("data/output-", as.numeric(Sys.time()), ".csv"), row.names=FALSE)
