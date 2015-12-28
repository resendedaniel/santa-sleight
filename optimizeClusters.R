source("api.R")
load("data/clusters.rda")

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
    # output <- neuralOptimizeCluster(split_cluster[[i]])
    output <- optimizeCluster(split_cluster[[i]])
    t <- proc.time() - t0
    t <- round(t[3], 2)
    cat("Cluster #", i, "optimized. ", t, " seconds. ETA: ", (length(split_cluster) - i) * t, "\n\n")
    output
}, simplify=FALSE)

deliveries <- do.call(rbind, split_cluster) %>%
    dplyr::rename(TripId = cluster) %>%
    dplyr::select(GiftId, TripId)

write.csv(deliveries, paste0("data/output-", as.numeric(Sys.time()), ".csv"), row.names=FALSE)
