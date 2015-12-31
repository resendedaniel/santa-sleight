source("source.R")
source("api.R")
load("data/clusters.rda")
n <- nrow(data)

split_cluster <- split(data, data$cluster)
# plotData(data)

cat("Optimizing clusters", "\n")
# n_cores <- detectCores() - 1
# cl <- makeCluster(n_cores)
# clusterExport(cl, c("split_cluster", "northPole", "neuralOptimizeCluster", "calcDist", "calcClusterDistance"))
# clusterEvalQ(cl,  library(dplyr))
# clusterEvalQ(cl,  library(geosphere))
# registerDoParallel(n_cores)
# split_cluster <- parLapply(cl, seq(split_cluster),  function(i) {
split_cluster <- lapply(seq(length(split_cluster)), function(i){
    t0 <- proc.time()
    cat("Otimizing cluser #", i, "/", length(split_cluster), "=", round(i / length(split_cluster) * 100, 1), "%", "\n")
    output <- neuralOptimizeCluster(split_cluster[[i]], proximity = .01)
    # output <- optimizeCluster(split_cluster[[i]])
    t <- proc.time() - t0
    t <- round(t[3], 1)
    cat("Cluster #", i, "optimized. ", round(t/60, 1), " seconds. ETA: ", round((length(split_cluster) - i) * t / 60, 1), "m", "\n\n")
    output
})
# save(distList, file="data/distList.rda")
# stopCluster(cl)

deliveries <- do.call(rbind, split_cluster) %>%
    dplyr::rename(TripId = cluster) %>%
    dplyr::select(GiftId, TripId)

write.csv(deliveries, paste0("data/output-", as.numeric(Sys.time()), ".csv"), row.names=FALSE)
