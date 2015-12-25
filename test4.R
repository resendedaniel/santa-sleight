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
          