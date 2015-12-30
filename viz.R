plotMap <- function(data) {
    ggplot(data, aes(x=Longitude, y=Latitude, color=factor(cluster))) + 
        geom_point() +
        theme(legend.position='none',
              axis.text.x=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks=element_blank())
}

sampleData <- data
breaks <- c(length(unique(data$cluster)), 2^(10:0))
g_sample <- sapply(seq(length(breaks)), function(i) {
    sampleData <<- sampleData %>%
        filter(cluster %in% sample(unique(sampleData$cluster), breaks[i]))
    g <- plotMap(sampleData) + ggtitle(paste(breaks[i], "clusters")) +
        xlim(range(data$Longitude)) +
        ylim(range(data$Latitude)) +
        xlab("") + ylab("")
    
    png(paste0("clustering_", sprintf("%03d", i), ".png"), width=800, height=600)
    print(g)
    dev.off()
    
    g
}, simplify=FALSE)
