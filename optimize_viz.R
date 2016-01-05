cluster_file <- "data/clusters.rda"
load(cluster_file)
split_cluster <- split(data, data$cluster)

i <- 728
proximity <- .02
cluster <- data %>% filter(cluster == i)
neuralOptimizeCluster(split_cluster[[i]], proximity)

optim_file <- "data/optimizing_data.rda"
load(optim_file)

steps <- lapply(GiftIds, function(order) {
    step <- cluster[match(order, cluster$GiftId), ]
    step <- step
})
dists <- sapply(steps, calcClusterDistance)
best <- which.min(dists)
plot(dists, type="l")


segments <- lapply(steps, function(step) {
    step <- createSegments(step) %>%
        mutate(best = FALSE)
})
best_segments <- segments[[best]]$id
segments <- do.call(rbind, segments) %>%
    mutate(count = 1)
segments <- aggregate(count ~ ., segments, sum) %>%
    mutate(best = id %in% best_segments) %>%
    mutate(alpha = sapply(count, function(c) c / sum(count))) %>%
    mutate(alpha = apply(data.frame(best=best, alpha=alpha), 1, function(seg) {
        x <- if(seg[1]) {
            max(alpha)
        } else {
            seg[2]
        }
        # x ^ (1/(seg[1] + 1))
    }))

ggplot(segments, aes(x=x, xend=xend, y=y, yend=yend, alpha=alpha, colour=!best)) +
    geom_segment() +
    geom_point() +
    scale_color_manual(values=c("#036564", "#000000")) +
    theme_bw() +
    theme(legend.position='none',
          axis.line = element_blank(), 
          axis.text.x = element_blank(), 
          axis.text.y = element_blank(),
          axis.ticks = element_blank(), 
          axis.title.x = element_blank(), 
          axis.title.y = element_blank()) +
    ggtitle(paste0("Cluster #", i, " | proximity: ", proximity, " | ", length(dists), " steps"))

library(beepr)
beep(4)

