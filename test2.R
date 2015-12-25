source("api.R")

n <- 5
centers <- sample_n(data, n) %>%
    dplyr::select(Latitude, Longitude) %>%
    mutate(center = factor(seq(n)))


# Loop
data$center <- factor(apply(data, 1, function(row) {
    row <- lapply(as.list(row), as.numeric)
    dists <- apply(centers, 1, function(crow) {
        crow <- lapply(as.list(crow), as.numeric)
        sqrt((crow$Longitude - row$Longitude)^2 + (crow$Latitude - row$Latitude)^2)
    })
    which.min(dists)
}))

centers <- split(data, data$center)
centers <- lapply(centers, function(x) {
    data.frame(
        center = x$center[1],
        Latitude = sum(x$Latitude * x$Weight / sum(x$Weight)),
        Longitude = sum(x$Longitude * x$Weight / sum(x$Weight))
    )
})
centers <- do.call(rbind, centers)

ggplot(data, aes(Longitude, Latitude, colour=center)) +
    geom_point(aes(size=sqrt(Weight)), alpha=.1) +
    theme_bw() +
    theme(legend.position="none") +
    geom_point(data=centers, aes(Longitude, Latitude, colour=center, size=10, colour=center), shape=4)
