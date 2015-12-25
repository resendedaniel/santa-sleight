source("api.R")

data <- data %>%
    mutate(dist = pitagDist(Longitude, Latitude)) %>%
    arrange(-dist, -Weight)


g <- ggplot(data, aes(Longitude, Latitude)) +
    geom_point(aes(size=sqrt(Weight))) +
    theme(legend.position="none")

curr <- data[1,]
g + geom_point(data=curr, aes(Longitude, Latitude, size=sqrt(Weight), colour="red")) +
    geom_segment(data=cbind(curr, northPole), aes(x=Longitude,
                                                  xend=NLong,
                                                  y=Latitude,
                                                  yend=NLat), linetype="dashed")



