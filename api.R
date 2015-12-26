## Require and install libs
package_list <- c('dplyr',
                  'ggplot2',
                  'geosphere')
for(p in package_list) {
    if(!(p %in% rownames(installed.packages()))) install.packages(p, repos='http://cran.rstudio.com', lib='/usr/local/lib/R/site-library/', dependencies=TRUE)
    suppressMessages(library(p, character.only = TRUE))
}

dir.create("data")

readData <- function(n=NULL) {
    raw_data <- read.csv(file)
    if(!is.null(n)) {
        raw_data <- sample_n(raw_data, 200)
    }
    
    raw_data
}

plotData <- function(data) {
    g_map <- plotMap(data)
    g_cluster_size <- ggplot(data.frame(cluster_size), aes(cluster_size)) +
        geom_histogram(binwidth=5) +
        ggtitle("Size of each cluster")
    g_cluster_variance <- ggplot(data.frame(variance), aes(variance)) +
        geom_histogram(binwidth=5) +
        ggtitle("Variance of each cluster's distance")
    
    print(g_cluster_variance)
    print(g_cluster_size)
    print(g_map)
}


processData <- function(data) {
    data$dist <- sapply(seq(nrow(data)), function(i) calcDist(data$Longitude[i], data$Latitude[i]))
    data <- data %>%
        arrange(-dist, -Weight) %>%
        mutate(picked = FALSE)
    
    data
}

file <- "data/gifts.csv"

northPole <- data.frame(Longitude=0, Latitude=90)
northPoleList <- as.list(northPole)

calcDist <- function(long, lat, reference=northPoleList) {
    distHaversine(c(long, lat), c(reference$Longitude, reference$Latitude))
}

plotMap <- function(data) {
    ggplot(data, aes(x=Longitude, y=Latitude, color=factor(cluster))) + 
        geom_point() +
        theme(legend.position='none',
              axis.text.x=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks=element_blank())
}

plotCluster <- function(cluster) {
    rownames(cluster) <- NULL
    cluster <- cluster %>% dplyr::select(Longitude, Latitude)
    # cluster <- do.call(rbind, list(northPole, cluster, northPole))
    n <- nrow(cluster)
    segments <- data.frame(x=cluster$Longitude[-n],
                           xend=cluster$Longitude[-1],
                           y=cluster$Latitude[-n],
                           yend=cluster$Latitude[-1])
    ggplot(segments, aes(x=x, xend=xend, y=y, yend=yend)) +
        geom_segment(linetype="dashed") +
        geom_point(aes(x, y))
}

calcClusterDistance <- function(cluster) {
    cluster_ <- cluster %>% dplyr::select(Longitude, Latitude)
    cluster_ <- do.call(rbind, list(northPole, cluster_, northPole))
    
    sum(sapply(seq(nrow(cluster_) - 1), function(i) {
        calcDist(cluster_$Longitude[i], cluster_$Latitude[i], cluster_[i+1,])
    }))
}

optimizeCluster <- function(cluster) {
    cluster_ <- cluster # %>% dplyr::select(Longitude, Latitude)
    n <- nrow(cluster_)
    GiftIds <- cluster_$GiftId[which.min(cluster_$dist)]
    while(length(GiftIds) != n) {
        remain <- cluster_ %>% filter(!GiftId %in% GiftIds)
        current <- cluster_[tail(GiftIds, 1) == cluster_$GiftId, ]
        nextDist <- apply(remain, 1, function(row) {
            row <- as.list(row)
            row$Longitude <- as.numeric(row$Longitude)
            row$Latitude <- as.numeric(row$Latitude)
            calcDist(row$Longitude, row$Latitude, current)
        })
        GiftIds <- c(GiftIds, remain$GiftId[which.min(nextDist)])
    }
    
    cluster_ <- cluster_[match(GiftIds, cluster$GiftId), ]
    
    cluster_
}

neuralOptimizeCluster <- function(cluster, proximity=.0075) {
    t0 <- proc.time()
    cluster_ <- cluster # %>% dplyr::select(Longitude, Latitude, GiftId)
    n <- nrow(cluster_)
    GiftIds <- list(cluster_$GiftId[which.min(cluster_$dist)])
    i <- 1
    # Loop trought lists, lists might increase as new paths are plausible
    while(i <= length(GiftIds)) {
        cat(i, "/", length(GiftIds), "=", round(i/length(GiftIds)*100,2), "% | ")
        nCandidates <- 0
        while(length(GiftIds[[i]]) != n) {
            remain <- cluster_ %>% filter(!GiftId %in% GiftIds[[i]])
            current <- cluster_[tail(GiftIds[[i]], 1) == cluster_$GiftId, ]
            nextDist <- apply(remain, 1, function(row) {
                row <- as.list(row)
                row$Longitude <- as.numeric(row$Longitude)
                row$Latitude <- as.numeric(row$Latitude)
                calcDist(row$Longitude, row$Latitude, current)
            })
            candidates <- which(nextDist < min(nextDist) + (median(nextDist) - min(nextDist)) * proximity)
            if (length(candidates) == 0) {
                GiftIds[[i]] <- c(GiftIds[[i]], remain$GiftId[which.min(nextDist)])
            } else {
                currentState <- GiftIds[[i]]
                GiftIds[[i]] <- c(currentState, remain$GiftId[candidates[1]])
                candidates <- candidates[-1]
                nCandidates <- nCandidates + length(candidates)
                newPaths <- sapply(candidates, function(nextCandidate) {
                    c(currentState, remain$GiftId[nextCandidate])
                }, simplify=FALSE)
                GiftIds <- c(GiftIds, newPaths)
            }
        }
        cat(nCandidates, "new candidates", "\n")
        i <- i + 1
    }
    
    t <- proc.time() - t0
    clusterDists <- sapply(GiftIds, function(x) {
        calcClusterDistance(cluster_[match(x, cluster$GiftId), ])
    })
    
    best <- GiftIds[[which.min(clusterDists)]]
    cluster_[match(best, cluster$GiftId), ]
}