file <- "data/gifts.csv"
northPole <- data.frame(Longitude=0, Latitude=90)
northPoleList <- as.list(northPole)

calcDist <- function(GiftId1, GiftId2=NULL) {
    i <- which(data$GiftId == GiftId1)
    dist <- if(is.null(GiftId2)) {
        p1 <- data[which(data$GiftId == GiftId1), ]
        distHaversine(c(p1$Longitude, p1$Latitude), c(northPole$Longitude, northPole$Latitude))
    } else {
        p1 <- data[which(data$GiftId == GiftId1), ]
        p2 <- data[which(data$GiftId == GiftId2), ]
        distHaversine(c(p1$Longitude, p1$Latitude), c(p2$Longitude, p2$Latitude))
    }
    
    dist
}

readData <- function(n=NULL) {
    raw_data <- read.csv(file)
    if(!is.null(n)) {
        raw_data <- sample_n(raw_data, 200)
    }
    
    raw_data
}

loadCachedDist <- function() {
    file <- "data/distMatrix.rda"
    if("cachedDist" %in% ls() & file.exist(file)) {
        load(file)
    } else {
        cachedMatrix <<- matrix()
    }
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
    data$dist <- sapply(seq(nrow(data)), function(i) calcDist(data$GiftId[i]))
    data <- data %>%
        arrange(-dist, -Weight) %>%
        mutate(picked = FALSE)
    
    data
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

calcClusterDistance <- function(cluster_) {
    sum(sapply(seq(nrow(cluster_) - 1), function(i) {
        calcDist(cluster_$GiftId[i], cluster_$GiftId[i+1])
    })) + calcDist(cluster_$GiftId[1]) + calcDist(cluster_$GiftId[nrow(cluster_)])
}

optimizeCluster <- function(cluster) {
    cluster_ <- cluster # %>% dplyr::select(Longitude, Latitude)
    n <- nrow(cluster_)
    GiftIds <- cluster_$GiftId[which.min(cluster_$dist)]
    while(length(GiftIds) != n) {
        remain <- cluster_ %>% filter(!GiftId %in% GiftIds)
        current <- cluster_$GiftId[tail(GiftIds, 1) == cluster_$GiftId]
        nextDist <- apply(remain, 1, function(row) {
            row <- as.list(row)
            calcDist(as.numeric(row$GiftId), current)
        })
        GiftIds <- c(GiftIds, remain$GiftId[which.min(nextDist)])
    }
    
    cluster_ <- cluster_[match(GiftIds, cluster$GiftId), ]
    
    cluster_
}

neuralOptimizeCluster <- function(cluster_, proximity=.01) {
    t0 <- proc.time()
    n <- nrow(cluster_)
    GiftIds <- list(cluster_$GiftId[which.min(cluster_$dist)])
    i <- 1
    # Loop trought lists, lists might increase as new paths are plausible
    while(i <= length(GiftIds)) {
        cat(i, "/", length(GiftIds), "=", round(i/length(GiftIds)*100,2), "% | ")
        nCandidates <- 0
        while(length(GiftIds[[i]]) != n) {
            remain <- cluster_ %>% filter(!GiftId %in% GiftIds[[i]])
            current <- cluster_$GiftId[tail(GiftIds[[i]], 1) == cluster_$GiftId]
            nextDist <- apply(remain, 1, function(row) {
                row <- as.list(row)
                calcDist(row$GiftId, current)
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
        t <- (proc.time() - t0)[3]
        cat(nCandidates, "new candidates", "|",
            "Elapsed:", round(t / 60), "m", "|",
            "ETA:", round(t * length(GiftIds) / i / 60), "m", "\n")
        i <- i + 1
    }
    
    if(!file.exists("data/optimizing_data.rda")) {
        save(GiftIds, file="data/optimizing_data.rda")
    }
    
    t <- proc.time() - t0
    clusterDists <- sapply(GiftIds, function(x) {
        calcClusterDistance(cluster_[match(x, cluster_$GiftId), ])
    })
    
    best <- GiftIds[[which.min(clusterDists)]]
    cluster_[match(best, cluster_$GiftId), ]
}