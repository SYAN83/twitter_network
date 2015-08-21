data_path <- "./twitterdata"
rter_id <- read.csv(paste(data_path, "tweets", "612121494232145920", sep = "/"))$id

# 609418768578674688
# 628783838190305280


rter <- c()
for(id in rter_id) {
    rter <- rbind(rter, rters[rters$id == id,])
}

rter_id <- c(fusion$id, rev(intersect(rter_id, dir(paste(data_path, "friends", sep = "/")))))
inDegrees <- c()
for(i in 2:length(rter_id)) {
    inDegree <- intersect(rter_id[1:(i-1)], 
                          read.csv(paste(data_path, "friends", rter_id[i], sep = "/"))$id)
    if(length(inDegree)) {
        inDegrees <- rbind(inDegrees, cbind(inDegree, rep(rter_id[i], length(inDegree))))
    }
}

inDegrees <- data.frame(matrix(sapply(inDegrees, function(x) rters$name[rters$id == x]), ncol = 2))
names(inDegrees) <- c("source", "target")
simpleNetwork(inDegrees)
between <- betweenness(graph(t(inDegrees)))
centra <- round(alpha_centrality(graph(t(inDegrees[,c(2,1)])), alpha = .5))

name <- names(between)
Nodes <- data.frame(t(sapply(name, function(x) c(x[[1]], centra[x][[1]], between[x][[1]]))))
names(Nodes) <- c("name", "Centrality", "Betweeness")
inDegrees
Links <- data.frame(source = inDegrees$source)

inDegrees
Nodes[1]
inDegrees$target[1:20]
sapply(inDegrees[,2], function(x) nameDict[x])
inDegrees[,1]
sapply(inDegrees[1:3, 1], function(x) x)
inDegrees$target
Links <- data.frame(source = nameDict[as.character(inDegrees$source)]-1,
                    target = nameDict[as.character(inDegrees$target)]-1)
forceNetwork(Links = Links, Nodes = Nodes, Source = "source", Target = "target", NodeID = "name", Group = "Betweeness")
