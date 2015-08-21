library(shiny)
library(igraph)
library(networkD3)
library(googleVis)
source("helper.R")

data_path <- "./twitterdata"

# library(twitteR)
# oauth <- read.csv(paste(data_path, "oauth", sep = "/"), sep = " ", header = FALSE)[,2]
# setup_twitter_oauth(consumer_key = oauth[1], 
#                     consumer_secret = oauth[2], 
#                     access_token = oauth[3], 
#                     access_secret = oauth[4])

rters <- fromDB(paste(data_path, "twitter.db", sep = "/"), "users")
tweets <- fromDB(paste(data_path, "twitter.db", sep = "/"), "tweets")
fusion <- rters[1,]

shinyServer(function(input, output) {
    # function to embed webpage in shiny
    getPage<-function(url) {
        return(tags$iframe(src = url, 
                           style="width:100%;",
                           frameborder="0",
                           id="iframe", 
                           height = "300px"))
    }
    # display tweet webpage using tweet id
    output$website <- renderUI({
        url <- strsplit(tweets$text[tweets$id == input$tweet], "http")[[1]][2]
        getPage(paste0("http", url))
    })
    
    # return retweeters id of tweet based on its id
    rter_id <- reactive({
        rter_id <- read.csv(paste(data_path, "tweets", 
                                  input$tweet, sep = "/"))$id
        rter_id
    })
    
    # return retweeters as a dataframe
    retweeters <- reactive({
        rter <- c()
        for(id in rter_id()) {
            rter <- rbind(rter, rters[rters$id == id,])
        }
        # wrap image url with img tag
        rter$profileImageUrl <- sapply(rter$profileImageUrl,
                                       function(x) paste0("<img src=\"", x, "\">"))
        # variable selection
        rter <- rter[, c("profileImageUrl",
                           "name",
                           "id",
                           "statusesCount",
                           "friendsCount",
                           "followersCount")]
        names(rter) <- c("Image",
                          "Name",
                          "Id",
                          "Tweets",
                          "Following",
                          "Followers")
        rter
    })
    
    # following relationships between retweeters
    # (whether a retweeter is following those who retweeted the same tweet before him)

    rtLinks <- reactive({
        rter_id <- c(fusion$id, rev(intersect(rter_id(), 
                                              dir(paste(data_path, "friends", sep = "/")))))
        friendShip <- c()
        for(i in 2:length(rter_id)) {
            friend <- intersect(rter_id[1:(i-1)], 
                                  read.csv(paste(data_path, "friends", rter_id[i], sep = "/"))$id)
            if(length(friend)) {
                if(input$n & length(friend) > input$n) {
                    friend <- rev(friend)[1:input$n]
                }
                friendShip <- rbind(friendShip, cbind(friend, rep(rter_id[i], length(friend))))
            }
        }
        friendShip <- data.frame(matrix(sapply(friendShip, 
                                               function(x) rters$name[rters$id == x]), ncol = 2))
    })
    
    rtNodes <- reactive({
        between <- round(betweenness(graph(t(rtLinks()))))
        centra <- round(alpha_centrality(graph(t(rtLinks()[,c(2,1)])), alpha = input$alpha))
        nodes <- data.frame(t(sapply(names(between),
                                     function(x) c(x, between[x], centra[x]))))
        rownames(nodes) <- NULL
        names(nodes) <- c("Name", "Betweeness", "Centrality")
        nodes
    }) 
    
    output$ndTable <- renderGvis({
        gvisTable(rtNodes(), options=list(page='enable',
                                          height='500px',
                                          width='300px'))
    })
    
    output$rtTable <- renderGvis({
        gvisTable(retweeters())
    })
    
    output$rtNetwork <- renderForceNetwork({
        rtNodes <- rtNodes()
        rtLinks <- rtLinks()
        nameDict <- seq(nrow(rtNodes)) - 1
        names(nameDict) <- rtNodes$Name

        rtLinks <- data.frame(source = nameDict[as.character(rtLinks[,1])],
                              target = nameDict[as.character(rtLinks[,2])])
        
        group <- c(0, rep(1, nrow(rtNodes) - 1))
        for(i in 1:nrow(rtLinks)) {
            group[rtLinks$target[i]+1] <- max(group[rtLinks$target[i]+1],
                                              group[rtLinks$source[i]+1] + 1)
        }
        rtNodes$group <- group
        forceNetwork(Links = rtLinks, Nodes = rtNodes, 
                     Source = "source", Target = "target", 
                     NodeID = "Name", Group = "group",
                     Nodesize = input$nodeType,
                     opacity = 0.8, legend = TRUE, zoom = TRUE, 
                     opacityNoHover = TRUE)
    })
})
