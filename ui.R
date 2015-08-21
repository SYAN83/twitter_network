library(shiny)
library(networkD3)

data_path <- "./twitterdata/"
tweets <- dir(paste(data_path, "tweets/", sep = "/"))


shinyUI(navbarPage(theme="readable.css",
                   "Fusion On Twitter",
                   tabPanel("Tweets",
                            fluidRow(
                                column(3,
                                       htmlOutput("website"),
                                       br(),
                                       fluidRow(
                                           column(4, h5("Tweet ID")),
                                           column(8, selectInput("tweet",
                                                              NULL,
                                                              tweets))),
                                       hr(),
                                       fluidRow(
                                           column(4, h5("Alpha")),
                                           column(6,
                                                  numericInput("alpha", NULL, 0.5,
                                                               min = .1, max = 1, step = .1))),
                                       fluidRow(
                                           column(4, h5("Link Limit")),
                                           column(6,
                                                  numericInput("n", NULL, 0,
                                                               min = 0, max = 10, step = 1))),
                                       fluidRow(
                                           column(4, h5("Node Size")),
                                           column(6,
                                                  selectInput("nodeType", NULL, 
                                                              c("Betweeness", "Centrality"))))),
                                column(9,
                                       tabsetPanel(
                                           tabPanel("Broadcasting Network",
                                                    br(),
                                                    fluidRow(
                                                        column(8,
                                                               forceNetworkOutput("rtNetwork")),
                                                        column(4,
                                                               htmlOutput("ndTable")))),
                                           tabPanel("Retweeters",
                                                    htmlOutput("rtTable"))
                                           )
                                       )
                                )
                            ),
                   tabPanel("Influencers",
                            fluidRow(
                                column(10,
                                       h4("Oops, this page is under construction!")))
                   )
))
