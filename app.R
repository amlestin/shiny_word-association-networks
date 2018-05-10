#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(visNetwork)
library(lexvarsdatr)#devtools::install_github("jaytimm/lexvarsdatr")
library(shinythemes)
library(tidyverse)

# Define UI for application that draws a histogram
ui <- 

  navbarPage(theme=shinytheme("flatly"), "word association networks",id="corpusQuery",
             tabPanel("viz",
                      sidebarLayout(
                        sidebarPanel(
                          h4("Forward cue associations"),
                          textInput("user_search", label = "Cue", value = "WORK"),
                          h5("Enter cue above or click on target in viz to reset view.")
                        ),
                        mainPanel(visNetworkOutput("network"))
                      )
               ),
             
             tabPanel("about", h4("lexvarsdatr"), h4("Source"), h5("Nelson, D. L., McEvoy, C. L., & Schreiber, T. A. (2004). The University of South Florida free association, rhyme, and word fragment norms. Behavior Research Methods, Instruments, & Computers, 36(3), 402-407.") ) )



# Define server logic required to draw a histogram
server <- function(input, output, session) {
   
netx <- eventReactive(input$user_search, { #input$click,
  lvdr_build_network(search=toupper(input$user_search))
})
  
   output$network <- renderVisNetwork({
      
    #net <- lvdr_build_network(search=toupper(input$user_search))
    net <- netx()
    
    net$edges <- net$edges %>% select(-value)
    #net$nodes <- net$nodes %>% rename(label =id)
    
    visNetwork(net$nodes, net$edges, main = input$user_search, sub= paste0('outdegree = ',nrow(net$nodes)-1),width = "100%", height = "800px") %>% 
      visNodes(scaling = list(min = 7, max = 21))%>% 
      visOptions(highlightNearest = list(enabled = T, degree = 1, hover = T)) %>%
      visGroups(groupname = "cue", color = "purple", shape = "square") %>%
      visEvents(click = "function(nodes){
                  Shiny.onInputChange('click', nodes.nodes);
                  ;}" )
   })
    
    observeEvent(input$click, {
    slt = input$click
    #if (is.null(slt$value) || slt$col != c(1,3)) return()
    updateTextInput(session, 'user_search', value = netx()$nodes$label[slt])
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

