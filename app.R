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
library(DT)#install.packages("DT")

# Define UI for application that draws a histogram
ui <- 

  navbarPage(theme=shinytheme("flatly"), "word association networks",id="corpusQuery",
             tabPanel("viz",
                      sidebarLayout(
                        sidebarPanel(
                          h4("Forward cue associations"),
                          textInput("user_search", label = "Cue", value = "DIRECTION"),
                          h5("Enter cue above or click on target in viz to reset view.") )
                        ,
                        mainPanel(visNetworkOutput("network"))  ))
               ,
             
             tabPanel("about", 
                      sidebarLayout(
                        sidebarPanel(
                          h4("Source"), 
                          helpText(a("Nelson, D. L., McEvoy, C. L., & Schreiber, T. A. (1998). The University of South Florida word association, rhyme, and word fragment norms.", href = "http://w3.usf.edu/FreeAssociation", target="_blank")),
                          h4("Author"), 
                          helpText(   a("Jason Timm",     href="https://www.jtimm.net", target="_blank"))),
                          
                        mainPanel(h4("Some network descriptives"),
                          h6("Nelson et al. (1998) make available free-association data based on responses from more than 6,000 participants.  Over 5,000 words served as cues, eliciting over 10,000 target responses."),
                          selectInput("type", "Centrality values:",
                                                         c("In-degree" = "TARGET",
                                                           "Out-degree" = "CUE")),
                          DT::dataTableOutput("indegree"))
                          
                        )
                      )
          )



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
    
    visNetwork(net$nodes, net$edges, main = input$user_search, sub= paste0('out-degree centrality = ',nrow(net$nodes)-1),width = "100%", height = "800px") %>% 
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
    
    
  output$indegree <- DT::renderDataTable({

    if (input$type == 'TARGET') {pos <- 'TPS'} else
    {pos <- 'QPS'}
    x1 <- lvdr_association %>%
      group_by_(input$type,pos) %>%
      summarize(count=n())%>%
      arrange(desc(count))
    
    x1 %>%
      DT::datatable(class = 'cell-border stripe', rownames = FALSE,width="100%", escape=FALSE, options = list(sDom  = '<"bottom">ip')) %>%
      DT::formatStyle('count',
        background = DT::styleColorBar(x1$count, 'steelblue'),
        backgroundSize = '80% 70%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'right') %>%
      DT::formatStyle(c(1:3),fontSize = '85%')
    
    })

#id <- input$filter

#if(id!= '') #If target clicked, subset table to target. Else, show all.
#  DT::datatable(subset(usage,usage[input$aggs]==id),selection='none',class = 'cell-border stripe', rownames = FALSE,width="100%", escape=FALSE,options = list(sDom  = '<"bottom">ip'))

    
    
}

# Run the application 
shinyApp(ui = ui, server = server)

