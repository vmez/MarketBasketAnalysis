#------------------------------------------------------------WORKING WITH SHINY APP DEVELOPMENT 
#Libraries----------------------------

library(shiny)
library(DT)
library(data.table)
library(arules)
library(grid)
library(arulesViz)
library(crosstalk)


#PreProcessed parameters--------------------------------------------------------------
supp=0.1
conf=0.5
vars=50

#Load files---------------------------------------------------------------------------


# Part 2 Shiny App ---------------------------------------------------->> the server 
ui <- fluidPage(
  sidebarPanel(
    
    conditionalPanel(
      condition = "input.samp=='Sample'",
      numericInput("nrule", 'Number of Rules', 5), br()
    ),
    
    conditionalPanel(
      condition = "input.mytab=='graph'",
      radioButtons('graphType', label='Graph Type', choices=c('itemsets','items'), inline=T), br()
    ),
    
    conditionalPanel(
      condition = "input.lhsv=='Subset'", 
      uiOutput("choose_lhs"), br()
    ),
    
    conditionalPanel(
      condition = "input.rhsv=='Subset'", 
      uiOutput("choose_rhs"), br()
    ),
    
    conditionalPanel(
      condition = "input.mytab=='grouped'",
      sliderInput('k', label='Choose # of rule clusters', min=1, max=150, step=1, value=15), br()
    ),
    
    conditionalPanel(
      condition = "input.mytab %in%' c('grouped', 'graph', 'table', 'datatable', 'scatter', 'paracoord', 'matrix', 'itemFreq')", 
      # radioButtons('samp', label='Sample', choices=c('All Rules', 'Sample'), inline=T), br(),
      #uiOutput("choose_columns"), br(),
      sliderInput("supp", "Support:", min = 0, max = .01, value = supp , step = 1/10000), br(),
      sliderInput("conf", "Confidence:", min = 0, max = 1, value = conf , step = 1/100), br(),
      selectInput('sort', label='Sorting Criteria:', choices = c('lift', 'confidence', 'support', 'count')), br(), br(),
      numericInput("minL", "Min. items per set:", 2), br(), 
      numericInput("maxL", "Max. items per set::", 3), br(),
      # radioButtons('lhsv', label='LHS variables', choices=c('All', 'Subset')), br(),
      # radioButtons('rhsv', label='RHS variables', choices=c('All', 'Subset')), br(),
      downloadButton('downloadData', 'Download Rules as CSV')
    )
    
  ),
  mainPanel(
    tabsetPanel(
      tabPanel("All Products", DT::dataTableOutput("support")),
      tabPanel("Histogram", plotOutput("hist")),
      tabPanel("Rules", DT::dataTableOutput("rules")),
      tabPanel("Scatter Plot", plotOutput("scatter")),
      tabPanel("Graph", plotOutput("graph")),
      
      tabPanel("Grouped Matrix", plotOutput("matrix"))
    )
  )
)




# Part 2 Shiny App ---------------------------------------------------->> the server
server <- function(input, output) {
  
  ###-----------> Run Apriori when you move the slider + This allows for interaction
  dataInput <- reactive({
    apriori (import, parameter = list(supp=input$supp,conf = input$conf, minlen= input$minL, maxlen=input$maxL))
  })
  
  ###-----------> Output a table showing the frequency of the items. 
  output$support = DT::renderDataTable({
    as.matrix(itemFrequency(import)) 
    
  })
  
  ###------------> Output rules, and make them sortable by the drop down
  output$rules = DT:: renderDataTable({
    
    inspect(  sort(dataInput(), by = input$sort))
    
  })
  ###------------> Output scatterplot
  output$scatter = renderPlot({
    plot(dataInput(), measure=c("lift", "count"), shading="confidence", interactive=F)
  })
  ###------------> Output the histogram of item frequencies...booooring!
  output$hist = renderPlot({
    itemFrequencyPlot(import, topN = 10)
  })
  ##---------> Output SpiderGraph
  output$graph = renderPlot({
    plot(dataInput(), method="graph")
  })
  ##---------> Output GroupedMatrix
  output$matrix = renderPlot({
    plot(dataInput(), method="grouped")
  })
  
}

# Part 3 --------------------------------->> Run the App 
shinyApp(ui = ui, server = server)
