library(shiny)
library(ggplot2)
library(MASS)
library(xtable)

LD <- function(Subjects, Mortality, Dose, LD.p=0.5, Link.function='probit') {
  Dead <- round(Subjects * Mortality, digits=0)
  Live <- Subjects - Dead
  
  # fitting generalized linear model (error distribution is binomial, and link function is probit)
  model <- glm(cbind(Dead, Live) ~ Dose, family=binomial(link=Link.function))
  
  # predict doses for binomial assay model
  ld.dose <- dose.p(model, p=c(LD.p))
  
  # return back results
  x  <- ld.dose[[1]]
  se <- attr(ld.dose, "SE")[[1]]
  a  <- model$coefficients[1]
  b  <- model$coefficients[2]

  list('x'=x, 'se'=se, 'a'=a, 'b'=b)
}

# Header ------------------------------------------------------------

header <- headerPanel('LD50')

# Sidebar -----------------------------------------------------------

sidebar <- sidebarPanel(
  tags$img(src='icarda.png', height=90, width=190),
  fileInput('datafile', 'Upload Your Data:', accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
  sliderInput('p', 'Effective (or Lethal) Dose:', min=0, max=100, value=50),
  radioButtons('link', 'Link Function:', c('Logit'='logit', 'Probit'='probit')),
  numericInput('subjects', 'Number of Subjects:', 10),
  uiOutput('mortality.col'),
  uiOutput('dose.col'),
  uiOutput('groups.col')
)

# Body ---------------------------------------------------------------

body <- mainPanel(
          tabsetPanel(
            tabPanel('Input Data', tags$h2('Input Data'), dataTableOutput('contents')),
            tabPanel('Analysis Output', tags$h2('Analysis Output'), tableOutput('ld.list')),
            tabPanel('Graphics Output', tags$h2('Graphics Output'), downloadButton('downloadGraph', 'Download High Resolution Graph'), plotOutput('ld.graph')),
            tabPanel('Help', tags$h2('Help'), withMathJax(), 
                     tags$h4('Logit function: $$Pr(x) = \\frac{1}{1+e^{-(a+b.x)}}$$
                              Probit function: $$Pr (x) = \\phi{(a+b.x)}$$
                              Normal Cumulative Distribution Function (CDF):
                              $$\\phi (x) = \\frac{1}{\\sqrt{2\\pi}} \\int_{-\\infty}^{x} e^{(-t^2/2)} dt$$'))
          )
)

# Setup Shiny app UI components -------------------------------------------

ui <- shinyUI(pageWithSidebar(header, sidebar, body))

# Setup Shiny app back-end components -------------------------------------

server <- function(input, output) { 
    
  # Import Data
  importedData <- reactive({  
    inFile <- input$datafile
    
    if (is.null(inFile)) return(NULL)
    
    read.csv(inFile$datapath, header=TRUE)
  })
  
  output$contents <- renderDataTable({
    importedData()
  })
  
  # Build user interface
  output$mortality.col <- renderUI({
    selectInput('mortality.col', 'Column of Mortality%:', as.list(c('', colnames(importedData()))))
  })
  
  output$dose.col <- renderUI({
    selectInput('dose.col', 'Column of Dose:', as.list(c('', colnames(importedData()))))
  })
  
  output$groups.col <- renderUI({
    selectInput('groups.col', 'Column of Groups:', as.list(c('', '', colnames(importedData()))))
  })

  # Do the Analysis
  results <- reactive({
    ld.list <- NULL
    treat <- 'All'
    
    if(input$groups.col != '') treat <- levels(importedData()[,input$groups.col])
    
    for(i in treat){
      if(input$groups.col != '') {
        LD.data <- subset(importedData(), importedData()[[input$groups.col]]==i)
      }else{
        LD.data <- importedData()
      }
      
      mortality <- LD.data[,input$mortality.col]
      dose <- LD.data[,input$dose.col]
      
      val <- LD(input$subjects, mortality, dose, input$p/100, input$link)
      ld.list <- rbind(ld.list, data.frame(i, input$p, val$x, val$se, val$a, val$b))
    }

    names(ld.list) <- c('Treat', 'LD', 'Dose', 'SE', 'Intercept (a)', 'Coefficient (b)')
    row.names(ld.list) <- NULL
    list(ld.list=ld.list)
  })

  output$ld.list <- renderTable({
      results()$ld.list
  })
  
  graph.height <- reactive({
    500*length(levels(results()$ld.list$Treat))
  })
  
  output$ld.graph <- renderPlot({
    g <- ggplot(importedData(), aes(x=importedData()[,input$dose.col], y=importedData()[,input$mortality.col])) + 
                geom_point(position=position_jitter(width=0, height=0.05)) + 
                geom_vline(aes(xintercept=Dose, group=Treat), data=results()$ld.list, col='red') + 
                geom_text(aes(x=Dose, label=paste0('LD %', input$p), group=Treat), y=0.1, angle=90, vjust=1.5, data=results()$ld.list, col='red') + 
                xlab(input$dose.col) + ylab(input$mortality.col) + 
                stat_smooth(method='glm', formula=cbind(y*input$subjects, (1-y)*input$subjects)~x, method.args=list(family=binomial(link=input$link)), fullrange=TRUE, size=1)
    
    if(input$groups.col != '') {
      g <- g + facet_wrap(as.formula(paste('~', input$groups.col)), ncol=1)
    }
    
    g
  }, height=graph.height, res=96)
  
  
  output$downloadGraph <- downloadHandler(
    filename = 'test.png',
    content = function(file) {
      device <- function(..., width=6, height=12) grDevices::png(..., width = 6, height = 12, res = 600, units = 'in')
      ggsave(file, device = device)
    }
  )  
}
              
# Render Shiny app --------------------------------------------------------

shinyApp(ui, server)