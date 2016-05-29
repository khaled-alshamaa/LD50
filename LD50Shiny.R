library(shiny)
library(ggplot2)
library(MASS)

# Header ------------------------------------------------------------

header <- headerPanel('LD50 - ICARDA')

# Sidebar -----------------------------------------------------------

sidebar <- sidebarPanel(
  fileInput('datafile', 'Upload Your Data:', accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
  tags$hr(),
  radioButtons('link', 'Link Function:', c('Logit'='logit', 'Probit'='probit')),
  numericInput('subjects', 'Number of Subjects:', 10),
  uiOutput('mortality.col'),
  uiOutput('dose.col'),
  uiOutput('groups.col')
)

# Body ---------------------------------------------------------------

body <- mainPanel(
          tabsetPanel(
            tabPanel("Input Data", dataTableOutput("contents")),
            tabPanel("Analysis Output", verbatimTextOutput("ld.list")),
            tabPanel("Graphics Output", plotOutput("ld.graph", height="auto")),
            tabPanel("Help", withMathJax(), 
                     tags$h4("Logit function: $$Pr(x) = \\frac{1}{1+e^{-(a+b.x)}}$$
                              Probit function: $$Pr (x) = \\phi{(a+b.x)}$$
                              Normal Cumulative Distribution Function (CDF):
                              $$\\phi (x) = \\frac{1}{\\sqrt{2\\pi}} \\int_{-\\infty}^{x} e^{(-t^2/2)} dt$$"))
          )
)

# Setup Shiny app UI components -------------------------------------------

ui <- shinyUI(pageWithSidebar(header, sidebar, body))

# Setup Shiny app back-end components -------------------------------------

server <- function(input, output) { 
    
    LD <- function(Subjects, Mortality, Dose, LD.p=0.5, Link.function="probit") {
        Dead <- round(Subjects * Mortality, digits=0)
        Live <- Subjects - Dead
        
        # fitting generalized linear model (error distribution is binomial, and link function is probit)
        model <- glm(cbind(Dead, Live) ~ Dose, family=binomial(link=Link.function))
        
        # predict doses for binomial assay model
        ld.dose <- dose.p(model, p=c(LD.p))
        
        # return back results
        list("LD"=ld.dose, "Model"=model$coefficients)
    }
    
    graph.height <- function(){
        500*length(levels(results()$ld.list$Treat))
    }
    
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
      selectInput("mortality.col", "Column of Mortality%:", as.list(c('', colnames(importedData()))))
  })
  
  output$dose.col <- renderUI({
      selectInput("dose.col", "Column of Dose:", as.list(c('', colnames(importedData()))))
  })
  
  output$groups.col <- renderUI({
      selectInput("groups.col", "Column of Groups:", as.list(c('', colnames(importedData()))))
  })

  # Do the Analysis
  results <- reactive({
      ld.list <- NULL
      
      for(p in c(0.5, 0.9)) {
          for(i in levels(importedData()[,input$groups.col])){
              LD.data <- subset(importedData(), importedData()[[input$groups.col]]==i)
              mortality <- LD.data[,input$mortality.col]
              dose <- LD.data[,input$dose.col]
              
              result <- LD(input$subjects, mortality, dose, p, input$link)
              x  <- round(result$LD[[1]], digits=2)
              se <- round(attr(result$LD, "SE")[[1]], digits=2)
              a  <- result$Model[1]
              b  <- result$Model[2]
              ld.list <- rbind(ld.list, data.frame(i, p*100, x, se, a, b))
          }
      }
      
      names(ld.list) <- c("Treat", "LD", "Dose", "SE", "Intercept (a)", "Coefficient (b)")
      row.names(ld.list) <- NULL
      list(ld.list=ld.list)
  })

  output$ld.list <- renderPrint({
      results()$ld.list
  })

  output$ld.graph <- renderPlot({
      mortality <- importedData()[,input$mortality.col]
      dose <- importedData()[,input$dose.col]
      ld.colors <- rep(c("gold4", "red"), length(levels(results()$ld.list$Treat)))

      ggplot(importedData(), aes(x=dose, y=mortality)) + 
          facet_wrap(as.formula(paste("~", input$groups.col)), ncol=1) + 
          geom_point(position=position_jitter(width=0, height=0.05)) + 
          geom_vline(aes(xintercept=Dose, group=Treat), data=results()$ld.list, col=ld.colors) + 
          #geom_text(aes(label=paste0("LD %", LD), group=Treat), y=0.1, angle=90, vjust=1.5, data=results()$ld.list, col=ld.colors) + 
          xlab("Dose (µl/l)") + ylab("Mortality %") + 
          stat_smooth(method="glm", formula=cbind(y*input$subjects, (1-y)*input$subjects)~x, method.args=list(family=binomial(link=input$link)), fullrange=TRUE, size=1)          
  }, height=graph.height, res=150)
  
}
              
# Render Shiny app --------------------------------------------------------

shinyApp(ui, server)