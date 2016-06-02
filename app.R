library(shiny)
library(ggplot2)
library(MASS)
library(xlsx)

ld <- function(subjects, responding, dose, ldP=0.5, linkFunction='probit') {
  dead <- responding
  live <- subjects - dead
  
  # fitting generalized linear model (error distribution is binomial, and link function is probit)
  model <- glm(cbind(dead, live) ~ dose, family=binomial(link=linkFunction))
  
  # predict doses for binomial assay model
  ldDose <- dose.p(model, p=c(ldP))
  
  # return back results
  x   <- ldDose[[1]]
  se  <- attr(ldDose, "SE")[[1]]
  a   <- summary(model)$coefficients[1,1]
  b   <- summary(model)$coefficients[2,1]
  ase <- summary(model)$coefficients[1,2]
  bse <- summary(model)$coefficients[2,2]
  
  list('x'=x, 'se'=se, 'a'=a, 'ase'=ase, 'b'=b, 'bse'=bse)
}

# Header ------------------------------------------------------------

header <- headerPanel('LD50')

# Sidebar -----------------------------------------------------------

sidebar <- sidebarPanel(
  tags$head(
    tags$style(type="text/css", ".col-sm-4 { max-width: 350px; } "),
    tags$style(type="text/css", ".well { max-width: 340px; }")
  ),
  
  tags$img(src='icarda.png', height=90, width=190),
  fileInput('datafile', 'Upload Your Data:', accept=c('application/vnd.ms-excel', 'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet', '.xls', '.xlsx')),
  sliderInput('p', 'Effective (or Lethal) Dose:', min=0, max=100, value=50),
  selectInput('link', 'Link Function:', c('Logit'='logit', 'Probit'='probit', 'Complementary log-log'='cloglog')),
  selectInput('transform', 'Take logs of Explanatory:', c('None'='none', 'Base 10'='log', 'Base e'='ln')),
  uiOutput('subjectsCol'),
  uiOutput('respondingCol'),
  uiOutput('doseCol'),
  uiOutput('groupsCol'),
  tags$small(tags$em('Tip: click Backspace to unselect groups.'))
)

# Body ---------------------------------------------------------------

body <- mainPanel(
          tabsetPanel(
            tabPanel('Input Data', tags$h2('Input Data'), dataTableOutput('contents')),
            tabPanel('Analysis Output', tags$h2('Analysis Output'), tableOutput('ldList')),
            tabPanel('Graphics Output', tags$h2('Graphics Output'), downloadButton('downloadGraph', 'Download High Resolution Graph'), plotOutput('ldGraph')),
            tabPanel('Help', tags$h2('Help'), withMathJax(),
                     tags$a(href='data.xlsx', 'Sample Data File'),
                     tags$h4('Logit function (1944): $$Pr(x) = \\frac{1}{1+e^{-(a+b.x)}}$$
                              Probit function (1933): $$Pr(x) = \\phi{(a+b.x)}$$
                              Where $\\phi$ is the standard normal Cumulative Distribution Function (CDF):
                              $$\\phi(x) = \\frac{1}{\\sqrt{2\\pi}} \\int_{-\\infty}^{x} e^{(-t^2/2)} dt$$
                             Complementary log-log (1922): $$Pr(x) = 1-e^{-e^{a+b.x}}$$'))
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
    
    read.xlsx(inFile$datapath, sheetIndex=1, header=TRUE)
  })
  
  output$contents <- renderDataTable({
    importedData()
  })
  
  # Build user interface
  output$subjectsCol <- renderUI({
    selectInput('subjectsCol', 'Numbers of Subjects (e.g. Total):*', as.list(c('', colnames(importedData()))))
  })
  
  output$respondingCol <- renderUI({
    selectInput('respondingCol', 'Numbers Responding (e.g. Dead):*', as.list(c('', colnames(importedData()))))
  })
  
  output$doseCol <- renderUI({
    selectInput('doseCol', 'Explanatory Variate (e.g. Dose):*', as.list(c('', colnames(importedData()))))
  })
  
  output$groupsCol <- renderUI({
    selectInput('groupsCol', 'Groups (e.g. Treatment):', c('None'='', as.list(colnames(importedData()))))
  })

  # Read the Data
  modelInputs <- reactive({
    validate(
      need(input$subjectsCol != '', 'Please select numbers of subjects column'),
      need(input$respondingCol != '', 'Please select numbers of responding column'),
      need(input$doseCol != '', 'Please select explanatory variate column')
    )
    
    validate(
      need(input$transform == 'none' | all(importedData()[,input$doseCol] > 0), 'You can not take logs for 0 or negative explanatory values!')
    )
    
    subjects   <- importedData()[,input$subjectsCol]
    responding <- importedData()[,input$respondingCol]
    
    if(input$transform == 'none') { 
      dose <- importedData()[,input$doseCol] 
    } else if(input$transform == 'log') {
      dose <- log10(importedData()[,input$doseCol])
    } else if(input$transform == 'ln') {
      dose <- log(importedData()[,input$doseCol])
    }
    
    if(input$groupsCol != '') {
      groups <- as.factor(importedData()[,input$groupsCol])
    } else {
      groups <- as.factor(rep.int('All', length(dose)))
    }
    
    data.frame(subjects, responding, dose, groups)
  })
  
  # Do the Analysis
  results <- reactive({
    ldList <- NULL

    for(i in levels(modelInputs()$groups)){
      subjects   <- modelInputs()$subjects[modelInputs()$groups == i]
      responding <- modelInputs()$responding[modelInputs()$groups == i]
      dose       <- modelInputs()$dose[modelInputs()$groups == i]
      
      val <- ld(subjects, responding, dose, input$p/100, input$link)
      ldList <- rbind(ldList, data.frame(i, input$p, val$x, val$se, val$a, val$ase, val$b, val$bse))
    }

    names(ldList) <- c('Treat', 'LD', 'Dose', 'SE', 'Intercept (a)', 'a SE', 'Coefficient (b)', 'b SE')
    row.names(ldList) <- NULL
    ldList
  })

  output$ldList <- renderTable({
    results()
  }, digits=c(0, 0, 0, 2, 3, 3, 4, 3, 4))
  
  graphHeight <- reactive({
    600*length(levels(results()$Treat))
  })
  
  respondingPlot <- reactive({
    g <- ggplot(modelInputs(), aes(x=dose, y=responding/subjects)) +
      facet_wrap(~Treat, ncol=1) + 
      geom_point(position=position_jitter(width=0, height=0.05)) +
      geom_vline(aes(xintercept=Dose, group=Treat), data=results(), col='red') +
      geom_text(aes(x=Dose, label=paste0('LD %', input$p), group=Treat), y=0.1, angle=90, vjust=1.5, data=results(), col='red') +
      xlab(input$doseCol) + ylab(paste(input$respondingCol, '%')) +
      stat_smooth(method='glm', formula=cbind(y*10, (1-y)*10)~x, method.args=list(family=binomial(link=input$link)), fullrange=TRUE, size=1)
  })
  
  output$ldGraph <- renderPlot({
    print(respondingPlot())
  }, height=graphHeight)
  
  
  output$downloadGraph <- downloadHandler(
    filename = function() { paste0(input$datafile$name, '.png') },
    content = function(file) {
      # 6 inches wide, and manage have golden ratio for graphs! 
      h <- 6 * length(levels(results()$Treat)) / 1.618
      png(file, res=600, height=h, width=6, units="in")
      print(respondingPlot())
      dev.off()
    },
    contentType = 'image/png'
  )  
}
              
# Render Shiny app --------------------------------------------------------

shinyApp(ui, server)