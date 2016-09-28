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

header <- headerPanel('ICARDA BioComputing Online (Phase II): Lethal Dose/Concentration Calculator')

# Sidebar -----------------------------------------------------------

sidebar <- sidebarPanel(
  tags$head(
    tags$style(type="text/css", ".col-sm-4 { max-width: 360px; } "),
    tags$style(type="text/css", ".well { max-width: 350px; }")
  ),
  
  tags$img(src='icarda.png', height=90, width=190),
  fileInput('datafile', 'Upload Your Data:', accept=c('application/vnd.ms-excel', 'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet', '.xls', '.xlsx')),
  tags$div(title='p hint', sliderInput('p', 'Effective (or Lethal) Dose/Concentration:', min=0, max=100, value=50)),
  tags$div(title='conf hint', sliderInput('conf', 'Level of Confidence Interval:', min=0, max=100, value=80)),
  tags$div(title='Function of the response proportion to link to linear function of dose', selectInput('link', 'Link Function:', c('Logit'='logit', 'Probit'='probit', 'Complementary log-log'='cloglog'))),
  tags$div(title='To transform the Explanatory to a linear scale', selectInput('transform', 'Take logs of Explanatory:', c('None'='none', 'Base 10'='log', 'Base e'='ln'))),
  tags$div(title='e.g. Total subjects tested', uiOutput('subjectsCol')),
  tags$div(title='e.g. Dead subjects observed', uiOutput('respondingCol')),
  tags$div(title='e.g. Dose', uiOutput('doseCol')),
  tags$div(title='e.g. Treatment', uiOutput('groupsCol')),
  tags$small(tags$em('Tip: click Backspace to unselect groups.')),
  tags$hr(), bookmarkButton()
)

# Body ---------------------------------------------------------------

body <- mainPanel(
          tabsetPanel(
            tabPanel('Input Data', tags$h2('Input Data'), dataTableOutput('contents')),
            tabPanel('Analysis Output', tags$h2('Model Parameters'), htmlOutput('modelParam'), tags$h2('Analysis Output'), tableOutput('ldList'), downloadButton('downloadResults', 'Download'),tags$h2('Summary Statistics'), verbatimTextOutput("summary")),
            tabPanel('Graphics Output', tags$h2('Graphics Output'), downloadButton('downloadGraph', 'Download High Resolution Graph'), plotOutput('ldGraph')),
            tabPanel('Help', tags$h2('Help'), withMathJax(),
                     'In toxicology, the median lethal dose, LD50 (abbreviation for "lethal dose, 50%"), LC50 (lethal concentration, 50%) 
                     is a measure of the lethal dose of a toxin, radiation, or pathogen. The value of LD50 for a substance is the dose 
                     required to kill half the members of a tested population after a specified test duration.',
                     
                     tags$a(href='data.xlsx', 'Sample Data File'),
                     tags$h4('Citation:'),
                     tags$cite(textOutput('citation')),
                     tags$h4('Contact us:'),
                     tags$address('Maintainer:	Khaled El-Sham\'aa <k.el-shamaa at cgiar.org>'),
                     tags$h4('J. Berkson (1944), Logit function:'),
                     tags$blockquote('$$Pr(x) = \\frac{1}{1+e^{-(a+b.x)}}$$'),
                     tags$h4('C. I. Bliss (1934), Probit function:'),
                     tags$blockquote('$$Pr(x) = \\phi{(a+b.x)}$$'),
                     tags$h4('Where normal Cumulative Distribution Function (CDF):'),
                     tags$blockquote('$$\\phi(x) = \\frac{1}{\\sqrt{2\\pi}} \\int_{-\\infty}^{x} e^{(-t^2/2)} dt$$'),
                     tags$h4('R. A. Fisher (1922), Complementary log-log:'),
                     tags$blockquote('$$Pr(x) = 1-e^{-e^{a+b.x}}$$'))
          )
)

# Setup Shiny app UI components -------------------------------------------

ui <- shinyUI(pageWithSidebar(header, sidebar, body))

# Setup Shiny app back-end components -------------------------------------

server <- function(input, output, session) { 
    
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
  
  output$citation <- renderText({
    url <- paste0(session$clientData$url_protocol, '//', session$clientData$url_hostname)
    if(session$clientData$url_port != '') url <- paste0(url, ':', session$clientData$url_port)
    url <- paste0(url, session$clientData$url_pathname)
    
    citeDate <- format(Sys.Date(), format='%Y, %B %d')
    paste0('Biometrics and Statistics Section, ICARDA. (', citeDate, '). LD50 (Version 1.0) [Web application]. Retrieved from ', url)
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

    names(ldList) <- c('Treat', 'LD/LC', 'Dose', 'SE', 'Intercept (a)', 'a SE', 'Coefficient (b)', 'b SE')
    row.names(ldList) <- NULL
    ldList
  })

  output$ldList <- renderTable({
    results()
  }, digits=c(0, 0, 0, 2, 3, 3, 4, 3, 4))
  
  output$downloadResults <- downloadHandler(
    filename = function() { paste0(input$datafile$name, '.csv') },
    content = function(file) {
      write.csv(results(), file)
    }
  )
  
  output$summary <- renderPrint({
    summary(data.frame(Dose=modelInputs()$dose, Responding=modelInputs()$responding, Subjects=modelInputs()$subjects, Groups=factor(modelInputs()$groups)))
  })
  
  output$modelParam <- renderText({
    l.label <- list('logit'='Logit', 'probit'='Probit', 'cloglog'='Complementary log-log')
    t.label <- list('none'='None', 'log'='Logarithmic Transformation (Base 10)', 'ln'='Natural Logarithm (Base e)')
    print(paste('<u>Link Function:</u>', l.label[input$link], '<br/>\n', '<u>Explanatory Transformation:</u>', input$transform))
  })
  
  graphHeight <- reactive({
    600*length(levels(results()$Treat))
  })
  
  respondingPlot <- reactive({
    data <- merge(modelInputs(), results(), by.x="groups", by.y="Treat", all.x=TRUE)
    g <- ggplot(data, aes(x=dose, y=responding/subjects))
    g <- g + facet_wrap(~groups, ncol=1)
    g <- g + geom_point(position=position_jitter(width=0, height=0.05))
    g <- g + geom_vline(aes(xintercept=Dose), col='red')
    g <- g + geom_text(aes(x=Dose, label=paste0('LD %', input$p)), y=0.1, angle=90, vjust=1.5, col='red')
    g <- g + xlab(input$doseCol) + ylab(paste(input$respondingCol, '%'))
    g <- g + stat_smooth(method='glm', method.args=list(family=binomial(link=input$link)), level=input$conf/100, fullrange=TRUE, size=1)
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

enableBookmarking(store="url")
shinyApp(ui, server)