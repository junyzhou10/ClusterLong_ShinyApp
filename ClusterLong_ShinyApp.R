require(shiny)
require(dplyr)
require(DT)
require(shinydashboard)
require(plotrix)
require(ClusterLong)
require(doMC)

sidebar <- dashboardSidebar(
  sidebarMenu(
    # read input
    fileInput("Datfile", "Choose a valid data file (csv, rds, Rdata)",
              accept = c(
                ".rds",
                ".Rdata",
                "text/csv",
                "text/comma-separated-values,text/plain",
                ".csv")
    ),
    tags$p("Note: please input data in LONG FORMAT"),
    tags$hr(),
    
    # tab: visualization
    menuItem("Data Visualization", tabName = "visualization", icon = icon("th")),
    
    # tab: Clustering analysis
    menuItem("Clustering Analysis", icon = icon("flag-checkered"), tabName = "ClusterLong",
             badgeLabel = "GO", badgeColor = "green"),
    
    # tab: Clustering analysis
    menuItem("Diagnosis", icon = icon("chart-bar"), tabName = "Diagnosis")
    
    
  ),
  
  tags$footer(p("Have a question? Spot an error? Send an email ", 
                tags$a(href = "mailto:junyzhou@iu.edu", 
                       tags$i(class = 'fa fa-envelope', style = 'color:#999999'), 
                       target = '_blank'), style = "font-size: 80%"), 
              p("App created by Junyi Zhou (2018)", style = "font-size: 80%"),
              p("Last updated: April 2021", style = "font-size: 65%"),
              align = "left", 
              style = "
              position:absolute;
              bottom:0;
              width:100%;
              height:100px;   /* Height of the footer */
              color: white;
              padding: 10px;
              background-color: black;
              z-index: 1000;")
)

body <- dashboardBody(
  ###### UI for manuItem 1 (Data Visualization) ######
  tabItems(
    tabItem(tabName = "visualization",
            h2("Visualize Longitudinal Data"),
            fluidRow(
              box(title = "Variable Specification",
                  status = "warning",
                  solidHeader = T,
                  width = 4,
                  collapsible = T,
                  conditionalPanel("output.fileUploaded", 
                                   selectInput('inputX', 'Which variable is the observational time (x)?', choices = c("Select a variable" = "")),  
                                   selectInput("inputY", "Which variable(s) is(are) the outcome(s) (Y)?", choices = c("Select variable(s)" = ""), multiple = T),
                                   selectInput('inputID', 'Which variable indicating subject ID (id)?', choices = c("Select a variable" = ""))
                  )
              ),
              box(title = "Table of Input Data", 
                  status = "primary",
                  solidHeader = T,
                  width = 8, collapsible = T,
                  div(DT::DTOutput("rawDat"), style = "font-size: 80%;")
              )
            ), # row
            fluidRow(
              box(title = "Plot Arguments",
                  status = "warning",
                  solidHeader = T,
                  width = 4, 
                  collapsible = T,
                  collapsed = F,
                  
                  conditionalPanel(
                    "input.inputY != ''", 
                    radioButtons("selectY", "Select One Outcome to Display", choices = "")
                  ),
                  
                  conditionalPanel(
                    "input.inputID != '' & !input.SubjSpec", 
                    sliderInput(inputId = "NoObs", 
                                label = "Select the Number of Samples to Display", 
                                min   = 1, 
                                max   = 100, 
                                step  = 1,
                                round = T,
                                value = 1),
                    tags$em("Samples are selected in random fashion")
                  ),
                  
                  tags$br(),
                  conditionalPanel(
                    "input.inputID != ''",
                    checkboxInput("SubjSpec", "View specific subjects?", value = FALSE)
                  ),
                  
                  conditionalPanel(
                    "input.SubjSpec",
                    selectizeInput("selectID", "Select Subject ID", choices = c("Select id(s) from list" = ""), multiple = T), 
                    tags$em("No more than 10 subjects at the same time")
                  )
              ),
              box(title = "Sample Spaghetti Plot", 
                  status = "primary",
                  solidHeader = T,
                  width = 8, collapsible = T, collapsed = F,
                  # plotlyOutput("Spaghetti")
                  plotOutput("Spaghetti"),
                  # download button
                  conditionalPanel(
                    "input.selectY",
                    div(
                      style = "position: absolute; right: 0.5em; bottom: 0.5em;",
                      downloadButton(outputId = "down_sample_plot", label = ""),
                      size = "xs",
                      icon = icon("download", class = "opt")
                    )
                  )
              )
            )
    ),
    
    ###### UI for manuItem 2 (Clustering Analysis) ######
    tabItem(tabName = "ClusterLong",
            h2("Longitudinal Data Clustering"),
            tags$em("based on R package ClusterLong (https://github.com/junyzhou10/ClusterLong)"),
            tags$hr(),
            fluidRow(
              box(title = "Settings",
                  status = "warning",
                  solidHeader = T,
                  width = 4,
                  collapsible = T,
                  conditionalPanel(
                    "input.inputID != '' & input.inputX != '' & input.inputY != ''", 
                    radioButtons("splineFunc", "Choose Spline Basis", choices = c("Cubic B-splines", "Natural Cubic Splines"), selected = "Cubic B-splines"),
                    tags$hr(),
                    numericInput("df", "Degrees of freedom (basis functions)", value = 7, min = 4, step = 1),
                    tags$hr(),
                    radioButtons("weightFunc", "How to aggregate multiple outcomes?", choices = c("Standard", "Softmax"), selected = "Standard"),
                    tags$em("Softmax only for the case with large amount of noise outcomes"),
                    tags$hr(),
                    checkboxInput("preprocess", "Data preprocessing? (recommend)", value = TRUE),
                    tags$hr(),
                    checkboxInput("parallel", "Parallel computing? (highly recommend)", value = FALSE)
                  ),
                  conditionalPanel(
                    "input.parallel",
                    numericInput("Num.Cores", "Register Cores", min = 2, step = 1, value = 7),
                    sliderInput("dropout", "Dropout at", min = 10, max = 100, value = 20, step = 1, round = T),
                    numericInput("grpSize", "Batch size", min = 50, step = 10, value = 300)
                  ),
                  
                  conditionalPanel(
                    "input.inputID != '' & input.inputX != '' & input.inputY != ''", 
                    tags$hr(),
                    actionButton("run", "RUN", icon = icon("flag-checkered"), style = "display: inline-block !important;"),
                    actionButton("reset", "RESET", icon = icon("redo"), style = "display:inline-block !important;")
                  )
              ),
              box(title = "Hierarchical Analysis Results", 
                  status = "primary",
                  solidHeader = T,
                  width = 8, collapsible = T,
                  textOutput("resTxt"),
                  textOutput("resTxt1"),
                  uiOutput("UserSpec"),
                  plotOutput("dendrogram"),
                  # download button
                  uiOutput("downDendro"),
                  plotOutput("NoClusterImages", height = "320px"),
                  uiOutput("MaxNoCl")
                  
              )
              
            )
            
    ),
    
    ###### UI for manuItem 3 (Diagnose tab) ######
    tabItem(tabName = "Diagnosis",
            h2("Visualize Clustering Results"),
            tags$hr(),
            fluidRow(
              box(title = "Number of Clusters",
                  status = "warning",
                  solidHeader = T,
                  width = 4,
                  collapsible = T,
                  textOutput("resTxt2"),
                  textOutput("resTxt3"),
                  uiOutput("FinalNoCl")
              ),
              box(title = "Clustering Results/Patterns", 
                  status = "primary",
                  solidHeader = T,
                  width = 8, collapsible = T,
                  div(style = 'overflow-x: scroll',verbatimTextOutput("resIDs")),
                  plotOutput("MeanTraj"),
                  uiOutput("downResTraj")
              )
              
            )
            
    )
  )
)

# Put them together into a dashboardPage
ui <- dashboardPage(
  dashboardHeader(title = "ClusterLong App"),
  sidebar,
  body
)



server <- function(input, output, session) {
  ###### Read input data file ######
  rawDat <- reactive({
    inFile <- input$Datfile
    if (is.null(inFile)) {
      return(NULL)
    }
    
    if (inFile$type == "text/csv") {
      rawDat = read.csv(inFile$datapath, header = T)
    } else {
      rawDat = readRDS(inFile$datapath)
    }
    return(rawDat)
  })
  
  # use to check if data is uploaded
  output$fileUploaded <- reactive({
    return(!is.null(rawDat()))
  })
  outputOptions(output, 'fileUploaded', suspendWhenHidden=FALSE)
  
  
  ###### table in manuItem 1 (Data Visualization) ######
  output$rawDat <- renderDT({
    rawDat()
  }, options = list(scrollX = TRUE))
  
  # once observe input data, update conditional panel
  observe({
    var_names <- colnames(rawDat())
    updateSelectInput(session, "inputX", choices = var_names, selected = "")
    updateSelectInput(session, "inputY", choices = var_names, selected = "")
    updateSelectInput(session, 'inputID', choices = var_names, selected = "")
  })
  
  
  observeEvent(input$inputID, {
    if (!input$inputID==""){
      id.seq = unique(rawDat()[,input$inputID])
      updateSliderInput(session, inputId = "NoObs", max = min(100, length(id.seq)))
      updateSelectizeInput(session, inputId = "selectID", choices = id.seq, 
                           selected = "", options= list(maxItems = 10, maxOptions = length(id.seq)))
    }
    
  })
  observeEvent(input$inputY, {
    updateRadioButtons(session, inputId = "selectY", choices = input$inputY, selected = "", inline = T)
  })
  
  
  ###### figure in manuItem 1 (Data Visualization) ######
  output$Spaghetti <- renderPlot({
    if (is.null(rawDat()) | input$selectY == "" | input$inputX == "" | input$inputID == "") {
      return(NULL)
    } else {
      if (input$SubjSpec) { # plot specific subjects using inputs from selectID
        plot( 1, type = "n", xlab = input$inputX, ylab = input$selectY, xlim = range(rawDat()[,input$inputX]), ylim = range(rawDat()[,input$selectY]) )
        id.sel = as.numeric(input$selectID)
        for (ids in id.sel){
          pos_ind = rawDat()[, input$inputID] == ids
          lines(rawDat()[pos_ind,input$inputX], rawDat()[pos_ind,input$selectY], type = "b", col = "gray30")
        }
        
      } else { # if not, random chose number of subjects according to NoObs
        plot( 1, type = "n", xlab = input$inputX, ylab = input$selectY, xlim = range(rawDat()[,input$inputX]), ylim = range(rawDat()[,input$selectY]) )
        id.sel = sample(unique(rawDat()[,input$inputID]), input$NoObs)
        for (ids in id.sel){
          pos_ind = rawDat()[, input$inputID] == ids
          lines(rawDat()[pos_ind,input$inputX], rawDat()[pos_ind,input$selectY], type = "b", col = "gray50")
        }
      }
    }
  })
  
  ##### manuItem 2 (Analysis): Reset button #####
  observeEvent(input$reset, {
    updateRadioButtons(session, "splineFunc", selected = "Cubic B-splines")
    updateNumericInput(session, "df", value = 7)
    updateRadioButtons(session, "weightFunc", selected = "Standard")
    updateCheckboxInput(session, "preprocess", value = TRUE) 
    updateCheckboxInput(session, "parallel", value = FALSE) 
    updateSliderInput(session, "dropout", value = 20)
    updateNumericInput(session, "grpSize", value = 200)
    updateNumericInput(session, "Num.Cores", value = 7)
    output$dendrogram <- NULL
    output$NoClusterImages <- NULL
    output$MaxNoCl <- NULL
    output$resTxt  <- NULL
    output$resTxt1 <- NULL
    output$resTxt2 <- NULL
    output$resTxt3 <- NULL
    output$FinalNoCl<-NULL
    output$UserSpec<- NULL
    output$resIDs  <- NULL
    output$MeanTraj<- NULL
    output$downResTraj<- NULL
  })
  
  ##### manuItem 2 (Analysis): Run button #####
  res = eventReactive(input$run, {
    showModal(modalDialog("Please wait...", footer=NULL))
    # call package ClusterLong
    if (input$parallel) {
      registerDoMC(cores = input$Num.Cores)
      x = rawDat()[,input$inputX]
      Y = rawDat()[, input$inputY]
      id = rawDat()[,input$inputID]
      functional = ifelse(input$splineFunc=="Cubic B-splines", "bs","ns")
      preprocess = input$preprocess
      weight.func = ifelse(input$weightFunc=="Standard", "standardize","softmax")
      dropout = input$dropout
      part.size = input$grpSize
      res = LongDataCluster(x,Y,id,functional, preprocess, weight.func, parallel = T, dropout,part.size)
    } else {
      res = LongDataCluster(x = rawDat()[,input$inputX],
                            Y = rawDat()[, input$inputY],
                            id = rawDat()[,input$inputID],
                            functional = ifelse(input$splineFunc=="Cubic B-splines", "bs","ns"),
                            preprocess = input$preprocess,
                            weight.func = ifelse(input$weightFunc=="Standard", "standardize","softmax"),
                            parallel = FALSE
      )
    }
    removeModal()
    return(res)
  })
  
  
  observeEvent(input$run, {
    output$resTxt <- renderText({
      paste0("Number of clusters (Gap.b): ", res()$No.Gapb)
    })
    output$resTxt1 <- renderText({
      paste0("Number of clusters (CH index): ", res()$No.CH)
    })
    output$resTxt2 <- renderText({
      paste0("Suggested by Gap.b: ", res()$No.Gapb)
    })
    output$resTxt3 <- renderText({
      paste0("(Suggested by CH index: ", res()$No.CH, ")")
    })
    
    output$UserSpec <- renderUI(
      numericInput(inputId = "UserNoCl", 
                   label = "Specify a different number", 
                   min   = 2, 
                   step  = 1,
                   value = res()$No.Gapb,
                   width = '200px')
    )
    # dendrogram
    output$dendrogram <- renderPlot({
      DendroPlot(res(), No.Cluster = input$UserNoCl, main = "Dendrogram")
    })
    
    output$downDendro <- renderUI(
      div(
        style = "position: absolute; right: 0.5em; bottom: 0.5em;",
        downloadButton(outputId = "down_dendro_plot", label = ""),
        size = "xs",
        icon = icon("download", class = "opt")
      )
    )
    
    # Ancillary plots
    output$NoClusterImages <- renderPlot({
      graphics::layout(matrix(c(1,1,1,2,2), nrow = 1))
      # gap.b & CH
      twoord.plot(lx = seq(length(res()$Gap_b)), rx = seq(length(res()$CH.index)), 
                  ly = res()$Gap_b, ry = res()$CH.index, xlim = c(1, ifelse(is.null(input$MaxNumber),10,input$MaxNumber)),
                  xlab= "Number of Clusters", ylab = "Gap.b", rylab = "CH index",
                  type = c("b", "b"), lpch = 20, rpch = 20, lcol = rgb(0, 0, 0.9, 0.9), rcol = rgb(0.3, 0.9, 0.3, 0.9),
                  main = "Gap.b & CH index")
      points(res()$No.Gapb, res()$Gap_b[res()$No.Gapb], col = "red", cex = 1, pch = 19)
      if (!is.null(input$UserNoCl)) {
        points(input$UserNoCl, res()$Gap_b[input$UserNoCl], col = "darkred", cex = 1.5, pch = 1, lwd = 2)
        legend("bottomright", c("Opt. Cluster No. (by Gap.b)", "User specified Cluster No."), pch = c(20,1), col = c("red","darkred"), bty = "n", cex = 1)
      } else {
        legend("bottomright", "Opt. Cluster No. (by Gap.b)", pch = 20, col = "red", bty = "n", cex = 1)
      }
      
      # merging cost
      plot(res()$Addres, xlim = c(1, ifelse(is.null(input$MaxNumber),10,input$MaxNumber)), type = "b", xlab = "Number of Clusters", ylab = "", main = "Merging Cost")
      # abline(h = mean(res()$Addres[(res()$No.Gapb-1):res()$No.Gapb]), lty = 2, col = "gray60")
      if (!is.null(input$UserNoCl)) {
        abline(h = mean(res()$Addres[(input$UserNoCl-1):input$UserNoCl]), lty = 2, col = "darkred")
      }
    })
    
    
    # select display range
    output$MaxNoCl <- renderUI(
      tagList(
        sliderInput(inputId = "MaxNumber", 
                    label = "Display range", 
                    min   = 5, 
                    max   = length(res()$Gap_b), 
                    step  = 1,
                    round = T,
                    value = length(res()$Gap_b)),
        tags$em("The optimal number of clusters is determined by:"),
        tags$br(),
        tags$em("Gap.b (main): the first local maxima"),
        tags$br(),
        tags$em("CH index (reference): the global maxima")
      )
    )
    
    ####### Diagnosis tab #######
    output$FinalNoCl <- renderUI(
      tagList(
        selectInput(inputId = "NumCl", 
                    label = "How many clusters?", 
                    choices = seq(1,length(res()$Gap_b)),
                    selected = 1),
        
        conditionalPanel(
          condition = "input.NumCl != 1 ",
          sliderInput(
            inputId = "NumSamples",
            label   = "Display how many samples in each cluster?",
            min     = 0,
            max     = min(100, length(res()$Gap_b)),
            value   = 0,
            step    = 1,
            round   = TRUE
          ),
          
          div(
            style = "float: right",
            downloadButton(outputId = "down_meantraj_plot", label = "Download Figure"),
            size = "xs",
            icon = icon("download", class = "opt")
          )
        ),
        
        conditionalPanel(
          condition = "input.NumSamples != 0 ",
          checkboxInput("fixSeed", "Fix seed", value = TRUE)
        )
      )
    )
    
    observeEvent(input$NumCl,{
      no.cl = as.numeric(input$NumCl)
      
      if (no.cl > 1){
        output$resIDs <- renderText({
          sapply(seq(no.cl),
                 function(ii) {
                   if (ii == 1) {
                     paste(" Cluster",ii, "ID :",paste(res()$Cluster.Lists[[no.cl]][[ii]], collapse = ",") )
                   } else {
                     paste("\n Cluster",ii, "ID :",paste(res()$Cluster.Lists[[no.cl]][[ii]], collapse = ",") ) 
                   }
                 })
        })
        
        output$MeanTraj <- renderPlot({
          alpha = ifelse(input$NumSamples>100, 0.05, 0.8-0.0075*input$NumSamples)
          if (input$fixSeed) {
            MeanPlot(res(), No.Cluster = no.cl, add.sample = input$NumSamples, trsp = alpha )
          } else {
            MeanPlot(res(), No.Cluster = no.cl, add.sample = input$NumSamples, seed = runif(1)*10000 , trsp = alpha)
          }
        })
        
        output$downResTraj <- renderUI(
          div(
            style = "position: absolute; right: 0.1em; bottom: -2.5em;",
            downloadButton(outputId = "down_res", label = "Save Clustering Results"),
            size = "xs",
            icon = icon("download", class = "opt")
          )
        )
      }
    })
    
  })
  
  
  ####### downloads #######
  ## Fig 1:
  output$down_sample_plot <- downloadHandler(
    filename = function(){
      paste("SpaghettiPlot", Sys.Date(), "png", sep = ".")
    },
    content = function(file){
      png(file)
      if (input$SubjSpec) { # plot specific subjects using inputs from selectID
        plot( 1, type = "n", xlab = input$inputX, ylab = input$selectY, xlim = range(rawDat()[,input$inputX]), ylim = range(rawDat()[,input$selectY]) )
        id.sel = as.numeric(input$selectID)
        for (ids in id.sel){
          pos_ind = rawDat()[, input$inputID] == ids
          lines(rawDat()[pos_ind,input$inputX], rawDat()[pos_ind,input$selectY], type = "b", col = "gray30")
        }
        
      } else { # if not, random chose number of subjects according to NoObs
        plot( 1, type = "n", xlab = input$inputX, ylab = input$selectY, xlim = range(rawDat()[,input$inputX]), ylim = range(rawDat()[,input$selectY]) )
        id.sel = sample(unique(rawDat()[,input$inputID]), input$NoObs)
        for (ids in id.sel){
          pos_ind = rawDat()[, input$inputID] == ids
          lines(rawDat()[pos_ind,input$inputX], rawDat()[pos_ind,input$selectY], type = "b", col = "gray50")
        }
      }
      dev.off()
    }
  )
  
  ## Fig 2: dendrogram & indices
  output$down_dendro_plot <- downloadHandler(
    filename = function(){
      paste("Dendrogram", Sys.Date(), "png", sep = ".")
    },
    content = function(file){
      png(file, width = 540, height = 660)
      graphics::layout(matrix(c(1,1,1,1,1,2,2,2,3,3), nrow = 2, byrow = T))
      # dendro
      DendroPlot(res(), No.Cluster = input$UserNoCl, main = "Dendrogram")
      # gap.b & CH
      twoord.plot(lx = seq(length(res()$Gap_b)), rx = seq(length(res()$CH.index)), 
                  ly = res()$Gap_b, ry = res()$CH.index, xlim = c(1, ifelse(is.null(input$MaxNumber),10,input$MaxNumber)),
                  xlab= "Number of Clusters", ylab = "Gap.b", rylab = "CH index",
                  type = c("b", "b"), lpch = 20, rpch = 20, lcol = rgb(0, 0, 0.9, 0.9), rcol = rgb(0.3, 0.9, 0.3, 0.9),
                  main = "Gap.b & CH index")
      points(res()$No.Gapb, res()$Gap_b[res()$No.Gapb], col = "red", cex = 1, pch = 19)
      if (!is.null(input$UserNoCl)) {
        points(input$UserNoCl, res()$Gap_b[input$UserNoCl], col = "darkred", cex = 1.5, pch = 1, lwd = 2)
        legend("bottomright", c("Opt. Cluster No. (by Gap.b)", "User specified Cluster No."), pch = c(20,1), col = c("red","darkred"), bty = "n", cex = 1)
      } else {
        legend("bottomright", "Opt. Cluster No. (by Gap.b)", pch = 20, col = "red", bty = "n", cex = 1)
      }
      
      # merging cost
      plot(res()$Addres, xlim = c(1, ifelse(is.null(input$MaxNumber),10,input$MaxNumber)), type = "b", xlab = "Number of Clusters", ylab = "", main = "Merging Cost")
      # abline(h = mean(res()$Addres[(res()$No.Gapb-1):res()$No.Gapb]), lty = 2, col = "gray60")
      if (!is.null(input$UserNoCl)) {
        abline(h = mean(res()$Addres[(input$UserNoCl-1):input$UserNoCl]), lty = 2, col = "darkred")
      }
      
      dev.off()
    }
  )
  
  ## Fig 3: 
  output$down_meantraj_plot <- downloadHandler(
    filename = function(){
      paste("MeanTraj", Sys.Date(), "png", sep = ".")
    },
    content = function(file){
      no.Y = length(input$inputY)
      mod3 = no.Y %% 3
      mod4 = no.Y %% 4
      if (mod3==0) {
        ncolumn = 3
      } else if (mod4==0) {
        ncolumn = 4
      } else {
        ncolumn = ifelse(mod3<=mod4, 4, 3)
      }
      
      png(file,
          width = ncolumn*180,
          height = ceiling(no.Y/ncolumn)*240)
      no.cl = as.numeric(input$NumCl)
      alpha = ifelse(input$NumSamples>100, 0.05, 0.8-0.0075*input$NumSamples)
      if (input$fixSeed) {
        MeanPlot(res(), No.Cluster = no.cl, add.sample = input$NumSamples, trsp = alpha )
      } else {
        MeanPlot(res(), No.Cluster = no.cl, add.sample = input$NumSamples, seed = runif(1)*10000 , trsp = alpha)
      }
      dev.off()
    }
  )
  
  ## Res file: 
  output$down_res <- downloadHandler(
    filename = function(){
      paste("ClusterRes", Sys.Date(), "Rdata", sep = ".")
    },
    content = function(file) {
      no.cl = as.numeric(input$NumCl)
      saveRDS(res()[[no.cl]], file)
    }
  )
}


# Run the application 
shinyApp(ui = ui, server = server)


