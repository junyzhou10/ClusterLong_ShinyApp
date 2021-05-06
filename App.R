require(shiny)
require(dplyr)
require(DT)
require(shinydashboard)
require(plotrix)
require(ClusterLong)
require(doMC)
require(stats)
require(shinyhelper)
require(plotly)

sidebar <- dashboardSidebar(
  sidebarMenu(
    # read input
    
    conditionalPanel(
      "!input.loadExample",
      helper(
        selectizeInput("toyExamples",label = "Choose a data file", choices = list.files(pattern = "*.Rdata"), options = list(
          placeholder = 'Please click and select',
          onInitialize = I('function() { this.setValue(""); }')
        )),
        colour = "lightblue", type = "inline", content = "<p>In both toy examples:</p>
      <p>id: subject id (id)</p>
      <p>obs: observational times (x)</p>
      <p>y_1,...,y_5: five outcomes (Y)</p>
      <p>label: correct labels (not required for clustering purpose)</p>"
      )
    ),
    
    checkboxInput("loadExample", label = "or, Input your own data"),
    
    # user input 
    conditionalPanel(
      'input.loadExample',
      helper(
        fileInput("Datfile", "Choose a valid data file (csv, rds, Rdata)",
                  accept = c(
                    ".rds",
                    ".Rdata",
                    "text/csv",
                    "plain",
                    ".csv")
        ),
        colour = "lightblue", type = "inline", content = "<p>Please input data in LONG FORMAT</p>"
      )
    ),
    tags$hr(),
    
    # tab: visualization
    menuItem("Data Visualization", tabName = "visualization", icon = icon("th")),
    
    # tab: Clustering analysis
    menuItem("Clustering Analysis", icon = icon("flag-checkered"), tabName = "ClusterLong" #badgeLabel = "GO", badgeColor = "green"
    ),
    
    # tab: Clustering analysis
    menuItem("Summary of Clusters", icon = icon("chart-bar"), tabName = "Diagnosis")
    
    
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
            h2("Visualization of Longitudinal Data"),
            fluidRow(
              box(title = "Variable Specification",
                  status = "warning",
                  solidHeader = T,
                  width = 4,
                  collapsible = T,
                  conditionalPanel("output.fileUploaded", 
                                   selectInput('inputX', 'Specify the variable for observation time (x)', choices = c("Select a variable" = "")),  
                                   selectInput('inputID', 'Select the variable for subject identification (id)', choices = c("Select a variable" = "")),
                                   selectInput("inputY", "Specify the variable(s) for longitudinal response (Y)", choices = c("Select variable(s)" = ""), multiple = T)
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
                    helper(
                      sliderInput(inputId = "NoObs", 
                                  label = "Select sample size", 
                                  min   = 1, 
                                  max   = 100, 
                                  step  = 1,
                                  round = T,
                                  value = 1),
                      colour = "lightblue", type = "inline", content = "Samples are selected in random fashion"
                    )
                  ),
                  
                  tags$br(),
                  conditionalPanel(
                    "input.inputID != ''",
                    checkboxInput("SubjSpec", "View specific subjects?", value = FALSE)
                  ),
                  
                  conditionalPanel(
                    "input.SubjSpec",
                    selectizeInput("selectID", "Select Subject ID", choices = c("Select id(s) from list" = ""), multiple = T,
                                   options = list(maxOptions = 100, maxItems = 10)), 
                    helpText("No more than 10 subjects at the same time")
                  )
              ),
              box(title = "Sample Spaghetti Plot", 
                  status = "primary",
                  solidHeader = T,
                  width = 8, collapsible = T, collapsed = F,
                  plotlyOutput("Spaghetti"),
                  # plotOutput("Spaghetti"),
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
                    radioButtons("splineFunc", "Choose Degree of B-Spline Basis", choices = c("Cubic B-splines", "Quadratic B-splines", "Other"), selected = "Cubic B-splines"),
                    conditionalPanel(
                      'input.splineFunc =="Other"',
                      helper(
                        numericInput("degree", "Degree other than 2 or 3:", value = 4, min = 4, step = 1),
                        colour = "lightblue", type = "inline", content = "Degree of the piecewise polynomial, e.g. 3 for cubic splines"
                      )
                    ),
                    tags$hr(),
                    helper(
                      numericInput("df", "Number of interval knots", value = 3, min = 0, step = 1),
                      colour = "lightblue", type = "inline", content = "The total number of spline basis functions = Number of interval knots + Degree of spline basis + 1"
                    ),
                    
                    tags$hr(),
                    helper(
                      radioButtons("weightFunc", "Weighting method:", choices = c("Standard", "Softmax"), selected = "Standard"),
                      colour = "lightblue", type = "inline", content = "<p>For the cases with multiple response outcomes.</p>
                      <p>Standard method simply takes the average of weights coefficients.</p>
                      <p>Softmax adopts the softmax transformation of weights coefficients, which is suggested for the cases with large amount of noise outcomes</p>"
                    ),
                    tags$hr(),
                    helper(
                      checkboxInput("parallel", "Parallel computing?", value = FALSE),
                      colour = "lightblue", type = "inline", content = "<p>Parallel computing is highly recommended for datasets with large number of subjects (> 500).
                      Please notice the difference between the number of subjects and the number of observations. </p>
                      <p>This App will split data into subsets with size roughly as the specificed 'batch size', and implement ClusterLong algorithm in a parallel manner.</p>"
                    )
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
              box(title = "Results of Hierarchical Clustering", 
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
            h2("Visualization of Clustering Results"),
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
              column(width = 8, 
                     box(title = "Cluster Members", 
                         status = "primary",
                         solidHeader = T,
                         width = 12, collapsible = T,
                         div(style = 'overflow-x: scroll',verbatimTextOutput("resIDs")),
                         uiOutput("downResTraj")
                     ),
                     box(title = "Cluster Mean Trajectories", 
                         status = "primary",
                         solidHeader = T,
                         width = 12, collapsible = T,
                         plotlyOutput("MeanTraj")
                     )
              )
            ),
            fluidRow(
              box(title = "Cluster Characteristics",
                  status = "warning",
                  solidHeader = T,
                  width = 4,
                  collapsible = T,
                  uiOutput("FeatureSel")
              ),
              box(title = "Cluster Specific Distributions",
                  status = "primary",
                  solidHeader = T,
                  width = 8,
                  collapsible = T,
                  plotOutput("FeaturePlot")
              )
            )
    )
  )
)

# Put them together into a dashboardPage
ui <- dashboardPage(
  dashboardHeader(title = span("ClusterLong App",
                               style = "color: white; font-size: 22px")),
  sidebar,
  body
)


###### SERVER ######
server <- function(input, output, session) {
  observe_helpers()
  options(shiny.maxRequestSize=100*1024^2) # maximum inputfile size is set to 100M
  ###### Read input data file ######
  rawDat <- reactive({
    inFile<-input$Datfile
    if (!input$loadExample) {
      inFile = NULL
      if (input$toyExamples != "") {
        return(readRDS(input$toyExamples))
      }
    } else {
      if (!is.null(inFile)) {
        if (inFile$type == "text/csv") {
          return(read.csv(inFile$datapath, header = T))
        } else {
          return(readRDS(inFile$datapath))
        }
      }
    }
    return(NULL)
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
    updateRadioButtons(session, "splineFunc", selected = "Cubic B-splines")
    updateNumericInput(session, "df", value = 3)
    updateRadioButtons(session, "weightFunc", selected = "Standard")
    updateCheckboxInput(session, "parallel", value = FALSE) 
    updateSliderInput(session, "dropout", value = 20)
    updateNumericInput(session, "grpSize", value = 200)
    updateNumericInput(session, "Num.Cores", value = 7)
    updateSelectInput(session, "NumCl", selected = "")
    updateCheckboxInput(session, "SubjSpec", value = FALSE)
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
    output$FeatureSel  <- NULL
    output$downDendro  <- NULL
    output$downResTraj <- NULL
    output$FeaturePlot <- NULL
  })
  
  
  observeEvent(input$inputID, {
    if (!input$inputID==""){
      id.seq = unique(rawDat()[,input$inputID])
      updateSliderInput(session, inputId = "NoObs", max = min(100, length(id.seq)))
      updateSelectizeInput(session, inputId = "selectID", choices = id.seq, selected = "", 
                           options = list(maxItems = 10, maxOptions = length(id.seq)))
    }
    
  })
  observeEvent(input$inputY, {
    updateRadioButtons(session, inputId = "selectY", choices = input$inputY, selected = input$inputY[1], inline = T)
  })
  
  
  ###### figure in manuItem 1 (Data Visualization) ######
  output$Spaghetti <- renderPlotly({
    if (is.null(rawDat()) | input$selectY == "" | input$inputX == "" | input$inputID == "") {
      return(NULL)
    } else {
      dat = rawDat()[complete.cases(rawDat()[, c(input$inputX, input$selectY, input$inputID)]),]
      if (input$SubjSpec) { # plot specific subjects using inputs from selectID
        if (!is.null(input$selectID)){
          id.sel = input$selectID
          subset = dat[dat[,input$inputID] %in% id.sel,]
          subset <- highlight_key(subset, key=~get(input$inputID))
          p <- ggplot(subset, aes_string(input$inputX, input$selectY, group = input$inputID)) + geom_line(linetype = "dotted", color = "gray50") + geom_point(size = 1, color = "gray50")
          gg <- highlight(ggplotly(p, tooltip = c(input$inputID)), "plotly_click", 'plotly_doubleclick')
          return(gg)
        }
        
        # plot( 1, type = "n", xlab = input$inputX, ylab = input$selectY, xlim = range(dat[,input$inputX]), ylim = range(dat[,input$selectY]) )
        # id.sel = input$selectID
        # for (ids in id.sel){
        #   pos_ind = as.character(dat[, input$inputID]) == ids
        #   lines(dat[pos_ind,input$inputX], dat[pos_ind,input$selectY], type = "b", col = "gray30")
        # }
      } else { # if not, randomly select number of subjects according to NoObs
        id.sel = sample(unique(dat[,input$inputID]), input$NoObs)
        subset = dat[dat[,input$inputID] %in% id.sel,]
        subset <- highlight_key(subset, key=~get(input$inputID))
        p <- ggplot(subset, aes_string(input$inputX, input$selectY, group = input$inputID)) + geom_line(linetype = "dotted", color = "gray50") + geom_point(size = 1, color = "gray50")
        gg <- highlight(ggplotly(p, tooltip = c(input$inputID)), "plotly_hover", 'plotly_doubleclick')
        return(gg)
        
        ## regular plot
        # plot( 1, type = "n", xlab = input$inputX, ylab = input$selectY, xlim = range(dat[,input$inputX]), ylim = range(dat[,input$selectY]) )
        # id.sel = sample(unique(dat[,input$inputID]), input$NoObs)
        # for (ids in id.sel){
        #   pos_ind = dat[, input$inputID] == ids
        #   lines(dat[pos_ind,input$inputX], dat[pos_ind,input$selectY], type = "b", col = "gray50")
        # }
      }
    }
  })
  
  ##### manuItem 2 (Analysis): Reset button #####
  observeEvent(input$reset, {
    updateRadioButtons(session, "splineFunc", selected = "Cubic B-splines")
    updateNumericInput(session, "df", value = 3)
    updateRadioButtons(session, "weightFunc", selected = "Standard")
    updateCheckboxInput(session, "parallel", value = FALSE) 
    updateSliderInput(session, "dropout", value = 20)
    updateNumericInput(session, "grpSize", value = 200)
    updateNumericInput(session, "Num.Cores", value = 7)
    updateSelectInput(session, "NumCl", selected = "")
    updateCheckboxInput(session, "SubjSpec", value = FALSE)
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
    output$FeatureSel  <- NULL
    output$downDendro  <- NULL
    output$downResTraj <- NULL
    output$FeaturePlot <- NULL
  })
  
  ##### manuItem 2 (Analysis): Run button #####
  res = eventReactive(input$run, {
    showModal(modalDialog("Please wait...", footer=NULL))
    
    # call package ClusterLong
    x = rawDat()[,input$inputX]
    Y = rawDat()[, input$inputY]
    id = rawDat()[,input$inputID]
    weight.func = ifelse(input$weightFunc=="Standard", "standardize","softmax")
    if (input$splineFunc == "Cubic B-splines") {
      degree = 3
    } else if (input$splineFunc == "Quadratic B-splines") {
      degree = 2
    } else {
      degree = as.numeric(input$degree)
    }
    df = degree + input$df 
    
    if (input$parallel) {
      registerDoMC(cores = input$Num.Cores)
      dropout = input$dropout
      part.size = input$grpSize
      res = LongDataCluster(x=x,
                            Y=Y,
                            id=id, 
                            weight.func=weight.func, 
                            parallel = T, dropout = dropout, part.size = part.size, 
                            df=df, degree = degree)
    } else {
      res = LongDataCluster(x = x,
                            Y = Y,
                            id = id,
                            weight.func = weight.func,
                            parallel = FALSE,
                            df=df, degree = degree)
    }
    removeModal()
    return(res)
  })
  
  
  observeEvent(input$run, {
    output$resTxt <- renderText({
      paste0("Number of clusters by Gap.b: ", res()$No.Gapb)
    })
    output$resTxt1 <- renderText({
      paste0("Number of clusters by CH index: ", res()$No.CH)
    })
    output$resTxt2 <- renderText({
      paste0("Suggested by Gap.b: ", res()$No.Gapb)
    })
    output$resTxt3 <- renderText({
      paste0("Suggested by CH index: ", res()$No.CH)
    })
    
    output$UserSpec <- renderUI(
      numericInput(inputId = "UserNoCl", 
                   label = "Number of clusters by user:", 
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
      selectizeInput(inputId = "NumCl", 
                     label = "Specified by User: ", 
                     choices = seq(2,length(res()$Gap_b)),
                     selected = input$UserNoCl,
                     options = list(
                       placeholder = 'Please click and select'
                     )
      )
    )
    
    ## cluster feature selection
    output$FeatureSel <- renderUI(
      helper(
        selectizeInput("selectFeature", "Select a variable to show", 
                       choices = colnames(rawDat())[!colnames(rawDat()) %in% c(input$inputX, input$inputID)],
                       options = list(
                         placeholder = 'Please click and select',
                         onInitialize = I('function() { this.setValue(""); }')
                       )
        ),
        colour = "lightblue", type = "inline", content = "<p>Compare baseline patterns of the selected variable in the original dataset among detected clusters.</p>
            <p>Factor-type covariates will be shown in percent stacked barcharts; Continuous-type covariates will be shown in boxplots.</p>
            <p>Note: though described for baseline comparisons, subject level covariates are supported natually.</p>"
      )
    )
    
    # yield mean pattern plots
    observeEvent(input$NumCl,{
      
      no.cl = as.numeric(input$NumCl)
      
      output$resIDs <- renderText({
        sapply(seq(no.cl),
               function(ii) {
                 if (ii == 1) {
                   paste0("Cluster ",ii, " (n=", length(res()$Cluster.Lists[[no.cl]][[ii]]),") " , "ID: ",paste(res()$Cluster.Lists[[no.cl]][[ii]], collapse = ",") )
                 } else {
                   paste0("\nCluster ",ii, " (n=", length(res()$Cluster.Lists[[no.cl]][[ii]]),") " , "ID: ",paste(res()$Cluster.Lists[[no.cl]][[ii]], collapse = ",") )
                 }
               })
      })
      
      output$MeanTraj <- renderPlotly({
        MeanPlot(res(), No.Cluster = no.cl)
      })
      
      output$downResTraj <- renderUI(
        div(
          # style = "position: absolute; right: 0.1em; bottom: -2.5em;",
          style = "float:right",
          downloadButton(outputId = "down_res", label = "Save Clustering Results"),
          size = "xs",
          icon = icon("download", class = "opt")
        )
      )
      
    })
    
    output$FeaturePlot <- renderPlot(
      if (!is.null(input$selectFeature) & input$selectFeature!='' & !is.null(input$NumCl)) {
        sel.dat = rawDat()[,c(input$inputID, input$inputX, input$selectFeature)]; sel.dat = sel.dat[complete.cases(sel.dat), ]
        sel.dat = sel.dat %>% group_by(get(input$inputID)) %>% arrange(get(input$inputX)) %>% filter(row_number()==1) %>% ungroup()
        sel.dat = as.data.frame(sel.dat)
        sel.x = sel.dat[, input$selectFeature]
        grp = NULL
        for (id in sel.dat[,1]) {grp = c(grp, which(sapply(res()$Cluster.Lists[[as.numeric(input$NumCl)]], function(x) id %in% x)==TRUE))}
        sel.dat$grp = as.factor(paste("Cluster", grp))
        
        
        if (class(sel.x) == "factor" | length(unique(sel.x)) <= 10) {
          tab = table(sel.x, sel.dat[,"grp"])
          p_val = ifelse(min(tab)<=5, fisher.test(tab)$p.value, chisq.test(tab)$p.value)
          barplot(t(t(tab)/colSums(tab)),  border="white", xlab="", ylab = "Percent", main = paste0(input$selectFeature," (p-val: ", round(p_val,3), ")") , legend = TRUE, 
                  args.legend = list(bty = "n", x = "right", ncol = 1), xlim = c(0,ncol(tab)*1.5) )
        } else { # treat as continuous, yield boxplot
          p_val = summary(aov(sel.x~sel.dat$grp))[[1]][1,"Pr(>F)"]
          ggplot(sel.dat, aes_string(x="grp", y=input$selectFeature)) + geom_boxplot() + theme_bw() + labs(x = "")+
            annotate("text",  x=Inf, y = Inf, label = paste("p-val:", round(p_val,3)), vjust=1, hjust=1)
        }
        
      }
    )
    
  })
  
  
  
  
  ####### downloads #######
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


