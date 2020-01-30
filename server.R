#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  #----------------------------------------------------------------- Basic ----
  
  source("functionsInApp.R")
  
  # actually render the dropdownMenu PROGRESS
  output$menu <- renderMenu({
    dropdownMenu(
      type = "tasks", badgeStatus = "success",
      taskItem(value = 20, color = "red",
               "read information"
      ),
      taskItem(value = 50, color = "orange",
               "upload Data"
      ),
      taskItem(value = sum(res$variableTypes$set!=0)/NCOL(res$data)*100, color = "green",
               "defined variables"
      ),
      taskItem(value = 90, color = "green",
               "clean data"
      )
    )
  })
  
  ## Shows certain activations in the console 
  show.events = F
  entwickler = F
  
  ## Init. of the reactive values
  abc <- reactiveValues(selectedTab = numeric())
  temp <- reactiveValues()
  res <- reactiveValues(n = 106)
  res$temp <- NULL
  res$filePath <- "/noPath"
  res$fileEnding <- ".noEnding"
  
  
  # Create a Progress object
  progress_defineVar <- shiny::Progress$new()
  # Make sure it closes when we exit this reactive, even if there's an error
  #on.exit(progress_defineVar$close())
  
  progress_defineVar$set(message = "Define/redefine Variables: ", value = 0)
  
  
  #----------------------------------------------------------------- 0. Information ----
  output$link.Pseudonomisierung <- renderUI({
    h5("This process is called pseudonymisation. Find it out",a("here", 
                                                                href="https://en.wikipedia.org/wiki/ABCD", 
                                                                target="_blank")) 
  })
  #----------------------------------------------------------------- 1. Get data ----
  temp.table.simple <- reactive({
    DT::datatable(temp$data,
                  class = 'cell-border stripe',
                  options = list(pageLength = 10,
                                 #autoWidth = TRUE,
                                 server = T,
                                 scrollX='400px')) 
  })
  
  temp.to.res <- reactive({
    res$data <- temp$data
    res$filePath <- temp$filePath
    res$fileEnding <- temp$fileEnding
    res$variableTypes <- init.variableTypes(res)
    res$classified <- init.classified(res)
    res$varNames <- init.varNames(res)
    #temp$data <- NULL
    updateTabItems(session, "sidebarmenu", "panelIdDefine2")
    print(temp$data)
  })
  
  output$preload.csv <- renderUI({req(temp$data)
    actionButton("actionButton.preload.xlsx","Accept Data!",icon = icon("file-upload"),width = "100%")})
  
  output$preload.base <- renderUI({req(temp$data)
    actionButton("actionButton.preload.csv","Accept Data!",icon = icon("file-upload"),width = "100%")})
  
  output$preload.csv <- renderUI({req(temp$data)
    actionButton("actionButton.preload.base","Accept Data!",icon = icon("file-upload"),width = "100%")})
  
  output$preload.base <- renderUI({req(temp$data)
    actionButton("actionButton.preload.RNG","Accept Data!",icon = icon("file-upload"),width = "100%")})
  
  observeEvent(input$actionButton.preload.xlsx,{
    temp.to.res()
  })
  
  observeEvent(input$actionButton.preload.csv,{
    temp.to.res()
  })
  
  observeEvent(input$actionButton.preload.base,{
    temp.to.res()
  })
  
  observeEvent(input$actionButton.preload.RNG,{
    temp.to.res()
  })
  nextTab <- function(input.tabs,session){
    newtab <- switch(input.tabs,
                     "panelSubIdxlsx" = "panelIdDefine2",
                     "panelSubIdcsv"   = "panelIdDefine2",
                     "john" = "panelIdDefine2",
                     "RDatabase" = "panelIdDefine2",
                     "panelSubIdRNG" = "panelIdDefine2",
                     "panelIdDefine2"= "panelIdExplore2",
                     "panelIdExplore2" = "panelIdOverview",
                     "panelIdOverview" = "panelIdEvaluation"
    )
    updateTabItems(session, "sidebarmenu", newtab)
  }
  
  reloadData <- function(){}
  
  prevTab <- function(input.tabs,session){
    newtab <- switch(input.tabs,
                     # "panelSubIdxlsx" = "panelIdDefine2",
                     # "panelSubIdcsv"   = "panelIdDefine2",
                     # "john" = "panelIdDefine2",
                     # "RDatabase" = "panelIdDefine2",
                     # "panelSubIdRNG" = "panelIdDefine2",
                     # "panelIdDefine2"= "panelIdExplore2",
                     "panelIdExplore2" = "panelIdDefine2",
                     "panelIdOverview" = "panelIdExplore2",
                     "panelIdEvaluation" = "panelIdOverview"
    )
    updateTabItems(session, "sidebarmenu", newtab)
  }
  
  observeEvent(input$btn_nextTab, {
    nextTab(input$sidebarmenu,session)
  })
  
  observeEvent(input$btn_prevTab, {
    prevTab(input$sidebarmenu,session)
  })
  #============================================================= CSV OUtPUT ====
  
  output$contents.csv <- DT::renderDataTable({
    if (show.events) print("output$contents.csv")
    
    # input$file.csv will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    req(input$file.csv)
    
    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    tryCatch(
      { 
        temp$filePath <- input$file.csv$datapath
        print(str(input$file.csv))
        temp$fileEnding <- ".csv"
        #### implementing for missings
        temp$data <- read.csv(input$file.csv$datapath,
                              header = input$header,
                              sep = input$sep,
                              quote = input$quote)        
        
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    return(temp.table.simple())
  })
  
  #============================================================= XLSX OUtPUT ====
  output$contents.xlsx <- DT::renderDataTable({
    if (show.events) print("output$contents.xlsx")
    
    req(input$file.xlsx)
    tryCatch(
      {
        sheet <- input$select.sheet
        temp$filePath <- input$file.xlsx$datapath
        temp$fileEnding <- ".xlsx"
        temp$data <- data.frame(xlsx::read.xlsx(input$file.xlsx$datapath,sheetIndex = sheet))
        #print(res$variableTypes)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    return(temp.table.simple())
  })
  
  # interactive selector for sheets 
  output$selectInput.sheet.xlsx <- renderUI({
    req(input$file.xlsx)
    gdata::sheetNames(input$file.xlsx$datapath)
    selectInput("select.sheet", "Select Sheet",
                choices = gdata::sheetNames(input$file.xlsx$datapath),
                selected = 1)
  })
  
  
  
  #============================================================= RDat OUtPUT ====
  output$contents.RDat <- DT::renderDataTable({
    if (show.events) print("output$contents.RDat")
    req(input$inputRDat)
    tryCatch(
      {
        temp$data <- get(input$inputRDat)
        temp$filePath <- input$inputRDat
        temp$fileEnding <- ".RData"
        # temp$variableTypes <- init.variableTypes(res)
        # temp$classified <- init.classified(res)
        # temp$varNames <- init.varNames(res)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    return(temp.table.simple())
  })
  #============================================================= RNG OUtPUT ====
  output$contents.RNG <- DT::renderDataTable({
    req(input$nVariables,
        input$nPatients)
    if (show.events) print("output$contents.RNG")
    if (is.null(temp$data)){
      res$rngString <- c()
      temp$data <- data.frame(ID = 1:input$nPatients)
    } 
    
    if (!is.null(temp$data)){
      if (input$nVariables > NCOL(temp$data)){
        nAddVars <- input$nVariables-NCOL(temp$data)
        res$rngString <- c(res$rngString,create.RNG.String(nAddVars))
        temp$data <- add.RNG.Variable(nAddVars,temp$data,res$rngString)
      }
      if (input$nVariables < NCOL(temp$data)){
        nRmVars <- input$nVariables-NCOL(temp$data)
        res$rngString <- res$rngString[-(input$nVariables:NCOL(temp$data))]
        temp$data <- rm.RNG.Variable(nRmVars,temp$data)
      }
      if (input$nPatients > NROW(temp$data)){
        nAddObs <- input$nPatients-NROW(temp$data)
        temp$data <- add.RNG.Obs(nAddObs,temp$data,res$rngString)
      }
      if (input$nPatients < NROW(temp$data)){
        nRmObs <- NROW(temp$data) - input$nPatients
        temp$data <- rm.RNG.Obs(nRmObs,temp$data)
      }
    }
    temp$filePath <- "randomData"
    temp$fileEnding <- ".RData"
    # return( DT::datatable(temp$data,
    #                       class = 'cell-border stripe',
    #                       options = list(pageLength = 10,
    #                                      #autoWidth = TRUE,
    #                                      server = T,
    #                                      scrollX='400px'))
    # )
    return(temp.table.simple())
  }) 
  
  observeEvent(input$saveDataRNG,{
    if (show.events) print("input$saveDataRNG")
    res$data <- data.frame(res$temp)
    res$variableTypes <- init.variableTypes(res)
    res$classified <- init.classified(res)
    res$varNames <- init.varNames(res)
  })
  
  observeEvent(input$addHazard,{
    if (show.events) print("observeEvent(input$addHazard)")
    res$temp <- add.Hazard(res$temp)
  })
  
  ## nothing Placeholder
  myOptions <- reactive({
    if (show.events) print("myOptions")
    list(
      page=ifelse(input$pageable==TRUE,'enable','disable'),
      pageSize=input$pagesize,
      width=550
    )
  })
  
  
  #----------------------------------------------------------------- 2a. Define Redefine ----
  previousPage <- NULL
  
  output$MainBody=renderUI({
    fluidPage(
      h3("progressbar"),
      DT::dataTableOutput("data"),
      textOutput('myText')
    )})
  
  # myValue <- reactiveValues(employee = '')
  # output$myText <- renderText({
  #   myValue$employee
  # })
  
  print.res <- function(){
    for(i in names(res)){
      print(paste("------------------------------------",i,"------------------------------------"))
      print(res[[i]])
    }
  }
  
  excel.LETTERS <- function(x){
    out <- c()
    for(i in x){
      tmp = c()
      while(i>0){
        tmp <- c(LETTERS[((i-1) %% 26)+1],tmp)
        i <- floor(i/26)
      }
      out <- c(out,paste0(tmp,collapse = ""))
    }
    return(out)
  }
  
  output$data <- DT::renderDataTable({
    req(res$varNames)
    res$varNames %>% 
      `colnames<-`(letters[1:NCOL(res$varNames)])
  }, server = F, escape = F, selection = 'none',  extensions = "FixedColumns",
  options = list(scrollX = TRUE,
                 fixedColumns = list(leftColumns = 2))
  )
  
  dataModal <- function(id) {
    selectedCol <- as.numeric(strsplit(id, "_")[[1]][2])
    selectedRow <- as.numeric(strsplit(id, "_")[[1]][3])
    modalDialog(
      h3("Selected column:",selectedRow,"(Excel:",excel.LETTERS(selectedRow),")"),
      #plotOutput("plot")
      DT::dataTableOutput("variableTable"),
      
      footer = tagList(
        modalButton("Cancel")
      )
    )
  }
  
  output$plot = renderPlot(plot(res$data[,4]))
  output$variableTable = DT::renderDataTable({
    req(res$data)
    selectedRow <- as.numeric(strsplit(input$select_check, "_")[[1]][3])
    
    res$data[,selectedRow,drop=F]
  },options = list(scrollY ='400px',paging = F))
  
  observeEvent(input$select_check, {
    showModal(dataModal(input$select_check))
  })
  
  observeEvent(input$show, {
    showModal(dataModal("id_1_1_123"))
  })
  
  observeEvent(input$select_button, {
    #print(input$select_button)
    print(res$varNames)
    selectedRow <- as.numeric(strsplit(input$select_button, "_")[[1]][3])
    selectedCol <- as.numeric(strsplit(input$select_button, "_")[[1]][2])
    # print(res$varNames)
    # print(selectedRow)
    # print(selectedCol)
    if (res$variableTypes$set[selectedRow]!=selectedCol){
      res$varNames$`.`[selectedRow] <<- '<i class="far fa-check-circle"></i>'
      res$variableTypes$set[selectedRow] = selectedCol
      progress_defineVar$set(sum(res$variableTypes$set!=0)/NCOL(res$data), detail = paste("selected",selectedRow))
      res$varNames <- toggleSelected(res$varNames,selectedCol,selectedRow,nCol = 6)
      res$variableTypes$nMissing[selectedRow] <- sum(is.missing(res$data[,selectedRow]))
      res$variableTypes$nInClass[selectedRow] <- sum(is.non(res$data[,selectedRow],"varName!",names(def.varNames.buttons)[res$variableTypes$set[selectedRow]]))
      res$variableTypes$nInMonitor[selectedRow] <- 5
    } else {
      res$varNames$`.`[selectedRow] <<- '<i class="fa fa-times-circle"></i>'
      res$variableTypes$set[selectedRow] = 0
      progress_defineVar$set(sum(res$variableTypes$set!=0)/NCOL(res$data), detail = paste("deselected",selectedRow))
      res$varNames <- unToggle(res$varNames,selectedRow,nCol = 6)
      res$variableTypes$nMissing[selectedRow] <- NA
      res$variableTypes$nInClass[selectedRow] <- NA
      res$variableTypes$nInMonitor[selectedRow] <- NA
    }
    print("names(def.varNames.buttons)[res$variableTypes$set]")
    print((names(def.varNames.buttons)[res$variableTypes$set])[selectedRow])
    print(res$variableTypes)
  })
  
  output$typeInfo_numbers <- renderUI({
    print("got me")
    HTML(paste0("<font color=\"",get.typeColor("numeric"),"\"><b>    numbers</b></font>: everything measurable or countable e.g. days spend in hospital,
                                  number of relapses, age, weight."))
    print(
      paste0("<font color=\"",get.typeColor("numeric"),"\"><b>    numbers</b></font>: everything measurable or countable e.g. days spend in hospital,
                                  number of relapses, age, weight."))
  })
  output$typeInfo_integers <- renderUI({tagList(HTML(paste0("<font color=\"#556677\"><b>integers</b></font>: just a subclass of numeric with only hole numbers."))) })
  output$typeInfo_dates <- renderUI({paste0("<font color=\"",get.typeColor("Date"),"\"><b>dates</b></font>: hard to identify for a computer due to many different formats e.g.
                                   february/1/2010, 1-feb-2010, 01.02.2010, 4533454.") })
  output$typeInfo_strings <- renderUI({paste0("<font color=\"",get.typeColor("character"),"\"><b>strings</b></font>: names and basically everything which is not a date or a number.
                                   This might be due to special symbols e.g. > <= % ( ] or different/wrong
                                   formatting.") })
  output$typeInfo_factors <- renderUI({paste0("<font color=\"",get.typeColor("factor"),"\"><b>factors</b></font>: Explain me!!!.") })
  
  #----------------------------------------------------------------- 3a. Explore ----
  output$exploreVarNames <- DT::renderDataTable({
    data.frame(variable = colnames(res$data),
               type=ifelse(res$variableTypes$set==0,
                           NA,
                           names(def.varNames.buttons)[res$variableTypes$set]), 
               monitor = NA, # set TRUE/FALSE
               nMissing=res$variableTypes$nMissing,
               nInClass=res$variableTypes$nInClass,
               nInMonitor=res$variableTypes$nInMonitor)
  },
  selection = 'single',options = list(scrollY ='400px',paging = F)
  )
  
  observeEvent(input$exploreVarNames_rows_selected, {
    print("res$variableTypes")
    print(res$variableTypes)
    print("names(def.varNames.buttons)[res$variableTypes$set]")
    print(names(def.varNames.buttons)[res$variableTypes$set])
    print("input$exploreVarNames_rows_selected")
    print(input$exploreVarNames_rows_selected)
  })
  
  output$plot.tmp <- renderPlotly({
    req(input$exploreVarNames_rows_selected)
    dta <- data.frame(1:NROW(res$data),res$data[,input$exploreVarNames_rows_selected])
    colnames(dta) <- c("row.nr.",names(res$data)[input$exploreVarNames_rows_selected])
    p <- ggplot(data=dta,aes_string(x="row.nr.",y=names(res$data)[input$exploreVarNames_rows_selected])) +
      geom_point() + ylab("a")
    ggplotly(p)
  })
  
  explore.rightPanel <- function(set){
    if (set==0){
      out <- list(h3("No type defined yet!"))
    }
    if (set==1){
      out <- list(
        fluidRow(
          column(12,
                 tags$head(
                   tags$style(
                     HTML("label{float:left;}")
                   )
                 ),
                 radioButtons("decimal",HTML("decimal separator:",HTML('&nbsp;&nbsp;&nbsp;')),c(". (dot)",", (comma)"),inline = T)
          )
        ),
        fluidRow(
          column(6,numericInput("min.1","min",-Inf)),
          column(6,numericInput("min.1","max",Inf))
        )
      )
    }
    if (set==2){
      out <- list(
        column(6,numericInput("min.2","min",-Inf)),
        column(6,numericInput("min.2","max",Inf))
      )
    }
    if (set==3){
      out <- list(h3("Define the correct classes"))
    }
    if (set==4){
      out <- list(h3("Define the correct classes"),
                  h3("And order."))
    }
    if (set==5){
      out <- list(
        h3("Define date format. If they are only numbers e.g. 492933 then write: dddd"),
        textInput("textinput","date format","e.g. dd.mm.yy")
      )
    }
    if (set==6){
      out <- list(h3("No structure!"))
    }
    return(out)
  }
  
  dynamicUI.explore.rightPanel <- reactive({
    #if (1==1)
    req(input$exploreVarNames_rows_selected)
    return(
      append(
        list(h5(paste("rowSelected:",input$exploreVarNames_rows_selected)),
             h5(paste("setSelected:",res$variableTypes$set[input$exploreVarNames_rows_selected]))),
        explore.rightPanel(res$variableTypes$set[input$exploreVarNames_rows_selected])
      )
    )
  })
  output$explore.sidePanel <- renderUI( {
    dynamicUI.explore.rightPanel()
  })
  #----------------------------------------------------------------- 4. Overview ----
  
  output$complete.Obs <- renderPlot({
    if (show.events) print("output$complete.Obs")
    req(res$data)
    return(plot.complete(res$data,res$variableTypes))
  })  
  
  # colors have problems
  output$Overview.out.right.dist <- renderPlot({
    if (show.events) print("output$Overview.out.right.dist")
    req(res$data)
    df <- res$variableTypes %>% 
      mutate(newest = get.newestTypes(.),
             col = get.typeColor(newest))
    collist <- df[!duplicated(df$newest),] %>%
      mutate(col = get.typeColor(newest))
    col = collist$col
    names(col) = collist$newest
    df %>% 
      ggplot(aes(x="newest",fill=newest)) + geom_bar(stat = "count") + #,fill = unique(get.typesDf(res$data)$col)) +
      scale_fill_manual(values = col) +
      coord_polar("y", start=0) + theme_void()+
      theme(axis.text.x=element_blank())
    
  })
  
  output$Overview.out.left.verbatim <- renderPrint({
    if (show.events) print("output$Overview.out.left.verbatim")
    req(res$data)
    list(nCol = NCOL(res$data),
         nRow = NROW(res$data),
         nComplete = sum(complete.cases(res$data)),
         nMissing = sum(is.na(res$data)),
         fileName = "Not yet implemented!",
         types = get.typesDf(res$data))
  })
  #----------------------------------------------------------------- 5. Evaluation (Interactive Histogram) ----
  
  ## nothing Placeholder
  dynamicUi <- reactive({
    return(
      list(
        sliderInput("obs", 
                    "Number of observations:", 
                    min = 1,
                    max = 1000, 
                    value = res$n),
        actionButton("action","Add somthing!",width = 120)
      )
    )
  })
  
  ## nothing Placeholder
  output$sidePanel <- renderUI( {
    dynamicUi()
  })
  
  ## nothing Placeholder
  output$distPlot <- renderPlot({
    
    # generate an rnorm distribution and plot it
    dist <- rnorm(res$n)
    hist(dist)
  })
  
  ## nothing Placeholder
  observeEvent(input$action,{
    print(ls())
    if (show.events) print("input$action")
    res$n <- res$n + 1
  })
  
  observeEvent(input$saveRes,{
    x <- reactiveValuesToList(res,all.names = TRUE)
    x <- reactiveValuesToList(res,all.names = TRUE)
    x <- reactiveValuesToList(res,all.names = TRUE)
    str(x)
    resSaveName <- "res.RData"
    save(x,file = resSaveName)
    print(paste0("res saved in ",resSaveName,"."))
  })
  
  observeEvent(input$obs,{
    print("-------------------------------- env.server ------------------------------")
    try(print(get.memory.allocation(env.server)))
    print("--------------------------------  .GlobalEnv --------------------------------")
    try(print(get.memory.allocation(.GlobalEnv)))
    #n <<- rnorm(1000000)
    #print(object.size())
    if (show.events) print("input$obs")
    res$n <- input$obs
  })
  
  #----------------------------------------------------------------- entwickler = T ----
  if (entwickler){
    env.server <- environment()
    env <- .GlobalEnv
    dynamicUi <- reactive({
      return(
        list(
          actionButton("showMemory","print memory in console"),
          tableOutput('fara'),
          tableOutput('foo'),
          tableOutput('fobs')
        )
      )
    })
    output$memoryUsage <- renderUI( {
      dynamicUi()
    })
    
    observeEvent(input$showMemory,{
      print("-------------------------------- env.server ------------------------------")
      try(print(get.memory.allocation(env.server)))
      print("--------------------------------  .GlobalEnv --------------------------------")
      try(print(get.memory.allocation(.GlobalEnv)))
    })
    
    output$foo <- renderTable({
      get.memory.allocation(env.server)
    })
    output$fara <- renderTable(get.memory.allocation(env.server,detail = F))
    output$fobs <- renderTable(gc())
    
  }
})
