#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

btn.style.preload <- "color: #fff; background-color: #666666; border-color: #999999"

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  #----------------------------------------------------------------- Basic ----
  
  source("functionsInApp.R")
  options(stringsAsFactors = FALSE)
  
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
                  options = list(paging = F,
                                 #pageLength = 10,
                                 #autoWidth = TRUE,
                                 server = T,
                                 scrollX='400px',
                                 scrollY='600px'),
                  selection = 'none') 
  })
  
  temp.to.res <- reactive({
    res$data <- temp$data
    res$filePath <- temp$filePath
    res$fileEnding <- temp$fileEnding
    res$variableTypes <- init.variableTypes(res)
    res$classified <- init.classified(res)
    res$monitor <- init.monitor(res)
    res$varNames <- init.varNames(res)
    #temp$data <- NULL
    updateTabItems(session, "sidebarmenu", "panelIdDefine2")
    print(temp$data)
  })
  
  output$preload.xlsx <- renderUI({req(temp$data)
    actionButton("actionButton.preload.xlsx","Accept Data!",icon = icon("file-upload"),width = "100%", style = btn.style.preload)})
  
  output$preload.csv <- renderUI({req(temp$data)
    actionButton("actionButton.preload.csv","Accept Data!",icon = icon("file-upload"),width = "100%", style = btn.style.preload)})
  
  output$preload.base <- renderUI({req(temp$data)
    actionButton("actionButton.preload.base","Accept Data!",icon = icon("file-upload"),width = "100%", style = btn.style.preload)})
  
  output$preload.RNG <- renderUI({req(temp$data)
    actionButton("actionButton.preload.RNG","Accept Data!",icon = icon("file-upload"),width = "100%", style = btn.style.preload)})
  
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
  
  
  #----------------------------------------------------------------- 2 Define Redefine ----
  previousPage <- NULL
  
  # collapsable information boxes
  observeEvent(input$infoBox_numeric_titleId, {
    js$collapse("infoBox_numeric")
  })  
  observeEvent(input$infoBox_factor_titleId, {
    js$collapse("infoBox_factor")
  })
  observeEvent(input$infoBox_ordered_titleId, {
    js$collapse("infoBox_ordered")
  })
  observeEvent(input$infoBox_integer_titleId, {
    js$collapse("infoBox_integer")
  })
  observeEvent(input$infoBox_Date_titleId, {
    js$collapse("infoBox_Date")
  })

  
  output$MainBody=renderUI({
    box(width=12,
      DT::dataTableOutput("data"),
      div(paste0("Variables defined: ",sum(res$variableTypes$set!=0),"/",NCOL(res$data))),
      prgoressBar(sum(res$variableTypes$set!=0)/NCOL(res$data)*100, color = "green", striped = TRUE, active = TRUE, size = "sm")
      
      
      # textOutput('myText')
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
  
  # renders the class selection table
  output$data <- DT::renderDataTable(
    {
      req(res$varNames)
      # print(res$varNames)
      addTooltip(session, "button_1_1", "title", placement = "bottom", trigger = "hover",
                 options = NULL)
      res$varNames %>% 
        `colnames<-`(letters[1:NCOL(res$varNames)])
    }, 
    server = F, 
    escape = F,
    selection = 'none',  
    extensions = "FixedColumns",
    options = list(scrollX = TRUE,
                   fixedColumns = list(leftColumns = 2))
  ) 
  # tooltip = tooltipOptions(title = "Click to see inputs !")
  
  dataModal <- function(id) {
    selectedCol <- as.numeric(strsplit(id, "_")[[1]][2])
    selectedRow <- as.numeric(strsplit(id, "_")[[1]][3])
    modalDialog(
      h3("Selected column:",selectedRow,"(Excel:",excel.LETTERS(selectedRow),")"),
      #plotOutput("plot")
      column(6,
             h5("some statistics"),
             h5("number of rows"),
             h5("number of empty entries"),
             h5("number of unique levels"),
             h5("number of empty cells")
      ),
      column(6,
      DT::dataTableOutput("variableTable"),
      ),
      footer = fluidRow(
        column(12,align="center",
          modalButton("Cancel")
        )
      )
      ,
      easyClose = T
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
  
  #### what does this
  observeEvent(input$show,{
    print("#================================================================================#")
    print(res)
    print("res$classified--------------------------------------------------------#")
    try(print(head(res$classified)))
    print("res$variableTypesn----------------------------------------------------#")
    try(print(res$variableTypes))
    print("res$n-----------------------------------------------------------------#")
    try(print(res$n))
    # try(print(res$varNames))
    
    #showModal(dataModal("id_1_1_123"))
  })
  
  observeEvent(input$select_button, {
    print("input$select_button")
    # print(input$select_button)
    # print(res$varNames)
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
      
      res$classified[,selectedRow] <- res$classified[,selectedRow] +
        10 * is.non(res$data[,selectedRow],"varName!",names(def.varNames.buttons)[res$variableTypes$set[selectedRow]])
      
      if (selectedCol == 6){
        res$variableTypes$hasMonitor[selectedRow] = NA
      }
      
      res$variableTypes$nMissing[selectedRow] <- sum(is.missing(res$data[,selectedRow]))
      res$variableTypes$nInClass[selectedRow] <- sum(is.non(res$data[,selectedRow],"varName!",names(def.varNames.buttons)[res$variableTypes$set[selectedRow]]))
      res$variableTypes$nInMonitor[selectedRow] <- NA
    } else {
      res$varNames$`.`[selectedRow] <<- '<i class="fa fa-times-circle"></i>'
      res$variableTypes$set[selectedRow] = 0
      progress_defineVar$set(sum(res$variableTypes$set!=0)/NCOL(res$data), detail = paste("deselected",selectedRow))
      res$varNames <- unToggle(res$varNames,selectedRow,nCol = 6)
      
      ## Check if monitor is still active! xxx!!!???
      res$classified[,selectedRow] <- res$classified[,selectedRow] %% 10
      
      res$variableTypes$nMissing[selectedRow] <- NA
      res$variableTypes$nInClass[selectedRow] <- NA
      res$variableTypes$nInMonitor[selectedRow] <- NA
    }
    print("names(def.varNames.buttons)[res$variableTypes$set]")
    #print((names(def.varNames.buttons)[res$variableTypes$set])[selectedRow])
    #print(res$variableTypes)
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
  observeEvent(input$infoBox_monitor_titleId, {
    js$collapse("infoBox_monitor")
  })  
  
  output$exploreVarNames <- DT::renderDataTable(
    {
      data.frame(variable = colnames(res$data),
                 type=res$variableTypes$set, 
                 monitor = NA, # set TRUE/FALSE
                 nMissing = res$variableTypes$nMissing,
                 nWrongClass = res$variableTypes$nInClass,
                 nNotInMonitor = res$variableTypes$nInMonitor)
    },
    selection = 'single',  
    #extensions = "FixedColumns",
    options = list(scrollX =TRUE)
  )
  
  observeEvent(input$exploreVarNames_rows_selected, {
    print("res$variableTypes")
    #print(res$variableTypes)
    print("names(def.varNames.buttons)[res$variableTypes$set]")
    #print(names(def.varNames.buttons)[res$variableTypes$set])
    print("input$exploreVarNames_rows_selected")
    #print(input$exploreVarNames_rows_selected)
  })
  
  output$plot.tmp <- renderPlotly({
    req(input$exploreVarNames_rows_selected)
    dta <- data.frame(1:NROW(res$data),res$data[,input$exploreVarNames_rows_selected])
    colnames(dta) <- c("row.nr.",names(res$data)[input$exploreVarNames_rows_selected])
    p <- ggplot(data=dta,aes_string(x="row.nr.",y=names(res$data)[input$exploreVarNames_rows_selected])) +
      geom_point() + ylab("a")
    ggplotly(p)
  })
  
  observeEvent(input$accept_monitor,{
    print(input$exploreVarNames_rows_selected) 
    
    # update hasMonitor
    res$variableTypes$hasMonitor[input$exploreVarNames_rows_selected] = TRUE
    
    # generate actual monitor
    if (res$variableTypes$set[input$exploreVarNames_rows_selected] == 1){
      res$monitor[[res$variableTypes$Variables[input$exploreVarNames_rows_selected]]]$decimal = input$decimal
      res$monitor[[res$variableTypes$Variables[input$exploreVarNames_rows_selected]]]$minimum = 
        ifelse(is.na(input$min_numeric),-Inf,input$min_numeric)
      res$monitor[[res$variableTypes$Variables[input$exploreVarNames_rows_selected]]]$maximum = 
        ifelse(is.na(input$max_numeric),Inf,input$max_numeric)
      print(res$monitor[[res$variableTypes$Variables[input$exploreVarNames_rows_selected]]])
    }
    if (res$variableTypes$set[input$exploreVarNames_rows_selected] == 2){
      res$monitor[[res$variableTypes$Variables[input$exploreVarNames_rows_selected]]]$minimum = 
        ifelse(is.na(input$min_integer),-Inf,input$min_integer)
      res$monitor[[res$variableTypes$Variables[input$exploreVarNames_rows_selected]]]$maximum = 
        ifelse(is.na(input$max_integer),Inf,input$max_integer)
      print(res$monitor[[res$variableTypes$Variables[input$exploreVarNames_rows_selected]]])
    }
      
  })
  
  output$out1 <- renderPrint(head(res$data[!is.na(res$data[,input$exploreVarNames_rows_selected]),input$exploreVarNames_rows_selected],5))
  
  explore.rightPanel <- function(set){
    if (set==0){
      return(
        out <- list(h4("There can not be a monitor for nothing!"))
      )
    }
    if (set==1){ # numeric
      out <- list(
        fluidRow(
          column(12,
                 tags$head(
                   tags$style(
                     HTML("label{float:left;}")
                   )
                 ),
                 radioButtons("decimal",HTML("decimal separator:",HTML('&nbsp;&nbsp;&nbsp;')),c(". (dot)",", (comma)"),inline = F)
          )
        ),
        fluidRow(
          column(6,numericInput("min_numeric","minimum",-Inf)),
          column(6,numericInput("max_numeric","maximum",Inf)),
          column(12,h5("If there is no limit, enter nothing."))
        )
      )
    }
    if (set==2){ # integer
      out <- list(
        column(6,numericInput("min_integer","minimum",-Inf)),
        column(6,numericInput("max_integer","maximum",Inf)),
        column(12,h5("If there is no limit, enter nothing."))
      )
    }
    if (set==3){ # cateforial
      out <- list(
        h3("Define the correct classes"),
        selectInput('in3', 'Options', unique(res$data[,input$exploreVarNames_rows_selected]), multiple=TRUE, selectize=FALSE)
      )
    }
    if (set==4){ # ordered factor
      out <- list(h3("Define the correct classes"),
                  h3("And order."),
                  selectInput('in3', 'Options', unique(res$data[,input$exploreVarNames_rows_selected]), multiple=TRUE, selectize=FALSE))
    }
    if (set==5){ # date
      out <- list(
        h5("The first five dates are recognized as follows:"),
        verbatimTextOutput('out1'),
        h5("Define date format. If they are only numbers e.g. 492933 then write: dddd and define a Origin"),
        textInput("textinput","date format","e.g. dd.mm.yy"),
        h5("Some software saves dates by a different origin. check your dates and consider updating the origin."),
        dateInput("origin","Origin:", "1900-01-01")
      )
    }
    if (set==6){
      return(
        out <- list(h4("No structure -> nothing to monitor!"))
      )
    }
    return(
      append(
        out,
        list(
          tags$hr(),
          column(12,
                 actionButton("accept_monitor","Accept Monitor!",width = "100%")
          )
        )
      )
    )
  }
  
  dynamicUI.explore.rightPanel <- reactive({
    #if (1==1)
    req(input$exploreVarNames_rows_selected)
    return(
      #append(
        # list(h5(paste("rowSelected:",input$exploreVarNames_rows_selected)),
        #      h5(paste("setSelected:",res$variableTypes$set[input$exploreVarNames_rows_selected]))),
        explore.rightPanel(res$variableTypes$set[input$exploreVarNames_rows_selected])
      #)
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
