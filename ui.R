#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(readxl)
library(plyr)
library(dplyr)
library(tidyr)
library(pander)
library(ggplot2)
library(formattable)
theme_set(theme_bw())

library(shiny)
library(shinythemes)
library(shinyWidgets)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyBS)
library(shinyjs)
library(plotly)


source("languageE.R")

btn.style.nextPrev <- "color: #fff; background-color: #337ab7; border-color: #2e6da4"

convertMenuItem <- function(mi,tabName) {
  mi$children[[1]]$attribs['data-toggle']="tab"
  mi$children[[1]]$attribs['data-value'] = tabName
  mi
}

exampleDatasetList <- data()$results %>% 
  data.frame() %>%
  filter(Package=="datasets") %>%
  dplyr::select("Item")
exampleData.frameList <- exampleDatasetList[
  apply(exampleDatasetList,1,
        function(x){
          tryCatch(is.data.frame(get(x)),error=function(e)F)
        }),1]

jscode <- "
shinyjs.collapse = function(boxid) {
$('#' + boxid).closest('.box').find('[data-widget=collapse]').click();
}
"

# Define UI for application that draws a histogram
dashboardPage(skin = "blue",
              useShinyjs(),
              
              
              #/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\ START navbarPage 
              # ========================================== header ====
              header = dashboardHeader(title = "Nice.Data",
                                       dropdownMenuOutput("menu")
              ),
              # ========================================== Sidebar ====
              sidebar =   dashboardSidebar(
                sidebarMenu(id = "sidebarmenu",
                            #----------------------------------------------- 0. Information ----
                            menuItem(all.tabs$label[all.tabs$pos==1], 
                                     tabName = all.tabs$tabName[all.tabs$pos==1], 
                                     icon = icon(all.tabs$icon[all.tabs$pos==1])),
                            #----------------------------------------------- 1. Get data ----
                            menuItem(all.tabs$label[all.tabs$pos==2], 
                                     tabName = all.tabs$tabName[all.tabs$pos==2], 
                                     icon = icon(all.tabs$icon[all.tabs$pos==2]),
                                     startExpanded = FALSE,
                                     #-------------------------------------- .xlsx ----
                                     convertMenuItem(
                                       menuItem(all.tabs$label[all.tabs$pos==2.1], 
                                                tabName = all.tabs$tabName[all.tabs$pos==2.1], 
                                                icon = icon(all.tabs$icon[all.tabs$pos==2.1]),
                                                list(fileInput("file.xlsx", "Choose xlsx File",
                                                               multiple = FALSE,
                                                               accept = c(".xlsx")),
                                                     # Horizontal line
                                                     # tags$hr(),
                                                     # Input: Checkbox if file has header
                                                     uiOutput("selectInput.sheet.xlsx")
                                                )
                                       ),
                                       tabName = all.tabs$tabName[all.tabs$pos==2.1]
                                     ),
                                     #-------------------------------------- .csv ----
                                     convertMenuItem(
                                       menuItem(all.tabs$label[all.tabs$pos==2.2], 
                                                tabName = all.tabs$tabName[all.tabs$pos==2.2], 
                                                icon = icon(all.tabs$icon[all.tabs$pos==2.2]),
                                                list(
                                                  fileInput("file.csv", "Choose CSV File",
                                                            multiple = FALSE,
                                                            accept = c("text/csv",
                                                                       "text/comma-separated-values,text/plain",
                                                                       ".csv")),
                                                  # Horizontal line
                                                  menuItem("options", tabName = "panelSubIdcsvOptions", icon = icon("cog"),
                                                           list(
                                                             # tags$hr(),
                                                             # Input: Checkbox if file has header
                                                             checkboxInput("header", "Header", TRUE),
                                                             # Input: Select separator
                                                             radioButtons("sep", "Separator",
                                                                          choices = c(Comma = ",",
                                                                                      Semicolon = ";",
                                                                                      Tab = "\t"),
                                                                          selected = ","),
                                                             # Input: Select quotes
                                                             radioButtons("quote", "Quote",
                                                                          choices = c(None = "",
                                                                                      "Double Quote" = '"',
                                                                                      "Single Quote" = "'"),
                                                                          selected = '"')
                                                           )
                                                  )
                                                )
                                       ),
                                       tabName = all.tabs$tabName[all.tabs$pos==2.2]
                                     ),
                                     #-------------------------------------- (placeholder) Johns-dataSet ----
                                     convertMenuItem(
                                       menuItem(all.tabs$label[all.tabs$pos==2.3], 
                                                tabName = all.tabs$tabName[all.tabs$pos==2.3], 
                                                icon = icon(all.tabs$icon[all.tabs$pos==2.3]),
                                                list(
                                                  h5("this is a placholder for"),
                                                  h5("the proposed dataformat of"),
                                                  h5("John.")
                                                )
                                       ),tabName = all.tabs$tabName[all.tabs$pos==2.4]
                                     ),
                                     #-------------------------------------- from R database ----
                                     convertMenuItem(
                                       menuItem(all.tabs$label[all.tabs$pos==2.4], 
                                                tabName = all.tabs$tabName[all.tabs$pos==2.4], 
                                                icon = icon(all.tabs$icon[all.tabs$pos==2.4]),
                                                list(
                                                  selectInput("inputRDat",NA,exampleData.frameList)
                                                )
                                       ),tabName = all.tabs$tabName[all.tabs$pos==2.4]
                                     ),
                                     #-------------------------------------- generate Dataset... ----
                                     convertMenuItem(
                                       menuItem(all.tabs$label[all.tabs$pos==2.5], 
                                                tabName = all.tabs$tabName[all.tabs$pos==2.5], 
                                                icon = icon(all.tabs$icon[all.tabs$pos==2.5]),
                                                list(
                                                  numericInput("nPatients","Number of Patients",
                                                               20,
                                                               min = 2,
                                                               max = 2000),
                                                  
                                                  # Horizontal line
                                                  tags$hr(),
                                                  
                                                  # Input: Number of Variables
                                                  numericInput("nVariables","Number of Variables",
                                                               4,
                                                               min = 2,
                                                               max = 200),
                                                  
                                                  # Input: Select to add Hazard
                                                  column(8,
                                                         actionButton("addHazard","Add hazard to Data!",width = '140%')
                                                  ),
                                                  column(4,
                                                         actionButton("haszardOptions","",width = '100%',
                                                                      icon = icon("cog"))
                                                  ),
                                                  # Horizontal line
                                                  tags$hr(),
                                                  # Input: Finalize
                                                  actionButton("saveDataRNG","Accept!",width = '100%')
                                                )
                                       ), tabName = all.tabs$tabName[all.tabs$pos==2.5]
                                     )
                            ),
                            #----------------------------------------------- 2a. Define/Redefine ----
                            menuItem(all.tabs$label[all.tabs$pos==3], 
                                     tabName = all.tabs$tabName[all.tabs$pos==3], 
                                     icon = icon(all.tabs$icon[all.tabs$pos==3])),
                            #----------------------------------------------- 3a. Explore ----
                            menuItem(all.tabs$label[all.tabs$pos==4], 
                                     tabName = all.tabs$tabName[all.tabs$pos==4], 
                                     icon = icon(all.tabs$icon[all.tabs$pos==4])),
                            #----------------------------------------------- 4. Overview ----
                            menuItem(all.tabs$label[all.tabs$pos==5], 
                                     tabName = all.tabs$tabName[all.tabs$pos==5], 
                                     icon = icon(all.tabs$icon[all.tabs$pos==5])),
                            #----------------------------------------------- 5. Evaluation ----
                            menuItem(all.tabs$label[all.tabs$pos==6], 
                                     tabName = all.tabs$tabName[all.tabs$pos==6], 
                                     icon = icon(all.tabs$icon[all.tabs$pos==6]))
                )
              ),
              # ========================================== Body ====
              body = dashboardBody(
                extendShinyjs(text = jscode),
                # defines a new box status 'primary'
                tags$style(HTML("


.box.box-solid.box-primary>.box-header {
  color:#fff;
  background:#666666
                    }

.box.box-solid.box-primary{
border-bottom-color:#666666;
border-left-color:#666666;
border-right-color:#666666;
border-top-color:#666666;
}

                                    ")),
                # defines a new box status 'primary'
                tags$style(
                HTML("
                  .box.box-solid.box-abs>.box-header {
                    color:#fff;
                    background:#666666
                  }
                  .box.box-solid.box-primary{
                    border-bottom-color:#666666;
                    border-left-color:#666666;
                    border-right-color:#666666;
                    border-top-color:#666666;
                  }
                ")),
                tabItems(
                  ####################################### 0. Information ####
                  tabItem(tabName = "panelIdInformation",
                          fluidRow(
                            dropdownButton(
                              actionButton("a","a"),
                              actionButton("a","a"),
                              circle = F, icon = icon("gear"), #width = "300px",status = "danger", 
                              tooltip = tooltipOptions(title = "Click to see inputs !")
                            ),
                            actionButton("show", "Show modal dialog"),
                            actionButton("a123","a123"),
                            bsPopover(id = "a123",
                                      title = as.character(actionButton("ee1","ee1")),
                                      content = as.character(actionButton("ee","ee")),
                                      placement = "right", 
                                      trigger = "click", 
                                      options = list(container = "body")
                            ),
                            box(width = 12,status = "primary",solidHeader = T,
                                title = "Why Nice.Data app?",
                                h5("The Nice Data App will 'try to' help you confirming that your data can be used for 
                       further analysis. First you need to Check if your Data is in the right form (see picture).
                       Then you might upload your data (Upload data).
                       Secondly you will check the correctness of the given datatypes (Define/redifiny Datatypes).
                       Thirdly we will check the consistency of your data, wheather or not there are outliers, if 
                       min/max values of your data make sense or if there are any other inconsitencies. ")
                            ),
                            
                            box(width = 12,status = "info",
                                title = "headers and data-body",
                                fluidRow(
                                  column(6,
                                         h4("variablenames:"),
                                         h5("The first row of your data needs to contain the variablenames. These names should be
                                ", tags$b("unique and selfexplanatory")," (instead of Base1, Base2, Base3 use sex, age, bmi or instead
                                of age, age1 use age, age.kategory)."),
                                         h5("Moreover, for easy processing an implementing with R, the variablenames should ", tags$b("not contain"),"
                                empty spaces and characters other than letters, non leading numbers, '.' and '_'. Click
                                on the name.checker to check individual variable-names or upload your data and look at
                                your changed variable names."),
                                         h5("If you like ", tags$b("special characters")," (e.g. % ( [ $ ² ′′) in the variablenames for your graphs and tables you can generate a name table of 
                                the following form (click this button)."),
                                         h4("data body:"),
                                         h5("The data body should ", tags$b("only")," consist of actual ", tags$b("data")," If e.g. you already calculated any sums or means
                                or added any comments under yout data, remove them. ", tags$b("Missing data")," should not be substituted by
                                any placeholders, just leave them empty."),
                                         h5("On the right you will find an example dataframe ---->" )
                                  ),
                                  
                                  column(6,
                                         img(src='DataFrameIdeal.png', align = "left", width = 400, height = 250)
                                  )
                                )
                            )
                          ),
                          fluidRow(
                            box(width = 6,status = "info",
                                title = "variables",
                                h5("Each column of a dataset is called a variable."),
                                h5("Variables with multiple levels wich should be individually counted, need to be split 
                     into seperate variables (e.g. picture)."),
                                h5("The computer is case-sensitive, which means he differs between 'yes' and 'Yes'."),
                                h5("The computer does not know how to use '3-5' or '>200'
                   besides a mere category.")#text-align: justify
                            ),
                            box(width = 6,status = "info",
                                title = "data security:",
                                h5("Working with clinical data often includes personal data, which you are not allowed to share 
                     e.g. Name, adress, birthday, case-number. To remove these variables from the dataset without 
                     losing theire information, we can split the table into the personal part and the part, which 
                     can be shared with us. To link these Datasets, one can define an explicit id which conects 
                     the teo datasets."),
                                htmlOutput("link.Pseudonomisierung"),
                                h5("Check the box if you want to upload your data. (not yet working)"),
                                checkboxInput("input.NonSensitive","My data do not contain sensitive information.")
                            )
                          ),
                          
                          box(width = 12,status = "info",
                              title = "wide vs. long dataformat",
                              h5("Especially in studies with a longer observation time the question arises, weather the 
                   observed data should be collected side by side (wide) or underneath each other (long)."),
                              h5("These can easily be transformed into each other and have certain advantages. In special
                   cases, the used statistical software needs the data to be in a prespecified format."),
                              h5("The following Picture shows a wide and a long dataset, carrying the same information."),
                              h6("Note for specific transformations (Excel,R) (e.g. URL or extra page)."),
                              img(src='DataFramewideLong.png', align = "left", width = 800, height = 300)
                          )
                  ),
                  ####################################### 1. Get data ####
                  tabItem(tabName = "panelSubIdxlsx",
                          box(title = "Preview:",width = 12,
                              DT::dataTableOutput("contents.xlsx",width = "100%"),
                              tags$hr(),
                              uiOutput("preload.xlsx")
                          )
                  ),
                  tabItem(tabName = "panelSubIdcsv",
                          box(title = "Preview:",width = 12,
                              DT::dataTableOutput("contents.csv",width = "100%"),
                              tags$hr(),
                              uiOutput("preload.csv")
                          )
                  ),
                  tabItem(tabName = "john",
                          icon("blind")
                  ),
                  tabItem(tabName = "RDatabase",
                          box(title = "Preview:",width = 12,
                              DT::dataTableOutput("contents.RDat",width = "100%"),
                              tags$hr(),
                              uiOutput("preload.base")
                          )
                  ),
                  tabItem(tabName = "panelSubIdRNG",
                          box(title = "Preview:",width = 12,
                              DT::dataTableOutput("contents.RNG",width = "100%"),
                              tags$hr(),
                              uiOutput("preload.RNG")
                          )
                  ),
                  ####################################### 2. define/redefine ####
                  tabItem(tabName = "panelIdDefine2",
                          fluidPage(
                            fluidRow(
                              box(
                                collapsible = TRUE,
                                title = "Step 1!",
                                h5("In this Panel you need to define the datatypes expected in each varaible (column).
                      We distinguish between five different data formats and no format, which can be assigned by clicking 
                      on the button next to the name of the varable. The five datatypes are ..."),
                                width = 12
                              )
                            ),
                              fluidRow(
                                uiOutput("MainBody")
                              ),
                            #Valid colors are: 
                            # red, yellow, aqua, blue, light-blue, green, navy, 
                            # teal, olive, lime, orange, fuchsia, purple, maroon, black.
                            box(id="infoBox_numeric",
                                title = actionLink("infoBox_numeric_titleId", "Numerical",icon =icon("info"),
                                                   style ="color: #fff" ), 
                                background = "orange",#get.typeColor("integer"),
                                solidHeader = T,
                                width = 4, collapsible = T,collapsed = T,
                                footer = h5(get.title("numeric"),style = "color: #000")
                            ),
                            box(id="infoBox_integer",
                                title = actionLink("infoBox_integer_titleId", "Integer",icon =icon("info"),
                                                   style ="color: #fff" ), 
                                background = "yellow",#get.typeColor("integer"), 
                                width = 4, collapsible = T,collapsed = T,
                                footer = h5(get.title("integer"),style = "color: #000")
                            ),
                            box(id="infoBox_factor",
                                title = actionLink("infoBox_factor_titleId", "Categorical",icon =icon("info"),
                                                   style ="color: #fff" ), 
                                background = "blue",  
                                width = 4, collapsible = T,collapsed = T,
                                footer = h5(get.title("factor"),style = "color: #000")
                            ),
                            box(id="infoBox_ordered",
                                title = actionLink("infoBox_ordered_titleId", "Ordinal",icon =icon("info"),
                                                   style ="color: #fff" ), 
                                background = "light-blue",  
                                width = 4, collapsible = T,collapsed = T,
                                footer = h5(get.title("ordered.factor"),style = "color: #000")
                            ),
                            box(id="infoBox_Date",
                                title = actionLink("infoBox_Date_titleId", "Date",icon =icon("info"),
                                                   style ="color: #fff" ), 
                                background = "purple",  
                                width = 4, collapsible = T,collapsed = T,
                                footer = h5(get.title("Date"),style = "color: #000")
                            ),
                             # textOutput('myText')#,
                            
                            div(
                              column(
                                1,
                                offset=1
                              ),
                              column(
                                1,
                                offset=8,
                                actionButton("next.panelIdDefine2",NULL,
                                             icon = icon("angle-double-right"),
                                             onclick = 'Shiny.onInputChange(\"btn_nextTab\",  this.id)',
                                             style = btn.style.nextPrev))
                            )
                          )
                  ),
                  ####################################### 3  Explore ####
                  tabItem(tabName = "panelIdExplore2",
                          fluidRow(
                            box(collapsible = TRUE,width = 12,
                                title = "Step 2: Check, confirm and monitor your data.(Confirm and check for consistency.)",
                                h5("Check: With the help of this app you will easily see if your data match your predefined datatype.
                                   Confirm: Change your data if necessary, reload and repeat check again."),
                            ),
                            box(width = 8,
                                h4("Overview of your Data and weather or not they are correctly classified or in th monitor."),
                                DT::dataTableOutput("exploreVarNames")
                            ),
                            column(4,
                              box(width = 12,
                                  title = "Monitor:",
                                  htmlOutput("explore.sidePanel")
                              ), 
                              box(id="infoBox_monitor",
                                  title = actionLink("infoBox_monitor_titleId", "Monitor",icon =icon("info"),
                                                     style ="color: #fff" ), 
                                  background = "navy",#get.typeColor("integer"), 
                                  width = 12, collapsible = T,collapsed = T,
                                  footer = h5("The task of a monitor is to check the consistency of your Data. 
                                              This will help you detect common errors like a shifting comma or other typos. 
                                              Here we will only set up easy, but powerfull monitors e.g. 'body height [m] must 
                                              be greater than 0 and smaller than 3' or 'there can only be 3 different groups' 
                                              etc..",style = "color: #000")
                              ),
                              # fluidRow(
                              #   box(width = 12,
                              #       plotlyOutput("plot.tmp")
                              #   )
                              # )
                            )
                          ),
                          div(
                            column(
                              1,
                              offset=1,
                              actionButton("prev.panelIdExplore2",NULL,
                                           icon = icon("angle-double-left"),
                                           onclick = 'Shiny.onInputChange(\"btn_prevTab\",  this.id)',
                                           style = btn.style.nextPrev)),
                            
                            column(
                              1,
                              offset=8,
                              actionButton("next.panelIdExplore2",NULL,
                                           icon = icon("angle-double-right"),
                                           onclick = 'Shiny.onInputChange(\"btn_nextTab\",  this.id)',
                                           style = btn.style.nextPrev))
                          )
                          
                  ),
                  ####################################### 4. Overview ####
                  tabItem(tabName = "panelIdOverview",
                          fluidRow(
                            box(width = 12,
                                plotOutput("complete.Obs")
                            ),
                            box(width = 6,
                                verbatimTextOutput("Overview.out.left.verbatim",
                                                   placeholder =  FALSE)
                            ),
                            box(width = 6,
                                plotOutput("Overview.out.right.dist")
                            )
                          ),
                          div(
                            column(
                              1,
                              offset=1,
                              actionButton("prev.panelIdOverview",NULL,
                                           icon = icon("angle-double-left"),
                                           onclick = 'Shiny.onInputChange(\"btn_prevTab\",  this.id)',
                                           style = btn.style.nextPrev)),
                            
                            column(
                              1,
                              offset=8,
                              actionButton("next.panelIdOverview",NULL,
                                           icon = icon("angle-double-right"),
                                           onclick = 'Shiny.onInputChange(\"btn_nextTab\",  this.id)',
                                           style = btn.style.nextPrev))
                          )
                  ),
                  ####################################### 5. Evaluation ####
                  tabItem(
                    tabName = "panelIdEvaluation",
                    fluidRow(
                      h4(paste("Your data-set is scored with ",sample((40:100)/10,1)," out of 10 points!")),
                      h5("This is the calculation of your Score: ..."),
                      h5("This section will give a score for the quality of the Data."),
                      h5("-> Did the Person check all the Variables?"),
                      h5("-> Are there any wrong classified cells left?"),
                      h5("-> More ideas?"),
                      actionButton("saveRes","Save reactive values"),
                      # uiOutput("memoryUsage"),
                      
                      
                      tags$hr(),
                      uiOutput("sidePanel"),
                      mainPanel(
                        plotOutput("distPlot")
                      )
                    ),
                    div(
                      column(
                        1,
                        offset=1,
                        actionButton("prev.panelIdEvaluation",NULL,
                                     icon = icon("angle-double-left"),
                                     onclick = 'Shiny.onInputChange(\"btn_prevTab\",  this.id)',
                                     style = btn.style.nextPrev)),
                      column(
                        1,
                        offset=8)
                    )
                  )
                  ############################################ END Body ####
                )
                
              )
)