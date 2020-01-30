
library(shiny)

#setwd("D:/Christoph/Desktop/Test")

# Define UI for application that plots random distributions 
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Hello Shiny!"),
  
  # Sidebar with a slider input for number of observations
  tabsetPanel(
    tabPanel("Tab1",
             
             sidebarPanel(
               tabsetPanel(
                 tabPanel("Upload",
                          # Input: Select a file ----
                          fileInput("file.csv", "Choose CSV File",
                                    multiple = FALSE,
                                    accept = c("text/csv",
                                               "text/comma-separated-values,text/plain",
                                               ".csv")),
                          
                          # Horizontal line ----
                          tags$hr(),
                          
                          # Input: Checkbox if file has header ----
                          checkboxInput("header", "Header", TRUE),
                          
                          # Input: Select separator ----
                          radioButtons("sep", "Separator",
                                       choices = c(Comma = ",",
                                                   Semicolon = ";",
                                                   Tab = "\t"),
                                       selected = ","),
                          
                          # Input: Select quotes ----
                          radioButtons("quote", "Quote",
                                       choices = c(None = "",
                                                   "Double Quote" = '"',
                                                   "Single Quote" = "'"),
                                       selected = '"'),
                          
                          # Horizontal line ----
                          tags$hr(),
                          
                          # Input: Select number of rows to display ----
                          radioButtons("disp", "Display",
                                       choices = c(Head = "head",
                                                   All = "all"),
                                       selected = "head")
                 ),
                 tabPanel("Work",
                          uiOutput("sidePanel")
                 ),
                 tabPanel("Information",
                          h5("This part becomes the 'How to' part.  And gives information on the pre upload data condition.")
                 )
               )
             ),
             
             # Show a plot of the generated distribution
             mainPanel(
               plotOutput("distPlot"),
               tableOutput("contents")
             )
    )
  )
))
