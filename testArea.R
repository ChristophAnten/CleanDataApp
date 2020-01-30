#//###################################\\#
#||    Test-Area for functions etc    ||#
#\\###################################//#

# Alt + o

## load functions in App
source("functionsInApp.R")

## define reactive values res
res <- create.res(20)
res$data %>% names()
res$variables <- init.varList(res)
res$variableTypes <- init.variableTypes(res)
res$classified <- init.classified(res)
res$varNames <- init.varNames(res)
# plot.complete(res$data)

init.varList <- function(res){
  out <- list()
  for (i in colnames(res$data)){
    out[[i]] <- list("name",
                               "unit",
                               "class",
                               "monitor")
  }
  return(out)
}

load("res.RData")
str(res)
#### plot colorized by type plus missing



nrow = NROW(res$data )
ncol = NCOL(res$data )
addingFill = -40
a.df <- res$data %>% mutate(complete = complete.cases(.),
                    Row = 1:length(complete)) %>% 
  gather(Variables,Observation,-Row,-complete) %>% 
  merge(get.typesDf(res$data),by = "Variables",sort = F) %>% #added
  mutate(State = ifelse(complete,"complete.case",
                        ifelse(is.na(Observation),"missing.obs","complete.obs")),
         colors = ifelse(State == "missing.obs","missing.obs",type),
         fills = ifelse(State != "complete.case",
                        ifelse( State == "missing.obs",
                                "white",add2col(col,addingFill,addingFill,addingFill)),
                                  col)) 
a.df%>%
  ggplot(aes(x = Variables,y = Row)) + 
  geom_raster(fill = a.df$fills) +
  #scale_fill_manual(values=c("green","blue", "orange","#FFFFFF")) + 
  geom_hline(yintercept = seq(0,nrow,by=10)+.5 ,col = "black",lwd = .1) + 
  geom_vline(xintercept = seq(0,ncol)+.5 ) +
  theme(legend.title = element_blank(),
        legend.position = "top") +
  scale_x_discrete(limits=names(res$data),position = "top") +
  scale_y_reverse()
# plot.complete
legend.df <- data.frame(a=1:4,b=1:4,col=c("#111111","#333333","#555555","#999999"),
                        names = c("a","b","c","c"),stringsAsFactors = F)

ggplot(legend.df,aes(x=a,y=b,col=names)) + geom_tile(fill="white") #+
  scale_fill_manual(values = legend.df$col)

ggplot(legend.df,aes(x=a,y=b,fill=names)) + geom_tile(col="black",fill=legend.df$col) +
  scale_fill_manual(values = legend.df$col)
values=c(myline1="red", myline2="blue", myline3="purple")
describe.variable("DISRUPT",res$data)

get.typesDf(res$data)
res$data %>% 
  get.typesDf() %>%
  ggplot(aes(x="class",fill=class)) + geom_bar(stat = "count") +
  scale_fill_manual(values = unique(get.typesDf(res$data)$col),
                    breaks = unique(get.typesDf(res$data)$class)) +
  coord_polar("y", start=0) + theme_void()+
  theme(axis.text.x=element_blank())


get.typesDf(res$data)
which.types("integer",res$data)
transform.nonNumeric("DISRUPT",res$data)
get.nonNumeric("DISRUPT",res$data)
is.nonNumeric("DISRUPT",res$data)

proposeVec <- function(inputVec,propType,df){
  # inputVec = name of input vector
  # propType = proposed type of inputVec
  # df = input data.frame
  # out = proposed Vector eg c('3.4','3,4') -> c(3.4,3.4)
  if (propType == "numeric"){
    tmp <- transform.nonNumeric(inputVec)
  }
}

plot.complete <- function(df ){
  nrow = NROW(df )
  ncol = NCOL(df )
  return(
    df %>% mutate(complete = complete.cases(.),
                  Row = 1:length(complete)) %>%
      gather(Variable,Observation,-Row,-complete) %>% 
      mutate(State = ifelse(complete,"complete.case",
                              ifelse(is.na(Observation),"missing.obs","complete.obs"))) %>%
      ggplot(aes(x = Variable,y = Row, fill = State)) + 
      geom_raster() +
      scale_fill_manual(values=c("#33dd33","#44bb44", "#cc3333")) + 
      geom_hline(yintercept = seq(0,nrow,by=10)+.5 ,col = "black",lwd = .1) + 
      geom_vline(xintercept = seq(0,ncol)+.5 ) +
      theme(legend.title = element_blank(),
            legend.position = "top")
  )
}
plot.complete(res$data)
add.Hazard <- function(df,p=.2){
  missing <- matrix(sample(c(TRUE,FALSE),NCOL(df)*NROW(df),prob = c(p,1-p),replace = TRUE),ncol = NCOL(df))
  df[missing] = NA
  return(df)
}

#### on data.types
res$data <- data.frame(read_xlsx("exampleInput.xlsx",sheet = 2))
#res$data <- read.csv("exampleInput.csv")
get.typesDf(res$data)
plot.complete(res$data)
describe.variable("group",res$data)

# name = "group"
# df %>% summarise_(paste0("class(",name,")"))
# name = quo("group")
# df %>% summarise(!!quo(paste0("class(",!!name,")")))


## types sollen 4 sein: Zahlen stetig/diskret,zeichen,datum 


is.nonNumeric <- function(df,name=NULL,na.rm = FALSE){
  if (is.null(name)){
    return(
      df %>% mutate_all(~is.na(suppressWarnings(as.numeric(.)))))
  }
  return(is.na(suppressWarnings(as.numeric(df[[name]]))))
}
is.nonNumeric <- function(x){
  return(is.na(suppressWarnings(as.numeric(as.character(x)))))
  }
get.nonNumeric <- function(name,df,na.rm = FALSE){
  return(df %>% select(name) %>% filter(is.nonNumeric(name,df,na.rm = FALSE)))
}
is.nonDate <- function(df,name=NULL,na.rm = FALSE){
  if (is.null(name)){
    return(
      df %>% mutate_all(~is.na(suppressWarnings(as.numeric(.)))))
  }
  return(is.na(suppressWarnings(as.numeric(df[[name]]))))
}
get.nonDate <- function(name,df,na.rm = FALSE){
  return(df %>% select(name) %>% filter(is.nonNumeric(name,df,na.rm = FALSE)))
}
transform.nonNumeric <- function(name,df,na.rm = FALSE){
  # get ,. differences
  return(NA)
}
df.numeric <- data.frame(a = letters[1:5],b=factor(1:5),c=as.character(1:5),d=c(1,"a",NA,2.4,"2,1"),
                 stringsAsFactors = F)
df.date <-  read_xlsx("exampleInput.xlsx",sheet = 2)
is.nonNumeric(df.numeric)



is.non <- function(variable,variableName,variableTypes){
  out <- c()
  # print(deparse(substitute(variable)))
  # print(variableName)
  type = variableTypes$changedTo[(variableTypes$Variables == variableName)]
  if (is.na(type))
    type = variableTypes$type[(variableTypes$Variables == variableName)]
  # print(type)
  if (type == "numeric"){
    out <- is.nonNumeric(variable)
  }
  if (type == "Date"){
    out <- is.nonDate(variable)
  }
  if (type == "character"){
    out <- F
  }
  # print(out)
  return(out)
}
is.non(res$data$date,"date",res$variableTypes)
setTableStyle <- function(df){
  out <-list()
  for (i in colnames(df)){
    # eval(
      substitute(
        {out[[I]] = formatter(
          "span",
          style = x~style(display = "block",
                          padding = "0 4px",
                          `color` = "white",
                          `border-radius` = "4px",
                          `background-color` = ifelse(is.non(x,I,res$variableTypes), "red", "green")
          )
          
        )},list(I =i)
      )
    # )
  }
  
  return(out)
}
a <- setTableStyle(res$data)
fg <- formattable(
  res$data,
    a
  )
res$variableTypes <- cbind(get.typesDf(res$data),changedTo = rep(NA,NCOL(res$data)))
as.datatable(fg)
is.nonNumeric(df$value)

df <- read.csv("exampleInput.csv")
df <- read.csv("exampleInput.csv")
a <- df$value
as.numeric(as.character(a))
library(shinyjs)
{
ui <- shinyUI(
  fluidPage(useShinyjs(),
    actionButton("button_1_1","1",onclick = 'Shiny.onInputChange(\"select_button\",  Math.random())' ),
    actionButton("button_2_1","2",onclick = 'Shiny.onInputChange(\"select_button\",  this.id +\"_\"+ Math.random())' ),
    actionButton("button_3_1","3",onclick = 'Shiny.onInputChange(\"select_button\",  this.id)' ),
    verbatimTextOutput("t1")
  )
)

server <- shinyServer(function(input, output) {
  output$t1 <- renderText({
    input$b1
  })
  
  observeEvent(input$select_button,{
    print(input$select_button)
    # selectedRow <- as.numeric(strsplit(input$select_button, "_")[[1]][3])
    # selectedCol <- as.numeric(strsplit(input$select_button, "_")[[1]][2])
    # #input$select_button <-NULL
    # toggleMultiple(selectedCol,selectedRow,3)
    })
})

shinyApp(ui,server)
}

ui <- fluidPage(
  tags$h1("Pretty radio buttons"),
  br(),
  
  fluidRow(
    column(
      width = 4,
      prettyRadioButtons(inputId = "radio1",
                         label = "Click me!",
                         choices = c("Click me !", "Me !", "Or me !")),
      verbatimTextOutput(outputId = "res1"),
      br(),
      prettyRadioButtons(inputId = "radio4",  label = "Click me!",
                         choices = c("Click me !", "Me !", "Or me !"),
                         outline = TRUE,
                         plain = TRUE, icon = icon("thumbs-up")),
      verbatimTextOutput(outputId = "res4")
    ),
    column(
      width = 4,
      prettyRadioButtons(inputId = "radio2",
                         label = "Click me!", thick = TRUE,
                         choices = c("Click me !", "Me !", "Or me !"),
                         animation = "pulse", status = "info"),
      verbatimTextOutput(outputId = "res2"),
      br(),
      prettyRadioButtons(inputId = "radio5",
                         label = "Click me!", icon = icon("check"),
                         choices = c("Click me !", "Me !", "Or me !"),
                         animation = "tada", status = "default"),
      verbatimTextOutput(outputId = "res5")
    ),
    column(
      width = 4,
      prettyRadioButtons(inputId = "radio3",  label = "Click me!",
                         choices = c("Click me !", "Me !", "Or me !"),
                         shape = "round", status = "danger",
                         fill = TRUE, inline = TRUE),
      verbatimTextOutput(outputId = "res3")
    )
  )
  
)

server <- function(input, output, session) {
  
  output$res1 <- renderPrint(input$radio1)
  output$res2 <- renderPrint(input$radio2)
  output$res3 <- renderPrint(input$radio3)
  output$res4 <- renderPrint(input$radio4)
  output$res5 <- renderPrint(input$radio5)
  
}

shinyApp(ui, server)
