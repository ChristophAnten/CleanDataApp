#//########################\\#
#||    Functions in App    ||#
#\\########################//#

## creates a list analogue to the list of reactive values in the App
create.res <- function(n = 20){
  res <- list(data = data.frame(id = 1:n,
                                sex = sample(c("m","w"),n,replace = TRUE),
                                date = paste0(sample(1:31,n,replace = T),"/",sample(1:12,n,replace = T),"/",sample(1999:2018,n,replace = T)),
                                group = c(rep(0,ceiling(n/2)),rep(1,floor(n/2))),
                                value = rnorm(c(rep(0,ceiling(n/2)),rep(1,floor(n/2)))),
                                DISRUPT = sample(c("44.3","22,3",33.3,"55.5","11,8",NA),n,replace = TRUE),
                                stringsAsFactors = F))
  res$selected$col = 2
  res$selected$row = 1
  res$selected$value = res$data[1,2]
  res$selected$name = res$data %>% dplyr::select(res$selected$col) %>% names()
  res$selected$type = typeof(res$data[,res$selected$name])
  res$selected$class = get.class(res$data[,res$selected$name])
  return(res)  
}

## initializes a list object with to collect all information on the variables
## e.g. monitors classes etc.
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

## gives some information on a variable in a data frame
# name = character of the name of the Variable
# df = the corresponding data.frame
# return = list()
describe.variable <- function(name=NULL,df){
  out <- list()
  # if (is.null(name)){
  #   out$type <- NULL
  #   out$nUnique <- NULL
  #   return(out)
  # }
  #if (is.numeric())
  #print()
  out$type <- df %>% 
    summarise_(paste0("get.class(",name,")")) 
  out$col <- df %>% 
    summarise(paste0("get.typeColor(",out$type,")"))
  out$nUnique <- df %>% select_(name) %>% unique() %>% NROW()
  out$nObs <- df %>% select_(name) %>% drop_na() %>% NROW()
  if (out$type == "character")
    out$statistic <- df %>% select_(name) %>% table() %>% prop.table() %>% data.frame()
  if (out$type == "factor")
    out$statistic <- df %>% select_(name) %>% table() %>% prop.table() %>% data.frame()
  if (out$type == "integer")
    out$statistic =  df %>% summarise_(median = paste0("median(",name,",na.rm=TRUE)"),
                                       IQR = paste0("IQR(",name,",na.rm=TRUE)"),
                                       min = paste0("min(",name,",na.rm=TRUE)"),
                                       max = paste0("max(",name,",na.rm=TRUE)")) %>% t()
  if (out$type == "numeric")
    out$statistic =  df %>% summarise_(median = paste0("mean(",name,",na.rm=TRUE)"),
                                       IQR = paste0("sd(",name,",na.rm=TRUE)"),
                                       min = paste0("min(",name,",na.rm=TRUE)"),
                                       max = paste0("max(",name,",na.rm=TRUE)")) %>% t()
  if (out$type == "Date")
    out$statistic =  df %>% summarise_(median = paste0("mean(",name,",na.rm=TRUE)"),
                                       IQR = paste0("sd(",name,",na.rm=TRUE)"),
                                       min = paste0("min(",name,",na.rm=TRUE)"),
                                       max = paste0("max(",name,",na.rm=TRUE)")) %>% t()
  out$table <- df %>% select_(name) %>% table() %>% data.frame()
  return(out)
}

## a function to return prespecified colors per type
# type = character ("integer",etc.)
# return = vector() of character()
get.typeColor <- function(type){
  out <- ifelse(type == "integer",'#f39c12',
                ifelse(type == "character",'#3B993B',
                       ifelse(type == "numeric", '#ff851b',
                              ifelse(type == "factor", '#3c8dbc',# #999900
                                     ifelse(type == "ordered.factor", '#0091d5',
                                            ifelse(type == "double", '#FF9900',
                                                   ifelse(type == "Date",'#605ca8', 'grey')
                                            )
                                     )
                              )
                       )
                )
  )
  return(out)
}

## function that changes the class() output to length one
# vector: a vector of a data.frame
# returns = collapsed class()
get.class <- function(vector){
  return(paste0(class(vector),collapse="."))
}

## returns the types, calsses and prespecified colors of every 
## variable in a specific data.frame
# df = the corresponding data.frame
# return = data.frame("Variables","type","class","col")
get.typesDf <- function(df){
  types <- data.frame(Variables = names(df),
                      type = "none",stringsAsFactors = F,
                      class = "none",
                      col = "white")
  for ( i in 1:length(names(df))){
    types[i,2] <- typeof(df[,i])
    types[i,3] <- get.class(df[,i])
    types[i,4] <- get.typeColor(types[i,3])
  }
  return(types)
}

## returns a logical vector showing, which variable of a given 
## data frame has the stated type
# type = character ("integer",etc.)
# df = the corresponding data.frame
# return = vector() of boolean()
which.types <- function(type,df){
  # referring to class
  return(get.typesDf(df)$Variables[(get.typesDf(df)$class == type)])
}
which.newest.types <- function(type,df.variableTypes){
  # referring to class
  type.init <- df.variableTypes$class
  type.changedTo <- df.variableTypes$changedTo
  types <- ifelse(is.na(type.changedTo),type.init,type.changedTo)
  print(df.variableTypes$Variables[(types==type)])
  return(df.variableTypes$Variables[(types==type)])
}

##################################################################### generate RNG.Dataframe
create.RNG.String <- function(n){
  rng = runif(n)
  n.pos <- 5
  out <- rep("1:nObs",n)
  for (i in 1:n){
    if ( rng[i] < 1/n.pos)
      out[i] = paste0("sample(1:",max(2,round(rng[i]*1000)%%10),",nObs,replace = T)")
    else if ( rng[i] < 2/n.pos)
      out[i] <- paste0("rnorm(nObs,",round(rng[i]*1000,1)%%10,",",max(.1,round(rng[i]*100,1)%%10),")")
    else if ( rng[i] < 3/n.pos)
      out[i] <- paste0("sample(c('",paste0(LETTERS[sample(1:26,sample(1:26,1),replace = T)],collapse = "','"),"'),nObs,replace = T)")
    else if ( rng[i] < 4/n.pos)
      out[i] <- paste0("factor(sample(1:",max(2,round(rng[i]*1000)%%10),",nObs,replace = T))")
    else
      out[i] <- paste0("as.Date(sample(4750:44750,nObs,replace = T), origin = '1899-12-30')")
  }
  return(out)
}
create.RNG.Variable <- function(nObs,rngString){
  return(eval(parse(text = rngString)) )
}
add.RNG.Variable <- function(nVars,df,rngString){
  if (nVars < 1){
    return(df)
  }
  for(i in NCOL(df):(NCOL(df)+nVars)){
    var <- data.frame(create.RNG.Variable(NROW(df),rngString[i]))
    colnames(var) <- paste0("X",(NCOL(df)))
    df <- data.frame(cbind(df,var))
  }
  return(df)
}
rm.RNG.Variable <- function(nVars,df){
  if (nVars >= NCOL(df)){
    print(paste0("nVars[",nVars,"] >= NCOL(df)[",NCOL(df),"]"))
    return(df)
  }
  return(df[,-((NCOL(df)-nVars+1):NCOL(df))])
}
add.RNG.Obs <- function(nObs,df,rngStrings){
  b <- paste0("data.frame((NROW(df)+1):(NROW(df)+nObs),",paste0(rngStrings,collapse = ","),")")
  out <- eval(parse(text = b))
  colnames(out) <- colnames(df)
  return(rbind(df,out))
}
rm.RNG.Obs <- function(nObs,df){
  if (nObs >= NROW(df)){
    print(paste0("nObs[",nObs,"] >= NROW(df)[",NROW(df),"]"))
    return(df)
  }
  return(df[-((NROW(df)-nObs+1):NROW(df)),])
}
##################################################################### Overview

plot.complete <- function(df, df.variableTypes,  addingFill = -40){
  nrow = NROW(df )
  ncol = NCOL(df )
  a.df <- df %>% mutate(complete = complete.cases(.),
                        Row = 1:length(complete)) %>% 
    gather(Variables,Observation,-Row,-complete) %>% 
    merge(df.variableTypes %>% 
            mutate(newest = get.newestTypes(df.variableTypes)),by = "Variables",sort = F) %>% #added
    mutate(State = ifelse(complete,"complete.case",
                          ifelse(is.na(Observation),"missing.obs","complete.obs")),
           colors = ifelse(State == "missing.obs","missing.obs",newest),
           fills = ifelse(State != "complete.case",
                          ifelse( State == "missing.obs",
                                  "white",add2col(get.typeColor(newest),addingFill,addingFill,addingFill)),
                          get.typeColor(newest))) 
  return(
    a.df%>%
      ggplot(aes(x = Variables,y = Row)) + 
      geom_raster(fill = a.df$fills) +
      # scale_fill_manual(values=c("green","blue", "orange","#FFFFFF")) + 
      geom_hline(yintercept = seq(0,nrow,by=10)+.5 ,col = "black",lwd = .1) + 
      geom_vline(xintercept = seq(0,ncol)+.5 ) +
      theme_minimal() +
      # theme(axis.text.x = element_text(angle = 90,hjust = .5,vjust = .5)) +
      # theme(legend.title = element_blank(),
      #       legend.position = "top") +
      scale_x_discrete(limits=names(df),position = "top") +
      scale_y_reverse()
  )
}
add.Hazard <- function(df,p=.2){
  missing <- matrix(sample(c(TRUE,FALSE),NCOL(df)*NROW(df),prob = c(p,1-p),replace = TRUE),ncol = NCOL(df))
  df[missing] = NA
  return(df)
}
add2col <- function(col,r,g,b){
  out <- col2rgb(col) + c(r,g,b)
  out <- ifelse(out>255,255,
                ifelse(out<0,0,out))
  out <- rgb(out[1,],out[2,],out[3,],maxColorValue = 255)
  return(out)
}

##################################################################### On defining and redefining

# x <- c("a","",1,-3,1.4,pi,NA)
# is.nonNumeric(x)
# is.nonInteger(x)
# is.nonDate(x)
# is.missing(x)

is.missing <- function(x){
  return(is.na(x) | x=="")
}
is.nonNumeric <- function(x){
  # print(deparse(substitute(x)))
  return(is.na(suppressWarnings(as.numeric(as.character(x)))))
}
is.nonInteger <- function(x){
  out <- suppressWarnings(as.numeric(as.character(x))) %% 1 != 0
  ifelse(is.na(out),TRUE,out)
}
is.nonFactor <- function(x){
  # needs a monitor
  return(rep(FALSE,length(x)))
}
is.nonOrdered.factor <- function(x){
  # needs a monitor
  return(rep(FALSE,length(x)))
}
is.nonDate <- function(x){
  # needs a monitor
  out <- tryCatch(as.Date(x),error = function(e){
    tryCatch(as.Date(suppressWarnings(as.numeric(as.character(x))),origin = "1900-01-01"),error = function(e){
      NA
    })
  })
  return(is.na(out))
}
is.nonEmpty <- function(x){
  !ifelse(is.na(x),T,ifelse(x=="",T,F))
}
is.nonNoData <- function(x){
  return(rep(FALSE,length(x)))
}

is.non <- function(variable,variableName,type){
  # print(paste0("is.non: type=",type,"; variableName=",variableName))
  if (type == "numeric"){
    return(is.nonNumeric(variable))
  }
  if (type == "integer"){
    return(is.nonInteger(variable))
  }
  if (type == "factor"){
    return(is.nonFactor(variable))
  }
  if (type == "ordered.factor"){
    return(is.nonOrdered.factor(variable))
  }
  if (type == "Date"){
    return(is.nonDate(variable))
  }
  return(is.nonNoData(variable))
}
## function to change the inner colors for package:formattable
setTableStyle <- function(df,df.variableTypes){
  out <- list()
  for (i in colnames(df)){
    type = get.newestType(df.variableTypes,i)
    out[[i]] = eval(substitute(
      formatter(
        "span",
        style = x ~ style(display = "block",
                          padding = "0 4px",
                          `color` = "white",
                          `border-radius` = "4px",
                          `background-color` = ifelse(is.non(x,I,TYPE), "red", NA)
        )
      )),list(I=i,TYPE=type))
  }
  return(out)
}

## function to initialise df.variableTypes
init.variableTypes <- function(res){
  return(data.frame(get.typesDf(res$data),
                    set = 0,
                    hasMonitor = FALSE, # can be NA, TRUE and FALSE
                    changed = FALSE,
                    changedTo = NA,
                    nMissing = NA,
                    nInClass = NA,
                    nInMonitor = NA))
}
# 
# variableTypes.getType <- function(row){
#   (names(def.varNames.buttons)[res$variableTypes$set])[selectedRow]
#   res$variableTypes$set
#   names(def.varNames.buttons)
# }

init.monitor <- function(res){
  out <- list()
  for(i in colnames(res$data)){
    print("a")
    out[[i]] <- list("isSet" = 0)
  }
  return(out)
}



init.classified <- function(res){
  # 0 : not classified
  # 1 : missing
  # 10 : correct classified
  # - : wrong classified
  # 100 : in Monitor
  # - : not in monitor
  # there is a hirarchy where the lowest number has priority. 
  # so it doesnt matter if you are not in the monitor if your class does not match or your even missing.
  out <- matrix(0,ncol=NCOL(res$data),nrow=NROW(res$data))
  out[which(is.na(res$data))]  <- 1
  for (i in which(res$variableTypes$class=="character")){
    out[which(res$data[,i]==""),i]  <- 1
  }
  return(out)
}

init.varNames.kannWeg <- function(res){
  return(
    data.frame(
      Name = colnames(res$data),
      `.` = rep(as.character(icon("times-circle")),NCOL(res$data)),#rep('<i class="far fa-times-circle"></i>',NCOL(res$data)),
      numeric = shinyInput(actionButton,
                           NCOL(res$data),
                           'button_1_',
                           label = def.varNames.buttons$numeri$label,
                           style = get.varNames.buttons.style("numeric"),
                           onclick = 'Shiny.onInputChange(\"select_button\",  this.id +\"_\"+ Math.random())' ),
      character = shinyInput(actionButton,
                             NCOL(res$data),
                             'button_2_',
                             label = def.varNames.buttons$character$label,
                             style = get.varNames.buttons.style("character"),
                             onclick = 'Shiny.onInputChange(\"select_button\",  this.id +\"_\"+ Math.random())' ),
      Date = shinyInput(actionButton,
                        NCOL(res$data),
                        'button_3_',
                        label = def.varNames.buttons$Date$label,
                        style = get.varNames.buttons.style("Date"),
                        onclick = 'Shiny.onInputChange(\"select_button\",  this.id +\"_\"+ Math.random())' ),
      noData = shinyInput(actionButton,
                          NCOL(res$data),
                          'button_4_',
                          label = def.varNames.buttons$noData$label,
                          style = get.varNames.buttons.style("noData"),
                          onclick = 'Shiny.onInputChange(\"select_button\",  this.id +\"_\"+ Math.random())' ),
      Check = shinyInput(actionButton,
                         NCOL(res$data),
                         'button_5_',
                         label = def.varNames.buttons$Check$label,
                         style = get.varNames.buttons.style("Check"),
                         onclick = def.varNames.buttons$Check$onclick ),
      stringsAsFactors = FALSE,
      row.names = 1:NCOL(res$data)
    )
  )
}

init.varNames <- function(res){
  out <- data.frame(
    Name = colnames(res$data),
    `.` = rep(as.character(icon("times-circle")),NCOL(res$data)),
    stringsAsFactors = FALSE,
    row.names = 1:NCOL(res$data)#rep('<i class="far fa-times-circle"></i>',NCOL(res$data))
  )
  for (i in names(def.varNames.buttons)){
    out[i] = shinyInput(actionButton,
                        NCOL(res$data),
                        id = def.varNames.buttons[[i]]$id.part,
                        label = def.varNames.buttons[[i]]$label,
                        style = get.varNames.buttons.style(i),
                        onclick = def.varNames.buttons[[i]]$onclick)#,
                        #title=def.varNames.buttons[[i]]$title,
                        #"data-toggle"="tooltip")
  }
  return(out)
}

init.varNames.button <- function(name,id.part,label,color,onclick.id,title){
  list(
    name = name,
    id.part = id.part,
    label = label,
    onclick = paste0('Shiny.onInputChange(\"',onclick.id,'\",  this.id +\"_\"+ Math.random())'),
    bgc = color,
    bc = "black",
    bgc.inactive = "white",
    bc.inactive = color,# 1=active, 0=inactive,
    title = title
  )
}

get.title <- function(name){
  if (name == "numeric")
    return("Every number with a dot as the decimal seperator, e.g. 3.141 or 5.")
  if (name == "integer")
    return("Observed or measured hole numbers. Used for counts and discrete measurements, e.g. -3 or 10.")
  if (name == "factor")
    return("Nominal Data. Can be numbers or characters. Normally defining groups, e.g. 'm' and 'f'.")
  if (name == "ordered.factor")
    return("A factor with an underlying order e.g. an observed score.")
  if (name == "Date")
    return("A measured Date. Comes in many different formats.")
  if (name == "noData")
    return("No specific Data type! e.g. empty columns or comments.")
  return("No tooltip set yet!")
}

def.varNames.buttons <- list(numeric = init.varNames.button(name = "numeric",
                                                            id.part = "button_1_",
                                                            label = "numerical",
                                                            color = get.typeColor("numeric"),
                                                            onclick.id = "select_button",
                                                            title = get.title("numeric")),
                             integer = init.varNames.button(name = "integer",
                                                            id.part = "button_2_",
                                                            label = "integer",
                                                            color = get.typeColor("integer"),
                                                            onclick.id = "select_button",
                                                            title = get.title("integer")),
                             factor = init.varNames.button(name = "factor",
                                                           id.part = "button_3_",
                                                           label = "categorical",
                                                           color = get.typeColor("factor"),
                                                           onclick.id = "select_button",
                                                           title = get.title("factor")),
                             ordered.factor = init.varNames.button(name = "ordered.factor",
                                                                   id.part = "button_4_",
                                                                   label = "ordinal",
                                                                   color = get.typeColor("ordered.factor"),
                                                                   onclick.id = "select_button",
                                                                   title = get.title("ordered.factor")),
                             Date = init.varNames.button(name = "Date",
                                                         id.part = "button_5_",
                                                         label = "date",
                                                         color = get.typeColor("Date"),
                                                         onclick.id = "select_button",
                                                         title = get.title("Date")),
                             noData = init.varNames.button(name = "noData",
                                                           id.part = "button_6_",
                                                           label = "none",
                                                           color = get.typeColor("noData"),
                                                           onclick.id = "select_button",
                                                           title = get.title("noData")),
                             Check = init.varNames.button(name = "Check",
                                                          id.part = "button_7_",
                                                          label = icon("columns"),
                                                          color = "white",
                                                          onclick.id = "select_check",
                                                          title = get.title("Check")))

get.varNames.buttons.style <- function(name,status=1){
  if (status)
    return(paste0("color: #000; background-color: ",def.varNames.buttons[[name]]$bgc,"; border-color: ",def.varNames.buttons[[name]]$bc))
  return(paste0("color: #888; background-color: ",def.varNames.buttons[[name]]$bgc.inactive,"; border-color: ",def.varNames.buttons[[name]]$bc.inactive))
}

get.newestType <- function(df.variableTypes,variableName){
  # print(df.variableTypes)
  # print(variableName)
  x <- (df.variableTypes$Variables == variableName)
  # print(x)
  if (is.na(df.variableTypes$changedTo[x]))
    return(df.variableTypes$class[x])
  return(df.variableTypes$changedTo[x])
}
get.newestTypes <- function(df.variableTypes){
  type.init <- df.variableTypes$class
  type.changedTo <- df.variableTypes$changedTo
  return(ifelse(is.na(type.changedTo),type.init,type.changedTo))
}

setToType <- function(set){
  ifelse(
    set==1,"numeric",
    ifelse(
      set==2,"integer",
      ifelse(
        set==3,"categorical",
        ifelse(
          set==4,"ordinal",
          ifelse(
            set==5,"Date",
            ifelse(
              set==6,"none",NA)
          )
        )  
      )  
    )
  )
}

# update.classified <- function(variableName,df.classified){
#   df.classified[,variableName] <- is.non()
# }

get.changedVars <- function(df.variableTypes){
  return(!is.na(df.variableTypes$changedTo))
}
get.nWrongClass <- function(){
  return(0)
}

get.memory.allocation <- function(env = .GlobalEnv,detail = T){
  if (detail)
    return(
      data.frame(
        object = ls(env),
        size = unlist(lapply(ls(env), function(x) {
          object.size(get(x, envir = env, inherits = FALSE))
        }))
      )
    )
  return(
    data.frame(
      memory = "total",
      size = sum(unlist(lapply(ls(env), function(x) {
        object.size(get(x, envir = env, inherits = FALSE))
      })))
    )
  )
}

### buttons in table
shinyInput <- function(FUN, len, id, ...) {
  inputs <- character(len)
  for (i in seq_len(len)) {
    inputs[i] <- as.character(FUN(paste0(id, i), ...))
  }
  #print(inputs)
  inputs
}

toggleSelected <- function(res.varNames,col,row,nCol,origin = 2){
  # print("row")
  # print(row) 
  # print("origin+col")
  # print(origin+col)
  # print("res$varNames")
  # print(res$varNames)
  res.varNames[row,origin+col] <- as.character(actionButton(
    paste0('button_',col,'_',row),
    label =  def.varNames.buttons[[col]]$label,
    style = get.varNames.buttons.style(col,1),
    onclick = 'Shiny.onInputChange(\"select_button\",  this.id +\"_\"+ Math.random())' ))
  for (i in (1:nCol)[-col]){
    res.varNames[row,origin+i] <- as.character(actionButton(
      paste0('button_',i,'_',row),
      label =  def.varNames.buttons[[i]]$label,
      style = get.varNames.buttons.style(i,0),
      onclick = 'Shiny.onInputChange(\"select_button\",  this.id +\"_\"+ Math.random())' ))
  }
  return(res.varNames)
}
unToggle <- function(res.varNames,row,nCol,origin = 2){
  for (i in (1:nCol)){
    res.varNames[row,origin+i] <- as.character(actionButton(
      paste0('button_',i,'_',row),
      label =  def.varNames.buttons[[i]]$label,
      style = get.varNames.buttons.style(i),
      onclick = 'Shiny.onInputChange(\"select_button\",  this.id +\"_\"+ Math.random())' ))
  }
  return(res.varNames)
  #print( res$varNames)
}

prgoressBar <- function(value = 0, label = FALSE, color = "aqua", size = NULL,
                        striped = FALSE, active = FALSE, vertical = FALSE) {
  stopifnot(is.numeric(value))
  if (value < 0 || value > 100)
    stop("'value' should be in the range from 0 to 100.", call. = FALSE)
  if (!(color %in% shinydashboard:::validColors || color %in% shinydashboard:::validStatuses))
    stop("'color' should be a valid status or color.", call. = FALSE)
  if (!is.null(size))
    size <- match.arg(size, c("sm", "xs", "xxs"))
  text_value <- paste0(value, "%")
  if (vertical)
    style <- htmltools::css(height = text_value, `min-height` = "2em")
  else
    style <- htmltools::css(width = text_value, `min-width` = "2em")
  tags$div(
    class = "progress",
    class = if (!is.null(size)) paste0("progress-", size),
    class = if (vertical) "vertical",
    class = if (active) "active",
    tags$div(
      class = "progress-bar",
      class = paste0("progress-bar-", color),
      class = if (striped) "progress-bar-striped",
      style = style,
      role = "progressbar",
      `aria-valuenow` = value,
      `aria-valuemin` = 0,
      `aria-valuemax` = 100,
      tags$span(class = if (!label) "sr-only", text_value)
    )
  )
}
