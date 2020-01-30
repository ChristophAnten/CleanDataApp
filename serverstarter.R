require(shiny)
folder_address =  "/home/canten/Dropbox/Data App"
#folder_address = "C:/Users/Christoph/Dropbox/Data App"

x <- system("ipconfig", intern=TRUE)
z <- x[grep("IPv4", x)]
ip <- gsub(".*? ([[:digit:]])", "\\1", z)

#ip <- "134.76.140.14"
print(paste0("the Shiny Web application runs on: http://", ip, ":1234/"))

runApp(folder_address, launch.browser=FALSE, port = 1234, host = ip)

### work
folder_address =  "/home/canten/Dropbox/Data App"
ip <- "134.76.140.14"

source("functionsInApp.R")
runApp(folder_address, launch.browser=FALSE, port = 1234, host = ip)
