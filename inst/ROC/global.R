library(shinythemes)
library(webr)
library(rrtable)
library(lubridate)
library(moonBook)
library(shinyBS)
library(ggplot2)
library(ztable)
library(dplyr)
# library(ggiraph)
# library(ggiraphExtra)
library(stringr)
library(DT)
library(rio)
library(editData)
library(flextable)
library(webrSub)
library(shinyFiles)


options(shiny.sanitize.errors = FALSE)


dic<-webrSub:::dic

# names=list.files("R")
# for(i in seq_along(names)){
#     if(str_detect(names[i],".R$")) source(paste0("R/",names[i]))
# }
#
# dic=rio::import("R/sysdata.rda")
#
# shiny::registerInputHandler("shinyjsexamples.chooser", function(data, ...) {
#     if (is.null(data))
#         NULL
#     else
#         list(left=as.character(data$left), right=as.character(data$right), selected=as.character(data$selected))
# }, force = TRUE)
