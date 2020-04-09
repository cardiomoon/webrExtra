#'Make Powerpoint List for tree analysis
#'@param fitstr string A string or R code
#'@return A data.frame
#'@importFrom party ctree
#'@importFrom caret confusionMatrix
#'@export
#'@examples
#'require(party)
#'result=makePPTList_tree("ctree(Species~.,data=iris)")
makePPTList_tree=function(fitstr){

     title<-type<-code<-c()

     title="Decision Tree"
     type="rcode"
     code=fitstr

     title=c(title,"Decision Tree(1)")
     type=c(type,"plot")
     code=c(code,paste0("plot(",fitstr,")"))

     title=c(title,"Decision Tree(2)")
     type=c(type,"plot")
     code=c(code,paste0("plot(",fitstr,",inner_panel=node_barplot,edge_panel=function(...) invisible(),tnex=1)"))


     title=c(title,"Confusion Matrix")
     type=c(type,"rcode")
     code=c(code,paste0("fit<-",fitstr,";confusionMatrix(Predict(fit),fit@responses@variables[[1]])"))

     title=c(title,"Statistics by class")
     type=c(type,"rcode")
     code=c(code,paste0("fit<-",fitstr,";t(confusionMatrix(Predict(fit),fit@responses@variables[[1]])$byClass)"))

     data.frame(title=title,type=type,code=code,stringsAsFactors = FALSE)

}


#'Make Powerpoint List for survival tree analysis
#'@param fitstr string A string or R code
#'@return A data.frame
#'@importFrom party ctree
#'@importFrom survival Surv
#'@export
#'@examples
#'require(party)
#'data(GBSG2,package="TH.data")
#'result=makePPTList_surTree("ctree(Surv(time,cens)~.,data=GBSG2)")
makePPTList_surTree=function(fitstr){

     temp=unlist(strsplit(fitstr,"data="))[2]
     temp=gsub(" ","",temp)
     temp=gsub("\\)","",temp)
     dataname=temp
     title<-type<-code<-c()

     title="Survival Tree"
     type="rcode"
     code=fitstr

     title=c(title,"Survival Tree(1)")
     type=c(type,"plot")
     code=c(code,paste0("plot(",fitstr,")"))

     title=c(title,"Survival Tree(2)")
     type=c(type,"plot")
     code=c(code,paste0("plot(",fitstr,",inner_panel=node_barplot,edge_panel=function(...) invisible(),tnex=1)"))


     title=c(title,"Raw Data of the first 10 patients")
     type=c(type,"rcode")
     code=c(code,paste0("head(",dataname,",10)"))

     title=c(title,"Prediction of median survival for the first 10 patients")
     type=c(type,"rcode")
     code=c(code,paste0("treeresponse(",fitstr,",newdata=",dataname,"[1:10,])"))


     title=c(title,"Prediction of median survival for the first 10 patients")
     type=c(type,"rcode")
     code=c(code,paste0("x<-",fitstr,";head(data.frame(predicted=Predict(x),observed=x@responses@variables[[1]]),10)"))

     data.frame(title=title,type=type,code=code,stringsAsFactors = FALSE)

}




