#' Calculate optimal lr.eta
#' @param x A list As a result of Epi::ROC()
#' @export
#' @examples
#' require(Epi)
#' x=ROC(form=am~wt,data=mtcars,plot="ROC")
#' optimal_lr.eta(x)
optimal_lr.eta=function(x){
  no=which.max(x$res$sens+x$res$spec)[1]
  result=x$res$lr.eta[no]
  result
}

#' Calculate optimal cutpoint
#' @param x A list As a result of Epi::ROC()
#' @importFrom stats fitted
#' @export
#' @examples
#' require(Epi)
#' x=ROC(form=am~wt,data=mtcars,plot="ROC")
#' optimal_cutpoint(x)
optimal_cutpoint=function(x){
  y=optimal_lr.eta(x)
  if(ncol(x$lr$model)==2){
    b0=unname(x$lr$coeff[1])
    b1=unname(x$lr$coeff[2])
    result=(-log(1/y-1)-b0)/b1

  } else{
    temp=fitted(x$lr)
    no=which(temp==y)[1]
    result=""
    for(i in 2:ncol(x$lr$model)){
      if(nchar(result)!=0) result=paste(result,",")
      result=paste(result,x$lr$model[no,i],sep="")
    }

  }
  result
}


#'Calculate x, y coordinate for ROC curve
#' @param x A list As a result of Epi::ROC()
#' @param no Integer
#' @return A data.frame
#' @export
#' @examples
#' require(Epi)
#' x=ROC(form=am~wt,data=mtcars,plot="ROC")
#' makeCoord(x)
makeCoord=function(x,no=1){
  df=data.frame(x=1-x$res$spec,y=x$res$sens)
  df=df[order(df$y),]
  df$label=no
  df
}

#' Make labels for ROC curve
#' @param x A list As a result of Epi::ROC()
#' @param no Integer
#' @return A data.frame
#' @importFrom stats wilcox.test
#' @importFrom pROC ci
#' @export
#' @examples
#' require(Epi)
#' x=ROC(form=am~wt,data=mtcars,plot="ROC")
#' makeLabels(x)
makeLabels=function(x,no=1){
  i=which.max(x$res$sens+x$res$spec)
  xx=1-x$res$spec[i]
  yy=x$res$sens[i]
  max=max(x$res$sens+x$res$spec)
  eta=paste0("lr.eta= ",round(optimal_lr.eta(x),3))
  sens=paste("Sens:",sprintf("%03.1f",x$res$sens[i]*100),"%\n",
             "Spec:",sprintf("%03.1f",x$res$spec[i]*100),"%\n",
             "PV+:",sprintf("%03.1f",x$res$pvp[i]*100),"%\n",
             "PV-:",sprintf("%03.1f",x$res$pvn[i]*100),"%\n",
             sep="")
  legend=paste("Model: ",colnames(x$lr$model)[1],"~",
               paste(colnames(x$lr$model)[-1],collapse="+"),sep="")
  cut=optimal_cutpoint(x)
  if(is.numeric(cut)) cut=round(cut,3)
  temp=round(x$AUC,3)
  if(ncol(x$lr$model)==2){
    ci=suppressMessages(ci(ROC2roc(x)))
    temp=paste(temp,"(",round(ci[1],3),"-",round(ci[3],3),")",sep="")
    result=wilcox.test(x$lr$model[,2],x$lr$model[,1])
    if(result$p.value<0.001) {
      temp=paste(temp,", p < 0.001")
    } else temp=paste(temp,", p ==",round(result$p.value,3))
  }
  labelAUC= paste(legend,"\n",
                  "Optimal Cutoff value: ",cut,"\n",
                  "AUC: ",temp )
  label=no
  ypos=no*0.11-0.05
  data.frame(x=xx,y=yy,max,eta,sens,label,labelAUC,ypos)
}



#' Draw ROC curve with variable names
#' @param yvar Name of dependent variable
#' @param xvars Names of independent variables
#' @param dataname Name of data
#' @param ... Arguments to be passed to plot_ROC
#' @export
#' @examples
#' require(moonBook)
#' plot_ROC2(yvar="male",xvars=c("weight","height","age"),dataname="radial")
plot_ROC2=function(yvar,xvars,dataname,...){
    x=lapply(xvars,function(x){
       formula=paste0(yvar,"~",x)
       eval(parse(text=paste0("ROC(form=",formula,",data=",dataname,",plot='')")))
    })
    plot_ROC(x,...)
}


#'Draw ROC curves
#'@param x A list of result(s) of Epi::ROC()
#'@param show.line logical
#'@param show.points logical
#'@param show.lr.eta logical
#'@param show.sens logical
#'@param show.AUC logical
#'@importFrom purrr map2_dfr
#'@importFrom ggplot2 ggplot aes_string geom_line geom_segment geom_text annotate labs
#'@importFrom ggplot2 theme_bw theme geom_point
#'@importFrom pROC roc.test
#'@export
#'@return A ggplot
#'@examples
#'require(moonBook)
#'require(Epi)
#'a=ROC(form=male~height,data=radial,plot="")
#'plot_ROC(a)
#'b=ROC(form=male~weight,data=radial,plot="")
#'plot_ROC(list(a,b))
#'c=ROC(form=male~weight+height,data=radial,plot="")
#'plot_ROC(list(a,b,c),show.lr.eta=FALSE,show.sens=FALSE)
plot_ROC=function(x,show.line=FALSE,show.points=TRUE,show.lr.eta=TRUE,show.sens=TRUE,
                  show.AUC=TRUE){
  # show.line=TRUE;show.points=TRUE;show.lr.eta=TRUE;show.sens=TRUE
  # show.AUC=TRUE
   if(length(x)==3){
       if(length(x[[3]])>=30){
           x=list(x)
       }
   }
  no=as.list(1:length(x))
df=map2_dfr(x,no,makeCoord)
df$label=factor(df$label)
df2=suppressWarnings(map2_dfr(x,no,makeLabels))
df2$label=factor(df2$label)

if(length(x)==1) {
   p=ggplot(df,aes_string(x="x",y="y"))+geom_line()
} else{
    p=ggplot(df,aes_string(x="x",y="y",group="label",color="label"))+geom_line()
}

if(show.line) p<-p+geom_segment(data=df2,aes_string(x="0",y="max-1",xend="2-max",yend="1"))
if(show.points) p<-p+geom_point(data=df2,pch=4,size=5)
if(show.lr.eta) p<-p+geom_text(data=df2,
                               aes_string(x="x-0.01",y="y+0.02",label="eta"),hjust=1)
if(show.sens) p<-p+geom_text(data=df2,
                             aes_string(x="x+0.01",y="y-0.09",label="sens"), hjust=0)
if(show.AUC)  p<-p+geom_text(data=df2,
                             aes_string(x="0.5",y="ypos",label="labelAUC"),hjust=0)

if(length(x)==2) {
  if((ncol(x[[1]]$lr$model)==2)&(ncol(x[[2]]$lr$model)==2)) {
    result=roc.test(ROC2roc(x[[1]]),ROC2roc(x[[2]]),plot=T)
    if(result$p.value <0.001) {
      temp="p < 0.001"
    } else temp=paste("p = ",round(result$p.value,3),sep="")

    p<-p+annotate(geom="text",0.5,3*0.11-0.05,
         label=paste("DeLong's test for two correlated ROC curves\n",
               "Z = ",round(result$statistic,3),", ",temp,sep=""),hjust=0)
  }
}

p<-p+labs(x="1-Specificity",y="Sensitivity")+theme_bw()+
  geom_segment(x=0,y=0,xend=1,yend=1,lty=2)+
  theme(legend.position="none")
p
}


#' Perform multiple logistic regression with stepwise
#' @param formula A formula for logistic regression
#' @param data A data.frame
#' @param plot logical If true, retrun a ggplot
#' @param trace if positive, information is printed during the running of step. Larger values may give more detailed information.
#' @param ... Further arguments to be passed to plot_ROC()
#' @importFrom stats glm terms na.omit step anova
#' @importFrom Epi ROC
#' @export
#' @return A ggplot or an object of class anova
#' @examples
#' require(moonBook)
#' step_ROC(male~weight+height+age,data=radial,plot=TRUE)
#' step_ROC(male~weight+height+age,data=radial,plot=FALSE)
step_ROC=function(formula,data,plot=TRUE,trace=0,...){
    call=paste(deparse(formula),", ","data= ",substitute(data),sep="")
    f=formula
    myt=terms(f,data=data)
    y=as.character(f[[2]])

    myvar=attr(myt,"term.labels")
    count=length(myvar)
    mydf=data[y]
    for(i in 1:count) {
      mydf=cbind(mydf,data[[myvar[i]]])
      colnames(mydf)[i+1]=myvar[i]
    }
    mydf=na.omit(mydf)
    #str(mydf)
    result=glm(formula,data=mydf)
    final=step(result,trace=trace)
    x=ROC(form=formula,data=mydf,plot="")
    #str(final$model)
    x2=ROC(form=final$formula,data=mydf,plot="")
    if(plot) {
      plot_ROC(list(x,x2),...)
    } else {
    result=anova(x$lr,x2$lr,test="Chisq")
    result
    }

}


#' Convert A list from ROC() to roc object
#' @param x A list As a result of Epi::ROC()
#' @importFrom pROC roc
ROC2roc=function(x){
  form=deparse(x$lr$formula)
  df=x$lr$data
  eval(parse(text=paste0("roc(formula=",form,",data=df,ci=T)")))
}

