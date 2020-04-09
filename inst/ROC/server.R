library(shiny)
library(RColorBrewer)
library(rmarkdown)
library(gcookbook)
library(shinythemes)
library(webr)
library(rrtable)
# library(lubridate)
library(moonBook)
library(shinyBS)
# library(ggplot2)
# library(ztable)
# library(dplyr)
# library(ggiraph)
# library(ggiraphExtra)
# library(stringr)
library(DT)
library(rio)
library(editData)
library(flextable)
library(shinyFiles)
library(webrSub)
library(Epi)
library(party)
library(TH.data)
library(webrExtra)
library(caret)
library(e1071)

shinyServer(function(input,output,session){

    dataEx=c("acs", "radial","iris","dirty","band_members")

    data(GBSG2,package="TH.data")

    dirty<<-rio::import("dirty_data.xlsx")

    dataname=reactiveValues(dataname="",preprocessing="")

    dic<-webrSub:::dic

    savedPPT=reactiveValues(type=c(),title=c(),code=c())

    langchoice=function(en,kor){
        ifelse(input$language=="en",en,kor)
    }

    langchoice1=function(id){
        temp=dic[dic$id==id,]
        ifelse(input$language=="en",temp$en,temp$kor)
    }


    output$title=renderUI({

        tagList(
            h1(langchoice("Web-based Analysis with R 4.0","웹에서 하는 R 통계분석 4.0")),
            hr(),
            # if(input$main=="DataSelect")
            myp(langchoice("With this app, you can perform analysis `without` R in your computer. You can `analyze` data, make `tables` and `plots` and download the report as a `pdf`, `docx` or `pptx` file. You can also download the high-quality plots with desired size and resolution.","자신의 컴퓨터에 R을 설치할 필요 없이 R을 이용한 통계분석을 할 수 있읍니다. 그룹변수와 행변수를 선택하여 쉽게 표를 만들 수 있으며 그래프를 통한 자료 탐색과 여러가지 통계분석이 가능합니다. `자신의 데이타`를 xlsx 또는 csv형식으로 업로드하여 분석을 할 수 있을 뿐 아니라 그 결과를 `pdf`,`docx`,`powerpoint` 파일로 다운로드할 수 있읍니다. 또한 Plot을 원하는 크기로 저장할 수 있읍니다. `표가 보일 때까지 잠시만` 기다려주세요.")),
            # if(input$main=="DataSelect")
            hr()
        )
    })
    result=callModule(dataSelect,"data",
                      dataEx=reactive(dataEx),
                      lang=reactive(input$language),
                      dataname=reactive(dataname$dataname),
                      preprocessing0=reactive(dataname$preprocessing))

    # observeEvent(result(),{
    #      dataname$dataname<-result()$name
    # })
    df=reactive({
        df=NULL
        if(!is.null(result()$name)){
            if(result()$name!=""){
                temp=result()$name
                if(result()$preprocessing!=""){
                    eval(parse(text=result()$preprocessing))
                }
                df=eval(parse(text=temp))
            }}
        df
    })

    resROC=callModule(ROCModule,"ROC1",
                      dataname=reactive(result()$name),
                      df=reactive(df()),
                      preprocessing=reactive(result()$preprocessing),
                      PPTdata=reactive(PPTdata()),
                      lang=reactive(input$language))

    resTree=callModule(TreeModule,"Tree",
                      dataname=reactive(result()$name),
                      df=reactive(df()),
                      preprocessing=reactive(result()$preprocessing),
                      PPTdata=reactive(PPTdata()),
                      lang=reactive(input$language))

    ressTree=callModule(sTreeModule,"sTree",
                       dataname=reactive(result()$name),
                       df=reactive(df()),
                       preprocessing=reactive(result()$preprocessing),
                       PPTdata=reactive(PPTdata()),
                       lang=reactive(input$language))

    observeEvent(resROC(),{
        savedPPT$type=resROC()$df$type
        savedPPT$title=resROC()$df$title
        savedPPT$code=resROC()$df$code
        if(!is.null(resROC()$exData)){
            if(resROC()$exData!="None") dataname$dataname<-resROC()$exData
        }

    })

    observeEvent(resTree(),{
        savedPPT$type=resTree()$df$type
        savedPPT$title=resTree()$df$title
        savedPPT$code=resTree()$df$code
        if(!is.null(resTree()$exData)){
            if(resTree()$exData!="None") dataname$dataname<-resTree()$exData
        }

    })

    observeEvent(ressTree(),{
        savedPPT$type=ressTree()$df$type
        savedPPT$title=ressTree()$df$title
        savedPPT$code=ressTree()$df$code
        if(!is.null(ressTree()$exData)){
            if(ressTree()$exData!="None") dataname$dataname<-ressTree()$exData
        }

    })

    origin=reactive({df()})


    pptdf=reactive({
        if(length(savedPPT$code)==0) {
            result=""
        } else{

            result<-data.frame(type=savedPPT$type,title=savedPPT$title,code=savedPPT$code,
                               stringsAsFactors = FALSE)
        }
        result
    })

    PPTdata=reactive({
        data.frame(type=savedPPT$type,title=savedPPT$title,code=savedPPT$code,
                   stringsAsFactors = FALSE)
    })

    pptdf2=callModule(pptxList,"List1",data=reactive(pptdf()),
                      preprocessing=reactive(dataname$preprocessing))

    output$text=renderPrint({
        # cat("str(df())\n")
        # str(df())
        str(df())
        # if(!is.null(result()$name)){
        #     if(result()$name!=""){
        #         temp=result()$name
        #         # cat("temp=",temp,"\n")
        #         assign(temp,df())
        #         temp1=paste0("str(",temp,")")
        #         cat(temp1,"\n")
        #         eval(parse(text=temp1))
        #     }}
    })

    output$table3=renderTable({head(df(),10)})

    output$table4=renderPrint({
        result<-NULL
        try(result<-pptdf2())
        if(!is.null(result)){
            # cat("str(pptdf2())\n")
            # str(result)
            # cat("is.null(pptdf2()$code)\n")
            # is.null(result$code)
            savedPPT$type=result$type
            savedPPT$title=result$title
            savedPPT$code=result$code
        }

    })



})
