#' UI function of ROC module
#' @param id id
#' @importFrom shiny NS uiOutput
#' @export
ROCModuleInput=function(id){
     ns <-NS(id)

     uiOutput(ns("ROCModule"))
}


#' Server function of ROC shiny module
#'
#' @param input input
#' @param output output
#' @param session session
#' @param dataname A data name
#' @param df A data.frame
#' @param preprocessing A character string of R code
#' @param PPTdata PPTdata
#' @param lang The language to be displayed
#' @importFrom editData checkboxInput3
#' @importFrom shiny selectInput checkboxInput wellPanel conditionalPanel htmlOutput reactiveValues
#' @importFrom shiny actionButton callModule column downloadButton downloadHandler fluidRow hr icon insertUI
#' @importFrom shiny observe observeEvent reactive removeUI renderPrint renderUI tagList updateCheckboxInput
#' @importFrom DT DTOutput datatable renderDT
#' @importFrom webrSub multiChooserUI multiChooser help_console showData showDataUI myp
#' @importFrom rrtable data2HTML data2pptx
#' @export
ROCModule=function(input,output,session,dataname,df,preprocessing,PPTdata,lang){

     ns <- session$ns

     langchoice=function(en,kor){
          ifelse(lang()=="en",en,kor)
     }

     dic<-webrSub:::dic

     langchoice1=function(id){
          temp=dic[dic$id==id,]
          ifelse(lang()=="en",temp$en,temp$kor)
     }

     savedPPT=reactiveValues(type=c(),title=c(),code=c())

     observeEvent(PPTdata(),{
          savedPPT$type<-PPTdata()$type
          savedPPT$title<-PPTdata()$title
          savedPPT$code<-PPTdata()$code
     })

     data=reactive({ df() })


     resultdf=reactive({
          list(
               df=data.frame(type=savedPPT$type,title=savedPPT$title,code=savedPPT$code,
                             stringsAsFactors = FALSE),
               exData=input$ROCEx
          )
     })




output$ROCModule=renderUI({
     tagList(
          myp(langchoice1(180)),
          hr(),
          fluidRow(
               column(7,
                      myp(langchoice1(127)),
                      multiChooserUI(ns("ROCChooser"))
               ),
               column(5,
                      myp(langchoice1(128)),
                      wellPanel(
                           checkboxInput3(ns("ROCmultiple"),"Models with multiple predictors",value=FALSE,width=250),
                           checkboxInput3(ns("ROCseparate"),"draw separate ROC",value=TRUE,width=150),
                           checkboxInput3(ns("ROCvanilla"),"vanilla table",value=TRUE),
                           checkboxInput3(ns("ROCshow.line"),"show.line",value=FALSE),
                           checkboxInput3(ns("ROCshow.points"),"show.points",value=TRUE),
                           checkboxInput3(ns("ROCshow.lr.eta"),"show.lr.eta",value=TRUE),
                           checkboxInput3(ns("ROCshow.sens"),"show.sens",value=TRUE),
                           checkboxInput3(ns("ROCshow.AUC"),"show.AUC",value=TRUE)),
                      selectInput(ns("ROCEx"),langchoice1(135),choices=c("None","radial")),
                      conditionalPanel(sprintf("input[['%s']]!='None'",ns("ROCEx")),
                                       checkboxInput(ns("showHelpROC"),"show help for data",value=FALSE)
                      ),
                      checkboxInput(ns("showROCData"),"show Data Table",value=FALSE)
               )
          ),
          htmlOutput(ns("helpDataROC")),
          DTOutput(ns("ROCData")),
          conditionalPanel("true==false",
                           checkboxInput(ns("ROCOK"),"ROCOK",value=FALSE)),
          uiOutput(ns("ROCUI")),
          conditionalPanel(sprintf("input[['%s']]==true",ns("ROCOK")),
                           fluidRow(
                                column(3,actionButton(ns('doROC')," ROC Analysis ")),
                                column(3,downloadButton(ns("ROCReport"),"download Report")),
                                column(3,downloadButton(ns("ROCPPTx"),"download PPTx")),
                                column(3,actionButton(ns("addROCList"),"add to PPT List",icon=icon("shopping-cart")))
                           )
          )

     )
})

ROClabels=reactive({c(langchoice1(150),
                      langchoice1(151))})

ROCmultiples=c(FALSE,TRUE)
ROCbivars=c(TRUE,FALSE)


ROCResult=callModule(multiChooser,"ROCChooser",labels=ROClabels,
                     df=reactive(data()),multiples=ROCmultiples,bivars=ROCbivars,
                     lang=reactive(lang()))

output$helpDataROC=renderPrint({
     if(input$showHelpROC){
          help_console(dataname(),"html")
     }


})

output$ROCData=renderDT({

     if(input$showROCData){
          datatable(eval(parse(text=dataname())))
     }

})

observe({
     res<-ROCResult()

     temp=""
     mode=0
     if(length(res)>0){
          if(res[[1]]=="") {
               temp=""
               mode=0
          } else if(res[[2]][1]=="") {
               temp=""
               mode=0
          } else {
               mode=1
          }
          updateCheckboxInput(session,"ROCOK",value=ifelse(mode==1,TRUE,FALSE))


     }


})


makeROCPPT=reactive({

     input$doROC
     input$ROCmultiple

     res<-ROCResult()

     result=makePPTxList_ROC(yvar=res[[1]],xvars=res[[2]],dataname=dataname(),
                             multiple=input$ROCmultiple,separate=input$ROCseparate,
                             vanilla=input$ROCvanilla,show.line=input$ROCshow.line,
                             show.points=input$ROCshow.points,
                             show.lr.eta=input$ROCshow.lr.eta,
                             show.sens=input$ROCshow.sens,
                             show.AUC=input$ROCshow.AUC)
     result



})

observeEvent(input$addROCList,{

     result=makeROCPPT()


     savedPPT$type=c(savedPPT$type,result$type)
     savedPPT$title=c(savedPPT$title,result$title)
     savedPPT$code=c(savedPPT$code,result$code)

})


output$ROCReport = downloadHandler(

     filename="ROC.HTML",
     content=function(file){

          owd <- setwd(tempdir())
          on.exit(setwd(owd))

          result=makeROCPPT()

          data2HTML(result)
          file.rename("report.HTML", file)

     }
)

output$ROCPPTx = downloadHandler(

     filename="ROC.pptx",
     content=function(file){

          owd <- setwd(tempdir())
          on.exit(setwd(owd))

          result=makeROCPPT()

          data2pptx(result)
          file.rename("report.pptx", file)

     }
)

remove_shiny_inputs <- function(id, .input) {
     invisible(
          lapply(grep(id, names(.input), value = TRUE), function(i) {
               .subset2(.input, "impl")$.values$remove(i)
          })
     )
}

observeEvent(input$doROC, {
     i <- sprintf('%04d', input$doROC)
     id <- sprintf('showData%s', i)
     insertUI(
          selector = sprintf('#%s',ns('ROCUI')),
          where = "beforeBegin",
          ui = showDataUI(ns(id))
     )
     callModule(showData, id,
                df=reactive(makeROCPPT()),
                plotheight="500px",
                plotwidth="500px")
     observeEvent(input[[paste0(id, '-deleteButton')]], {
          removeUI(selector = sprintf('#%s', ns(id)))
          remove_shiny_inputs(id, input)
     })
})

return(resultdf)

}
