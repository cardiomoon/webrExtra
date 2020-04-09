#' UI function of sTree module
#' @param id id
#' @importFrom shiny NS uiOutput
#' @export
sTreeModuleInput=function(id){
     ns <-NS(id)

     uiOutput(ns("sTreeModule"))
}


#' Server function of sTree shiny module
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
#' @importFrom shiny textInput updateTextInput
#' @importFrom shiny actionButton callModule column downloadButton downloadHandler fluidRow hr icon insertUI
#' @importFrom shiny observe observeEvent reactive removeUI renderPrint renderUI tagList updateCheckboxInput
#' @importFrom DT DTOutput datatable renderDT
#' @importFrom webrSub multiChooserUI multiChooser help_console showData showDataUI myp
#' @importFrom rrtable data2HTML data2pptx
#' @export
sTreeModule=function(input,output,session,dataname,df,preprocessing,PPTdata,lang){

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
               exData=input$sTreeEx
          )
     })




output$sTreeModule=renderUI({
     tagList(
          myp(langchoice1(184)),
          hr(),
          fluidRow(
               column(7,
                      myp(langchoice1(127)),
                      multiChooserUI(ns("sTreeChooser"))
               ),
               column(5,
                      selectInput(ns("sTreeEx"),langchoice1(135),choices=c("None","GBSG2")),
                      conditionalPanel(sprintf("input[['%s']]!='None'",ns("sTreeEx")),
                                       checkboxInput(ns("showHelpsTree"),"show help for data",value=FALSE)
                      ),
                      checkboxInput(ns("showsTreeData"),"show Data Table",value=FALSE)
               )
          ),
          htmlOutput(ns("helpDatasTree")),
          DTOutput(ns("sTreeData")),
          conditionalPanel("true==false",
                           checkboxInput(ns("sTreeOK"),"sTreeOK",value=FALSE)
                           ),

          conditionalPanel(sprintf("input[['%s']]==true",ns("sTreeOK")),
                           textInput(ns("sTreeEq"),"Equation",value="",width="100%"),
                           uiOutput(ns("sTreeUI")),
                           fluidRow(

                                column(3,actionButton(ns('dosTree')," sTree Analysis ")),
                                column(3,downloadButton(ns("sTreeReport"),"download Report")),
                                column(3,downloadButton(ns("sTreePPTx"),"download PPTx")),
                                column(3,actionButton(ns("addsTreeList"),"add to PPT List",icon=icon("shopping-cart")))
                           )
          )

     )
})

sTreelabels=reactive({c(langchoice1(163),
                        langchoice1(164),
                      langchoice1(151))})

sTreemultiples=c(FALSE,FALSE,TRUE)
sTreebivars=c(FALSE,TRUE,FALSE)


sTreeResult=callModule(multiChooser,"sTreeChooser",labels=sTreelabels,
                     df=reactive(data()),multiples=sTreemultiples,bivars=sTreebivars,
                     lang=reactive(lang()))

output$helpDatasTree=renderPrint({
     if(input$showHelpsTree){
          help_console(dataname(),"html")
     }


})

output$sTreeData=renderDT({

     if(input$showsTreeData){
          datatable(eval(parse(text=dataname())))
     }

})

observe({
     res<-sTreeResult()

     temp=""
     mode=0
     if(length(res)>0){
             if(res[[1]]=="") {
                     temp=""
                     mode=0
             } else if(res[[2]]=="") {
                     temp=""
                     mode=0
             } else {
                     mode=1
                     temp=paste0("ctree(Surv(",res[[1]],",",res[[2]],")~")
                     if(length(res)>2){
                             if(!is.null(res[[3]])){
                                     if(length(res[[3]])>1){
                                             temp=paste(temp,paste0(res[[3]],collapse="+"))
                                             mode=2
                                     } else if(res[[3]]!=""){
                                             temp=paste(temp,paste0(res[[3]],collapse="+"))
                                             mode=2
                                     }
                             }

                     }
                     temp=paste0(temp,",data=",dataname(),")")
             }
             updateTextInput(session,"sTreeEq",value=temp)
             updateCheckboxInput(session,"sTreeOK",value=ifelse(mode==2,TRUE,FALSE))


     }


})


makesTreePPT=reactive({

     input$dosTree
     input$sTreemultiple

     res<-sTreeResult()

     result=makePPTList_surTree(input$sTreeEq)
     result



})

observeEvent(input$addsTreeList,{

     result=makesTreePPT()


     savedPPT$type=c(savedPPT$type,result$type)
     savedPPT$title=c(savedPPT$title,result$title)
     savedPPT$code=c(savedPPT$code,result$code)

})


output$sTreeReport = downloadHandler(

     filename="sTree.HTML",
     content=function(file){

          owd <- setwd(tempdir())
          on.exit(setwd(owd))

          result=makesTreePPT()

          data2HTML(result)
          file.rename("report.HTML", file)

     }
)

output$sTreePPTx = downloadHandler(

     filename="sTree.pptx",
     content=function(file){

          owd <- setwd(tempdir())
          on.exit(setwd(owd))

          result=makesTreePPT()

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

observeEvent(input$dosTree, {
     i <- sprintf('%04d', input$dosTree)
     id <- sprintf('showData%s', i)
     insertUI(
          selector = sprintf('#%s',ns('sTreeUI')),
          where = "beforeBegin",
          ui = showDataUI(ns(id))
     )
     callModule(showData, id,
                df=reactive(makesTreePPT()))

     observeEvent(input[[paste0(id, '-deleteButton')]], {
          removeUI(selector = sprintf('#%s', ns(id)))
          remove_shiny_inputs(id, input)
     })
})

return(resultdf)

}
