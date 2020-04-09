#' UI function of Tree module
#' @param id id
#' @importFrom shiny NS uiOutput
#' @export
TreeModuleInput=function(id){
     ns <-NS(id)

     uiOutput(ns("TreeModule"))
}


#' Server function of Tree shiny module
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
TreeModule=function(input,output,session,dataname,df,preprocessing,PPTdata,lang){

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
               exData=input$TreeEx
          )
     })




output$TreeModule=renderUI({
     tagList(
          myp(langchoice1(183)),
          hr(),
          fluidRow(
               column(7,
                      myp(langchoice1(127)),
                      multiChooserUI(ns("TreeChooser"))
               ),
               column(5,
                      selectInput(ns("TreeEx"),langchoice1(135),choices=c("None","iris")),
                      conditionalPanel(sprintf("input[['%s']]!='None'",ns("TreeEx")),
                                       checkboxInput(ns("showHelpTree"),"show help for data",value=FALSE)
                      ),
                      checkboxInput(ns("showTreeData"),"show Data Table",value=FALSE)
               )
          ),
          htmlOutput(ns("helpDataTree")),
          DTOutput(ns("TreeData")),
          conditionalPanel("true==false",
                           checkboxInput(ns("TreeOK"),"TreeOK",value=FALSE)
                           ),

          conditionalPanel(sprintf("input[['%s']]==true",ns("TreeOK")),
                           textInput(ns("treeEq"),"Equation",value="",width="100%"),
                           uiOutput(ns("TreeUI")),
                           fluidRow(
                                column(3,actionButton(ns('doTree')," Tree Analysis ")),
                                column(3,downloadButton(ns("TreeReport"),"download Report")),
                                column(3,downloadButton(ns("TreePPTx"),"download PPTx")),
                                column(3,actionButton(ns("addTreeList"),"add to PPT List",icon=icon("shopping-cart")))
                           )
          )

     )
})

Treelabels=reactive({c(langchoice1(150),
                      langchoice1(151))})

Treemultiples=c(FALSE,TRUE)
Treebivars=c(FALSE,FALSE)


TreeResult=callModule(multiChooser,"TreeChooser",labels=Treelabels,
                     df=reactive(data()),multiples=Treemultiples,bivars=Treebivars,
                     lang=reactive(lang()))

output$helpDataTree=renderPrint({
     if(input$showHelpTree){
          help_console(dataname(),"html")
     }


})

output$TreeData=renderDT({

     if(input$showTreeData){
          datatable(eval(parse(text=dataname())))
     }

})

observe({
     res<-TreeResult()

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
          updateCheckboxInput(session,"TreeOK",value=ifelse(mode==1,TRUE,FALSE))
          if(mode==1){
                  temp=paste0("ctree(",res[[1]],"~",paste0(res[[2]],collapse="+"),",data=",dataname(),")")
                  updateTextInput(session,"treeEq",value=temp)

          }
     }


})


makeTreePPT=reactive({

     input$doTree
     input$Treemultiple

     res<-TreeResult()

     result=makePPTList_tree(input$treeEq)
     result



})

observeEvent(input$addTreeList,{

     result=makeTreePPT()


     savedPPT$type=c(savedPPT$type,result$type)
     savedPPT$title=c(savedPPT$title,result$title)
     savedPPT$code=c(savedPPT$code,result$code)

})


output$TreeReport = downloadHandler(

     filename="Tree.HTML",
     content=function(file){

          owd <- setwd(tempdir())
          on.exit(setwd(owd))

          result=makeTreePPT()

          data2HTML(result)
          file.rename("report.HTML", file)

     }
)

output$TreePPTx = downloadHandler(

     filename="Tree.pptx",
     content=function(file){

          owd <- setwd(tempdir())
          on.exit(setwd(owd))

          result=makeTreePPT()

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

observeEvent(input$doTree, {
     i <- sprintf('%04d', input$doTree)
     id <- sprintf('showData%s', i)
     insertUI(
          selector = sprintf('#%s',ns('TreeUI')),
          where = "beforeBegin",
          ui = showDataUI(ns(id))
     )
     callModule(showData, id,
                df=reactive(makeTreePPT()))

     observeEvent(input[[paste0(id, '-deleteButton')]], {
          removeUI(selector = sprintf('#%s', ns(id)))
          remove_shiny_inputs(id, input)
     })
})

return(resultdf)

}
