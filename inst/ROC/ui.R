library(webrExtra)

shinyUI(fluidPage(
    uiOutput("title"),
    singleton(
        tags$head(tags$script(src = "message-handler.js"))
    ),
    radioButtons(inputId = "language", label = "Select Language",
                 choices = list("English" = "en", "한국어(Korean)" = "kor"),
                 selected = "en",inline=TRUE),
    navbarPage( "Web-R.org",
                tabPanel("DataSelect",
                         dataSelectInput("data"),
                         tableOutput("table3")
                         ,verbatimTextOutput("text")

                ),
                tabPanel("ROC",
                        ROCModuleInput("ROC1")),
                navbarMenu("Classification",
                           tabPanel("Decision Tree",
                                    TreeModuleInput("Tree")),
                           tabPanel("Survival Tree",
                                    sTreeModuleInput("sTree"))),
                tabPanel("PPTxList",
                         pptxListInput("List1")),
                id='main',
                theme=shinytheme("cerulean")
    )
    ,verbatimTextOutput("table4")

)
)
