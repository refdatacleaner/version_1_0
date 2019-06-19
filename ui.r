library("shiny")
library("shinydashboard")
library("shinyjs")
library("shinyalert")
library("markdown")
library("splus2R")
library("DT")
library("data.table")
library("xlsx")
library("jsonlite")
library("readr")
library("XML")
library("rvest")
library("tableHTML")
library("kulife")
library("expss")
library("pracma")
library("shinyBS")

fluidPage(
  navbarPage("RefDataCleaner",
             tabPanel("Fix",
                      tags$head(
                        tags$script(src = "message-handler.js"),
                        tags$link(rel = "stylesheet", type = "text/css", href = "estilos.css")
                      ),
                      useShinyjs(),
                      
                      tags$div(id="zero",
                               fileInput('datafile','Choose your file',
                                         accept=c('text/csv','text/comma-separated-values,text/plain',".txt",".csv",".xls",".xlsx",".json",".xml",".html"))
                      ),
                      tags$div(id="one",
                               tags$div(id="two_1",
                                        fileInput('datafile2','Add reference file',
                                                  accept=c('text/csv','text/comma-separated-values,text/plain',".txt",".csv",".xls",".xlsx",".json",".xml",".html"))
                               ),
                               tags$div(id="two_2",
                                        selectInput("select_reference", label="View reference file", choices=c(""))
                               )
                      ),  
                      tags$div(id="three",
                               tableOutput("filetable")
                      ),
                      tags$div(id="four",
                               tableOutput("filetable2")
                      ),
                      tags$div(id="five-a",
                               p("Add Rule")
                      ),
                      tags$div(id="six-a",
                               p("Apply Rules")
                      ),
                      tags$div(id="six_b",
                               selectInput("select_file_download", label="Select file type", choices=c(".xlsx",".xls",".csv",".txt",".json",".xml",".html"), width="100px")
                      ),
                      tags$div(id="five",
                               #bsButton("b_add", icon = icon("plus-circle"),"Add new rule"),
                               actionButton("b_add",icon("plus-circle")),
                               bsTooltip("b_add","Add new rule", placement = "bottom", trigger = "hover",
                                         options = NULL),
                               actionButton("b_remove",icon("minus-circle")),
                               bsTooltip("b_remove","Remove selected rule", placement = "bottom", trigger = "hover",
                                         options = NULL),
                               actionButton("b_up",icon("arrow-circle-up")),
                               bsTooltip("b_up","Move up selected rule", placement = "bottom", trigger = "hover",
                                         options = NULL),
                               actionButton("b_down",icon("arrow-circle-down")),
                               bsTooltip("b_down","Move down selected rule", placement = "bottom", trigger = "hover",
                                         options = NULL),
                               downloadButton('downloadRules', ''),
                               bsTooltip("downloadRules","Download made rules", placement = "bottom", trigger = "hover",
                                         options = NULL)
                      ),
                      tags$div(id="six",
                               actionButton("b_fix",icon("play-circle")),
                               bsTooltip("b_fix","Apply made rules", placement = "bottom", trigger = "hover",
                                         options = NULL),
                               downloadButton('downloadData', ''),
                               bsTooltip("downloadData","Download corrected file", placement = "bottom", trigger = "hover",
                                         options = NULL)
                      ),
                      tags$div(id="seven",
                               DT::dataTableOutput("ruleTable")
                      ),
                      tags$div(id="eight",
                               tableOutput("resultTable")
                      )
                      
             ),
             tabPanel("Help",
                      includeMarkdown("files/start.md")
             )
  )
)