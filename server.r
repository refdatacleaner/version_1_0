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

function(input, output, session) {
  
  var_options<<-c("Substitution")
  var_values_references <- ""
  var_row_rule<-0
  name<-""
  HTMLClassName_reference<-c("addField_r_1")
  var_array_condition <-0
  var_array_fix <-0
  var_array_condition2 <-0
  var_array_reference <-0
  var_array_fix2 <-0
  
  var_b_condition <- 1
  var_condition <- 0
  var_b_condition2 <- 1
  var_condition2 <- 0
  var_b_reference <- 1
  var_reference <- 0
  var_b_fix <- 1
  var_fix <- 0
  var_b_fix2 <- 1
  var_fix2 <- 0
  input_df <- NULL
  my.list <- list()
  my.list.reference <- list()
  var_list<-2
  rule_number <- 1
  row_file<-NULL
  resultTable<-NULL
  file_name<-NULL
  file_reference_number<-1
  file_type_download<-""
  
  #make dataframe rules
  vals <- reactiveValues(
    rule_df = data.frame(type=character(),
                         condition=character(),
                         reference=character(),
                         fix=character(),
                         file=character())
  )
  
  #upload filedata to server
  filedata <-function(infile,file_number){
    if (is.null(infile)) {
      # User has not uploaded a file yet
      return(NULL)
    }
    # Determine document format;
    ptn <- "\\.[[:alnum:]]{1,5}$"
    suf <- tolower(regmatches(infile$name, regexpr(ptn, infile$name)))
    #print(suf)
    file_name[[file_number]]<<-infile$name
    if(suf==".csv"){
      my_fila_df <<- read.csv(infile$datapath,sep = ",", fileEncoding="latin1")
      #print(str(my_fila_df))
    }else if(suf==".txt"){
      my_fila_df <<- read.delim(infile$datapath,sep = ",", fileEncoding="latin1")
      #print(str(my_fila_df))
    }else if(suf %in% c('.xls', '.xlsx')){
      #my_fila_df <<- readWorksheetFromFile(infile$datapath,sheet = 1)
      my_fila_df <<- read.xlsx(infile$datapath,1,TRUE)
      #print(str(my_fila_df))
    }else if(suf %in% c('.json')){
      result_j <- fromJSON(infile$datapath)
      my_fila_df <- as.data.frame(result_j)
      #print(str(my_fila_df))
    }else if(suf %in% c('.xml')){
      my_fila_df <<- xmlToDataFrame(infile$datapath)
      #print(str(my_fila_df))
    }else if(suf %in% c('.html')){
      my_fila_df <<- as.data.frame(read_html(infile$datapath) %>% html_table(fill=TRUE))
    } 
    my_fila_df <- type.convert(my_fila_df, as.is = TRUE)
    #print(str(my_fila_df))
    return(my_fila_df)
  }
  
  #This previews the CSV data file
  output$filetable <- renderTable({
    my.list[[1]] <<- filedata(input$datafile,1)
  })
  
  observeEvent(input$datafile, {
    #print("new file")
    vals$rule_df<<-vals$rule_df[0,]
  })  
  
  #upload reference file
  observeEvent(input$datafile2, {
    file_reference_number<<-file_reference_number+1
    infile<-input$datafile2
    ptn <- "\\.[[:alnum:]]{1,5}$"
    suf <- tolower(regmatches(infile$name, regexpr(ptn, infile$name)))
    name<<-sub(suf,"",infile$name)
    my.list.reference[[name]] <<- filedata(input$datafile2,file_reference_number)
    
    var_values_references <<- c(name,var_values_references)
    
    shinyjs::enable("two_2")
    
    var_options<<-c("Substitution","Reference")
    
    # Can also set the label and select items
    updateSelectInput(session, "select_reference",
                      label = paste("Select reference file", length(name)),
                      choices = var_values_references,
                      selected = tail(name, 1)
    )
  })
  
  observe({
    if (is.null(input$datafile) || input$datafile == "") {
      #shinyjs::disable("one")
      #shinyjs::disable("two_1")
      #shinyjs::disable("datafile2")
      shinyjs::disable("two_2")
      shinyjs::disable("b_add")
      shinyjs::disable("b_remove")
      shinyjs::disable("b_up")
      shinyjs::disable("b_down")
      shinyjs::disable("downloadRules")
      shinyjs::disable("six")
      shinyjs::disable("six_b")
      shinyjs::disable("downloadData")
    }else if (!is.null(input$datafile) || input$datafile != "") {
      #shinyjs::enable("one")
      #shinyjs::enable("two_1")
      #shinyjs::enable("datafile2")
      shinyjs::enable("b_add")
    }
  })
  
  #select file to show
  select_referenceInput <- reactive({
    name<<-input$select_reference
    my.list.reference[[name]]
  })
  
  #print reference file
  output$filetable2 <- renderTable({
    select_referenceInput()
  })  
  
  #make rules
  observeEvent(input$b_add, {
    shinyjs::hide(id = "box_reference")
    var_b_condition<<-var_b_condition + 1
    var_b_fix<<-var_b_fix + 1
    var_condition<<-var_b_condition - 1
    var_fix<<-var_b_fix - 1
    
    var_b_condition2<<-var_b_condition2 + 1
    var_b_reference<<-var_b_reference + 1
    var_b_fix2<<-var_b_fix2 + 1
    var_condition2<<-var_b_condition2 - 1
    var_reference<<-var_b_reference - 1
    var_fix2<<-var_b_fix2 - 1
    
    # Show a modal when the button is pressed
    showModal(
      modalDialog(
        title = "Add Rule",
        selectInput("type_rule", label="Select rule type", choices=var_options),
        div(id = "box_reference", 
            selectInput("select_file", label="Select reference file", choices=var_values_references),
            tags$p("Conditions"),
            fluidRow(
              column(4,"column name"),
              column(4,"condition"),
              column(4,"value")),
            tags$p(" "),
            uiOutput("fieldInputPanel3"),
            fluidRow(
              column(1,actionButton("addButtonCondition2",icon("plus-circle"))),
              column(11,"Add Condition")),
            tags$hr(style="border-color: blue;"),
            tags$p("References"),
            fluidRow(
              column(3,"column name input file FK"),
              column(3," "),
              column(3,"column name reference file PK"),
              column(3," ")),
            tags$p(" "),
            uiOutput("fieldInputPanel4"),
            fluidRow(
              column(1,actionButton("addButtonReference",icon("plus-circle"))),
              column(11,"Add Reference")),
            tags$hr(style="border-color: blue;"),
            tags$p("Assigning"),
            fluidRow(
              column(3,"column name input file"),
              column(3," "),
              column(3,"column name reference file"),
              column(3," ")),
            tags$p(" "),
            uiOutput("fieldInputPanel5"),
            fluidRow(
              column(1,actionButton("addButtonFix2",icon("plus-circle"))),
              column(11,"Add Fix"))
        ),
        div(id = "box_substitution",
            tags$p("Conditions"),
            fluidRow(
              column(4,"column name"),
              column(4,"condition"),
              column(4,"value")),
            tags$p(" "),
            uiOutput("fieldInputPanel"),
            fluidRow(
              column(1,actionButton("addButtonCondition",icon("plus-circle"))),
              column(11,"Add Condition")),
            tags$hr(style="border-color: blue;"),
            tags$p("Assigning"),
            fluidRow(
              column(4,"column name input file"),
              column(4," "),
              column(4,"value")),
            tags$p(" "),        
            uiOutput("fieldInputPanel2"),     
            fluidRow(
              column(1,actionButton("addButtonFix",icon("plus-circle"))),
              column(11,"Add Fix"))
        ),
        footer = fluidRow(
          column(1,actionButton("save_modal",label = "Save")),
          column(11,actionButton("dismiss_modal",label = "Dismiss"))),
        size="l"
      ))
    
    observeEvent(input$type_rule, {
      if(input[['type_rule']] == "Substitution"){
        shinyjs::hide(id = "box_reference")
        shinyjs::show(id = "box_substitution")
        shinyjs::disable("addButtonFix")
      }else{
        shinyjs::show(id = "box_reference")
        shinyjs::hide(id = "box_substitution")
        shinyjs::disable("addButtonReference")
        shinyjs::disable("addButtonFix2")
      }
    })
    
    observeEvent(input$select_file, {
      name<<-input$select_file
      #print(name)
      input_df<<-NULL
      input_df<<-my.list.reference[[name]]
      #print(HTMLClassName_reference)
      if(HTMLClassName_reference!=""){
        for (i in 1:length(HTMLClassName_reference)){
          #print(HTMLClassName_reference[[i]])
          removeUI(selector = paste0(".",HTMLClassName_reference[[i]]), multiple = TRUE)
        }
      }
      var_array_reference<<-0
      var_array_fix2<<-0
      HTMLClassName_reference<<-""
    })    
    
  })
  
  #define type value
  type_first_row<-function(var_text){
    if(var_text=="" | is.na(var_text)==TRUE | var_text=="NA"| var_text==" " ){
      return("Empty")
    }else if(is.na(as.numeric(var_text))==FALSE){
      return("Number")
    }else{
      return("String")
    }
  }
  
  evaluate_options<-function(value_1,value_2,sygnal,css){
    choices=c("< ","> ","<=",">=")
    var_one<-type_first_row(value_1)
    var_two<-type_first_row(value_2)
    var_three<-sygnal %in% choices
    if(var_one=="Empty" | var_two=="Empty"){
      shinyjs::enable(css)
    }else if(var_one==var_two){
      if(var_one=="String" & var_three==FALSE){
        shinyjs::enable(css)
      }else if(var_one=="Number"){shinyjs::enable(css)
      }else{shinyjs::disable(css)}
    }else{shinyjs::disable(css)}
  }
  
  
  #Make insertUI into show modal  
  insertUI_form<-function(selector_form,HTMLClassName,inputFieldName,outputList,refFieldName,outputList2,inpuText,outputList3,deleteButtonName){
    insertUI(
      selector = selector_form,
      where = "beforeEnd",
      ui = tagList(
        div(class = HTMLClassName, style="display: inline-block;vertical-align:top; width: 300px; margin-top:0px; font-size:9pt;",
            selectInput(inputFieldName, label=NULL, choices=outputList)),
        div(class = HTMLClassName, style="display: inline-block;vertical-align:top; width: 100px; margin-top:0px; font-size:9pt;",
            selectInput(refFieldName, label=NULL, choices=outputList2)),
        div(class = HTMLClassName,  style="display: inline-block;vertical-align:top; width: 300px; margin-top:0px; font-size:9pt;",
            if(outputList3==""){  
              textInput(inpuText,label=NULL,value = "")
            }else{
              selectInput(inpuText, label=NULL, choices=outputList3)  
            }
        ), 
        div(class = HTMLClassName, style="display: inline-block;vertical-align:top; width: 50px; margin-top:0px; font-size:9pt;",
            actionButton(deleteButtonName, "",icon("minus-circle")))
      ))
  }
  
  
  observeEvent(input$addButtonCondition,{
    
    var_condition<<-var_condition + 1
    var_array_condition <<- c(var_array_condition,var_condition)
    
    outputList = colnames(my.list[[1]])
    outputList2 = c("==","< ","> ","<=",">=","!=")
    deleteButtonName = paste0("deleteButton_c_",var_condition)
    inputFieldName = paste0("inputFieldName_c_",var_condition)
    refFieldName = paste0("refFieldName_c_",var_condition)
    inpuText=paste0("inputText_c_",var_condition)
    HTMLClassName = paste0("addField_c_",var_condition)
    insertUI_form("#fieldInputPanel",HTMLClassName,inputFieldName,outputList,refFieldName,outputList2,inpuText,"",deleteButtonName)
    
    observeEvent(input[[deleteButtonName]], {
      valor<-strtoi(paste0(gsub("addField_c_", "", HTMLClassName)))
      remove <- c (0, valor)
      var_array_condition<<-var_array_condition [! var_array_condition %in% remove]
      removeUI(selector = paste0(".",HTMLClassName), multiple = TRUE)
      if(length(var_array_condition)==0){
        shinyjs::disable("addButtonFix")
        shinyjs::disable("save_modal")
      }
    })
    
    observe({
      column<-input[[inputFieldName]]
      if(is.null(column)==FALSE){
        df<-my.list[[1]][1,]
        evaluate_options(df[column],"",input[[refFieldName]],"addButtonFix")
      }else{
        shinyjs::disable("addButtonFix")
        shinyjs::disable("save_modal")
      }
    })
    
  })
  
  observeEvent(input$addButtonCondition2,{
    var_condition2<<-var_condition2 + 1
    var_array_condition2 <<- c(var_array_condition2,var_condition2)
    outputList = colnames(my.list[[1]])
    outputList2 = c("==","< ","> ","<=",">=","!=")
    deleteButtonName = paste0("deleteButton_c2_",var_condition2)
    inputFieldName = paste0("inputFieldName_c2_",var_condition2)
    refFieldName = paste0("refFieldName_c2_",var_condition2)
    inpuText=paste0("inputText_c2_",var_condition2)
    HTMLClassName = paste0("addField_c2_",var_condition2)
    insertUI_form("#fieldInputPanel3",HTMLClassName,inputFieldName,outputList,refFieldName,outputList2,inpuText,"",deleteButtonName)
    
    observeEvent(input[[deleteButtonName]], {
      valor<-strtoi(paste0(gsub("addField_c2_", "", HTMLClassName)))
      remove <- c (0, valor)
      var_array_condition2<<-var_array_condition2 [! var_array_condition2 %in% remove]
      removeUI(selector = paste0(".",HTMLClassName), multiple = TRUE)
      if(length(var_array_condition2)==0){
        shinyjs::disable("addButtonReference")
        shinyjs::disable("addButtonFix2")
        shinyjs::disable("save_modal")
      }
    })
    
    observe({
      column<-input[[inputFieldName]]
      if(is.null(column)==FALSE){
        df<-my.list[[1]][1,]
        evaluate_options(df[column],"",input[[refFieldName]],"addButtonReference")
      }else{
        shinyjs::disable("addButtonReference")
        shinyjs::disable("addButtonFix2")
        shinyjs::disable("save_modal")
      }
    })
    
  })
  
  observeEvent(input$addButtonReference,{
    var_reference<<-var_reference + 1
    var_array_reference <<- c(var_array_reference,var_reference)
    remove <- c (0)
    var_array_reference<<-var_array_reference [! var_array_reference %in% remove]
    outputList = colnames(my.list[[1]])
    outputList2 = "=="
    outputList3 = colnames(input_df)
    deleteButtonName = paste0("deleteButton_r_",var_reference)
    inputFieldName = paste0("inputFieldName_r_",var_reference)
    refFieldName = paste0("refFieldName_r_",var_reference)
    inputReferenceName = paste0("inputReferenceName_r_",var_reference)
    HTMLClassName = paste0("addField_r_",var_reference)
    HTMLClassName_reference<<-c(HTMLClassName,HTMLClassName_reference)
    insertUI_form("#fieldInputPanel4",HTMLClassName,inputFieldName,outputList,refFieldName,outputList2,inputReferenceName,outputList3,deleteButtonName)
    
    observeEvent(input[[deleteButtonName]], {
      valor<-strtoi(paste0(gsub("addField_r_", "", HTMLClassName)))
      remove <- c (valor)
      var_array_reference<<-var_array_reference [! var_array_reference %in% remove]
      removeUI(selector = paste0(".",HTMLClassName), multiple = TRUE)
      if(length(var_array_reference)==0){
        shinyjs::disable("addButtonFix2")
        shinyjs::disable("save_modal")
      }else if(length(var_array_reference)<5){
        shinyjs::enable("addButtonReference")
      }else if(length(var_array_reference)>=5){
        shinyjs::disable("addButtonReference")
      }
    })
    
    observe({
      column<-input[[inputFieldName]]
      if(is.null(column)==FALSE){
        df<-my.list[[1]][1,]
        df2<-input_df[1,]
        column2<-input[[inputReferenceName]]
        evaluate_options(df[column],df2[column2],input[[refFieldName]],"addButtonFix2")
      }else{
        shinyjs::disable("addButtonFix2")
        shinyjs::disable("save_modal")
      }
      #print(var_array_reference)
      if(length(var_array_reference)<5){
        shinyjs::enable("addButtonReference")
      }else if(length(var_array_reference)>=5){
        shinyjs::disable("addButtonReference")
      }
    })
    
  })  
  
  observeEvent(input$addButtonFix,{
    var_fix<<-var_fix + 1
    var_array_fix <<- c(var_array_fix,var_fix)
    outputList = colnames(my.list[[1]])
    outputList2="<-"
    deleteButtonName = paste0("deleteButton_f_",var_fix)
    inputFieldName = paste0("inputFieldName_f_",var_fix)
    refFieldName = paste0("refFieldName_f_",var_fix)
    inpuText=paste0("inputText_f_",var_fix)
    HTMLClassName = paste0("addField_f_",var_fix)
    insertUI_form("#fieldInputPanel2",HTMLClassName,inputFieldName,outputList,refFieldName,outputList2,inpuText,"",deleteButtonName)
    
    observeEvent(input[[deleteButtonName]], {
      valor<-strtoi(paste0(gsub("addField_f_", "", HTMLClassName)))
      remove <- c (0, valor)
      var_array_fix<<-var_array_fix [! var_array_fix %in% remove]
      removeUI(selector = paste0(".",HTMLClassName), multiple = TRUE)
      if(length(var_array_fix)==0){
        shinyjs::disable("save_modal")
      }
    })
    
    observe({
      column<-input[[inputFieldName]]
      if(is.null(column)==FALSE){
        df<-my.list[[1]][1,]
        evaluate_options(df[column],input[[inpuText]],"==","save_modal")
      }else{
        shinyjs::disable("save_modal")
      }
    })
    
  })
  
  
  observeEvent(input$addButtonFix2,{
    var_fix2<<-var_fix2 + 1
    var_array_fix2 <<- c(var_array_fix2,var_fix2)
    outputList = colnames(my.list[[1]])
    outputList2 = "<-" 
    outputList3 = colnames(input_df)
    deleteButtonName = paste0("deleteButton_f2_",var_fix2)
    inputFieldName = paste0("inputFieldName_f2_",var_fix2)
    refFieldName = paste0("refFieldName_f2_",var_fix2)
    inputReferenceName = paste0("inputReferenceName_f2_",var_fix2)
    HTMLClassName = paste0("addField_f2_",var_fix2)
    HTMLClassName_reference<<-c(HTMLClassName_reference,HTMLClassName)
    insertUI_form("#fieldInputPanel5",HTMLClassName,inputFieldName,outputList,refFieldName,outputList2,inputReferenceName,outputList3,deleteButtonName)
    
    observeEvent(input[[deleteButtonName]], {
      valor<-strtoi(paste0(gsub("addField_f2_", "", HTMLClassName)))
      remove <- c (0, valor)
      var_array_fix2<<-var_array_fix2 [! var_array_fix2 %in% remove]
      removeUI(selector = paste0(".",HTMLClassName), multiple = TRUE)
      if(length(var_array_fix2)==0){
        shinyjs::disable("save_modal")
      }
    })
    
    observe({
      column<-input[[inputFieldName]]
      if(is.null(column)==FALSE){
        df<-my.list[[1]][1,]
        df2<-input_df[1,]
        column2<-input[[inputReferenceName]]
        evaluate_options(df[column],df2[column2],"==","save_modal")
      }else{
        shinyjs::disable("save_modal")
      }
    })
    
  })
  
  observeEvent(input$select_file_download,{
    #print(input$select_file_download)
    file_type_download<<-input$select_file_download
  })
  
  observeEvent(input$save_modal,{
    #print("saving")
    if(input[['type_rule']] == "Substitution"){
      #print("FALSE")
      c_file=""
      f_file=""
      var_subs_a=0
      var_subs_b=0
      for (i in var_b_condition:var_condition){
        if(i %in% var_array_condition == TRUE){
          a<-input[[paste0("inputFieldName_c_",i)]]   
          b<-input[[paste0("refFieldName_c_",i)]]   
          c<-input[[paste0("inputText_c_",i)]]
          if(c==""){c<-""}
          c_file=paste(c_file,a,b,c,"&",sep = "")
          var_subs_a=1
        }
      }
      for (i in var_b_fix:var_fix){
        if(i %in% var_array_fix == TRUE){
          a<-input[[paste0("inputFieldName_f_",i)]]   
          b<-input[[paste0("refFieldName_f_",i)]]   
          c<-input[[paste0("inputText_f_",i)]]
          if(c==""){c<-""}
          f_file=paste(f_file,a,b,c,"&",sep = "")
          var_subs_b=1
        }                 
      }
      if(var_subs_a==1 & var_subs_b==1){  
        d1<-NULL
        var_b_condition<<-var_condition + 1
        var_b_fix<<-var_fix + 1
        c_file=substr(c_file,1,nchar(c_file)-1)
        f_file=substr(f_file,1,nchar(f_file)-1)
        d1<-data.frame(type="substitution",condition=c_file,reference="",fix=f_file,file="")
        vals$rule_df<-rbind(vals$rule_df, d1)
      }
    }else {
      #print("TRUE")
      c2_file=""
      r_file=""
      f2_file=""
      var_subs_a=0
      var_subs_b=0
      var_subs_c=0
      for (i in var_b_condition2:var_condition2){
        if(i %in% var_array_condition2 == TRUE){
          a<-input[[paste0("inputFieldName_c2_",i)]]   
          b<-input[[paste0("refFieldName_c2_",i)]]   
          c<-input[[paste0("inputText_c2_",i)]]
          if(c==""){c<-""}
          c2_file=paste(c2_file,a,b,c,"&",sep = "")
          var_subs_a=1
        }
      }
      for (i in var_b_reference:var_reference){
        if(i %in% var_array_reference == TRUE){
          a<-input[[paste0("inputFieldName_r_",i)]]   
          b<-input[[paste0("refFieldName_r_",i)]]   
          c<-input[[paste0("inputReferenceName_r_",i)]]
          r_file=paste(r_file,a,b,c,"&",sep = "")
          var_subs_b=1
        }
      }
      for (i in var_b_fix2:var_fix2){
        if(i %in% var_array_fix2 == TRUE){
          a<-input[[paste0("inputFieldName_f2_",i)]]   
          b<-input[[paste0("refFieldName_f2_",i)]]   
          c<-input[[paste0("inputReferenceName_f2_",i)]]
          f2_file=paste(f2_file,a,b,c,"&",sep = "")
          var_subs_c=1
        }
      }
      if(var_subs_a==1 & var_subs_b==1 & var_subs_c==1){
        d2<-NULL
        var_b_condition2<<-var_condition2 + 1
        var_b_reference<<-var_reference + 1
        var_b_fix2<<-var_fix2 + 1
        c2_file=substr(c2_file,1,nchar(c2_file)-1)
        r_file=substr(r_file,1,nchar(r_file)-1)  
        f2_file=substr(f2_file,1,nchar(f2_file)-1)
        d2<-data.frame(type="reference",condition=c2_file,reference=r_file,fix=f2_file,file=input$select_file)
        vals$rule_df<-rbind(vals$rule_df, d2)
      }
    }  
    
    output$ruleTable  <- DT::renderDataTable({
      total_rows<-nrow(vals$rule_df)
      if(total_rows>=1){
        shinyjs::enable("downloadRules")
        shinyjs::enable("six")
        shinyjs::disable("b_remove")
        shinyjs::disable("b_up")  
        shinyjs::disable("b_down")
      }else{
        shinyjs::disable("downloadRules")
        shinyjs::disable("six")
        shinyjs::disable("b_remove")
        shinyjs::disable("b_up")  
        shinyjs::disable("b_down")
      }
      vals$rule_df
    }, selection = 'single', rownames = FALSE)
    
    removeModal()
    
  })
  
  observeEvent(input$ruleTable_rows_selected,{
    #print(input$ruleTable_rows_selected)
    var_row_rule<<-input$ruleTable_rows_selected
    total_rows<-nrow(vals$rule_df)
    if(var_row_rule==1  & total_rows==1){
      shinyjs::enable("b_remove")
      shinyjs::disable("b_up")  
      shinyjs::disable("b_down")
    }else if(var_row_rule==1 & var_row_rule<total_rows){
      shinyjs::enable("b_remove")
      shinyjs::enable("b_down")
      shinyjs::disable("b_up")
    }else if(var_row_rule>1 & var_row_rule<total_rows){
      shinyjs::enable("b_remove")
      shinyjs::enable("b_up")  
      shinyjs::enable("b_down")
    }else if(var_row_rule==total_rows & var_row_rule>1){
      shinyjs::enable("b_remove")
      shinyjs::enable("b_up")  
      shinyjs::disable("b_down")
    }else{
      shinyjs::disable("b_remove")
      shinyjs::disable("b_up")  
      shinyjs::disable("b_down")
    }
  })
  
  observeEvent(input$dismiss_modal,{
    removeModal()
    #print("modal 1 closed")
  })
  
  observeEvent(input$b_remove, {
    #    print("remove")
    vals$rule_df <- vals$rule_df[-c(as.numeric(var_row_rule)), ]
    shinyjs::disable("b_remove")
    shinyjs::disable("b_up")
    shinyjs::disable("b_down")
    var_row_rule<<-0
  })
  
  proxy = dataTableProxy('ruleTable')
  
  #Move up into dataframe rules
  observeEvent(input$b_up, {
    #print("up")
    rule_df0<-NULL
    rule_df1<-NULL
    rule_df2<-NULL
    rule_df3<-NULL
    rule_df4<-NULL
    total_rows<-nrow(vals$rule_df)
    selection_var<-as.numeric(var_row_rule) # 3
    selection_beg<-selection_var-2 # 1
    selection_aft<-selection_var-1 # 2
    selection_bef<-selection_var+1 # 4
    selection_las<-selection_var+2 # 5
    if(total_rows==2 & selection_var==2){
      rule_df1 <- vals$rule_df[selection_var, ]
      rule_df2 <- vals$rule_df[selection_aft, ]
      rule_df0 <- rbind(rule_df1,rule_df2)
      vals$rule_df <- rule_df0
      proxy %>% selectRows(as.numeric(1))
    }else if(total_rows==3 & selection_var==2){
      rule_df1 <- vals$rule_df[selection_var, ]
      rule_df2 <- vals$rule_df[selection_aft, ]
      rule_df3 <- vals$rule_df[selection_bef, ]
      rule_df0 <- rbind(rule_df1,rule_df2,rule_df3)
      vals$rule_df <- rule_df0
      proxy %>% selectRows(as.numeric(1))
    }else if(total_rows==selection_var & total_rows>1){
      rule_df1 <- vals$rule_df[1:selection_beg, ]
      rule_df2 <- vals$rule_df[selection_var, ]
      rule_df3 <- vals$rule_df[selection_aft, ]
      rule_df0 <- rbind(rule_df1,rule_df2,rule_df3)
      vals$rule_df <- rule_df0
      selection_var=selection_var-1
      proxy %>% selectRows(as.numeric(selection_var))
    }else if(total_rows>3 & selection_var==2){
      rule_df1 <- vals$rule_df[selection_var, ]
      rule_df2 <- vals$rule_df[selection_aft, ]
      rule_df3 <- vals$rule_df[selection_bef:total_rows, ]
      rule_df0 <- rbind(rule_df1,rule_df2,rule_df3)
      vals$rule_df <- rule_df0
      proxy %>% selectRows(as.numeric(1))
    }else if(total_rows>3 & selection_var==3){
      rule_df1 <- vals$rule_df[selection_beg, ]
      rule_df2 <- vals$rule_df[selection_var, ]
      rule_df3 <- vals$rule_df[selection_aft, ]
      rule_df4 <- vals$rule_df[selection_bef:total_rows, ]      
      rule_df0 <- rbind(rule_df1,rule_df2,rule_df3,rule_df4)
      vals$rule_df <- rule_df0
      proxy %>% selectRows(as.numeric(2))
    }else if(total_rows>4 & selection_var>3){
      rule_df1 <- vals$rule_df[1:selection_beg, ]
      rule_df2 <- vals$rule_df[selection_var, ]
      rule_df3 <- vals$rule_df[selection_aft, ]
      rule_df4 <- vals$rule_df[selection_bef:total_rows, ]      
      rule_df0 <- rbind(rule_df1,rule_df2,rule_df3,rule_df4)
      vals$rule_df <- rule_df0
      selection_var=selection_var-1
      proxy %>% selectRows(as.numeric(selection_var))
    }
  })
  
  
  #Move down into dataframe rules  
  observeEvent(input$b_down, {
    #print("down")
    rule_df0<-NULL
    rule_df1<-NULL
    rule_df2<-NULL
    rule_df3<-NULL
    rule_df4<-NULL
    total_rows<-nrow(vals$rule_df)
    selection_pen<-total_rows-1
    selection_var<-as.numeric(var_row_rule) # 3
    selection_beg<-selection_var-2 # 1
    selection_aft<-selection_var-1 # 2
    selection_bef<-selection_var+1 # 4
    selection_las<-selection_var+2 # 5
    if(total_rows==2 & selection_var==1){
      rule_df1 <- vals$rule_df[selection_bef, ]
      rule_df2 <- vals$rule_df[selection_var, ]
      rule_df0 <- rbind(rule_df1,rule_df2)
      vals$rule_df <- rule_df0
      proxy %>% selectRows(as.numeric(2))
    }else if(total_rows>2 & selection_var==1){
      rule_df1 <- vals$rule_df[selection_bef, ]
      rule_df2 <- vals$rule_df[selection_var, ]
      rule_df3 <- vals$rule_df[selection_las:total_rows, ]
      rule_df0 <- rbind(rule_df1,rule_df2,rule_df3)
      vals$rule_df <- rule_df0
      proxy %>% selectRows(as.numeric(2))
    }else if(selection_pen==selection_var){
      rule_df1 <- vals$rule_df[1:selection_aft, ]
      rule_df2 <- vals$rule_df[selection_bef, ]
      rule_df3 <- vals$rule_df[selection_var, ]
      rule_df0 <- rbind(rule_df1,rule_df2,rule_df3)
      vals$rule_df <- rule_df0
      selection_var=selection_var+1
      proxy %>% selectRows(as.numeric(selection_var))
    }else if(total_rows>3 & selection_var==2){
      rule_df1 <- vals$rule_df[1, ]
      rule_df2 <- vals$rule_df[selection_bef, ]
      rule_df3 <- vals$rule_df[selection_var, ]
      rule_df4 <- vals$rule_df[selection_las:total_rows, ]
      rule_df0 <- rbind(rule_df1,rule_df2,rule_df3,rule_df4)
      vals$rule_df <- rule_df0
      proxy %>% selectRows(as.numeric(3))
    }else if(total_rows>4 & selection_var==3){
      rule_df1 <- vals$rule_df[1:selection_aft, ]
      rule_df2 <- vals$rule_df[selection_bef, ]
      rule_df3 <- vals$rule_df[selection_var, ]
      rule_df4 <- vals$rule_df[selection_las:total_rows, ]
      rule_df0 <- rbind(rule_df1,rule_df2,rule_df3,rule_df4)
      vals$rule_df <- rule_df0
      proxy %>% selectRows(as.numeric(4))
    }else if(total_rows!=selection_var){
      rule_df1 <- vals$rule_df[1:selection_aft, ]
      rule_df2 <- vals$rule_df[selection_bef, ]
      rule_df3 <- vals$rule_df[selection_var, ]
      rule_df4 <- vals$rule_df[selection_las:total_rows, ]
      rule_df0 <- rbind(rule_df1,rule_df2,rule_df3,rule_df4)
      vals$rule_df <- rule_df0
      selection_var=selection_var+1
      proxy %>% selectRows(as.numeric(selection_var))
    }
  })
  
  eval_condition<-function(var_row,var_expressions){
    var_result<-TRUE
    var_expression<-unlist(strsplit(var_expressions, "[&]" ))
    choices=c("==","< ","> ","<=",">=","!=")
    pattern=paste(choices, collapse = "|")
    for(i in 1:length(var_expression)){
      var_separate<-unlist(strsplit(var_expression[i],pattern))
      var_signal=regmatches(var_expression[i], regexpr(pattern, var_expression[i]))
      col_name<-var_separate[1]
      s1<-unlist(paste0(var_row[col_name],""))
      s2<-unlist(paste0(c(var_separate[2]),""))
      if(var_signal=="=="){
        if(strcmpi(s1, s2)){
          var_result<-identical(TRUE,var_result)
        #}else if(is.na(s1)==is.na(s2)){
          #var_result<-identical(TRUE,var_result)
        }else{return(FALSE)}
      }else if(var_signal=="!="){
        if(!strcmpi(s1, s2)){
          var_result<-identical(TRUE,var_result)
        }else{return(FALSE)}
      }else { 
        if(!is.na(as.numeric(s1))==FALSE | !is.na(as.numeric(s2))==FALSE){
          return(FALSE)
        }else{
          s3<-var_signal
          c<-paste0(s1,s3,s2)
          if(eval(parse(text=c))==TRUE){
            var_result<-identical(TRUE,var_result)
          }else{return(FALSE)}
        }
      }
    }
    return(var_result)
  }
  
  eval_reference<-function(c,input_df,row){
    reference_f=unlist(strsplit(c, "[&]"))
    #            variable<-NULL
    #for(row_r in 1:length(reference_f)){
    # if(row_r>1 & row_r<length(reference_f)){
    #   variable[1]<-paste0(variable[1]," & ")
    #   }
    # col_r<-unlist(strsplit(reference_f[row_r],"=="))
    # a<-paste0("variable_a_",row_r)   
    # b<-paste0("variable_b_",row_r)
    # variable[a]=col_r[2]
    # variable[b]=paste0(unlist(row[,col_r[1]]),"")
    # variable[1]<-paste0(input_df[variable[a]]==variable[b],"")
    # }
    if(length(reference_f)==1){
      col_r<-unlist(strsplit(reference_f,"=="))
      variable_a<-col_r[2]
      variable_b<-paste0(unlist(row[,col_r[1]]),"")
      variable<-input_df[variable_a]==variable_b
    }else if(length(reference_f)==2){
      col_r<-unlist(strsplit(reference_f[1],"=="))
      variable_a<-col_r[2]
      variable_b<-paste0(unlist(row[,col_r[1]]),"")
      col_r2<-unlist(strsplit(reference_f[2],"=="))
      variable_c<-col_r2[2]
      variable_d<-paste0(unlist(row[,col_r2[1]]),"")
      variable<-input_df[variable_a]==variable_b & input_df[variable_c]==variable_d
    }else if(length(reference_f)==3){
      col_r<-unlist(strsplit(reference_f[1],"=="))
      variable_a<-col_r[2]
      variable_b<-paste0(unlist(row[,col_r[1]]),"")
      col_r2<-unlist(strsplit(reference_f[2],"=="))
      variable_c<-col_r2[2]
      variable_d<-paste0(unlist(row[,col_r2[1]]),"")
      col_r3<-unlist(strsplit(reference_f[3],"=="))
      variable_e<-col_r3[2]
      variable_f<-paste0(unlist(row[,col_r3[1]]),"")
      variable<-input_df[variable_a]==variable_b & input_df[variable_c]==variable_d & input_df[variable_e]==variable_f
    }else if(length(reference_f)==4){
      col_r<-unlist(strsplit(reference_f[1],"=="))
      variable_a<-col_r[2]
      variable_b<-paste0(unlist(row[,col_r[1]]),"")
      col_r2<-unlist(strsplit(reference_f[2],"=="))
      variable_c<-col_r2[2]
      variable_d<-paste0(unlist(row[,col_r2[1]]),"")
      col_r3<-unlist(strsplit(reference_f[3],"=="))
      variable_e<-col_r3[2]
      variable_f<-paste0(unlist(row[,col_r3[1]]),"")
      col_r4<-unlist(strsplit(reference_f[4],"=="))
      variable_g<-col_r4[2]
      variable_h<-paste0(unlist(row[,col_r4[1]]),"")
      variable<-input_df[variable_a]==variable_b & input_df[variable_c]==variable_d & input_df[variable_e]==variable_f & input_df[variable_g]==variable_h
    }else if(length(reference_f)==5){
      col_r<-unlist(strsplit(reference_f[1],"=="))
      variable_a<-col_r[2]
      variable_b<-paste0(unlist(row[,col_r[1]]),"")
      col_r2<-unlist(strsplit(reference_f[2],"=="))
      variable_c<-col_r2[2]
      variable_d<-paste0(unlist(row[,col_r2[1]]),"")
      col_r3<-unlist(strsplit(reference_f[3],"=="))
      variable_e<-col_r3[2]
      variable_f<-paste0(unlist(row[,col_r3[1]]),"")
      col_r4<-unlist(strsplit(reference_f[4],"=="))
      variable_g<-col_r4[2]
      variable_h<-paste0(unlist(row[,col_r4[1]]),"")
      col_r5<-unlist(strsplit(reference_f[5],"=="))
      variable_i<-col_r5[2]
      variable_j<-paste0(unlist(row[,col_r5[1]]),"")
      variable<-input_df[variable_a]==variable_b & input_df[variable_c]==variable_d & input_df[variable_e]==variable_f & input_df[variable_g]==variable_h & input_df[variable_i]==variable_j
    }    
    return(variable)
  }
  
  observeEvent(input$b_fix, {
    # Show a modal when the button is pressed
    #print("fix")
    var_list<<-var_list+1
    #print(var_list)
    condition_v<-NULL
    fix_v<-NULL
    reference_v<-NULL
    ruletype_v<-NULL
    rule_file<-NULL
    df<-NULL
    for(fila in 1:nrow(vals$rule_df)) {
      row <- vals$rule_df[fila,]
      condition_v[fila]=row['condition']
      fix_v[fila]=row['fix']
      reference_v[fila]=row['reference']
      ruletype_v[fila]=as.character(row[['type']])
      rule_file[fila]=as.character(row[['file']])
    }
    #print(paste0("condicion: ",condition_v))
    #print(paste0("tipo de regla: ",ruletype_v))
    data_f_list<-NULL
    data_f_list=as.data.frame(my.list[1])
    #print(data_f_list)
    for(fila in 1:nrow(data_f_list)) {
      row <- data_f_list[fila,]
      #print(row)
      for(fila_c in 1:length(condition_v)){
        delta<-NULL
        c=as.character(condition_v[[fila_c]])
        delta<-eval_condition(row,c)
        if(delta==TRUE){  
          c=as.character(fix_v[[fila_c]])
          cond_f=unlist(strsplit(c, "[&]")) 
          t=as.character(ruletype_v[fila_c])
          if (t=='substitution'){
            for(fila_cf in 1:length(cond_f)){
              col_f=unlist(strsplit(cond_f[fila_cf],"<-"))
              row[,col_f[1]]<-col_f[2]
            }
          }
          else if (t=='reference'){
            rule_file_reference=rule_file[[fila_c]]
            input_df<<-NULL
            input_df<<-my.list.reference[[rule_file_reference]]
            c=as.character(reference_v[[fila_c]])
            variable<-eval_reference(c,input_df,row)
            for(row_f in 1:length(cond_f)){
              col_f<-unlist(strsplit(cond_f[row_f],"<-"))
              variable_c<-col_f[2]
              #             alfa<-as.character(unique(input_df[variable_c][eval(parse(text=variable[1]))]))
              alfa<-input_df[variable_c][variable]
              #print(alfa)
              if(length(alfa)>0){
                if(length(alfa)>1){alfa=paste(alfa, collapse = "|")}
                row[,col_f[1]]<-alfa
              }
            }
          }
          else {
            #print("another rule type")
          }
        }
      }
      if(fila==1){df<-data.frame(row)}
      else{
        df <- rbind(df, row)
      }
    }
    
    #print(df)
    my.list[[var_list]]<<-df
    
    output$resultTable <- renderTable({
      shinyjs::enable("downloadData")
      shinyjs::enable("six_b")
      my.list[[var_list]]
    })
    
  })
  
  write_json <- function(df, path, df_type = "rows", raw_type = "mongo"){
    df %>% 
      toJSON(dataframe = df_type, raw = raw_type) %>%
      write_lines(path)
    df
  }
  
  output$downloadRules <- downloadHandler(
    filename = function() {
      file_name_download=paste0("Rules_",format(Sys.time(), "_%Y-%M-%d-%X"))
      paste(file_name_download,".csv")
    },
    content = function(file) {
      write.csv(vals$rule_df, file, row.names = FALSE)
    }  
  )
  
  output$downloadData <- downloadHandler(
    filename = function() {
      ptn <- "\\.[[:alnum:]]{1,5}$"
      suf <- tolower(regmatches(file_name[[1]], regexpr(ptn, file_name[[1]])))
      file_name_download<<-sub(suf,"",file_name[[1]])
      suf <- file_type_download
      file_name_download=paste0(file_name_download,format(Sys.time(), "_%Y-%M-%d-%X"),suf)
    },
    content = function(file) {
      ptn <- "\\.[[:alnum:]]{1,5}$"
      #suf <- tolower(regmatches(file, regexpr(ptn, file)))
      suf <- file_type_download
      if(suf==".csv"){
        write.csv(my.list[[var_list]], file, row.names = FALSE)
      }else if(suf==".txt"){
        write.table(my.list[[var_list]], file, sep=",", row.names=FALSE)
      }else if(suf %in% c('.xls', '.xlsx')){
        write.xlsx(my.list[[var_list]], file, sheetName="Result",row.names=FALSE)
      }else if(suf %in% c('.json')){
        write_json(my.list[[var_list]], file)
        #toJSON(unname(split(my.list[[var_list]], 1:nrow(my.list[[var_list]]))))
      }else if(suf %in% c('.xml')){
        write.xml(my.list[[var_list]], file)
      }else if(suf %in% c('.html')){
        write_tableHTML(tableHTML(my.list[[var_list]]), file, complete_html = TRUE)
      }
    }  
  )
  
}
