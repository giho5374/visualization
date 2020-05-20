library(shiny)
library(ggplot2)
library(dplyr)
library(reshape2)
library(DT)
library(shinydashboard)
ui <- dashboardPage(skin="yellow",
  dashboardHeader(title = "Basic dashboard",
                  dropdownMenu(type = "messages",
                               messageItem(from = "박현준", message = "60132582"),
                               messageItem(from = "박형빈", message = "60122847"),
                               messageItem(from = "이기호",  message = "60132599"),
                               messageItem(from = "이용균", message = "60132609"),
                               messageItem(from = "전희수", message = "60132617",icon=icon("crown"))
                  )),
  dashboardSidebar(
    sidebarMenu(
      fileInput("file1", "Choose CSV File",
                accept = c(".txt", ".csv")),
      menuItem("View File", tabName = "file", icon = icon("th")),
      
      menuItem("Plots", icon = icon("bar-chart-o"),
               menuSubItem("Histogram", tabName = "histogram"),
               menuSubItem("Scatter Plot", tabName = "scatter"),
               menuSubItem("Box Plot", tabName = "box"),
               menuSubItem("Bar Chart", tabName = "bar")
      ),
      menuItem("Dplyr", icon = icon("bar-chart-o"),
               menuSubItem("Select", tabName = "inputselect"),
               menuSubItem("Filter", tabName = "inputfilter"),
               menuSubItem("Arrange", tabName = "inputarrange"),
               menuSubItem("Summary", tabName = "inputsummary"),
               menuSubItem("Mutate", tabName = "inputmutate")
      ),
      menuItem("Reshape Data", icon = icon("bar-chart-o"),
               menuSubItem("Melt Data", tabName = "meltdata"),
               menuSubItem("Dcast Data", tabName = "dcastdata")
      ),
      menuItem("Regression analysis", tabName = "model1", icon = icon("bar-chart-o"))
     
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName="file",
              fluidRow(
                box(tableOutput("contents1"))
              )),
      tabItem(tabName = "histogram",
              sidebarPanel(
                selectInput("select", label = h3("X-axis"),  choices = NULL),
                sliderInput("bins","Number of bins:",min = 1,max = 50,value = 30),
                radioButtons('color', 'Select Color',choices = c("blue", "red","dark green"),'blue'),
                checkboxInput(inputId = "check","View Density",FALSE)
              ),
              mainPanel(
                uiOutput("contents2"),
                verbatimTextOutput("sum1")
              )
              ),
      tabItem(tabName = "scatter",
              tabPanel(title="Scatter Plot",
                       sidebarLayout(
                         sidebarPanel(
                           selectInput("selX","X-axis",choices = NULL),
                           selectInput("selY","Y-axis",choices = NULL)
                         ),
                         mainPanel(
                           uiOutput("contents3")
                         )
                       )
              )
              ),
    tabItem(tabName = "box",
            tabPanel(title="Box Plot",
                     sidebarLayout(
                       sidebarPanel(
                         selectInput("selX1","X-axis",choices = NULL),
                         selectInput("selY1","Y-axis",choices = NULL)
                       ),
                       mainPanel(
                         uiOutput("contents4")
                       )
                     )
            )),
    tabItem(tabName = "bar",
            title="Bar Chart",
            sidebarLayout(
              sidebarPanel(
                selectInput("selX2","X-axis",choices = NULL),
                selectInput("selY2","Y-axis",choices = NULL),
                radioButtons("bar1","Choose Type",c("Stack","Dodge","Identity","Fill"))
              ),
              mainPanel(
                uiOutput("contents5")
              )
            )
            ),
    tabItem(tabName = "meltdata",
            tabPanel(title = "melt Data",
                     sidebarLayout(
                       sidebarPanel(
                         checkboxGroupInput("selectJ", label = "Input File", choices = NULL),width=2),
                       mainPanel(DT::dataTableOutput("contents2J"))
                     )
            )  
            ),
    tabItem(tabName = "dcastdata",
            title="Dcast Data",
            sidebarLayout(
              sidebarPanel(
                checkboxGroupInput("row", "Row variable", choices = NULL),width=2,
                selectInput("col", "Column variable", NULL),
                selectInput("var", "Value variable", NULL)),
              mainPanel(DT::dataTableOutput("contents3J"))
            )
    ),
    tabItem(tabName = "model1",
            title="Dcast Data",
            sidebarLayout(
              sidebarPanel(selectInput("select1S", label = "Independent variable", choices = NULL),
                           selectInput("select2S", label = "Dependent variable", choices = NULL),width=2),
              mainPanel(plotOutput("plot1"),
                        verbatimTextOutput("modelsummary1"))
            )
    ),
    tabItem(tabName = "inputselect",
            title="Select",
            sidebarLayout(
              sidebarPanel(
                selectInput("inputselect", label = "열람하고 싶은 변수를 선택하세요.", choices = NULL),width=2),
              mainPanel(
                uiOutput("contentsselect"))
            )),
    tabItem(tabName = "inputfilter",
            title="Filter",
            sidebarLayout(
              sidebarPanel(
                selectInput("inputvarfilter", label = "필터를 적용할 변수를 선택하세요.", choices = NULL),
                textInput("inputfindfilter", label = "위 변수에서 찾고 싶은 수치를 입력하세요.", value = NULL),width=2
              ),
              mainPanel(
                uiOutput("contentsfilter"))
            )),
    tabItem(tabName = "inputarrange",
            title="Arrange",
            sidebarLayout(
              sidebarPanel(
                selectInput("inputvararrange", label = "정렬하고 싶은 변수를 선택하세요.", choices = NULL),
                radioButtons("torf", label = "오름차순/내림차순 여부를 선택하세요.", choices = c("오름차순", "내림차순")),width=2
              ),
              mainPanel(
                uiOutput("contentsarrange"))
            )),
    tabItem(tabName = "inputsummary",
            title="Summary",
            sidebarLayout(
              sidebarPanel(
                selectInput("inputvarsummary", label = "총계를 내고 싶은 변수를 선택하세요.", choices = NULL),width=2
              ),
              mainPanel(
                uiOutput("contentsummary")
              )
            )),
    tabItem(tabName = "inputmutate",
            title="Mutate",
            sidebarLayout(
              sidebarPanel(
                selectInput("inputvarmutate1", label = "파생변수의 재료가 될 첫째 변수를 선택하세요.", choices = NULL),
                selectInput("inputvarmutate2", label = "파생변수의 재료가 될 둘째 변수를 선택하세요.", choices = NULL),
                radioButtons("calc", label = "두 변수 사이에 이루어질 사칙연산을 선택하세요", choices = c("+","-","*","÷"))
                ,width=2),
              mainPanel(
                uiOutput("contentsmutate")
              )
            ))
      
    )
  )
)

server <- function(input, output,session) {
  data <- reactive({
    
    inFile <- input$file1 
    if (is.null(inFile))
      return(NULL)
    
    df<-read.csv(inFile$datapath)
    updateCheckboxGroupInput(session, "selectJ", label = "Select Variable", choices = names(df),selected=names(df))
    updateCheckboxGroupInput(session, "row", label ="Select Variable", choices = names(df),selected=names(df))
    updateSelectInput(session, "col", label ="Select Variable", choices = names(df))
    updateSelectInput(session, "var", label ="Select Variable", choices = names(df))
    updateSelectInput(session, "select", label = "selected", choices = names(df))
    updateSelectInput(session, "selX", label = "selected", choices = names(df))
    updateSelectInput(session, "selY", label = "selected", choices = names(df))
    updateSelectInput(session, "selX1", label = "selected", choices = names(df))
    updateSelectInput(session, "selY1", label = "selected", choices = names(df))
    updateSelectInput(session, "selX2", label = "selected", choices = names(df))
    updateSelectInput(session, "selY2", label = "selected", choices = names(df))
    updateSelectInput(session, "selX3", label = "selected", choices = names(df))
    updateSelectInput(session, "select1S", label = "selected", choices = names(df))
    updateSelectInput(session, "select2S", label = "selected", choices = names(df))
    updateSelectInput(session, "inputselect", label = "selected", choices = names(df))
    updateSelectInput(session, "inputvarfilter", label = "selected", choices = names(df))
    updateSelectInput(session, "inputvararrange", label = "selected", choices = names(df))
    updateSelectInput(session, "inputvarsummary", label = "selected", choices = names(df))
    updateSelectInput(session, "inputvarmutate1", label = "selected", choices = names(df))
    updateSelectInput(session, "inputvarmutate2", label = "selected", choices = names(df))
     return(df)
  })
  output$contents1 <- renderTable({
    data()
  })
  output$meltdata <- renderTable({
    xJ<-data()[input$selectJ]
    meltdata <-melt(data(),id=names(xJ))
  })
  output$dcastdata <- renderTable({
    
    formula <- as.formula(paste(paste(input$row, collapse = "+"), "~", input$col))
    print(formula)
    
    z<-data()[input$var]
    dcastdata<-dcast(data(),formula, value.var = names(z))  
  })
  output$contents2J <- DT::renderDataTable({
    if(is.null(data()))
      h5("No available data yet.")
    else
      xJ<-data()[input$selectJ]
    meltdata <-melt(data(),id=names(xJ))
    datatable(meltdata, options = list(pageLength = 10))
  })
  output$contents3J <- DT::renderDataTable({
    if(is.null(data()))
      h5("No available data yet.")
    else
      formula <- as.formula(paste(paste(input$row, collapse = "+"), "~", input$col))
    print(formula)
    
    z<-data()[input$var]
    dcastdata<-dcast(data(),formula, value.var = names(z))
    datatable(dcastdata, options = list(pageLength = 10))  
  })
  output$contents2<-renderTable({
    data()[input$select]
  })
  output$contents3<-renderTable({
    data()[input$selX]
  })
  output$contents3<-renderTable({
    data()[input$selY]
  })
  output$histogram <- renderPlot({
    x<-data()[, input$select]
    hist(x,breaks=input$bins,col=input$color,border='white')
  })
  output$scatter<-renderPlot({
    ggplot(data = data() ) + geom_point(mapping = aes_string(x = input$selX,y=input$selY))+
      geom_smooth(mapping = aes_string(x = input$selX, y = input$selY),se=FALSE)
    
    
  })
  output$box<-renderPlot({
    ggplot(data = data(), mapping = aes_string(x =input$selX1, y = input$selY1)) + geom_boxplot()
    
  })
  output$bar<-renderPlot({
    ggplot(data = data(),aes_string(x=input$selX2)) + geom_bar(mapping=aes_string(x=input$selX2, fill = input$selX2))
  })
  output$bar1<-renderPlot({
    if(input$bar1=="Stack")
      ggplot(data = data(), aes_string(x = input$selX2, fill = input$selY2)) + geom_bar(position = "stack")
    else if(input$bar1=="Dodge")
      ggplot(data = data(), aes_string(x = input$selX2, fill = input$selY2)) + geom_bar(position = "dodge")
    else if(input$bar1=="Identity")
      ggplot(data = data(), aes_string(x = input$selX2, fill = input$selY2)) + geom_bar(alpha = 2/5, position = "identity") 
    else
      ggplot(data = data(), aes_string(x = input$selX2, fill = input$selY2)) + geom_bar(position = "fill")
    
  })
  output$den<-renderPlot({
    ggplot(data=data()) + geom_freqpoly(mapping=aes_string(x=input$select))
  })
  output$contents2 <- renderUI({
    if(is.null(data()))
      h5("No available data yet.")
    if(input$check==T)
      tabsetPanel(tabPanel("Density", plotOutput("den")))
    else
      tabsetPanel(tabPanel("Histogram", plotOutput("histogram")))
  })
  output$contents3<-renderUI({
    if(is.null(data()))
      h5("No available data yet.")
    else
      tabsetPanel(tabPanel("Scatter Plot", plotOutput("scatter")))
  })
  output$contents4<-renderUI({
    if(is.null(data()))
      h5("No availabe data yet.")
    else
      tabsetPanel(tabPanel("Box Plot",plotOutput(("box"))))
  })
  output$contents5<-renderUI({
    if(is.null(data()))
      h5("No availabe data yet.")
    else
      tabsetPanel(tabPanel("Bar Plot(Only X)",plotOutput(("bar"))),
                  tabPanel("Bar Plot2",plotOutput(("bar1"))))
  })
  model1 <- reactive({
    
    lm(reformulate(input$select1S, input$select2S),data=data())
    
  })
  output$contents1S <- renderTable({
    data()
    
  })
  output$plot1 <- renderPlot({
    
    ggplot(data=data(), mapping=aes_string(x=input$select1S,y=input$select2S))+
      geom_smooth(method=lm, se=FALSE) +
      geom_point()
    
    
  })
  output$modelsummary1 <- renderPrint({
    
    summary(model1())
    
  })
  output$sum1 <- renderPrint({
    zz<-data()[,input$select]
    summary(zz)
    
  })
  output$contentsselect <- renderTable({
    select_(data(), input$inputselect)
  })
  
  output$contentsfilter <- renderTable({
    filter(data(), !!(sym(input$inputvarfilter)) == input$inputfindfilter)
  })
  
  output$contentsarrange <- renderTable({
    if(input$torf == "오름차순"){arrange(data(), !!(sym(input$inputvararrange)))}
    else if(input$torf == "내림차순"){arrange(data(), desc(!!(sym(input$inputvararrange))))}
    else (print("정렬 여부를 선택해 주세요."))
  })
  
  output$contentsummary <- renderTable({
    summarise(data(), mean = mean(!!(sym(input$inputvarsummary))),
              median = median(!!(sym(input$inputvarsummary))),
              total = sum(!!(sym(input$inputvarsummary))),
              min = min(!!(sym(input$inputvarsummary))),
              max = max(!!(sym(input$inputvarsummary)))
    )
  })
  
  output$contentsmutate <- renderTable({
    if (input$calc == "+"){
      mutate(data(), sum = !!(sym(input$inputvarmutate1)) + !!(sym(input$inputvarmutate2)))
    } else if (input$calc == "-"){
      mutate(data(), difference = !!(sym(input$inputvarmutate1)) - !!(sym(input$inputvarmutate2)))
    } else if (input$calc == "*"){
      mutate(data(), multiplied = !!(sym(input$inputvarmutate1)) * !!(sym(input$inputvarmutate2)))
    } else if (input$calc == "÷"){
      mutate(data(), divided = !!(sym(input$inputvarmutate1)) / !!(sym(input$inputvarmutate2)))
    } else (print("사칙연산자를 선택하세요."))
    
  })
  
    }

shinyApp(ui, server)
