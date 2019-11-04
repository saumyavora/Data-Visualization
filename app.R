library(shiny)
#Defining UI
ui<-fluidPage(
    titlePanel(title="Homework 4"),
    fluidRow(
      column(4,   sliderInput("num",
                              "Choose a number",
                              min = 2,
                              max = 1000,
                              value = 30)),
      column(4,   radioButtons("dist", "Distribution type:",
                               c("Normal" = "norm",
                                 "Uniform" = "unif"
                               ))),
      
      column(4, actionButton(inputId = "btn",label="Update"))
    ),
    sidebarLayout(position = "right",
                  sidebarPanel("Sidebar",
                               textInput("txt1", "Type your name", "Name "),
                               selectInput("side", "what is your graduation year:", choices = c("2019", "2020")
                               )
                  ),
     mainPanel(column(11, 
                      plotOutput("distPlot"),
                      
                      verbatimTextOutput("text")
     ),
    tabsetPanel(type = "tabs",
                tabPanel("Tabset1", verticalLayout("Saumya", 
                                                        verbatimTextOutput("txtout"), 
                                                        "This is the link of the normal distribution", 
                                                        a(href="https://en.wikipedia.org/wiki/Normal_distribution", "Link of normal distribution"),
                                                        "This is example of Split Layout",
                                                        "Random two Plots",
                                                        splitLayout(plotOutput("plot1"),plotOutput("plot2")))),
                tabPanel("Tabset2", splitLayout( fluidPage(
                  titlePanel("Basic widgets"),
                  
    fluidRow(
      column(10,
             h3("Buttons"),
             actionButton("action", "Action1"),
             br(),
             br(), 
             verbatimTextOutput("v1")),
      column(10,
             h3("Single checkbox"),
             checkboxInput("checkbox", "Choice A", value = TRUE),
             verbatimTextOutput("v2")),
      column(10,
             checkboxGroupInput("checkboxgroup",
                                h3("Checkbox group"),
                                choices=list("Choice 1"=1,
                                             "Choice 2"=2,
                                             "Choice 3"=3),
                                selected=1),
             verbatimTextOutput("v3"))),
    
      fluidRow(
      column(10, 
             dateInput("date", 
                       h3("Date input"), 
                       value = "2019-11-02"),
             verbatimTextOutput("v4")),
  
      column(10,
             dateRangeInput("dates", h3("Date range")),
             fluidRow(column(10,verbatimTextOutput("v5")))),
      
      column(10,
             fileInput("file", h3("File input"))),
      ),
      fluidRow(
      column(10, 
             numericInput("nume", 
                          h3("Numeric input"), 
                          value = 1),
             verbatimTextOutput("v6")
             ),
      
      column(10,
             radioButtons("radio", h3("Radio buttons"),
                          choices = list("Choice 1" = 1, "Choice 2" = 2,
                                         "Choice 3" = 3),selected = 1),
             verbatimTextOutput("v7")),
      
      column(10,
             selectInput("select", h3("Select box"), 
                         choices = list("Choice 1" = 1, "Choice 2" = 2,
                                        "Choice 3" = 3), selected = 1),
             verbatimTextOutput("v8")),
      ),
    
      fluidRow(
      column(10, 
             sliderInput("slider1", h3("Slider1"),
                         min = 0, max = 100, value = 50),
             verbatimTextOutput("v9")),
      
      column(10, 
             textInput("text", h3("Text input"), 
                       value = "Enter text..."),
              verbatimTextOutput("v10")),
    
      column(10,
             sliderInput("slider2", h3("Slider2"),
                         min = 0, max = 100, value = c(25, 75)),
             verbatimTextOutput("v11"))
    )))
))
)
))    
#Defining Server
server<-
  function(input,output) {
   vals = eventReactive(input$btn, {
     runif(input$num)
    })
    
    output$distPlot <- renderPlot({
      dist = switch(input$dist,
                    norm = rnorm,
                    unif = runif,
                    rnorm)
      hist(dist(vals()), main="Distribution")
    })
    output$text = renderPrint({
      
      summary(dist(vals()))
    })
    output$plot1 <- renderPlot(plot(cars))
    output$plot2 <- renderPlot(plot(pressure))
    output$txtout <- renderText({paste(input$txt1)})
    
    output$v1=renderPrint({input$action})
    output$v2=renderPrint({input$checkbox})
    output$v3=renderPrint({input$checkboxgroup})
    output$v4=renderPrint({input$date})
    output$v5=renderPrint({input$dates})
    output$v6=renderPrint({input$nume})
    output$v7=renderPrint({input$radio})
    output$v8=renderPrint({input$select})
    output$v9=renderPrint({input$slider1})
    output$v10=renderPrint({input$text})
    output$v11=renderPrint({input$slider2})}


shinyApp(ui = ui, server = server)
  
