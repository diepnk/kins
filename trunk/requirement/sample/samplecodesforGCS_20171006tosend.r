
library(shiny)
library(shinythemes)
library(shinydashboard)
require(RODBC)
library(MASS)
library(data.table)
require(DT)
library(ggplot2)
setwd("D:/KondoWork/RWebApp/GCSDevelopment/")

# dbName<-"******"
# cString<-odbcConnect(dbName)
# # 
# variables<-"*"
# sql_learn<-paste("select", variables, " from [***].[dbo].[****]")
# df_learn_source<- na.omit(sqlQuery(cString, sql_learn))
# df_learn<-df_learn_source 


 df_learn<-read.csv("dummydata.csv", sep = ",")#tantative
 
 basedata <- as.data.frame(lapply(df_learn, mean))
 basedata$sex = 1
 basedata[21:180]<-0


#################
#sample
recalc.risk<-function(data)
{
  Diabetes<-1/(1 + rnorm(1,mean = 1, sd = 0.5))
  Stroke<-1/(1 + rnorm(1,mean = 1, sd = 0.5))
  HeartAtack<-1/(1 + rnorm(1,mean = 1, sd = 0.5))
  pred<-data.frame(Diabetes, Stroke, HeartAtack)
  return(pred)
}
#sample
 recalc.days<-function(data)
 {
   rnd1<-rnorm(1,mean = 2, sd = 1)
   x.range<-seq(30,80)
   pred<-data.frame(val = rnd1*x.range*x.range/100)
   names(pred)<-"Pred Admission days"
   # plot(x.range, y.youin)
   return(pred)
 }


do.judgement<-function(dataset, val)
{
  val.ratio<-subset(dataset, dataset[,1] == val)[,4]
  if ( val.ratio >= 5) judgement = 1 
  else if( val.ratio >= 2) judgement = 2
  else if(val.ratio >= 1) judgement = 3
  else judgement = 4
  return(c(val.ratio,judgement))
}

disp.color<-c("red", "yellow", "aqua", "green")
disp.judge<-c("Refusal","Substandard","Substandard","Standard")
disp.premium<-c(0,1,1,0)


ui <- dashboardPage(
  dashboardHeader(
    title = "dashboard prototype", titleWidth = 600),
    skin = "blue",


  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard"),
      menuItem("Raw data", tabName = "rawdata")
    ),

    
  #add a slider input
  numericInput("age", "Age", value = ceiling(basedata$age), min = 30, max = 80),
  sliderInput("bmi", "BMI", min = 15, max = 40, value = ceiling(basedata$BMI), step = 1, animate = T),
  sliderInput("bp_h", "Blood Pressure(H)", 80, 150, ceiling(basedata$BP_H), step = 10, animate = T),
  sliderInput("bp_l", "Blood Pressure(L)", 50, 100, ceiling(basedata$BP_L), step = 10, animate = T),
  sliderInput("hba1c", "HbA1c", 4, 10, ceiling(basedata$HbA1c_HOKAN), step = 0.2, animate = T),
  
  # numericInput("age", "Age", 40, 30, 80),
  # sliderInput("bmi", "BMI", 15, 40, 5, step = 1, animate = T),
  # sliderInput("bp", "Blood Pressure(H)", 80, 150, 5, step = 10, animate = T),
  # sliderInput("hba1c", "HbA1c", 4, 10, 5, step = 0.2, animate = T),
  #add a checkbox input
  checkboxGroupInput('selected_youin', 
                     'labels:',
                     c(names(df_learn)[21:28], names(df_learn[35:37])))
  ),

        # Show a table summarizing the values entered
  dashboardBody(
    # followings are the codes to modify color of header, sideber and tabmenu.
    # tags$head(tags$style(HTML('
    #           /* logo */
    #           .skin-blue .main-header .logo {
    #           background-color: #f4b943;
    #           }
    #           
    #           /* logo when hovered */
    #           .skin-blue .main-header .logo:hover {
    #           background-color: #f4b943;
    #           }
    #           
    #           /* navbar (rest of the header) */
    #           .skin-blue .main-header .navbar {
    #           background-color: #f4b943;
    #           }        
    #           
    #           /* main sidebar */
    #           .skin-blue .main-sidebar {
    #           background-color: #f4b943;
    #           }
    #           
    #           /* active selected tab in the sidebarmenu */
    #           .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
    #           background-color: #ff0000;
    #           }
    #           
    #           /* other links in the sidebarmenu */
    #           .skin-blue .main-sidebar .sidebar .sidebar-menu a{
    #           background-color: #00ff00;
    #           color: #000000;
    #           }
    #           
    #           /* other links in the sidebarmenu when hovered */
    #           .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
    #           background-color: #ff69b4;
    #           }
    #           /* toggle button when hovered  */                    
    #           .skin-blue .main-header .navbar .sidebar-toggle:hover{
    #           background-color: #ff69b4;
    #           }
    #           '))),
    # 

    tabItems(
      tabItem("dashboard",
        fluidRow(
        valueBoxOutput("ratio"), valueBoxOutput("recommend"), valueBoxOutput("premium")),
        
        ##pattern1
        # plotOutput("distPlot"),
        # plotOutput("riskPlot"),
        # 
        #pattern2
        fluidRow(
          column(7, plotOutput("distPlot", width = "100%", height = "600px")),
          column(5, plotOutput("riskPlot", width = "100%", height = "600px"))
        ),
        # example of webGL usage
        # fluidRow(
        #   webGLOutput("gl")
        # ),
        verbatimTextOutput("comment")
      ),
      tabItem("rawdata",

              dataTableOutput("view"),
              # tableOutput("view2"),
              actionButton("Save","Save as .csv"),
              verbatimTextOutput("saved")
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- shinyServer(function(input, output) {

# 
#   sliderValues <- reactive({
#   # Compose data frame
#     data.frame(
#       Name = "plotitems",
#       Value = input$plotitems,
#       stringsAsFactors=FALSE)
#   })
#   
  output$checkboxOut<-renderPrint({ 
    names(basedata[input$selected_youin])
  })
# 
#   InitialParam<- reactive({
#     basedata
#   })
  
  updatedParam <- reactive({
    updatedParam<-basedata
    #use input values
    updatedParam$age<-input$age
    updatedParam$BMI<-input$bmi
    updatedParam$BP_H<-input$bp_h
    updatedParam$BP_L<-input$bp_l
    updatedParam$HbA1c_HOKAN<-input$hba1c
    updatedParam[input$selected_youin]<-1
    data.frame(updatedParam)
    
  })
  
  
  #define datasetInput and load data
  datasetInput <- reactive({
    
    b<-basedata[setdiff(colnames(basedata), "age")] 
    age<-seq(30,80)
    df.data<-cbind(age, b)
    
    # df.data$age<-input$age
    df.data[input$selected_youin]<-0
    df.preds.zero<-recalc.risk(df.data)
    df.pred.days.zero<-recalc.days(df.preds.zero)
    names(df.pred.days.zero)<-"preds.hyoujun"

    #use input values
    df.data$BMI<-input$bmi
    df.data$BP_H<-input$bp_h
    df.data$BP_L<-input$bp_l
    df.data$HbA1c_HOKAN<-input$hba1c
    
    
    df.data[input$selected_youin]<-1
    df.preds.nonzero<-recalc.risk(df.data)
    df.pred.days.nonzero<-recalc.days(df.preds.nonzero)
    names(df.pred.days.nonzero)<-"preds.youin"
    
    data.frame(age = age,
               hyoujun = df.pred.days.zero[,1],
               youin = df.pred.days.nonzero[,1],
               ratio = df.pred.days.nonzero[,1]/df.pred.days.zero[,1]
    )
  })
  
  datasetRiskProfile <- reactive({
    b<-basedata[setdiff(colnames(basedata), "age")] 
    age<-seq(30,80)
    df.data<-cbind(age, b)
    #use input values
    df.data$BMI<-input$bmi
    df.data$BP_H<-input$bp_h
    df.data$BP_L<-input$bp_l
    df.data$HbA1c_HOKAN<-input$hba1c
    
    
    df.data[input$selected_youin]<-1
    df.preds.nonzero<-data.frame(age = age, recalc.risk(df.data))
    df.preds.age<-subset(df.preds.nonzero,df.preds.nonzero$age == input$age)
    df.plot<-df.preds.age[setdiff(colnames(df.preds.age), "age")]
    sum.risk<-sum(df.plot)
    
    df.plot/sum.risk
    
  })
  
  
  output$distPlot <- renderPlot({
    df.hyoujun<-data.frame(age = datasetInput()[,1], preds = datasetInput()[,2])
    df.hyoujun<-cbind(df.hyoujun, class = "hyoujun")
    df.youin<-data.frame(age = datasetInput()[,1], preds = datasetInput()[,3])
    df.youin<-cbind(df.youin, class = "youin")
    df.plot<-rbind(df.hyoujun, df.youin)
    #plot predicted days
     ggplot(df.plot, aes(x = age, y = preds, fill = class, colour = class)) + geom_line(size = 1.2) + labs(title = "predicted days") +
       theme(legend.position=c(0.2,0.8), title = element_text(size = 15), axis.text.x = element_text(size=10), axis.text.y = element_text(size=15))

    #plot ratio
    # ggplot(datasetInput(), aes(x = age, y = ratio, color = "red")) + geom_line(size = 1.2) + labs(title = "Risk Ratio") +
    #   theme(legend.position= "none", title = element_text(size = 15), axis.text.x = element_text(size=10), axis.text.y = element_text(size=15))
    # 
    
  })
  
  
  output$view<-renderDataTable({
    datasetInput()
    },options = list(orderClasses = TRUE))
  
  output$saved<-renderPrint({
    input$Save
    write.table(datasetInput(), "testdata.csv")
    # dff<-as.data.frame(datasetInput)
    print(datasetInput()$hyoujun[1])
  })

  output$ratio <- renderValueBox({ 
    val<-do.judgement(datasetInput(), input$age)
    valueBox( 
      value = formatC(val[1], digits = 3, format = "f"), 
      subtitle = "risk ratio", 
      icon = icon("area-chart"), 
      color =  disp.color[val[2]]
    ) 
  })
  
  output$recommend <- renderValueBox({
    val<-do.judgement(datasetInput(), input$age)
    valueBox( 
      value = disp.judge[val[2]],
      subtitle = "Comments", 
      icon = icon("gears"), 
      color =  "black"
    ) 
  })
  
  
  output$premium <- renderValueBox({
    fee = 1000
    val<-do.judgement(datasetInput(), input$age)
    valueBox( 
      value = paste("+", as.character(ceiling(fee * val[1] * disp.premium[val[2]])), "YEN"),
      subtitle = "Add on premium", 
      icon = icon("gears"), 
      color =  "orange"
    ) 
  })
  
  output$riskPlot <- renderPlot({
    name.class<-names(datasetRiskProfile())
    name.class<-gsub("Admission_", "", name.class)
    name.class<-gsub("_After", "", name.class)
    tdf<-data.frame(value = t(datasetRiskProfile()), class = name.class)
    names(tdf)<-c("value", "class")

    ggplot(tdf, aes(x = class, y = value, fill = class)) + geom_bar(stat = "identity") + labs(title = "Risk profile of Admission", size = 20) + xlab(NULL) + ylab("Risk value") + coord_flip() + 
      theme(legend.position="none", title = element_text(size = 15), axis.text.x = element_text(size=10),
            axis.text.y = element_text(size=15)) + coord_polar()
    # panel.background = element_rect(fill = "transparent",color = NA),panel.grid.minor = element_line(color = NA), 
    # panel.grid.major = element_line(color = NA), plot.background = element_rect(fill = "transparent",color = NA), 
    
   })
  
  output$comment <- renderPrint({
    val<-do.judgement(datasetInput(), input$age)
    if(val[2] == 4)
    {
      txt<- "echo > No significant risk is observed."
    }else{
      
            diff<-updatedParam() - basedata
      diff.nonzero<-subset(t(diff), t(diff) != 0)
      
      res.day<-data.frame()
      for(i in 1:nrow(diff.nonzero))
      {
        diff.zero<-diff * 0
        name.item<-rownames(diff.nonzero)[i]
        diff.zero[name.item]<-diff.nonzero[i]
        pred.risk<-recalc.risk(diff.zero)
        pred.day<-recalc.days(pred.risk)
        res.day<-rbind(res.day, data.frame(variable = name.item, pred.day = pred.day))
      }
      idx<-order(res.day$Pred.Admission.days, decreasing = T)
      top.effect<-res.day$variable[idx[1]]
      txt<- paste("echo > ", top.effect , " is the major negative effect on future admission risk")
    }
    print(txt)
  })
  
  # example of webGL usage
  # output$gl <- renderWebGL({
  #   points3d(1:10,1:10,1:10)
  # })
  
})

# Run the application
shinyApp(ui = ui, server = server)