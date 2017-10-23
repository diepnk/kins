library(shiny)
library(shinythemes)
library(shinydashboard)
require(RODBC)
library(MASS)
library(data.table)
require(DT)
library(ggplot2)
#setwd("D:/Project/TS/KINS/GIT/deliverables/trunk/implement/source/simulator/src/main/R")

#Get data
{
cString<-odbcDriverConnect('driver={SQL Server};server=KHANGDOAN3383\\SQLEXPRESS;database=kins;uid=sa; pwd=admin')
variables<-"*"
sql_learn<-paste("select", variables, " from [dbo].[test]")#sql
df_learn_source<- na.omit(sqlQuery(cString, sql_learn))
df_learn<-df_learn_source
basedata <- as.data.frame(lapply(df_learn, mean))
basedata$sex = 1
basedata[21:180]<-0

#Threshold for sliderbars and global varialbe
threshold <- read.csv(file="Threshold.csv", header=TRUE, sep=",")

maxHistogram <- 3
}

#Recalculate
{
recalc.risk<-function(data)
{
  Diabetes<-1/(1 + rnorm(1,mean = 1, sd = 0.5))
  Stroke<-1/(1 + rnorm(1,mean = 1, sd = 0.5))
  HeartAtack<-1/(1 + rnorm(1,mean = 1, sd = 0.5))
  pred<-data.frame(Diabetes, Stroke, HeartAtack)
  return(pred)
}

recalc.days<-function(data)
{
  rnd1<-rnorm(1,mean = 2, sd = 1)
  x.range<-seq(30,80)
  pred<-data.frame(val = rnd1*x.range*x.range/100)
  names(pred)<-"Pred Admission days"
  return(pred)
}
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

#Color definition
{
disp.color<-c("red", "yellow", "aqua", "green")
disp.judge<-c("Refusal","Substandard","Substandard","Standard")
disp.premium<-c(0,1,1,0)
}

#UI
{
ui <- dashboardPage(
  dashboardHeader(title = "dashboard prototype"),
    
  dashboardSidebar(
    sidebarMenu(id = "mytabs",
      menuItem("Dashboard", tabName = "dashboard"),
      menuItem("Raw data", tabName = "rawdata"),
      menuItem("Histogram & Heatmap", tabName = "histogram")
    ),
#######################################################################################################################
#DASHBOARD TAB 

  #add a slider input for Dashboard
  conditionalPanel(condition = "input.mytabs == 'dashboard'",
    #AGE
    numericInput("age", "Age", value = ceiling(basedata$age), min = 30, max = 80),
    #BMI
    sliderInput("bmi", "BMI", min = 15, max = 40, value = ceiling(basedata$BMI), step = 1),
    conditionalPanel(condition = paste("input.bmi>",as.character(threshold["bmi_hight"])),
      fluidRow(column(12,align="center", div(style = "height:18px;background-color: red" ,"BMI IN HIGHT RANGE")))),
    conditionalPanel(condition = paste("input.bmi<=",as.character(threshold["bmi_hight"]) ,"& input.bmi>=",as.character(threshold["bmi_low"])),
      fluidRow(column(12,align="center",  div(style = "height:18px;background-color: blue","BMI IN NORMAL RANGE")))),
    conditionalPanel(condition = paste("input.bmi<",as.character(threshold["bmi_low"])),
      fluidRow(column(12,align="center",  div(style = "height:18px;background-color: red","BMI IN LOW RANGE")))),
    #BP_H
    sliderInput("bp_h", "Blood Pressure(H)", 80, 150, ceiling(basedata$BP_H), step = 10),
    conditionalPanel(condition = paste("input.bp_h>",as.character(threshold["bp_h_hight"])),
      fluidRow(column(12,align="center", div(style = "height:18px;background-color: red" ,"BP_H IN HIGHT RANGE")))),
    conditionalPanel(condition = paste("input.bp_h<=",as.character(threshold["bp_h_hight"]) ,"& input.bp_h>=",as.character(threshold["bp_h_low"])),
      fluidRow(column(12,align="center",  div(style = "height:18px;background-color: blue","BP_H IN NORMAL RANGE")))),
    conditionalPanel(condition = paste("input.bp_h<",as.character(threshold["bp_h_low"])),
      fluidRow(column(12,align="center",  div(style = "height:18px;background-color: red","BP_H IN LOW RANGE")))),
    #BP_L
    sliderInput("bp_l", "Blood Pressure(L)", 50, 100, ceiling(basedata$BP_L), step = 10),
    conditionalPanel(condition = paste("input.bp_l>",as.character(threshold["bp_l_hight"])),
      fluidRow(column(12,align="center", div(style = "height:18px;background-color: red" ,"BP_L IN HIGHT RANGE")))),
    conditionalPanel(condition = paste("input.bp_l<=",as.character(threshold["bp_l_hight"]) ,"& input.bp_l>=",as.character(threshold["bp_l_low"])),
      fluidRow(column(12,align="center",  div(style = "height:18px;background-color: blue","BP_L IN NORMAL RANGE")))),
    conditionalPanel(condition = paste("input.bp_l<",as.character(threshold["bp_l_low"])),
      fluidRow(column(12,align="center",  div(style = "height:18px;background-color: red","BP_L IN LOW RANGE")))),
    #HBA1C
    sliderInput("hba1c", "HbA1c", 4, 10, ceiling(basedata$HbA1c_HOKAN), step = 0.2, animate = T),
    conditionalPanel(condition = paste("input.hba1c>",as.character(threshold["hba1c_hight"])),
      fluidRow(column(12,align="center", div(style = "height:18px;background-color: red" ,"HBA1C IN HIGHT RANGE")))),
    conditionalPanel(condition = paste("input.hba1c<=",as.character(threshold["hba1c_hight"]) ,"& input.hba1c>=",as.character(threshold["hba1c_low"])),
      fluidRow(column(12,align="center",  div(style = "height:18px;background-color: blue","HBA1C IN NORMAL RANGE")))),
    conditionalPanel(condition = paste("input.hba1c<",as.character(threshold["hba1c_low"])),
      fluidRow(column(12,align="center",  div(style = "height:18px;background-color: red","HBA1C IN LOW RANGE")))),
    #Checkbox list
    checkboxGroupInput('selected_youin', 'labels:', c(names(df_learn)[21:28], names(df_learn[35:37])))
    )
    #For Hist
    #conditionalPanel(condition = "input.mytabs == 'histogram'",
      #fluidRow(column(12, div(style = "overflow-x: scroll;height:250px",
        #checkboxGroupInput('forHistogram', 'For histograms', names(df_learn)))))
    #),
    #For Heatmap
    #conditionalPanel(condition = "input.mytabs == 'histogram'",
                 #fluidRow(column(12, div(style = "overflow-x: scroll; height:250px",
                                         #checkboxGroupInput('forHeatmap', 'For heatmap:', names(df_learn))))))

   ),


  #add checkboxGroup for histogram and heatmap
  

#######################################################################################################################
#HISTORGRAM&HEATMAP TAB


  dashboardBody(
    tabItems(
      tabItem("dashboard",
        fluidRow(
        valueBoxOutput("ratio"), valueBoxOutput("recommend"), valueBoxOutput("premium")),
        fluidRow(
          column(7,radioButtons('chartType', 'Chart type:', c("Ratio","Days","3D"),selected = "Ratio",inline = TRUE))
        ),
        fluidRow(
          column(7, plotOutput("distPlot", width = "100%", height = "400px")),
          column(5, plotOutput("riskPlot", width = "100%", height = "400px"))
        ),
        verbatimTextOutput("comment")
      ),
      tabItem("rawdata",
              dataTableOutput("view"),
              actionButton("Save","Save as .csv"),
              verbatimTextOutput("saved")
      ),
      tabItem("histogram",
        #Out of limited message
        conditionalPanel(condition = paste("input.forHistogram.length > ",as.character(maxHistogram)),
        fluidRow(column(12,align="center",  div(style = "height:18px;background-color: red","Just 3 are rendered even there is more than 3")))
        ),
        #
        fluidRow(
          column(3,plotOutput("plot01", width = "100%", height = "200px"),textOutput("meanValue1"),textOutput("maxValue1"),textOutput("minValue1")),
          column(3,plotOutput("plot02", width = "100%", height = "200px")),
          column(3,plotOutput("plot03", width = "100%", height = "200px")),
          #column(2,plotOutput("plot04", width = "100%", height = "200px")),
          column(3,div(style = "height:200px; overflow: scroll; overflow-x: hidden",
                       checkboxGroupInput('forHistogram', 'For histograms', names(df_learn))))
        ),
        fluidRow(align="center","#######################################################################################################"),
        fluidRow(
          column(4,plotOutput("plot05", width = "100%", height = "400px")),
          column(8,plotOutput("plot06", width = "100%", height = "400px"))
        )
       )
      )
    )
  )

}

# Server
{
server <- shinyServer(function(input, output) {
  
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
    # [Diep] Get all columns from db excepts the "age" column
    b<-basedata[setdiff(colnames(basedata), "age")] 
    # [Diep] the "age" will hard-code init by a sequence from 30 to 80 
    age<-seq(30,80)
    # [Diep] re-combine the age into the basedata
    df.data<-cbind(age, b)
    
    # df.data$age<-input$age
    # [Diep] Calucate the risk at the standard data (do not check checkboxes, preds is zero)
    df.data[input$selected_youin]<-0
    df.preds.zero<-recalc.risk(df.data)
    df.pred.days.zero<-recalc.days(df.preds.zero)
    names(df.pred.days.zero)<-"preds.hyoujun"

    # [Diep] Get the real input from the UI
    #use input values
    df.data$BMI<-input$bmi
    df.data$BP_H<-input$bp_h
    df.data$BP_L<-input$bp_l
    df.data$HbA1c_HOKAN<-input$hba1c
    
    
    df.data[input$selected_youin]<-1
    # [Diep] Calucate the risk base on the real input data that get from UI
    df.preds.nonzero<-recalc.risk(df.data)
    df.pred.days.nonzero<-recalc.days(df.preds.nonzero)
    names(df.pred.days.nonzero)<-"preds.youin"
    
    # [Diep] Build a data frame that includes age, risk base on the standard data, risk base on the input data and the ratio between them
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
  
  
  # [Diep] Render Risk Ratio plot
  output$distPlot <- renderPlot({
    df.hyoujun<-data.frame(age = datasetInput()[,1], preds = datasetInput()[,2])
    df.hyoujun<-cbind(df.hyoujun, class = "hyoujun")
    df.youin<-data.frame(age = datasetInput()[,1], preds = datasetInput()[,3])
    df.youin<-cbind(df.youin, class = "youin")
    df.plot<-rbind(df.hyoujun, df.youin)
    #plot predicted days
    if(input$chartType[1] == "Ratio"){
      ggplot(df.plot, aes(x = age, y = preds, fill = class, colour = class)) + geom_line(size = 1.2) + labs(title = "Ratio") +
      theme(legend.position=c(0.2,0.8), title = element_text(size = 15), axis.text.x = element_text(size=10), axis.text.y = element_text(size=15))
    }
    else
    {
      ggplot(df.plot, aes(x = preds, y = age, fill = class, colour = class)) + geom_line(size = 1.2) + labs(title = "Days") +
        theme(legend.position=c(0.2,0.8), title = element_text(size = 15), axis.text.x = element_text(size=10), axis.text.y = element_text(size=15))
    }
  })
  
#######################################################################################################################
#RAWDATA TAB
  
  # [Diep] Render the datatable that list all values
  output$view<-renderDataTable({
    datasetInput()
    },options = list(orderClasses = TRUE))
  
  # [Diep] Export CSV
  output$saved<-renderPrint({
    input$Save
    write.table(datasetInput(), "testdata.csv")
    # dff<-as.data.frame(datasetInput)
    print(datasetInput()$hyoujun[1])
  })
  
#######################################################################################################################
#HISTORGRAM&HEATMAP TAB 
  
  output$riskPlot <- renderPlot({
    name.class<-names(datasetRiskProfile())
    name.class<-gsub("Admission_", "", name.class)
    name.class<-gsub("_After", "", name.class)
    tdf<-data.frame(value = t(datasetRiskProfile()), class = name.class)
    names(tdf)<-c("value", "class")
    ggplot(tdf, aes(x = class, y = value, fill = class)) + geom_bar(stat = "identity") + labs(title = "Risk profile of Admission", size = 20) + xlab(NULL) + ylab("Risk value") + coord_flip() + 
      theme(legend.position="none", title = element_text(size = 15), axis.text.x = element_text(size=10),
            axis.text.y = element_text(size=15)) + coord_polar()
    
   })
  ######### Them vao
  
  
  #tempChart <- renderPlot({
    #temp <- as.numeric(unlist(df_learn["BMI"]))
    #hist(temp)
  #})

observe(
  if(is.null(input$forHistogram)){
    for(x in 1:6){
      tempName <- paste("plot0",as.character(x),sep = "") 
      output[[tempName]] <-renderPlot("")}
  }
  else if(length(input$forHistogram)<5){
    for(x in 1:4){
      tempName <- paste("plot0",as.character(x),sep = "") 
      output[[tempName]] <-renderPlot("")}
    for(y in 1:length(input$forHistogram)){
      local({
      temp <- as.numeric(unlist(df_learn[input$forHistogram[y]]))
      tempChart <- renderPlot(hist(temp))
      tempName <- paste("plot0",as.character(y),sep = "") 
      output[[tempName]] <-tempChart
      output[[tempName]] <-tempChart
      })
    }
  }
)  
  output$meanValue1<-renderText("ABC")
  output$maxValue1<-renderText("ABC")
  output$minValue1<-renderText("ABC")
  
  #for (i in 1:4){
    #tempChart <- renderPlot({
      #temp <- as.numeric(unlist(df_learn["BMI"]))
      #hist(temp)
    #})
    #tempName <- paste("plot0",as.character(i),sep = "") 
    #output[[tempName]] <-tempChart
  #}
  
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
})
}


# Run the application
shinyApp(ui = ui, server = server)