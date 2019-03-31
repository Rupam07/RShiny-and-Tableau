##group 3
install.packages("shiny")
library(shiny)
install.packages("tidyverse")
library(tidyverse)
install.packages("ggplot2")
library(ggplot2)
install.packages("dplyr")
library(dplyr)
install.packages("treemap")
library(treemap)
install.packages("plotly")
library(plotly)
install.packages("ggExtra")
library(ggExtra)
install.packages("treemapify")
library(treemapify)

options(scipen=8)

####Data Cleaning####
data_clean <- read.csv(file.choose(), header = TRUE)  ##read file
subset1<- subset(data_clean, select = c(AV_TOTAL,GROSS_TAX,LU,R_BLDG_STYL,R_VIEW,YR_BUILT,ZIPCODE,R_OVRALL_CND) ) ##including only required fields
subset2 <- subset(subset1, LU %in% c("A", "R1","R2","R3","R4"))                                                   ##residential properties only
subset3 <-na_if(subset2,"")                                                                                       ##check for NA's
subset4 <- na.omit(subset3)                                                                                       ##remove NA's
SUBSET5 <-subset(subset4,!YR_BUILT==0)                                                                            ##remove zero values for year build 
data_clean <- SUBSET5                                                                                             ##assigning to main dataset
data_clean$ZIPCODE <- paste0("0", data_clean$ZIPCODE)                                                             ##including leading zero for zipcodes
data_clean$LU <- as.character(data_clean$LU)
##changing names to normal form for LU
data_clean$LU[data_clean$LU == "R3"] <- "Residential 3-family"
data_clean$LU[data_clean$LU == "R2"] <- "Residential 2-family"
data_clean$LU[data_clean$LU == "R1"] <- "Residential 1-family"
data_clean$LU[data_clean$LU == "A"] <- "Residential 7 or more units"
##changing names to normal form for R VIEW
data_clean$R_VIEW <- as.character(data_clean$R_VIEW)
data_clean$R_VIEW[data_clean$R_VIEW == "A"] <- "Average"
data_clean$R_VIEW[data_clean$R_VIEW == "E"] <- "Excellent"
data_clean$R_VIEW[data_clean$R_VIEW == "F"] <- "Fair"
data_clean$R_VIEW[data_clean$R_VIEW == "G"] <- "Good"
data_clean$R_VIEW[data_clean$R_VIEW == "P"] <- "Poor"
data_clean$R_VIEW[data_clean$R_VIEW == "S"] <- "Special"
data_clean$R_OVRALL_CND <- as.character(data_clean$R_OVRALL_CND)
##changing names to normal form for R Overall Condition
data_clean$R_OVRALL_CND[data_clean$R_OVRALL_CND == "A"] <- "Average"
data_clean$R_OVRALL_CND[data_clean$R_OVRALL_CND == "E"] <- "Excellent"
data_clean$R_OVRALL_CND[data_clean$R_OVRALL_CND == "F"] <- "Fair"
data_clean$R_OVRALL_CND[data_clean$R_OVRALL_CND == "G"] <- "Good"
data_clean$R_OVRALL_CND[data_clean$R_OVRALL_CND == "P"] <- "Poor"

###Data Cleaning End ###

##Data Frame for line chart##
median_line <- data_clean%>% group_by(YR_BUILT, ZIPCODE) %>% summarise(median_total = median(AV_TOTAL), median_tax = median(GROSS_TAX))
##choices for input check list
choices_check <- list("Brighton(02135)" = "02135", "Backbay(02116)" = "02116", "North End(02114)" = "02114", "Beacon Hill(02108)" = "02108", "Government Center(02109)" = "02109", "China Town(02111)"= "02111", "Government Center(02113)" = "02113", "South End(02118)" = "02118", "Kenmore(02115)" = "02115", "Roxbury(02119)" = "02119",  "Roxbury(02120)" = "02120", "Dorchester(02121)" = "02121","Dorchester(02122)"= "02122",  "Dorchester(02124)" = "02124", "Dorchester(02125)" = "02125", "Mattapan(02126)" = "02126", "South Boston(02127)" = "02127", "East Boston(02128)" =  "02128", "Charles Town(02129)" = "02129", "Jamaica Plain(02130)" = "02130","Roslindale(02131)"= "02131","West Roxbury(02132)" = "02132", "Allston(02134)" =  "02134","Hyde Park(02136)" = "02136","Milton(02186)" = "02186","Prudential(02199)"="02199","Fenway(02215)" = "02215","Brookline(02445)" = "02445","Brookline(02446)" = "02446","Chestnut Hill(02467)" = "02467")

# Define UI for application that plots features of movies 
ui <- fluidPage(
  
  # Sidebar layout with a input and output definitions 
  sidebarLayout(
    
    # Inputs
    sidebarPanel(
      
      
      # Select variable for y-axis
      conditionalPanel(condition = "input.ts == 'rv'",
                       selectInput(inputId = "x", 
                                   label = "Y-axis:",
                                   choices = c("Residential View" = "R_VIEW",  "Type of Property"= "LU"), 
                                   selected = "R_VIEW")),
      
      # Select variable for y-axis
      conditionalPanel(condition = "input.ts == 'rv' && input.x == 'R_VIEW'", #contitional panel for different tabs and different X axis for R_VIEW and LU
                       selectInput(inputId = "y", 
                                   label = "X-axis:",
                                   choices = c("Median Assessed Value RView" = "median_total_view", "Median Gross Tax RView" = "median_tax_view"), 
                                   selected = " median_total_view"),
                       selectInput(inputId = "z", 
                                   label = "Select Color:",
                                   choices = c("Median Assessed Value RView" = "median_total_view", "Median Gross Tax RView" = "median_tax_view"),
                                   selected = "median_tax_view")),
      
      conditionalPanel(condition = "input.ts == 'rv' && input.x == 'LU'",
                       selectInput(inputId = "y1", 
                                   label = "X-axis:",
                                   choices = c("Median Assessed Value LU" = "median_total_lu", "Median Gross Tax LU" = "median_tax_lu"), 
                                   selected = " median_total_lu"),
                       selectInput(inputId = "z1", 
                                   label = "Select Color:",
                                   choices = c("Median Assessed Value LU" = "median_total_lu", "Median Gross Tax LU" = "median_tax_lu"),
                                   selected = "median_tax_lu")),

      ##For Second Tab
      conditionalPanel(condition = "input.ts == 'zc'",
                     wellPanel(checkboxGroupInput("check", "Select the area", choices = choices_check, selected = c("02135","02108", "02109","02111","02113","02124","02125","02126","02127")),
                     actionLink("selectall","Select All Areas")),
      wellPanel(
      sliderInput("year", "Choose Year Range",                                     ##year slider for line chart
                  min = 1700, max = 2018,value = c(2000,2018))))
       
    ),
    
    # Outputs
    mainPanel(
      tabsetPanel(id = 'ts',
                  tabPanel(title = "Property Type and View ", value = 'rv',  ##for first tab 
                           h3("Residential View and Property Type"),           ##heading for first tab
                           plotlyOutput(outputId = "barplot"),                 ##output for bar plots in first tab 
                           br()),                                              ##break rule for spaces
                  
                  tabPanel(title = "Know Your Property Well", value = 'zc',       ##title for second tab
                           br(),
                           h5("Assessed Value and Gross Tax based on areas"),
                           plotOutput("threemap_population_country"),          ##tree map output
                           br(),br(),br(),                                     ##break rule
                           plotlyOutput("barplot2"),                           ##plot output for second bar chart
                           br(),br(),br(),                                     ##break rule
                           
                           plotlyOutput("lineChart"),                          ##plot output for line chart
                           br())
                  
      )
    )
  )
)
##server function
server <- function(input, output, session) {
  ##output code for bar plot
  output$barplot <- renderPlotly({
    median_rview <- data_clean%>% group_by(R_VIEW) %>% summarise(median_total_view = median(AV_TOTAL), median_tax_view = median(GROSS_TAX)) ##finding out median based on R_view
    median_lu <- data_clean%>% group_by(LU) %>% summarise(median_total_lu = median(AV_TOTAL), median_tax_lu = median(GROSS_TAX))            ##finding out median based on LU
    
    xyz <- merge(data.frame(median_lu, row.names=NULL), data.frame(median_rview, row.names=NULL), by = 0, all = TRUE)[-1]                   ##merging two frames
   ## cleaning DF
    xyz$median_tax_lu[is.na(xyz$median_tax_lu)]<- 0
    xyz$median_total_lu[is.na(xyz$median_total_lu)]<-0
    xyz$LU<-as.character(xyz$LU)
    xyz$LU[is.na(xyz$LU)]<-""
    
    if(input$x == "R_VIEW"){  ## plot when Y axis is R_VIEW
    view <- xyz[,4:6]         
    view$R_VIEW <- factor(view$R_VIEW, levels = view$R_VIEW[order(-view$median_total_view)]) ##sorting based on Median Assessed Value
    plot_bar_r <- ggplot(data = view, aes_string(x = input$x, y = input$y, fill = input$z)) +
      geom_bar(stat = "identity", position = "dodge") + labs(x = 'Residential View', y = 'Median Assessed Value', 
                                                             title = 'Residential View') + coord_flip() + removeGrid()
    ggplotly(plot_bar_r) %>% config(displayModeBar = F)   ##ggplot for residential view bar chart
    
    
    }
    else{
      LU <- xyz[1:4,1:3]                                                              ##When Y axis is LU
      LU$LU <- factor(LU$LU, levels = LU$LU[order(-LU$median_total_lu)])              ##sorting based on Median Assessed Value
      
      plot_bar_lu <- ggplot(data = LU, aes_string(x = input$x, y = input$y1, fill = input$z1)) +
      geom_bar(stat = "identity", position = "dodge") +labs(x = 'Type of Property', y = 'Median Assessed Value', 
                                                            title = 'Land use')+ coord_flip()+ removeGrid()
      ggplotly(plot_bar_lu) %>% config(displayModeBar = F)                           ##Plot for LU 
      
      
    }
  })
  ##filtering data
  data_filter <-reactive({
    selected <-c(input$check)
   data_fil <- subset(data_clean, data_clean$ZIPCODE%in%selected)
    return(data_fil)
  })
  
  
    output$threemap_population_country <- renderPlot({                                                     ##plot for treemap
    median_av <- data_clean%>% group_by(ZIPCODE) %>% summarise(median_total = median(AV_TOTAL), median_tax = median(GROSS_TAX))
    median_av <- median_av %>% filter(ZIPCODE %in% input$check)
    ggplot(median_av, aes(area = median_total, fill = median_tax, label = ZIPCODE)) +
      geom_treemap() +  geom_treemap_text(place = "centre")            ##ggplot for treemap
    
  })
  
  output$barplot2 <- renderPlotly({ 
    
    median_ovrcd <- data_clean%>% group_by(R_OVRALL_CND, LU,ZIPCODE) %>% summarise(median_total_rovr = median(AV_TOTAL), median_tax_rovr = median(GROSS_TAX))
    median_ovrcd <- median_ovrcd %>% filter(ZIPCODE %in% input$check)
    plot <- ggplot(median_ovrcd, aes(fill= LU , y= median_total_rovr, x= R_OVRALL_CND)) + geom_bar( stat="identity") +labs(x = 'Residential Overall Condition', y = 'Median Assessed Value', 
                                                                                                                   title = 'Overall Residential Condition with Median Assessed Valued wrt Property Type') + removeGrid() + coord_flip()
    ggplotly(plot) %>% config(displayModeBar = F)                                                       ####Plot for stacked bar plot
  
  })
  
  year_filter <-reactive({
    median_line <- median_line %>% filter(YR_BUILT > input$year[1] & YR_BUILT < input$year[2])    ##code to get slider value in year build filter
    return(median_line)
  })
  
##code for line chart  
  output$lineChart <- renderPlotly({
    median_line <- year_filter()
    median_line <- median_line %>% filter(ZIPCODE %in% input$check)
    
    labels <- c("median_total","median_tax")
    plot_line <- ggplot(data = median_line, aes_string(x = median_line$YR_BUILT, y = median_line$median_total, z = median_line$median_tax))+
      geom_line(aes(y=median_total,colour="median_total"), size = 1)+
      geom_line(aes(y=median_tax,colour="median_tax"), size = 0.50)+
      scale_color_manual(values=c("purple", "orange"))+
      ggtitle("Line graph")+ removeGrid() + labs(x = 'Year Built', y = 'Median Values', title = 'Trend line for Year Built wrt Assessed Value and Gross Tax ')
    ggplotly(plot_line) %>% config(displayModeBar = F)  
  })
## code for select all areas button  
  observe({
    if(input$selectall == 0) return(NULL) 
    else if (input$selectall%%2 == 0)
    {
      updateCheckboxGroupInput(session,"check","Select the area",choices= choices_check)
    }
    else
    {
      updateCheckboxGroupInput(session,"check","Select the area",choices= choices_check,selected= choices_check)
    }
  })
  
}

shinyApp(ui = ui, server = server)
