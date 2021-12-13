#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(DT)
library(ggplot2)
library(shinydashboard)
library(plotly)
library(rjson)
library(plotly)
library(data.table)
library(readxl)

setwd('/Users/prashntshukla/Desktop/Crime_Dashboard_Prashant Shukla_Rshiny_Comm')

Data_main<-read_excel('New File Created.xlsx')
Data_sec<-read_excel('New_second data.xlsx')
state_crime<-read_excel('Crime_per_Million.xlsx')
DT <- data.table(Data_main)
Crimes<-DT[, sum(`TOTAL IPC CRIMES`), by = YEAR]
colnames(Crimes)<-c("Year","CrimesReported")

Data_sec$`Crimes Type`<-as.factor(Data_sec$`Crimes Type`)

Crimes$Year<-as.character(Crimes$Year)



# Define UI for application that draws a histogram
ui <- dashboardPage( skin='green',
    dashboardHeader(title = "Crimes in India "),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Crimes_Dashboard", tabName = "Crimes_Dashboard", icon = icon("dashboard")),
            
            menuItem("Analysis", tabName = "Analysis", icon = icon("dashboard"))
            
        )
    ),
        
    dashboardBody(
        tabItems(
            # First tab content
            tabItem(tabName = "Crimes_Dashboard",
            fluidRow(
                valueBoxOutput("Crimes_Reported", width = 4),
                valueBoxOutput("Crime_Against_Women", width = 4),
                valueBoxOutput("Lives_Lost", width = 4)
            ),
            # row
            fluidRow(
                box(title = "State Wise Crimes Reported Per Million Population",background = "black", solidHeader = T,
                    width = 12, collapsible = T,
                    plotlyOutput("crime_map"))
             ),
        ),
            
        tabItem(tabName = "Analysis",
                
            fluidRow(
                box(title = "Top Seven Crimes",background = "black", solidHeader=T,
                    width = 12, collapsible = T,
                    plotlyOutput("Crime_type"))
            ),
            
            fluidRow(
                box(title = "Top States in Crimes Against Women Per Million Population",background = "black", solidHeader = T,
                    width = 12, collapsible = T,
                    plotlyOutput("Funnel"))
            ),
            fluidRow(
                box(title = "Year Wise Crimes Reported",background = "black", solidHeader = T,
                    width = 12, collapsible = T,
                    plotlyOutput("Year_crime"))
            ),
            fluidRow(
                box(title = "Crime Analysis 2013",background = "black", solidHeader = T,
                    width = 12, collapsible = T,
                    plotlyOutput("Doughnut"))  
            ), #row 
         )
        ) 
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
        output$crime_map <- renderPlotly({
            
            counties <- rjson::fromJSON(file='states2.json')
            
            
            df <- state_crime
            
            plot_ly()%>% add_trace(
                type="choroplethmapbox",
                geojson=counties,
                locations=df$State,
                z=df$`Crime per Million`,
                colorscale="Portland",
                zmin=3000,
                zmax=92000,
                marker=list(line=list(
                    width=0.5),
                    opacity=2
                )
            )%>% colorbar(title = "Crimes Per Million")%>% layout(
                mapbox=list(
                    style="carto-positron",
                    zoom =2,
                    center=list(lon= 68.18625	, lat=6.754256))
            )%>%layout(autosize = T,scope = 'asia')

            
    })
    
    
    output$Crime_type <- renderPlotly({
        
        colors2 <- c('#3399FF','#99FFFF','#99FFFF','#99FFFF','#99FFFF','#99FFFF','#99FFFF')
        
        
        
        fig2<-plot_ly(Data_sec, x = ~reorder(`Crimes Type`,`Total Crimes`), y = ~`Total Crimes`, type = 'bar', marker = list(color = colors2))
        
        fig2 %>% layout(title = "Top Seven Crimes",
                        xaxis = list(title = "Crime Type"),
                        yaxis = list(title = "Crimes Reported"))
 })
    
    
    
    
    output$Funnel <- renderPlotly({
        fig3 <- plot_ly() 
        fig3 <- fig3 %>%
            add_trace(
                type = "funnel",
                y = c("Madhya Pradesh","Rajasthan","Maharshtra","Uttar Pradesh","West Bengal"),
                x = c(2768,1571,1081, 942,934),marker = list(color = c('#00CCCC','#00FFFF','#66FFFF','#99FFFF','#CCFFFF')),
                connector = list(line = list(color = "royalblue", dash = "dot", width = 3)),textposition = "inside")
        
        fig3 %>%
            layout( title = "Crimes Against Women Per Million",yaxis = list(categoryarray = c("Madhya Pradesh","Rajasthan","Maharshtra","Uttar Pradesh","West Bengal")))
        
    })
    
    output$Year_crime <- renderPlotly({
        f <- list(
            family = " Year Wise Crimes",
            size = 18,
            color = "#7f7f7f"
        )
        x <- list(
            title='Year', titlefont = f,zeroline = T,
            showline = T,
            showticklabels = T,
            showgrid = FALSE, tickangle = 90,tick0 = 0.5)
        
        y <- list(
            title = "Total Crimes Reported",
            titlefont = f, range=c(0,3000000)
        )
        
        
        plot_ly(Crimes, x = ~Crimes$Year, y = ~Crimes$CrimesReported, type = 'scatter', mode = "markers+lines", marker=list(size=10,color = c('#FFCCCC','#FF9999','#FF6666','#FF3333','#FF0000','#CC0000','#990000'))) %>%
            layout(xaxis = x, yaxis = y,title = "Year Wise Crimes Reported")
    })
    
    output$Doughnut <- renderPlotly({
        fig4 <-plot_ly(labels = ~Data_sec$`Crimes Type`, values = ~Data_sec$`Y-2013`)
        fig4 <- fig4 %>% add_pie(hole = 0.2)
        fig4%>% layout(title = "Crime Analysis 2013",  showlegend = T,
                       xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                       yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),legend = list(orientation = 'h'))
    })
    
    
    
    
    # value boxes
    output$Crimes_Reported <- renderValueBox({
        valueBox(sum(Data_main$`TOTAL IPC CRIMES`), "Crimes Reported", icon = icon("fire"), color = "red")
    })
    
    output$Crime_Against_Women <- renderValueBox({
        valueBox(sum(Data_main$`Crime Against Women`), "Crimes Against Women", icon = icon("fire"), color = "red")
    })
    
    output$Lives_Lost <- renderValueBox({
        valueBox(sum(Data_main$`Total Deaths`), "Lives Lost", icon = icon("fire"), color = "red")
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
