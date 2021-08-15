#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

### install packages & library packages ###
packages = c('igraph','tidygraph','ggraph','visNetwork',
             'lubridate','clock','tidyverse',"tm",
             'tidytext', "plyr",'widyr', 'wordcloud',
             'DT', 'ggwordcloud','textplot','hms',"shinyWidgets",
             'tidygraph', 'ggraph','igraph','flipTime',
             "shiny","lubridate","ggplot2","dplyr","plotly",
             "rgdal","tmap","sf","raster","clock","shinythemes")
for(p in packages){
    if(!require(p,character.only = T)){
        install.packages(p)
    }
    library(p,character.only = T)
}

### load in data for MC1###
GAStech_nodes_new <- readRDS("data source/MC1/rds/GAStech_nodes_new.rds")
GAStech_edges_aggregated2 <- readRDS("data source/MC1/rds/GAStech_edges_aggregated2.rds")
nodes <- readRDS("data source/MC1/rds/nodes.rds")
name_list <- nodes$Name
GAStech_edges_aggregated3 <- readRDS("data source/MC1/rds/GAStech_edges_aggregated3.rds")
GAStech_edges_aggregated4 <- readRDS("data source/MC1/rds/GAStech_edges_aggregated4.rds")
GAStech_edges_aggregated5 <- readRDS("data source/MC1/rds/GAStech_edges_aggregated5.rds")
newsgroup_cors <- readRDS("data source/MC1/rds/newsgroup_cors.rds")
Node_present <- readRDS("data source/MC1/rds/Node_present.rds")
incidents <- readRDS("data source/MC1/rds/incidents.rds")

### load in data for MC2###
gps_path2 <- readRDS("data source/MC2/gps_path2.rds")
cc_loyal3 <- readRDS("data source/MC2/cc_loyal3.rds")
GPS_filter <- readRDS("data source/MC2/GPS_filter.rds")
bgmap <- raster("data source/MC2/MC2-tourist.tif")

### list creating for MC2###
my_list<-seq(1, 35, by=1)
named_list<- list(101,104,105,106,107)
list1<-append(my_list, named_list)
list2<-seq(6, 19, by=1)


### UI ###
ui <- fluidPage(theme = shinytheme("cerulean"),
    tabsetPanel(
        tabPanel("Network Demonstration",
                 titlePanel("Network Demonstration By Eamil Frequency") ,
                 sidebarLayout(
                     sidebarPanel(
                         tags$head( tags$style( type = "text/css", '
                     .js-irs-0 .irs-bar {
                     background: linear-gradient(to bottom, #DDD -50%, #FFF 150%);
                     border-top: 1px solid #CCC ;
                     border-bottom: 1px solid #CCC ;
                                            }')), 
                         sliderInput("email_no1",
                                     "Least number of emails:",
                                     max = 45,min = 1,value = 10),
                         selectInput(inputId = "from_person",
                                     label = "The people who sent emails",
                                     choices = name_list,
                                     selected = "lars.azada",
                                     multiple = FALSE),
                         dateRangeInput('dateRange',
                                        label = 'Date range input for email detail checking',
                                        start = '2014-01-06', '2014-01-15')
                     ),
                     mainPanel(visNetworkOutput("network1"),
                               DT::dataTableOutput(outputId = "persons1")
                     )
                 )
        ),
        tabPanel("Time Stamps",
                 titlePanel("The Time Variation Of News") ,
                 sidebarLayout(
                     sidebarPanel(
                         dateRangeInput('dateRange2',
                                        label = 'Date range input for News demonstration',
                                        start = '1982-10-02', '2014-03-26')
                     ),
                     mainPanel(plotOutput("Timevary"),
                               DT::dataTableOutput(outputId = "news")
                     )
                 )
        ),
        tabPanel("Map Visualization",
                 titlePanel("Vehicle Tracking for GAStech Employee "),
                 sidebarLayout(sidebarPanel(selectInput(inputId = "CarId",
                                                        label = "Car ID",
                                                        choices = list1,
                                                        selected = "1",
                                                        multiple = TRUE),
                                            selectInput(inputId = "Day",
                                                        label = "Tracking Day",
                                                        choices = list2,
                                                        selected = "6"),
                                            
                                            checkboxInput(inputId = "show_data",
                                                          label = "Show data table",
                                                          value = TRUE)
                 ),
                 mainPanel(tmapOutput("mapPlot"),
                           DT::dataTableOutput(outputId = "Transaction")
                 )
                 )
        ),
        tabPanel("Backup table",
                 titlePanel("The Back Up DT Table For Email Checking") ,
                 sidebarLayout(
                     sidebarPanel(
                         dateRangeInput('dateRange3',
                                        label = 'Date range input for email detail checking',
                                        start = '2014-01-06', '2014-01-15'),
                         selectInput(inputId = "to_person",
                                     label = "The people who recieves emails",
                                     choices = name_list,
                                     selected = "lars.azada",
                                     multiple = FALSE)
                     ),
                     mainPanel(DT::dataTableOutput(outputId = "persons2")
                     )
                 )
        )
    )
)






server <- function(input, output,session) {
    
    
    output$network1 <- renderVisNetwork({
        GAStech_edges_aggregated5 <- GAStech_edges_aggregated4 %>%
            filter(Date>=input$dateRange[1] & Date<=input$dateRange[2])
        GAStech_edges_aggregated5 <- rename(dplyr::count(GAStech_edges_aggregated5,
                                                         from, to),Freq = n)
        colnames(GAStech_edges_aggregated5) <- c("from", "to", "width")
        
        set.seed(2017)
        visNetwork(GAStech_nodes_new, GAStech_edges_aggregated5 %>%
                       filter(width > input$email_no1)) %>%
            visIgraphLayout(layout = "layout_with_fr") %>%
            visOptions(highlightNearest = TRUE,
                       nodesIdSelection = TRUE) %>%
            visEdges(arrows = "to") %>%
            visLegend()
    })
    
    output$persons1 <- DT::renderDataTable({
        Node_present$day <- AsDate(Node_present$Date)
        DT::datatable(data=Node_present %>%
                          filter(Date>=input$dateRange[1] & 
                                     Date<=input$dateRange[2] &
                                     From == input$from_person)
        )
    })
    
    output$Timevary <- renderPlot({
        col1 = "#d8e1cf" 
        col2 = "#438484"
        
        incidents2 <- incidents %>%
            filter(ymd>=input$dateRange2[1] & ymd<=input$dateRange2[2])
        yearMonth <- ddply(incidents2, c( "year", "month" ), summarise,
                           N    = length(ymd))
        
        yearMonth$month <- factor(yearMonth$month, levels=rev(levels(yearMonth$month)))
        
        ggplot(yearMonth, aes(year, month)) + geom_tile(aes(fill = N),colour = "white") +
            scale_fill_gradient(low = col1, high = col2) +  
            guides(fill=guide_legend(title="Total Incidents")) +
            labs(title = "Histogram of Gastech Incidents by Month and Year",
                 x = "Year", y = "Month") +
            theme_bw() + theme_minimal() 
    })
    
    output$news <- DT::renderDataTable({
        incidents3 <- incidents[,c(1,2,3,5)]
        DT::datatable(data=incidents3 %>%
                          filter(ymd>=input$dateRange2[1] & 
                                     ymd<=input$dateRange2[2])
        )
    })
    
    output$persons2 <- DT::renderDataTable({
        Node_present$day <- AsDate(Node_present$Date)
        DT::datatable(data=Node_present %>%
                          filter(Date>=input$dateRange3[1] & 
                                     Date<=input$dateRange3[2] &
                                     To == input$to_person)
        )
    })
    
    dataset = reactive({
        gps_path2 %>%
            filter(id %in% input$CarId) %>% #对%n% 改了==
            filter(day %in% input$Day)
    })
    dataset2=reactive({
        GPS_filter %>%
            filter(id %in% input$CarId) %>%
            filter(day %in% input$Day)
    })
    
    output$mapPlot <- renderTmap({
        tm_shape(shp = dataset())+tm_lines(col="id",legend.show = FALSE,lwd=2)+
            tmap_options(max.categories = 40)+  #改了color
            tm_shape(shp = dataset2())+tm_dots(palette="Accent",col="red",size=0.08,legend.show = FALSE)+
            tmap_options(max.categories = 40)+   #改了color
            tm_shape(bgmap) + 
            tm_rgb(bgmap, r = 1,g = 2,b = 3,
                   alpha = NA,
                   saturation = 1,
                   interpolate = TRUE,
                   max.value = 255) 
    })
    output$Transaction <- DT::renderDataTable({
        if(input$show_data){
            DT::datatable(data=cc_loyal3 %>%
                              dplyr::select(1:6),caption = 'Transaction Data',rownames = FALSE,filter = 'top', 
                          options = list(
                              pageLength = 5, autoWidth = TRUE))
            
        }
    })}

shinyApp (ui=ui, server=server)
