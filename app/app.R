#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(tidyverse)
library(tmap)
library(sf)
library(raster)
library(clock)
library(dplyr)
library(rconnect)


gps_path2 <- readRDS("data/gps_path2.rds")
cc_loyal3 <- readRDS("data/cc_loyal3.rds")
GPS_filter <- readRDS("data/GPS_filter.rds")
bgmap <- raster("Data/MC2-tourist.tif")




my_list<-seq(1, 35, by=1)

named_list<- list(101,104,105,106,107)

list1<-append(my_list, named_list)

list2<-seq(6, 19, by=1)

ui <- fluidPage(titlePanel("Vehicle Tracking for Gastech Employee "),
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
                ))


server <- function(input, output,session) {
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

