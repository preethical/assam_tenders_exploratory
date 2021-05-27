library(shiny)
library(tidyverse)
library(ggplot2)
library(plotly)
library(psych)
library(lubridate)
assam_published <- read.csv("assam_tenders_published.csv", header = T)
print(str(glimpse(assam_published)))
ui <- fluidPage(titlePanel("Basic Exploratory Dashboard"),
                sidebarLayout(
                    sidebarPanel(
                        helpText("The various inputs this takes are Tender Organization, product category"),
                        selectizeInput("Org", label="Organization", choices =  unique(assam_published$Org), options = list(maxItems = 4, placeholder = "Please select an Organization")),
                        selectizeInput("product_category", label="Product Category", choices =  unique(assam_published$product_category), options = list(placeholder = "Please select a type of product")),
                        radioButtons("KPI", label = "Key Performance Indicator", 
                                     choices = c("Tender Stage","Tender Status","Number of bids received","Number of days for AOC", "Number of days for evaluation"),
                                     selected = "Tender Stage")
                        ),
                                    mainPanel(
                                        plotOutput("Tender_stage_plot"),
                                        plotOutput("Tender_status_plot"),
                                        plotOutput("bids_recieved"),
                                        plotlyOutput("AOC_days"),
                                        plotOutput("Evaluation_days"),
                                        br(), br(),
                                        tableOutput("results")
                                    )
                               )
                )
            
server <- function(input, output) {
    output$Tender_stage_plot <- renderPlot({
        filtered <- assam_published %>% filter(Org == input$Org, type == input$product_category)
      ggplot(filtered, aes(x=Org, y = tender_stage)) + 
            geom_boxplot(outlier.colour="red", outlier.shape=8,outlier.size=4)
    }
    )
}

shinyApp(ui = ui, server = server)