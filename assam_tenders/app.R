## Think of other useful comparisons to make. 
## next things to do - make it work with just one drop down filled out, make it work by separating out the comparators, 
#Think of other comparisons to make 
#add aoc data stuff


library(shiny)
library(tidyverse)
library(ggplot2)
library(plotly)
library(psych)
library(lubridate)
assam_published <- read.csv("assam_tenders_published.csv", header = T)

ui <- fluidPage(titlePanel("Basic Exploratory Dashboard"),
                sidebarLayout(
                    sidebarPanel(
                        helpText("The various inputs this takes are Tender Organization, product category"),
                        uiOutput("Org_output"),
                        uiOutput("category_output"),
                        radioButtons("KPI", label = "Key Performance Indicator", 
                                     choices = c("Tender Stage","Tender Status","Number of bids received","Number of days for AOC", "Number of days for evaluation"),
                                     selected = "Tender Stage"), 
                        actionButton(inputId="DatabyPage", label="Calculate")
                        ),
                                    mainPanel(
                                        plotlyOutput("tender_stage_plot"),
                                        plotlyOutput("bids_recieved"),
                                        plotlyOutput("AOC_days"),
                                        plotlyOutput("Evaluation_days"),
                                        tableOutput("results")
                                    )
                               )
                )
            
server <- function(input, output, session) {
    output$Org_output <- renderUI ({
        selectizeInput("input_Org", label="Organization", choices =  sort(unique(assam_published$Org)), 
                       multiple = TRUE, selected = NULL)
    })
    output$category_output <- renderUI ({
        selectizeInput("input_category", label="Tender Category",
                       choices =  sort(unique(assam_published$tender_category)), multiple = TRUE, selected = NULL)
    })    
        filtered <- reactive({
            if (is.null(input$input_Org)) {
                return(NULL)
            } 
        assam_published %>% filter(Org %in% input$input_Org, tender_category %in% input$input_category)
        })
    output$tender_stage_plot <- renderPlotly({
        if (is.null(filtered())) {
            return()
        }
        ggplotly(ggplot(data = filtered(), aes(x = tender_stage,fill = tender_status)) + 
                     geom_bar(position = "dodge") + facet_grid(~publishedyear, scales = "free") + 
                     theme(axis.text.x=element_text(angle=90,hjust=1,vjust=.5,colour='black')))
    })
    output$bids_recieved<- renderPlotly({
        if (is.null(filtered())) {
            return()
        }
        ggplotly(ggplot(data = filtered(), aes(x = publishedyear, y = no_of_bids_received)) + 
                     geom_boxplot(outlier.colour="red", outlier.shape=8,
                                  outlier.size=4))
    })
    output$results <- renderTable({
        filtered()
    })
}
shinyApp(ui = ui, server = server)