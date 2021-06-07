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
#setwd("~/Documents/R_things/assam_tenders_exploratory/assam_tenders") ##change this to wherever your local files are
assam_published <- read.csv("assam_merge.csv", header = T)


ui <- fluidPage(titlePanel("Basic Exploratory Dashboard"),
                sidebarLayout(
                    sidebarPanel(
                        helpText("The various inputs this takes are Tender Organization"),
                        uiOutput("Org_output"),
                        radioButtons("KPI", label = "Key Performance Indicator", 
                                     choices = c("Number of tenders","Tender Stage","Number of bids received","Number of days for AOC", "Number of days for prize bid", "Number of days for evaluation"),
                                     selected = "Number of tenders"), 
                        actionButton(inputId="DatabyPage", label="Calculate")
                        ),
                                    mainPanel(
                                        plotlyOutput("number_plot"), plotlyOutput("tender_stage_plot"),plotlyOutput("bids_recieved"),
                                        plotlyOutput("AOC_days"),plotlyOutput("AOC_days2"),
                                        plotlyOutput("Evaluation_days"),
                                        tableOutput("results")
                                    )
                               )
                )
            
server <- function(input, output, session) {
    output$Org_output <- renderUI ({
        selectizeInput("input_Org", label="Organization", choices =  sort(unique(assam_published$Org.x)), 
                       multiple = TRUE, selected = NULL)
    })
        filtered <- reactive({
            if (is.null(input$input_Org)) {
                return()
            } 
        assam_published %>% filter(Org.x %in% input$input_Org)
        })

 ##Number of tenders plot       
        observeEvent(input$DatabyPage,{
            if(input$KPI == "Number of tenders"){
            output$number_plot <- renderPlotly ({
                 if (is.null(filtered())) {
                     return(ggplotly(ggplot(data = assam_published, aes(x = as.character(publishedyear.x))) + 
                                            geom_bar() + facet_grid(~tender_category.x, scales = "free") + 
                                            theme(axis.text.x=element_text(angle=90,hjust=1,vjust=.5,colour='black'))))
                    }
                 else 
                     return (ggplotly(ggplot(data = filtered(), aes(x = as.character(publishedyear.x))) + 
                                 geom_bar() + facet_grid(~tender_category.x, scales = "free") + 
                                 theme(axis.text.x=element_text(angle=90,hjust=1,vjust=.5,colour='black'))))
                    
            })  
                output$results <- renderTable({
                        filtered()
                    })
            }
            })
    ##Tender stage plot        
        observeEvent(input$DatabyPage,{
            if(input$KPI == "Tender Stage"){
            output$tender_stage_plot <- renderPlotly({
                    if (is.null(filtered())) {
                        return(ggplotly(ggplot(data = assam_published, aes(x = as.character(publishedyear.x),fill = tender_status)) + 
                                            geom_bar(position = "dodge") + facet_grid(~tender_category.x, scales = "free") + 
                                            theme(axis.text.x=element_text(angle=90,hjust=1,vjust=.5,colour='black'))))
                    }
                    else
                        return (ggplotly(ggplot(data = filtered(), aes(x = as.character(publishedyear.x),fill = tender_status)) + 
                                 geom_bar(position = "dodge") + facet_grid(~tender_category.x, scales = "free") + 
                                 theme(axis.text.x=element_text(angle=90,hjust=1,vjust=.5,colour='black'))))
                    })
                output$results <- renderTable({
                    filtered()
                })
            }
            })
        ##bids received plot
        observeEvent(input$DatabyPage,{
            if(input$KPI == "Number of bids received"){
            output$bids_recieved<- renderPlotly({
                    if (is.null(filtered())) {
                        return(ggplotly(ggplot(data = assam_published, aes(x = as.character(publishedyear.x), y = no_of_bids_received.x)) + 
                                            geom_boxplot(outlier.colour="red", outlier.shape=8,
                                                         outlier.size=4)+facet_grid(~tender_category.x, scales = "free")+ 
                                            theme(axis.text.x=element_text(angle=90,hjust=1,vjust=.5,colour='black'))))
                    }
                    else 
                        return (ggplotly(ggplot(data = filtered(), aes(x = as.character(publishedyear.x), y = no_of_bids_received.x)) + 
                                 geom_boxplot(outlier.colour="red", outlier.shape=8,
                                              outlier.size=4)+ facet_grid(~tender_category.x, scales = "free")))
                    })
                output$results <- renderTable({
                        filtered()
                    })
            }
            })
        
        ##Number of days for aoc plot
        
        observeEvent(input$DatabyPage,{
            if(input$KPI == "Number of days for AOC"){
                output$AOC_days<- renderPlotly({
                    if (is.null(filtered())) {
                        return(ggplotly(ggplot(data = subset(assam_published,!is.na(aocdate)), aes(x = as.character(publishedyear.x), y = aocdate)) + 
                                            geom_boxplot(outlier.colour="red", outlier.shape=8,
                                                         outlier.size=4)+facet_grid(~tender_category.x, scales = "free") +
                                            theme(axis.text.x=element_text(angle=90,hjust=1,vjust=.5,colour='black'))))
                    }
                    else
                        return(ggplotly(ggplot(data = subset(filtered(),!is.na(aocdate)), aes(x = as.character(publishedyear.x), y = aocdate)) + 
                                            geom_boxplot(outlier.colour="red", outlier.shape=8,
                                                         outlier.size=4)+ facet_grid(~tender_category.x, scales = "free")))
                })
                output$results <- renderTable({
                    filtered()
                })
            }  
        })
            ##Number of days for prizebid
         observeEvent(input$DatabyPage,{
            if(input$KPI == "Number of days for prize bid"){
            output$AOC_days2<- renderPlotly({
                    if (is.null(filtered())) {
                        return(ggplotly(ggplot(data = subset(assam_published,!is.na(cycle_time_bet_e_publishing_date_and_opening_of_price_bid_in_days)), aes(x = as.character(publishedyear.x), y = cycle_time_bet_e_publishing_date_and_opening_of_price_bid_in_days)) + 
                                            geom_boxplot(outlier.colour="red", outlier.shape=8,
                                                         outlier.size=4)+facet_grid(~tender_category.x, scales = "free") +
                                            theme(axis.text.x=element_text(angle=90,hjust=1,vjust=.5,colour='black'))))
                    }
                    else
                        return(ggplotly(ggplot(data = subset(filtered(),!is.na(cycle_time_bet_e_publishing_date_and_opening_of_price_bid_in_days)), aes(x = as.character(publishedyear.x), y = cycle_time_bet_e_publishing_date_and_opening_of_price_bid_in_days)) + 
                                 geom_boxplot(outlier.colour="red", outlier.shape=8,
                                              outlier.size=4)+ facet_grid(~tender_category.x, scales = "free")))
                                    })
            output$results <- renderTable({
                filtered()
            })
            }  
      })
         
         ##Number of days for eval
 
         observeEvent(input$DatabyPage,{
             if(input$KPI == "Number of days for evaluation"){
                 output$Evaluation_days<- renderPlotly({
                     if (is.null(filtered())) {
                         return(ggplotly(ggplot(data = subset(assam_published,!is.na(no_of_days_b_w_tech_and_finance_opening)), aes(x = as.character(publishedyear.x), y = no_of_days_b_w_tech_and_finance_opening)) + 
                                             geom_boxplot(outlier.colour="red", outlier.shape=8,
                                                          outlier.size=4)+facet_grid(~tender_category.x, scales = "free") +
                                             theme(axis.text.x=element_text(angle=90,hjust=1,vjust=.5,colour='black'))))
                     }
                     else
                         return(ggplotly(ggplot(data = subset(filtered(),!is.na(no_of_days_b_w_tech_and_finance_opening)), aes(x = as.character(publishedyear.x), y = no_of_days_b_w_tech_and_finance_opening)) + 
                                             geom_boxplot(outlier.colour="red", outlier.shape=8,
                                                          outlier.size=4)+ facet_grid(~tender_category.x, scales = "free")))
                 })
                 output$results <- renderTable({
                     filtered()
                 })
             }  
         })
}         
shinyApp(ui = ui, server = server)
