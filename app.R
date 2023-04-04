## Jacob M. Lundeen
## Final Project
## Data Visualization
## Spring 2021

library(shiny)
library(shinythemes)
library(dplyr)
library(tidyr)
library(readxl)
library(ggplot2)
library(shinyBS)
library(plotly)
library(hrbrthemes)
library(cowplot)
library(tidyverse)
library(nflfastR)
library(gridExtra)
library(olsrr)
library(skimr)
library(ggrepel)
library(ggimage)
library(dplyr)
library(knitr)
library(purrr)
library(qs)
library(httr)
library(readr)
library(reshape2)
library(kableExtra)
library(Hmisc)

pbp <- load_pbp(2011:2020)

# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(
    titlePanel("NFL QB Efficiency Data"),
    
    sidebarLayout(
        sidebarPanel(
            fluidRow(
                column(
                    br(),
                    img(scr="logo.png", width = "200px", height = "200px"),
                    br(),
                    br(),
                    p("This R Shiny application is for my Data Visualization 
                      course from Johns Hopkins University during Spring 2021. 
                      The data used comes from Mr. Ben Baldwin's nflfastR which 
                      contains all play-by-play (PBP) data from 2001-2020.", style = "text-align:left"),
                    br(),
                    p("For this project, I will be using the PBP data from 2011-2020 
                      and focusing on Quarterback (QB) efficiency. Efficiency is 
                      defined by two metrics: Completed Passes Over Expectation (CPOE), 
                      and Expected Points Added (EPA). These will be regressed 
                      on Wins to see how well they predict wins.", style = "text-align:left"),
                    width = 12
                )
            )
        ),
        mainPanel(
            
            tabsetPanel(
                type = "tabs",
                tabPanel("CPOE", 
                         plotlyOutput("plot"),
                         br(),
                         p("Here we are looking at Wins vs. CPOE. The data is 
                           colored by the number of games played and sized by 
                           the number of passing attempts. As we can see, there is 
                           a clear positive relationship between Wins and CPOE. 
                           There is a clear concentration of data between -4 
                           and +4 CPOE. For anyone that knows football, we see 
                           Tom Brady pretty much breaking the model.", style = "text-align:left")
                         ),
                tabPanel("EPA", 
                         plotlyOutput("plot2"),
                         br(),
                         p("Here we are looking at Wins vs. EPA. It is colored 
                           and sized the same as previous chart. With EPA, most 
                           of the data exists between 0 and 0.3 EPA. If we had 
                           running back data included on this plot, we would see 
                           a clear delineation between the value of passing and 
                           rushing. And once again, Tom Brady is breaking the model.", 
                           style = "text-align:left")
                         ),
                tabPanel("Model", 
                         p("Below we see the linear model with CPOE, EPA, Atts, 
                           and Num_Games regressed on Wins. Both CPOE and Atts 
                           have large p-values, and the correlation matrix shows 
                           that CPOE is highly correlated with EPA and Atts is 
                           highly correlated with Number of games.", 
                           style = "text-align:left"),
                         br(),
                         p("The second model has removed CPOE and Atts and we see
                           EPA and Num_Games are both statistically significant 
                           and the model has an adjusted R^2 of 0.93.", 
                           style = "test-align:left"),
                         verbatimTextOutput("model")
                         ),
                tabPanel("Residuals",
                         p("Here are the residual plots for both models.", 
                           style = "test-align:left"),
                         plotOutput("resid")
                ),
                tabPanel("Summary", 
                         p("Here we have the standard descriptive statistics of 
                           the variables.", style = "test-align:left"),
                         verbatimTextOutput("summary")
                         ),
                tabPanel("Table", 
                         p("This is a print out of the data for visual inspection.", 
                           style = "test-align:left"),
                         tableOutput("table")
                         )
            )
            
        )
    )
))

# Define server logic required to draw a histogram
server <- shinyServer(
    function(input, output) {
        
        qb_wins <- pbp %>%
            mutate(
                wins = case_when(
                    result == 0 ~ 0.5,
                    result > 0 & posteam == home_team ~ 1,
                    result < 0 & posteam == away_team ~ 1,
                    TRUE ~ 0
                )
            ) %>%
            filter(!is.na(cpoe)) %>%
            group_by(name, game_id) %>%
            summarise(wins = first(wins)) %>%
            summarise(
                Wins = sum(wins),
                Num_Games = n()
            )
        
        qb_eff <- pbp %>%
            filter(!is.na(cpoe) & !is.na(epa)) %>%
            group_by(name) %>%
            summarise(
                CPOE = round(mean(cpoe), 2),
                EPA = round(mean(epa), 2),
                Atts = n()
            ) %>%
            ungroup()
        
        results <- bind_cols(qb_wins, qb_eff[2:4]) %>%
            filter(Atts > 500)
        
        linemod <- lm(Wins ~ CPOE + EPA + Atts + Num_Games, data = results)
        model2 <- lm(Wins ~ EPA + Num_Games, data = results)
        
        mean_wins <- results %>%
            pull(Wins) %>%
            mean()
        
        mean_cpoe <- results %>%
            pull(CPOE) %>%
            mean()
        
        mean_epa <- results %>%
            pull(EPA) %>%
            mean()
        
        output$plot <- renderPlotly({
            
            g <- ggplot(results, aes(label = name, label2 = Atts, label3 = Num_Games)) + 
                geom_point(aes(x = CPOE, y = Wins, color = Num_Games, size = Atts)) + 
                theme_bw() + 
                theme(plot.title = element_text(hjust = 0.5),
                      plot.caption = element_text(hjust = 0.0, vjust = 0.5)) + 
                geom_smooth(aes(x = CPOE, y = Wins), method='lm', formula = y~x) + 
                labs(title = "Wins versus CPOE", caption = "Source: nflfastR") +
                scale_color_continuous(name = "# Games")
            
            g <- ggplotly(g, tooltip = c('label', 'label2', 'label3', 'x', 'y'))
            
        })
        
        output$plot2 <- renderPlotly({
            
            g <- ggplot(results, aes(label = name, label2 = Atts, label3 = Num_Games)) + 
                geom_point(aes(x = EPA, y = Wins, color = Num_Games, size = Atts)) + 
                theme_bw() + 
                theme(plot.title = element_text(hjust = 0.5)) + 
                geom_smooth(aes(x = EPA, y = Wins), method='lm', formula = y~x) + 
                labs(title = "Wins versus EPA", caption = "Source: nflfastR") +
                scale_color_continuous(name = "# Games")
            
            g <- ggplotly(g, tooltip = c('label', 'label2', 'label3', 'x', 'y'))
            
        })
        
        output$model <- renderPrint({
            print(summary(linemod))
            res <- rcorr(as.matrix(results[2:6]))
            print(res, digits = 10)
            print(summary(model2))
        })
        
        output$resid <- renderPlot({
            q <- ggplot(linemod, aes(.fitted, .resid)) + geom_point() + 
                geom_hline(yintercept = 0, col = "red") + theme_bw() + 
                labs(title = "Residual Plot for Model 1", y = 'Residuals',
                     x = "Fitted")
            
            p <- ggplot(model2, aes(.fitted, .resid)) + geom_point() + 
                geom_hline(yintercept = 0, col = "red") + theme_bw() +
                labs(title = "Residual Plot for Model 2", y = "Residuals", 
                     x = "Fitted")

            grid.arrange(q, p, ncol = 1, nrow = 2)
        })
        
        output$summary <- renderPrint({
            summary(results)
        })
        
        output$table <- renderTable({
            results
        })
        
    }
    
    
)

# Run the application 
shinyApp(ui = ui, server = server)
