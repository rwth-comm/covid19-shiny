## app.R ##
library(shinydashboard)
library(shiny)
library(tidyverse)
library(lubridate)
library(gghighlight)

raw <- read_csv("https://www.arcgis.com/sharing/rest/content/items/f10774f1c63e40168479a1feb6c7ca74/data") %>% 
    mutate(Meldedatum = as_date(Meldedatum)) %>% 
    arrange(Meldedatum)


#raw %>% filter(Landkreis == "StadtRegion Aachen") %>% group_by(Meldedatum) %>% summarise(AnzahlFall = sum(AnzahlFall))


lks <- raw$Landkreis %>% unique() %>% sort()
ages <- raw$Altersgruppe %>% unique() %>% sort()


ui <- 
    dashboardPage(
        dashboardHeader(title = "Basic dashboard"),
        dashboardSidebar(
            sidebarMenu(
                menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
                menuItem("Widgets", tabName = "widgets", icon = icon("address-book"))
            )
        ),
        dashboardBody(
            tabItems(
                tabItem(tabName = "dashboard",
                    fluidRow(
                        box( width = 8,
                             title = "Fälle pro Tag",
                             plotOutput("plot1"),
                             plotOutput("plotDetail"),
                             plotOutput("plotSumme")
                        ),
                        
                        box(
                            width = 4, 
                            title = "Controls",
                            selectInput("lk_selector", "Landkreis auswählen", choices = lks),
                            selectInput("age_selector", "Altersgruppe auswählen", choices = ages)
                        ),
                        
                        box(width = 8,
                            title = "Empty",
                            textOutput("error")
                            )
                    
                        
                        
                        
                        
                        
                        
                        
                        
                        
                    )
                
                    
                    
                    
                    
                    
                    
                    
                ),
                tabItem(tabName = "widgets", 
                    fluidRow(
                        box(width = 12,
                            title = "Empty2",
                            h1("Überschrift"),
                            h2("Überschrift 2"),
                            h3("Überschrift 3"),
                            h4("Überschrift 4"),
                            p("Hallo das ist ein Test"),
                            br(), br(), 
                            p("Zweiter Paragraph"),
                            a(href = "www.google.com", "Hallo"),
                            strong(p("Test")),
                            hr()

                            
                            
                            )
                    )
                )
            
            )
        )
    )

server <- function(input, output) {

    
    lk_daten <- reactive({
        raw %>% 
            filter(Landkreis == input$lk_selector)
    })
    

    output$plot1 <- renderPlot({
       lk_daten() %>% 
            ggplot() +
            aes(x = Meldedatum) +
            aes(y = AnzahlFall) +
            aes(fill = Altersgruppe) + 
            geom_col() +
            labs(title = "Anzahl Fälle pro Tag",
                 x = "Datum", y = "Anzahl Fälle")
        
    })

    
    output$plotDetail <- renderPlot({
        lk_daten() %>% 
            ggplot() +
            aes(x = Meldedatum) +
            aes(y = AnzahlFall) +
            aes(fill = Altersgruppe) + 
            geom_col() +
            labs(title = "Anzahl Fälle pro Tag",
                 x = "Datum", y = "Anzahl Fälle") +
            gghighlight::gghighlight(Altersgruppe == input$age_selector)
        
    })

    
    output$plotSumme <- renderPlot({
        lk_daten() %>% 
            group_by(Meldedatum) %>% 
            summarise(AnzahlFall = sum(AnzahlFall)) %>% 
            mutate(AlleFaelle = cumsum(AnzahlFall)) %>% 
            ggplot() +
            aes(x = Meldedatum) +
            aes(y = AlleFaelle) +
            geom_col() +
            labs(title = "Anzahl Fälle pro Tag (kumuliert)",
                 x = "Datum", y = "Anzahl Fälle")
        
    })
    
    
}

shinyApp(ui, server)