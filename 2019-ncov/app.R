library(shiny)
library(ggrepel)
source("etl_data.R")


ui <- fluidPage(
  titlePanel(h1("Spread of 2019-ncov ('2019  Corona Virus')", 
                h5("Source: Johns Hopkins University CSSE, https://systems.jhu.edu/research/public-health/ncov/"))),
  tags$br(),
  tags$br(),
  fluidRow(
    column(12, div(style = "font-size:30px;",
                  textOutput("text_totals")))
  ),
  
  fluidRow(
    column(12, div(style = "font-size:15px;",
                   textOutput("text_updated")))
  ),
  
  hr(),
  
  fluidRow(
    column(7,
           plotOutput("linegraph"),
           radioButtons("lgraphoptions", label = NULL,
                        c("Hubei/ China/ other" = "hubei_china_world",
                          "China/ other" = "china_ornot", 
                          "Total" = ""))
    ),
    column(6, offset = 1,
           tableOutput("tableovervw"))
  ),
  
  fluidRow(
    imageOutput("map")
  )
  
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$text_totals <- renderText({
    paste0("Total number of confirmed cases: ", confirmed, "  |   Total number of deaths: ", deaths, "  |  Total number of recovered: ", recovered)
  })
   
  output$text_updated <- renderText({
    paste0("last updated on: ", date_last_update, "(GMT -5)")
  })
  
  
  
   output$linegraph <- renderPlot({
     
     
     if (input$lgraphoptions == "hubei_china_world") {
       
       all_data_max_day %>%
         mutate(hubei_china_world = ifelse(hubei_ornot == "Hubei", "Hubei",
                                           ifelse(china_ornot == "Mainland China", "Mainland China", "Rest of the world"))) %>% 
         group_by(day, hubei_china_world) %>%
         summarise(confirmed = sum(confirmed, na.rm = TRUE)) %>% 
         ggplot(aes(x = day, y = confirmed, color = hubei_china_world, label = confirmed)) +
         geom_label_repel(data = plot_labels1, aes(x = day, y = confirmed, color = hubei_china_world, label = confirmed), 
                          fill = "white", nudge_x = -0.5, nudge_y = +200) +
         geom_point() +
         geom_line(key_glyph = "timeseries") +
         guides(colour = guide_legend(override.aes = list(size = 1.3)))+
         ggtitle("Progression of confirmed cases of the 2019-ncov virus") +
         ylab("Confirmed Cases") +
         theme_minimal() +
         theme(legend.title = element_blank(),
               legend.text = element_text(size = 14)) +
         guides(label = FALSE)
       
     }else if(input$lgraphoptions == "china_ornot"){
       
       all_data_max_day %>%
         group_by(day, china_ornot) %>%
         summarise(confirmed = sum(confirmed, na.rm = TRUE)) %>% 
         ggplot(aes(x = day, y = confirmed, color = china_ornot, label = confirmed)) +
         geom_label_repel(data = plot_labels2, aes(x = day, y = confirmed, color = china_ornot, label = confirmed), 
                          fill = "white", nudge_x = -0.5, nudge_y = +200) +
         geom_point() +
         geom_line(key_glyph = "timeseries") +
         guides(colour = guide_legend(override.aes = list(size = 1.3)))+
         ggtitle("Progression of confirmed cases of the 2019-ncov virus") +
         ylab("Confirmed Cases") +
         theme_minimal() +
         theme(legend.title = element_blank(),
               legend.text = element_text(size = 16, face = "bold")) +
         guides(label = FALSE)
       
     }else {
       
       all_data_max_day %>%
         group_by(day) %>%
         summarise(confirmed = sum(confirmed, na.rm = TRUE)) %>% 
         ggplot(aes(x = day, y = confirmed, color = "day", label = confirmed)) +
         geom_label_repel(data = plot_labels3, aes(x = day, y = confirmed, color = "day", label = confirmed), 
                          fill = "white", nudge_x = -0.5, nudge_y = +200) +
         geom_point() +
         geom_line(key_glyph = "timeseries") +
         guides(colour = guide_legend(override.aes = list(size = 1.3)))+
         ggtitle("Progression of confirmed cases of the 2019-ncov virus") +
         ylab("Confirmed Cases") +
         theme_minimal() +
         theme(legend.position="none")
     }

   })
}

# Run the application 
shinyApp(ui = ui, server = server)

