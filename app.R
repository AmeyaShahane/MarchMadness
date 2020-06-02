#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# install.packages(c("shiny", "ggplot2", "wordcloud2", "dplyr", "ggthemes"))
library(shiny)
library(ggplot2)
library(wordcloud2)
library(dplyr)
library(ggthemes)

Final_data = readRDS("Final_data.rds")
wordcount_combined = readRDS("wordcount_combined.rds")

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Twitter Usage during NCAA 2019"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId = "games",
                        label = "Choose a game:",
                        choices = c(
                            "Texas Tech Vs Michigan" = "TexasTech_vs_Michigan",
                            "Virginia Tech Vs Duke" = "VirginiaTech_vs_Duke",
                            "Purdue Vs Virginia" = "Purdue_vs_Virginia",
                            "Auburn Vs Kentucky" = "Auburn_vs_Kentucky",
                            "Auburn Vs Virginia" = "Auburn_vs_Virginia",
                            "Texas Tech Vs Michigan State" = "TexasTech_vs_MichiganState",
                            "Texas Tech Vs Virginia" = "TexasTech_vs_Virginia"
                        )),
            actionButton("button",
                         "Show me the stats!")
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(
                tabPanel("Graph", plotOutput(outputId = "graph", click = "plot_click"),
                         fluidRow(
                             column(width = 12,
                                    h4("Points near click"),
                                    tableOutput("click_info")
                             )
                         )),
                tabPanel("Summary", verbatimTextOutput(outputId = "summary"), helpText("Our multiple-regression linear model estimates the number of twitter followers for a user based on their statuses count, friends count, and favourties count.")),
                tabPanel("Word Cloud", wordcloud2Output(outputId = "wordcloud"))
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    #actives
    active_data = eventReactive(input$button, {
        Final_data
    })
    
    active_word = eventReactive(input$button, {
        wordcount_combined
    })
    
    active_group = eventReactive(input$button, {
        active_data()[active_data()$GameName == isolate(input$games),] %>%
            group_by(timeinterval) %>%
            summarise(Frequency_count = n()) %>%
            mutate(numeric_timeinterval = as.POSIXct(timeinterval))
    })
    
    active_model = 
        eventReactive(input$button,{
            lm(followers_count ~ statuses_count + friends_count + favourites_count, data = active_data()[active_data()$GameName == (input$games),])
        })
    
    active_click = eventReactive(input$plot_click,{
        selected_point = nearPoints(active_group(), input$plot_click, addDist = TRUE)
        if(is.null(selected_point)|nrow(selected_point) == 0){
            stop("Please select a valid point on the graph!")
        }else(selected_point)
    })
    
    #graph
    output$graph <- renderPlot({
        input$button
        
    #plot graph
       ggplot(data = active_group())+
            aes(x = numeric_timeinterval, y = Frequency_count)+
            geom_point()+
            geom_line(color = "steelblue")+
            labs(x = "time", y = "No. of Tweets")+
            theme_economist()+
            scale_colour_economist()
    }) 
    
    #points near click
    output$click_info <- renderTable({
        
        active_click()
        
        active_data() %>% 
            filter(timeinterval == active_click()$timeinterval) %>%
            arrange(desc(followers_count)) %>%
            select(c("Tweets" = text,"Time stamp" = timeinterval,"Number of follwers" = followers_count)) %>%
            head(5)
    })
    #summary
    output$summary <- renderPrint({
        input$button
        summary(active_model())
    })
    
    #wordcloud
    output$wordcloud <- renderWordcloud2({
        input$button
        wordcloud2(
            active_word()[active_word()$GameName == isolate(input$games),],
            size = 1.5,
            color = rep_len(c("steelblue","skyblue","royalblue","cornflowerblue"), nrow(active_word())),
            shape = "circle"
        )
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
