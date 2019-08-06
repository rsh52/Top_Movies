library(shiny)
library(tidyverse)
library(rvest)
library(knitr)
library(kableExtra)
library(highcharter)
library(viridisLite)


ui <- fluidPage(
    
    # Application title
    titlePanel("Top 50 IMDB Movies by User Rating"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(width = 1,
                     checkboxGroupInput(inputId = "choicefilter", 
                                        label = "Select Movie Genre(s)", inline = FALSE,
                                        choices = sort(c("Drama", "Crime", "Action",
                                                         "Adventure", "Fantasy", 
                                                         "Biography", "History", "Sci-Fi",
                                                         "Romance", "Western", 
                                                         "Animation", "Family",
                                                         "War", "Comedy",
                                                         "Mystery", "Thriller",
                                                         "Music", "Horror")), 
                     )
        ),
        
        fluidRow(
            column(4,
                   tableOutput("movietablet")# Total movie table
            ), 
            column(4,
                   tableOutput("movietablef")  # Filtered movie table
            )
        )
    ),
    
    fluidRow(column(width = 5, highchartOutput("movieratingspie", height = "400px")),
             column(width = 4, highchartOutput("moviegenrespie", height = "400px"))),
    
    fluidRow(column(width = 2,textOutput("InfoBox")))
    
)



server <- function(input, output) {
    
    source("top_movies.R") # Load all of the variables from the top_movies.r file
    
    # Create the movie_table from the variables declared in source()
    movie_table <- data.frame(
        Rank = seq(1, length(year), 1),
        Ratings = ratings,
        Titles = titles,
        Year = year,
        Runtime = runtimes,
        Genre = genre, 
        Certificate = Certificate)
    
    movie_table_t <- movie_table %>% 
        mutate(
            Ratings = cell_spec(x = Ratings, format = "html", bold = T, 
                                color = "white", 
                                background = ifelse(Ratings > mean(Ratings), "#66bf3f", "#0e5a9b"))
        ) %>%
        kable(escape = F) %>% # NOTE must have "escape = F" for HTML to render
        kable_styling(bootstrap_options = c("striped", "hover"), full_width = F)
    
    
    # Create a table that is reactive based on OR conditions for the 3 filter genre inputs
    movie_table_f <- reactive({
        
        validate(
            need(input$choicefilter != "", "Please select a movie genre")
        )
        
        # Apply filters here
        movie_table_f <- movie_table %>% filter(grepl(paste(input$choicefilter, collapse = "|"), Genre))
        
        # Feed the movie table through kableExtra to produce pretty output
        movie_table_f <- movie_table_f %>% 
            mutate(
                Ratings = cell_spec(x = Ratings, format = "html", bold = T, 
                                    color = "white", 
                                    background = ifelse(Ratings > mean(Ratings), "#66bf3f", "#0e5a9b"))
            ) %>%
            kable(escape = F) %>% # NOTE must have "escape = F" for HTML to render
            kable_styling(bootstrap_options = c("striped", "hover"), full_width = F)
    })
    
    # Output for total movietable
    output$movietablet <- function(){
        movie_table_t %>% 
            scroll_box(width = "550px", height = "400px")
    }
    
    # Output for filtered movie_table_f
    output$movietablef <- function(){
        movie_table_f() %>% 
            scroll_box(width = "550px", height = "400px")
    }
    
    output$InfoBox <- renderText({
        "This application scrapes web data from IMDB's top 250 movies by user
        ratings."
    })
    
    output$movieratingspie <- renderHighchart({
        # Pie Chart Construction================================================
        
        clrs <- c("#E45402", "#E48D02", "#0E4B95", "#02966D", "#B70268")
        
        MovieRatings.pie <- cbind.data.frame(titles, Certificate) %>% 
            group_by(Certificate) %>% 
            tally() %>% 
            hchart(type = "pie", hcaes(x = Certificate, y = n)) %>% 
            hc_title(text = "<b>Movie Ratings</b>",
                     margin = 20, align = "center",
                     style = list(color = "##000000", useHTML = TRUE)) %>% 
            hc_colors(clrs)
      
        MovieRatings.pie  
    })
    
    output$moviegenrespie <- renderHighchart({
        # Pie Chart Construction================================================
        
        genrepie.df <- data.frame(unlist(strsplit(as.character(genre), ",")))
        colnames(genrepie.df) <- c("genres")
        genrepie.df$genres <- trimws(genrepie.df$genres, which = c("both", "left", "right"), whitespace = "[ \t\r\n]")
        
        clrs2 <- viridis(n = length(unique(genrepie.df$genres)))
        
        MovieGenres.pie <-  genrepie.df %>% 
            group_by(genres) %>% 
            tally() %>% 
            hchart(type = "pie", hcaes(x = genres, y = n)) %>% 
            hc_title(text = "<b>Movie Genres</b>",
                     margin = 20, align = "center",
                     style = list(color = "##000000", useHTML = TRUE)) %>% 
            hc_colors(clrs2)
        
        MovieGenres.pie 

    })
    
}


# Run the application 
shinyApp(ui = ui, server = server)
