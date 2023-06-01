# Title: app.R
# Author: Mackenzie Ross
# Description: Building the user interface and server of the "IMDb Top 250 Explorer" web app
# Date Last Modified: 16 May 2023

# import necessary libraries
library(shiny)
library(shinythemes)
library(DBI)
library(RMySQL)
library(RSQLite)
library(tidyverse)
library(odbc)

# connect to database
conn <- dbConnect(RSQLite::SQLite(), dbname = "IMDbTop250.db")
# conn <- dbConnect(MySQL(),
#                   dbname = "imdb_top250",
#                   user = "adt_final",
#                   password = "final",
#                   host = "localhost",
#                   unix.socket = '/Applications/MAMP/tmp/mysql/mysql.sock',
#                   port = 3306)

# conn <- dbConnect(odbc(), Driver = "SQLite Driver", Database = "imdb_top250", 
#                   UID = "adt_final", PWD = "final", host = "localhost", Port = 3306)

# get list of movies, directors, writers, and actors
movieChoices <- dbGetQuery(conn, "SELECT title FROM movies ORDER BY title;")
directorChoices <- dbGetQuery(conn, "SELECT name FROM directors ORDER BY name;")
writerChoices <- dbGetQuery(conn, "SELECT name FROM writers ORDER BY name;")
actorChoices <- dbGetQuery(conn, "SELECT name FROM actors ORDER BY name;")

# define user interface (UI) for app
ui <- fluidPage(theme = shinytheme("sandstone"),
  navbarPage(
    # app title
    "IMDb Top 250 Explorer",
    # Home Page
    tabPanel("Home",
             mainPanel(width = 12, style = "border-style: solid; border-color: black",
               h1("Welcome to the IMDb Top 250 Explorer!", align="center"),
               br(),
               HTML('<center><img src="imdb_logo.png" height = "100" width = "200"></center>'),
               br(),
               h2("What is IMDb?"),
               HTML('<h4>IMDb stands for <strong>I</strong>nternet <strong>M</strong>ovie <strong>D</strong>ata<strong>b</strong>ase. It is a popular website for people to find information about their favorite movies and TV shows.</h4>'),
               HTML('<h4>To visit the IMDb website: <a href="https://www.imdb.com" target="_blank" rel="noopener noreferrer">Click here!</a></h4>'),
               br(),
               h2("What is the IMDb Top 250 Explorer?"),
               HTML('<h4>The goal of this web app is to allow movie lovers to learn more about the top 250 highest rated films on IMDb. On the <strong>Dashboard</strong> page users are able to see plots that summarize data about the 250 top rated movies on IMDb. Users are able to filter the data used to generate the plots. On the <strong>Pick a Movie</strong> page, users are able to select a movie and infomation about the movie will be output on the page.</h4>')
             )
    ),
    
    # Dashboard Page
    tabPanel("Dashboard",
             # sidebar
             sidebarPanel(
               h4("Instructions:"),
               helpText("Use the options to change the outputs of the charts on the 'Ratings and Genres' tab and the tables on the 'Director', 'Writer', and 'Actor' tabs."),
               sliderInput("movieRanks", strong("Choose the ranks of movies to display"),
                           min = 0, max = 250, value = 250),
               numericInput("n_directors", strong("Number of Directors to Display"),
                            value = 10, min = 1),
               numericInput("n_writers", strong("Number of Writers to Display"),
                            value = 10, min = 1),
               numericInput("n_actors", strong("Number of Actors to Display"),
                            value = 10, min = 1),
               h4("Note:"),
               HTML("<p><strong>MPAA Rating:</strong> Value assigned to a movie by the Motion Picture Association to indicate what audience is the movie is best suited for")
             ),
             
             # main panel
             mainPanel(
               tabsetPanel(type = "tabs",
                           tabPanel("Ratings and Genres", fluidRow(
                             column(6, plotOutput("ratingsBar")),
                             column(6, plotOutput("genrePie"))
                           )),
                           tabPanel("Directors", 
                                    fluidRow(column(4, uiOutput("topNDirectors")), 
                                             column(8, h3("Director Search"), 
                                                    p("Select a director from the drop down to view the movies they directed."))),
                                    fluidRow(column(4, tableOutput("topDirectors")), 
                                             column(8, selectInput("directorSel", "Choose a Director", choices = directorChoices),
                                                    tableOutput("selectedDirector")))
                           ),
                           tabPanel("Writers", 
                                    fluidRow(column(4, uiOutput("topNWriters")),
                                             column(8, h3("Writer Search"),
                                                    p("Select a writer from the drop down to view the movies they wrote."))),
                                    fluidRow(column(4, tableOutput("topWriters")),
                                             column(8, selectInput("writerSel", "Choose a Writer", choices = writerChoices),
                                                    tableOutput("selectedWriter")))
                           ),
                           tabPanel("Actors", 
                                    fluidRow(column(4, uiOutput("topNActors")), 
                                             column(8, h3("Actor Search"),
                                                    p("Select a actor from the drop down to view the movies they acted in."))),
                                    fluidRow(column(4, tableOutput("topActors")), 
                                             column(8, selectInput("actorSel", "Choose a Actor", choices = actorChoices),
                                                    tableOutput("selectedActor")))
                           )
               )
             )
             ),
    
    # Pick a Movie Page
    tabPanel("Pick a Movie",
             sidebarPanel(
               p("Select a movie from the list. Information about the movie will be displayed to the right."),
               uiOutput("PAMinput")
             ),
             mainPanel(
               h2("Movie Information"),
               p("After you select a movie from the dropdown menu in the sidebar important infomation about the movie will be displayed below. If you wish to visit the IMDb page for your chosen movie, just click on the title of the movie."),
               br(),
               uiOutput(outputId = "movieOverview"),
               uiOutput(outputId = "movieIMDBRankOutput"),
               uiOutput(outputId = "movieReleaseYearOutput"),
               uiOutput(outputId = "movieMPAAOutput"),
               uiOutput(outputId = "movieRuntimeOutput"),
               uiOutput(outputId = "moviePlotOutput"),
               uiOutput(outputId = "movieGenreOutput"),
               uiOutput(outputId = "movieDirectorOutput"),
               uiOutput(outputId = "movieWriterOutput"),
               uiOutput(outputId = "movieCastOutput")
             )
    )
  )
)

# define server logic to run app
server <- function(input, output, session) {
  # Dashboard Page
  numDirectors <- reactive({
    topDirectors <- dbGetQuery(conn, "SELECT name, COUNT(*) AS 'number_of_credits' FROM directors INNER JOIN movie_directors ON directors.director_id=movie_directors.director_id GROUP BY name ORDER BY number_of_credits DESC;")
    head(topDirectors, input$n_directors)
  })
  
  numWriters <- reactive({
    topWriters <- dbGetQuery(conn, "SELECT name, COUNT(*) AS 'number_of_credits' FROM writers INNER JOIN movie_writers ON writers.writer_id=movie_writers.writer_id GROUP BY name ORDER BY number_of_credits DESC;")
    head(topWriters, input$n_writers)
  })
  
  numActors <- reactive({
    topActors <- dbGetQuery(conn, "SELECT name, COUNT(*) AS 'number_of_credits' FROM actors INNER JOIN movie_actors ON actors.actor_id=movie_actors.actor_id GROUP BY name ORDER BY number_of_credits DESC;")
    head(topActors, input$n_actors)
  })
  
  # director table heading
  output$topNDirectors <- renderText({
    num_directors <- input$n_directors
    HTML(paste0("<h3>Top ", num_directors, " Directors</h3>"))
  })
  
  # writer table heading
  output$topNWriters <- renderText({
    num_writers <- input$n_writers
    HTML(paste0("<h3>Top ", num_writers, " Writers</h3>"))
  })
  
  # actor table heading
  output$topNActors <- renderText({
    num_actors <- input$n_actors
    HTML(paste0("<h3>Top ", num_actors, " Actors</h3>"))
  })
  
  # director selection output
  directedMovies <- reactive({
    directed <- dbGetQuery(conn, paste0("SELECT title, imdb_rank FROM movies INNER JOIN movie_directors ON movies.movie_id = movie_directors.movie_id INNER JOIN directors ON movie_directors.director_id = directors.director_id WHERE directors.name = '", input$directorSel, "';"))
    directed
  })
  
  # writer selection output
  writtenMovies <- reactive({
    written <- dbGetQuery(conn, paste0("SELECT title, imdb_rank FROM movies INNER JOIN movie_writers ON movies.movie_id = movie_writers.movie_id INNER JOIN writers ON movie_writers.writer_id = writers.writer_id WHERE writers.name = '", input$writerSel, "';"))
    written
  })
  
  # actor selection output
  actedMovies <- reactive({
    acted <- dbGetQuery(conn, paste0("SELECT title, imdb_rank FROM movies INNER JOIN movie_actors ON movies.movie_id = movie_actors.movie_id INNER JOIN actors ON movie_actors.actor_id = actors.actor_id WHERE actors.name = \"", input$actorSel, "\";"))
    acted
  })
  
  # plots
  # bar chart
  output$ratingsBar <- renderPlot({
    # get count of each type of rating
    maxRank <- input$movieRanks
    ratings <- dbGetQuery(conn, paste0("SELECT mpaa_rating, count(*) AS count from movies WHERE imdb_rank <= ", maxRank, " GROUP BY mpaa_rating;"))
    ratings_filtered <- ratings %>% filter(count > 1)

    ggplot(ratings_filtered, aes(reorder(mpaa_rating, count))) +
      geom_bar(aes(weight = count), fill="slateblue4") + 
      coord_flip() +
      ggtitle("MPAA Ratings in the Top 250") +
      xlab("MPAA Rating") +
      ylab("Number of Movies") +
      theme_bw(base_size = 16)
  })
  output$genrePie <- renderPlot({
    # get count of each type of rating
    maxRank <- input$movieRanks
    genres <- dbGetQuery(conn, paste0("SELECT genre, COUNT(*) AS count FROM genres INNER JOIN movie_genres ON genres.genre_id=movie_genres.genre_id INNER JOIN movies ON movie_genres.movie_id=movies.movie_id WHERE movies.imdb_rank <= ", maxRank, " GROUP BY genre;"))
    # colors for pie chart
    c25 <- c(
      "dodgerblue2", "#E31A1C", # red
      "green4",
      "#6A3D9A", # purple
      "#FF7F00", # orange
      "black", "gold1",
      "skyblue2", "#FB9A99", # lt pink
      "palegreen2",
      "#CAB2D6", # lt purple
      "#FDBF6F", # lt orange
      "gray70", "khaki2",
      "maroon", "orchid1", "deeppink1", "blue1", "steelblue4",
      "darkturquoise", "green1", "yellow4", "yellow3",
      "darkorange4", "brown"
    )
    # pie chart
    ggplot(genres, aes(x="", y=count, fill=genre)) +
      geom_bar(stat="identity", width=1, color="white") +
      coord_polar("y", start=0) +
      ggtitle("Movie Genres") + 
      scale_fill_manual(values = c25) +
      theme_void()
  })
  
  # tables
  # director tab
    output$topDirectors <- renderTable({
      numDirectors()
    })
    output$selectedDirector <- renderTable({
      directedMovies()
    })
  # writer tab
    output$topWriters <- renderTable({
      numWriters()
    })
    output$selectedWriter <- renderTable({
      writtenMovies()
    })
  # actor 
    output$topActors <- renderTable({
      numActors()
    })
    output$selectedActor <- renderTable({
      actedMovies()
    })
  
  # Pick A Movie Page
  # movie options dropdown
  output$PAMinput <- renderUI({
    selectInput(
      "movieTitle", 
      "Choose a Movie",
      choices = movieChoices)
  })
  
  output$movieOverview <- renderText({
    title <- dbGetQuery(conn, paste0("SELECT title FROM movies WHERE title='", input$movieTitle, "';"))
    link <- dbGetQuery(conn, paste0("SELECT link FROM movies WHERE title='", input$movieTitle, "';"))
    HTML(paste0("<h4><strong>Movie Title:</strong> <a href='", link, "/'target='_blank' rel='noopener noreferrer'>", title, "</a></h4>"))
  })
  output$movieIMDBRankOutput <- renderText({
    imdb_rank <- dbGetQuery(conn, paste0("SELECT imdb_rank FROM movies WHERE title='", input$movieTitle, "';"))
    HTML(paste0("<h4><strong>IMDb Rank:</strong> ", imdb_rank))
  })
  output$movieReleaseYearOutput <- renderText({
    year <- dbGetQuery(conn, paste0("SELECT year FROM movies WHERE title='", input$movieTitle, "';"))
    HTML(paste0("<h4><strong>Release Year:</strong> ", year))
  })
  output$moviePlotOutput <- renderText({
    plot <- dbGetQuery(conn, paste0("SELECT plot FROM movies WHERE title='", input$movieTitle, "';"))
    HTML(paste0("<h4><strong>Plot:</strong> ", plot))
  })
  output$movieGenreOutput <- renderText({
    genre <- dbGetQuery(conn, paste0("SELECT genre FROM genres INNER JOIN movie_genres ON genres.genre_id=movie_genres.genre_id INNER JOIN movies ON movie_genres.movie_id=movies.movie_id WHERE title='", input$movieTitle, "';"))
    genre_df <- data.frame(genre) %>% .[[1]] %>% toString()
    HTML(paste0("<h4><strong>Genre(s):</strong> ", genre_df))
  })
  output$movieDirectorOutput <- renderText({
    director <- dbGetQuery(conn, paste0("SELECT name FROM directors INNER JOIN movie_directors ON directors.director_id=movie_directors.director_id INNER JOIN movies ON movie_directors.movie_id=movies.movie_id WHERE title='", input$movieTitle, "';"))
    director_df <- data.frame(director) %>% .[[1]] %>% toString()
    HTML(paste0("<h4><strong>Director(s):</strong> ", director_df))
  })
  output$movieWriterOutput <- renderText({
    writer <- dbGetQuery(conn, paste0("SELECT name FROM writers INNER JOIN movie_writers ON writers.writer_id=movie_writers.writer_id INNER JOIN movies ON movie_writers.movie_id=movies.movie_id WHERE title='", input$movieTitle, "';"))
    writer_df <- data.frame(writer) %>% .[[1]] %>% toString()
    HTML(paste0("<h4><strong>Writer(s):</strong> ", writer_df))
  })
  output$movieCastOutput <- renderText({
   cast <- dbGetQuery(conn, paste0("SELECT name FROM actors INNER JOIN movie_actors ON actors.actor_id=movie_actors.actor_id INNER JOIN movies ON movie_actors.movie_id=movies.movie_id WHERE title='", input$movieTitle, "';"))
   cast_df <- data.frame(cast) %>% .[[1]] %>% toString()
   HTML(paste0("<h4><strong>Cast:</strong> ", cast_df))
  })
  output$movieMPAAOutput <- renderText({
    mpaa_rating <- dbGetQuery(conn, paste0("SELECT mpaa_rating FROM movies WHERE title='", input$movieTitle, "';"))
    HTML(paste0("<h4><strong>MPAA Rating:</strong> ", mpaa_rating))
  })
  output$movieRuntimeOutput <- renderText({
    runtime <- dbGetQuery(conn, paste0("SELECT duration FROM movies WHERE title='", input$movieTitle, "';"))
    HTML(paste0("<h4><strong>Runtime:</strong> ", runtime, " minutes"))
  })
  session$onSessionEnded(function() {
    dbDisconnect(conn)
  })
}

# call shinyApp
shinyApp(ui = ui, server = server)

