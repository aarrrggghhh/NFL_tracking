library(shiny)
library(bslib)
library(tidyverse)

source("helpers.R", local = TRUE)

# get list of files from the data folder, and read the data into local objects.
# The project has two files for each player, a table of tracking data (##2018app.rds), and a table of play outcomes when they were targeted (##2018app_results).
file_paths <- list.files(path = './data', pattern = '.rds', full.names = TRUE)
file_names <- gsub(pattern = "\\.rds$", replacement = "", x = basename(file_paths))
for(i in 1:length(file_names)) {
  assign(file_names[i], readRDS(file_paths[i]))
}

# create manifest of players for which we have data. First list only the base files (##2018app)
base_file_names <- file_names[!str_detect(file_names, "result")]

# pull various versions of player name, and their teams into small tibble
player_list <- map_vec(1:length(base_file_names), function(x) {
  tibble(fileName = paste0(base_file_names[x], ".rds"), 
         dataElement = base_file_names[x], 
         resultElement = paste0(base_file_names[x], "_results"), 
         fullName = get(base_file_names[x])$displayName[1], 
         shortName = get(paste0(base_file_names[x], "_results"))$target[1], 
         team = get(paste0(base_file_names[x], "_results"))$possessionTeam[1],
         prefix = str_sub(base_file_names[x], 1, 2))
})

# create player name list to provide to player selection drop down menu
pl_inp_list <- setNames(as.list(player_list$prefix), paste0(player_list$fullName, " (", player_list$team, ")"))


# sidebar format for NFL route map app
ui <- page_sidebar(
            
          title = "NFL route maps from 2018 season",
          
          sidebar = sidebar(
            selectInput("player", "Player:", pl_inp_list
                        ),
            
            radioButtons("plot_type", "Plot type:",
                         choices = c("Routes" = "lines", "Density" = "density"), inline = TRUE
                        ),
                        
            # Show these inputs only when plotting actual route maps (not density)
            conditionalPanel("input.plot_type == 'lines'",
                            card(
                               card_header("Plot parameters"),
                               div(
                                 tags$style(".checkbox input {     width: .9em;
                                                                   height: .9em;
                                                                  }
                                            label {font-size: .8rem;}"
                                            ),
                                     checkboxGroupInput("weeks",
                                                "Weeks:",
                                                choices = c(seq(1,17,1)),
                                                selected = 1,
                                                inline = TRUE
                                      ),
                                 div(
                                   tags$style(".btn {--bs-btn-font-size: 80%;
                                                    --bs-btn-padding-y: 2px;
                                                    --bs-btn-padding-x: 8px;
                                              }"),
                                   actionButton("all", "All"),
                                   actionButton("none", "None")
                                 ),
                                 br(),
                                      checkboxInput("outcomes",
                                           "Show play outcomes",
                                           value = FALSE
                                      )
                                  ),
                                actionButton("redraw", "Update")
                             
                           )
          )
      ),
        layout_columns(
           plotOutput("routes"),
           col_widths = c(10, -2)
      )
)



server <- function(input, output, session) {

# select all weeks button  
    observeEvent(input$all, {
      updateCheckboxGroupInput(session, "weeks", selected = c(seq(1,17,1)))
    })
  
# select no weeks button
    observeEvent(input$none, {
      updateCheckboxGroupInput(session, "weeks", selected = 0)
    })
    
    output$routes <- renderPlot({
            switch(input$plot_type,
                   "lines" = plot_routes(input$player, input$weeks, isTRUE(input$outcomes)),
                   "density" = plot_density(input$player)
           )
    })  |>
      bindEvent(c(input$redraw, input$plot_type, input$player), ignoreNULL = TRUE, ignoreInit = TRUE)

#    output$test <- renderText({
#      input$player
#    })
}


shinyApp(ui = ui, server = server)
