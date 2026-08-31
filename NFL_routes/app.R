
# sidebar format for NFL route map app
ui <- page_sidebar(
            
          title = "NFL route maps from 2018 season",
          
          sidebar = sidebar(
            selectInput("player", "Player:", c("Cooper Kupp (LAR)" = "ck",
                                               "Julio Jones (ATL)" = "jj",
                                               "Davante Adams (GB)" = "da")
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
          
           plotOutput("routes"),
            textOutput("test")
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

}


shinyApp(ui = ui, server = server)
