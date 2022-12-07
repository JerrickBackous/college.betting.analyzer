#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom bslib bs_theme font_google
#' @importFrom shinyjs useShinyjs hidden
#' @importFrom waiter use_waiter autoWaiter spin_hexdots transparent
#' @importFrom sever useSever
#' @importFrom shinyWidgets awesomeCheckbox pickerInput
#' @importFrom ggiraph girafeOutput
#' @importFrom reactable reactableOutput
#' @importFrom htmltools HTML
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    fluidPage(
      theme = bslib::bs_theme(
        bg = "#FFF",
        fg = "#6c0000",
        primary = "#6c0000",
        secondary = "#6c0000",
        base_font = bslib::font_google("Roboto")
      ),
      tags$head(
        tags$style(
          type = "text/css",
          ".well {
               background-color: #FFF;
               }",
          ".btn-light {
               color: #984D4D;
               background-color: #FFF;
               border-color: #C49999;
               }",
          "text {
               font-family: Roboto
               }"
        )
      ),
      # shinyjs::useShinyjs(),
      waiter::use_waiter(),
      waiter::autoWaiter(html = waiter::spin_hexdots(), color = waiter::transparent(0.3)),
      sever::useSever(),
      # p(style = "text-align: right;", paste0("Updated: ", update_date())),
      # p(style = "text-align: right;", paste0("Updated: ", update_date)),
      tabsetPanel(
        type = "tabs",
        tabPanel(
          tabName = "player_stat_history", title = "Player Prop Plot",
          sidebarLayout(
            sidebarPanel(
              width = 4,
              fluidRow(
                column(
                  width = 12,
                  sliderInput(inputId = "season_player_game_plot",
                              label = "Select Season",
                              min = 2014,
                              max = most_recent_season(),
                              value = most_recent_season(),
                              step = 1
                  )
                )
              ),
              fluidRow(
                column(
                  width = 12,
                  sliderInput(inputId = "week_player_game_plot",
                              label = "Select Weeks",
                              min = 1,
                              max = 17,
                              value = c(1,17),
                              step = 1
                  )
                )
              ),
              fluidRow(
                column(
                  width = 12,
                  selectizeInput("player_input_game_plot",
                                 "Select Player",
                                 choices = NULL,
                                 multiple = FALSE
                  )
                )
              ),
              fluidRow(
                column(
                  width = 12,
                  shinyWidgets::pickerInput("metric_player_game_plot",
                                            "Select Statistic",
                                            choices = NULL,
                                            multiple = FALSE
                  )
                )
              ),
              fluidRow(
                column(
                  width = 12,
                  numericInput("threshold_player_game_plot",
                               "Input Prop Value",
                               value = NULL,
                               min = -50,
                               max = 500,
                               step = .5
                  )
                )
              ),
              fluidRow(
                column(
                  width = 12,
                  br(),
                  downloadButton(outputId = "player_game_plot_download", label = "Save Plot")
                )
              )
            ),
            mainPanel(
              fluidRow(
                width = 8,
                ggiraph::girafeOutput("player_game_plot")
              )
            )
          )
        )#,
        # tabPanel(
        #   tabName = "player_game_table", title = "Player Game Table",
        #   br(),
        #   fluidRow(
        #     column(
        #       width = 6,
        #       selectizeInput("player_game_table",
        #                      label = "Select Team(s)",
        #                      choices = NULL,
        #                      multiple = TRUE
        #       )
        #     ),
        #     column(
        #       width = 6,
        #       shinyWidgets::pickerInput("metric_team_table",
        #                                 label = "Select Metric(s)",
        #                                 choices = NULL,
        #                                 options = list(`actions-box` = TRUE, `live-search` = TRUE),
        #                                 multiple = TRUE
        #       )
        #     )
        #   ),
        #   fluidRow(
        #     column(
        #       width = 12,
        #       reactable::reactableOutput("metric_team_table")
        #     )
        #   ),
        #   fluidRow(
        #     column(
        #       offset = 9,
        #       width = 3,
        #       align = "right",
        #       downloadButton('downloadData', 'Download data')
        #     )
        #   )
        # )
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(ext = "png"),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "college.betting.analyzer"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
