#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom rlang .data
#' @importFrom dplyr tbl collect distinct arrange select filter slice_min
#' @importFrom pool dbPool poolClose
#' @importFrom RPostgres Postgres
#' @importFrom sever sever
#' @importFrom shinyWidgets updatePickerInput
#' @importFrom ggiraph renderGirafe girafe opts_selection opts_hover_inv
#' @importFrom shinyjs onclick toggle
#' @importFrom reactable renderReactable reactable colDef
#' @importFrom ggplot2 ggsave
#' @importFrom aws.s3 get_bucket s3readRDS
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  sever::sever()

  betting_player_stats_raw <- reactive({
    c2cbucket <- aws.s3::get_bucket(bucket = "campus2canton", region = "")

    betting_player_stats_raw <- aws.s3::s3readRDS(object = "app_data/betting_game_stats.rds", bucket = c2cbucket, region = "")
  })

  future_games <- reactive({
    c2cbucket <- aws.s3::get_bucket(bucket = "campus2canton", region = "")

    betting_player_future_games <- aws.s3::s3readRDS(object = "app_data/prop_future_games.rds", bucket = c2cbucket, region = "")
  })

  filter_seasons <- reactive({
    betting_player_stats_raw() |>
      dplyr::select(.data$season) |>
      dplyr::distinct(.data$season) |>
      dplyr::arrange(dplyr::desc(.data$season))
  })

  filter_weeks <- reactive({
    betting_player_stats_raw() |>
      dplyr::filter(.data$season == input$season_player_game_plot) |>
      dplyr::select(.data$week) |>
      dplyr::distinct(.data$week) |>
      dplyr::arrange(.data$week)
  })

#### Add conference options for graph ####
observe({
  updateSliderInput(
    session,
    "week_player_game_plot",
    min = min(filter_weeks()),
    max = max(filter_weeks()),
    value = c(min(filter_weeks()), max(filter_weeks())),
    step = 1,
  )
})

  player_input <- reactive({
    betting_player_stats_raw() |>
      dplyr::filter(.data$season == input$season_player_game_plot) |>
      dplyr::arrange(dplyr::desc(.data$`Fantasy Score`)) |>
      dplyr::select(.data$athlete_name) |>
      dplyr::distinct(.data$athlete_name)
  })

  #### Add conference options for graph ####
  observe({
    updateSelectizeInput(
      session,
      "player_input_game_plot",
      choices = player_input()$athlete_name,
      selected = NULL,
      server = TRUE
    )
  })

  #### Add conference options for graph ####

  observe({
    shinyWidgets::updatePickerInput(
      session,
      "metric_player_game_plot",
      choices = colnames(betting_player_stats_raw() |>
                           dplyr::select(.data$`Fantasy Score`:.data$`Kick Ret TDs`)),
      selected = NULL
    )
  })

  observe({
    updateNumericInput(
      session,
      "threshold_player_game_plot",
      value = NA,
      min = -50,
      max = 500,
      step = .5
    )
  })

  #### Print the ggplot to Team Graph Tab ####
  output$player_game_plot <- ggiraph::renderGirafe({
    req(input$player_input_game_plot, input$threshold_player_game_plot)
      ggiraph::girafe(
        ggobj = output_player_game_plot(betting_player_stats_raw(),
                                        future_games(),
                                        input$season_player_game_plot,
                                        input$week_player_game_plot,
                                        input$player_input_game_plot,
                                        input$metric_player_game_plot,
                                        input$threshold_player_game_plot),
        width_svg = 9,
        height_svg = 5,
        options = list(
          ggiraph::opts_selection(type = "single", only_shiny = FALSE),
          ggiraph::opts_hover_inv(css = "opacity:0.5")
        )
      )
    })

#### Download the Team Plot ####
output$player_game_plot_download <- downloadHandler(
  filename = function() {
    paste0(glue::glue("player_game_stats"), ".png")
  },
  # content is a function with argument file. content writes the plot to the device
  content = function(file) {
    ggplot2::ggsave(file, plot = download_player_game_plot(betting_player_stats_raw(),
                                                         future_games(),
                                                         input$season_player_game_plot,
                                                         input$week_player_game_plot,
                                                         input$player_input_game_plot,
                                                         input$metric_player_game_plot,
                                                         input$threshold_player_game_plot), width = 16, height = 9) # for GGPLOT
  }
)

  # Stop the app timing out
  autoInvalidate <- reactiveTimer(10000)
  observe({
    autoInvalidate()
    cat(".")
  })
}
