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
      dplyr::select(.data$athlete_name) |>
      dplyr::distinct(.data$athlete_name)
  })

  #### Add conference options for graph ####
  observe({
    updateSelectizeInput(
      session,
      "player_input_game_plot",
      choices = player_input(),
      selected = NULL,
      server = TRUE
    )
  })

  #### Add conference options for graph ####

  observe({
    shinyWidgets::updatePickerInput(
      session,
      "metric_player_game_plot",
      choices = colnames(betting_player_stats_raw()[82:99]),
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

  # #### Download the Team Plot ####
  # output$player_game_plot_download <- downloadHandler(
  #   filename = function() {
  #     paste0(glue::glue("player_game_stats"), ".png")
  #   },
  #   # content is a function with argument file. content writes the plot to the device
  #   content = function(file) {
  #     ggplot2::ggsave(file, plot = download_team_proe_plot(pool, input$season_team_proe_graph, input$week_team_proe_graph, input$down_team_proe_graph, input$wp_team_proe_graph, input$conference_team_proe_graph, input$team_proe_graph, input$average_team_proe_graph), width = 16, height = 9) # for GGPLOT
  #   }
  # )
#
#   filter_neutral_conf <- reactive({
#     filter_conf_team() |>
#       dplyr::select(.data$conference) |>
#       dplyr::distinct(.data$conference) |>
#       dplyr::arrange(.data$conference)
#   })
#
#   #### Add conference options for graph ####
#   observe({
#     shinyWidgets::updatePickerInput(
#       session,
#       "conference_team_neutral_graph",
#       choices = filter_neutral_conf()$conference,
#       selected = c("SEC", "Big 12", "Big Ten", "Pac-12", "ACC")
#     )
#   })
#
#   #### Add conference options for graph ####
#
#   filter_neutral_teams <- reactive({
#     filter_conf_team() |>
#       dplyr::filter(.data$conference %in% !!input$conference_team_neutral_graph) |>
#       dplyr::select(.data$pos_team) |>
#       dplyr::arrange(.data$pos_team) |>
#       dplyr::distinct(.data$pos_team)
#   })
#
#   observe({
#     shinyWidgets::updatePickerInput(
#       session,
#       "team_neutral_graph",
#       choices = filter_neutral_teams()$pos_team,
#       selected = NULL
#     )
#   })
#
#   shinyjs::onclick(
#     "toggle_neutral_situation_settings",
#     shinyjs::toggle(id = "more_neutral_situation_settings", anim = TRUE)
#   )
#
#   #### Print the ggplot to Team Graph Tab ####
#   output$team_neutral_plot <- ggiraph::renderGirafe({
#     input$neutral_situation_submit
#     isolate({
#       req(input$conference_team_neutral_graph, input$team_neutral_graph)
#       ggiraph::girafe(
#         ggobj = output_team_neutral_plot(pool, input$season_team_neutral_graph, input$week_team_neutral_graph, input$down_team_neutral_graph, input$wp_team_neutral_graph, input$conference_team_neutral_graph, input$team_neutral_graph, input$average_team_neutral_graph),
#         width_svg = 9,
#         height_svg = 5,
#         options = list(
#           ggiraph::opts_selection(type = "single", only_shiny = FALSE),
#           ggiraph::opts_hover_inv(css = "opacity:0.5")
#         )
#       )
#     })
#   })
#
#   #### Download the Team Plot ####
#   output$team_neutral_download <- downloadHandler(
#     filename = function() {
#       paste0(glue::glue("team_neutral_rate"), ".png")
#     },
#     # content is a function with argument file. content writes the plot to the device
#     content = function(file) {
#       ggplot2::ggsave(file, plot = download_team_neutral_plot(pool, input$season_team_neutral_graph, input$week_team_neutral_graph, input$down_team_neutral_graph, input$wp_team_neutral_graph, input$conference_team_neutral_graph, input$team_neutral_graph, input$average_team_neutral_graph), width = 16, height = 9) # for GGPLOT
#     }
#   )
#
#   #### Add conference options for team graph ####
#   observe({
#     shinyWidgets::updatePickerInput(
#       session,
#       "conference_team_graph",
#       choices = filter_neutral_conf()$conference,
#       selected = c("SEC", "Big 12", "Big Ten", "Pac-12", "ACC")
#     )
#   })
#
#   #### Add coach options for team graph ####
#
#   # advanced_stats <- reactive({
#   #   c2cbucket <- aws.s3::get_bucket(bucket = "campus2canton", region = "")
#   #
#   #   advanced_stats <- aws.s3::s3readRDS(object = "app_data/advanced_stats.rds", bucket = c2cbucket, region = "")
#   # })
#   filter_plot_conf <- reactive({
#     filter_conf_team() |>
#       dplyr::filter(
#         .data$conference %in% !!input$conference_team_graph,
#         .data$year %in% !!input$season_team_graph
#       ) |>
#       dplyr::select(.data$pos_team) |>
#       dplyr::distinct(.data$pos_team) |>
#       dplyr::arrange(.data$pos_team)
#   })
#
#   filter_plot_teams <- reactive({
#     filter_conf_team() |>
#       dplyr::filter(
#         .data$conference %in% !!input$conference_team_graph,
#         .data$year %in% !!input$season_team_graph
#       ) |>
#       dplyr::select(.data$pos_team) |>
#       dplyr::distinct(.data$pos_team) |>
#       dplyr::arrange(.data$pos_team)
#   })
#
#   advanced_stats_axis <- reactive({
#     pool |>
#       dplyr::tbl("advanced_stats") |>
#       dplyr::slice_min(.data$Team, n = 1) |>
#       dplyr::collect()
#   })
#
#   #### Add y axis options for coach graph ####
#   observe({
#     shinyWidgets::updatePickerInput(
#       session,
#       "team_graph",
#       choices = filter_plot_teams()$pos_team,
#       selected = NULL
#     )
#   })
#
#   observe({
#     updateSelectInput(
#       session,
#       "team_y_axis",
#       choices = colnames(advanced_stats_axis()[5:length(colnames(advanced_stats_axis()))]),
#       selected = "Offense EPA/Play"
#     )
#   })
#
#   #### Add x axis options for coach graph ####
#   observe({
#     updateSelectInput(
#       session,
#       "team_x_axis",
#       choices = colnames(advanced_stats_axis()[5:length(colnames(advanced_stats_axis()))]),
#       selected = "Defense EPA/Play"
#     )
#   })
#
#   #### Print the ggplot to Coach Graph Tab ####
#   output$team_plot <- ggiraph::renderGirafe({
#     input$metric_plot_submit
#     isolate({
#       req(input$team_graph)
#       ggiraph::girafe(
#         ggobj = output_team_plot(pool, input$team_x_axis, input$team_y_axis, input$conference_team_graph, input$team_graph, input$season_team_graph, input$average_team_metric_graph),
#         width_svg = 9,
#         height_svg = 5,
#         options = list(
#           ggiraph::opts_selection(type = "single", only_shiny = FALSE),
#           ggiraph::opts_hover_inv(css = "opacity:0.5")
#         )
#       )
#     })
#   })
#
#   #### Download the team Plot ####
#   output$team_download <- downloadHandler(
#     filename = function() {
#       paste0(glue::glue("{input$season_team_graph} team metric plot"), ".png")
#     },
#     # content is a function with argument file. content writes the plot to the device
#     content = function(file) {
#       ggplot2::ggsave(file, plot = download_team_plot(pool, input$team_x_axis, input$team_y_axis, input$conference_team_graph, input$team_graph, input$season_team_graph, input$average_team_metric_graph), width = 16, height = 9) # for GGPLOT
#     }
#   )
#
#   filter_table_team <- reactive({
#     filter_conf_team() |>
#       dplyr::select(.data$pos_team) |>
#       dplyr::distinct(.data$pos_team) |>
#       dplyr::arrange(.data$pos_team)
#   })
#
#   #### Add team options for table ####
#   observe({
#     updateSelectizeInput(
#       session,
#       "team_table",
#       choices = filter_table_team()$pos_team,
#       selected = NULL,
#       server = TRUE
#     )
#   })
#
#   #### Add metric options for table ####
#   observe({
#     shinyWidgets::updatePickerInput(
#       session,
#       "metric_team_table",
#       choices = colnames(advanced_stats_axis()[1:length(colnames(advanced_stats_axis()))]),
#       selected = c("Season", "Team", "Offense Pass Rate", "Offense Pass EPA/Play", "Offense Pass Success Rate", "Offense Rush EPA/Play", "Offense Rush Success Rate")
#     )
#   })
#
#   #### Create Table ####
#   output$metric_team_table <- reactable::renderReactable(
#     reactable::reactable(
#       {
#         req(input$metric_team_table)
#
#         team_table(pool, input$team_table, input$metric_team_table)
#       },
#       filterable = TRUE,
#       defaultColDef = reactable::colDef(sortNALast = TRUE),
#       showPageSizeOptions = TRUE
#     )
#   )
#
#   output$downloadData <- downloadHandler(
#     filename = function() {
#       paste("college-team-data-", Sys.Date(), ".csv", sep="")
#     },
#     content = function(file) {
#       utils::write.csv(team_table(pool, input$team_table, input$metric_team_table), file)
#     })

  # Stop the app timing out
  autoInvalidate <- reactiveTimer(10000)
  observe({
    autoInvalidate()
    cat(".")
  })
}
