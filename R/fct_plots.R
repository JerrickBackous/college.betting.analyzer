#' Player metric plot
#'
#' @param input_df data frame
#' @param input_next_game data frame with future games
#' @param input_season players to filter
#' @param input_weeks positions to filter
#' @param input_player metrics to filter
#' @param input_metric minimum weeks played
#' @param input_threshold minimum weeks played
#'
#' @return Player metric plot
#' @importFrom rlang .data
#' @importFrom dplyr select filter case_when collect
#' @importFrom magick image_read
#' @importFrom cowplot ggdraw draw_plot draw_image
#' @importFrom ggrepel geom_label_repel
#' @importFrom ggiraph geom_point_interactive
#' @importFrom scales alpha
#' @importFrom glue glue
#' @importFrom geomtextpath geom_textsmooth
#' @importFrom stats lm
#' @export
output_player_game_plot <- function(input_df, input_next_game, input_season, input_weeks, input_player, input_metric, input_threshold) {

  player_plot_data <- input_df |>
    dplyr::filter(.data$season %in% input_season,
                  .data$week %in% c(input_weeks[1]:input_weeks[2]),
                  .data$athlete_name %in% input_player
    ) |>
    dplyr::mutate(color = dplyr::case_when(input_metric >= input_threshold ~ "green",
                                    input_metric < input_threshold ~ "red"),
                  EPAP = dplyr::case_when(
                    input_metric %in% c("Pass Completions",
                                        "Pass Attempts",
                                        "Pass Yards",
                                        "Pass TDs",
                                        "Pass Int",
                                        "Receptions",
                                        "Rec Yards",
                                        "Rec TDs") ~ .data$opponent_def_passing_plays_ppa,
                    input_metric %in% c("Rush Carries",
                                        "Rush Yards",
                                        "Rush TDs") ~ .data$opponent_def_rushing_plays_ppa,
                    input_metric %in% c("Fantasy Score",
                                        "Pass & Rush Yards",
                                        "Pass & Rush & Rec TDs",
                                        "Rush & Rec Yards",
                                        "Fumbles Lost",
                                        "Punt Ret TDs",
                                        "Kick Ret TDs") ~ .data$opponent_def_ppa
                  ),
                  SR = dplyr::case_when(
                    input_metric %in% c("Pass Completions",
                                        "Pass Attempts",
                                        "Pass Yards",
                                        "Pass TDs",
                                        "Pass Int",
                                        "Receptions",
                                        "Rec Yards",
                                        "Rec TDs") ~ .data$opponent_def_passing_plays_success_rate,
                    input_metric %in% c("Rush Carries",
                                        "Rush Yards",
                                        "Rush TDs") ~ .data$opponent_def_rushing_plays_success_rate,
                    input_metric %in% c("Fantasy Score",
                                        "Pass & Rush Yards",
                                        "Pass & Rush & Rec TDs",
                                        "Rush & Rec Yards",
                                        "Fumbles Lost",
                                        "Punt Ret TDs",
                                        "Kick Ret TDs") ~ .data$opponent_def_success_rate)) |>
    dplyr::select(.data$week,
                  .data$athlete_id,
                  .data$athlete_name,
                  .data$team,
                  .data$opponent,
                  .data$opponent_def_ppa,
                  .data$opponent_def_success_rate,
                  .data$opponent_def_rushing_plays_ppa,
                  .data$opponent_def_rushing_plays_success_rate,
                  .data$opponent_def_passing_plays_ppa,
                  .data$opponent_def_passing_plays_success_rate,
                  .data$color,
                  .data$EPAP,
                  .data$SR,
                  input_metric) |>
    dplyr::distinct() |>
    dplyr::tibble()

  next_player_game <- input_next_game |>
    dplyr::arrange(.data$week) |>
    dplyr::filter(.data$season %in% input_season,
                  .data$team %in% player_plot_data$team[1]) |>
    dplyr::select(.data$week,
                  .data$team,
                  .data$opponent,
                  .data$opponent_def_ppa,
                  .data$opponent_def_success_rate,
                  .data$opponent_def_rushing_plays_ppa,
                  .data$opponent_def_rushing_plays_success_rate,
                  .data$opponent_def_passing_plays_ppa,
                  .data$opponent_def_passing_plays_success_rate) |>
    dplyr::mutate(color = "grey",
                  EPAP = dplyr::case_when(
                    input_metric %in% c("Pass Completions",
                                        "Pass Attempts",
                                        "Pass Yards",
                                        "Pass TDs",
                                        "Pass Int",
                                        "Receptions",
                                        "Rec Yards",
                                        "Rec TDs") ~ .data$opponent_def_passing_plays_ppa,
                    input_metric %in% c("Rush Carries",
                                        "Rush Yards",
                                        "Rush TDs") ~ .data$opponent_def_rushing_plays_ppa,
                    input_metric %in% c("Fantasy Score",
                                        "Pass & Rush Yards",
                                        "Pass & Rush & Rec TDs",
                                        "Rush & Rec Yards",
                                        "Fumbles Lost",
                                        "Punt Ret TDs",
                                        "Kick Ret TDs") ~ .data$opponent_def_ppa
                  ),
                  SR = dplyr::case_when(
                    input_metric %in% c("Pass Completions",
                                        "Pass Attempts",
                                        "Pass Yards",
                                        "Pass TDs",
                                        "Pass Int",
                                        "Receptions",
                                        "Rec Yards",
                                        "Rec TDs") ~ .data$opponent_def_passing_plays_success_rate,
                    input_metric %in% c("Rush Carries",
                                        "Rush Yards",
                                        "Rush TDs") ~ .data$opponent_def_rushing_plays_success_rate,
                    input_metric %in% c("Fantasy Score",
                                        "Pass & Rush Yards",
                                        "Pass & Rush & Rec TDs",
                                        "Rush & Rec Yards",
                                        "Fumbles Lost",
                                        "Punt Ret TDs",
                                        "Kick Ret TDs") ~ .data$opponent_def_success_rate))

  player_plot_data <- player_plot_data |>
    dplyr::bind_rows(next_player_game[1,]) |>
    dplyr::arrange(dplyr::desc(.data$week)) |>
    dplyr::filter(!is.na(.data$week),
                  !is.na(.data$team))

  player_plot_data$athlete_id[1] <- dplyr::last(player_plot_data$athlete_id)
  player_plot_data$athlete_name[1] <- dplyr::last(player_plot_data$athlete_name)
  player_plot_data$athlete_name[1] <- dplyr::last(player_plot_data$athlete_name)
  player_plot_data[length(colnames(player_plot_data))][1,] <- 0

  max_y <- max(player_plot_data |> dplyr::select(input_metric), input_threshold, na.rm = TRUE)
  count_x <- length(player_plot_data$week)

  type_epap_sr <- dplyr::case_when(
    input_metric %in% c("Pass Completions",
                        "Pass Attempts",
                        "Pass Yards",
                        "Pass TDs",
                        "Pass Int",
                        "Receptions",
                        "Rec Yards",
                        "Rec TDs") ~ "passing",
    input_metric %in% c("Rush Carries",
                        "Rush Yards",
                        "Rush TDs") ~ "rushing",
    input_metric %in% c("Fantasy Score",
                        "Pass & Rush Yards",
                        "Pass & Rush & Rec TDs",
                        "Rush & Rec Yards",
                        "Fumbles Lost",
                        "Punt Ret TDs",
                        "Kick Ret TDs") ~ "team")

  p1 <- ggplot2::ggplot(player_plot_data,
                        ggplot2::aes(x = factor(get("week")),
                                     y = get(input_metric))) +
    ggplot2::geom_col(
      ggplot2::aes(group = .data$color,
                   fill = get(input_metric) >= input_threshold),
      show.legend = FALSE,
      na.rm = TRUE
    ) +
    ggplot2::geom_hline(yintercept = input_threshold,
                        color = "red",
                        linetype = "dashed") +
    ggplot2::geom_text(ggplot2::aes(label = paste0("EPA/P: ", round(.data$EPAP,2))), fontface = "bold",size = ifelse(count_x >= 6, 30/count_x, 5),  nudge_y = ifelse(count_x >= 8, -max_y/8, -max_y/7), color = "#6c0000", na.rm = TRUE) +
    ggplot2::geom_text(ggplot2::aes(label = paste0("SR: ", round(.data$SR,2))), fontface = "bold",size = ifelse(count_x >= 6, 30/count_x, 5),  nudge_y = ifelse(count_x >= 8, -max_y/12, -max_y/11), color = "#6c0000", na.rm = TRUE) +

    cfbplotR::geom_cfb_logos(ggplot2::aes(team = .data$opponent),
                             width = .05,
                             alpha = .7,
                             na.rm = TRUE) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      x = "Week",
      y = input_metric,
      subtitle = glue::glue("Prop Line: {input_threshold} {input_metric}  |  Displayed EPA/Play and Success Rate metrics are {type_epap_sr} only"),
      caption = paste0("Figure: @JerrickBackous | @campus2canton\n Data: @CFB_Data with @cfbfastR")
    ) +
    ggplot2::theme(
      text = ggplot2::element_text(color = "#6c0000"),
      plot.title = ggplot2::element_text(size = 18, face = "bold"),
      plot.subtitle = ggplot2::element_text(size = 12),
      axis.text = ggplot2::element_text(size = 14, color = "#6c0000"),
      axis.title = ggplot2::element_text(size = 14),
      plot.caption = ggplot2::element_text(size = 12, hjust = 1)
    ) +
    ggplot2::scale_y_continuous() +
    ggplot2::scale_x_discrete() +
    ggplot2::guides(color = "none") +
    ggplot2::ggtitle(glue::glue("{input_player} {input_metric} by Week, {input_season}"))

  c2c <- magick::image_read(system.file("app/www/c2c_logo.png", package = "college.betting.analyzer"))

  p2 <- cowplot::ggdraw() +
    cowplot::draw_plot(p1) +
    cowplot::draw_image(
      c2c,
      x = 1.01,
      y = .99,
      hjust = 1,
      vjust = 1,
      width = 0.10,
      height = 0.10)

  return(p2)
}
#
# download_player_metric_plot <- function(input_df, input_player, input_position, input_metric, input_min_weeks) {
#   top_24_plot <- input_df |>
#     filter(Position == input_position &
#       case_when(
#         input_min_weeks == "Full Season" ~ TRUE,
#         input_min_weeks == 12L ~ `Games Played` >= 12L,
#         input_min_weeks == 10L ~ `Games Played` >= 10L,
#         input_min_weeks == 8L ~ `Games Played` >= 8L,
#         input_min_weeks == 6L ~ `Games Played` >= 6L,
#         input_min_weeks == 4L ~ `Games Played` >= 4L
#       ))
#
#   max_year_played <- input_df |>
#     filter(player_select %in% input_player)
#
#   max_year_played <- max(max_year_played$`Year out of HS`, na.rm = TRUE)
#
#   selected_player_plot <- input_df |>
#     filter(player_select %in% input_player &
#       case_when(
#         input_min_weeks == "Full Season" ~ TRUE,
#         input_min_weeks == 12L ~ `Games Played` >= 12L,
#         input_min_weeks == 10L ~ `Games Played` >= 10L,
#         input_min_weeks == 8L ~ `Games Played` >= 8L,
#         input_min_weeks == 6L ~ `Games Played` >= 6L,
#         input_min_weeks == 4L ~ `Games Played` >= 4L
#       ))
#
#   games_played_min_wording <- case_when(
#     input_min_weeks == "Full Season" ~ "Full Season",
#     input_min_weeks != "Full Season" ~ paste0(glue::glue("Min. {input_min_weeks} Games Played"))
#   )
#
#   wording_top_x <- case_when(
#     input_position == "WR" ~ "24",
#     TRUE ~ "12"
#   )
#
#   cbPalette <- c("#0072B2", "#D55E00", "#009E73", "#000000", "#56B4E9", "#E69F00", "#CC79A7", "#D55E00", "#0072B2", "#009E73", "#000000", "#56B4E9", "#E69F00", "#CC79A7", "#0072B2", "#009E73", "#000000", "#56B4E9", "#E69F00", "#CC79A7")
#
#   p1 <- ggplot(top_24_plot, aes(x = get("Year out of HS"), y = get(input_metric))) +
#     geom_point(
#       data = subset(top_24_plot, Top.X.Finishes.Y.N >= 1 & Class >= 2010 & get("Year out of HS") <= ifelse(max_year_played <= 3, 4, (max_year_played + 1)) & Player != "Demaryius Thomas"),
#       size = 3,
#       color = "light grey",
#       na.rm = TRUE
#     ) +
#     geom_textsmooth(
#       data = subset(top_24_plot, Top.X.Finishes.Y.N >= 1 & Class >= 2010 & get("Year out of HS") <= 3),
#       method = lm,
#       formula = y ~ x,
#       fullrange = TRUE,
#       se = FALSE,
#       linetype = "dashed",
#       color = "#6c0000",
#       label = glue::glue("Avg of {input_position}s with a Top {wording_top_x} NFL Season"),
#       hjust = 1,
#       vjust = 1.2,
#       fontface = "bold",
#       size = 5,
#       alpha = .5,
#       na.rm = TRUE
#     ) +
#     geom_point(
#       data = selected_player_plot,
#       aes(color = Player),
#       size = 4,
#       alpha = .5,
#       na.rm = TRUE
#     ) +
#     geom_label_repel(
#       data = selected_player_plot,
#       aes(label = Player, color = Player),
#       size = ifelse(max_year_played <= 3, 15, 12),
#       fontface = "bold",
#       alpha = .75,
#       nudge_x = ifelse(max_year_played <= 3, .035, .05),
#       hjust = 0,
#       label.padding = 0.01,
#       min.segment.length = .8,
#       direction = "y",
#       label.size = NA,
#       fill = alpha(c("white"), 0.5),
#       na.rm = TRUE
#     ) +
#     scale_colour_manual(values = cbPalette) +
#     stat_summary(
#       data = subset(top_24_plot, Top.X.Finishes.Y.N >= 1 & Class >= 2010 & get("Year out of HS") <= 3),
#       fun = "mean",
#       geom = "point",
#       col = "#6c0000",
#       size = 2.5,
#       alpha = .5,
#       na.rm = TRUE
#     ) +
#     theme_minimal() +
#     theme(
#       text = element_text(color = "#6c0000"),
#       plot.title = element_text(size = 24, face = "bold"),
#       plot.subtitle = element_text(size = 14),
#       axis.text = element_text(size = 16, color = "#6c0000"),
#       axis.title = element_text(size = 18),
#       plot.caption = element_text(size = 12, hjust = 1),
#       plot.background = element_rect(fill = "white"),
#     ) +
#     labs(
#       x = "Year out of High School",
#       y = input_metric,
#       subtitle = glue::glue("campus2canton.com | {games_played_min_wording}"),
#       caption = paste0("Figure: @JerrickBackous \n Data: @_TanHo | @CFB_Data with @cfbfastR")
#     ) +
#     scale_y_continuous() +
#     scale_x_continuous() +
#     guides(color = "none") +
#     ggtitle(glue::glue("Experience Adjusted {input_metric}"))
#
#   c2c <- image_read("c2c_logo.png")
#
#   p2 <- ggdraw() +
#     draw_plot(p1) +
#     draw_image(c2c, x = .085, y = .11, hjust = 1, vjust = 1, width = 0.10, height = 0.10)
# }
