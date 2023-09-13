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
    dplyr::mutate(spread = as.numeric(.data$spread),
                  over_under = as.numeric(.data$over_under),
                  color = dplyr::case_when(input_metric >= input_threshold ~ "green",
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
                  .data$spread,
                  .data$over_under,
                  input_metric) |>
    dplyr::distinct() |>
    dplyr::tibble()

  next_player_game <- input_next_game |>
    dplyr::arrange(.data$week) |>
    dplyr::filter(.data$season %in% input_season &
                  .data$team %in% player_plot_data$team[1]) |>
    dplyr::select(.data$week,
                  .data$team,
                  .data$opponent,
                  .data$opponent_def_ppa,
                  .data$opponent_def_success_rate,
                  .data$opponent_def_rushing_plays_ppa,
                  .data$opponent_def_rushing_plays_success_rate,
                  .data$opponent_def_passing_plays_ppa,
                  .data$opponent_def_passing_plays_success_rate,
                  .data$spread,
                  .data$over_under) |>
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
  if (is.na(player_plot_data[length(colnames(player_plot_data))][1,])) {
    player_plot_data[length(colnames(player_plot_data))][1,] <- 0
  }

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
    ggplot2::geom_text(ggplot2::aes(label = paste0("Spread: ", round(.data$spread,1))), fontface = "bold",size = ifelse(count_x >= 8, 30/count_x, 4),  nudge_y = ifelse(count_x >= 8, -max_y/12, -max_y/11), color = "#6c0000", na.rm = TRUE) +
    ggplot2::geom_text(ggplot2::aes(label = paste0("O/U: ", round(.data$over_under,2))), fontface = "bold",size = ifelse(count_x >= 8, 30/count_x, 4),  nudge_y = ifelse(count_x >= 8, -max_y/8, -max_y/6.6), color = "#6c0000", na.rm = TRUE) +
    ggplot2::geom_text(ggplot2::aes(label = paste0("SR: ", paste0(round(.data$SR*100,0),"%"))), fontface = "bold",size = ifelse(count_x >= 8, 30/count_x, 4),  nudge_y = ifelse(count_x >= 8, -max_y/6.15, -max_y/4.8), color = "#6c0000", na.rm = TRUE) +
    ggplot2::geom_text(ggplot2::aes(label = paste0("EPA/P: ", round(.data$EPAP,2))), fontface = "bold",size = ifelse(count_x >= 8, 30/count_x, 4),  nudge_y = ifelse(count_x >= 8, -max_y/5, -max_y/3.75), color = "#6c0000", na.rm = TRUE) +

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


#' Download player metric plot
#'
#' @param input_df data frame
#' @param input_next_game data frame with future games
#' @param input_season players to filter
#' @param input_weeks positions to filter
#' @param input_player metrics to filter
#' @param input_metric minimum weeks played
#' @param input_threshold minimum weeks played
#'
#' @return Download player metric plot
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
download_player_game_plot <- function(input_df, input_next_game, input_season, input_weeks, input_player, input_metric, input_threshold) {

  player_plot_data <- input_df |>
    dplyr::filter(.data$season %in% input_season,
                  .data$week %in% c(input_weeks[1]:input_weeks[2]),
                  .data$athlete_name %in% input_player
    ) |>
    dplyr::mutate(spread = as.numeric(.data$spread),
                  over_under = as.numeric(.data$over_under),
                  color = dplyr::case_when(input_metric >= input_threshold ~ "green",
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
                  .data$spread,
                  .data$over_under,
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
                  .data$opponent_def_passing_plays_success_rate,
                  .data$spread,
                  .data$over_under) |>
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
    ggplot2::geom_text(ggplot2::aes(label = paste0("Spread: ", round(.data$spread,2))), fontface = "bold",size = ifelse(count_x >= 8, 50/count_x, 6),  nudge_y = ifelse(count_x >= 8, -max_y/13, -max_y/13), color = "#6c0000", na.rm = TRUE) +
    ggplot2::geom_text(ggplot2::aes(label = paste0("O/U: ", round(.data$over_under,2))), fontface = "bold",size = ifelse(count_x >= 8, 50/count_x, 6),  nudge_y = ifelse(count_x >= 8, -max_y/9.5, -max_y/8.5), color = "#6c0000", na.rm = TRUE) +
    ggplot2::geom_text(ggplot2::aes(label = paste0("SR: ", paste0(round(.data$SR*100,0),"%"))), fontface = "bold",size = ifelse(count_x >= 8, 50/count_x, 6),  nudge_y = ifelse(count_x >= 8, -max_y/7.5, -max_y/6.45), color = "#6c0000", na.rm = TRUE) +
    ggplot2::geom_text(ggplot2::aes(label = paste0("EPA/P: ", round(.data$EPAP,2))), fontface = "bold",size = ifelse(count_x >= 8, 50/count_x, 6),  nudge_y = ifelse(count_x >= 8, -max_y/6.2, -max_y/5.2), color = "#6c0000", na.rm = TRUE) +

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
      plot.title = ggplot2::element_text(size = 24, face = "bold"),
      plot.subtitle = ggplot2::element_text(size = 16),
      axis.text = ggplot2::element_text(size = 14, color = "#6c0000"),
      axis.title = ggplot2::element_text(size = 20),
      plot.caption = ggplot2::element_text(size = 14, hjust = 1),
      legend.position = "none",
      plot.background = ggplot2::element_rect(fill = "white")
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
      y = .995,
      hjust = 1,
      vjust = 1,
      width = 0.08,
      height = 0.08)

  return(p2)
}
