# Making an NWHL plot using team logos and colors

library(tidyverse)

# need these for the final plot
# install.packages("ggimage")
# install.packages("scales")

# get logo data
team_logos <- read_csv("https://raw.githubusercontent.com/danmorse314/nwhl/main/nwhl%20logos.csv",
                       col_types = cols()) # this line just removes the output in the console

# get bdc data with xG
bdc_data <- read_csv("https://raw.githubusercontent.com/danmorse314/bdc21/main/nwhl_bdc_data.csv",
                     col_types = cols()) # this line just removes the output in the console

# games played per team
games <- bdc_data %>%
  filter(league == "NWHL") %>%
  select(team, game_id) %>%
  distinct() %>%
  group_by(team) %>%
  summarize(games = n(), .groups = "drop") %>%
  ungroup()

# xg allowed
chart_d <- bdc_data %>%
  filter(league == "NWHL") %>%
  mutate(opponent = ifelse(team == home_team, away_team, home_team)) %>%
  group_by(opponent) %>%
  summarize(
    xga = sum(xg, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  ungroup()

# xg for
chart <- bdc_data %>%
  filter(league == "NWHL") %>%
  mutate(opponent = ifelse(team == home_team, away_team, home_team)) %>%
  group_by(team) %>%
  summarize(
    xgf = sum(xg, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  ungroup() %>%
  # combine with xg against
  left_join(chart_d, by = c("team" = "opponent")) %>%
  # combine with lgames played
  left_join(games, by = "team") %>%
  mutate(
    xgf_pg = xgf / games,
    xga_pg = xga / games,
    xg_diff_pg = round(xgf_pg - xga_pg, 2)
  ) %>%
  # combine with logo and color info
  left_join(team_logos, by = c("team" = "team_name")) %>%
  arrange(-xg_diff_pg)

# make a plot!
chart %>%
  ggplot(aes(reorder(team, xg_diff_pg), xg_diff_pg)) +
  # xg bars
  geom_col(fill = chart$team_color, color = chart$team_color2) +
  # add team logos, offset from actual xg numbers
  ggimage::geom_image(
    aes(y = ifelse(xg_diff_pg < 0, xg_diff_pg - .3, xg_diff_pg + .3), image = team_logo),
    size = .12
    ) +
  annotate("text", x = .5, y = 2, hjust = 1, vjust = 0, size = 3,
           label = "chart: @danmorse_\ndata: Stathletes") +
  # axes limits and breaks
  scale_y_continuous(breaks = scales::pretty_breaks(), limits = c(-2.2,2)) +
  coord_flip() +
  theme_bw() +
  theme(
    # remove team name labels because the logos are enough
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  labs(x = NULL, y = "xG +/- per game",
       title = "NWHL 2021 stats")
