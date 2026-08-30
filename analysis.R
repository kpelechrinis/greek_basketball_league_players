library(tidyverse)
library(scales)


raw_data_path <- "greek_league_player_minutes.csv"

raw <- read_csv(raw_data_path, show_col_types = FALSE) |>
  mutate(
    is_greek = str_to_lower(as.character(is_greek)) == "true",
    exclude_data_contamination =
      str_to_lower(as.character(exclude_data_contamination)) == "true",
    season_end = 2000L + as.integer(str_sub(season, -2))
  )

# Definitions:
#   greek_share = Greek player-minutes / all player-minutes.
#   rotation player = at least 5 appearances and at least 10 MPG.
#   p_i = player i's share of his team's season minutes.
#   Shannon entropy H = -sum(p_i * log(p_i)).
#   Effective rotation = exp(H).
# Team-level rotation metrics are averaged equally across teams within a season.

# Exclude the archived modern rows that leak into the 2009-10 ESAKE query.
# Zero-minute players remain in coverage counts, but cannot contribute to entropy.
clean <- raw |>
  filter(!exclude_data_contamination)

positive_minutes <- clean |>
  filter(
    !is.na(team),
    team != "",
    estimated_total_minutes > 0
  )

team_metrics <- positive_minutes |>
  group_by(season, season_end, team) |>
  mutate(
    team_total_minutes = sum(estimated_total_minutes),
    minute_share = estimated_total_minutes / team_total_minutes
  ) |>
  summarise(
    shannon_entropy = -sum(minute_share * log(minute_share)),
    effective_rotation = exp(shannon_entropy),
    top5_minutes_share =
      sum(head(sort(estimated_total_minutes, decreasing = TRUE), 5)) /
      sum(estimated_total_minutes),
    players_used = n(),
    rotation_players = sum(games >= 5 & minutes_per_game >= 10),
    greek_players_used = sum(is_greek),
    foreign_players_used = sum(!is_greek),
    greek_rotation_players =
      sum(is_greek & games >= 5 & minutes_per_game >= 10),
    foreign_rotation_players =
      sum(!is_greek & games >= 5 & minutes_per_game >= 10),
    greek_20plus_mpg_players =
      sum(is_greek & games >= 5 & minutes_per_game >= 20),
    foreign_20plus_mpg_players =
      sum(!is_greek & games >= 5 & minutes_per_game >= 20),
    .groups = "drop"
  )

team_season_summary <- team_metrics |>
  group_by(season, season_end) |>
  summarise(
    teams = n(),
    avg_shannon_entropy = mean(shannon_entropy),
    avg_effective_rotation = mean(effective_rotation),
    avg_top5_minutes_share = mean(top5_minutes_share),
    avg_players_used = mean(players_used),
    avg_rotation_players = mean(rotation_players),
    avg_greek_players_used = mean(greek_players_used),
    avg_foreign_players_used = mean(foreign_players_used),
    avg_greek_rotation_players = mean(greek_rotation_players),
    avg_foreign_rotation_players = mean(foreign_rotation_players),
    avg_greek_20plus_mpg_players = mean(greek_20plus_mpg_players),
    avg_foreign_20plus_mpg_players = mean(foreign_20plus_mpg_players),
    .groups = "drop"
  )

minute_summary <- clean |>
  group_by(season, season_end) |>
  summarise(
    total_minutes = sum(estimated_total_minutes),
    greek_minutes = sum(estimated_total_minutes[is_greek]),
    greek_share = greek_minutes / total_minutes,
    blank_country_players = sum(is.na(country_esake) | str_trim(country_esake) == ""),
    blank_country_minutes_share =
      sum(estimated_total_minutes[is.na(country_esake) |
                                  str_trim(country_esake) == ""]) /
      total_minutes,
    .groups = "drop"
  )

df <- minute_summary |>
  left_join(team_season_summary, by = c("season", "season_end")) |>
  arrange(season_end) |>
  select(
    season,
    season_end,
    teams,
    total_minutes,
    greek_minutes,
    greek_share,
    blank_country_players,
    blank_country_minutes_share,
    avg_shannon_entropy,
    avg_effective_rotation,
    avg_top5_minutes_share,
    avg_players_used,
    avg_rotation_players,
    avg_greek_players_used,
    avg_foreign_players_used,
    avg_greek_rotation_players,
    avg_foreign_rotation_players,
    avg_greek_20plus_mpg_players,
    avg_foreign_20plus_mpg_players
  )





p_share <- ggplot(df, aes(x = season_end, y = greek_share)) +
  annotate(
    "rect",
    xmin = 2009.5,
    xmax = 2013.5,
    ymin = -Inf,
    ymax = Inf,
    fill = "#D9D9D9",
    alpha = 0.35
  ) +
  geom_line(linewidth = 0.7, color = "#777777", alpha = 0.75) +
  geom_point(size = 3, color = "#17365D") +
  geom_smooth(
    aes(weight = total_minutes),
    method = "loess",
    formula = y ~ x,
    span = 0.65,
    se = TRUE,
    level = 0.95,
    linewidth = 1.7,
    color = "#C62828",
    fill = "#C62828",
    alpha = 0.18
  ) +
  scale_x_continuous(
    breaks = df$season_end,
    labels = df$season,
    expand = expansion(mult = c(0.02, 0.03))
  ) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    breaks = seq(0.25, 0.65, by = 0.05)
  ) +
  coord_cartesian(ylim = c(0.25, 0.65)) +
  labs(
    title = "Η συμμετοχή των Ελλήνων παικτών στην Α1",
    subtitle = paste0(
      "Ποσοστό των συνολικών λεπτών συμμετοχής ανά σεζόν\n",
      "και εξομάλυνση LOESS σταθμισμένη με τα συνολικά λεπτά"
    ),
    x = NULL,
    y = "Ποσοστό λεπτών Ελλήνων παικτών",
    caption = paste0(
      "Η κόκκινη σκιασμένη περιοχή αποτυπώνει την εκτιμώμενη ",
      "αβεβαιότητα γύρω από την εξομαλυμένη τάση.\n",
      "Στη γκρίζα περίοδο η καταγραφή της εθνικότητας των παικτών\n",
      "στα αρχεία του ΕΣΑΚΕ είναι λιγότερο πλήρης."
    )
  ) +
  theme_minimal(base_size = 15, base_family = "Arial") +
  theme(
    plot.title = element_text(face = "bold", size = 20, color = "#17365D"),
    plot.subtitle = element_text(size = 13, lineheight = 1.15, margin = margin(b = 14)),
    axis.title.y = element_text(face = "bold", margin = margin(r = 10)),
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold", size = 10),
    axis.text.y = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    plot.caption = element_text(hjust = 0, size = 10, color = "#555555", lineheight = 1.15, margin = margin(t = 12)),
    plot.margin = margin(t = 15, r = 20, b = 15, l = 15)
  )

print(p_share)

rotation_long <- df |>
  select(
    season,
    season_end,
    avg_greek_rotation_players,
    avg_foreign_rotation_players
  ) |>
  pivot_longer(
    cols = c(avg_greek_rotation_players, avg_foreign_rotation_players),
    names_to = "group",
    values_to = "players"
  ) |>
  mutate(
    group = recode(
      group,
      avg_greek_rotation_players = "Έλληνες",
      avg_foreign_rotation_players = "Ξένοι"
    )
  )

p_rotation <- ggplot(
  rotation_long,
  aes(x = season_end, y = players, color = group)
) +
  geom_line(linewidth = 1.3) +
  geom_point(size = 3) +
  scale_color_manual(values = c("Έλληνες" = "#17365D", "Ξένοι" = "#C62828")) +
  scale_x_continuous(breaks = df$season_end, labels = df$season) +
  labs(
    title = "Έλληνες και ξένοι παίκτες στο rotation των ομάδων",
    subtitle = "Μέσος αριθμός ανά ομάδα και σεζόν",
    x = NULL,
    y = "Παίκτες ανά ομάδα",
    color = NULL,
    caption = "Παίκτης rotation: τουλάχιστον 5 συμμετοχές και τουλάχιστον 10 λεπτά ανά αγώνα."
  ) +
  theme_minimal(base_size = 15, base_family = "Arial") +
  theme(
    plot.title = element_text(face = "bold", size = 20, color = "#17365D"),
    axis.title.y = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold", size = 10),
    axis.text.y = element_text(face = "bold"),
    legend.position = "top",
    legend.text = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    plot.caption = element_text(hjust = 0, size = 10, color = "#555555")
  )

print(p_rotation)


