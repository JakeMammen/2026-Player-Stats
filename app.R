  # =============================================================================
  # 2026 Position Stats — WR / TE / RB / QB
  # One Shiny app, four reactive tables
  #
  # Data: nflreadr (PBP built by nflfastR + official player stats)
  # RZ = inside the 25    EZ = inside the 5
  #
  # Install if needed:
  # install.packages(c("nflreadr", "dplyr", "tidyr", "shiny", "bslib",
  #                    "reactable", "htmltools", "scales", "ggplot2", "rsconnect"))
  # =============================================================================

library(nflreadr)
library(dplyr)
library(tidyr)
library(shiny)
library(bslib)
library(reactable)
library(htmltools)
library(scales)
library(ggplot2)
library(rsconnect)
library(cowplot)
library(magick)
library(png)

SEASON <- 2026
RZ_LINE <- 25
EZ_LINE <- 5

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

safe_div <- function(num, den) {
  ifelse(!is.na(den) & den > 0, num / den, 0)
}

color_scale <- function(x, palette = "good", dark = FALSE) {
  na_color <- if (dark) "#1e293b" else "#f4f4f4"
  mid_color <- if (dark) "#334155" else "#e8eef5"
  if (!is.numeric(x)) return(rep(na_color, length(x)))
  
  rng <- range(x, na.rm = TRUE)
  if (!is.finite(rng[1]) || rng[1] == rng[2]) {
    return(ifelse(is.na(x), na_color, mid_color))
  }
  
  pal <- if (dark) {
    switch(
      palette,
      good  = colour_ramp(c("#1e3a5f", "#2563eb", "#93c5fd")),
      value = colour_ramp(c("#7f1d1d", "#a16207", "#166534")),
      bad   = colour_ramp(c("#1e293b", "#7f1d1d", "#ef4444")),
      colour_ramp(c("#1e293b", "#475569"))
    )
  } else {
    switch(
      palette,
      good  = colour_ramp(c("#f1f5f9", "#93c5fd", "#1d4ed8")),
      value = colour_ramp(c("#ef4444", "#fef3c7", "#16a34a")),
      bad   = colour_ramp(c("#f8fafc", "#fecaca", "#b91c1c")),
      colour_ramp(c("#f8fafc", "#cbd5e1"))
    )
  }
  
  out <- pal(rescale(x, to = c(0, 1), from = rng))
  out[is.na(x)] <- na_color
  out
}

style_numeric <- function(vec, palette = "good", dark = FALSE) {
  colors <- color_scale(vec, palette, dark = dark)
  text_col <- if (dark) "#f8fafc" else "#0f172a"
  function(value, index) {
    list(background = colors[[index]], color = text_col, fontWeight = 600)
  }
}

player_cell <- function(data) {
  function(value, index) {
    row <- data[index, ]
    htmltools::tagList(
      tags$img(
        src = row$headshot_url,
        alt = value,
        style = "height:28px;width:28px;border-radius:50%;vertical-align:middle;margin-right:8px;object-fit:cover;"
      ),
      tags$span(style = "font-weight:600;", value)
    )
  }
}

team_cell <- function(data) {
  function(value, index) {
    src <- data$team_logo_espn[index]
    htmltools::tagList(
      tags$img(src = src, alt = value, style = "height:22px;vertical-align:middle;margin-right:6px;"),
      tags$span(value)
    )
  }
}

# Reactable's default td background is white and beats theme$backgroundColor.
# Paint every cell so uncolored columns stay readable in dark mode.
zebra_style <- function(dark = FALSE) {
  function(value, index) {
    odd <- as.integer(index) %% 2L == 1L
    if (isTRUE(dark)) {
      list(
        background = if (odd) "#0f172a" else "#162033",
        color = "#f8fafc"
      )
    } else {
      list(
        background = if (odd) "#ffffff" else "#f8fafc",
        color = "#0f172a"
      )
    }
  }
}

base_reactable <- function(data, columns, column_groups = NULL, default_sorted = "total_fp", dark = FALSE) {
  if (nrow(data) == 0) {
    empty <- data.frame(Note = "No players match the current filters.")
    return(reactable(
      empty,
      theme = if (dark) {
        reactableTheme(color = "#f8fafc", backgroundColor = "#0f172a", borderColor = "#334155")
      } else {
        reactableTheme()
      }
    ))
  }
  
  sort_arg <- list()
  sort_arg[[default_sorted]] <- "desc"
  
  tbl_theme <- if (dark) {
    reactableTheme(
      color = "#f8fafc",
      backgroundColor = "#0f172a",
      borderColor = "#334155",
      stripedColor = "#1e293b",
      highlightColor = "#1e3a5f",
      cellPadding = "7px 6px",
      searchInputStyle = list(
        width = "280px",
        backgroundColor = "#1e293b",
        color = "#f8fafc",
        border = "1px solid #475569"
      ),
      inputStyle = list(
        backgroundColor = "#1e293b",
        color = "#f8fafc",
        border = "1px solid #475569"
      ),
      selectStyle = list(
        backgroundColor = "#1e293b",
        color = "#f8fafc"
      ),
      paginationStyle = list(color = "#cbd5e1", backgroundColor = "#0f172a"),
      pageButtonHoverStyle = list(backgroundColor = "#334155", color = "#f8fafc"),
      pageButtonActiveStyle = list(backgroundColor = "#1e3a5f", color = "#f8fafc"),
      headerStyle = list(
        background = "#020617",
        color = "#e2e8f0"
      ),
      style = list(fontFamily = "Oswald, Helvetica, Arial, sans-serif", fontSize = "13px", backgroundColor = "#0f172a")
    )
  } else {
    reactableTheme(
      color = "#0f172a",
      backgroundColor = "#ffffff",
      borderColor = "#e2e8f0",
      stripedColor = "#f8fafc",
      highlightColor = "#e0f2fe",
      searchInputStyle = list(width = "280px"),
      style = list(fontFamily = "Oswald, Helvetica, Arial, sans-serif", fontSize = "13px")
    )
  }
  
  reactable(
    data,
    highlight = TRUE,
    striped = TRUE,
    compact = TRUE,
    searchable = TRUE,
    filterable = TRUE,
    defaultSorted = sort_arg,
    defaultPageSize = 25,
    pageSizeOptions = c(10, 25, 50, 100),
    showPageSizeOptions = TRUE,
    onClick = "select",
    language = reactableLang(searchPlaceholder = "Search a player..."),
    columnGroups = column_groups,
    defaultColDef = colDef(
      align = "center",
      minWidth = 68,
      style = zebra_style(dark),
      headerStyle = list(
        background = if (dark) "#020617" else "#0f172a",
        color = "#e2e8f0",
        fontWeight = 700,
        borderBottom = if (dark) "2px solid #31a354" else "2px solid #1e293b"
      )
    ),
    theme = tbl_theme,
    columns = columns
  )
}

hidden_meta_cols <- function() {
  list(
    player_id = colDef(show = FALSE),
    headshot_url = colDef(show = FALSE),
    team_wordmark = colDef(show = FALSE),
    team_logo_espn = colDef(show = FALSE)
  )
}

# ---------------------------------------------------------------------------
# Load data once
# ---------------------------------------------------------------------------

season <- if (length(SEASON) == 1 && SEASON >= 1999) SEASON else most_recent_season()

player_week <- load_player_stats(seasons = season, summary_level = "week") %>%
  filter(season_type == "REG")

rosters_week <- load_rosters_weekly(seasons = season) %>%
  filter(!is.na(gsis_id), position %in% c("WR", "TE", "RB", "QB", "FB", "HB"))

latest_roster <- rosters_week %>%
  group_by(gsis_id) %>%
  slice_max(week, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(player_id = gsis_id, player_name = full_name, team, position, headshot_url, pfr_id)

full_pbp <- load_pbp(seasons = season) %>%
  filter(season_type == "REG")

max_week <- suppressWarnings(max(c(full_pbp$week, player_week$week), na.rm = TRUE))
if (!is.finite(max_week)) max_week <- NA_integer_
week_label <- if (is.finite(max_week)) paste("Through Week", max_week) else paste(season, "season")

team_pass <- full_pbp %>%
  filter(pass_attempt == 1, !is.na(receiver_id), down %in% 1:4) %>%
  group_by(posteam) %>%
  summarise(
    team_targets = n(),
    team_rz_targets = sum(yardline_100 <= RZ_LINE, na.rm = TRUE),
    team_ez_targets = sum(yardline_100 <= EZ_LINE, na.rm = TRUE),
    .groups = "drop"
  )

# PFR snap counts are game-level and keyed by pfr_player_id, not GSIS.
snap_week <- tryCatch(
  load_snap_counts(seasons = season),
  error = function(e) {
    warning("load_snap_counts() failed: ", conditionMessage(e))
    tibble()
  }
)

if (nrow(snap_week) > 0) {
  snap_clean <- snap_week %>%
    filter(game_type == "REG" | is.na(game_type)) %>%
    mutate(off_pct = if_else(offense_pct > 1.5, offense_pct / 100, offense_pct))
  
  snap_season <- snap_clean %>%
    group_by(pfr_player_id) %>%
    summarise(
      offense_snaps = sum(offense_snaps, na.rm = TRUE),
      offense_pct = weighted.mean(off_pct, w = pmax(offense_snaps, 1e-6), na.rm = TRUE),
      .groups = "drop"
    )
  
  snap_team <- snap_clean %>%
    group_by(pfr_player_id, player, team, position) %>%
    summarise(
      offense_snaps = sum(offense_snaps, na.rm = TRUE),
      offense_pct = weighted.mean(off_pct, w = pmax(offense_snaps, 1e-6), na.rm = TRUE),
      .groups = "drop"
    )
} else {
  snap_season <- tibble(pfr_player_id = character(), offense_snaps = numeric(), offense_pct = numeric())
  snap_team <- tibble(
    pfr_player_id = character(), player = character(), team = character(),
    position = character(), offense_snaps = numeric(), offense_pct = numeric()
  )
}

team_rush <- full_pbp %>%
  filter(rush_attempt == 1, !is.na(rusher_id), down %in% 1:4) %>%
  group_by(posteam) %>%
  summarise(
    team_carries = n(),
    team_rz_carries = sum(yardline_100 <= RZ_LINE, na.rm = TRUE),
    team_ez_carries = sum(yardline_100 <= EZ_LINE, na.rm = TRUE),
    .groups = "drop"
  )

team_meta <- load_teams()
team_meta <- team_meta %>%
  select(any_of(c(
    "team_abbr", "team_wordmark", "team_logo_espn",
    "team_color", "team_color2", "team_color3", "team_color4"
  )))


# Resolve a theme-specific logo that works locally and on shinyapps.io.
# Place both files in ./logo (or ./www) next to the app script:
#   FSP_Logo_Dark.png  -> dark mode  (#0b1220)
#   FSP_Logo_White.png -> light mode (#ffffff)
resolve_logo_path <- function(dark = FALSE) {
  filename <- if (isTRUE(dark)) "FSP_Logo_Dark.png" else "FSP_Logo_White.png"
  dirs <- c(
    "logo",
    "www",
    file.path("www", "logo")
  )
  candidates <- file.path(dirs, filename)
  fallback <- file.path(dirs, c("FSP_logo.png", "FSP_Logo.png"))
  hit <- candidates[file.exists(candidates)]
  if (length(hit)) return(hit[[1]])
  hit_fb <- fallback[file.exists(fallback)]
  if (length(hit_fb)) return(hit_fb[[1]])
  candidates[[1]]
}

team_fill_colors <- function(team_abbr, n) {
  n <- max(1L, as.integer(n))
  row <- team_meta[team_meta$team_abbr == team_abbr, , drop = FALSE]
  cols <- character()
  for (nm in c("team_color", "team_color2", "team_color3", "team_color4")) {
    if (nm %in% names(row) && nrow(row) && !is.na(row[[nm]][1]) && nzchar(row[[nm]][1])) {
      cols <- c(cols, row[[nm]][1])
    }
  }
  cols <- unique(cols)
  if (length(cols) == 0) cols <- c("#1d4ed8", "#93c5fd")
  if (length(cols) == 1) cols <- c(cols, "#f8fafc")
  grDevices::colorRampPalette(cols)(n)
}

player_last <- function(x) {
  suffix_tokens <- c("JR", "SR", "II", "III", "IV", "V", "VI")
  particle_tokens <- c("ST", "SAINT")
  
  vapply(as.character(x), function(nm) {
    parts <- unlist(strsplit(trimws(nm), "\\s+"))
    parts <- parts[nzchar(parts)]
    if (!length(parts)) return(nm)
    
    norm <- toupper(gsub("[.,]", "", parts))
    n <- length(parts)
    has_suffix <- n >= 2 && norm[n] %in% suffix_tokens
    base_i <- if (has_suffix) n - 1L else n
    
    if (base_i >= 2 && norm[base_i - 1L] %in% particle_tokens) {
      paste(parts[(base_i - 1L):n], collapse = " ")
    } else if (has_suffix) {
      paste(parts[base_i:n], collapse = " ")
    } else {
      parts[n]
    }
  }, character(1), USE.NAMES = FALSE)
}

share_pie_plot <- function(data, team_abbr, positions, dark = FALSE,
                           title_metric = "share", subtitle = "", empty_noun = "data") {
  week_bit <- if (is.finite(max_week)) paste("Week", max_week) else paste(season)
  bg <- if (dark) "#0b1220" else "#ffffff"
  fg <- if (dark) "#f8fafc" else "#0f172a"
  
  empty_plot <- function(msg) {
    ggplot() +
      annotate("text", x = 0.5, y = 0.5, label = msg, color = fg, size = 5) +
      xlim(0, 1) + ylim(0, 1) +
      theme_void() +
      theme(plot.background = element_rect(fill = bg, color = NA))
  }
  
  if (is.null(team_abbr) || identical(team_abbr, "ALL")) {
    return(empty_plot("Select a team in the sidebar to draw the chart."))
  }
  if (length(positions) == 0) {
    return(empty_plot("Select at least one position."))
  }
  
  pos_want <- unique(unlist(list(
    QB = "QB",
    RB = c("RB", "FB", "HB"),
    WR = "WR",
    TE = "TE"
  )[positions], use.names = FALSE))
  
  pie_df <- data %>%
    filter(team == team_abbr, position %in% pos_want, value > 0) %>%
    arrange(desc(value))
  
  if (nrow(pie_df) == 0) {
    return(empty_plot(paste("No", empty_noun, "for", team_abbr)))
  }
  
  pie_df <- pie_df %>%
    mutate(
      player_lab = paste0(player, " (", position, ")"),
      slice_lab = paste0(player_last(player), "\n", round(pct * 100), "%"),
      player_lab = factor(player_lab, levels = unique(player_lab)),
      fraction = value / sum(value),
      ymax = cumsum(fraction),
      ymin = lag(ymax, default = 0),
      mid = (ymin + ymax) / 2,
      outside = fraction < 0.08
    )
  
  spread_mids <- function(mids, min_gap = 0.045) {
    if (length(mids) <= 1) return(mids)
    o <- order(mids)
    y <- mids[o]
    for (i in seq_along(y)[-1]) {
      if (y[i] - y[i - 1] < min_gap) y[i] <- y[i - 1] + min_gap
    }
    overflow <- y[length(y)] - 0.98
    if (overflow > 0) y <- y - overflow
    for (i in seq_along(y)[-1]) {
      if (y[i] - y[i - 1] < min_gap) y[i] <- y[i - 1] + min_gap
    }
    out <- mids
    out[o] <- pmin(pmax(y, 0.02), 0.98)
    out
  }
  
  pie_df$label_at <- pie_df$mid
  if (any(pie_df$outside)) {
    pie_df$label_at[pie_df$outside] <- spread_mids(pie_df$mid[pie_df$outside])
  }
  
  pie_df <- pie_df %>%
    mutate(
      # Push the text off the leader along the arc (same direction already used to unstack labels).
      text_at = if_else(
        outside,
        label_at + sign(label_at - mid + 1e-4) * 0.022,
        mid
      ),
      slice_lab = paste0(player_last(player), "\n", round(pct * 100), "%")
    )
  
  fills <- team_fill_colors(team_abbr, nrow(pie_df))
  names(fills) <- levels(pie_df$player_lab)
  
  outside_df <- pie_df[pie_df$outside, , drop = FALSE]
  inside_df  <- pie_df[!pie_df$outside, , drop = FALSE]
  
  ggplot(pie_df) +
    geom_rect(
      aes(xmin = 0.18, xmax = 1.05, ymin = ymin, ymax = ymax, fill = player_lab),
      color = bg,
      linewidth = 0.7
    ) +
    geom_segment(
      data = outside_df,
      aes(x = 1.05, xend = 1.20, y = mid, yend = label_at),
      color = fg,
      linewidth = 0.4
    ) +
    geom_segment(
      data = outside_df,
      aes(x = 1.20, xend = 1.32, y = label_at, yend = label_at),
      color = fg,
      linewidth = 0.4
    ) +
    geom_segment(
      data = outside_df,
      aes(x = 1.32, xend = 1.40, y = label_at, yend = text_at),
      color = fg,
      linewidth = 0.4
    ) +
    geom_text(
      data = inside_df,
      aes(x = 0.62, y = mid, label = slice_lab),
      color = "#ffffff",
      size = 5.1,
      fontface = "bold",
      lineheight = 0.95
    ) +
    geom_text(
      data = outside_df,
      aes(x = 1.48, y = text_at, label = slice_lab),
      color = fg,
      size = 4.4,
      fontface = "bold",
      lineheight = 0.92,
      hjust = 0,
      vjust = 0.5
    ) +
    scale_fill_manual(values = fills) +
    coord_polar(theta = "y") +
    xlim(0.05, 1.95) +
    labs(
      title = paste0(team_abbr, " ", title_metric, " — ", week_bit),
      subtitle = subtitle,
      fill = NULL
    ) +
    theme_void(base_family = "sans") +
    theme(
      plot.background = element_rect(fill = bg, color = NA),
      panel.background = element_rect(fill = bg, color = NA),
      plot.title = element_text(color = fg, face = "bold", size = 20, hjust = 0.5),
      plot.subtitle = element_text(color = fg, size = 12, hjust = 0.5, margin = margin(b = 8)),
      legend.position = "none",
      plot.margin = margin(8, 8, 8, 8)
    )
}

# Bake the logo into the ggplot so on-screen and downloaded PNGs both include it.
add_plot_logo <- function(p, dark = FALSE) {
  bg <- if (isTRUE(dark)) "#0b1220" else "#ffffff"
  logo_file <- resolve_logo_path(dark)
  
  if (is.null(logo_file) || !nzchar(logo_file) || !file.exists(logo_file)) {
    warning("Logo file not found at: ", logo_file)
    return(p)
  }
  
  if (requireNamespace("cowplot", quietly = TRUE) &&
      requireNamespace("magick", quietly = TRUE)) {
    return(
      cowplot::ggdraw(p) +
        theme(
          plot.background = element_rect(fill = bg, color = NA),
          panel.background = element_rect(fill = bg, color = NA)
        ) +
        cowplot::draw_image(
          logo_file,
          x = 0.985,
          y = 0.02,
          width = 0.12,
          height = 0.12,
          hjust = 1,
          vjust = 0,
          halign = 1,
          valign = 0
        )
    )
  }
  
  logo_img <- tryCatch(png::readPNG(logo_file), error = function(e) NULL)
  if (is.null(logo_img)) {
    warning("Could not read logo PNG: ", logo_file)
    return(p)
  }
  
  logo_grob <- grid::rasterGrob(logo_img, interpolate = TRUE)
  
  if (requireNamespace("cowplot", quietly = TRUE)) {
    return(
      cowplot::ggdraw(p) +
        theme(
          plot.background = element_rect(fill = bg, color = NA),
          panel.background = element_rect(fill = bg, color = NA)
        ) +
        cowplot::draw_grob(
          logo_grob,
          x = 0.985,
          y = 0.02,
          width = 0.12,
          height = 0.12,
          hjust = 1,
          vjust = 0
        )
    )
  }
  
  p
}

draw_share_pie <- function(p, dark = FALSE) {
  print(add_plot_logo(p, dark = dark))
}

snap_pie_plot <- function(team_abbr, positions, dark = FALSE) {
  share_pie_plot(
    snap_team %>% transmute(player, team, position, value = offense_snaps, pct = offense_pct),
    team_abbr, positions, dark,
    title_metric = "offensive snap share",
    subtitle = "Slice size = offensive snap share.",
    empty_noun = "offensive snap data"
  )
}

draw_snap_pie <- function(team_abbr, positions, dark = FALSE) {
  draw_share_pie(snap_pie_plot(team_abbr, positions, dark = dark), dark = dark)
}

# ---------------------------------------------------------------------------
# Official season rollups
# ---------------------------------------------------------------------------

official_all <- player_week %>%
  group_by(player_id) %>%
  summarise(
    player_name_stats = dplyr::last(na.omit(player_display_name)),
    team_stats = dplyr::last(na.omit(team)),
    position_stats = dplyr::last(na.omit(position)),
    headshot_stats = dplyr::last(na.omit(headshot_url)),
    games_played = n_distinct(week),
    targets = sum(targets, na.rm = TRUE),
    receptions = sum(receptions, na.rm = TRUE),
    receiving_yards = sum(receiving_yards, na.rm = TRUE),
    yards_after_catch = sum(receiving_yards_after_catch, na.rm = TRUE),
    air_yards = sum(receiving_air_yards, na.rm = TRUE),
    receiving_td = sum(receiving_tds, na.rm = TRUE),
    carries = sum(carries, na.rm = TRUE),
    rushing_yards = sum(rushing_yards, na.rm = TRUE),
    rushing_td = sum(rushing_tds, na.rm = TRUE),
    completions = sum(completions, na.rm = TRUE),
    attempts = sum(attempts, na.rm = TRUE),
    passing_yards = sum(passing_yards, na.rm = TRUE),
    passing_td = sum(passing_tds, na.rm = TRUE),
    interceptions = sum(passing_interceptions, na.rm = TRUE),
    passing_air_yards = sum(passing_air_yards, na.rm = TRUE),
    sacks = sum(sacks_suffered, na.rm = TRUE),
    fumbles = sum(receiving_fumbles, rushing_fumbles, sack_fumbles, na.rm = TRUE),
    fumbles_lost = sum(receiving_fumbles_lost, rushing_fumbles_lost, sack_fumbles_lost, na.rm = TRUE),
    official_ppr = sum(fantasy_points_ppr, na.rm = TRUE),
    official_fp = sum(fantasy_points, na.rm = TRUE),
    receiving_epa = sum(receiving_epa, na.rm = TRUE),
    rushing_epa = sum(rushing_epa, na.rm = TRUE),
    passing_epa = sum(passing_epa, na.rm = TRUE),
    .groups = "drop"
  )

# ---------------------------------------------------------------------------
# PBP usage by player
# ---------------------------------------------------------------------------

pbp_recv <- full_pbp %>%
  filter(pass_attempt == 1, down %in% 1:4, !is.na(receiver_id)) %>%
  group_by(player_id = receiver_id) %>%
  summarise(
    pbp_targets = n(),
    rz_targets = sum(yardline_100 <= RZ_LINE, na.rm = TRUE),
    rz_rec = sum(yardline_100 <= RZ_LINE & complete_pass == 1, na.rm = TRUE),
    ez_targets = sum(yardline_100 <= EZ_LINE, na.rm = TRUE),
    ez_rec = sum(yardline_100 <= EZ_LINE & complete_pass == 1, na.rm = TRUE),
    recv_first_downs = sum(first_down_pass == 1, na.rm = TRUE),
    recv_epa_pbp = sum(epa, na.rm = TRUE),
    recv_team = names(sort(table(posteam[!is.na(posteam)]), decreasing = TRUE))[1],
    .groups = "drop"
  )

pbp_rush <- full_pbp %>%
  filter(rush_attempt == 1, down %in% 1:4, !is.na(rusher_id)) %>%
  group_by(player_id = rusher_id) %>%
  summarise(
    pbp_carries = n(),
    rz_carries = sum(yardline_100 <= RZ_LINE, na.rm = TRUE),
    ez_carries = sum(yardline_100 <= EZ_LINE, na.rm = TRUE),
    rush_first_downs = sum(first_down_rush == 1, na.rm = TRUE),
    rush_epa_pbp = sum(epa, na.rm = TRUE),
    rush_team = names(sort(table(posteam[!is.na(posteam)]), decreasing = TRUE))[1],
    .groups = "drop"
  )

pbp_pass <- full_pbp %>%
  filter(pass_attempt == 1, down %in% 1:4, !is.na(passer_id)) %>%
  group_by(player_id = passer_id) %>%
  summarise(
    pbp_attempts = n(),
    rz_pass_att = sum(yardline_100 <= RZ_LINE, na.rm = TRUE),
    rz_pass_td = sum(yardline_100 <= RZ_LINE & pass_touchdown == 1, na.rm = TRUE),
    ez_pass_att = sum(yardline_100 <= EZ_LINE, na.rm = TRUE),
    ez_pass_td = sum(yardline_100 <= EZ_LINE & pass_touchdown == 1, na.rm = TRUE),
    pass_epa_pbp = sum(epa, na.rm = TRUE),
    cpoe = mean(cpoe, na.rm = TRUE),
    pass_team = names(sort(table(posteam[!is.na(posteam)]), decreasing = TRUE))[1],
    .groups = "drop"
  )

# ---------------------------------------------------------------------------
# Position tables
# ---------------------------------------------------------------------------

attach_identity <- function(df) {
  if (!"recv_team" %in% names(df)) df$recv_team <- NA_character_
  if (!"rush_team" %in% names(df)) df$rush_team <- NA_character_
  if (!"pass_team" %in% names(df)) df$pass_team <- NA_character_
  
  df %>%
    left_join(latest_roster, by = "player_id") %>%
    mutate(
      player_name = coalesce(player_name, player_name_stats),
      team = coalesce(team, team_stats, recv_team, rush_team, pass_team),
      position = coalesce(position, position_stats),
      headshot_url = coalesce(headshot_url, headshot_stats)
    ) %>%
    left_join(team_meta, by = c("team" = "team_abbr"))
}

skill_stats <- official_all %>%
  left_join(pbp_recv, by = "player_id") %>%
  left_join(pbp_rush, by = "player_id") %>%
  attach_identity() %>%
  left_join(snap_season, by = c("pfr_id" = "pfr_player_id")) %>%
  left_join(team_pass, by = c("team" = "posteam")) %>%
  left_join(team_rush, by = c("team" = "posteam")) %>%
  mutate(
    targets = coalesce(targets, pbp_targets, 0),
    carries = coalesce(carries, pbp_carries, 0),
    rz_targets = replace_na(rz_targets, 0),
    rz_rec = replace_na(rz_rec, 0),
    ez_targets = replace_na(ez_targets, 0),
    ez_rec = replace_na(ez_rec, 0),
    rz_carries = replace_na(rz_carries, 0),
    ez_carries = replace_na(ez_carries, 0),
    first_downs = replace_na(recv_first_downs, 0) + replace_na(rush_first_downs, 0),
    total_epa = coalesce(recv_epa_pbp, 0) + coalesce(rush_epa_pbp, 0),
    target_share = safe_div(targets, team_targets),
    rz_tgt_share = safe_div(rz_targets, team_rz_targets),
    ez_tgt_share = safe_div(ez_targets, team_ez_targets),
    rz_rec_share = safe_div(rz_rec, rz_targets),
    ez_rec_share = safe_div(ez_rec, ez_targets),
    fd_share = safe_div(first_downs, targets),
    carry_share = safe_div(carries, team_carries),
    ypc = safe_div(rushing_yards, carries),
    rush_fd_pct = safe_div(replace_na(rush_first_downs, 0), carries),
    rz_carry_share = safe_div(rz_carries, team_rz_carries),
    ez_carry_share = safe_div(ez_carries, team_ez_carries),
    total_td = replace_na(receiving_td, 0) + replace_na(rushing_td, 0),
    offense_snaps = replace_na(offense_snaps, 0),
    offense_pct = replace_na(offense_pct, 0),
    total_fp = coalesce(official_ppr, 0),
    fp_g = safe_div(total_fp, games_played)
  ) %>%
  filter(!is.na(player_name))

tgt_team <- skill_stats %>%
  filter(!is.na(team), replace_na(targets, 0) > 0) %>%
  transmute(
    player = player_name,
    team,
    position,
    value = targets,
    pct = target_share
  )

tgt_pie_plot <- function(team_abbr, positions, dark = FALSE) {
  share_pie_plot(
    tgt_team,
    team_abbr, positions, dark,
    title_metric = "target share",
    subtitle = "Slice size = target share",
    empty_noun = "target data"
  )
}

draw_tgt_pie <- function(team_abbr, positions, dark = FALSE) {
  draw_share_pie(tgt_pie_plot(team_abbr, positions, dark = dark), dark = dark)
}

wr_stats <- skill_stats %>%
  filter(position == "WR", targets > 0 | receiving_yards > 0 | rushing_yards > 0) %>%
  arrange(desc(total_fp), desc(targets)) %>%
  mutate(rank = row_number()) %>%
  select(
    rank, player_id, player_name, headshot_url, team, team_wordmark, team_logo_espn,
    games_played, offense_snaps, offense_pct, targets, receptions, receiving_yards, yards_after_catch, air_yards,
    target_share, receiving_td, rushing_yards, rushing_td, fumbles,
    rz_targets, rz_rec, rz_tgt_share, rz_rec_share,
    ez_targets, ez_rec, ez_tgt_share, ez_rec_share,
    first_downs, fd_share, total_epa, fp_g, total_fp
  )

te_stats <- skill_stats %>%
  filter(position == "TE", targets > 0 | receiving_yards > 0 | rushing_yards > 0) %>%
  arrange(desc(total_fp), desc(targets)) %>%
  mutate(rank = row_number()) %>%
  select(
    rank, player_id, player_name, headshot_url, team, team_wordmark, team_logo_espn,
    games_played, offense_snaps, offense_pct, targets, receptions, receiving_yards, yards_after_catch, air_yards,
    target_share, total_td, carries, rushing_yards, fumbles,
    rz_targets, rz_rec, rz_tgt_share, rz_rec_share,
    ez_targets, ez_rec, ez_tgt_share, ez_rec_share,
    first_downs, fd_share, total_epa, fp_g, total_fp
  )

rb_stats <- skill_stats %>%
  filter(position %in% c("RB", "FB", "HB"), carries > 0 | targets > 0) %>%
  arrange(desc(total_fp), desc(carries)) %>%
  mutate(rank = row_number()) %>%
  select(
    rank, player_id, player_name, headshot_url, team, team_wordmark, team_logo_espn,
    games_played, offense_snaps, offense_pct, carries, rushing_yards, ypc, rushing_td, carry_share,
    rz_carries, rz_carry_share, ez_carries, ez_carry_share,
    targets, receptions, receiving_yards, receiving_td,
    rush_first_downs, rush_fd_pct, total_epa, fp_g, total_fp
  )

qb_stats <- official_all %>%
  left_join(pbp_pass, by = "player_id") %>%
  left_join(pbp_rush, by = "player_id") %>%
  attach_identity() %>%
  left_join(snap_season, by = c("pfr_id" = "pfr_player_id")) %>%
  mutate(
    attempts = coalesce(attempts, pbp_attempts, 0),
    completions = replace_na(completions, 0),
    cmp_pct = safe_div(completions, attempts),
    ypa = safe_div(passing_yards, attempts),
    adot = safe_div(passing_air_yards, attempts),
    rz_pass_att = replace_na(rz_pass_att, 0),
    rz_pass_td = replace_na(rz_pass_td, 0),
    ez_pass_att = replace_na(ez_pass_att, 0),
    ez_pass_td = replace_na(ez_pass_td, 0),
    cpoe = replace_na(cpoe, 0),
    total_epa = coalesce(pass_epa_pbp, passing_epa, 0) + coalesce(rush_epa_pbp, rushing_epa, 0),
    total_fp = coalesce(official_ppr, official_fp, 0),
    fp_g = safe_div(total_fp, games_played),
    offense_snaps = replace_na(offense_snaps, 0),
    offense_pct = replace_na(offense_pct, 0)
  ) %>%
  filter(position == "QB", attempts > 0 | rushing_yards != 0) %>%
  arrange(desc(total_fp), desc(attempts)) %>%
  mutate(rank = row_number()) %>%
  select(
    rank, player_id, player_name, headshot_url, team, team_wordmark, team_logo_espn,
    games_played, offense_snaps, offense_pct, completions, attempts, cmp_pct, passing_yards, ypa, adot,
    passing_td, interceptions, sacks, cpoe,
    carries, rushing_yards, rushing_td,
    rz_pass_att, rz_pass_td, ez_pass_att, ez_pass_td,
    total_epa, fp_g, total_fp
  )

all_teams <- sort(unique(na.omit(c(wr_stats$team, te_stats$team, rb_stats$team, qb_stats$team))))

# ---------------------------------------------------------------------------
# Table builders
# ---------------------------------------------------------------------------

pass_catcher_table <- function(data, player_label = "Receiver", te = FALSE, dark = FALSE) {
  extra <- if (te) {
    list(
      total_td = colDef(name = "TDs"),
      carries = colDef(name = "Rush Att"),
      rushing_yards = colDef(name = "Rush Yds", format = colFormat(separators = TRUE))
    )
  } else {
    list(
      receiving_td = colDef(name = "Rec TD"),
      rushing_yards = colDef(name = "Rush Yds", format = colFormat(separators = TRUE)),
      rushing_td = colDef(name = "Rush TD")
    )
  }
  
  cols <- c(
    hidden_meta_cols(),
    list(
      rank = colDef(name = "Rk", minWidth = 48, filterable = FALSE),
      player_name = colDef(name = player_label, align = "left", minWidth = 210, sticky = "left", cell = player_cell(data), style = zebra_style(dark)),
      team = colDef(name = "Team", minWidth = 84, cell = team_cell(data), style = zebra_style(dark)),
      games_played = colDef(name = "G", minWidth = 46),
      offense_snaps = colDef(name = "Off Snaps", style = style_numeric(data$offense_snaps, "good", dark = dark)),
      offense_pct = colDef(name = "Off Snap %", format = colFormat(percent = TRUE, digits = 0), style = style_numeric(data$offense_pct, "good", dark = dark)),
      targets = colDef(name = "Tgt", style = style_numeric(data$targets, "good", dark = dark)),
      receptions = colDef(name = "Rec"),
      receiving_yards = colDef(name = "Rec Yds", format = colFormat(separators = TRUE)),
      yards_after_catch = colDef(name = "YAC", format = colFormat(separators = TRUE)),
      air_yards = colDef(name = "Air Yds", format = colFormat(separators = TRUE)),
      target_share = colDef(name = "Tgt %", format = colFormat(percent = TRUE, digits = 0), style = style_numeric(data$target_share, "good", dark = dark))
    ),
    extra,
    list(
      fumbles = colDef(name = "Fum", style = style_numeric(data$fumbles, "bad", dark = dark)),
      rz_targets = colDef(name = "RZ Tgt"),
      rz_rec = colDef(name = "RZ Rec"),
      rz_tgt_share = colDef(name = "RZ Tgt %", format = colFormat(percent = TRUE, digits = 0), style = style_numeric(data$rz_tgt_share, "good", dark = dark)),
      rz_rec_share = colDef(name = "RZ Rec %", format = colFormat(percent = TRUE, digits = 0), style = style_numeric(data$rz_rec_share, "good", dark = dark)),
      ez_targets = colDef(name = "EZ Tgt"),
      ez_rec = colDef(name = "EZ Rec"),
      ez_tgt_share = colDef(name = "EZ Tgt %", format = colFormat(percent = TRUE, digits = 0), style = style_numeric(data$ez_tgt_share, "good", dark = dark)),
      ez_rec_share = colDef(name = "EZ Rec %", format = colFormat(percent = TRUE, digits = 0), style = style_numeric(data$ez_rec_share, "good", dark = dark)),
      first_downs = colDef(name = "FD"),
      fd_share = colDef(name = "FD %", format = colFormat(percent = TRUE, digits = 0)),
      total_epa = colDef(name = "EPA", format = colFormat(digits = 1), style = style_numeric(data$total_epa, "value", dark = dark)),
      fp_g = colDef(name = "PPR/G", format = colFormat(digits = 1), style = style_numeric(data$fp_g, "value", dark = dark)),
      total_fp = colDef(name = "PPR", format = colFormat(digits = 1), style = style_numeric(data$total_fp, "value", dark = dark))
    )
  )
  
  base_reactable(data, cols, dark = dark)
}

rb_table <- function(data, dark = FALSE) {
  cols <- c(
    hidden_meta_cols(),
    list(
      rank = colDef(name = "Rk", minWidth = 48, filterable = FALSE),
      player_name = colDef(name = "Running Back", align = "left", minWidth = 210, sticky = "left", cell = player_cell(data), style = zebra_style(dark)),
      team = colDef(name = "Team", minWidth = 84, cell = team_cell(data), style = zebra_style(dark)),
      games_played = colDef(name = "G", minWidth = 46),
      offense_snaps = colDef(name = "Off Snaps", style = style_numeric(data$offense_snaps, "good", dark = dark)),
      offense_pct = colDef(name = "Off Snap %", format = colFormat(percent = TRUE, digits = 0), style = style_numeric(data$offense_pct, "good", dark = dark)),
      carries = colDef(name = "Car", style = style_numeric(data$carries, "good", dark = dark)),
      rushing_yards = colDef(name = "Rush Yds", format = colFormat(separators = TRUE)),
      ypc = colDef(name = "YPC", format = colFormat(digits = 1), style = style_numeric(data$ypc, "value", dark = dark)),
      rushing_td = colDef(name = "Rush TD"),
      carry_share = colDef(name = "Car %", format = colFormat(percent = TRUE, digits = 0), style = style_numeric(data$carry_share, "good", dark = dark)),
      rz_carries = colDef(name = "RZ Car"),
      rz_carry_share = colDef(name = "RZ Car %", format = colFormat(percent = TRUE, digits = 0), style = style_numeric(data$rz_carry_share, "good", dark = dark)),
      ez_carries = colDef(name = "EZ Car"),
      ez_carry_share = colDef(name = "EZ Car %", format = colFormat(percent = TRUE, digits = 0), style = style_numeric(data$ez_carry_share, "good", dark = dark)),
      targets = colDef(name = "Tgt"),
      receptions = colDef(name = "Rec"),
      receiving_yards = colDef(name = "Rec Yds", format = colFormat(separators = TRUE)),
      receiving_td = colDef(name = "Rec TD"),
      rush_first_downs = colDef(name = "Rush FD"),
      rush_fd_pct = colDef(name = "FD %", format = colFormat(percent = TRUE, digits = 0)),
      total_epa = colDef(name = "EPA", format = colFormat(digits = 1), style = style_numeric(data$total_epa, "value", dark = dark)),
      fp_g = colDef(name = "PPR/G", format = colFormat(digits = 1), style = style_numeric(data$fp_g, "value", dark = dark)),
      total_fp = colDef(name = "PPR", format = colFormat(digits = 1), style = style_numeric(data$total_fp, "value", dark = dark))
    )
  )
  base_reactable(data, cols, dark = dark)
}

qb_table <- function(data, dark = FALSE) {
  cols <- c(
    hidden_meta_cols(),
    list(
      rank = colDef(name = "Rk", minWidth = 48, filterable = FALSE),
      player_name = colDef(name = "Quarterback", align = "left", minWidth = 210, sticky = "left", cell = player_cell(data), style = zebra_style(dark)),
      team = colDef(name = "Team", minWidth = 84, cell = team_cell(data), style = zebra_style(dark)),
      games_played = colDef(name = "G", minWidth = 46),
      offense_snaps = colDef(name = "Off Snaps", style = style_numeric(data$offense_snaps, "good", dark = dark)),
      offense_pct = colDef(name = "Off Snap %", format = colFormat(percent = TRUE, digits = 0), style = style_numeric(data$offense_pct, "good", dark = dark)),
      completions = colDef(name = "Cmp"),
      attempts = colDef(name = "Att", style = style_numeric(data$attempts, "good", dark = dark)),
      cmp_pct = colDef(name = "Cmp %", format = colFormat(percent = TRUE, digits = 1)),
      passing_yards = colDef(name = "Pass Yds", format = colFormat(separators = TRUE), style = style_numeric(data$passing_yards, "good", dark = dark)),
      ypa = colDef(name = "YPA", format = colFormat(digits = 1)),
      adot = colDef(name = "ADOT", format = colFormat(digits = 1)),
      passing_td = colDef(name = "Pass TD"),
      interceptions = colDef(name = "INT", style = style_numeric(data$interceptions, "bad", dark = dark)),
      sacks = colDef(name = "Sck", style = style_numeric(data$sacks, "bad", dark = dark)),
      cpoe = colDef(name = "CPOE", format = colFormat(digits = 1), style = style_numeric(data$cpoe, "value", dark = dark)),
      carries = colDef(name = "Rush Att"),
      rushing_yards = colDef(name = "Rush Yds", format = colFormat(separators = TRUE)),
      rushing_td = colDef(name = "Rush TD"),
      rz_pass_att = colDef(name = "RZ Att"),
      rz_pass_td = colDef(name = "RZ TD"),
      ez_pass_att = colDef(name = "EZ Att"),
      ez_pass_td = colDef(name = "EZ TD"),
      total_epa = colDef(name = "EPA", format = colFormat(digits = 1), style = style_numeric(data$total_epa, "value", dark = dark)),
      fp_g = colDef(name = "PPR/G", format = colFormat(digits = 1), style = style_numeric(data$fp_g, "value", dark = dark)),
      total_fp = colDef(name = "PPR", format = colFormat(digits = 1), style = style_numeric(data$total_fp, "value", dark = dark))
    )
  )
  
  base_reactable(
    data,
    cols,
    column_groups = list(
      colGroup(name = "Passing", columns = c("completions", "attempts", "cmp_pct", "passing_yards", "ypa", "adot", "passing_td", "interceptions", "sacks", "cpoe")),
      colGroup(name = "Rushing", columns = c("carries", "rushing_yards", "rushing_td")),
      colGroup(name = "RZ / EZ passing", columns = c("rz_pass_att", "rz_pass_td", "ez_pass_att", "ez_pass_td"))
    ),
    dark = dark
  )
}

# ---------------------------------------------------------------------------
# Shiny app
# ---------------------------------------------------------------------------

slider_max <- function(x, floor_val) {
  x <- suppressWarnings(as.numeric(x))
  x <- x[is.finite(x)]
  max(floor_val, if (length(x)) max(x) else floor_val)
}

ui <- fluidPage(
  theme = bs_theme(
    version = 5,
    base_font = font_google("Oswald"),
    heading_font = font_google("Silkscreen")
  ),
  
  tags$head(
    tags$style(HTML("
      .title-header-banner {
        background: linear-gradient(135deg, #112233 0%, #1f3a60 100%);
        color: #ffffff !important;
        padding: 24px;
        margin: -20px -20px 24px -20px;
        border-bottom: 4px solid #31a354;
      }
      .title-header-banner h2 {
        margin: 0;
        font-weight: 700;
        text-transform: uppercase;
        letter-spacing: 0.5px;
      }
      .title-header-banner p {
        margin: 8px 0 0;
        opacity: 0.85;
        font-family: Oswald, sans-serif;
        letter-spacing: 0.3px;
      }
      .sidebar-panel-styled {
        background-color: #ffffff;
        color: #212529;
        border: 1px solid #e2e8f0;
        border-radius: 12px;
        padding: 24px;
        box-shadow: 0 4px 6px -1px rgba(0, 0, 0, 0.05), 0 2px 4px -1px rgba(0, 0, 0, 0.03);
      }
      [data-bs-theme='dark'] .sidebar-panel-styled {
        background-color: #1e293b !important;
        color: #f8fafc !important;
        border-color: #334155;
      }
      [data-bs-theme='dark'] .rt-table,
      [data-bs-theme='dark'] .Reactable,
      [data-bs-theme='dark'] .reactable {
        color: #f8fafc !important;
        background-color: #0f172a !important;
      }
      [data-bs-theme='dark'] .rt-tbody .rt-tr:nth-child(odd) .rt-td {
        background-color: #0f172a;
        color: #f8fafc;
      }
      [data-bs-theme='dark'] .rt-tbody .rt-tr:nth-child(even) .rt-td {
        background-color: #162033;
        color: #f8fafc;
      }
      [data-bs-theme='dark'] .rt-pagination,
      [data-bs-theme='dark'] .rt-search,
      [data-bs-theme='dark'] .rt-th-filter {
        background-color: #0f172a;
        color: #f8fafc;
      }
      [data-bs-theme='dark'] .tab-content,
      [data-bs-theme='dark'] .tabbable,
      [data-bs-theme='dark'] .tab-pane,
      [data-bs-theme='dark'] .main-content,
      [data-bs-theme='dark'] .col-sm-9,
      [data-bs-theme='dark'] .col-lg-9,
      [data-bs-theme='dark'] .shiny-plot-output,
      [data-bs-theme='dark'] .shiny-plot-output img {
        background-color: #0b1220 !important;
      }
      [data-bs-theme='dark'] body,
      [data-bs-theme='dark'] .container-fluid {
        background-color: #0b1220 !important;
      }
      [data-bs-theme='dark'] .nav-tabs {
        border-bottom-color: #334155;
      }
      [data-bs-theme='dark'] .nav-tabs .nav-link {
        color: #cbd5e1;
      }
      [data-bs-theme='dark'] .nav-tabs .nav-link.active {
        background-color: #1e293b;
        color: #f8fafc;
        border-color: #334155 #334155 #0b1220;
      }
    "))
  ),
  
  div(
    class = "title-header-banner",
    tags$h2("Fantasy Sports Pack 2026 Player Stats Tool"),
    tags$p(paste(week_label, "| Data: nflreadr | PPR scoring"))
  ),
  
  sidebarLayout(
    sidebarPanel(
      class = "sidebar-panel-styled",
      width = 3,
      div(
        style = "display: flex; justify-content: space-between; align-items: center;",
        p("Theme Toggle:", style = "margin: 0; font-weight: bold;"),
        input_dark_mode(id = "color_mode")
      ),
      hr(),
      selectInput("team", "Team", choices = c("All teams" = "ALL", all_teams), selected = "ALL"),
      conditionalPanel(
        condition = "input.pos_tab == 'WR'",
        sliderInput(
          "min_wr", "Minimum targets",
          min = 0, max = slider_max(wr_stats$targets, 5), value = 1, step = 1
        )
      ),
      conditionalPanel(
        condition = "input.pos_tab == 'TE'",
        sliderInput(
          "min_te", "Minimum targets",
          min = 0, max = slider_max(te_stats$targets, 5), value = 1, step = 1
        )
      ),
      conditionalPanel(
        condition = "input.pos_tab == 'RB'",
        sliderInput(
          "min_rb", "Minimum carries",
          min = 0, max = slider_max(rb_stats$carries, 10), value = 10, step = 1
        )
      ),
      conditionalPanel(
        condition = "input.pos_tab == 'QB'",
        sliderInput(
          "min_qb", "Minimum pass attempts",
          min = 0, max = slider_max(qb_stats$attempts, 10), value = 10, step = 1
        )
      ),
      conditionalPanel(
        condition = "input.pos_tab == 'Snap Share' || input.pos_tab == 'Target Share'",
        checkboxGroupInput(
          "pie_pos",
          "Positions",
          choices = c("QB", "RB", "WR", "TE"),
          selected = c("QB", "RB", "WR", "TE"),
          inline = TRUE
        )
      ),
      conditionalPanel(
        condition = "input.pos_tab == 'Snap Share'",
        downloadButton("snap_download", "Download plot"),
        helpText("Pick one team. Slice size is offensive snap share.")
      ),
      conditionalPanel(
        condition = "input.pos_tab == 'Target Share'",
        downloadButton("tgt_download", "Download plot"),
        helpText("Pick one team. Slice size is target share.")
      ),
      conditionalPanel(
        condition = "input.pos_tab != 'Snap Share' && input.pos_tab != 'Target Share'",
        helpText("RZ = inside the 25. EZ = inside the 5. WR/TE Tgt % = player zone targets / team zone targets. Rec % = zone receptions / zone targets. RB Car % = player zone carries / team zone carries.")
      )
    ),
    mainPanel(
      width = 9,
      tabsetPanel(
        id = "pos_tab",
        tabPanel("WR", reactableOutput("wr_table")),
        tabPanel("TE", reactableOutput("te_table")),
        tabPanel("RB", reactableOutput("rb_table")),
        tabPanel("QB", reactableOutput("qb_table")),
        tabPanel(
          "Snap Share",
          br(),
          plotOutput("snap_pie", height = "760px")
        ),
        tabPanel(
          "Target Share",
          br(),
          plotOutput("tgt_pie", height = "760px")
        )
      ),
      tags$p(
        style = "color:#64748b;font-size:12px;margin-top:12px;",
        "By Jake Mammen | X: @FantasySPack"
      )
    )
  )
)

filter_team <- function(df, team) {
  if (!identical(team, "ALL")) df <- df[df$team == team, , drop = FALSE]
  df
}

rerank <- function(df) {
  df$rank <- seq_len(nrow(df))
  df
}

server <- function(input, output, session) {
  is_dark <- reactive({
    identical(input$color_mode, "dark")
  })
  
  wr_f <- reactive({
    df <- filter_team(wr_stats, input$team)
    rerank(df[df$targets >= input$min_wr, , drop = FALSE])
  })
  te_f <- reactive({
    df <- filter_team(te_stats, input$team)
    rerank(df[df$targets >= input$min_te, , drop = FALSE])
  })
  rb_f <- reactive({
    df <- filter_team(rb_stats, input$team)
    rerank(df[df$carries >= input$min_rb, , drop = FALSE])
  })
  qb_f <- reactive({
    df <- filter_team(qb_stats, input$team)
    rerank(df[df$attempts >= input$min_qb, , drop = FALSE])
  })
  
  output$wr_table <- renderReactable({
    pass_catcher_table(wr_f(), "Receiver", te = FALSE, dark = is_dark())
  })
  output$te_table <- renderReactable({
    pass_catcher_table(te_f(), "Tight End", te = TRUE, dark = is_dark())
  })
  output$rb_table <- renderReactable({
    rb_table(rb_f(), dark = is_dark())
  })
  output$qb_table <- renderReactable({
    qb_table(qb_f(), dark = is_dark())
  })
  
  output$snap_pie <- renderPlot({
    draw_snap_pie(input$team, input$pie_pos, dark = is_dark())
  }, bg = "transparent")
  
  output$tgt_pie <- renderPlot({
    draw_tgt_pie(input$team, input$pie_pos, dark = is_dark())
  }, bg = "transparent")
  
  pie_filename <- function(kind) {
    week_bit <- if (is.finite(max_week)) paste0("week", max_week) else paste0(season)
    team_bit <- if (identical(input$team, "ALL")) "team" else input$team
    paste0(team_bit, "_", kind, "_", week_bit, ".png")
  }
  
  output$snap_download <- downloadHandler(
    filename = function() pie_filename("snap_share"),
    content = function(file) {
      bg_col <- if (is_dark()) "#0b1220" else "#ffffff"
      grDevices::png(file, width = 1400, height = 1100, res = 140, bg = bg_col)
      draw_snap_pie(input$team, input$pie_pos, dark = is_dark())
      grDevices::dev.off()
    }
  )
  
  output$tgt_download <- downloadHandler(
    filename = function() pie_filename("target_share"),
    content = function(file) {
      bg_col <- if (is_dark()) "#0b1220" else "#ffffff"
      grDevices::png(file, width = 1400, height = 1100, res = 140, bg = bg_col)
      draw_tgt_pie(input$team, input$pie_pos, dark = is_dark())
      grDevices::dev.off()
    }
  )
}

shinyApp(ui, server)