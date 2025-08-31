# =============================================================================
# MORRIS WATER MAZE - MAIN ANALYSIS SCRIPT
# =============================================================================

source("R/functions.R")

# =============================================================================
# DATA LOADING AND PREPROCESSING
# =============================================================================

# Load main dataset
file_paths_main <- c(
  "../data/df_wm_clean_final_v2.csv",
  "./data/df_wm_clean_final_v2.csv"
)
df_wm_raw <- load_data_safe(file_paths_main)
df_wm_flx <- process_wm_data(df_wm_raw)

# Load estrategias data
file_paths_pre <- c(
  "../data/estrategias_pre_good_juntos.csv",
  "./data/estrategias_pre_good_juntos.csv"
)
file_paths_post <- c(
  "../data/estrategias_post_juntos.csv",
  "./data/estrategias_post_juntos.csv"
)

estrategias_df_pre <- load_data_safe(file_paths_pre) %>% recode_estrategias()
estrategias_df_post <- load_data_safe(file_paths_post) %>% recode_estrategias()

# Load entropy data
entropia <- read.csv("./data/entropia_clean.csv") %>%
  janitor::clean_names() %>%
  filter(!str_detect(experimento, "Ket")) %>%
  mutate(
    Tratamiento.Farm = case_when(
      tratamiento %in% c("Flx", "Fluoxetina-CUMS") ~ "Fluoxetina",
      tratamiento %in% c("Salina-CUMS-F", "Salina-Control") ~ "Salina"
    ),
    Estres = case_when(
      tratamiento %in% c("Fluoxetina-CUMS", "Salina-CUMS-F") ~ "CUMS",
      tratamiento %in% c("Flx", "Salina-Control") ~ "NO-Estrés"
    ),
    entropia_blanco = case_when(
      stage %in% c("prueba_1", "prueba_2") ~ entropia_ne,
      stage == "prueba_rev" ~ entropia_so
    ),
    dia = case_when(
      stage == "prueba_1" ~ 1,
      stage == "prueba_2" ~ 2,
      stage == "prueba_rev" ~ 3
    ),
    Tratamiento.Farm = factor(
      Tratamiento.Farm,
      levels = c("Salina", "Fluoxetina")
    )
  )

message("✓ Data loading and preprocessing completed")

# =============================================================================
# VARIABLES PARA USO COMÚN
# =============================================================================

n_animales <- df_wm_flx|>
  group_by(Tratamiento, Estres) |>
  summarise(n = n_distinct(id), .groups = "drop")

# n_animales

# Como lista con nombres
n_list <- n_animales %>%
  mutate(grupo = paste(Tratamiento, Estres, sep = "_")) %>%
  select(grupo, n) %>%
  deframe()

# n_list

# como texto directo para ggplot2
caption_text <- n_animales %>%
  mutate(txt = paste0(Tratamiento, "-", Estres, ": n=", n)) %>%
  pull(txt) %>%
  paste(collapse = "; ")

# caption_text

# ggplot(df_wm_flx, aes(...)) +
#   geom_boxplot() +
#   labs(caption = paste("Graficado con media ± error estándar.", caption_text))



# =============================================================================
# TRAINING PHASE ANALYSIS
# =============================================================================

# Escape latencies during training
latencies_analysis <- run_mwm_analysis(
  df = df_wm_flx,
  test_filter = "Ent",
  dv = "latencia",
  id_col = "id",
  within_col = "Tiempo",
  between_cols = c("Tratamiento", "Estres"),
  treatment_var = "Tratamiento",
  facet_var = "Estres",
  geom = "line",
  x_lab_name = "Día",
  y_lab_name = "Latencia (seg)",
  hline_val = 0,
  limy = 60,
  animate = FALSE
)

# latencies_analysis$anova_results
# format_anova_table(latencies_analysis$anova_results, "Caption")

# Add significance annotations
latencies_plot <- latencies_analysis$plot +
  geom_text(
    data = data.frame(Tiempo = 1, latencia = 38, Estres = factor("NO-Estrés")),
    aes(x = Tiempo, y = latencia, label = "*"),
    colour = "#2c3e50",
    size = 7,
    inherit.aes = FALSE
  ) +
  geom_text(
    data = data.frame(Tiempo = 2, latencia = 32, Estres = factor("CUMS")),
    aes(x = Tiempo, y = latencia, label = "**"),
    colour = "#e74c3c",
    size = 7,
    inherit.aes = FALSE
  ) +
  labs(
    title = "Latencias de Escape - Fase de Entrenamiento",
    caption = paste("Graficado con media ± error estándar.", caption_text)
  ) +
  scale_x_continuous(
    n.breaks = 4,
    labels = c("1", "2", "3", "4"),
    expand = c(0, 0.1)
  )

# latencies_plot

latencies_post_analysis <- run_mwm_analysis(
  df = df_wm_flx,
  test_filter = "Reversa",
  dv = "latencia",
  id_col = "id",
  within_col = "Tiempo",
  between_cols = c("Tratamiento", "Estres"),
  treatment_var = "Tratamiento",
  facet_var = "Estres",
  geom = "line",
  x_lab_name = "Día",
  y_lab_name = "Latencia (seg)",
  hline_val = 0,
  limy = 60,
  animate = FALSE
)

latencies_reversa_plot <- latencies_post_analysis$plot +
  # geom_text(
  #   data = data.frame(Tiempo = 1, latencia = 38, Estres = factor("NO-Estrés")),
  #   aes(x = Tiempo, y = latencia, label = "*"),
  #   colour = "#2c3e50",
  #   size = 7,
  #   inherit.aes = FALSE
  # ) +
  # geom_text(
  #   data = data.frame(Tiempo = 2, latencia = 32, Estres = factor("CUMS")),
  #   aes(x = Tiempo, y = latencia, label = "**"),
  #   colour = "#e74c3c",
  #   size = 7,
  #   inherit.aes = FALSE
  # ) +
  labs(
    title = "Latencias de Escape - Fase de Entrenamientos Reversa",
    caption = paste("Graficado con media ± error estándar.", caption_text)
  ) +
  scale_x_continuous(
    n.breaks = 2,
    labels = c("1", "2"),
    expand = c(0, 0.1)
  )

# Search strategies during training
estrategias_plot <- create_estrategias_plot(estrategias_df_pre, animate = FALSE)

message("✓ Training phase analysis completed")

# =============================================================================
# PROBE TESTS ANALYSIS
# =============================================================================

# Create progressive revelation plots for probe tests
probe_tests <- c("P-1", "P-2", "P-Rev")
progressive_analyses <- create_progressive_plots(
  df = df_wm_flx,
  tests = probe_tests,
  cols_to_pivot = c("zona_blanco", "zona_reversa"),
  dv = "Porcentaje",
  id_col = "id",
  within_col = "Preferencia",
  between_cols = c("Tratamiento", "Estres"),
  treatment_var = "Tratamiento",
  facet_var = "Estres",
  strip_colors = c("#ecf0f1", "#e74c3c")
)

# Add specific annotations for reversal test
reversal_comps <- list(
  data.frame(
    Preferencia = "zona_blanco",
    Porcentaje = 17,
    lab = "*",
    Estres = factor("NO-Estrés"),
    Tratamiento = factor("Salina")
  ),
  data.frame(
    Preferencia = "zona_reversa",
    Porcentaje = 15,
    lab = "*",
    Estres = factor("NO-Estrés"),
    Tratamiento = factor("Salina")
  ),
  data.frame(
    Preferencia = "zona_blanco",
    Porcentaje = 17,
    lab = "***",
    Estres = factor("CUMS"),
    Tratamiento = factor("Salina")
  ),
  data.frame(
    Preferencia = "zona_reversa",
    Porcentaje = 14,
    lab = "*",
    Estres = factor("CUMS"),
    Tratamiento = factor("Salina")
  )
)

progressive_analyses[["P-Rev"]]$plot <- progressive_analyses[["P-Rev"]]$plot +
  geom_text(
    data = reversal_comps[[1]],
    aes(label = lab),
    colour = "#2c3e50",
    size = 7
  ) +
  geom_text(
    data = reversal_comps[[2]],
    aes(label = lab),
    colour = "#2c3e50",
    size = 7
  ) +
  geom_text(
    data = reversal_comps[[3]],
    aes(label = lab),
    colour = "#e74c3c",
    size = 7
  ) +
  geom_text(
    data = reversal_comps[[4]],
    aes(label = lab),
    colour = "#e74c3c",
    size = 7
  )

message("✓ Probe tests analysis completed")

# =============================================================================
# CREATE ANIMATED COMPARISON
# =============================================================================

# Create animated comparison across all probe tests
animated_comparison <- create_animated_comparison(
  df = df_wm_flx,
  tests = probe_tests,
  cols_to_pivot = c("zona_blanco", "zona_reversa"),
  dv = "Porcentaje"
)

message("✓ Animated comparison created")

# =============================================================================
# DISTANCE TO TARGET ANALYSIS
# =============================================================================

distance_analyses <- map(
  probe_tests,
  ~ {
    df_filtered <- prepare_data(df_wm_flx, .x)

    anova_res <- df_filtered %>%
      rstatix::anova_test(
        dv = distancia_media_blanco,
        wid = id,
        between = c("Tratamiento", "Estres"),
        type = 3,
        detailed = FALSE
      )

    anova_table <- rstatix::get_anova_table(anova_res, correction = "auto") %>%
      rstatix::add_significance() %>%
      rstatix::p_format()

    anova_caption <- rstatix::get_test_label(anova_res, detailed = TRUE)

    plot <- ggplot(
      df_filtered,
      aes(Estres, distancia_media_blanco, fill = Tratamiento)
    ) +
      stat_summary(
        fun = "mean",
        geom = "col",
        color = "black",
        width = 0.8,
        position = position_dodge(width = 0.85)
      ) +
      stat_summary(
        fun.data = "mean_se",
        geom = "errorbar",
        width = 0.18,
        size = 0.6,
        position = position_dodge(width = 0.85)
      ) +
      scale_fill_manual(values = get_treatment_colors()) +
      theme_mwm() +
      labs(
        x = "Estrés",
        y = "Distancia (m)",
        title = paste("Distancia Media al Annulus -", toupper(.x)),
        subtitle = anova_caption
      ) +
      scale_y_continuous(limits = c(0, 0.8), expand = c(0, 0.02))

    list(plot = plot, anova = anova_table)
  }
)
names(distance_analyses) <- probe_tests

message("✓ Distance analysis completed")

# =============================================================================
# EXPORT RESULTS FOR REVEAL.JS
# =============================================================================

# Save plots for reveal.js presentation
save_plot_for_reveal <- function(plot, filename, width = 12, height = 8) {
  ggsave(
    filename = paste0("plots/", filename),
    plot = plot,
    width = width,
    height = height,
    dpi = 300,
    bg = "white"
  )
}

# Create plots directory
if (!dir.exists("plots")) {
  dir.create("plots")
}

# Save static plots
# save_plot_for_reveal(latencies_plot, "latencies_training.png")
# save_plot_for_reveal(estrategias_plot, "search_strategies.png")

# Save progressive revelation plots
# iwalk(
#   progressive_analyses,
#   ~ {
#     save_plot_for_reveal(.x$plot, paste0("probe_test_", .y, ".png"))
#   }
# )

# Save distance plots
# iwalk(
#   distance_analyses,
#   ~ {
#     save_plot_for_reveal(.x$plot, paste0("distance_", .y, ".png"))
#   }
# )

# Save animated plots
# if (requireNamespace("gganimate", quietly = TRUE)) {
#   anim_latencies <- latencies_analysis$plot +
#     gganimate::transition_reveal(Tiempo) +
#     labs(title = "Latencias de Escape - Día: {closest_state}")

#   gganimate::anim_save(
#     "plots/latencies_animated.gif",
#     anim_latencies,
#     width = 1200,
#     height = 800,
#     res = 150
#   )

#   gganimate::anim_save(
#     "plots/probe_tests_comparison.gif",
#     animated_comparison,
#     width = 1400,
#     height = 800,
#     res = 150
#   )
# }

# message("✓ Plots exported for Reveal.js presentation")

# =============================================================================
# CREATE SUMMARY TABLES
# =============================================================================

# Format ANOVA tables
latencies_table <- format_anova_table(
  latencies_analysis$anova_results,
  "anova: latencias de escape"
)

probe_tables <- map(
  progressive_analyses,
  ~ {
    format_anova_table(.x$anova_results, "ANOVA: Tiempo en Zonas")
  }
)

# distance_tables <- map(
#   distance_analyses,
#   ~ {
#     format_anova_table(.x$anova, "ANOVA: Distancia Media")
#   }
# )

# message("✓ Summary tables created")
# message("🎉 Analysis pipeline completed successfully!")

# =============================================================================
# INTERACTIVE DASHBOARD (OPTIONAL)
# =============================================================================

# if (requireNamespace("plotly", quietly = TRUE)) {
#   # Create interactive versions of key plots
#   interactive_latencies <- plotly::ggplotly(
#     latencies_plot,
#     tooltip = c("x", "y", "colour")
#   )

#   interactive_probes <- map(
#     progressive_analyses,
#     ~ {
#       plotly::ggplotly(.x$plot, tooltip = c("x", "y", "fill"))
#     }
#   )

#   message("✓ Interactive plots created")
# }

# =============================================================================
# NEW ANALYSES FOR CONTROLLED PRESENTATION
# =============================================================================

# Create distance analyses
# distance_progression <- create_distance_analyses(df_wm_flx)

# Create latencies reversa analysis
# latencies_reversa_progression <- create_latencies_reversa_analysis(df_wm_flx)

# Export individual static images for distance progression
# iwalk(
#   distance_progression$individual_plots,
#   ~ {
#     ggsave(
#       filename = paste0("plots/distance_", .y, ".png"),
#       plot = .x,
#       width = 10,
#       height = 6,
#       dpi = 300,
#       bg = "white"
#     )
#   }
# )

# Export individual static images for latencies reversa
# iwalk(
#   latencies_reversa_progression$individual_plots,
#   ~ {
#     ggsave(
#       filename = paste0("plots/latencies_reversa_", .y, ".png"),
#       plot = .x,
#       width = 10,
#       height = 6,
#       dpi = 300,
#       bg = "white"
#     )
#   }
# )

# Create and save animated GIFs
# if (requireNamespace("gganimate", quietly = TRUE)) {
  
#   # Distance animation
#   distance_data <- df_wm_flx %>% 
#     filter(str_detect(prueba, "P-")) %>%
#     mutate(
#       Test_Label = case_when(
#         prueba == "P-1" ~ "Prueba 1: Adquisición",
#         prueba == "P-2" ~ "Prueba 2: Retención",
#         prueba == "P-Rev" ~ "Prueba 3: Reversa"
#       )
#     )
  
  # distance_anim <- ggplot(
  #   distance_data,
  #   aes(Estres, distancia_media_blanco, color = Tratamiento, group = Tratamiento)
  # ) +
  #   stat_summary(fun = "mean", geom = "line", size = 1.2, 
  #                position = position_dodge(0.1)) +
  #   stat_summary(fun = "mean", geom = "point", size = 3.5,
  #                position = position_dodge(0.1)) +
  #   stat_summary(fun.data = "mean_se", geom = "errorbar", 
  #                width = 0.1, size = 0.8, position = position_dodge(0.1)) +
  #   scale_color_manual(values = c("#bba800", "#006A8E")) +
  #   scale_y_continuous(limits = c(0, 0.8)) +
  #   labs(
  #     title = "Distancia Media al Annulus: {closest_state}",
  #     x = "Estrés",
  #     y = "Distancia Media (m)"
  #   ) +
  #   theme_classic(base_size = 14) +
  #   theme(
  #     legend.position = "top",
  #     plot.title = element_text(size = 16, face = "bold")
  #   ) +
  #   gganimate::transition_states(
  #     Test_Label,
  #     transition_length = 2,
  #     state_length = 3
  #   ) +
  #   gganimate::ease_aes('cubic-in-out')
  
  # gganimate::anim_save(
  #   "plots/distance_progression.gif",
  #   distance_anim,
  #   width = 1000,
  #   height = 600,
  #   res = 150
  # )
  
  # Latencies reversa animation
  # latencies_reversa_data <- df_wm_flx %>% 
  #   filter(str_detect(prueba, "Reversa"))
  
#   latencies_reversa_anim <- ggplot(
#     latencies_reversa_data,
#     aes(Estres, latencia, color = Tratamiento, group = Tratamiento)
#   ) +
#     stat_summary(fun = "mean", geom = "line", size = 1.2, 
#                  position = position_dodge(0.1)) +
#     stat_summary(fun = "mean", geom = "point", size = 3.5,
#                  position = position_dodge(0.1)) +
#     stat_summary(fun.data = "mean_se", geom = "errorbar", 
#                  width = 0.1, size = 0.8, position = position_dodge(0.1)) +
#     scale_color_manual(values = c("#bba800", "#006A8E")) +
#     scale_y_continuous(limits = c(0, 60)) +
#     labs(
#       title = "Latencias de Escape - Día: {closest_state}",
#       x = "Estrés",
#       y = "Latencia (seg)"
#     ) +
#     theme_classic(base_size = 14) +
#     theme(
#       legend.position = "top",
#       plot.title = element_text(size = 16, face = "bold")
#     ) +
#     gganimate::transition_states(
#       Tiempo,
#       transition_length = 2,
#       state_length = 3
#     ) +
#     gganimate::ease_aes('cubic-in-out')
  
#   gganimate::anim_save(
#     "plots/latencies_reversa_progression.gif",
#     latencies_reversa_anim,
#     width = 1000,
#     height = 600,
#     res = 150
#   )
# }

message("✓ New controlled presentation analyses completed")
