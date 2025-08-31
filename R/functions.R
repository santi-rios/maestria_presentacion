# =============================================================================
# MORRIS WATER MAZE ANALYSIS FUNCTIONS
# =============================================================================

# Load required libraries
suppressPackageStartupMessages({
  library(tidyverse)
  library(ggprism)
  library(gghighlight)
  library(ggpattern)
  library(patchwork)
  library(lmerTest)
  library(interactions)
  library(emmeans)
  library(kableExtra)
  library(rstatix)
  library(ggpubr)
  library(gganimate)
  library(plotly)
})

# =============================================================================
# DATA LOADING AND PREPROCESSING FUNCTIONS
# =============================================================================

#' Load data from multiple possible file paths
#' @param paths Character vector of file paths to try
#' @return data.frame or error if no file found
load_data_safe <- function(paths) {
  for (path in paths) {
    if (file.exists(path)) {
      message(paste("Loading data from:", path))
      return(read.csv(path))
    }
  }
  stop(
    "CSV file not found in any of the specified paths: ",
    paste(paths, collapse = ", ")
  )
}

#' Recode estrategias data
#' @param df Raw estrategias dataframe
#' @return Processed dataframe with recoded variables
recode_estrategias <- function(df) {
  df %>%
    mutate(
      Tratamiento.Farm = case_when(
        tratamiento %in% c("Flx", "Flx-CUMS") ~ "Fluoxetina",
        tratamiento %in% c("Sal-CUMS-F", "Salina-Control") ~ "Salina"
      ),
      Estres = case_when(
        tratamiento %in% c("Flx-CUMS", "Sal-CUMS-F") ~ "CUMS",
        tratamiento %in% c("Flx", "Salina-Control") ~ "Control"
      ),
      estrategias_hipo = forcats::fct_recode(
        as.factor(estrategias_hipo),
        "Alocéntricas" = "hipocampo_dependientes",
        "Egocéntricas" = "hipocampo_independientes",
        "Perseverancia" = "perseverancia"
      ),
      Tratamiento.Farm = factor(
        Tratamiento.Farm,
        levels = c("Salina", "Fluoxetina")
      )
    )
}

#' Process main water maze data
#' @param df_wm Raw water maze dataframe
#' @return Processed dataframe with all transformations
process_wm_data <- function(df_wm) {
  # Apply transformations
  df_processed <- df_wm %>%
    mutate(
      Tratamiento.Farm = case_when(
        tratamientos %in% c("Flx", "Flx-CUMS") ~ "Fluoxetina",
        tratamientos %in% c("Sal-CUMS-F", "Salina-Control") ~ "Salina"
      ),
      Estres = case_when(
        tratamientos %in% c("Flx-CUMS", "Sal-CUMS-F") ~ "CUMS",
        tratamientos %in% c("Flx", "Salina-Control") ~ "Control"
      ),
      zona_blanco = case_when(
        prueba %in% c("P-1", "P-2") ~ zona_ne,
        prueba == "P-Rev" ~ zona_so
      ),
      zona_reversa = case_when(
        prueba %in% c("P-1", "P-2") ~ zona_so,
        prueba == "P-Rev" ~ zona_ne
      ),
      distancia_media_blanco = case_when(
        prueba %in% c("P-1", "P-2") ~ distancia_annulus_ne,
        prueba == "P-Rev" ~ distancia_annulus_so
      ),
      distancia_media_opuesto = case_when(
        prueba %in% c("P-1", "P-2") ~ distancia_annulus_so,
        prueba == "P-Rev" ~ distancia_annulus_ne
      ),
      Blanco = case_when(
        prueba %in% c("P-1", "P-2") ~ cuadrante_ne,
        prueba == "P-Rev" ~ cuadrante_so
      ),
      Opuestos = (cuadrante_no + cuadrante_se + cuadrante_so) / 3,
      # Convert to factors
      tratamientos = factor(
        tratamientos,
        levels = c("Flx", "Flx-CUMS", "Salina-Control", "Sal-CUMS-F")
      ),
      id = factor(id),
      stage = factor(stage),
      prueba = factor(prueba),
      Tratamiento.Farm = factor(Tratamiento.Farm),
      Estres = factor(Estres)
    )

  # Filter and rename
  df_final <- df_processed %>%
    filter(
      tratamientos %in% c("Flx", "Flx-CUMS", "Sal-CUMS-F", "Salina-Control")
    ) %>%
    rename(tiempo_2 = tiempo, Tratamiento = Tratamiento.Farm, Tiempo = dia) %>%
    droplevels() %>%
    mutate(
      Tratamiento = factor(Tratamiento, levels = c("Salina", "Fluoxetina")),
      Estres = factor(
        Estres,
        levels = c("Control", "CUMS"),
        labels = c("NO-Estrés", "CUMS")
      )
    ) %>%
    # Remove animals that didn't pass criteria
    filter(!id %in% c("6", "9", "f_g3_3", "f_g3_7"))

  return(df_final)
}

# =============================================================================
# STATISTICAL ANALYSIS FUNCTIONS
# =============================================================================

#' Prepare data for analysis
#' @param df Input dataframe
#' @param test_filter Filter pattern for test
#' @param cols_to_pivot Columns to pivot (optional)
#' @param names_to Name for pivoted names column
#' @param values_to Name for pivoted values column
#' @return Filtered and potentially pivoted dataframe
prepare_data <- function(
  df,
  test_filter,
  cols_to_pivot = NULL,
  names_to = "Preferencia",
  values_to = "Porcentaje"
) {
  df_filtered <- df %>%
    filter(str_detect(prueba, test_filter))

  if (str_detect(test_filter, "P-") && !is.null(cols_to_pivot)) {
    df_filtered <- df_filtered %>%
      pivot_longer(
        cols = all_of(cols_to_pivot),
        names_to = names_to,
        values_to = values_to,
        values_drop_na = TRUE
      )
  }

  return(df_filtered)
}

#' Run ANOVA test with proper error handling
#' @param df Input dataframe
#' @param dv Dependent variable
#' @param id_col ID column
#' @param within_col Within-subjects variable
#' @param between_cols Between-subjects variables
#' @param agrupar_variables Whether to group variables
#' @return ANOVA results table
# Fixed run_anova_test function
run_anova_test <- function(
  df,
  dv,
  id_col,
  within_col = NULL,
  between_cols = NULL,
  agrupar_variables = FALSE
) {
  
  # Validate inputs
  stopifnot(is.data.frame(df))
  stopifnot(dv %in% colnames(df))
  stopifnot(id_col %in% colnames(df))
  
  if (agrupar_variables && !is.null(within_col)) {
    df_summarized <- df %>%
      dplyr::group_by(across(c(all_of(between_cols), id_col, within_col))) %>%
      dplyr::summarise(respuesta = mean(!!rlang::sym(dv), na.rm = TRUE), .groups = 'drop')
    
    if (!is.null(within_col) && !is.null(between_cols)) {
      # Mixed design ANOVA
      anova_result <- df_summarized %>%
        rstatix::anova_test(
          dv = respuesta,
          wid = !!sym(id_col),
          within = !!sym(within_col),
          between = all_of(between_cols),
          type = 3
        )
    } else if (!is.null(between_cols)) {
      # Between-subjects ANOVA
      anova_result <- df_summarized %>%
        rstatix::anova_test(
          dv = respuesta,
          wid = !!sym(id_col),
          between = all_of(between_cols),
          type = 3
        )
    }
  } else {
    # Direct analysis without grouping
    if (!is.null(within_col) && !is.null(between_cols)) {
      anova_result <- df %>%
        rstatix::anova_test(
          dv = !!sym(dv),
          wid = !!sym(id_col),
          within = !!sym(within_col),
          between = all_of(between_cols),
          type = 3
        )
    } else if (!is.null(between_cols)) {
      anova_result <- df %>%
        rstatix::anova_test(
          dv = !!sym(dv),
          wid = !!sym(id_col),
          between = all_of(between_cols),
          type = 3
        )
    }
  }
  
  # Get ANOVA table and add significance
  anova_table <- rstatix::get_anova_table(anova_result, correction = "auto")
  
  if ("p" %in% colnames(anova_table)) {
    anova_table <- anova_table %>%
      rstatix::add_significance() %>%
      rstatix::p_format()
  }
  
  return(anova_table)
}

# Fixed format_anova_table function
format_anova_table <- function(anova_results, caption = "ANOVA Results") {
  
  # Ensure we have a proper data frame
  if (!is.data.frame(anova_results)) {
    stop("anova_results must be a data frame")
  }
  
  # Clean and format the table
  anova_formatted <- anova_results %>%
    as_tibble() %>%
    mutate(across(where(is.numeric), ~ round(.x, 3)))
  
  # Create the table
  kable_table <- anova_formatted %>%
    kable(
      digits = 3,
      format = "html",
      escape = FALSE,
      caption = caption
    ) %>%
    kable_styling(
      bootstrap_options = c("striped", "hover", "condensed"),
      full_width = FALSE,
      font_size = 12,
      position = "center"
    ) %>%
    footnote(
      general = c(
        "η² > 0.14 = Efecto grande, η² > 0.06 = Efecto medio, η² < 0.06 = Efecto pequeño",
        "* p < 0.05, ** p < 0.01, *** p < 0.001"
      ),
      general_title = "Notas:",
      escape = FALSE
    )
  
  return(kable_table)
}
# =============================================================================
# PLOTTING FUNCTIONS
# =============================================================================

#' Create base plot theme for Morris Water Maze
#' @return ggplot theme
theme_mwm <- function() {
  theme_classic(base_size = 14) +
    theme(
      legend.position = "top",
      axis.text.x = element_text(angle = 0, hjust = 0.5),
      plot.title = element_text(size = 16, face = "bold"),
      plot.subtitle = element_text(size = 11, color = "gray30"),
      strip.background = element_rect(fill = "white", color = "black"),
      strip.text = element_text(face = "bold"),
      panel.grid.minor = element_blank()
    )
}

#' Create color palette for treatments
#' @return Named vector of colors
get_treatment_colors <- function() {
  c("Salina" = "#efc000", "Fluoxetina" = "#006A8E")
}

#' Create main plot for Morris Water Maze data
#' @param df Prepared dataframe
#' @param y_var Y variable name
#' @param treatment_var Treatment variable name
#' @param facet_var Faceting variable name
#' @param within_col Within-subjects variable
#' @param geom Plot geometry type
#' @param ... Additional parameters
#' @return ggplot object
create_mwm_plot <- function(
  df,
  y_var,
  treatment_var,
  facet_var,
  within_col,
  geom = "col_pattern",
  dodge_width = 0.85,
  x_lab_name,
  y_lab_name,
  hline_val,
  limy,
  use_custom_y_scale = FALSE,
  anova_caption = NULL,
  animate = FALSE
) {
  facet_formula <- as.formula(paste("~", facet_var))

  # Base plot setup
  if (geom == "line") {
    plot <- ggplot(
      df,
      aes(
        x = !!sym(within_col),
        y = !!sym(y_var),
        group = !!sym(treatment_var),
        color = !!sym(treatment_var)
      )
    )
  } else {
    plot <- ggplot(
      df,
      aes(
        x = interaction(Preferencia, !!sym(treatment_var)),
        y = !!sym(y_var),
        fill = !!sym(treatment_var)
      )
    )
  }

  # Add geoms based on plot type
  if (geom == "line") {
    plot <- plot +
      stat_summary(
        fun = "mean",
        geom = "line",
        position = position_dodge(width = 0.1),
        size = 1.2
      ) +
      stat_summary(
        fun = "mean",
        geom = "point",
        position = position_dodge(width = 0.1),
        size = 3.5
      ) +
      stat_summary(
        fun.data = "mean_se",
        geom = "errorbar",
        width = 0.1,
        size = 0.6,
        position = position_dodge(width = 0.1)
      ) +
      geom_jitter(width = .25, height = 0, alpha = .15, size = 1.5) +
      scale_color_manual(
        values = get_treatment_colors(),
        aesthetics = c("colour", "fill")
      )
  } else {
    plot <- plot +
      stat_summary(
        aes(pattern = Preferencia),
        fun = "mean",
        geom = "col_pattern",
        color = "#2c3e50",
        pattern_fill = "#8f5497",
        pattern_alpha = 0.4,
        pattern_density = 0.4,
        pattern_key_scale_factor = 0.8,
        width = 0.8,
        position = position_dodge(width = dodge_width)
      ) +
      stat_summary(
        fun.data = "mean_se",
        geom = "errorbar",
        width = 0.18,
        size = 0.8,
        position = position_dodge(width = dodge_width)
      ) +
      scale_color_manual(
        values = get_treatment_colors(),
        aesthetics = c("colour", "fill")
      )
  }

  # Common plot elements
  plot <- plot +
    theme_mwm() +
    facet_grid(facet_formula) +
    geom_hline(
      yintercept = hline_val,
      linetype = 2,
      color = "#e74c3c",
      linewidth = 0.8
    ) +
    labs(x = x_lab_name, y = y_lab_name, subtitle = anova_caption)

  # Adjust scales and guides
  if (geom != "line") {
    plot <- plot +
      scale_x_discrete(labels = rep(c("Blanco", "Op."), times = 2)) +
      scale_pattern_manual(values = c("none", "crosshatch"), guide = "none") +
      guides(fill = guide_legend(override.aes = list(pattern = "none")))
  }

  # Y-axis customization
  if (use_custom_y_scale) {
    plot <- plot +
      scale_y_continuous(
        labels = function(x) paste0(round(x * 100 / 60, 0), "%"),
        breaks = function(limits) seq(0, 60, by = 12),
        limits = c(0, limy),
        expand = c(0, 0.02),
        guide = "prism_offset_minor"
      )
  } else {
    plot <- plot +
      scale_y_continuous(
        limits = c(0, limy),
        expand = c(0, 0.02),
        breaks = scales::pretty_breaks(n = 5),
        guide = "prism_offset_minor"
      )
  }

  # Add animation if requested
  if (animate && geom == "line") {
    plot <- plot +
      gganimate::transition_reveal(!!sym(within_col)) +
      gganimate::ease_aes('linear') +
      labs(title = paste(y_lab_name, "- Día: {closest_state}"))
  }

  return(plot)
}

#' Customize facet strip colors
#' @param plot ggplot object
#' @param strip_colors Vector of colors for strips
customize_facet_strips <- function(
  plot,
  strip_colors = c("#ecf0f1", "#e74c3c")
) {
  g <- ggplot_gtable(ggplot_build(plot))
  strip_both <- which(grepl('strip-', g$layout$name))

  for (k in seq_along(strip_both)) {
    strip_index <- strip_both[k]
    color_index <- ((k - 1) %% length(strip_colors)) + 1

    if ("grob" %in% class(g$grobs[[strip_index]])) {
      rect_idx <- which(sapply(
        g$grobs[[strip_index]]$grobs[[1]]$children,
        function(x) grepl("rect", x$name)
      ))
      if (length(rect_idx) > 0) {
        g$grobs[[strip_index]]$grobs[[1]]$children[[
          rect_idx
        ]]$gp$fill <- strip_colors[color_index]
      }
    }
  }

  grid::grid.draw(g)
}

# =============================================================================
# INTEGRATED ANALYSIS FUNCTIONS
# =============================================================================

#' Run complete analysis pipeline
#' @param df Input dataframe
#' @param test_filter Test filter pattern
#' @param ... Additional parameters passed to plotting functions
#' @return List with ANOVA results and plot
run_mwm_analysis <- function(
  df,
  test_filter,
  cols_to_pivot = NULL,
  dv,
  id_col,
  use_custom_y_scale = FALSE,
  within_col,
  between_cols,
  treatment_var,
  facet_var,
  geom = "col_pattern",
  dodge_width = 0.85,
  x_lab_name,
  y_lab_name,
  hline_val,
  limy,
  strip_colors = c("#ecf0f1", "#e74c3c"),
  animate = FALSE,
  interactive = FALSE
) {
  # Prepare data
  prepared_data <- prepare_data(df, test_filter, cols_to_pivot)

  # Run ANOVA
  anova_results <- run_anova_test(
    prepared_data,
    dv = dv,
    id_col = id_col,
    within_col = within_col,
    between_cols = between_cols,
    agrupar_variables = TRUE
  )

  # Generate ANOVA caption
  anova_caption <- rstatix::get_test_label(anova_results, detailed = TRUE)

  # Create plot
  plot <- create_mwm_plot(
    df = prepared_data,
    y_var = dv,
    treatment_var = treatment_var,
    facet_var = facet_var,
    within_col = within_col,
    geom = geom,
    dodge_width = dodge_width,
    x_lab_name = x_lab_name,
    y_lab_name = y_lab_name,
    hline_val = hline_val,
    limy = limy,
    use_custom_y_scale = use_custom_y_scale,
    anova_caption = anova_caption,
    animate = animate
  )

  # Make interactive if requested
  if (interactive && !animate) {
    plot <- plotly::ggplotly(plot, tooltip = c("x", "y", "fill", "colour"))
  }

  # Return results
  return(list(
    anova_results = anova_results,
    plot = plot,
    data = prepared_data,
    strip_colors = strip_colors
  ))
}

#' Create distance to target analysis across probe tests
#' @param df Input dataframe
#' @param tests Vector of test names
#' @return List of plots and data for each test
create_distance_analyses <- function(
  df, 
  tests = c("P-1", "P-2", "P-Rev")
) {
  
  # Filter data for probe tests only
  df_filtered <- df %>% filter(str_detect(prueba, "P-"))
  
  # Create mixed effects model
  mi_formulalmer <- lme4::lmer(
    distancia_media_blanco ~ Tratamiento * Estres * prueba + (1 | id), 
    data = df_filtered
  )
  
  # Create the interaction plot
  distance_plot <- cat_plot(
    mi_formulalmer, 
    pred = prueba, 
    modx = Tratamiento, 
    mod2 = Estres, 
    geom = "line", 
    point.shape = TRUE, 
    errorbar.width = 0.4,
    x.label = "Prueba", 
    y.label = "Distancia Media (m)",
    main.title = "Distancia Media al Annulus",
    colors = c("#bba800", "#006A8E"), 
    interval = TRUE
  ) + 
    jtools::theme_apa(legend.pos = "top") +
    ggplot2::scale_y_continuous(limits = c(0, 0.8)) +
    annotate(
      "text", label = "**",
      x = 3, y = 0.5, size = 7, colour = "black"
    ) +
    theme(
      plot.title = element_text(size = 16, face = "bold"),
      axis.title = element_text(size = 12),
      legend.text = element_text(size = 10)
    )
  
  # Create individual plots for each test
  individual_plots <- map(tests, ~ {
    test_data <- df_filtered %>% filter(prueba == .x)
    
    test_title <- case_when(
      .x == "P-1" ~ "Prueba 1: Adquisición",
      .x == "P-2" ~ "Prueba 2: Retención", 
      .x == "P-Rev" ~ "Prueba 3: Reversa"
    )
    
    ggplot(test_data, aes(Estres, distancia_media_blanco, 
                         color = Tratamiento, group = Tratamiento)) +
      stat_summary(fun = "mean", geom = "line", size = 1.2, 
                   position = position_dodge(0.1)) +
      stat_summary(fun = "mean", geom = "point", size = 3.5,
                   position = position_dodge(0.1)) +
      stat_summary(fun.data = "mean_se", geom = "errorbar", 
                   width = 0.1, size = 0.8, position = position_dodge(0.1)) +
      scale_color_manual(values = c("#bba800", "#006A8E")) +
      scale_y_continuous(limits = c(0, 0.8)) +
      labs(
        title = test_title,
        x = "Estrés",
        y = "Distancia Media (m)"
      ) +
      theme_classic(base_size = 14) +
      theme(
        legend.position = "top",
        plot.title = element_text(size = 16, face = "bold")
      )
  })
  
  names(individual_plots) <- tests
  
  return(list(
    combined_plot = distance_plot,
    individual_plots = individual_plots,
    model = mi_formulalmer
  ))
}

#' Create latencies analysis for reversal training
#' @param df Input dataframe
#' @return List with analysis results
create_latencies_reversa_analysis <- function(df) {
  
  # Filter reversal training data
  df_reversa <- df %>% filter(str_detect(prueba, "Reversa"))
  
  # Create the main plot
  latencies_plot <- ggplot(
    df_reversa,
    aes(x = Tiempo, y = latencia, color = Tratamiento, group = Tratamiento)
  ) +
    stat_summary(fun = "mean", geom = "line", size = 1.2, 
                 position = position_dodge(0.1)) +
    stat_summary(fun = "mean", geom = "point", size = 3.5,
                 position = position_dodge(0.1)) +
    stat_summary(fun.data = "mean_se", geom = "errorbar", 
                 width = 0.1, size = 0.8, position = position_dodge(0.1)) +
    geom_jitter(width = 0.25, height = 0, alpha = 0.15, size = 1.5) +
    facet_grid(~Estres) +
    scale_color_manual(values = c("#bba800", "#006A8E")) +
    scale_x_continuous(n.breaks = 2, labels = c("1", "2")) +
    scale_y_continuous(limits = c(0, 60)) +
    labs(
      title = "Latencias de Escape - Entrenamiento Reversa",
      x = "Día",
      y = "Latencia (seg)"
    ) +
    theme_classic(base_size = 14) +
    theme(
      legend.position = "top",
      plot.title = element_text(size = 16, face = "bold")
    )
  
  # Create individual day plots
  individual_plots <- map(unique(df_reversa$Tiempo), ~ {
    day_data <- df_reversa %>% filter(Tiempo == .x)
    
    ggplot(day_data, aes(Estres, latencia, color = Tratamiento, group = Tratamiento)) +
      stat_summary(fun = "mean", geom = "line", size = 1.2, 
                   position = position_dodge(0.1)) +
      stat_summary(fun = "mean", geom = "point", size = 3.5,
                   position = position_dodge(0.1)) +
      stat_summary(fun.data = "mean_se", geom = "errorbar", 
                   width = 0.1, size = 0.8, position = position_dodge(0.1)) +
      scale_color_manual(values = c("#bba800", "#006A8E")) +
      scale_y_continuous(limits = c(0, 60)) +
      labs(
        title = paste("Día", .x),
        x = "Estrés", 
        y = "Latencia (seg)"
      ) +
      theme_classic(base_size = 14) +
      theme(
        legend.position = "top",
        plot.title = element_text(size = 16, face = "bold")
      )
  })
  
  names(individual_plots) <- paste0("Day_", unique(df_reversa$Tiempo))
  
  return(list(
    combined_plot = latencies_plot,
    individual_plots = individual_plots
  ))
}

# =============================================================================
# SPECIALIZED PLOTTING FUNCTIONS
# =============================================================================

#' Create estrategias plot
#' @param data Estrategias dataframe
#' @param animate Whether to animate the plot
#' @return ggplot object
create_estrategias_plot <- function(data, animate = FALSE) {
  p <- data %>%
    mutate(
      estrategias_hipo = fct_relevel(
        estrategias_hipo,
        "Egocéntricas",
        "Alocéntricas"
      )
    ) %>%
    ggplot(aes(fill = estrategias_hipo, y = rate, x = dia)) +
    geom_bar(
      position = "fill",
      stat = "identity",
      color = "black",
      size = 0.3
    ) +
    facet_grid(Estres ~ Tratamiento.Farm) +
    scale_fill_manual(values = alpha(c("#e74c3c", "#3498db"), 0.8)) +
    scale_y_continuous(labels = c("0", "1", "2", "3", "4")) +
    theme_mwm() +
    guides(fill = guide_legend(title = "")) +
    labs(x = "Día", y = "Número Estrategias", title = "Estrategias de búsqueda")

  if (animate) {
    p <- p +
      gganimate::transition_time(dia) +
      gganimate::ease_aes('linear') +
      labs(title = "Estrategias de búsqueda - Día: {frame_time}")
  }

  return(p)
}

# =============================================================================
# TABLE FORMATTING FUNCTIONS
# =============================================================================

#' Format ANOVA table for presentation
#' @param anova_results ANOVA results dataframe
#' @param caption Table caption
#' @return Formatted kable
format_anova_table <- function(anova_results, caption = "ANOVA Results") {
  
  # Ensure we have a proper data frame
  if (!is.data.frame(anova_results)) {
    stop("anova_results must be a data frame")
  }
  
  # Clean and format the table
  anova_formatted <- anova_results %>%
    as_tibble() %>%
    mutate(across(where(is.numeric), ~ round(.x, 3)))
  
  # Create the table
  kable_table <- anova_formatted %>%
    kable(
      digits = 3,
      format = "html",
      escape = FALSE,
      caption = caption
    ) %>%
    kable_styling(
      bootstrap_options = c("striped", "hover", "condensed"),
      full_width = FALSE,
      font_size = 12,
      position = "center"
    ) %>%
    footnote(
      general = c(
        "η² > 0.14 = Efecto grande, η² > 0.06 = Efecto medio, η² < 0.06 = Efecto pequeño",
        "* p < 0.05, ** p < 0.01, *** p < 0.001"
      ),
      general_title = "Notas:",
      escape = FALSE
    )
  
  return(kable_table)
}
# =============================================================================
# REVEAL.JS SPECIFIC FUNCTIONS
# =============================================================================

#' Create progressive revelation plots for Reveal.js
#' @param df Input dataframe
#' @param tests Vector of test names (e.g., c("P-1", "P-2", "P-Rev"))
#' @param ... Additional parameters
#' @return List of plots for progressive revelation
create_progressive_plots <- function(
  df,
  tests = c("P-1", "P-2", "P-Rev"),
  cols_to_pivot,
  dv,
  id_col,
  within_col,
  between_cols,
  treatment_var,
  facet_var,
  ...
) {
  plots <- list()

  for (i in seq_along(tests)) {
    test_name <- tests[i]

    # Create analysis for current test
    analysis <- run_mwm_analysis(
      df = df,
      test_filter = test_name,
      cols_to_pivot = cols_to_pivot,
      dv = dv,
      id_col = id_col,
      within_col = within_col,
      between_cols = between_cols,
      treatment_var = treatment_var,
      facet_var = facet_var,
      x_lab_name = "Zona",
      y_lab_name = "Tiempo de Ocupación",
      hline_val = 6.6,
      limy = 30,
      use_custom_y_scale = TRUE,
      ...
    )

    # Customize plot title
    plot_title <- case_when(
      test_name == "P-1" ~ "Prueba 1: Adquisición",
      test_name == "P-2" ~ "Prueba 2: Retención",
      test_name == "P-Rev" ~ "Prueba 3: Reversa"
    )

    analysis$plot <- analysis$plot +
      labs(title = plot_title) +
      theme(
        plot.title = element_text(size = 18, face = "bold", color = "#2c3e50")
      )

    plots[[test_name]] <- analysis
  }

  return(plots)
}

#' Create animated comparison across tests
#' @param df Input dataframe
#' @param tests Vector of test names
#' @param ... Additional parameters
#' @return Animated ggplot object
create_animated_comparison <- function(
  df,
  tests = c("P-1", "P-2", "P-Rev"),
  cols_to_pivot,
  dv,
  ...
) {
  # Prepare data for all tests
  all_data <- map_dfr(
    tests,
    ~ {
      prepare_data(df, .x, cols_to_pivot) %>%
        mutate(
          Test = .x,
          Test_Label = case_when(
            .x == "P-1" ~ "Prueba 1",
            .x == "P-2" ~ "Prueba 2",
            .x == "P-Rev" ~ "Prueba Reversa"
          )
        )
    }
  )

  # Create animated plot
  p <- ggplot(
    all_data,
    aes(
      x = interaction(Preferencia, Tratamiento),
      y = !!sym(dv),
      fill = Tratamiento
    )
  ) +
    stat_summary(
      fun = "mean",
      geom = "col",
      position = position_dodge(0.8),
      width = 0.7
    ) +
    stat_summary(
      fun.data = "mean_se",
      geom = "errorbar",
      position = position_dodge(0.8),
      width = 0.2
    ) +
    facet_grid(~Estres) +
    scale_fill_manual(values = get_treatment_colors()) +
    scale_x_discrete(labels = rep(c("Blanco", "Op."), times = 2)) +
    scale_y_continuous(
      labels = function(x) paste0(round(x * 100 / 60, 0), "%"),
      limits = c(0, 30),
      expand = c(0, 0.02)
    ) +
    geom_hline(
      yintercept = 6.6,
      linetype = 2,
      color = "#e74c3c",
      linewidth = 0.8
    ) +
    theme_mwm() +
    labs(
      x = "Zona",
      y = "Tiempo de Ocupación",
      title = "Comparación entre Pruebas: {closest_state}"
    ) +
    gganimate::transition_states(
      Test_Label,
      transition_length = 2,
      state_length = 3
    ) +
    gganimate::ease_aes('cubic-in-out')

  return(p)
}
