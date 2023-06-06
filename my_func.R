# Helpers
get_colour_palette <- function(dat){
  
  n_depth <- dat %>%
    select(contains("low_tide")) %>%
    distinct() %>%
    nrow()
  
  if(n_depth > 6){
    colour_palette <- viridis(n_depth, option = "D", direction = -1)
  } else{
    colour_palette <- viridis(6, option = "D", direction = -1)
  }
  
  colour_palette
  
}

ss_create_variable_labels <- function(dat_long) {
  
  var_order <- c(
    "Temperature \n(\u00B0C)",
    "Dissolved Oxygen \n(% sat)",
    "Uncorrected \nDissolved Oxygen \n(mg / L)",
    "Dissolved Oxygen \n(mg / L)",
    "Salinity \n(PSU)",
    "Sensor Depth \n(m)"
  )
  
  dat_long %>%
    mutate(
      variable_label = case_when(
        variable == "dissolved_oxygen_percent_saturation" ~
          "Dissolved Oxygen \n(% sat)",
        variable == "dissolved_oxygen_uncorrected_mg_per_l" ~
          "Uncorrected \nDissolved Oxygen \n(mg / L)",
        variable == "dissolved_oxygen_mg_per_l" ~
          "Dissolved Oxygen \n(mg / L)",
        variable == "salinity_psu" ~ "Salinity \n(PSU)",
        variable == "sensor_depth_measured_m" ~ "Sensor Depth \n(m)",
        variable == "temperature_degree_c" ~ "Temperature \n(\u00B0C)",
        TRUE ~ variable
      ),
      variable_label = factor(
        variable_label, levels = var_order, ordered = TRUE
      )
    )
}

filter_dat_to_plot <- function(
    dat,
    filter_to = c("start", "end", "custom"),
    period = "2 days",
    custom_start = NULL,
    custom_end = NULL
) {
  
  
  assert_that(filter_to %in% c("start", "end", "custom"))
  
  dat <- dat %>% rename(timestamp_ = contains("timestamp_"))
  
  if(filter_to == "start") {
    
    dat <- dat %>%
      filter(
        timestamp_ <=
          (na.omit(min(dat$timestamp_)) %m+% lubridate::period(period))
      )
  }
  
  if(filter_to == "end") {
    
    dat <- dat %>%
      filter(
        timestamp_ >=
          (na.omit(max(dat$timestamp_)) %m-% lubridate::period(period))
      )
  }
  
  if(filter_to == "custom") {
    
    assert_that(is.POSIXct(custom_start))
    assert_that(is.POSIXct(custom_end))
    
    dat <- dat %>%
      filter(
        timestamp_ >= custom_start & timestamp_ <= custom_end
      )
  }
  
  dat
  
}





ss_ggplot_variables_jb <- function(
    dat,
    measured_depth = TRUE,
    superchill = NULL,
    color_palette = NULL,
    legend_name = "Depth (m)",
    legend_position = "right"
) {
  
  theme_set(theme_light())
  
  if(is.null(color_palette)){
    color_palette <- get_colour_palette(dat)
  }
  scale_depth_colour <- scale_colour_manual(
    name = legend_name, values = color_palette, drop = FALSE
  )
  
  if (isFALSE(measured_depth)) {
    dat <- dat %>% select(-contains("sensor_depth_measured"))
  }
  
  if (is.null(superchill) && "temperature_degree_c" %in% colnames(dat)) {
    
    if(min(na.omit(dat$temperature_degree_c)) <= -0.7) {
      superchill <- TRUE
    } else superchill <- FALSE
    
  }
  
  #  format data -------------------------------------------------------------
  
  dat <- dat %>%
     select(
       Date = contains("timestamp_"),
       contains("dissolved_oxygen"),
       contains("temperature"),
       contains("salinity"),
       contains("depth"),
       contains("sensor")
     ) %>%
     ss_pivot_longer() %>% #maybe check if long or wide first
     ss_create_variable_labels() %>%
     ss_convert_depth_to_ordered_factor()
  
  
  # plot --------------------------------------------------------------------
  
  p <- ggplot(
    dat,
    aes(
      Date, value, colour = sensor_depth_at_low_tide_m,
      text = paste(
        "date: ", Date, "\n",
        "value: ", value, "\n",
        "depth: ", sensor_depth_at_low_tide_m, "\n",
        "sensor_type: ", sensor_type, "\n",
        "sensor_serial_number: ", sensor_serial_number
      )
    )
  ) +
    geom_point(size = 0.25) +
    scale_y_continuous(name = "") +
    scale_x_discrete(name = "") +
    scale_depth_colour +
    facet_wrap(~ variable_label, scales = "free_y", ncol = 1, strip.position = "left") +
    theme(
      strip.placement = "outside",
      strip.background = element_blank(),
      strip.text = element_text(colour = "black", size = 10),
      legend.position = legend_position
    ) +
    guides(color = guide_legend(override.aes = list(size = 4)))
  
  
  # add superchill shading --------------------------------------------------
  
  if (isTRUE(superchill)) {

    facet_panel <- data.frame(variable = "temperature_degree_c") %>%
      ss_create_variable_labels()

    p <- p +
      geom_rect(
        data = facet_panel,
        aes(
          xmin = as_datetime(-Inf),
          xmax = as_datetime(Inf),
          ymin = -Inf, ymax = -0.7
        ),
        alpha = 0.3, fill = "#A6CEE3", inherit.aes = FALSE
      )
  }

  p
  
}
