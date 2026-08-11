
data_organize <- function(
  data = data
)
{
  # Select relevant columns
  
  # SOE
  #quality.df <- dplyr::select(data, c(1, 3, 4, 5, 8, 7, 19:21, 10:12))

  # GSL
  quality.df <- dplyr::select(data, c(1, 2, 3, 4, 6, 5, 17, 18, 19, 8, 9, 10))
  

  # Change column names for easier scripting
  colnames(quality.df) <- c(
    "Site",
    "Date",
    "Year",
    "Month",
    "Sample control number",
    "Matrix",
    "Param_Group",
    "Parameter",
    "Unit",
    "Value_Text",
    "Value",
    "Detect_Limit")
  
  quality.df <- quality.df |>
    dplyr::mutate(
      Month = trimws(Month)
    )
  
  # Clean Site column for standardization:
  quality.df <- dplyr::mutate(quality.df, 
                              Site = stringr::str_to_lower(stringr::str_trim(Site)))
  
  # Convert to proper date class
  quality.df$Date <- as.Date(quality.df$Date, format = "%d-%b-%y")
  
  # Add river/lake name to each site (use WQ_dependencies.R)
  quality.df <- add_water_body(data = quality.df)
  
  quality.df <- add_WSC_ID(data = quality.df)
}


# Add river/lake name to each site
# Sorted alphabetically

add_water_body <- function(
  data = quality.df
)
{
data <- dplyr::mutate(data, water_body = dplyr::case_when(
  Site == "arctic red river 4 kms above the mouth" ~ "Arctic Red River",
  Site == "baker creek at footbridge" ~ "Baker Creek",
  Site == "bosworth creek at canol drive bridge" ~ "Bosworth Creek",
  Site == "boundary creek at highway 3 bridge" ~ "Great Slave Lake",
  Site == "buffalo river at highway 5 bridge" ~ "Buffalo River",
  Site == "cameron river" ~ "Cameron River",
  Site == "cameron river above the bridge" ~ "Cameron River",
  Site == "clinton-colden outlet"  ~ "Lockhart River",
  Site == "daring lake" ~ "Yamba River (Daring Lake)",
  Site == "desteffany lake" ~ "Coppermine River",
  Site == "east channel  25 kms below inuvik"  ~ "Mackenzie River (Delta)",
  Site == "great bear river at the mouth" ~ "Great Bear River",
  Site == "great slave lake at fort resolution" ~ "Great Slave Lake",
  Site == "great slave lake yellowknife bay at dettah" ~ "Great Slave Lake",
  Site == "great slave lake yellowknife bay at n'dilo" ~ "Great Slave Lake",
  Site == "hay river west channel below the bridge"  ~ "Hay River", 
  Site == "hay river west channel above bridge"  ~ "Hay River",
  Site == "hay river at nt / ab border"  ~ "Hay River",
  Site == "hay river at paradise gardens"  ~ "Hay River",
  Site == "hay river at the mouth" ~ "Hay River",
  Site == "hr-ks1-3a" ~ "Hay River",
  Site == "hr-ks2-3a" ~ "Hay River",
  Site == "hr-ks3-3a" ~ "Hay River",
  Site == "hr-ks4-3a" ~ "Hay River",
  Site == "hr-ks5-3b" ~ "Hay River",
  Site == "hr-ks6-3b" ~ "Hay River",
  Site == "island river 1 km above the mouth of trout lake" ~ "Island River",
  Site == "island river 500 m above the mouth" ~ "Island River",
  Site == "jean marie river  170 m below check point" ~ "Jean Marie River",
  Site == "kakisa river at highway 1 bridge" ~ "Kakisa River",                  #not in excel sheet
  Site == "kakisa river below kakisa lake" ~ "Kakisa River",
  Site == "king lake" ~ "King Lake",
  Site == "lac de gras outlet" ~ "Lac de Gras",
  Site == "lake of the enemy" ~ "Lake of the Enemy",
  Site == "liard river above fort simpson ferry" ~ "Liard River",
  Site == "liard river above kotaneelee river" ~ "Liard River",
  Site == "little buffalo river 1 kms above the mouth" ~ "Little Buffalo River",
  Site == "little buffalo river above highway 6 bridge" ~ "Little Buffalo River",
  Site == "little buffalo river above highway 5 bridge" ~ "Little Buffalo River",
  Site == "little buffalo river at highway 5 bridge" ~ "Little Buffalo River",
  Site == "little buffalo river at highway 6 bridge" ~ "Little Buffalo River",
  Site == "lockhart river at outlet of artillery lake" ~ "Lockhart River",
  Site == "mackay lake" ~ "Mackay Lake",
  Site == "mackenzie river above fort providence bridge" ~ "Mackenzie River",         
  Site == "mackenzie river above norman wells" ~ "Mackenzie River",                   
  Site == "mackenzie river above tsiigehtchic" ~ "Mackenzie River",                   
  Site == "mackenzie river at the mouth of the liard river" ~ "Mackenzie River",      
  Site == "mackenzie river below fort providence boat launch" ~ "Mackenzie River",    
  Site == "mackenzie river below norman wells" ~ "Mackenzie River",                   
  Site == "mackenzie river below tulita" ~ "Mackenzie River", 
  Site == "marian river above franks channel bridge" ~ "Marian River",
  Site == "marian river at franks channel" ~ "Marian River",
  Site == "miller creek" ~ "Great Slave Lake",
  Site == "peel channel 15 kms above aklavik" ~ "Mackenzie River (Delta)",
  Site == "peel river above fort mcpherson" ~ "Peel River",
  Site == "point lake" ~ "Point Lake",
  Site == "rabbit skin river at the mouth" ~ "Rabbitskin River",
  Site == "rocknest lake" ~ "Rocknest Lake",
  Site == "salt river at highway 5 bridge" ~ "Salt River",
  Site == "slave river above the mouth" ~ "Slave River",
  Site == "slave river 95 kms above the mouth" ~ "Slave River",
  Site == "slave river at big eddy" ~ "Slave River",
  Site == "slave river at the mouth" ~ "Slave River",
  Site == "slave river below rapids of the drowned - mid river" ~ "Slave River",
  Site == "slave river below the rapids of the drowned at the boat launch" ~ "Slave River",
  Site == "sr-ks1-3b" ~ "Slave River",
  Site == "sr-ks2-3a" ~ "Slave River",
  Site == "sr-ks3-3b" ~ "Slave River",
  Site == "sr-ks4-3a" ~ "Slave River",
  Site == "sr-ks4-3b" ~ "Slave River",
  Site == "sr-ks5-3a" ~ "Slave River",
  Site == "sr-ks6-3b" ~ "Slave River",
  Site == "stagg river" ~ "Great Slave Lake",
  Site == "talston river at the mouth" ~ "Taltson River",
  Site == "taltson river below nonacho lake dam" ~ "Taltson River",
  Site == "tazin river at the border"  ~ "Tazin River",
  Site == "trout lake at sambaa k'e"  ~ "Trout Lake",
  Site == "trout lake at the southwest bay"  ~ "Trout Lake",
  Site == "vital narrows" ~ "Baker Creek",
  Site == "yellowknife river 3 kms above the bridge" ~ "Yellowknife River",
  Site == "yellowknife river at bridge" ~ "Yellowknife River"
))

}

# Add Water Survey of Canada station ID

add_WSC_ID <- function(
    data = quality.df
)
{

data <- dplyr::mutate(data, Station_ID = dplyr::case_when(
  #Sites without Station IDS can either have NA or not be part of the list
  Site == "arctic red river 4 kms above the mouth" ~ "10LA002",
  Site == "baker creek at footbridge" ~ "07SB013",
  Site == "bosworth creek at canol drive bridge" ~ "10KA007",
  Site == "boundary creek at highway 3 bridge" ~ NA,
  Site == "buffalo river at highway 5 bridge" ~ "07PA001",
  Site == "cameron river" ~ "07SB010",
  Site == "cameron river above the bridge" ~ "07SB010",
  Site == "clinton-colden outlet"  ~ "07RD001",
  Site == "daring lake" ~ "10PA002",
  Site == "desteffany lake" ~ "10PA001",
  Site == "east channel  25 kms below inuvik"  ~ "10LC002",
  Site == "great bear river at the mouth" ~ "10JC003",
  Site == "great slave lake at fort resolution" ~ "07OB002",
  Site == "great slave lake yellowknife bay at dettah" ~ "07SB001",
  Site == "great slave lake yellowknife bay at n'dilo" ~ "07SB001",
  Site == "hay river west channel above bridge"  ~ "07OB001",
  Site == "hay river west channel below the bridge"  ~ "07OB001",
  Site == "hay river at the mouth" ~ "07OB001",
  Site == "hay river at nt / ab border"  ~ "07OB008",
  Site == "hay river at paradise gardens" ~ "07OB001",
  Site == "hr-ks1-3a" ~ "07OB001",
  Site == "hr-ks2-3a" ~ "07OB001",
  Site == "hr-ks3-3a" ~ "07OB001",
  Site == "hr-ks4-3a" ~ "07OB001",
  Site == "hr-ks5-3b" ~ "07OB001",
  Site == "hr-ks6-3b" ~ "07OB001",
  Site == "island river 1 km above the mouth of trout lake" ~ NA,
  Site == "island river 500 m above the mouth" ~ NA,
  Site == "jean marie river  170 m below check point" ~ "10FB005",
  Site == "kakisa river at highway 1 bridge" ~ "07UC001",
  Site == "kakisa river below kakisa lake" ~ "07UC001",
  Site == "king lake" ~ "07RB001",
  Site == "lac de gras outlet" ~ "10PA001",
  Site == "lake of the enemy" ~ "07RB001",
  Site == "liard river above fort simpson ferry" ~ "10ED001",
  Site == "liard river above kotaneelee river" ~ "10ED001",         
  Site == "little buffalo river 1 kms above the mouth" ~ NA,
  Site == "little buffalo river above highway 6 bridge" ~ NA,
  Site == "little buffalo river above highway 5 bridge" ~ NA,
  Site == "little buffalo river at highway 5 bridge" ~ NA,
  Site == "little buffalo river at highway 6 bridge" ~ NA,                      # All little buffalo river changed from "07PB002"
  Site == "lockhart river at outlet of artillery lake" ~ "07RD001",           
  Site == "mackay lake" ~ "07RB001",
  Site == "mackenzie river above fort providence bridge" ~ NA,
  Site == "mackenzie river at the mouth of the liard river" ~ "10GC001",
  Site == "mackenzie river below fort providence boat launch" ~ NA,       # This and above fort prov bridge changed from "10FB001"
  Site == "mackenzie river below norman wells" ~ "10KA001",
  Site == "mackenzie river below tulita" ~ "10KA001",
  Site == "marian river above franks channel bridge" ~ "07SA009",
  Site == "marian river at franks channel" ~ "07SA009",
  Site == "peel channel 15 kms above aklavik" ~ "10MC003",
  Site == "mackenzie river above norman wells" ~ "10KA001",
  Site == "mackenzie river above tsiigehtchic" ~ "10KA001",
  Site == "miller creek" ~ NA,
  Site == "peel river above fort mcpherson" ~ "10MC002",
  Site == "point lake" ~ NA,                                             # "10PB003"
  Site == "rabbit skin river at the mouth" ~ NA,
  Site == "rocknest lake" ~ "10PC004",
  Site == "salt river at highway 5 bridge" ~ NA,
  Site == "slave river above the mouth" ~ "07NB001",
  Site == "slave river 95 kms above the mouth" ~ "07NB001",
  Site == "slave river at the mouth" ~ "07NB001",
  Site == "slave river at big eddy" ~ "07NB001",
  Site == "slave river below the rapids of the drowned at the boat launch" ~ "07NB001",
  Site == "slave river below rapids of the drowned - mid river" ~ "07NB001",
  Site == "sr-ks1-3b" ~ "07SB001",
  Site == "sr-ks2-3a" ~ "07SB001",
  Site == "sr-ks3-3b" ~ "07SB001",
  Site == "sr-ks4-3a" ~ "07SB001",
  Site == "sr-ks4-3b" ~ "07SB001",
  Site == "sr-ks5-3a" ~ "07SB001",
  Site == "sr-ks6-3b" ~ "07SB001",
  Site == "stagg river" ~ NA,
  Site == "talston river at the mouth" ~ "07QD007",
  Site == "taltson river below nonacho lake dam" ~ "07QD007",
  Site == "tazin river at the border"  ~ "07QC007",
  Site == "trout lake at sambaa k'e"  ~ NA,
  Site == "trout lake at the southwest bay"  ~ NA,
  Site == "vital narrows" ~ "07SB013",
  Site == "yellowknife river 3 kms above the bridge" ~ "07SB002",  #Check this one
  Site == "yellowknife river at bridge" ~ "07SB002",
  TRUE ~ NA_character_
))

}

# Building the dataframe

build.df <- function(
    parameter,
    data,
    Station_ID
) 
{
  data.frame1 <- data.frame()
  
  for(i in Station_ID) {
    
    data.ind <- hydro_calc_daily(
      station_number = i,
      parameter = parameter,
      start_date = "1930-01-01",
      end_date = Sys.Date(),
      realtime_dl = TRUE
    )
    
    
    # Edit column names
    colnames(data.ind)[c(1,4)] <- c(
      "Station_ID", 
      parameter
    )
    
    data.ind <- data.ind %>%
      dplyr::mutate(DayofYear = lubridate::yday(Date))
    
    # Select columns
    data.ind <- dplyr::select(
      data.ind, 
      c("Station_ID", "Date", parameter)
    )
    
    
    # Add individual station (data.ind) to larger level dataframe (data.frame1)
    data.frame1 <- dplyr::bind_rows(
      data.frame1, 
      data.ind
    )
  }
  
  if(any(!is.na(data$Station_ID))) {
    # Calculate percentiles
    data.pctl <- data.frame1 %>%
      dplyr::group_by(Station_ID) %>%
      dplyr::mutate(
        Pctl = dplyr::percent_rank(.data[[parameter]]) * 100
      ) %>%
      dplyr::ungroup() %>%
      dplyr::select(Station_ID, Date, Pctl)
    
    # Combine water quality and flow/level data
    data <- dplyr::left_join(data,
                             data.frame1,
                             by = c(
                               "Station_ID" = "Station_ID",
                               "Date" = "Date")
    )
    
    data <- dplyr::left_join(data,
                             data.pctl,
                             by = c("Station_ID" = "Station_ID",
                                    "Date" = "Date")
    )
  } else {
    data$Pctl <- NA
  }
  
  return(data)
  
}

# Making the standard plot
make_plot <- function(
    plot_ind,
    facet_by,
    type,
    x_var,
    y_var,
    title,
    y_lab,
    colour_with,
    include_flow_or_level,
    legend_fill
) 
{ 
  # Y-axis spacing
  y_vals <- plot_ind[[y_var]]
  y_vals <- y_vals[is.finite(y_vals)]
  
  if (length(y_vals) > 1 && diff(range(y_vals)) > 0) {
    y_range <- diff(range(y_vals))
    raw_y_width <- y_range / 5
    y_major_width <- 10^floor(log10(raw_y_width))
    y_minor_width <- y_major_width / 5
  } else {
    y_minor_width <- 1
  }
  
  # Check whether x-axis is a Date
  is_date_x <- inherits(plot_ind[[x_var]], "Date")
  
  
  p <- ggplot2::ggplot(
      data = plot_ind,
      ggplot2::aes(
        x = .data[[x_var]],
        y = .data[[y_var]],
        colour = .data[[colour_with]]
      )
    ) +
    ggplot2::geom_point() +
    ggplot2::scale_y_continuous(
      breaks = scales::breaks_extended(n = 5),
      minor_breaks = NULL
    ) +
  ggplot2::labs(
    title = title,
    x = x_var,
    y = y_lab
  )
    # +
    # ggplot2::geom_hline(
    #   aes(yintercept = Detect_Limit),
    #   colour = "grey45",
    #   linetype = "dotted",
    #   linewidth = 0.35,
    #   alpha = 0.5
    # )
 
  # X-axis scale
  if (is_date_x) {
    p <- p +
      ggplot2::scale_x_date(
        date_breaks = "5 years",
        date_labels = "%Y",
        date_minor_breaks = "1 year"
      )
  } else {
    p <- p +
      ggplot2::scale_x_continuous(
        breaks = scales::breaks_pretty(n = 5),
        minor_breaks = NULL
      )
  }
  
  # Apply the theme last
  p <- p +
    ggplot2::theme_classic() +
    ggplot2::theme(
      legend.position = legend_fill,
      strip.text = ggtext::element_markdown(),
      strip.background = ggplot2::element_blank(),
      
      plot.background = ggplot2::element_rect(
        fill = "white",
        colour = NA
      ),
      panel.background = ggplot2::element_rect(
        fill = "white",
        colour = NA
      ),
      
      panel.grid.major = ggplot2::element_line(
        colour = "grey80",
        linewidth = 0.4
      ),
      panel.grid.minor = ggplot2::element_line(
        colour = "grey92",
        linewidth = 0.25
      ),
      plot.title = ggplot2::element_text(hjust = 0.5)
    )
  
  # Add-ons to appearance
  if (!is.null(facet_by)) {
    p <- p +
      ggplot2::facet_wrap(
        stats::as.formula(paste("~", facet_by)),
        scales = "fixed",
        axes = "all"
      )
  }
  
  if (include_flow_or_level == T) {
    p <- p +
      ggplot2::scale_colour_gradient2(
        name = paste0(type, "\npercentile \n(grey = no data)"),
        low = "#d73027",
        mid = "grey80",
        high = "#4575b4",
        midpoint = 50,
        limits = c(0,100),
        na.value = "grey30",
        breaks = seq(0, 100, 25),
        labels = function(x) paste0(x, "%")
      )
  }
  
  return(p)
}

# Making the C-Q plot
# If b < 0: as water flow increases, the concentration of the parameter decreases
# If b = 0: chemostatis - concentration stays constant no matter how much water flows
# If b > 0: as flow increases, concentration also increases
make_CQ_plot <- function(
    plot_ind,
    facet_by
) 
{
  # Prepare to fit a function to the plot
  plot_ind_fit <- plot_ind |>
    dplyr::filter(
      !is.na(Flow),
      !is.na(Value),
      is.finite(Flow),
      is.finite(Value),
      Flow > 0,
      Value > 0
    )
  
  cq_fit <- lm(
    log10(Value) ~ log10(Flow),
    data = plot_ind_fit
  )
  
  a <- 10^(coef(cq_fit)[1])
  b <- coef(cq_fit)[2]
  
  line_df <- data.frame(
    Flow = seq(
      min(plot_ind_fit$Flow),
      max(plot_ind_fit$Flow),
      length.out = 100
    )
  )
  
  line_df$Value_pred <- a * line_df$Flow^b
  
  # Plot it
  p <- ggplot2::ggplot(
    data = plot_ind_fit,
    ggplot2::aes(
      x = Flow,
      y = Value
    )
  ) +
    ggplot2::geom_point() +
    ggplot2::geom_line(
      data = line_df,
      ggplot2::aes(
        x = Flow,
        y = Value_pred
      )
    ) +
    ggplot2::scale_x_log10() +
    ggplot2::scale_y_log10() +
    ggplot2::theme_classic() +
    ggplot2::labs(
      title = paste0(plot_ind_fit$Parameter[1], " in ", plot_ind_fit[[facet_by]][1]),
      x = "log(Discharge (m³/s))",
      y = paste0("Concentration (", plot_ind_fit$Unit[1], ")")
    ) +
    ggplot2::theme(
      strip.background = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(hjust = 0.5)
    ) +
    ggplot2::annotate(
      "text",
      x = Inf,
      y = Inf,
      label = paste0("b = ", round(b, 2)),
      hjust = 1.1,
      vjust = 1.5
    )
  
  return(p)
  
}

# Making statistical analysis graphics
stat_graphic <- function(
    df_use,
    sites_included,
    site_loc_included,
    y_axis_order,
    x_text_size,
    y_text_size,
    n_cols,
    n_rows,
    colour_scale,
    gradient,
    p_value,
    x_name,
    y_name,
    value,
    units,
    legend_name,
    title_name,
    subtitle_name,
    filename,
    save_path
    )
{
  # ── 6. Thesis colour palette ──────────────────────────────────
  colour <- c(
    "#1B3A5C",
    "#3A6EA8",
    "#A8C5D8",
    "#D4E5EF",
    "#F0EDE8",  
    "#F0EDE8",  
    "#F2D3A4",
    "#D4894A",
    "#B85C2A",
    "#8B2E12"
  )
  
  # Define the text colour so it will be visible
  get_text_col <- function(value) {
    value <- as.numeric(value)
    ifelse(abs(value) > 0.4, "#F7F3EC", "#2C1A0E")
  }
  
  tile  <- 1
  site_text <- paste0(site_loc_included, collapse = "\n")
  
  df_use[[y_name]] <- factor(
    df_use[[y_name]],
    levels = rev(y_axis_order)
  )
  
   # ── 7. Plot ───────────────────────────────────────────────────
  p <- ggplot(
    df_use,
    ggplot2::aes(
      x = .data[[x_name]],
      y = .data[[y_name]],
      fill = .data[[gradient]]
    )) +
    geom_tile(colour = "white", linewidth = 1, height = 1, width = 1,) +
    # Add a black border around significant ones
    # 2 layers of significance: p<= 0.5, p <= 0.01 ???
    geom_tile(
      data = dplyr::filter(df_use, p_value < 0.05),
      height = 1,
      width = 1,
      fill = NA,
      colour = "black",
      linewidth = 1
    ) +
    # geom_tile(
    #   data = dplyr::filter(df_use, p_value < 0.01),
    #   height = 1,
    #   width = 1,
    #   fill = NA,
    #   colour = "black",
    #   linewidth = 2.25
    # ) +
    geom_text(
      aes(
        label = stringr::str_wrap(sprintf("%.4f", .data[[value]]), width = 8),
        colour = I(get_text_col(.data[[gradient]]))
      ),
      size = 4,
      vjust = 0.5,
      lineheight = 0.85,
      fontface = "plain"
    ) +
    scale_fill_gradientn(
      colours = colour,
      values  = scales::rescale(colour_scale),
      limits  = c(-1, 1),
      name    = legend_name,
      guide   = guide_colorbar(
        barwidth       = 0.8,
        barheight      = 10,
        ticks          = TRUE,
        title.position = "top",
        title.hjust = 0.5
      )
    ) +
    scale_x_discrete(position = "top", labels = scales::label_wrap(max(8, 15 - n_cols))) +
    scale_y_discrete(labels = scales::label_wrap(20)) +
    coord_cartesian(clip = "off") +
    labs(
      title = title_name,
      subtitle = paste0(subtitle_name, "\n\nSites Included:\n", site_text),
      x     = NULL,
      y     = NULL
    ) +
    theme_minimal(base_size = 11) +
        theme(
          plot.background = element_rect(fill = "white", colour = NA), 
          panel.background = element_rect(fill = "white", colour = NA), 
          panel.grid = element_blank(),
  
      axis.text.x = element_text(
        size = x_text_size, colour = "#2C3E50", face = "plain", vjust = 0.5
      ),
      axis.text.y = element_text(
        size = y_text_size, colour = "#2C3E50", hjust = 1
      ),
      axis.ticks = element_blank(),
      
      plot.title  = element_text(
        size = 16, 
        face = "bold",
        colour = "#1E4D2B", 
        margin = margin(b = 10),
        hjust = 0.5
      ),
      
      plot.subtitle = element_text(
        size = 10,
        face = "plain",
        colour = "#1E4D2B",
        margin = margin(b = 12),
        hjust = 0.5
      ),
      
      plot.margin = margin(30, 16, 40, 30),
      
      legend.position  = "right",
      legend.title     = element_text(size = 11, colour = "#2C3E50"),
      legend.text      = element_text(size = 10, colour = "#2C3E50"),
      legend.key.width = unit(0.9, "cm")
    )
  
  # Add latitude sorting for sites
  if(sites_included == T) {
    p <- p +
      labs(
        y = "South ──────────────────► North"
      ) +
      theme(
        axis.title.y = element_text(
          angle = 90,
          face = "bold",
          colour = "#2C3E50",
          size = y_text_size + 1,
          margin = margin(r = 20, b = 25)
      )
    )
  }
  
  # Add if statement for if spec conductivity is included....
  
  ggplot2::ggsave(
    filename = filename,
    plot = p,
    width = max(10, n_cols * tile),
    height = max(4.5, n_rows * tile + 2 + length(unique(df_use[[y_name]]))),
    units = "in",
    dpi = 300,
    path = ifelse(
      exists("save_path"),
      save_path,
      getwd()),
  )
  
  print(p)
}


