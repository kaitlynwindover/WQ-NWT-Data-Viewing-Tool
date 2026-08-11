#' WQ_Plot
#'
#' Plots NWT water quality metrics over time
#' @return ggplot objects of water quality plots
#' @export

# Function to plot water quality data
# Author: Ryan Connon
# Email:  Ryan_Connon@gov.nt.ca

# Data access:
# Received data from Robin Staples (Robin.Staples@gov.nt.ca) as a pull from Lodestar
# From: 2000
# To:   2025

#Load library for trend analysis
library(trend)


# First function: builds the data frame
Water_Quality_df <- function(
  directory,            
  file_name,              
  file_extension,
  all_data,
  parameters,  
  sites,                     
  water_body_order, 
  start_date,
  end_date,   
  include_flow_or_level,          
  remove_outliers       
  
) 
{
  # Read .csv into R, identifying proper path
  raw.df <- read.csv(
    paste0(
      directory,
      file_name,
      file_extension
      )
    )
  
  # Load the coordinates in
  coors <- read.csv(
    paste0(
      directory,
      all_data,
      file_extension
    )
  )
  
  # Use WQ_dependencies.R to organize data
  data <- data_organize(data = raw.df)
  
  # Subset parameters as per arguments
  data <- dplyr::filter(
    data,
    .data$Parameter %in% .env$parameters
  )
  
  # Error messaging for user
  # Parameters that were found in filtering
  found_parameters <- unique(data$Parameter)
  
  # Parameters that were requested but not found
  missing_parameters <- setdiff(parameters, found_parameters)
  
  if (length(missing_parameters) > 0) {
    cat("The following requested parameters were not found at all in the data:\n")
    print(missing_parameters)
  }
  
  # Subset sites as per arguments
  data <- dplyr::filter(
    data,
    Site %in% sites
  )
  
  # Error messaging for user
  for (i in seq_along(found_parameters)) {
    
    found_sites <- unique(
      data$Site[data$Parameter == found_parameters[i]]
    )
    
    missing_sites <- setdiff(sites, found_sites)
    
    if (length(missing_sites) > 0) {
      cat("\nParameter:", found_parameters[i], "\n")
      cat("The following requested sites were not found:\n")
      print(missing_sites)
    }
  }
  
  before_date_filter <- data |>
    dplyr::distinct(Site, Parameter)
  
  # Subset dates as per arguments
  data <- dplyr::filter(
    data,
    Date > as.Date(start_date),
    Date < as.Date(end_date)
  )
  
  after_date_filter <- data |>
    dplyr::distinct(Site, Parameter)
  
  if (nrow(before_date_filter) != nrow(after_date_filter)) {
    
    # Find the Site + Parameter combinations that disappeared
    removed_combos <- dplyr::anti_join(
      before_date_filter,
      after_date_filter,
      by = c("Site", "Parameter")
    )
    
    cat("\nThe following Site + Parameter combinations were removed by the date filter:\n")
    
    for (i in seq_len(nrow(removed_combos))) {
      cat(
        "  Site:", removed_combos$Site[i],
        "| Parameter:", removed_combos$Parameter[i],
        "\n"
      )
    }
  }
  

  if(remove_outliers == T) {
    #remove the reported outliers - specefic samples
    data <- dplyr::filter(
      data,
      !(`Sample control number` %in% c(
        "CBM-2013-00019-002",
        "CBM-2013-00019-003",
        "CBM-2013-00019-004",
        "CBM-2013-00019-005",
        "CBM-2013-00019-006",
        "CBM-2013-00033-002",
        "CBM-2013-00033-003",
        "CBM-2013-00033-004",
        "CBM-2013-00033-005",
        "CBM-2013-00033-006",
        "CBM-2013-00033-007",
        "CBM-2013-00033-008",
        
        # PROPOSED OUTLIERS
        "LRM-2009-00001-003",
        "CBM-2021-00042-003",
        "SRSW13108",
        "LRM-2007-00002-004",
        "LRM-2008-00002-008"
      ))
    )
  }
  
  if(include_flow_or_level == F) {
    #data$Pctl <- NA
    data$Pctl <- rep(NA_real_, nrow(data))
  } 
  
  else if(include_flow_or_level == T) {
    
    # Create vector of all Station_IDs (and remove NAs)
    Station_ID <- unique(data$Station_ID[!is.na(data$Station_ID)])
    
    # Seperate flow data (rivers) and level data (lakes)
    lakes <- c(
      "07SB001",
      "07OB002",
      "07SA009"
    )
    
    rivers <- c(
      "10LA002",
      "07SB013",
      "10KA007",
      "07PA001",
      "07SB010",
      "07RD001",
      "10PA002",
      "10LC002",
      "10JC003",
      "07OB001",
      "07OB008",
      "10FB005",
      "07UC001",
      "07RB001",
      "10PA001",
      "10ED001",
      "07PB002",
      "07RD001",
      "10FB001",
      "10GC001",
      "10KA001",
      "10MC003",
      "10MC002",
      "10PB003",
      "10PC004",
      "07NB001",
      "07QD007",
      "07QC007",
      "07SB002"
    )
    
    # Download data using hydro_calc_daily() from hydroclim package
    # Use flow data for river and water level data for lake
    # Determined by the station ID for the site
    if(
      any(data$Station_ID %in% lakes) &&
      any(data$Station_ID %in% rivers)
    ) {
      print("Cannot compare river and lake data.")
      return(NULL)
    }
    
    else if(any(data$Station_ID %in% lakes)) {
      
      data <- build.df(
        parameter = "Level",
        data = data,
        Station_ID = Station_ID
      )
      
    } 
    
    else if (any(data$Station_ID %in% rivers)) {
      
      data <- build.df(
        parameter = "Flow",
        data = data,
        Station_ID = Station_ID
      )
      
    } 
    
    else {
      data$Pctl <- NA
    }
    
  } 
  
#----------------------------Case specific adjustments------------------------------------------------ 
  # Edit DOC parameter name (if using)
  if("Dissolved Organic Carbon - space at end" %in% parameters) {
    data$Parameter[data$Parameter == "Dissolved Organic Carbon - space at end"] <- "Dissolved Organic Carbon"
  }
  
  # Edit Hardness parameter name (if using)
  if(any(c("Hardness (Inactive)", "Hardness, as CaCO3") %in% parameters)) {
    
    data$Parameter[
      data$Parameter %in% c(
        "Hardness (Inactive)",
        "Hardness, as CaCO3"
      )
    ] <- "Hardness"
    
  }
  
  data <- data |>
    dplyr::mutate(Value = as.numeric(Value))
  
  data <- dplyr::mutate(
    data,
    
    #Changing the magnitude of the values before switching the units
    Value = dplyr::case_when(
      Parameter == "Strontium" & Unit != "ng/L" ~ Value * 1000,
      TRUE ~ Value
    ),
    
    Unit = dplyr::case_when(
      Parameter == "Strontium" ~ "ng/L",
      TRUE ~ Unit
    )
  )
  
  # Remove data points for mercury that are units of ug/L
  data <- data |>
    dplyr::filter(
      !(
        Parameter == "Mercury" &
          Unit != "ng/L"
      )
    )
  
  # Use pH, field measurements when available and pH, physical parameters when field not available
  data <- data |>
    dplyr::group_by(Site, Date, Parameter) |>
    dplyr::filter(
      !(Parameter == "pH" &
          Param_Group == "Physical Parameters" &
          any(Param_Group == "Field Measurements"))
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      Param_Group = dplyr::if_else(
        Parameter == "pH",
        "All Measurements",
        Param_Group
     )
    )
  
  # Same for Turbidity
  data <- data |>
    dplyr::group_by(Site, Date, Parameter) |>
    dplyr::filter(
      !(Parameter == "Turbidity" &
          Param_Group == "Physical Parameters" &
          any(Param_Group == "Field Measurements"))
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      Param_Group = dplyr::if_else(
        Parameter == "Turbidity",
        "All Measurements",
        Param_Group
      )
    )
  
  # Filter to remove any lake samples (Great Slave Lake) from Hay River
  ## Note: Could also do this for other sites where needed
  data <- dplyr::filter(
    data,
    !(water_body == "Hay River" & Matrix != "River Water")
  )
  
  # Filter to remove erroneous values
  ## Note:: Could improve by having this line at start and removing specific lines from raw.df
  ## But this is good placeholder for now
  data <- dplyr::filter(
    data, 
    !(Parameter == "Total Nitrogen" & Value > 20)
  )
  
  # Change the name of some parameters
  data <- data |>
    dplyr::mutate(
      Parameter = dplyr::case_when(
        Parameter == "Total Hardness, as CaCO3 (calc'd)" ~ "Total Hardness",
        Parameter == "Total Alkalinity, as CaCO3" ~ "Total Alkalinity",
        TRUE ~ Parameter
      )
    )
  
  # Calculate percentage at the detection limit for each Site + Parameter
  # Rows with a missing Detect_Limit are excluded from the calculation.
  # If every Detect_Limit is missing, pct_dl is set to NA.
  data <- data |>
    dplyr::group_by(Site, Parameter) |>
    dplyr::mutate(
      pct_dl = dplyr::if_else(
        sum(!is.na(Detect_Limit) & !is.na(Value)) == 0,
        NA_real_,
        mean(
          Value[!is.na(Detect_Limit) & !is.na(Value)] ==
            Detect_Limit[!is.na(Detect_Limit) & !is.na(Value)]
        ) * 100
      )
    ) |>
    dplyr::ungroup()
  
  
  # Flag Site + Parameter combinations with at least 50%
  # of valid observations at the detection limit
  removed <- data |>
    dplyr::distinct(Site, Parameter, pct_dl) |>
    dplyr::filter(
      !is.na(pct_dl),
      pct_dl >= 50
    ) |>
    dplyr::transmute(
      Site,
      Parameter,
      detection_removed = TRUE
    )
  
  
  # Add the removal flag and replace values at the detection limit
  # with half the detection limit only when:
  # 1. Detect_Limit is not missing
  # 2. Value equals Detect_Limit
  # 3. The Site + Parameter combination was not removed
  data <- data |>
    dplyr::left_join(
      removed,
      by = c("Site", "Parameter")
    ) |>
    dplyr::mutate(
      detection_removed = dplyr::coalesce(
        detection_removed,
        FALSE
      ),
      
      Value = dplyr::if_else(
        !is.na(Detect_Limit) &
          !is.na(Value) &
          Value == Detect_Limit &
          !detection_removed,
        Detect_Limit * 0.5,
        Value
      )
    )
#------------------------------------------------------------------------------------
  
  # Add the parameter group to the parameter name
  data <- data |>
    dplyr::mutate(
      Parameter = paste(
        Parameter,
        Param_Group,
        sep = ", "
      )
    )
  
  # Turn coordinates csv into one row per site to read
  coors_unique <- coors |>
    dplyr::select(Site, Lat) |>
    dplyr::group_by(Site) |>
    dplyr::summarise(
      Lat = dplyr::first(Lat),
      .groups = "drop"
    )
  
  # Sort the sites by latitude for ggplot
  data <- data |>
    dplyr::left_join(
      coors_unique |>
        dplyr::select(Site, Lat),
      by = c("Site" = "Site")
    ) |>
    dplyr::arrange(Lat) |>
    dplyr::mutate(
      Site = factor(Site, levels = unique(Site))
    ) |>
    dplyr::select(-Lat)
  
  # Create water_body_order if not previously defined
  # Note: Likely will not need to use water_body_colours
  if(is.null(water_body_order)) {
    water_body_order <- sort(unique(data$water_body))
  }
  
  # Assign factor order to water_body names
  data$water_body <- factor(data$water_body, levels = water_body_order)
  
  # Automate parameters for second function
  if(length(unique(data$water_body)) > 1) {
    group_by_water_body_result <- T
    water_body_result <- NA
  } else {
    group_by_water_body_result <- F
    water_body_result <- unique(data$water_body)[1]
  }
  
  # Return parameters
  results <- list(
    data = data,
    group_by_water_body_result = group_by_water_body_result,
    water_body_result = water_body_result
  )
  
  return(results)
  
}


# Second function: plots the dataframe
water_Quality_plots <- function(
    # Arguments for generating figure
    data,
    plot_title,           
    n_row,                
    n_col,                
    legend_position, 
    plot_width,          
    plot_height,       
    dpi,                 
    save_path, 
    plot_name,     
    group_by_water_body,   
    auto_save,            
    water_body,
    include_flow_or_level
  )
{
  
  if ("Level" %in% names(data)) {
    type <- "Level"
  } 
  else if ("Flow" %in% names(data)) {
    type <- "Flow"
  } 
  else {
    type <- NA
  }
    
    # Run loop to produce individual plots and return as list as plots
    plots <- list()
    
    for(i in seq_along(unique(data$Parameter))) {
      
      # This loop shuffles through each parameter
      plot_ind <- dplyr::filter(
        data,
        Parameter == unique(data$Parameter)[i]
      )
      
      if(group_by_water_body == T) {
        
        n_made <- plot_ind |>
          dplyr::filter(
            !is.na(water_body),
            !is.na(Value),
            is.finite(Value)
          ) |>
          dplyr::summarise(
            n = dplyr::n_distinct(water_body)
          ) |>
          dplyr::pull(n)
        
        #only facet_wrap if grouping by water body
        plots[[i]] <- make_plot(
          plot_ind = plot_ind,
          facet_by = "water_body",
          type = type,
          x_var = "Date",
          y_var = "Value",
          title = plot_ind$Parameter[1],
          y_lab = paste0(plot_ind$Parameter[1], " (", plot_ind$Unit[1], ")"),
          colour_with = "Pctl",
          include_flow_or_level = include_flow_or_level,
          legend_fill = "right"
        )
      
      if(!is.na(n_row) || !is.na(n_col)) {
        plots[[i]] <- plots[[i]] +
          ggplot2::facet_wrap(
            ~ water_body,
            nrow = n_row, 
            ncol = n_col,
            scales = "fixed",
            axes = "all"
          )
        }
      }
      
      if(group_by_water_body == F) {
        
        site_data_counts <- plot_ind |>
          dplyr::group_by(Site) |>
          dplyr::summarise(
            n_plot_values = sum(
              !is.na(Date) &
                !is.na(Value) &
                is.finite(Value)
            ),
            .groups = "drop"
          )
        
        n_made <- site_data_counts |>
          dplyr::filter(
            !is.na(Site),
            n_plot_values > 0
          ) |>
          nrow()
        
        #Put the site names back in capitals
        plot_ind$Site <- stringr::str_to_title(plot_ind$Site)
        
        plots[[i]] <- make_plot(
          plot_ind = plot_ind,
          facet_by = "Site",
          type = type,
          x_var = "Date",
          y_var = "Value",
          title = plot_ind$Parameter[1],
          y_lab = paste0(plot_ind$Parameter[1], " (", plot_ind$Unit[1], ")"),
          colour_with = "Pctl",
          include_flow_or_level = include_flow_or_level,
          legend_fill = "right"
        )
      }
      
      if(auto_save == T) {
        #Make the plots different sizes depending on the orientation for aesthetics
        if(n_made == 1 | n_made == 4 | n_made == 7 | n_made == 8 | n_made == 9) {
          plot_width <- 18
          plot_height <- 18
        } else if(n_made == 2) {
          plot_width <- 30
          plot_height <- 15
        } else if(n_made == 3) {
          plot_width <- 40
          plot_height <- 15
        } else if(n_made == 5 | n_made == 6) {
          plot_width <- 35
          plot_height <- 20
        } else {
          plot_width <- 30
          plot_height <- 25
        } 
       
        ggplot2::ggsave(
          paste0(
            plot_name,
            "_",
            plot_ind$Parameter[1],
            ".png"
          ),
          plot = plots[[i]],
          device = "png",
          path = ifelse(
            exists("save_path"),
            save_path,
            getwd()),
          scale = 1,
          width = plot_width,
          height = plot_height,
          units = c("cm"),
          dpi = dpi
          )
      }
    }
   
    return(plots)
    
}

# Produces C-Q plots with log scale
concentration_flow_relation <- function(
    data,
    plot_title,           
    n_row,                
    n_col,                
    legend_position, 
    plot_width,          
    plot_height,       
    dpi,                 
    save_path, 
    plot_name,     
    group_by_water_body,   
    auto_save,            
    water_body  
)
{
  
  if ("Flow" %in% names(data)) {
    
    plots <- list()
    plot_counter <- 1
    
    parameters_plot <- unique(data$Parameter)
    
    # Loop through parameters
    for (i in seq_along(parameters_plot)) {
      
      param_i <- parameters_plot[i]
      
      # Data for current parameter
      param_data <- data |>
        dplyr::filter(
          Parameter == param_i
        )
      
      # Skip parameter if it has no flow data
      if (all(is.na(param_data$Flow))) {
        next
      }
      
      # Get water bodies for this parameter
      water_bodies <- unique(
        stats::na.omit(param_data$water_body)
      )
      
      # Loop through water bodies
      for (j in seq_along(water_bodies)) {
        
        water_body_j <- water_bodies[j]
        
        # Data for current parameter + water body
        plot_ind <- param_data |>
          dplyr::filter(
            water_body == water_body_j,
            !is.na(Flow),
            !is.na(Value)
          )
        
        if (nrow(plot_ind) == 0) {
          next
        }
        
        # Number of sites actually being plotted
        n_made <- dplyr::n_distinct(plot_ind$Site)
        
        # Make plot
        plots[[plot_counter]] <- make_CQ_plot(
          plot_ind = plot_ind,
          facet_by = "Site"
        ) +
          ggplot2::labs(
            title = paste(
              param_i,
              "-",
              water_body_j
            )
          )
        
        # Optional facet layout
        if (!is.na(n_row) || !is.na(n_col)) {
          
          plots[[plot_counter]] <- plots[[plot_counter]] +
            ggplot2::facet_wrap(
              ~ Site,
              nrow = n_row,
              ncol = n_col,
              scales = "fixed",
              axes = "all"
            )
        }
        
        
        # Autosave
        if (auto_save == T) {
          
          ggplot2::ggsave(
            filename = paste0(
              plot_name,
              "_",
              param_i,
              "_",
              water_body_j,
              "_2.png"
            ),
            plot = plots[[plot_counter]],
            device = "png",
            path = if (exists("save_path")) {
              save_path
            } else {
              getwd()
            },
            scale = 1,
            width = 18,
            height = 18,
            units = "cm",
            dpi = dpi
          )
        }
        
        plot_counter <- plot_counter + 1
      }
    }
    
    return(plots)
  }
  
  else {
    print("Cannot produce C-Q plot with water level data or flow data was not requested.")
  }
}


# Performs the Mann Kendall test
water_Quality_trends <- function(
    data,
    month_param,
    directory,
    all_data,
    file_extension,
    save_path
    ) 
{
  coors <- read.csv(
    paste0(
      directory,
      all_data,
      file_extension
    )
  )

  mk_results <- list()
  counter <- 1
  
  # Get rid of entries with too much values at detection limit for analysis
  # Print what is being removed
  if (any(data$detection_removed, na.rm = TRUE)) {
    cat("\nRemoving the following Site + Parameter combinations:\n")
    
    data |>
      dplyr::filter(detection_removed) |>
      dplyr::distinct(Site, Parameter, pct_dl) |>
      apply(1, function(x) {
        cat(sprintf(
          "  %s - %s (%.1f%% at detection limit)\n",
          x["Site"],
          x["Parameter"],
          as.numeric(x["pct_dl"])
        ))
      })
    cat("\n")
  }
  
  # Remove only combinations with >= 50% at the detection limit.
  # Keep parameters with no detection limit information (pct_dl = NA).
  data <- data |>
    dplyr::filter(
      is.na(pct_dl) | pct_dl < 50
    ) |>
    dplyr::select(-pct_dl)
  
  # Take the average of each year for the month inputted
  data <- data |>
    dplyr::group_by(Site, Parameter, Year) |>
    dplyr::summarise(
      dplyr::across(
        -Value,
        dplyr::first
      ),
      Value = mean(Value, na.rm = TRUE),
      .groups = "drop"
    )

  # Get results for each site/parameter combo
  parameters <- unique(na.omit(data$Parameter))
  sites <- unique(na.omit(data$Site))
  
  for(i in seq_along(parameters)) {
    
    param_i <- parameters[i]
    unit_i <- unique(data$Unit[data$Parameter == param_i])[1]
    
    for(j in seq_along(sites)) {
      
      site_j <- sites[j]
      
      plot_ind <- data |>
        dplyr::filter(
          Parameter == param_i,
          Site == site_j
        )
      
      data_test <- plot_ind |>
        dplyr::filter(
          !is.na(Value),
          !is.na(Date)
        )
      
      n_site_param <- nrow(data_test)

      if(n_site_param == 0) {
        next
      }

      # Use 3 as the minimum data points for statistical significance
      if(n_site_param < 4) {
        next
      }
      
      data_test <- data_test |>
        dplyr::arrange(Date) |>
        dplyr::mutate(
          Year = lubridate::decimal_date(Date)
        )
      
      stat_test <- trend::mk.test(data_test$Value)
      
      sen_test <- zyp::zyp.sen(
        Value ~ Year,
        data = data_test
      )
      
      # Get the slope in units/year
      mk_results[[counter]] <- data.frame(
        Site = site_j,
        Parameter = param_i,
        n = n_site_param,
        tau = unname(stat_test$estimates[["tau"]]),
        p_value = stat_test$p.value,
        sen_slope = unname(sen_test$coefficients[["Year"]]),
        sen_slope_units = paste0(unit_i, "/year")
      )
      
      counter <- counter + 1
    }
  }
  
  
  mk_results <- dplyr::bind_rows(mk_results)
  
  # Build grid heat map with dataframe
  site_order <- unique(mk_results$Site)
  
  mk_results <- dplyr::mutate(
    mk_results,
    value = sprintf("%.4f", sen_slope),
    Parameter = factor(
      Parameter,
      levels = unique(Parameter)
    )
  )
  
  # Turn coordinates csv into one row per site to read
  coors_unique <- coors |>
    dplyr::select(Site, Lat) |>
    dplyr::group_by(Site) |>
    dplyr::summarise(
      Lat = dplyr::first(Lat),
      .groups = "drop"
    )
  
  # Sort the sites by latitude for ggplot
  mk_results <- mk_results |>
    dplyr::left_join(
      coors_unique |>
        dplyr::select(Site, Lat),
      by = c("Site" = "Site")
    ) |>
    dplyr::arrange(Lat) |>
    dplyr::mutate(
      Site = factor(Site, levels = unique(Site))
    ) |>
    dplyr::select(-Lat)
  
  
  # Automate size of text based on how big the grid is
  n_rows <- length(unique(mk_results$Site))
  n_cols <- length(unique(mk_results$Parameter))
  
  x_text_size <- max(7.5, min(10, 90 / n_cols))
  y_text_size <- max(7.5, min(10, 90 / n_rows))
  
  colour_scale <- c(-1, -0.7, -0.5, -0.3, -0.1,
                    0.1, 0.3, 0.5, 0.7, 1)
  
  site_loc_included = unique(mk_results$Site)
  y_axis_order <- unique(mk_results$Site)
  
#__________________________________________ Case specific adjustments__________
  unit_label_specific <- character()
  
  if ("Turbidity, All Measurements" %in% mk_results$Parameter) {
    unit_label_specific <- c(
      unit_label_specific,
      "*Turbidity in units NTU/year*"
    )
  }
  
  if ("Specific Conductivity, Physical Ions" %in% mk_results$Parameter) {
    unit_label_specific <- c(
      unit_label_specific,
      "*Specific Conductivity in units uS/cm/year*"
    )
  }
  
  if ("pH, All Measurements" %in% mk_results$Parameter) {
    unit_label_specific <- c(
      unit_label_specific,
      "*pH is unitless*"
    )
  }
  
  if ("Strontium, Dissolved Metals" %in% mk_results$Parameter || "Strontium, Total Metals" %in% mk_results$Parameter) {
    unit_label_specific <- c(
      unit_label_specific,
      "*Strontium in units ng/L/year*"
    )
  }
  
  if ("Mercury, Dissolved Metals" %in% mk_results$Parameter || "Mercury, Total Metals" %in% mk_results$Parameter) {
    unit_label_specific <- c(
      unit_label_specific,
      "*Mercury in units ng/L/year*"
    )
  }
  
  print("Mann_Kendall information:")
  print("*Skips when n < 4*")
  print(mk_results)
  cat("\n")
  
  unit_label_specific <- paste(unit_label_specific, collapse = "\n")
#_______________________________________________________________________________
  
  mk_result <- stat_graphic(
    df_use = mk_results,
    sites_included = T,
    site_loc_included = site_loc_included,
    y_axis_order = y_axis_order,
    x_text_size = x_text_size,
    y_text_size = y_text_size,
    n_cols = n_cols,
    n_rows = n_rows,
    colour_scale = colour_scale,
    gradient = "tau",
    p_value = p_value,
    x_name = "Parameter",
    y_name = "Site",
    value = "sen_slope",
    units = "sen_slope_units",
    legend_name = "Kendall's τ (Trend Consistency)",
    title_name = paste0("Water Quality - Mann-Kendall Test for ", month_param),
    subtitle_name = paste0("Cell Values: Sen's Slope (mg/L/year)\n",  unit_label_specific ,"\n\nBlack Bold: p < 0.05"),
    filename = "Mann_Kendall_Heatmap.png",
    save_path = save_path
  )
  
  return(mk_result)
      
}


# Performs pearson correlation for given data
pearson_correlation <- function(
    layers,
    data_WQ,
    month_param,
    directory,
    file_name,
    file_extension,
    save_path,
    auto_save,
    include_flow_or_level = include_flow_or_level
    )
{
  # For data from Adler
  # Read .csv into R, identifying proper path
  data_ext <- read.csv(
    paste0(
      directory,
      file_name,
      file_extension
    )
  )

  # Organize new data
  data_ext <- data_ext |>
    dplyr::select(
      3, 11, 12, 14,
      dplyr::all_of(layers)
    )
  
  #Build two dataframes for analysis
  vars_select <- unique(data_WQ$Parameter)
  
  site_loc_included = unique(data_WQ$Site)
  
  # Average all the data points in a year
  data_WQ <- data_WQ |>
    dplyr::group_by(Site, Parameter, Year) |>
    dplyr::summarise(
      dplyr::across(
        -Value,
        dplyr::first
      ),
      Value = mean(Value, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Water quality parameters
  df_WQ <- data_WQ |>
    dplyr::select(Site, Date, Year, Parameter, Value) |>
    tidyr::pivot_wider(
      id_cols = c(Site, Date, Year),
      names_from = Parameter,
      values_from = Value,
      values_fn = dplyr::first
    )
  
  # Fix date format before the join
  data_ext$Date <- as.Date(data_ext$Date)
  
  # Join dataframes by Site and Date
  df_all <- df_WQ |>
    dplyr::inner_join(data_ext, by = c("Site", "Date"))
  
  #____________________ Print graphs of each correlation ______________
  n_made <- length(vars_select) * length(layers)
  
  # Run loop to produce individual plots and return as list as plots
  plots <- list()
  counter <- 1
  
  for (j in layers) {
    for (i in vars_select) {
      
      plot_ind <- df_all |>
        dplyr::select(
          Site,
          Year,
          dplyr::all_of(i),
          dplyr::all_of(j)
        ) |>
        dplyr::filter(
          !is.na(.data[[i]]),
          !is.na(.data[[j]])
        ) |>
        dplyr::mutate(
          NoColour = NA_real_
        )
      
      plots[[counter]] <- make_plot(
        plot_ind = plot_ind,
        facet_by = NULL,
        type = NULL,
        x_var = j,
        y_var = i,
        title = paste(i, "vs", j),
        y_lab = i,
        colour_with = "NoColour",
        include_flow_or_level = include_flow_or_level,
        legend_fill = "none"
      ) +
        ggplot2::labs(
          title = paste(i, "vs", j)
        )
      
      counter <- counter + 1
    }
  }
  
  if(auto_save == T) {
    #Make the plots different sizes depending on the orientation for aesthetics
    if(n_made == 1 | n_made == 4 | n_made == 7 | n_made == 8 | n_made == 9) {
      plot_width <- 18
      plot_height <- 18
    } else if(n_made == 2) {
      plot_width <- 30
      plot_height <- 15
    } else if(n_made == 3) {
      plot_width <- 40
      plot_height <- 15
    } else if(n_made == 5 | n_made == 6) {
      plot_width <- 35
      plot_height <- 20
    } else {
      plot_width <- 18
      plot_height <- 18
    } 
    
    combined_plot <- patchwork::wrap_plots(
      plots,
      ncol = length(vars_select),
      byrow = TRUE
    )
    
    if (isTRUE(auto_save)) {
      
      ggplot2::ggsave(
        filename = "Standard_Plots_Pearson.png",
        plot = combined_plot,
        path = if (exists("save_path")) save_path else getwd(),
        width = 5 * length(vars_select),
        height = 4 * length(layers),
        dpi = 300,
        limitsize = FALSE
      )
    }
  }
  
  print(combined_plot)
  
  #____________________________________________________________________

  # Inform user of no data combinations
  for (i in vars_select) {
    if (all(is.na(df_all[[i]]))) {
      cat(sprintf("No data for parameter: %s\n", i))
    }
  }

  for (j in layers) {
    if (all(is.na(df_all[[j]]))) {
      cat(sprintf("No data for layer: %s\n", j))
    }
  }

  # Size of plot
  n_WQ <- length(vars_select)
  n_ext <- length(layers)

  results <- tidyr::expand_grid(
    WQ_parameter = vars_select,
    external_variable = layers
  ) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      n = sum(stats::complete.cases(df_all[[WQ_parameter]], df_all[[external_variable]])),

      r = {
        x <- df_all[[WQ_parameter]]
        y <- df_all[[external_variable]]
        ok <- stats::complete.cases(x, y)

        if (sum(ok) < 4 ||
            length(unique(x[ok])) < 2 ||
            length(unique(y[ok])) < 2) {
          NA_real_
        } else {
          unname(stats::cor.test(x[ok], y[ok], method = "pearson")$estimate)
        }
      },

      pval = {
        x <- df_all[[WQ_parameter]]
        y <- df_all[[external_variable]]
        ok <- stats::complete.cases(x, y)

        if (sum(ok) < 4 ||
            length(unique(x[ok])) < 2 ||
            length(unique(y[ok])) < 2) {
          NA_real_
        } else {
          stats::cor.test(x[ok], y[ok], method = "pearson")$p.value
        }
      },

      Start_Date = {
        ok <- stats::complete.cases(
          df_all$Date,
          df_all[[WQ_parameter]],
          df_all[[external_variable]]
        )

        if (sum(ok) == 0) {
          as.Date(NA)
        } else {
          min(df_all$Date[ok])
        }
      },

      End_Date = {
        ok <- stats::complete.cases(
          df_all$Date,
          df_all[[WQ_parameter]],
          df_all[[external_variable]]
        )

        if (sum(ok) == 0) {
          as.Date(NA)
        } else {
          max(df_all$Date[ok])
        }
      },
    ) |>
    dplyr::ungroup()

  # ── 5. Build plot data frame ──────────────────────────────────
  print("Pearson correlation information:")
  print("*Skips when n < 4*")
  print(results)

  df_plot <- results |>
    dplyr::mutate(
      r_lab = sprintf("%.2f", r),
      num_label = factor(WQ_parameter, levels = rev(vars_select)),
      cat_label = factor(external_variable, levels = layers)
    )

  y_axis_order <- c(
    "Burn__YTD_km2",
    "Burn__YTD_pct",
    "Burn__1yr_km2",
    "Burn__1yr_pct",
    "Burn__5yr_km2",
    "Burn__5yr_pct",
    "Burn__8yr_km2",
    "Burn__8yr_pct",
    "Burn__10yr_km2",
    "Burn__10yr_pct"
  )

  # Automate size of text based on how big the grid is
  x_text_size <- max(8, min(12, 60 / n_ext))
  y_text_size <- max(8, min(12, 60 / n_WQ))

  colour_scale <- c(-1, -0.7, -0.5, -0.3, -0.1,
                    0.1, 0.3, 0.5, 0.7, 1)

  p_result <- stat_graphic(
    df_use = df_plot,
    sites_included = F,
    site_loc_included = site_loc_included,
    y_axis_order = y_axis_order,
    x_text_size = x_text_size,
    y_text_size = y_text_size,
    n_cols = n_ext,
    n_rows = n_WQ,
    colour_scale = colour_scale,
    gradient = "r",
    p_value = results$pval,
    x_name = "WQ_parameter",
    y_name = "external_variable",
    value = "r",
    units = "r_lab",
    legend_name = "R value",
    title_name = paste0("Water Quality - Pearson Correlation for ", month_param),
    subtitle_name = "Black Bold: p < 0.05",
    filename = "Pearson_Correlation_Heatmap.png",
    save_path = save_path
  )
  
  returns <- list(
    plot = p_result,
    standard_plot = combined_plot,
    df_all = df_all
  )
  
  return(returns)
  
}

