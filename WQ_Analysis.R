# Water Quality code for NWT

# Other codes to bring into R enviornment: WQ_plot, WQ_dependencies, hydro_calc_daily, hydro_compile_daily, hydroclim_load, hydro_filter
# Data as a dataframe (unit debugged)

# All parameters are listed here
# See WQ_dependencies for all site options
# User input to 'include flow' and to 'remove outliers' can be done in 'Water_Quality_df' function call

water_quality_analysis <- function(
  
  # Arguments for file path
  directory = "C:/Users/23vrx1/OneDrive - Queen's University/SWEP 2026/WQ Project/WQ_Project_NWT/Raw data/",  # The location of the input file. 
  
  #file_name = "SOE Output for RC and NW Mar 20 2026",                            # The name of the input file.
  file_name = "GSL Tributaries Data for Queens May 2026 Nutrients",           # e.g. "Water_Quality_Data"
  
  file_extension = ".csv",                                                                           # The file extension type. Default is ".csv".
  
  file_data_name_ext = "WatershedsWithWQData_AndFireHistory",
  
  all_data_csv = "total_dataset_version9",
  
  # Arguments for subsetting data. For now, this must be the same as in the .csv sheet
  parameters = c(                                                                 # e.g. c("Mercury", "Total Nitrogen", "Strontium")
    #------------------- SOE parameter names (few more in GSL data too)
    # "Chloride",
    #"Dissolved Organic Carbon - space at end",
    # "Dissolved Organic Carbon",
    # "Hardness (Inactive)",
    # "Hardness, as CaCO3",
    # "Sodium",
    # "Sulphate",
    #------------------- T.Metals and D.Metals below
    # "Aluminum",
    # "Antimony",
    # "Arsenic",
    # "Barium",
    # "Beryllium",
    # "Bismuth",
    # "Boron",
    # "Cadmium",
    # "Cerium",
    # "Cesium",
    # "Chromium",
    # "Cobalt",
    # "Copper",
    # "Europium",
    # "Gadolinium",
    # "Gallium",
    # "Germanium",
    # "Hafnium",
    # "Holmium",
    # "Indium",
    # "Iridium",
    # "Iron",
    # "Lanthanum",
    # "Lead",
    # "Lithium",
    # "Lutetium",
    # "Magnesium",
    # "Manganese",
    #"Mercury",                                        #Also SOE
    # "Molybdenum",
    # "Neodymium",
    # "Nickel",
    # "Niobium",
    # "Phosphorus",
    # "Platinum",
    # "Praseodymium",
    # "Rubidium",
    # "Ruthenium",
    # "Samarium",
    # "Scandium",
    # "Selenium",
    # "Silicon",
    # "Silver",
    #"Strontium",                                       #Also SOE
    # "Sulphur",
    # "Tellurium",
    # "Terbium",
    # "Thallium",
    # "Tin",
    # "Titanium",
    # "Tungsten",
    # "Uranium",
    # "Vanadium",
    # "Ytterbium",
    # "Yttrium",
    # "Zinc",
    # "Zirconium"
    #----------------------- Nutrients below
    "Dissolved Ammonia, as N",
    # "Dissolved Nitrate/Nitrite, as N",
    "Dissolved Nitrogen",
    # "Dissolved o-Phosphate, as P",
    "Dissolved Phosphorus"
    # "Nitrate, as N",
    # "Nitrate/Nitrite, as N",
    # "Nitrite, as N",
    # "o-Phosphate, as P",
    # "Total Ammonia, as N",
    # "Total Dissolved Nitrogen",
    # "Total Dissolved Phosphorus",
    # "Total Nitrogen"                                #Also SOE
    # "Total Phosphorus",                               #Also SOE
    #----------------------- Physical Ions below
    # "Dissolved Calcium",
    # "Dissolved Chloride",                             #Also SOE
    # "Dissolved Magnesium",
    #"Dissolved Organic Carbon"                   #Also SOE
    # "Dissolved Potassium",
    # "Dissolved Sodium",                               #Also SOE
    # "Dissolved Sulphate"                           #Also SOE
    # "pH",
    # "Specific Conductivity",
    # "Total Alkalinity, as CaCO3",
    # "Total Dissolved Solids",
    # "Total Hardness, as CaCO3 (calc'd)"
    # "Total Organic Carbon",
    # "Total Suspended Solids",
    # "Turbidity"
  ),  
  
  sites = c(                # e.g. c(hay river at the mouth", "yellowknife river at bridge")
    #------------------------ Only in SOE Data
    # "arctic red river 4 kms above the mouth",
    # "bosworth creek at canol drive bridge",
    # "cameron river",
    # "clinton-colden outlet"
    # "daring lake",
    # "desteffany lake",
    # "east channel  25 kms below inuvik",
    # "great bear river at the mouth"
    # "great slave lake at fort resolution",
    # "great slave lake yellowknife bay at dettah",
    # "great slave lake yellowknife bay at n'dilo",
    # "hay river west channel below the bridge",
    # "hay river at nt / ab border",
    # "island river 1 km above the mouth of trout lake",
    # "island river 500 m above the mouth",
    # "jean marie river  170 m below check point",
    # "king lake",
    # "lac de gras outlet",
    # "lake of the enemy",
    # "liard river above fort simpson ferry",
    # "liard river above kotaneelee river",
    # "little buffalo river 1 kms above the mouth",
    # "little buffalo river at highway 5 bridge",
    # "mackay lake",
    # "mackenzie river above fort providence bridge",
    # "mackenzie river at the mouth of the liard river",
    # "mackenzie river below fort providence boat launch",
    # "mackenzie river below norman wells",
    # "mackenzie river below tulita",
    # "mackenzie river above norman wells",
    # "mackenzie river above tsiigehtchic",
    # "marian river at franks channel",
    # "peel channel 15 kms above aklavik",
    # "peel river above fort mcpherson",
    # "point lake",
    # "rabbit skin river at the mouth",
    # "rocknest lake",
    # "slave river below the rapids of the drowned at the boat launch",
    # "slave river 95 kms above the mouth",
    # "slave river above the mouth",
    # "talston river at the mouth",                    # Type in raw data. Is Taltson River.
    # "tazin river at the border",
    # "trout lake at sambaa k'e",
    # "trout lake at the southwest bay",
    # "yellowknife river at bridge",,
    
    #-------------------------- In both SOE and GSL data
    # "baker creek at footbridge",
    # "buffalo river at highway 5 bridge",
    "hay river at the mouth",
    # "kakisa river at highway 1 bridge",
    # "kakisa river below kakisa lake",
    # "little buffalo river above highway 6 bridge",
    # "lockhart river at outlet of artillery lake",
    # "salt river at highway 5 bridge",
    "slave river below rapids of the drowned - mid river",
    #"slave river at big eddy"
    # "taltson river below nonacho lake dam",
    "yellowknife river 3 kms above the bridge",
    
    #--------------------------- Only in GSL data
    # "boundary creek at highway 3 bridge",
    # "cameron river above the bridge",
    # "hay river west channel above bridge",
    # "hr-ks1-3a",
    # "hr-ks2-3a",
    # "hr-ks3-3a",
    # "hr-ks4-3a",
    # "hr-ks5-3b",
    # "hr-ks6-3b",
    # "hay river at nt / ab border",
    # "hay river at paradise gardens"
    "little buffalo river above highway 5 bridge"
    # "marian river above franks channel bridge",
    # "miller creek",
    # "slave river at the mouth",
    # "sr-ks1-3b",
    # "sr-ks2-3a",
    # "sr-ks3-3b",
    # "sr-ks4-3a",
    # "sr-ks4-3b"
    # "sr-ks5-3a",
    # "sr-ks6-3b"
    # "stagg river",
    # "vital narrows"
  ),
  
  layers = c(
    "Burn__YTD_km2",
    # "Burn__YTD_pct",
    "Burn__1yr_km2",
    #"Burn__1yr_pct",
    "Burn__5yr_km2",
    #"Burn__5yr_pct",
    #"Burn__8yr_km2",
    #"Burn__8yr_pct",
    "Burn__10yr_km2"
    #"Burn__10yr_pct"
  ),
  
  start_date = "2000-01-01", # format: "yyyy-mm-dd".
  end_date = "2025-12-31",   # format: "yyyy-mm-dd".
  month_param = "July",            # Specify the month for analysis
  auto_save = T,              # User decision to auto save to folder or not.
  include_flow_or_level = T
) 

{
#______________________________________________________________________________
  results <- Water_Quality_df(
    directory = directory,            
    file_name = file_name,
    file_extension = file_extension,
    all_data = all_data_csv,
    parameters = parameters,  
    sites = sites,                     
    water_body_order = NULL,   # e.g. c("Hay River", "Yellowknife River"). Default is NULL. Is how you want to represent in a plot.
    start_date = start_date, 
    end_date = end_date,
    include_flow_or_level = include_flow_or_level,          # Include a flow percentile aesthetic layer on plot. Must be F in all sites included have no data.
    remove_outliers = T        # Choice to remove outlier points or not.
  )
  
  # Saving the dataframe as 'built.df'
  built.df <- results$data

  # Saving all the parameters from the first function
  group_by_water_body_result <- results$group_by_water_body_result
  water_body_result <- results$water_body_result
#______________________________________________________________________________
  # Get concentration-discharge plot
  c_q_plot <- concentration_flow_relation(
    data = built.df,
    plot_title = NA,           # Title for plot. Default is NA.
    n_row = NA,                # Define number of rows in plots. If NA, facet_wrap chooses default.
    n_col = NA,                # Define number of rows in plots. If NA, facet_wrap chooses default.
    legend_position = "right", # Location for the legend position. Default is "right".
    plot_width,           # Width of the figure (cm).
    plot_height,          # Height of the figure (cm).
    dpi = 900,                 # Resoultion of the figure (dots per square inch). Default is 900.
    save_path = directory,     # Location to save the file. Default is the same directory as input file.
    plot_name = "CQ_Plot",     # Name of the saved figure to be saved. Default is "WQ_Plot"
    group_by_water_body = group_by_water_body_result,
    auto_save = auto_save,
    water_body = water_body_result
  )
  
  print(c_q_plot)
  
  #______________________________________________________________________________
  # Get standard plots
  standard_plots <- water_Quality_plots(
    # Arguments for generating figure
    data = built.df,
    plot_title = NA,           # Title for plot. Default is NA.
    n_row = NA,                # Define number of rows in plots. If NA, facet_wrap chooses default.
    n_col = NA,                # Define number of rows in plots. If NA, facet_wrap chooses default.
    legend_position = "right", # Location for the legend position. Default is "right".
    plot_width,           # Width of the figure (cm).
    plot_height,          # Height of the figure (cm).
    dpi = 900,                 # Resoultion of the figure (dots per square inch). Default is 900.
    save_path = directory,     # Location to save the file. Default is the same directory as input file.
    plot_name = "WQ_Plot",     # Name of the saved figure to be saved. Default is "WQ_Plot"
    group_by_water_body = group_by_water_body_result,
    auto_save = auto_save,
    water_body = water_body_result,
    include_flow_or_level = include_flow_or_level
  )
  
  print(standard_plots)
  #______________________________________________________________________________
  # Get standard plots for the month inputted
  standard_plots_one_month <- water_Quality_plots(
    # Arguments for generating figure
    data = built.df |>
      dplyr::filter(
        trimws(.data$Month) == trimws(.env$month_param)
      ),
    plot_title = NA,           # Title for plot. Default is NA.
    n_row = NA,                # Define number of rows in plots. If NA, facet_wrap chooses default.
    n_col = NA,                # Define number of rows in plots. If NA, facet_wrap chooses default.
    legend_position = "right", # Location for the legend position. Default is "right".
    plot_width,           # Width of the figure (cm).
    plot_height,          # Height of the figure (cm).
    dpi = 900,                 # Resoultion of the figure (dots per square inch). Default is 900.
    save_path = directory,     # Location to save the file. Default is the same directory as input file.
    plot_name = paste0("WQ_Plot_", month_param),     # Name of the saved figure to be saved. Default is "WQ_Plot"
    group_by_water_body = group_by_water_body_result,
    auto_save = auto_save,
    water_body = water_body_result,
    include_flow_or_level = include_flow_or_level
  )
  
  print(standard_plots_one_month)
  #______________________________________________________________________________
  # Perform statistical analysis
  mk_results <- water_Quality_trends(
    data = built.df |>
      dplyr::filter(
        trimws(.data$Month) == trimws(.env$month_param)
      ),
    month_param = month_param,
    directory = directory,
    all_data = all_data_csv,
    file_extension = file_extension,
    save_path = directory
  )

  print(mk_results)
  
#______________________________________________________________________________
  # Perform pearson correlation on data inputted
  
  pearson_results <- pearson_correlation(
    layers = layers,
    data_WQ = built.df |>
      dplyr::filter(
        trimws(.data$Month) == trimws(.env$month_param)
      ),
    month_param = month_param,
    directory = directory,
    file_name = file_data_name_ext,
    file_extension = file_extension,
    save_path = directory,
    auto_save = auto_save,
    include_flow_or_level = include_flow_or_level
  )
  
  print(pearson_results$plot)
#______________________________________________________________________________
  # Run View(final_result$data) to access the dataframe itself
  # Run View(final_result$mk_results) to access the Mann-Kendall results as a dataframe
  # Run View(final_result$pearson_df) to access df for that test
  returns <- list(
    data = built.df,
    plots = standard_plots,
    mk_results = mk_results,
    pearson_results = pearson_results,
    pearson_df = pearson_results$df_all
  )
  
  return(returns)
  
}

final_result <- water_quality_analysis()

