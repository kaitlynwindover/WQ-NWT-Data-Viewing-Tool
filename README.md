# WQ-NWT-Data-Viewing-Tool

Introduction
- 
What it Does
- 
This code is a tool that can be used to explore water quality data from all over the NWT. It provides standard plots to view raw data easily, concentration-flow plots, performs a Mann-Kendall test and displays it as a heatmap for parameters versus sites, and performs a Pearson Correlation and displays it as a heatmap for parameters versus other layers that are added to the data. These layers include km^2 or percent of area burned by wildfire in different timescales.  
It pulls hydrometric data from the stations in real-time. It also filters and cleans data provided by the GNWT by removing outliers and sorting through parameters. 

Why it Exists
- 
There is an extensive amount of data on physical ions, nutrients, dissolved metals, and total metals for the NWT from the 1980s to the present day. There are 91 parameters and 85 individual sites, which can be sorted into 32 water bodies or into watersheds. 

Instillation 
- 
Scripts to Have Open in Environment
- 
- WQ_Analysis.R : Most changes to the code for running different inputs happen here. This is the script that contains all others. 

- WQ_plotting_and_trends.R : Has a function for each different output the code provides

- WQ_dependencies.R : Contains any chunks of code that may be repeated, such as make_plot

- hydro_calc_daily.R : Function to calculate daily hydrometric data

- hydro_compile_daily.R : Compiles hydrometric data

- hydroclim_load.R : Defines parameter based on multiple potential user inputs

- hydro_filter.R : Hydrometric filter function


Packages to Install and Load
- 
Run these lines in the console
- install.packages("tidyhydat")
- library(tidyhydat)
- install.packages("ggplot2")
- library(ggplot2)
- install.packages("tibble")
- library(tibble)

Workflow
- 
1. In WQ_Analysis script, choose the inputs desired.
2. Hit source on each of the scripts needed to be open in the enviornment
3. In the data_organize function at the top of WQ_dependencies script, ensure the variable quality.df is being defined correctly; if GSL data is used, select the columns of the raw data correlated. If SOE data is used, use the other columns selected.
4. Run final_result at the bottom of the WQ_Analysis script to produce the results (with curser on the line, do ctrl + enter to execute. Or simply run WQ_Analysis.

Inputs
- 
1. directory: use the 'copy address' function, or similar, in files to get the address of the directory where the data lives
2. file_name: input the exact name of the actual raw data file used
3. file_extension: likely leave this as ".csv"
4. file_data_name_ext: the csv file that contains the additional layers to be used in the pearson correlation. **This file currently has burn area (km^2) and burn percentage for 1 year, 5 years, 8 years, and 10 years.**
5. all_data_csv: a separate csv file that contains **all the sites with their coordinates**
6. parameters: comment in/out the parameters, as desired. Ensure the parameters being chosen are in the raw data file being inputted
7. sites: comment in/out the sites, as desired. Ensure the sites being chosen are in the raw data file being inputted
8. layers: the additional layers that can be layered on the water quality data for the Pearson correlation
9. start_date
10. end_date
11. month_param: the month desired to analysis the trend in the data
12. auto_save: T or F depending on what you want. Files will save to the same directory as inputted
13. include_flow_or_level: T or F depending on what you want. If T, real-time hydrometric data will be pulled and the code may run a bit slower

Outputs
- 
1. WQ_Plot_Parameter: standard plots of the data
2. WQ_Plot_Month_Parameter: standard plots of the data for only the month_param desired
3. CQ_Plot_Parameter: concentration-flow plots for each parameter inputted
4. Standard_Plots_Pearson: Parameter x Layer plots for a plotted visual of the heatmap
5. Mann_Kendall_Heatmap
6. Pearson_Correlation_Heatmap

The console will also output messages letting you know if some data cannot be retrieved and the reason. As well, it will show the values of the dataframes for the trend analysis'. A useful parameter to have may be n (number of data points).

Functions
-
In WQ_dependencies
-
1. data_organize: organizes the data using other functions
   
2. add_water_body: **groups each individual site into their respective water bodies.** This may be useful to look at for site selection.
   
3. add_WSC_ID: **Adds the water survey of canada ID for flow/level data.** Most sites use flow data. Some are lakes and therefore use water level data, as it makes more sense.
   
4. build.df: builds the general dataframe based on inputs
   
5. make_plot: makes and controls the aesthetics of the standard plots
    
6. make_CQ_plot: makes and controls the aesthetics of the concentration-flow plots
    
7. stat_graphic: makes and controls the aesthetics of the statistical trend analysis heatmaps

In WQ_plotting_and_trends
-
1. Water_Quality_df: builds the dataframe and goes through **case-specific adjustments**

2. water_Quality_plots: loops through make_plot for all input cases. This will plot the **concentration of each parameter against time.**
   
3. concentration_flow_relation: produces C-Q plots with a log scale. This will use the flow data to **plot concentration against flow.**
   
4. water_Quality_trends: **Performs the Mann-Kendall test**. Tau - ranges from -1 to +1 and answers - **how strong is the trend and how is it changing over time?** P-value answers - **is it a real trend or is it probable that it happened by chance? The lower the value, the more significant.**
   
5. pearson_correlation: **Performs the Pearson correlation.** This uses other data of the sites, such as burn area and burn percentage, and compares it against parameter concentrations. **R value - as X changes, does Y tend to change with it in a consistent linear way, and how strongly?** P-value answers - **is it a real trend or is it probable that it happened by chance? The lower the value, the more significant.** 

In WQ_Analysis
-
1. water_quality_analysis: takes in all user inputs, runs through all the functions, and produces all results

Assumptions
- 
-	For data used for the Mann-Kendall test and for the Pearson Correlation test : if the number of data points is less than 4 (n < 4) , data is not statistically significant and therefore not included
-	Data at the detection limit is treated as a value 50% of such detection limit
-   If data for a site + parameter combo has >50% of its values at the detection limit, data is not statistically significant and therefore not included
-	Chose to look at burn history for 1 year, 5 years, 8 years, and 10 years as the significant time markers
-	For pH and turbidity, field measurements were used when available, as opposed to physical parameters, for the most accuracy 

Known Issues & Limitations
- 
-	“Lockhart River at Outlet of Artillery Lake” has Turbidity units in JTU (very old measurements)
-	Some sites have two sampling locations (lake water vs river water)
-	Mercury measurements that were in units of ug/L are removed due to all of them being at the detection limit
-	The order of the standard plots for individual sites are sorted by latitude. This sometimes is the same as up-stream/down-stream order and sometimes not

Common Errors
- 
- when commenting/un-commenting anything in lists (parameters, sites), ensure there are commas separating all of them with no comma following the final list entry.
- when copying your directory address, ensure forward slashes are used instead of backward slashes.

Working Example
-
A prompt: 

"I want to know how the concentrations of dissolved organic carbon and total suspended solids are changing between the years 2000 to 2025 in Hay River specifically. I am going to compare the data over the years using the month of July. Additionally, I want to see if there are any significant trends with these parameters and area burned for the last year and the last 10 years. I would also like to see how the concentrations change with the flow of the river."


What the inputs would look like:

water_quality_analysis <- function(
  directory = "C:/Users/OneDrive - Example/",                     
  file_name = "GSL Tributaries Data for Queens May 2026 Physical Ions",          
  file_extension = ".csv",                                                                      
  file_data_name_ext = "WatershedsWithWQData_AndFireHistory",
  all_data_csv = "total_dataset_version9",
  
  parameters = c("Dissolved Organic Carbon", "Total Suspended Solids"),  
  sites = c("hay river at the mouth", "hay river west channel above bridge", "hr-ks1-3a", "hr-ks2-3a", "hr-ks3-3a", "hr-ks4-3a", "hr-ks5-3b", "hr-ks6-3b", "hay river at nt / ab border", "hay river at paradise gardens"),
  
  layers = c("Burn__1yr_km2", "Burn__10yr_km2"),
  
  start_date = "2000-01-01", 
  end_date = "2025-12-31",  
  month_param = "July",           
  auto_save = T,             
  include_flow_or_level = T
) 

Contributions
-
- Ryan Connon
- Nick Wilson
- Robin Staples
- Emma Gregory
- Adler Grienke
- Lydia Morrow
- Kaitlyn Windover

To contact for questions about the code/workflow: 23vrx1@queensu.ca
