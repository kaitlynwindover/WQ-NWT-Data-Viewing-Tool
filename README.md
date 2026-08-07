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

- WQ_plot_version2.R : Has a function for each different output the code provides

- WQ_dependencies.R : Contains any chunks of code that may be repeated, such as make_plot

- hydro_calc_daily.R : Function to calculate daily hydrometric data

- hydro_compile_daily.R : Compiles hydrometric data

- hydroclim_load.R : Defines parameter based on multiple potential user inputs

- hydro_filter.R : Hydrometric filter function


Packages to Install and Load
- 
Run these lines in the console
- install.packages("ggplot2")
- library(ggplot2)
- install.packages("tibble")
- library(tibble)

Workflow
- 
1. In WQ_Analysis script, choose the inputs desired.
2. Hit source on each of the scripts needed to be open in the enviornment
3. In the data_organize function at the top of WQ_dependencies script, ensure the variable quality.df is being defined correctly; if GSL data is used, select the columns of the raw data correlated. If SOE data is used, use the other columns selected.
4. Run final_result at the bottom of the WQ_Analysis script to produce the results.

Inputs
- 
1. directory: use the 'copy address' function, or similar, in files to get the address of the directory where the data lives
2. file_name: input the exact name of the actual raw data file used
3. file_extension: likely leave this as ".csv"
4. all_data_csv: a separate csv file that contains all the sites with their coordinates
5. parameters: comment in/out the parameters, as desired. Ensure the parameters being chosen are in the raw data file being inputted
6. sites: comment in/out the sites, as desired. Ensure the sites being chosen are in the raw data file being inputted
7. layers: the additional layers that can be layered on the water quality data for the Pearson correlation
8. start_date
9. end_date
10. month_param: the month desired to analysis the trend in the data
11. auto_save: T or F depending on what you want. Files will save to the same directory as inputted
12. include_flow_or_level: T or F depending on what you want. If T, real-time hydrometric data will be pulled and the code may run a bit slower

Outputs
- 
1. WQ_Plot_Parameter: standard plots of the data
2. WQ_Plot_Month_Parameter: standard plots of the data for only the month_param desired
3. CQ_Plot_Parameter: concentration-flow plots for each parameter inputted
4. Standard_Plots_Pearson: Parameter x Layer plots for a plotted visual of the heatmap
5. Mann_Kendall_Heatmap
6. Pearson_Correlation_Heatmap

Functions
- 
In WQ_dependencies
1. data_organize: organizes the data with other functions
3. add_water_body: groups each individual site into water bodies. May be useful to look at for site selection
4. add_WSC_ID: Adds the water survey of canada ID for flow/level data
5. build.df: builds the general dataframe based on inputs
6. make_plot: makes and controls the aesthetics of the standard plots
7. make_CQ_plot: makes and controls the aesthetics of the CQ plots
8. stat_graphic: makes and controls the aesthetics of the statistical trend analysis heatmaps

In WQ_plot_version2
1. Water_Quality_df: builds the dataframe and goes through case-specific adjustments
2. water_Quality_plots: loops through make_plot for all input cases
3. concentration_flow_relation: produces C-Q plots with a log scale
4. water_Quality_trends: Performs the Mann-Kendall test
5. pearson_correlation: Performs the Pearson correlation

In WQ_Analysis
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

Contributions
-
- Ryan Connon
- Nick Wilson
- Robin Staples
- Emma Gregory
- Adler Grienke
- Kaitlyn Windover
