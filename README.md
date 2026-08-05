# WQ-NWT-Data-Viewing-Tool

Introduction

What it Does

This code is a tool that can be used to explore water quality data from all over the NWT. It provides standard plots to view raw data easily, concentration-flow plots, performs a Mann-Kendall test and displays it as a heatmap for parameters versus sites, and performs a Pearson Correlation and displays it as a heatmap for parameters versus other layers that are added to the data. These layers include km^2 or percent of area burned by wildfire in different timescales.  
It pulls hydrometric data from the stations in real-time. It also filters and cleans data provided by the GNWT by removing outliers and sorting through parameters. 

Why it Exists

There is an extensive amount of data on physical ions, nutrients, dissolved metals, and total metals for the NWT from the 1980s to the present day. There are 91 parameters and 85 individual sites, which can be sorted into 32 water bodies or into watersheds. 


Instillation 

Scripts to Have Open in Environment

- WQ_Analysis.R : Most changes to the code for running different inputs happen here. This is the script that contains all others. 

- WQ_plot_version2.R : Has a function for each different output the code provides

- WQ_dependencies.R : Contains any chunks of code that may be repeated, such as make_plot

- hydro_calc_daily.R : Function to calculate daily hydrometric data

- hydro_compile_daily.R : Compiles hydrometric data

- hydroclim_load.R : Defines parameter based on multiple potential user inputs

- hydro_filter.R : Hydrometric filter function


Packages to Install and Load

- install.packages("ggplot2") , library(ggplot2)
- install.packages("tibble") , library(tibble)

Usage


Workflow

Inputs

Outputs

Functions


Assumptions

-	For data used for the Mann-Kendall test and for the Pearson Correlation test : if the number of data points is less than 4 (n < 4) , data is not statistically significant and therefore not included
-	Data at the detection limit is treated as a value 50% of such detection limit
- If data for a site + parameter combo has >50% of its values at the detection limit, data is not statistically significant and therefore not included
-	Chose to look at burn history for 1 year, 5 years, 8 years, and 10 years as the significant time markers

Known Issues & Limitations
-	“Lockhart River at Outlet of Artillery Lake” has Turbidity units in JTU (very old measurements)
-	Some sites have two sampling locations (lake water vs river water)

Contributions



