# Temp_nitrogen_data_2026

**This repository contains files for the air-temperature and soil-nitrogen supply experiment.**

## **Summary of files**

### **scripts folder**
Inside the scripts folder includes:
- The file [Analysis_temp_nit_2026.r](scripts/Analysis_temp_nit_2026.r) contains the code for the data analysis and data visualization. It also contains codes for the exponential quadratic equation used for the temperature response curves. 

- The file [climate.R](scripts/climate.R) contains the scripts for data visualization of the diurnal temperature and relative humidity for the growth temperatures (37°C, 30°C, and 23°). Data was recorded with the HOBO sensors.

### **output folder**
The output data folder contains the following files:

- The [temp_nitrogen_2026.csv](output/temp_nitrogen_2026.csv) contains output of chlorophyll, Vcmax, Jmax, and nitrogen allocation-related files.

- The [vcmax_tempresp_fits.csv](output/vcmax_tempresp_fits.csv) contains parameter values for vcmax, estimated from the quadratic model.

- The [jmax_tempresp_fits1.csv](output/jmax_tempresp_fits1.csv) contains parameter values for jmax, estimated from the quadratic model.

### **data folder**

- Here [biomass_2025_data](data/biomass_2025_data.csv) contains morphology and biomass data.

- Here [curve_level_data.csv](data/curve_level_data.csv) contains the curve level data.

- Here [chlorophyll_extraction.numbers](data/chlorophyll_extraction.numbers) contains chrlorophyll extraction data.

- Here [vcmax25_jmax25_only.xlsx](data/vcmax25_jmax25_only.xlsx) contains related files of vcmax25 and jmax25 extracted, but the file was merged with the leaf n content file manually.

- Climate data from HOBO sensors includes [High_air_temp.xlsx](data/High_air_temp.xlsx), [low_temp.xlsx](data/low_temp.xlsx), and [Medium_temp.xlsx](data/Medium_temp.xlsx).


## **important names to take note:**

- plant_id: the individual pot ID.

- Rack: the position of the pot in the light bank (top/down).

- Airtemp: the air temperature treatment (37°C/30°C/23°C).

- airtemp_factor: the air temperature treatment as a factor (High/Medium/Low).

- Nfert: soil nitrogen supply treatment (ppm).

**Current DOI badge
[![DOI](https://zenodo.org/badge/1280644874.svg)](https://doi.org/10.5281/zenodo.20866655)

**Contact**

- Any questions or issues can be submitted to Daniel Owusu Kwakye at (dowusukw@ttu.edu).



