# Temp_nitrogen_data_2026

**Repository Description**

This repository contains files for the air-temperature and soil-nitrogen supply experiment.

## **Summary of files**

### **scripts folder**
- Inside the scripts folder, the file [Analysis_temp_nit_2026.r](scripts/Analysis_temp_nit_2026.r) contains the code for the data analysis and data visualization. It also contains codes for the exponential quadratic equation used for the temperature response curves. 

### **output data folder**
The output data folder contains the following files:

- The [temp_nitrogen_2026.csv](output data/temp_nitrogen_2026.csv) contains output of chlorophyll, Vcmax, Jmax, and nitrogen allocation-related files.

- The [vcmax_tempresp_fits.csv](output data/vcmax_tempresp_fits.csv) contains parameter values for vcmax, estimated from the quadratic model (Vcmax = exp(a + b * T + c * T^2).

- The [jmax_tempresp_fits1.csv](output data/jmax_tempresp_fits1.csv) contains parameter values for jmax, estimated from the quadratic model (Jmax= exp(a + b * T + c * T^2).

### **data folder**

- Here [biomass_2025_data](data/biomass_data.csv) contains morphology and biomass data.

- Here [curve_level data.csv](data/curve_level data.csv) contains the curve level data.

- Here [chlorophyll extraction.numbers](data/chlorophyll extraction.numbers) contains chrlorophyll. extraction of raw data

- Here [vcmax25_jmax25_only.xlsx](data/vcmax25_jmax25_only.xlsx) contains related files of vcmax25 and jmax25 extracted, but the file was merged with the leaf n content file manually.


## **important names to take note:**

- plant_id: the individual pot ID.

- Rack: the position of the pot in the light bank (top/down).

- Aitemp: the air temperature treatment (37°C/30°C/23°C).

- airtemp_factor: the air temperature treatment as a factor (High/Medium/Low).

- Nfert: soil nitrogen supply treatment (ppm).


- For inquiries, contact me at (dowusukw@ttu.edu).

