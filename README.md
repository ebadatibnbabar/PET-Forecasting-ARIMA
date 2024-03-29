# PET Forecasting using ARIMA
Application of the AutoRegressive Integrated Moving Average (ARIMA) model to forecast potential evapotranspiration (PET) patterns. Leveraging time-series analysis techniques, it predicts future rainfall levels by analyzing historical data specifically from Bahawalnagar District, Punjab, Pakistan. Additionally, this project aids in anticipating drought patterns across the region.

![PET Forecasting ARIMA](output-pet-forecast-arima.png)

# Model Type
The ARIMA (AutoRegressive Integrated Moving Average) model employed in this repository is a stochastic time series analysis technique utilized for forecasting rainfall patterns. ARIMA models are adept at capturing both short-term and long-term dependencies within sequential data, making them particularly suitable for modeling and predicting temporal phenomena such as rainfall fluctuations.

# Data
The repository contains time series data consisting of montly PET data spanning from 1993 to 2020. PET was calculated from minimum and maximum temperature using Hargreaves method by DrinC software. The data was meticulously recorded by the Pakistan Meteorological Department and obtained for M.Phil Research purposes.

# Requirements
-	RStudio version 2023.06.1 Build 524
-	**R Libraries:** lubridate, ggplot2, readxl, tidyverse, dplyr, astsa, forecast, urca, ggfortify, tsutils, writexl

# Citation
If you use this repository or the data provided, please cite the following:
- Rehman, E. (2024). PET Forecasting ARIMA. GitHub. https://github.com/ebadatibnbabar/PET-Forecasting-ARIMA
- Pakistan Meteorological Department. (1992-2021). Temperature Data. Retrieved from https://www.pmd.gov.pk/en/
