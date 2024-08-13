# Groundwater Level Dataset for Chile - README

## Overview

This dataset provides comprehensive information about groundwater levels in Chile, spanning the period from 1970 to the present. The data includes Depth to Water (DTW) and Water Levels above sea level (GWL) from monitoring wells across the nation. The dataset is organized into Excel files, each containing essential parameters for in-depth groundwater analysis.

## Excel Columns

1. **ID:** Unique identifier for each record.
2. **Name:** Name of the monitoring well.
3. **Code:** BNA Code, including Basin, Sub-basin, and Area information.
4. **Elevation:** Elevation above sea level of the monitoring well. [m.a.s.l.]
5. **Basin:** Geographic basin to which the well belongs.
6. **Sub_Basin:** Sub-basin within the larger geographic basin.
7. **Depth to water (m):** Depth to water measured in meters.
8. **Outlier:** Boolean flag indicating whether the data point is an outlier.
9. **Status:** Classification of well condition during measurements (e.g., "Static," "Dynamic," "No Access," "Dry," "Embedded," "Surging").
10. **Longitude_GCS_WGS_1984:** Longitude coordinates in the GCS_WGS_1984 system.
11. **Latitude_GCS_WGS_1984:** Latitude coordinates in the GCS_WGS_1984 system.
12. **Date_String:** Date of the measurements.
13. **elevation_NASADEM:** Elevation data from NASADEM (Shuttle Radar Topography Mission data - a Digital Elevation Model, DEM). [m.a.s.l.]
14. **slope_NASADEM:** Slope data from NASADEM. [degree]
15. **aspect_NASADEM:** Aspect data from NASADEM.
16. **elevation_Alos_Palsar:** Elevation data from ALOS PALSAR (Digital Terrain Model, DTM). [m.a.s.l.]

## File Formats and Access Links

- **Pozos1970-2018 Dataset:** Compressed RAR Archive. [Access Link](https://osf.io/pguw7)
- **Pozos2019-2021 Dataset:** Compressed RAR Archive. [Access Link](https://osf.io/qnzgu)
- **Concatenated Files:** Compressed ZIP Archive. [Access Link](https://osf.io/m5k72)
- **Processing Code:** Jupyter Notebook (IPython). [Access Link](https://osf.io/swdg9)

## Elevation Datasets

Two external elevation datasets were incorporated for enhanced analysis:
- **NASADEM:** Shuttle Radar Topography Mission data. [Reference](https://www2.jpl.nasa.gov/srtm/)
- **ALOS PALSAR:** Digital Terrain Model. [Reference](https://asf.alaska.edu/data-sets/topographic/)

## Geographical Distribution

Check Figure 2 in the documentation for a geographical distribution of DTW records available for Chile, spanning from 1970 to 2021.

## Technical Validation

The dataset underwent thorough validation using standard statistical methods and cross-validation with elevation data from NASADEM and ALOS PALSAR. Refer to the documentation for details.

---

Author:
Héctor L. Venegas-Quiñones
University of Arizona, Hydrology and Atmospheric Sciences, 1133 E James E. Rogers Way, Tucson, AZ 85719, The United States
venegas-quinones@arizona.edu