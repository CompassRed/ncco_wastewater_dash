# NCCo COVID-19 Wastewater Testing Dashboard

[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

## Overview
In April of 2020, [New Castle County (NCCo)](https://nccde.org/) Executive Matt Meyer announced a partnership with Biobot Analytics, Inc. to begin regular analysis of county and city of Wilmington wastewater in order to estimate the level of novel coronavirus in the sewer service area. In August, those efforts are being continued through a parternship with the University of Delaware's newly created [Center for Environmental and Wastewater Epidemiological Research (CEWER)](https://www.udel.edu/academics/colleges/canr/departments/animal-and-food-sciences/affiliated-centers/cewer/). NCCo tasked [CompassRed](https://www.compassred.com/) with creating an interactive dashboard for this information (previously presented to the public in an Excel file).

## Technology Used
This dashboard was created using R's application framework `shiny`. Main R packages used are:

- `tidyverse`: data manipulation and transformation
- `leaflet`: choropleth map (OpenStreetMap)
- `ggplot2` and `plotly`: bar and trendline charts
- `shinycssloaders` and `bsplus`: front-end improvements
- `shinyjs`: sending JavaScript to the browser and back to the R server

Additionally, QGIS was used for transforming county shape data (`geodatabase` format) into `.geojson` format for use in `leaflet` mapping.

## Relevant Links

- [Live app running on shinyapps.io](http://compassred.shinyapps.io/ncco_wastewater)
- [Raw data](http://compassred.shinyapps.io/ncco_wastewater)
- [New Castle County, Delaware (NCCo)](https://nccde.org/)
- [Biobot Analytics](https://www.biobot.io/)
- [University of Delaware's Center for Environmental and Wastewater Epidemiological Research (CEWER)](https://www.udel.edu/academics/colleges/canr/departments/animal-and-food-sciences/affiliated-centers/cewer/)
- [CompassRed](https://www.compassred.com/)

## Development Notes
Requires `.Renviron` with `LOCAL` variable set to `TRUE` or `FALSE`
