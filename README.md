# Identifying food consumption waste clusters in Mexico City through exploratory spatial data analysis

## Abstract

While formal definitions of food systems may vary, in general they include the way we produce, distribute and consume food. Food consumption is evidently related to the production of large quantities of certain types of waste that can and should be handled differently than other types of urban wastes, namely urban organic waste. Possible waste management techniques include local composting sites, home composting and proper waste separation. In large metropolitan areas such as Mexico City, proper food waste management is a daunting challenge for local governments and vital for improvement of overall quality of life and to reduce potential sites for health concerns. Poor food waste management could result in foci of infection and pests. In this regard, this work argues that as we push toward more environmentally friendly cities, the identification of food waste clusters in Mexico City could provide valuable information for better planning of waste management in the future. Furthermore, the paper discusses possible strategies for food waste management based on the results. This work uses data from the National Institute of Statistics and Geography (INEGI) in Mexico regarding business activities and categorizes them in terms of the likelihood of onsite food consumption, and thus possible food waste production clusters. While restaurants are the first food outlets that come to mind in terms of onsite food waste production; grocery stores, convenience stores, hotels, recreational sites are also different types of businesses considered. Identification of clusters is achieved through a 4 stage process: 1) qualitative reclassification of businesses types according to the probability of onsite food consumption, 2) assigning weights to each of the reclassification categories, 3) evaluating spatial clustering with local spatial autocorrelation of the businesses by neighborhood and 4) mapping results.

## Files and directories

- data_prep.R - Takes raw data and GIS data and creates RData files to run program (not necessary to run, just read RData files)
- data_analysis.R - Analyzes data and creates plots
- esda.R - Creates more plots and runs autocorrelation processes with rgeoda package. Also creates shape files of results.
- scratch.R - scratch file to try code
- foodwastemexicocity_DATA.zip - contains raw data, gis data, resulting shape files and RData files
- README.md - this text

## Deployment

Before running remember to:

- Decompress "foodwastemexicocity_DATA.zip"
- Decompress all ZIP files in the "RData" directory
- Rename directory where needed

## Built With

| Software       | Description                                                      | Version           |
|------------------------|------------------------|------------------------|
| R              | Statistical computing software                                   | 4.2.2             |
| Rstudio        | IDE for R                                                        | Spotted Wakerobin |
| ddplyr         | Grammar of data manipulation that helps solve common challenges. | 1.1-3             |
| R Color Brewer | Provides color schemes for maps designed by Cynthia Brewer       | 1.0.10            |
| ggplot2        | System for creating graphics based on "The Grammar of Graphics"  | 3.3.6             |
| sf             | Binds to GDAL for reading and writing geospatial data            | 1.0-8             |
| rgeoda         | R Library for Spatial Data Analysis                              | 0.0.9             |
| rgdal          | Bindings for 'Geospatial' Data Abstraction Library               | 1.5-32            |

## Data sources

-   BUSINESS DIRECTORY (DENUE): <https://www.inegi.org.mx/temas/directorio/>
-   BOROUGHS (ALCALDIAS): <https://datos.cdmx.gob.mx/dataset/alcaldias/resource/8648431b-4f34-4f1a-a4b1-19142f944300>
-   NEIGHBORHOODS W/ POPULATION DENSITY (COLONIAS): <https://datos.cdmx.gob.mx/dataset/densidad-de-poblacion-2010>

## Author

Iskar J. Waluyo Moreno [waluyo\@usc.edu](mailto:waluyo@usc.edu){.email}

## License

This project is licensed under the [CC0 1.0 Universal](LICENSE.md) Creative Commons License - see the [LICENSE.md](LICENSE.md) file for details

## Acknowledgments

Project for USC Spatial Sciences Institute
