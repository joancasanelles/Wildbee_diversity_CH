# Wild bee diversity in Switzerland

## Description

This repository contains the scripts for the figures in the paper "*Wild bee taxonomic and functional metrics reveal a spatial mismatch between α- and ß-diversity in Switzerland*" in Conservation Biology.

Authors: Bertrand Fournier & Joan Casanelles-Abella

## Data

To run the scripts, download the necessary data. The data can be found in the repository [envidat](www.envidat.ch) under the following doi: <https://www.doi.org/10.16904/envidat.337>

The data includes the following:

-   The calculated community-level metrics (the responses) used in the paper per plot:

    -   **Alpha taxonomic community metrics: s**pecies richness and Shannon diversity

    -   **Alpha functional community metrics:** Functional richness (using the Trait Onion Peeling index, TOP), functional eveness (using the Trait Even Distribution index, TED) and the functional dispersion.

    -   **Community weighted means of 8 functional traits**

    -   **The local community contributions on the functional and taxonomic beta diversity (LCBD).**

-   **Spatial environmental predictors and responses:** The environmental predictors used for the modelling

    -   Predictors

        -   The raster on the climate contains the PCA axes on the CHELSA variables.

        -   The raster on resource availability contains the PCA axes on plant communities

        -   The raster on land-use contains the land-use composition at 2500 m.

        -   The raster on beekeeping intensity contains thedensity of hives within 2500 m

    -   Responses

        -   Rasterstack of the responses without masking waterbodies

        -   Rasterstack of the responses masking waterbodies

-   **The model evaluation, variable importance and partial dependece data.**

-   **Elevation and waterbodies:** Rasters of the elevation and waterbodies of Switzerland

### Citation

When using the data, please cite is as: "Joan Casanelles Abella; Bertrand Fournier; Simone Fontana; Marco Moretti (2022). Data on wild bee taxonomic and functional diversity in Switzerland. EnviDat. doi: 10.16904/envidat.337."

## Scripts

Create two folders, "input" and "output". Add the data in the input file. When creating subfolders within "input", adapt the script.

The scripts included are the following:

-   Diversity_modelling_caret.R = This script was used to perform the spatial biodiversity models

-   Scripts F1-F5: scripts for the main figures of the paper

-   Other scripts: for the supplementary figures
