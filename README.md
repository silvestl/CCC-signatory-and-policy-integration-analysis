# Climate City Contracts as Instruments of Urban Climate Governance Embeddedness: A Bipartite Network Analysis

Replication data and code for PUBLICATIONNAME

## Repository Contents

### Data
- **NetworkstakeholdersCCC.xlsx** — Main dataset. Contains 2,449 CCC co-signatories across 80 European Climate City Contracts, coded by actor type and policy sector (CAP Topic codes).
- **AdditionalData.xlsx** — Supplementary data used in the regression analysis collected from various sources (see sources on dataset).
- **Elementary data on cities involved with the EU's Cities Mission (Silvestri, 2024).xlsx** — Supplementary data used in the regression analysis on EU Cities Mission member cities, first published in Silvestri (2024).

### Code
- **BipartiteNetworkfinal.py** — Main Python script. Constructs weighted bipartite networks for each CCC, computes weighted network density, Gini coefficients, and Lorenz curves.
- **interactive_degree_distribution_lorenz_curves.py** — Generates interactive visualizations of degree distributions and Lorenz curves across CCC networks.
- **codenzcccc.R** — R script for the Herfindahl–Hirschman Index calculations, descriptive statistics, regression model, and all other figures and visualizations.

## Data Sources
CCC co-signatories were manually extracted from Climate City Contracts published on the NetZeroCities Knowledge Repository (https://netzerocities.app/knowledge) as of March 2026.
Policy sectors were assigned using the Comparative Agendas Project Master Codebook v1.1 (https://www.comparativeagendas.net).

## Requirements
**Python**: `pandas`, `networkx`, `numpy`, `matplotlib`, `plotly`, `adjustText` 
**R**: `tidyverse`, `ggplot2`, `readxl`, `stringr`, `vegan`, `forcats`, `tidyr`, `tibble`, `writexl`, `ggrepel`, `igraph`, `scales`, `janitor`, `lmtest`, `car`, `sf`, `rnaturalearth`, `rnaturalearthdata`,
`tidygeocoder`, `tidygraph`, `viridis`, `patchwork`, `maps`, `lubridate`, `fixest`

## Citation
If you use this data or code, please cite:
PUBLICATIONNAME

## Contact
Lorenzo Silvestri — lorenzo.silvestri@uni-graz.at  
Wegener Center for Climate and Global Change, University of Graz
