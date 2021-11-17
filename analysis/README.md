This folder contains the scripts used for the project's analysis:

- `fitted.mods.rds`: models for the example in `tapir-example.Rmd`;
- `hfi-regressions.R`: regressions of movement parameters against machine-learning-based human footprint index;
- `land-use-boxplots.R`: creates stacked bar graph of land type and boxplots of proportion of land use by region;
- `land-use-regressions.R`: runs the GAMs of various movement parameters against land use types;
- `maps.R`: creates `figures/data-map.png` and `figures/data-map.png`, which was then edited outside of `R` (see `figures/hr-map_inset_dark.png`);
- `meta-figure.R`: creates `figures/meta.png`;
- `outlier-cleaning.R`: contains any outlier cleaning/checking done by Stefano;
- `summary-boxplots.R`: creates `figures/boxplots.png`;
- `tapir-example.pdf`: example of movement analysis on a single tapir;
- `tapir-example.Rmd`: example of movement analysis on a single tapir;
- `tapir-models.R`: fits the movement models to each tapir's data;
- `tapirs-summary-csv.R`: creates a summary `csv` file containing all relevant data for each tapir;
- `theta0.rds`: initial parameter guess for the movement model fit in `tapir-exaple.*`;
- `vhf-hr-regression.R`: fits the GLM for home range size against sampling method (VHF or GPS).
