# Ending the ‘thermostat wars’: a Bayesian causal framework for thermal comfort decision-making under occupant heterogeneity and uncertainty

**Matteo Favero<sup>1</sup>, Marika Vellei<sup>2,3</sup>, Andrew Sonta<sup>1</sup>**\
<sup>1</sup> ETHOS Lab, École Polytechnique Fédérale de Lausanne (EPFL), Lausanne, Switzerland\
<sup>2</sup> Univ. Bordeaux, CNRS, Bordeaux INP, I2M, UMR 5295, F-33400, Talence, France\
<sup>3</sup> Arts et Metiers Institute of Technology, CNRS, Bordeaux INP, Hesam Universite, I2M, UMR 5295, F-33400 Talence, France


---

## About

This repository contains the data processing code, statistical analysis, and manuscript source for the paper *"Ending the ‘thermostat wars’: a Bayesian causal framework for thermal comfort decision-making under occupant heterogeneity and uncertainty"*, submitted to *Building and Environment*.

This study applies Bayesian causal inference to historical laboratory thermal comfort data to quantify the effects and uncertainty of heating and cooling interventions. We show that thermal responses vary by sex and exposure duration, challenging fixed and steady-state assumptions in the PMV model, and develop a utility-weighted dissatisfaction index to support more equitable, energy-efficient building decisions.

---

## Repository Structure

The repository is organised as follows:

```         
    thermostat-war/
    │
    ├── .quarto/              # Quarto internal files and project configuration
    ├── docs/                 # Rendered documentation and project website files
    ├── figures/              # Figures used in the main article
    ├── tables/               # Tables used in the main article
    ├── notebooks/            # Supplementary material (for render)
    ├── supporting/           # Supportying analysis (for render)
    ├── src/                  
    │   ├── data/
    │   │   ├── processed/    # Cleaned and processed datasets
    │   │   └── raw/          # Original input datasets
    │   ├── script/           # Data processing, modeling, and statistical analysis (R)
    │   ├── figures/          # Figures generated during the analysis
    │   ├── tables/           # Tables generated during the analysis
    │   └── models/           # Model outputs
    │
    ├── index.qmd             # Main Quarto document presenting the project
    ├── README.md             # Project overview and documentation
    ├── references.bib        # Bibliographic references
    ├── styles.css            # Custom CSS styling
    ├── _quarto.yml           # Main Quarto configuration
    ├── _quarto-supp.yml      # Supplementary Quarto configuration
    ├── render-all.bat        # Windows script to render project documents
    └── thermostat-war.Rproj  # RStudio project file
```

\*Note: Folders under `src/models/`, `src/figures/`, and `src/tables/` are generated programmatically by the R scripts based on the prior configurations (`prior_select`, `prior_weak`) and specific model IDs.

---

## Analysis Pipeline

The analysis runs in two stages:

### Stage 1 - Statistical analysis (R)

#### Project configuration

| Script | Purpose |
|--------|---------|
| `scripts/run_all.R` | Orchestrates the analysis workflow by executing the required scripts in sequence. |
| `scripts/config.R` | Defines project configuration, including folder paths and file locations. |
| `scripts/parameters.R` | Defines project-wide parameters used throughout data preparation and analysis. |
| `scripts/development_settings.R` | Controls model-development settings, including model selection and workflow execution. |

#### Data preparation and Bayesian modelling

| Script | Purpose |
|--------|---------|
| `scripts/01_data_preparation.R` | Prepares the Nevins dataset for Bayesian modelling, including variable transformations, data centring, and aggregation of thermal sensation votes (TSV). |
| `scripts/02_model_setup.R` | Defines model specifications, formulas, priors, and the objects required for Bayesian model fitting. |
| `scripts/priors/prior_select/prior_mod_[model_id].R` | Defines prior distributions for individual model specifications. |
| `scripts/03_fit_models.R` | Fits Bayesian multinomial models using the specifications defined in `02_model_setup.R`. |

#### Model evaluation and validation

| Script | Purpose |
|--------|---------|
| `scripts/04_model_diagnostics.R` | Evaluates model convergence and Hamiltonian Monte Carlo (HMC) sampling diagnostics. |
| `scripts/05_model_comparison.R` | Compares fitted models using leave-one-out cross-validation (LOO-CV) and evaluates their predictive performance. |
| `scripts/06_kfold_cross_validation.R` | Performs stratified K-fold cross-validation as a sensitivity analysis of model predictive performance. |

#### Posterior predictive checks

| Script | Purpose |
|--------|---------|
| `scripts/07_posterior_predictive_check.R` | Performs posterior predictive checks to assess model fit against observed thermal sensation data. |

#### Posterior predictions

| Script | Purpose |
|--------|---------|
| `scripts/08_predictions.R` | Generates posterior expected predictions from the fitted Bayesian models. |

#### Average thermal sensation

| Script | Purpose |
|--------|---------|
| `scripts/09/09_average_thermal_sensation.R` | Calculates average thermal sensation. |
| `scripts/09/09a_1_visualisation_heatmap.R` | Generates heatmap visualisations of predicted thermal sensation. |
| `scripts/09/09a_2_visualisation_temperature.R` | Generates temperature-based visualisations of predicted thermal sensation. |
| `scripts/09/visualisation_setting.R` | Defines visualisation settings for the thermal sensation analysis. |

#### Risk prediction and intervention effects

| Script | Purpose |
|--------|---------|
| `scripts/10/10_risk_prediction_risk_difference_effect_modification.R` | Coordinates the risk prediction, risk difference, and effect modification analyses. |
| `scripts/10/10a_1_risk_prediction.R` | Estimates predicted dissatisfaction risk under different environmental conditions. |
| `scripts/10/10a_2_visualisation.R`  | Visualises predicted dissatisfaction risk. |
| `scripts/10/10b_1_risk_difference.R` | Estimates risk differences associated with heating and cooling interventions. |
| `scripts/10/10b_2_effect_modification.R` | Evaluates heterogeneity in intervention effects across occupant characteristics and conditions. |
| `scripts/10/10b_3_visualisation.R`  | Visualises risk differences and effect modification results. |
| `scripts/10/10/visualisation_setting.R` | Defines visualisation settings for risk prediction and intervention analyses. |

#### Utility-weighted thermal dissatisfaction index

| Script | Purpose |
|--------|---------|
| `scripts/11/11a_0_UWTDI.R` | Calculates the utility-weighted thermal dissatisfaction index (UWTDI). |
| `scripts/11/11a_1_visualisation_temperature.R` | Visualises UWTDI results across temperature conditions. |
| `scripts/11/11a_2_visualisation_heatmap.R` | Generates heatmaps of UWTDI results. |
| `scripts/11/11b_0_UWTDI_intervention.R` | Evaluates the UWTDI under heating and cooling interventions. |
| `scripts/11/11b_1_visualisation_distribution.R` | Visualises the distribution of UWTDI results across intervention scenarios. |
| `scripts/11/cost_schemes.R` | Defines the utility or cost schemes used to calculate the UWTDI. |
| `scripts/11/visualisation_setting.R` | Defines visualisation settings for UWTDI analyses. |

### Stage 2 - Manuscript rendering

The rendering process of the entire Quarto project, including the main article (`index.qmd`) and all supplementary documents, can be launched from the RStudio Terminal by typing `render-all.bat`.

``` bash
render-all.bat
```

The rendered manuscript is written to `docs/`.

---

## Citation

> Favero M, Vellei M, Sonta A. Ending the 'thermostat wars': a Bayesian causal framework for thermal comfort decision-making under occupant heterogeneity and uncertainty. *Building and Environment* (under review).
