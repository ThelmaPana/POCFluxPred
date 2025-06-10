# POCFluxPred

Predict POC flux from POC concentration profiles.

------------------------------------------------------------------------

The purpose of this work is to use machine learning to relate POC (particulate organic carbon) flux measured by a sediment trap at a given depth to various predictors (environment, POC concentration…) as well as measurement depths. This includes a set of experiments using various sets of predictors.

## Input data

-   POC concentration data from <https://data.marine.copernicus.eu/product/MULTIOBS_GLO_BIO_BGC_3D_REP_015_010/description>

-   Environmental data from various sources including WOA and other satellite products

-   POC flux data, originally from [Mouw at al. 2016](https://doi.pangaea.de/10.1594/PANGAEA.855594), then by Ramondenc et al. in prep

## Modelling

In terms of modelling, we use XGBoost for regression. To simultaneously tune model parameters and compute robust estimate of model performance, we use nested cross validation. At each iteration, the outer split serves as the test set while inner splits are used for model tuning.

Two resampling strategies are used for the nested CV:

-   stratified sampling, based on deciles of the target variable (POC flux), providing an upper estimate of model performance

-   spatial sampling, based of spatial blocks, providing a lower estimate of model performance

All the modelling is performed using the [tidymodels](https://www.tidymodels.org/) framework.

## Repo organisation

The repository contains the following directories:

-   `data`: where data live

-   `experiments`: performed ML experiments

-   `models`: saved ML models

-   `R`: scripts to run experiments

-   `reports`: quarto reports for results

Four notebooks are present for data preparation:

-   `00.prepare_poc_flux_data`: to process POC flux data

-   `01.prepare_poc_conc_data`: to process POC concentration data

-   `02.prepare_env_data`: to process environmental data

-   `03.assemble_dataset`: to assemble the final dataset

Other scripts include (see below for more details):

-   `render_master_reports.R`: to render the report comparing all experiments

-   `render_reports.R`: to render the report for a given experiment

-   `train.R`: to run an experiment

-   `utils.R`: contains utilities

There is also a quarto dashboard (`poc_flux_dashboard`) to explore the POC  flux dataset.

## Running experiments

1.  Before starting, restore your R environment with `renv::restore()`
2.  Set-up a new experiment using the template in `experiments/exp_new`: fill in description, predictors… in `config.yaml`
3.  From a terminal, call `Rscript train.R --experiments/exp_XX/config.yaml`
4.  Once training is done, generate report for the experiment by running `Rscript render_reports.R` (see help for options)
5.  Once all experiments are done, call `Rscript render_master_reports.R` to generate a master report of all your experiments
