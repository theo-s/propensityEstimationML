## Propensity Score Estimation using Machine Learning

This repository contains several methods to correct predicted propensity scores 
for observational data estimated by machine learning algorithms.

# Simulations

In order to run the standard estimation simulations, run the script:
```
Rscript code/sims.R
```
The figures will then be saved in the `figures` folder.
In order to run the high dimensional simulations, run the script:
```
Rscript code/high_dim_sims.R
```

# Data Analysis

In order to run the analysis of the census data, you need to first pull the data.
This requires the `folktables` package, you can install this with:
``
pip install folktables
``
The script `code/pull_data.py` then pulls the appropriate data and stores it in the 
`data` folder.
