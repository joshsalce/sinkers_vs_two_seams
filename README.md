# On Sinkers and "Two-Seam" Fastballs

## Description & Motivation
This is a short analysis looking for any possible distinction between sinkers and two-seam fastballs at the MLB level. My motivation for this project is as follows: There appears to be a visual distinction one can draw when watching different fastballs classified as sinkers, and yet there is no defined distinction between sinkers that get a lot of depth and sinkers that get a lot of horizontal movement. There also appears to be conflicting views on whether or not there are multiple types of sinkers, if one were to look to the past for answers (e.g. [2017 Fangraphs article](https://blogs.fangraphs.com/players-view-are-two-seamers-and-sinkers-the-same-pitch/)). My analysis is an attempt to distinguish between these two groups of sinkers and uncover any insights, large or small, from the perspective of seeing sinkers coming in two different forms.

## Languages, Packages Used
- R
- R Packages: ggplot2, dplyr, tidyr/tidyverse, mgcv, MySQL
- Python Packages: [pandas](https://pandas.pydata.org/docs/), [numpy](https://numpy.org/doc/), [sklearn](https://scikit-learn.org/stable/index.html), [MlFlow](https://mlflow.org/), [XGBoost](https://xgboost.readthedocs.io/en/stable/)

## Table of Contents

| Component | Description |
|-------|---------------------------------------------------------------------------------------------------------------------------------------------------|
| [analysis.R](https://github.com/joshsalce/sinkers_vs_two_seams/blob/main/analysis.R)| R Script with main analysis involving MLB Statcast data and basic "stuff" model predictions |
| [writeup.pdf](https://github.com/joshsalce/sinkers_vs_two_seams/blob/main/writeup.pdf)| Short writeup including important visuals, tables |
| [sql](https://github.com/joshsalce/sinkers_vs_two_seams/tree/main/sql)| Includes all relevant queries of MLB Statcast as part of my analysis | 
| [Notebooks](https://github.com/joshsalce/sinkers_vs_two_seams/tree/main/Notebooks) | Includes all Jupyter Notebooks used for model training & predictions |
| [Plots](https://github.com/joshsalce/sinkers_vs_two_seams/tree/main/Plots) | Includes all visuals created in [analysis.R](https://github.com/joshsalce/sinkers_vs_two_seams/blob/main/analysis.R) |

