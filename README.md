# Sleep Health Multivariate Analysis

## 1. Project Description

This project explores the relationships between **sleep quality**, **health metrics**, and **lifestyle factors** using multivariate statistical methods. The project was developed in the context of the course [LSTAT2110 - Analyse des données](https://uclouvain.be/cours-2024-lstat2110) (UCLouvain, 2024-2025).

The analysis is based on a pubicly available dataset (see [Kaggle](https://www.kaggle.com/datasets/uom190346a/sleep-health-and-lifestyle-dataset) or the `data` folder of this repository). It contains information from 374 individuals including sleep duration, stress levels, blood pressure, BMI, and sleep disorders.

The goal was to gain understanding about the key drivers of sleep.

## 2. Methodology

| Method | Purpose |
|--------|---------|
| **Principal Component Analysis** | Dimensionality reduction and variable relationships |
| **Multiple Correspondence Analysis** | Categorical variables associations |
| **Hierarchical Clustering** | Optimal cluster number selection (Ward's method) |
| **K-means** | Final clustering consolidation |

## 3. Project Structure

```
Unsupervised-Learning-Sleep-Health/
├── README.md
├── data/
│   └── sleep_health_data.csv
├── R/
│   ├── functions.R              # Custom visualization & analysis functions
│   └── config.R                 # Configuration
├── analysis/
│   └── sleep_analysis.Rmd       # Main analysis notebook
├── figures/                     # Generated plots
└── renv.lock                    # R package dependencies
```

## 4. Getting Started

### Prerequisites

- R (≥ 4.0)
- RStudio (recommended)

### Installation

1. Install R dependencies
```r
# Install renv for dependency management
install.packages("renv")
renv::restore()
```

2. Run the analysis
```r
# Open and knit the R Markdown file
rmarkdown::render("analysis/sleep_analysis.Rmd")
```
