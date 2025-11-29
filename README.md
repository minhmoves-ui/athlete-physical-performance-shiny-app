# Soccer Athlete Performance Intelligence Dashboard

This repository contains a Shiny web application for profiling soccer athletes
using performance testing data.

## What the app does

- Displays a player card for each athlete
- Shows Overall Athleticism, Strength, Speed, and Endurance scores
- Breaks down key test metrics:
  - IMTP Peak and Relative
  - Sprint 0–10 m and 30 m
  - Top Speed
  - YYIRT1
- Compares athletes within position groups using radar charts

## Files

- `app.R` – main Shiny app (UI + server in one file)
- `PlayerCard_Tscores.csv` – standardized T-scores

## How to run the app locally

1. Download `app.R` and `PlayerCard_Tscores.csv` into the same folder.
2. Open `app.R` in RStudio.
3. Make sure the working directory is that folder.
4. Run:

```r
library(shiny)
runApp()
