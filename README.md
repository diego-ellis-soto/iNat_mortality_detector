# Inat mortality detector

Analysis and Shiny App

## Overview

The **iNat Mortality Detector** is a data-driven tool designed to analyze wildlife mortality trends based on observations from [iNaturalist](https://www.inaturalist.org/). It provides insights into species-specific mortality patterns across regions, time, anthropogenic, biotic and environmental factors.

Specifically, we have hosted a Shiny web-based application on a Huggingface Space on https://huggingface.co/spaces/diegoellissoto/iNaturalist_mortality_detector

## Features

-   **Interactive Visualization**: Explore mortality hotspots using dynamic maps and plots.
-   **Species Analysis**: Generate reports for specific species or groups (birds, mammals, etc.).
-   **Data Integration**: Combines iNaturalist data with custom filtering and query options.

This tool is powered by **R** and **Shiny**, leveraging reproducible workflows and dynamic visualization capabilities. It’s a valuable resource for ecologists, conservationists, and researchers studying biodiversity.

## Installation

Clone the repository

The framework provides an intuitive and interactive interface for analyzing wildlife mortality data. Below are screenshots showcasing the key components:

### 1. Interactive Map

![Interactive Map](assets/Screenshot1.png)

This interactive map highlights mortality hotspots for various species, enabling easy visualization of trends across regions.

### 2. Species Analysis Dashboard

![Species Analysis Dashboard](assets/Screenshot2.png)

The dashboard allows users to delve deeper into mortality patterns, filter by species, and generate reports.

Designed by Diego Ellis Soto email: [diego.ellissoto\@berkeley.edu](mailto:diego.ellissoto@berkeley.edu){.email}

### 3. Folder structure:

Necessary code to replicate the figures and analysis of the manuscript are located in folder src/analysis/ms_analysis_figures.R

Necessary code, package dependencies and Docker container to replicate our Shiny Web app can be found in our folder src/Huggingface_Shiny_App/
