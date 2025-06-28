# Financial Ratio Analysis Project

This is an end-to-end project focused on analyzing how financial ratios influence stock price changes using **Bayesian hierarchical modeling**. The project encompasses data cleaning, exploratory analysis, posterior diagnostics, and culminates in a comprehensive R Markdown report.

---

### 🔗 **[CLICK HERE TO VIEW THE INTERACTIVE PROJECT REPORT](https://tkaky.github.io/tkaky-Financial-Ratio-Analysis/)** 🔗

The detailed report contains all the code, analysis, and interactive plots generated throughout this project.

---

## Project Overview

This report provides an in-depth financial ratio analysis, detailing the methodology from data acquisition to model interpretation.

### Data Acquisition & Notes

Financial reports were obtained using **Yahoo Finance's hidden API**. Historical stock prices were retrieved using the **`quantmod` R package**. The list of companies and their respective sectors was sourced from the **Public Disclosure Platform (KAP)**, a government website where Turkish companies share all their official financial and corporate info.

The primary analysis in this project utilizes **annual reports**. However, the provided code also includes the process for pulling **quarterly reports**, which is very similar. Please note a limitation of Yahoo Finance's API: it typically provides financial data for a maximum of the past 5 quarters and 4 annual reports.

## Repository Structure

* `index.html`: The **live, interactive project report**, generated from the R Markdown source and hosted via GitHub Pages. This is the main output document.
* `Financial-Ratio-Analysis.rmd`: The **source R Markdown file** for this report, containing all the R code, analysis steps, and narrative. This is the core reproducible script.
* `Data/`: This folder contains all the raw **CSV data files** that were used in the analysis, including processed financial statements and company lists.
* `R-Script/`: Stores supporting **R scripts**.
    * `Data_pull.R`: This script contains the complete code for pulling financial reports from Yahoo Finance's API, historical prices, and company sector information from KAP.
    * `Line_items.R`: This script includes code for exploring and seeing all the line item names Yahoo Finance uses. It was developed through exploration, as another comprehensive method to list all line items for various sectors was not readily available.
* `Samples/`: Houses the **MCMC sample objects** (`mod_sim.rds` file) generated from the Bayesian models. 
* `README.md`: This overview file provides an introduction to the project and guides users through the repository.

---

**Author:** Taha Kerem Akyol
* [LinkedIn Profile](https://www.linkedin.com/in/taha-akyol-8b07bb355)
