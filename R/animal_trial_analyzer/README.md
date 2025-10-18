# Animal Trial Analyzer: Veterinarian Guide

Welcome to the **Animal Trial Analyzer**, a Shiny-powered application designed to help veterinarians and animal health researchers evaluate intervention outcomes with confidence. This guide walks you through preparing your data, navigating the app, and interpreting the results so you can focus on clinical insights.

## 1. Before You Begin

### System Requirements
- R (version 4.2 or newer) with the packages listed in the project root `README.md`.
- A modern web browser (Chrome, Firefox, Edge, or Safari) for the interactive dashboard.

### Gather Your Trial Data
Prepare a spreadsheet (CSV or Excel) with the following recommended columns:

| Column | Description | Example |
| --- | --- | --- |
| `animal_id` | Unique identifier for each animal | `Cow-101` |
| `treatment_group` | Treatment or control group name | `Vaccine A`, `Placebo` |
| `time_point` | Observation time (day, week, etc.) | `Day 14` |
| `outcome_measure` | Primary metric recorded (e.g., antibody titer, weight) | `125` |
| `secondary_measure` *(optional)* | Additional outcomes (e.g., temperature) | `38.5` |
| `notes` *(optional)* | Relevant veterinary observations | `Mild swelling at injection site` |

> **Tip:** Keep column names short and consistent. Avoid spaces or special characters other than underscores.

## 2. Launching the App
1. Open R or RStudio and set your working directory to the project root.
2. Run `source("app.R")` or click **Run App** in RStudio.
3. The Animal Trial Analyzer dashboard will open in your default browser.

## 3. Uploading Data
1. Navigate to the **Upload Data** tab.
2. Click **Browse** to select your CSV or Excel file.
3. If your file includes column headers, ensure the **Header** checkbox is selected.
4. Use the **Preview** table to confirm the data loaded correctly.
5. (Optional) Use the **Filter** controls to focus on specific species, treatments, or time frames.

> **Veterinary Insight:** Confirm that all animals have a treatment group assigned—missing assignments can skew downstream analyses.

## 4. Analyzing Outcomes
### One-Way ANOVA
Use when comparing a single outcome across multiple treatment groups.
1. Open the **One-Way ANOVA** tab.
2. Select the outcome variable (e.g., antibody titer).
3. Choose the grouping factor (e.g., treatment group).
4. Review the ANOVA table and post-hoc comparisons to identify statistically significant differences.

### Two-Way ANOVA
Use when examining the interaction between two factors (e.g., treatment group and time point).
1. Open the **Two-Way ANOVA** tab.
2. Select the outcome variable and both factors.
3. Evaluate interaction plots to determine if combined factors influence outcomes.

### Additional Analytics
- **Summary Statistics:** Quickly review means, medians, and standard deviations for each group.
- **Diagnostics:** Residual plots and normality checks help verify model assumptions.

> **Clinical Tip:** Pair statistical findings with veterinary observations (e.g., behavior changes, adverse reactions) for holistic decision-making.

## 5. Visualizing Results
1. Visit the **Visualize** tab.
2. Choose chart types (boxplots, line graphs, or bar charts) relevant to your study design.
3. Customize colors and facets to highlight specific species or treatment arms.
4. Export high-resolution images for reports or regulatory submissions.

## 6. Interpreting and Reporting
- **Significance Levels:** Default thresholds are set at p < 0.05. Adjust as needed for exploratory analyses.
- **Effect Sizes:** Review effect size metrics alongside p-values to gauge clinical relevance.
- **Documentation:** Use the built-in download buttons to capture tables and plots for your trial dossier.

## 7. Troubleshooting
| Issue | Possible Cause | Recommended Action |
| --- | --- | --- |
| Data fails to upload | Unsupported file format | Save as CSV or XLSX and retry |
| Missing factor levels | Inconsistent group names | Standardize naming conventions before upload |
| Unexpected ANOVA results | Violated model assumptions | Check diagnostics and consider data transformation |

## 8. Best Practices for Veterinarians
- Maintain detailed clinical notes alongside quantitative data to contextualize results.
- Schedule routine data audits during trials to catch entry errors early.
- Collaborate with statisticians when designing complex multifactor studies.

## 9. Need Support?
- Review the project root `README.md` for installation and package setup.
- Submit questions or feedback through your usual project communication channel.

Thank you for using the Animal Trial Analyzer to advance animal health research. Your expertise drives better outcomes for the animals in your care.
