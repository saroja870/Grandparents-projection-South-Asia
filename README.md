# Grandparents-projection-South-Asia
The future of grandparenthood in South Asia: The role of population aging and educational expansion
Overview

This repository contains the R code supporting the study "The Future of Grandparenthood in South Asia: The Role of Population Aging and Educational Expansion" by Saroja Adhikari and Diego Alburez-Gutierrez.

The study examines how demographic change and educational expansion will reshape the availability and characteristics of living grandparents in eight South Asian countries: Bangladesh, Bhutan, India, Iran, Maldives, Nepal, Pakistan, and Sri Lanka.

Using time-variant, two-sex demographic kinship models, we estimate the average number of living grandparents and their age and sex composition from 1950 to 2100. We then combine these estimates with population projections by educational attainment to examine how the educational composition of grandparents may change between 2020 and 2080 under three Shared Socioeconomic Pathway (SSP) scenarios.

The analysis considers both maternal and paternal grandparents, regardless of whether they live with their grandchildren. It also examines how grandparent availability and characteristics change across the life course of grandchildren.

**Methods and Data**

The analysis uses time-variant, two-sex kinship models implemented through the R package DemoKin. These models combine age-specific fertility and mortality schedules to estimate the expected number and age distribution of living grandparents for individuals of different ages.

**The main data sources are:**

UN World Population Prospects 2024 (WPP2024): Historical estimates and projections of age-specific fertility and surviaval probability.

Schoumaker (2019): Difference in mean age at childbearing between men and women used to approximate male age-specific fertility schedules from female fertility rates.

Wittgenstein Centre Human Capital Data Explorer (2023 revision): Population share by age, sex, and educational attainment under SSP1, SSP2, and SSP3 scenarios.


Grandparents' educational composition is estimated by applying age-, sex-, and period-specific educational distributions from population projections to the kinship estimates. The approach assumes that grandparents have the same educational distribution as the general population of the corresponding age and sex for given country and time.

**Scope of the Analysis**

The code supports the estimation and analysis of:

Historical and projected trends in the average number of living grandparents.

Differences in grandparent availability by sex and age group (under 60, 60–69, and 70+ years).

Changes in grandparents' educational composition under three SSP scenarios.

Cross-country differences in the demographic characteristics of grandparents.

Grandparent availability and educational composition across the life course of grandchildren.

**Reproducibility**

The repository is intended to facilitate the reproduction of the study's demographic estimates, projections, and figures. The analysis relies on publicly available data, with source information provided in the paper.
**Associated Paper**

Adhikari, S., & Alburez-Gutierrez, D. (2025). The Future of Grandparenthood in South Asia: The Role of Population Aging and Educational Expansion. MPIDR Working Paper WP-2025-003, revised January 2026.

https://doi.org/10.4054/MPIDR-WP-2025-003

**Authors**

Saroja Adhikari and Diego Alburez-Gutierrez

Max Planck Institute for Demographic Research, Rostock, Germany.
