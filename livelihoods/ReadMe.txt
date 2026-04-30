
Western Wildfire Resilience Index (WWRI) - Livelihoods Domain
============================================================

Overview:
---------
The Livelihoods Domain of the Western Wildfire Resilience Index (WWRI) is designed to measure both the current status of the livelihoods and the resilience of those livelihoods to a wildfire event within an administrative area.

Model:
------
The model includes a calculation of Status and Resilience for livelihoods within the administrative boundaries of the US and Canada.

Status:
-------
The Status metric is an unweighted average of three indicators: unemployment rate, housing burden, and median income. These indicators are rescaled to a range between 0 and 1 at the country level, reflecting differences in data collection and economic conditions across regions.

  - Unemployment:
    Rescaled based on the unemployment rate, which is the number of unemployed persons divided by the labor force. Data sources vary by country:
    * US data from the American Community Survey via the R Census API.
    * Canada data from Statistics Canada.

  - Median Income:
    Rescaled based on the median income level of households or individuals within each area.
    * US households: Data from the American Community Survey.
    * Canada individuals: Data from Statistics Canada.

  - Housing Burden:
    Rescaled based on the percentage of the population that spends 30% or more of their income on housing.
    * US data includes rent-burdened housing units as a proportion of total housing units.
    * Canada data reflects the proportion of households paying 30% or more on housing.

Resilience:
-----------
Resilience is calculated using the formula: 1 - (1 - Resistance) * (1 - Recovery)
  - Resistance measures the proportion of jobs highly impacted by wildfires, rescaled between 0 and 1.
  - Recovery is based on the Shannon diversity index of job types within each area, rescaled between 0 and 1.

Data Table:
-----------
The following table provides detailed sources and data specifics used for the calculations:

| Title                          | Score  | Source                        | Reference                                               | Resolution            | Frequency   | Updated      |
|--------------------------------|--------|-------------------------------|---------------------------------------------------------|-----------------------|-------------|--------------|
| Canadian Median Income         | Status | Statistics Canada             | Table 98-10-0070-01 Income statistics                   | Census Subdivision    | Occasional  | 2022-07-01   |
| Canada Unemployment            | Status | Statistics Canada             | Table 98-10-0485-01 Labour statistics                   | Census Subdivision    | Occasional  | 2023-11-01   |
| Canada Housing Burdened        | Status | Statistics Canada             | Table 98-10-0243-01 Housing statistics                  | Census Subdivision    | Occasional  | 2023-10-01   |
| US Median Income: B19013_001E  | Status | American Community Survey     | Census Data API /data/2019/acs/acs5/groups/B19013_001E  | Census Tract          | Annual      | [Last Update]|

Reporting Areas:
----------------
Data is reported at the census tract level for the US and the census subdivision level for Canada.
