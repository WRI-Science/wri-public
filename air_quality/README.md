# Air Domain Data Sources

## Documenting where data is stored and data sources 
- All data files used are listed here and stored on wwri-wildfire/data/air-quality on CyberDuck

## Asthma Prevalence
- Canada percent prevalence of asthma among adults aged >=12 from StatsCanada - https://www150.statcan.gc.ca/t1/tbl1/en/cv.action?pid=1310011301
- US crude percent prevalence of current asthma among adults aged >=18 csv from CDC website - https://ephtracking.cdc.gov/DataExplorer/?c=3&i=90&m=-1

## Prevalence of Chronic Obstructive Pulmonary Disease (COPD)
- US 2021 CDC data - https://ephtracking.cdc.gov/DataExplorer/?c=3&i=90&m=-1
- Canada 2019/2020: Health characteristics, two-year period estimates - https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=1310011301

## Farmworkers in the US
- US data obtained at county level from the NCFH dashboard for Total Farm Workers (2017) and H-2A Temporary Workers (2023) - https://ncfh.shinyapps.io/farmlabor_dashboard/?code=XfPgO3uyMa7VSR6ZqQ7KVQRLr5gRjeV9pusEEMObSF8hs&state=TBojGXtvf9

## Hospitals / Urgent & Primary Care Centers / Walk in Centers 
- US Hospitals shapefile - https://hifld-geoplatform.hub.arcgis.com/search?groupIds=2900322cc0b14948a74dca886b7d7cfc
- US Urgent Care Facilities - https://hifld-geoplatform.hub.arcgis.com/search?groupIds=2900322cc0b14948a74dca886b7d7cfc
- BC Hospital shapefiles - https://catalogue.data.gov.bc.ca/dataset/hospitals-in-bc
- BC Walk in clinics - https://catalogue.data.gov.bc.ca/dataset/walk-in-clinics-in-bc
- BC Urgent & Primary Care Centers - https://catalogue.data.gov.bc.ca/dataset/urgent-and-primary-care-centres
- BC Emergency Rooms - https://catalogue.data.gov.bc.ca/dataset/emergency-rooms-in-bc
- YK hospital shapefiles - https://open.yukon.ca/data/datasets/health-care-facilities-50k
- (Archived bc no geometry) US hospital csv file - CDC under Community Characteristics -> Medical Infrastructure https://ephtracking.cdc.gov/DataExplorer/?c=3&i=90&m=-1

- BC Sites Registry (didn't use, but good to point check locations as it is a comprehensive database of all BC public institutions) - https://catalogue.data.gov.bc.ca/dataset/sites-registry-open-government-licence-

## NAICS Codes: Jobs exposed to poor air quality
- Canada 2021 Census data - https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=9810059201
- US 2021 data pulled through the getCensus API

## Vulnerable Populations (children and elderly)
- US ACS 5 year tract level 2022-2017 from tidycensus API
- BC & YT - CensusSubdivisions BC and YT:  https://www12.statcan.gc.ca/census-recensement/2021/dp-pd/prof/details/download-telecharger.cfm?Lang=E

## AQI Data
- US EPA Daily AQI by County .csv files - https://aqs.epa.gov/aqsweb/airdata/download_files.html#AQI
- BC & YT - .csv files obtained from emailing dmc@airnowtech.org

---------------- Archived -----------------------
### Public Schools and Libraries 
- BC K-12 Schools with Francophone Indicators - https://catalogue.data.gov.bc.ca/dataset/bc-schools-k-12-with-francophone-indicators 
- BC Public Libraries Branches & Locations - https://catalogue.data.gov.bc.ca/dataset/bc-public-libraries-systems-branches-and-locations
- YT Education Facilities - https://yukon.maps.arcgis.com/home/item.html?id=d619fae8776f4b8ea61d930cb80580cb#overview
- YT Public Libraries List (csv location coordinates in cyberduck) - https://yukon.ca/en/arts-and-culture/libraries-and-archives/find-library
- BC Sites Registry - https://catalogue.data.gov.bc.ca/dataset/sites-registry-open-government-licence-
- US Public School Characteristics 2021-22 - https://data-nces.opendata.arcgis.com/datasets/b480c30aa8654d23be9b79a0feb436e3_0/explore?location=33.611427%2C-96.401189%2C4.63
- US Public Library Locations - https://www.imls.gov/research-evaluation/data-collection/public-libraries-survey

### Count data per 100,000
- BC Practitioners/Specialists/Supp.Practitioners per 100,000 - http://communityhealth.phsa.ca/GetTheData/SearchByTopic
- US Hospitals/Pharmacies/Pharmacists per 100,000 - https://ephtracking.cdc.gov/DataExplorer/?c=3&i=90&m=-1

## PPE
- Canada PPE Demand and Supply - https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=1310078601

