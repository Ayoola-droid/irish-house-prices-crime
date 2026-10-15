# Data

Most raw and intermediate files are far larger than GitHub allows (the property and crime-buffer tables are about 300 MB each), so this folder holds small inputs and a sample.

## Included

| File | What it is | Source |
| --- | --- | --- |
| `geo_property_sample_5000.csv` | Random 5,000 geocoded, validated sales with location features | Property Price Register + my geocoding |
| `geo_crime.csv` | Garda station crime counts by year, geocoded | CSO + my geocoding |
| `airport.geojson`, `train station.geojson`, `Built_Up_Areas.geojson` | Amenity locations | Tailte Éireann (OSi) open data |
| `com_airport.csv`, `citycenter.csv`, `pub.csv` | Commercial airports, town centres, hospitals | Compiled manually; hospitals from HSE |
| `retail intrest rates.csv` | Mortgage interest rates | Central Bank of Ireland |
| `scsi_index.csv` | Construction tender price index | SCSI |
| `county_size.csv` | County areas | OSi |

## Not included: download from source

| Data | Source |
| --- | --- |
| Property Price Register (all sales) | https://www.propertypriceregister.ie/ |
| Recorded crime by Garda station | https://data.cso.ie/ |
| Construction cost index | https://data.gov.ie/dataset/national-house-construction-cost-index |
| Mortgage rates | https://opendata.centralbank.ie/ |
| Housing stock, population, new house registrations | CSO tables F2020, F1001, HSQ10 |
| County boundaries, built-up areas, train stations, airports | https://data-osi.opendata.arcgis.com/ |
| OpenStreetMap Ireland extract | https://download.geofabrik.de/europe/ireland-and-northern-ireland.html |

Public-sector data is used under the CC BY 4.0 licence. Property Price Register data is © Property Services Regulatory Authority.
