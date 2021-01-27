# PFAS_Analysis_and_Intervention
PFAS Analysis and Intervention Shiny App


## Data Dictionary

We have one main table in our data warehouse that feeds our application. PFAS_Main, a combination of three tables - chemical, siteloc, storet - that originate from the waterboard's database (https://www.waterboards.ca.gov/drinking_water/certlic/drinkingwater/EDTlibrary.html). 

| Column Name | Description | Type | Specifications | Unique | Required | Key |
| --- | --- | --- | --- | --- | --- | --- |
| date | Date | DATE | YYYY-MM-DD | No | Yes | NA |
|  parameter | name of chemical/parameter | STRING | NA | No | Yes | NA |
| ResultValue | numerical result of analysis | FOAT | NA | No | Yes | NA |
| PRI_STA_C | primary station code or state source number | STRING | NA | Yes | Yes | Primary Key |
