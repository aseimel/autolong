# autolong
This function combines, aggregates, and harmonizes multiple datasets into a
single long format dataset. The user must provide a standardized csv table
containing all necessary coding information, which can be found in the
github repository (template.csv). 

**Features**
* automatically detect file format. Currently supports dta, sav, por, csv and sas7bdat files
* rescales to a standarized scale
* aggregates data by country and year using survey weights. It is also possible to only aggregate by country or year
* harmonizes country codes either using a custom link tables or the [countrycodes package](https://github.com/vincentarelbundock/countrycode)
* adds labels for the project and item
