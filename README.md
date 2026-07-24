# Health Facility MNH Innovations - Nigeria

Data monitoring and analysis for Health Facility MNH Innovations in Nigeria

## Directory structure

`make.R` : calls scripts in correct order to replicate analysis

- `./data`: location of data
- `./src`: location of code, organized by domain (e.g., `set-up`, `monitoring`, `indicators`)
- `./gen`: location of files generated from code, mirroring the structure of `./src`

```
project/
├── make.R
├── data/
│ └── [files pulled from API]
├── src/
│ ├── set-up/
│ │ ├── pull-data.R
│ ├── monitoring/
│ │ ├── helper-functions.R
│ │ ├── monitoring-table-toolA.Rmd
│ │ ├── monitoring-table-toolB.Rmd
│ │ ├── monitoring-report-toolA.Rmd
│ │ └── monitoring-report-toolB.Rmd
│ ├── indicators/
│ │ └── ...
│ └── ...
├── gen/
│ ├── monitoring/
│ │ ├── reports/
│ │ │ ├── archive/
│ │ │ ├── monitoring-report-toolA_latest.html
│ │ │ └── monitoring-report-toolB_latest.html
│ │ └── tables/
│ │ │ ├── archive/
│ │ │ ├── data-queries-toolA_YYYYMMDD.xlsx
│ │ │ ├── data-queries-toolB_YYYYMMDD.xlsx
│ │ │ ├── monitoring-table-toolA_latest.docx
│ │ │ └── monitoring-table-toolB_latest.docx
│ └── indicators/
│ └── ...
```