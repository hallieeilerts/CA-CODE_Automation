
# CA-CODE automation

Demo-ing automation for CA-CODE simple update to 2021.

## Developer instructions

-   Clone repository to computer
-   Copy and paste data inputs for desired update from CA-CODE_Warehouse folder on Dropbox into `/data`
    -   If necessary, replace code in `/src/data-management` with code from `/src/archive` to match the years of the update
-   Manually set variables in `/src/prepare-session/set-inputs`
    -   Do not make changes to any other scripts
-   Run make file
-   View results locally in `/gen/results/output` and `/gen/visualizations/output`


## Directory structure

This project framework was conceptualized using resources from the [Tilburg Science Hub](https://tilburgsciencehub.com/), in accordance with recommended workflow and data management principles for research projects.

### Source code

Source code is made available in the `src` folder, with sub-folders for each stage of the project pipeline. Source code are all files that are required to execute the project's pipeline. In addition, there is a `make.R` file in the main directory folder which makes explicit how the source code needs to be run. Code in the visualizations folder is to be run ad-hoc and is not included in the `make.R` file.

Our pipeline consists of seven stages:

-   data-management
-   estimation
-   prediction
-   squeezing
-   uncertainty
-   results
-   aggregation
-   visualizations

The directory structure for `/src` is thus:

    /src/data-management/
    /src/estimation/
    /src/prediction/
    /src/squeezing/
    /src/uncertainty/
    /src/results/
    /src/aggregation/
    /src/visualizations/

There is are two additional folders in `/src`:

-   `/src/archive` contains data-management source code from previous updates
-   `/src/prepare-session` is where the user manually sets the age group and years for estimation

### Generated files

Generated files are all files that are created by running the source code (`/src`) on the raw data (`/data`). They are stored in the `gen` folder. The `/gen` subdirectories match the pipeline stages.

Each subdirectory in `gen` contains the following subdirectories:

-   `input`: any required input files to run this step of the pipeline
-   `temp`: temporary files, such as an Excel dataset that needs to be converted into a CSV
-   `output`: stores the final result of the pipeline stage
-   `audit`: quality checks, diagnostic information on the performance of each step in the pipeline. For example, in `/data-management/audit` this could be a txt file with information on missing observations in the final dataset.

``` bash
├───gen
│   ├───agggregation
│   │   ├───audit
│   │   ├───input
│   │   ├───output
│   │   └───temp
│   ├───data-management
│   │   ├───audit
│   │   ├───input
│   │   ├───output
│   │   └───temp
│   ├───estimation
│   │   ├───audit
│   │   ├───input
│   │   ├───output
│   │   └───temp
│   ├───prediction
│   │   ├───audit
│   │   ├───input
│   │   ├───output
│   │   └───temp
│   ├───results
│   │   ├───audit
│   │   ├───input
│   │   ├───output
│   │   └───temp
│   └───squeezing
│   │   ├───audit
│   │   ├───input
│   │   ├───output
│   │   └───temp
│   └───uncertainty
│   │   ├───audit
│   │   ├───input
│   │   ├───output
│   │   └───temp
│   └───visualizations
│       ├───audit
│       ├───input
│       ├───output
│       └───temp
└───src
    ├───archive
        ├───data-management_2000-2021
    ├───data-management
    ├───estimation
    ├───prediction
    ├───results
    ├───squeezing
    ├───uncertainty
    └───visualizations
```

## Resources

[Modelling processes](https://docs.google.com/spreadsheets/d/1Yi904nUtTaoQu0HJcjPuPzmXdDtzOLPuktkfNjG4a2k/edit#gid=484739312)

[Dictionary and style guide](https://docs.google.com/spreadsheets/d/1g3oknz_RNwO5iuzxfyUoE4fl8oLL3Hj_u94alKk0OKo/edit#gid=219546148)

[Procurement timelines](https://docs.google.com/spreadsheets/d/1BnVdzqHqocNhnASHD5cCIbbq1Kds605Pd2lUwRhA0A4/edit#gid=0)
