
# CA-CODE automation

Demo-ing automation for CA-CODE simple update to 2021.

## Developer instructions

-   Clone repository to computer
-   Add data inputs from [CA-CODE_Warehouse](https://www.dropbox.com/scl/fo/iilncw5lay5cppoj9jg6n/h?rlkey=gcgqspqan03c1fup5ydlczl40&dl=0) folder on Dropbox to local `/data` folder
-   The current files in `/src/data-management` are for the Simple Update 2000-2021. If producing estimates for a different set of years, replace with appropriate `data-management` code in `/src/archive`.
-   Manually set variables in `/src/prepare-session/set-inputs`
    -   Do not make changes to any other scripts
-   Run make file
-   View results locally in `/gen/results/output` and `/gen/visualizations/output`

## Directory structure

This project framework was conceptualized using resources from the [Tilburg Science Hub](https://tilburgsciencehub.com/), in accordance with recommended workflow and data management principles for research projects.

### Source code

Source code is made available in the `src` folder, with sub-folders for each stage of the project pipeline. Source code contains all code that is required to execute the project's pipeline. There is a `make.R` file in the main directory folder which makes explicit how the source code needs to be run. 

Our pipeline consists of seven main stages:

-   `prepare-session`
-   `data-management`
-   `estimation`
-   `prediction`
-   `squeezing`
-   `uncertainty`
-   `results`

There are additional folders in `/src` which contain code not referenced in the `make.R` file. These folders are:

-   `adhoc-requests` : Code used to complete one-off requests that are not part of routine estimation process.
-   `aggregation` : Age/sex aggregation of estimates. Can only be run after results are generated for all age/sex groups.
-   `archive` : Contains `data-management` source code from previous update rounds.
-   `visualizations` : Code used to generate ad-hoc visualizations after producing results.

### Generated files

Generated files are all files that are created by running the source code (`/src`) on the raw data (`/data`). They are stored in the `gen` folder. The `/gen` subdirectories match the pipeline stages.

Each subdirectory in `gen` contains the following subdirectories:

-   `input`: any required input files to run this step of the pipeline
-   `temp`: temporary files, such as an Excel dataset that needs to be converted into a CSV
-   `output`: stores the final result of the pipeline stage
-   `audit`: quality checks, diagnostic information on the performance of each step in the pipeline. For example, in `/data-management/audit` this could be a txt file with information on missing observations in the final dataset.

## Resources

[Objective 1 timeline](https://docs.google.com/spreadsheets/d/1daewLt2dCeYvt5EB01fpR3cPNWxtdGlVVz8cpcNvm3M/edit#gid=1309778647)

[Data procurement for 2000-2023](https://docs.google.com/spreadsheets/d/1BnVdzqHqocNhnASHD5cCIbbq1Kds605Pd2lUwRhA0A4/edit#gid=0)

[Dictionary and code style guide](https://docs.google.com/spreadsheets/d/1g3oknz_RNwO5iuzxfyUoE4fl8oLL3Hj_u94alKk0OKo/edit#gid=219546148)

