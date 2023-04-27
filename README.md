# CA-CODE automation

Demo-ing automation for CA-CODE simple update to 2021.

## Process spreadsheet

[Spreadsheet](https://docs.google.com/spreadsheets/d/1Yi904nUtTaoQu0HJcjPuPzmXdDtzOLPuktkfNjG4a2k/edit#gid=484739312) for progress tracking, variable names, and process descriptions.

## Directory structure

This project framework was conceptualized using resources from the [Tilburg Science Hub](https://tilburgsciencehub.com/), in accordance with recommended workflow and data management principles for research projects.

### Source code

Source code is made available in the `src` folder, with sub-folders for each stage of the project pipeline. Source code are all files that are required to execute the project's pipeline. In addition, there is a makefile in the main directory folder which makes explicit how the source code needs to be run.

Our pipeline consists of five stages:

-   data-prep
-   estimation
-   prediction
-   squeezing
-   uncertainty

The directory structure for `/src` is thus:

    /src/data-prep/
    /src/estimation/
    /src/prediction/
    /src/squeezing/
    /src/uncertainty/

### Generated files

Generated files are all files that are generated by running the source code (`/src`) on the raw data (`/data`). They are stored in the `gen` folder. The `/gen` subdirectories match the pipeline stages.

Each subdirectory in `gen` contains the following subdirectories:

-   `input`: any required input files to run this step of the pipeline
-   `temp`: temporary files, such as an Excel dataset that needs to be converted into a CSV
-   `output`: stores the final result of the pipeline stage
-   `audit`: quality checks, diagnostic information on the performance of each step in the pipeline. For example, in `/data_preparation/audit` this could be a txt file with information on missing observations in the final dataset

``` bash
├───data
│   ├───china
│   ├───classification-keys
│   ├───good-vr
│   ├───igme
│   ├───prediction-database
│   ├───single-causes
│   └───study-database
├───docs
│   ├───diagram
│   └───workflow
├───gen
│   ├───data-prep
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
│   └───squeezing
│       ├───audit
│       ├───input
│       ├───output
│       └───temp
└───src
    ├───data-prep
    ├───estimation
    ├───prediction
    ├───squeezing
    └───uncertainty
```

## Developer instructions

-   Only adjust variables at top of make file, don't make changes to any other scripts
-   Add any required inputs manually to the `/gen/[subfolder]/input folder`
