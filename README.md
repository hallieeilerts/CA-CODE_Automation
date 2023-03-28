# CACODE-automation

Demo-ing automation for CA-CODE simple update to 2021

## Process spreadsheet

Add [hyperlink](https://docs.google.com/spreadsheets/d/1MZLZmxsd6w54V00ApEoiVen-qsgS_pDxbcjyYVoWDU0/edit?usp=sharing) to spreadsheet

## Folder structure

Source code is made available in the src folder. There are subdirectories for each stage of the pipeline. Source code are all files that are required to execute the project’s pipeline. In addition, source code consists of a makefile which makes explicit how the source code needs to be run, and in which order, and scripts which put the /gen/[pipeline-stage]/output files from the current pipeline stage to the file exchange (put_output), so that other pipeline stages can pull in that data to its /gen/[pipeline-stage]/input folder (get_input). In simple terms, a file exchange “mirrors” (parts of) your generated files in /gen, so that you or your team members can download the outputs of previous pipeline stages.

add to the readme this text. new branch


```bash
├───data
│   ├───china
│   ├───classification_keys
│   ├───goodVR
│   ├───igme
│   ├───prediction_database
│   ├───single_causes
│   └───study_database
├───docs
│   └───workflow
├───gen
│   ├───data_preparation
│   │   ├───audit
│   │   ├───input
│   │   ├───output
│   │   └───temp
│   ├───estimation
│   │   ├───audit
│   │   ├───input
│   │   ├───output
│   │   └───temp
│   └───prediction
│       ├───audit
│       ├───input
│       ├───output
│       └───temp
└───src
    ├───data_preparation
    ├───estimation
    └───prediction
```

## Developer instructions

* Only adjust variables at top of make file, don't make changes to any other scripts
* Add any required inputs manually to the gen/[subfolder]/input folder

