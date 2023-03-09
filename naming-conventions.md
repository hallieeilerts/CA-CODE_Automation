## Naming conventions

This file details the naming conventions used in the master workflow
document which details all modelling processes from start to finish.

Naming conventions result in improvements in terms of communication,
code integration, consistency and clarity. While many of the names below
were gleaned from the existing code, they do not match the code exactly.
If/when we try to standardize naming conventions across all code, the
notation system described here could be a starting point.

All entities should be named following the format:
**class_objectINSTANCE_source**

    ## Warning in kable_styling(kable_input, "none", htmltable_class = light_class, :
    ## Please specify format in kable. kableExtra can customize either HTML or LaTeX
    ## outputs. See https://haozhu233.github.io/kableExtra/ for details.

    ## Warning in column_spec(., 1, bold = T): Please specify format in kable.
    ## kableExtra can customize either HTML or LaTeX outputs. See
    ## https://haozhu233.github.io/kableExtra/ for details.

    ## Warning in collapse_rows(., columns = 1:2, valign = "middle"): Please specify
    ## format in kable. kableExtra can customize either HTML or LaTeX outputs. See
    ## https://haozhu233.github.io/kableExtra/ for details.

    ## Warning in column_spec(., 1, bold = T, border_right = T): Please specify format
    ## in kable. kableExtra can customize either HTML or LaTeX outputs. See
    ## https://haozhu233.github.io/kableExtra/ for details.

    ## Warning in column_spec(., 2, width = "30em", background = "yellow"): Please
    ## specify format in kable. kableExtra can customize either HTML or LaTeX outputs.
    ## See https://haozhu233.github.io/kableExtra/ for details.

| Component | Description                                                                                                                         | Example Values     | Value Description                             |
|:----|:--------------------------------------------|:------|:----------------|
| class     | Type of information contained in the object.                                                                                        | db                 | database                                      |
| class     | Type of information contained in the object.                                                                                        | dth                | death counts                                  |
| class     | Type of information contained in the object.                                                                                        | fm                 | fitted model                                  |
| class     | Type of information contained in the object.                                                                                        | mf                 | model frame                                   |
| class     | Type of information contained in the object.                                                                                        | p                  | proportions                                   |
| class     | Type of information contained in the object.                                                                                        | res                | results                                       |
| object    | Name of object.                                                                                                                     | \[age/sex group\]  | 0to1, 1to59, 5to9, 10to14, 15to19f, 15to19m   |
| object    | Name of object.                                                                                                                     | \[cause of death\] | e.g., meas, tb, collectvio, etc.              |
| object    | Name of object.                                                                                                                     | env                | envelope                                      |
| object    | Name of object.                                                                                                                     | pred               | prediction                                    |
| instance  | Distinguishes different instances of the same object.                                                                               | <country>          | e.g., China, India                            |
| instance  | Distinguishes different instances of the same object.                                                                               | cf/ci              | crisis-free/crisis-included                   |
| instance  | Distinguishes different instances of the same object.                                                                               | end/epi            | endemic/epidemic                              |
| instance  | Distinguishes different instances of the same object.                                                                               | hmm/lmm            | high/low mortality modelled                   |
| instance  | Distinguishes different instances of the same object.                                                                               | resp/nonr          | respiratory/non-respiratory                   |
| source    | Indicates the source of the entity, if external. Written in all uppercase. Paired instances should have the same number of letters. | igme               | UN Inter-agency Group of Mortality Estimation |
| source    | Indicates the source of the entity, if external. Written in all uppercase. Paired instances should have the same number of letters. | who                | World Population Prospects                    |
| source    | Indicates the source of the entity, if external. Written in all uppercase. Paired instances should have the same number of letters. | wpp                | World Health Organization                     |
