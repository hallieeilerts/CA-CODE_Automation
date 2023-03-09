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

<table class=" lightable-paper" style="font-size: 10px; font-family: &quot;Arial Narrow&quot;, arial, helvetica, sans-serif; width: auto !important; margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;">
Component
</th>
<th style="text-align:left;">
Description
</th>
<th style="text-align:left;">
Example Values
</th>
<th style="text-align:left;">
Value Description
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;font-weight: bold;vertical-align: middle !important;" rowspan="6">
class
</td>
<td style="text-align:left;vertical-align: middle !important;width: 30em; background-color: yellow !important;" rowspan="6">
Type of information contained in the object.
</td>
<td style="text-align:left;">
db
</td>
<td style="text-align:left;">
database
</td>
</tr>
<tr>
<td style="text-align:left;">
dth
</td>
<td style="text-align:left;width: 30em; background-color: yellow !important;">
death counts
</td>
</tr>
<tr>
<td style="text-align:left;">
fm
</td>
<td style="text-align:left;width: 30em; background-color: yellow !important;">
fitted model
</td>
</tr>
<tr>
<td style="text-align:left;">
mf
</td>
<td style="text-align:left;width: 30em; background-color: yellow !important;">
model frame
</td>
</tr>
<tr>
<td style="text-align:left;">
p
</td>
<td style="text-align:left;width: 30em; background-color: yellow !important;">
proportions
</td>
</tr>
<tr>
<td style="text-align:left;">
res
</td>
<td style="text-align:left;width: 30em; background-color: yellow !important;">
results
</td>
</tr>
<tr>
<td style="text-align:left;font-weight: bold;vertical-align: middle !important;" rowspan="4">
object
</td>
<td style="text-align:left;vertical-align: middle !important;width: 30em; background-color: yellow !important;" rowspan="4">
Name of object.
</td>
<td style="text-align:left;">
\[age/sex group\]
</td>
<td style="text-align:left;">
0to1, 1to59, 5to9, 10to14, 15to19f, 15to19m
</td>
</tr>
<tr>
<td style="text-align:left;">
\[cause of death\]
</td>
<td style="text-align:left;width: 30em; background-color: yellow !important;">
e.g., meas, tb, collectvio, etc.
</td>
</tr>
<tr>
<td style="text-align:left;">
env
</td>
<td style="text-align:left;width: 30em; background-color: yellow !important;">
envelope
</td>
</tr>
<tr>
<td style="text-align:left;">
pred
</td>
<td style="text-align:left;width: 30em; background-color: yellow !important;">
prediction
</td>
</tr>
<tr>
<td style="text-align:left;font-weight: bold;vertical-align: middle !important;" rowspan="5">
instance
</td>
<td style="text-align:left;vertical-align: middle !important;width: 30em; background-color: yellow !important;" rowspan="5">
Distinguishes different instances of the same object.
</td>
<td style="text-align:left;">
<country> </country>
</td>
<td style="text-align:left;">
e.g., China, India
</td>
</tr>
<tr>
<td style="text-align:left;">
cf/ci
</td>
<td style="text-align:left;width: 30em; background-color: yellow !important;">
crisis-free/crisis-included
</td>
</tr>
<tr>
<td style="text-align:left;">
end/epi
</td>
<td style="text-align:left;width: 30em; background-color: yellow !important;">
endemic/epidemic
</td>
</tr>
<tr>
<td style="text-align:left;">
hmm/lmm
</td>
<td style="text-align:left;width: 30em; background-color: yellow !important;">
high/low mortality modelled
</td>
</tr>
<tr>
<td style="text-align:left;">
resp/nonr
</td>
<td style="text-align:left;width: 30em; background-color: yellow !important;">
respiratory/non-respiratory
</td>
</tr>
<tr>
<td style="text-align:left;font-weight: bold;vertical-align: middle !important;" rowspan="3">
source
</td>
<td style="text-align:left;vertical-align: middle !important;width: 30em; background-color: yellow !important;" rowspan="3">
Indicates the source of the entity, if external. Written in all
uppercase. Paired instances should have the same number of letters.
</td>
<td style="text-align:left;">
igme
</td>
<td style="text-align:left;">
UN Inter-agency Group of Mortality Estimation
</td>
</tr>
<tr>
<td style="text-align:left;">
who
</td>
<td style="text-align:left;width: 30em; background-color: yellow !important;">
World Population Prospects
</td>
</tr>
<tr>
<td style="text-align:left;">
wpp
</td>
<td style="text-align:left;width: 30em; background-color: yellow !important;">
World Health Organization
</td>
</tr>
</tbody>
</table>

``` r
as.tibble(df_tab) %>%
  gt() %>%
  fmt_markdown(columns = everything()) %>%
  tab_options(table.width = px(400))
```

    ## Warning: `as.tibble()` was deprecated in tibble 2.0.0.
    ## ℹ Please use `as_tibble()` instead.
    ## ℹ The signature and semantics have changed, see `?as_tibble`.

<div id="lapgagalhh" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#lapgagalhh .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: 400px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#lapgagalhh .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#lapgagalhh .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#lapgagalhh .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#lapgagalhh .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#lapgagalhh .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#lapgagalhh .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#lapgagalhh .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#lapgagalhh .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#lapgagalhh .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#lapgagalhh .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#lapgagalhh .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#lapgagalhh .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}

#lapgagalhh .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#lapgagalhh .gt_from_md > :first-child {
  margin-top: 0;
}

#lapgagalhh .gt_from_md > :last-child {
  margin-bottom: 0;
}

#lapgagalhh .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#lapgagalhh .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#lapgagalhh .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#lapgagalhh .gt_row_group_first td {
  border-top-width: 2px;
}

#lapgagalhh .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#lapgagalhh .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#lapgagalhh .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#lapgagalhh .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#lapgagalhh .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#lapgagalhh .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#lapgagalhh .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#lapgagalhh .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#lapgagalhh .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#lapgagalhh .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-left: 4px;
  padding-right: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#lapgagalhh .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#lapgagalhh .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#lapgagalhh .gt_left {
  text-align: left;
}

#lapgagalhh .gt_center {
  text-align: center;
}

#lapgagalhh .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#lapgagalhh .gt_font_normal {
  font-weight: normal;
}

#lapgagalhh .gt_font_bold {
  font-weight: bold;
}

#lapgagalhh .gt_font_italic {
  font-style: italic;
}

#lapgagalhh .gt_super {
  font-size: 65%;
}

#lapgagalhh .gt_footnote_marks {
  font-style: italic;
  font-weight: normal;
  font-size: 75%;
  vertical-align: 0.4em;
}

#lapgagalhh .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#lapgagalhh .gt_indent_1 {
  text-indent: 5px;
}

#lapgagalhh .gt_indent_2 {
  text-indent: 10px;
}

#lapgagalhh .gt_indent_3 {
  text-indent: 15px;
}

#lapgagalhh .gt_indent_4 {
  text-indent: 20px;
}

#lapgagalhh .gt_indent_5 {
  text-indent: 25px;
}
</style>
<table class="gt_table">
  
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="Component">Component</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="Description">Description</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="Example Values">Example Values</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="Value Description">Value Description</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="Component" class="gt_row gt_left"><div class='gt_from_md'><p>class</p>
</div></td>
<td headers="Description" class="gt_row gt_left"><div class='gt_from_md'><p>Type of information contained in the object.</p>
</div></td>
<td headers="Example Values" class="gt_row gt_left"><div class='gt_from_md'><p>db</p>
</div></td>
<td headers="Value Description" class="gt_row gt_left"><div class='gt_from_md'><p>database</p>
</div></td></tr>
    <tr><td headers="Component" class="gt_row gt_left"><div class='gt_from_md'><p>class</p>
</div></td>
<td headers="Description" class="gt_row gt_left"><div class='gt_from_md'><p>Type of information contained in the object.</p>
</div></td>
<td headers="Example Values" class="gt_row gt_left"><div class='gt_from_md'><p>dth</p>
</div></td>
<td headers="Value Description" class="gt_row gt_left"><div class='gt_from_md'><p>death counts</p>
</div></td></tr>
    <tr><td headers="Component" class="gt_row gt_left"><div class='gt_from_md'><p>class</p>
</div></td>
<td headers="Description" class="gt_row gt_left"><div class='gt_from_md'><p>Type of information contained in the object.</p>
</div></td>
<td headers="Example Values" class="gt_row gt_left"><div class='gt_from_md'><p>fm</p>
</div></td>
<td headers="Value Description" class="gt_row gt_left"><div class='gt_from_md'><p>fitted model</p>
</div></td></tr>
    <tr><td headers="Component" class="gt_row gt_left"><div class='gt_from_md'><p>class</p>
</div></td>
<td headers="Description" class="gt_row gt_left"><div class='gt_from_md'><p>Type of information contained in the object.</p>
</div></td>
<td headers="Example Values" class="gt_row gt_left"><div class='gt_from_md'><p>mf</p>
</div></td>
<td headers="Value Description" class="gt_row gt_left"><div class='gt_from_md'><p>model frame</p>
</div></td></tr>
    <tr><td headers="Component" class="gt_row gt_left"><div class='gt_from_md'><p>class</p>
</div></td>
<td headers="Description" class="gt_row gt_left"><div class='gt_from_md'><p>Type of information contained in the object.</p>
</div></td>
<td headers="Example Values" class="gt_row gt_left"><div class='gt_from_md'><p>p</p>
</div></td>
<td headers="Value Description" class="gt_row gt_left"><div class='gt_from_md'><p>proportions</p>
</div></td></tr>
    <tr><td headers="Component" class="gt_row gt_left"><div class='gt_from_md'><p>class</p>
</div></td>
<td headers="Description" class="gt_row gt_left"><div class='gt_from_md'><p>Type of information contained in the object.</p>
</div></td>
<td headers="Example Values" class="gt_row gt_left"><div class='gt_from_md'><p>res</p>
</div></td>
<td headers="Value Description" class="gt_row gt_left"><div class='gt_from_md'><p>results</p>
</div></td></tr>
    <tr><td headers="Component" class="gt_row gt_left"><div class='gt_from_md'><p>object</p>
</div></td>
<td headers="Description" class="gt_row gt_left"><div class='gt_from_md'><p>Name of object.</p>
</div></td>
<td headers="Example Values" class="gt_row gt_left"><div class='gt_from_md'><p>[age/sex group]</p>
</div></td>
<td headers="Value Description" class="gt_row gt_left"><div class='gt_from_md'><p>0to1, 1to59, 5to9, 10to14, 15to19f, 15to19m</p>
</div></td></tr>
    <tr><td headers="Component" class="gt_row gt_left"><div class='gt_from_md'><p>object</p>
</div></td>
<td headers="Description" class="gt_row gt_left"><div class='gt_from_md'><p>Name of object.</p>
</div></td>
<td headers="Example Values" class="gt_row gt_left"><div class='gt_from_md'><p>[cause of death]</p>
</div></td>
<td headers="Value Description" class="gt_row gt_left"><div class='gt_from_md'><p>e.g., meas, tb, collectvio, etc.</p>
</div></td></tr>
    <tr><td headers="Component" class="gt_row gt_left"><div class='gt_from_md'><p>object</p>
</div></td>
<td headers="Description" class="gt_row gt_left"><div class='gt_from_md'><p>Name of object.</p>
</div></td>
<td headers="Example Values" class="gt_row gt_left"><div class='gt_from_md'><p>env</p>
</div></td>
<td headers="Value Description" class="gt_row gt_left"><div class='gt_from_md'><p>envelope</p>
</div></td></tr>
    <tr><td headers="Component" class="gt_row gt_left"><div class='gt_from_md'><p>object</p>
</div></td>
<td headers="Description" class="gt_row gt_left"><div class='gt_from_md'><p>Name of object.</p>
</div></td>
<td headers="Example Values" class="gt_row gt_left"><div class='gt_from_md'><p>pred</p>
</div></td>
<td headers="Value Description" class="gt_row gt_left"><div class='gt_from_md'><p>prediction</p>
</div></td></tr>
    <tr><td headers="Component" class="gt_row gt_left"><div class='gt_from_md'><p>instance</p>
</div></td>
<td headers="Description" class="gt_row gt_left"><div class='gt_from_md'><p>Distinguishes different instances of the same object.</p>
</div></td>
<td headers="Example Values" class="gt_row gt_left"><div class='gt_from_md'><country>
</div></td>
<td headers="Value Description" class="gt_row gt_left"><div class='gt_from_md'><p>e.g., China, India</p>
</div></td></tr>
    <tr><td headers="Component" class="gt_row gt_left"><div class='gt_from_md'><p>instance</p>
</div></td>
<td headers="Description" class="gt_row gt_left"><div class='gt_from_md'><p>Distinguishes different instances of the same object.</p>
</div></td>
<td headers="Example Values" class="gt_row gt_left"><div class='gt_from_md'><p>cf/ci</p>
</div></td>
<td headers="Value Description" class="gt_row gt_left"><div class='gt_from_md'><p>crisis-free/crisis-included</p>
</div></td></tr>
    <tr><td headers="Component" class="gt_row gt_left"><div class='gt_from_md'><p>instance</p>
</div></td>
<td headers="Description" class="gt_row gt_left"><div class='gt_from_md'><p>Distinguishes different instances of the same object.</p>
</div></td>
<td headers="Example Values" class="gt_row gt_left"><div class='gt_from_md'><p>end/epi</p>
</div></td>
<td headers="Value Description" class="gt_row gt_left"><div class='gt_from_md'><p>endemic/epidemic</p>
</div></td></tr>
    <tr><td headers="Component" class="gt_row gt_left"><div class='gt_from_md'><p>instance</p>
</div></td>
<td headers="Description" class="gt_row gt_left"><div class='gt_from_md'><p>Distinguishes different instances of the same object.</p>
</div></td>
<td headers="Example Values" class="gt_row gt_left"><div class='gt_from_md'><p>hmm/lmm</p>
</div></td>
<td headers="Value Description" class="gt_row gt_left"><div class='gt_from_md'><p>high/low mortality modelled</p>
</div></td></tr>
    <tr><td headers="Component" class="gt_row gt_left"><div class='gt_from_md'><p>instance</p>
</div></td>
<td headers="Description" class="gt_row gt_left"><div class='gt_from_md'><p>Distinguishes different instances of the same object.</p>
</div></td>
<td headers="Example Values" class="gt_row gt_left"><div class='gt_from_md'><p>resp/nonr</p>
</div></td>
<td headers="Value Description" class="gt_row gt_left"><div class='gt_from_md'><p>respiratory/non-respiratory</p>
</div></td></tr>
    <tr><td headers="Component" class="gt_row gt_left"><div class='gt_from_md'><p>source</p>
</div></td>
<td headers="Description" class="gt_row gt_left"><div class='gt_from_md'><p>Indicates the source of the entity, if external. Written in all uppercase. Paired instances should have the same number of letters.</p>
</div></td>
<td headers="Example Values" class="gt_row gt_left"><div class='gt_from_md'><p>igme</p>
</div></td>
<td headers="Value Description" class="gt_row gt_left"><div class='gt_from_md'><p>UN Inter-agency Group of Mortality Estimation</p>
</div></td></tr>
    <tr><td headers="Component" class="gt_row gt_left"><div class='gt_from_md'><p>source</p>
</div></td>
<td headers="Description" class="gt_row gt_left"><div class='gt_from_md'><p>Indicates the source of the entity, if external. Written in all uppercase. Paired instances should have the same number of letters.</p>
</div></td>
<td headers="Example Values" class="gt_row gt_left"><div class='gt_from_md'><p>who</p>
</div></td>
<td headers="Value Description" class="gt_row gt_left"><div class='gt_from_md'><p>World Population Prospects</p>
</div></td></tr>
    <tr><td headers="Component" class="gt_row gt_left"><div class='gt_from_md'><p>source</p>
</div></td>
<td headers="Description" class="gt_row gt_left"><div class='gt_from_md'><p>Indicates the source of the entity, if external. Written in all uppercase. Paired instances should have the same number of letters.</p>
</div></td>
<td headers="Example Values" class="gt_row gt_left"><div class='gt_from_md'><p>wpp</p>
</div></td>
<td headers="Value Description" class="gt_row gt_left"><div class='gt_from_md'><p>World Health Organization</p>
</div></td></tr>
  </tbody>
  
  
</table>
</div>
