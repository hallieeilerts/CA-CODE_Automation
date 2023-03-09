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
<td style="text-align:left;vertical-align: middle !important;" rowspan="6">
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
<td style="text-align:left;">
death counts
</td>
</tr>
<tr>
<td style="text-align:left;">
fm
</td>
<td style="text-align:left;">
fitted model
</td>
</tr>
<tr>
<td style="text-align:left;">
mf
</td>
<td style="text-align:left;">
model frame
</td>
</tr>
<tr>
<td style="text-align:left;">
p
</td>
<td style="text-align:left;">
proportions
</td>
</tr>
<tr>
<td style="text-align:left;">
res
</td>
<td style="text-align:left;">
results
</td>
</tr>
<tr>
<td style="text-align:left;font-weight: bold;vertical-align: middle !important;" rowspan="4">
object
</td>
<td style="text-align:left;vertical-align: middle !important;" rowspan="4">
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
<td style="text-align:left;">
e.g., meas, tb, collectvio, etc.
</td>
</tr>
<tr>
<td style="text-align:left;">
env
</td>
<td style="text-align:left;">
envelope
</td>
</tr>
<tr>
<td style="text-align:left;">
pred
</td>
<td style="text-align:left;">
prediction
</td>
</tr>
<tr>
<td style="text-align:left;font-weight: bold;vertical-align: middle !important;" rowspan="5">
instance
</td>
<td style="text-align:left;vertical-align: middle !important;" rowspan="5">
Distinguishes different instances of the same object.
</td>
<td style="text-align:left;">
\[country\]
</td>
<td style="text-align:left;">
e.g., China, India
</td>
</tr>
<tr>
<td style="text-align:left;">
cf/ci
</td>
<td style="text-align:left;">
crisis-free/crisis-included
</td>
</tr>
<tr>
<td style="text-align:left;">
end/epi
</td>
<td style="text-align:left;">
endemic/epidemic
</td>
</tr>
<tr>
<td style="text-align:left;">
hmm/lmm
</td>
<td style="text-align:left;">
high/low mortality modelled
</td>
</tr>
<tr>
<td style="text-align:left;">
resp/nonr
</td>
<td style="text-align:left;">
respiratory/non-respiratory
</td>
</tr>
<tr>
<td style="text-align:left;font-weight: bold;vertical-align: middle !important;" rowspan="3">
source
</td>
<td style="text-align:left;vertical-align: middle !important;" rowspan="3">
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
<td style="text-align:left;">
World Population Prospects
</td>
</tr>
<tr>
<td style="text-align:left;">
wpp
</td>
<td style="text-align:left;">
World Health Organization
</td>
</tr>
</tbody>
</table>
