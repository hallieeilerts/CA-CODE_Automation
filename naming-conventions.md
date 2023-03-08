## Naming conventions

Naming conventions result in improvements in terms of “four Cs”:
communication, code integration, consistency and clarity. The idea is
that “code should explain itself”.

All entities should be named following the format:
**class_objectINSTANCE_source**

``` r
df_class <- data.frame(eg = c("dth","p","db","mf","fm","res"),
                       egdesc = c("death counts", "proportions", "database", "model frame", "fitted model", "results"))
df_class$component <- "class"
df_class$desc <- "Indicates the data type. These prefixes intend to capture the distinguishing aspect of the entity, rather than the type of data in a strict sense."
df_class <- df_class[order(df_class$eg),]

df_obj <- data.frame(eg = c("pred","env","[cause of death]","[age/sex group]"),
                       egdesc = c("prediction", "envelope", "e.g., meas, tb, collectvio", "0to1, 1to59, 5to9, 10to14, 15to19f, 15to19m"))
df_obj <- df_obj[order(df_obj$eg),]
df_obj$component <- "object"
df_obj$desc <- "Describes the content of the entity."

df_tab <- rbind(df_class, df_obj)
df_tab <- df_tab[,c("component","desc","eg","egdesc")]
names(df_tab) <- c("Component", "Description",  "Example Values",   "Value Description")

kbl(df_tab, row.names = FALSE) %>%
  kable_paper(full_width = F) %>%
  column_spec(1, bold = T) %>%
  collapse_rows(columns = 2, valign = "middle")
```

<table class=" lightable-paper" style="font-family: &quot;Arial Narrow&quot;, arial, helvetica, sans-serif; width: auto !important; margin-left: auto; margin-right: auto;">
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
<td style="text-align:left;font-weight: bold;">
class
</td>
<td style="text-align:left;">
Indicates the data type. These prefixes intend to capture the
distinguishing aspect of the entity, rather than the type of data in a
strict sense.
</td>
<td style="text-align:left;">
db
</td>
<td style="text-align:left;">
database
</td>
</tr>
<tr>
<td style="text-align:left;font-weight: bold;">
class
</td>
<td style="text-align:left;">
Indicates the data type. These prefixes intend to capture the
distinguishing aspect of the entity, rather than the type of data in a
strict sense.
</td>
<td style="text-align:left;">
dth
</td>
<td style="text-align:left;">
death counts
</td>
</tr>
<tr>
<td style="text-align:left;font-weight: bold;">
class
</td>
<td style="text-align:left;">
Indicates the data type. These prefixes intend to capture the
distinguishing aspect of the entity, rather than the type of data in a
strict sense.
</td>
<td style="text-align:left;">
fm
</td>
<td style="text-align:left;">
fitted model
</td>
</tr>
<tr>
<td style="text-align:left;font-weight: bold;">
class
</td>
<td style="text-align:left;">
Indicates the data type. These prefixes intend to capture the
distinguishing aspect of the entity, rather than the type of data in a
strict sense.
</td>
<td style="text-align:left;">
mf
</td>
<td style="text-align:left;">
model frame
</td>
</tr>
<tr>
<td style="text-align:left;font-weight: bold;">
class
</td>
<td style="text-align:left;">
Indicates the data type. These prefixes intend to capture the
distinguishing aspect of the entity, rather than the type of data in a
strict sense.
</td>
<td style="text-align:left;">
p
</td>
<td style="text-align:left;">
proportions
</td>
</tr>
<tr>
<td style="text-align:left;font-weight: bold;">
class
</td>
<td style="text-align:left;">
Indicates the data type. These prefixes intend to capture the
distinguishing aspect of the entity, rather than the type of data in a
strict sense.
</td>
<td style="text-align:left;">
res
</td>
<td style="text-align:left;">
results
</td>
</tr>
<tr>
<td style="text-align:left;font-weight: bold;">
object
</td>
<td style="text-align:left;">
Describes the content of the entity.
</td>
<td style="text-align:left;">
\[age/sex group\]
</td>
<td style="text-align:left;">
0to1, 1to59, 5to9, 10to14, 15to19f, 15to19m
</td>
</tr>
<tr>
<td style="text-align:left;font-weight: bold;">
object
</td>
<td style="text-align:left;">
Describes the content of the entity.
</td>
<td style="text-align:left;">
\[cause of death\]
</td>
<td style="text-align:left;">
e.g., meas, tb, collectvio
</td>
</tr>
<tr>
<td style="text-align:left;font-weight: bold;">
object
</td>
<td style="text-align:left;">
Describes the content of the entity.
</td>
<td style="text-align:left;">
env
</td>
<td style="text-align:left;">
envelope
</td>
</tr>
<tr>
<td style="text-align:left;font-weight: bold;">
object
</td>
<td style="text-align:left;">
Describes the content of the entity.
</td>
<td style="text-align:left;">
pred
</td>
<td style="text-align:left;">
prediction
</td>
</tr>
</tbody>
</table>
