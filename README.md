# RAP BUI 1 Tiered Analysis
Analysis workflow and data visualizations for Remedial Action Plan Beneficial Use Impairment 1 - Restrictions on Fish and Wildlife Consumption


## This project is in a BETA phase. I am currently working to add clear documentation and a master file to be usable immediately by users.


# Features


* Automatic generation of Tier 1 and Tier 2 analysis reports for the BUI 1 Assessment using parameterized R markdown report
* Render tables for Tier 1 (AOC advisory levels compared to restrictive threshold) and Tier 2 (AOC advisory levels compared to medians of suitable reference sites)
* Tables sorted by Species, Population (General/Sensitive), and size class
* Output list of species that pass/fail each tier
* Tier 2 table automatically excludes species that pass Tier 2 (optional)
* Configurable options for AOC, reference sites, threshold for restrictive meals/month, and species of interest
* Customizable tables including options for contaminant driver icons 





# Planned updates

* Full UI for configuration options
* Support for AOCs with multiple fishing zones (currently have to generate reports separately) with options for median, max, or lowest advisory values
* Custom functions to be packaged into a... package
* Ongoing bug fixes and efficiency upgrades
* Ongoing improvements to presentation
* Support for Shiny widget integration


# Usage
This workflow is designed for use in RStudio. You can either download this repository and open the project directly in RStudio (using the RProject file), or clone this repository and open the project in RStudio GIT.

This workflow utilizes the official [MECP Guide to Eating Ontario Fish](https://www.ontario.ca/page/guide-eating-ontario-fish) dataset to report on fish consumption advisories. This dataset is included in the `data` folder. 

The main usage file is the RMarkdown (.Rmd) file titled [Master Report.Rmd]. To generate a report, you can simply change the parameters in the header. The header (YAML) looks like this:

```
title: "Master BUI 1 Report" 

output:
  html_notebook
  
params:
  AOC: '45087425' 
  reference_sites: ['44007727',
  '43557712',
  '43497905',
  '43117922',
  '43397900',
  '43517847',
  '43527851',
  '44397535',
  '43557717',
  '43527856'] 
  restrict_threshold: 8 
  interest_species: NULL 
```

`Title` <- Change the title as you wish

`output`  <- html_notebook allows you to Preview the file as you work, you can also change to HTML or PDF

`AOC` <- This is where you put your AOC's waterbody ID. Crucially, this is from the waterbody_group column in the MECP dataset (some waterbodies have the same guide name, but always different IDs). By default this is the SLR AOC. There is a helpful lookup tool included to find a waterbody_group value given its guide name, more on that below.

`reference_sites` <- This is where you can add a list of Tier 2 reference sites. The format is [ID1, ID2, ...]. By default, this is all Lake Ontario non-AOC sites.

`restrict_threshold` <- Here you can modify your "restrictive" advisory threshold, which by default is set to 8 meals per month.

`interest_species` <- This is where you can specify a list of species of interest (format [Spec1, Spec2, ....]). By default this is set to NULL, which will return tables for all species with advisories in the AOC.


Once you change these variables to your liking, you can Run All code in the Markdown file, then knit to HTML or PDF. If you are using html_notebook as the output, you can also Preview the file as you work.

## `setup.R`
This file is a convenient and important tool to setup the project to use all of the included data and tools. By running `source(setup.R)`, the project will load all of the required packages, the MECP advisory dataset (which will be named `cons_data`), all of the custom functions, and other utilities. 

## Functions and helper tools
This repository uses a number of custom functions to generate the Tiered analysis outputs, so that they are portable, reusable, and configurable. At this time I have not created a convenient R package to bundle them all together, but the `setup.R` file will load them all automatically. All of the utility scripts can be found in the `R` folder. By running `source(setup.R)`, all of the necessary functions will be loaded for you to use. The first chunk in the Master Report runs `setup.R`. Below are some of the main functions you will be interfacing with to generate the table outputs. Detailed documentation for the "behind-the-scenes" functions is yet to come.

### get_waterbody_ids() and get_waterbody_names()
If you are not sure of your AOC or reference sites' ID values, you can use the included function `get_waterbody_ids()`. You can pass this function first the dataset (which in this project is by default named cons_data), then a waterbody name or list of waterbody names (in vector format,  `c("Name 1", "Name 2", etc...)`). Here is an example to find the St. Lawrence River AOC ID:

```
> get_waterbody_ids(cons_data, site_names = "St. Lawrence River (Lake St. Francis)")
[1] 45087425
```

There is also a reverse function to look up the site name from a given ID (`get_waterbody_names()`), which is used in the Master report Rmd file to populate table fields.

### make_restrict_table()
This function is necessary to create the Tier 1 and Tier 2 tables for display. Essentially, it creates a wide-format table ready to be passed to Reactable for display using the data from the MECP advisory table and the species from your AOC. It has three arguments:

`cons_data` <- Refers to the MECP dataset

`aoc_id` <- The `waterbody_group` value for your AOC/area of interest

`length_levels` <- A character vector containing the length levels. The reason this exists is that by default the table will sort the size categories alphabetically which messes up the table.

All three of these values are set by default to the objects defined in `setup.R` and by your report parameters. Therefore, calling `make_restrict_table()` in the report on its own should work. If you renamed any of those variables, you will have to specify them manually.


### extract_unique_adv_causes() and mappings()
These two functions create icons to represent the contaminant drivers of consumption advisories in the AOC. These are necessary to call in the current version to properly render the t1 table using `render_t1_table()`. They will extract the listed drivers of contaminants, give them shape and colour icons, and pass them along to your T1 table. At the current time, the function can only support up to 6 contaminant drivers, which should be fine for most fishing zones. They can be used as follows:

```
# Set contaminant mappings
unique_contaminants <- extract_unique_adv_causes(df) # Where "df" is your output from make_restrict_table()
mappings <- assign_contaminant_mappings(unique_contaminants)
contaminant_shapes <- mappings$shapes
contaminant_colours <- mappings$colours
```

These objects will be passed to the next function, `render_t1_table()`.


### render_t1_table()
This function generates the Tier 1 table using Reactable. It will automatically detect the drivers of contaminants for any particular advisory and match them with a shape and colour for visualization. This function relies on an input table created from `make_restrict_table()`. It uses these arguments:

`df` <- Your `make_restrict_table()` output

`length_levels` <- As above, defined by default from `setup.R`

`interest_species` <- A character string of your species of interest, defaulted to your report parameters

`generate_shape_fn` <- The function used to generate contaminant driver shapes, defaulted to `generate_shape`, from the `contaminant_icons.R`

`table_height` <- You can define the default height of your table, defaulted to 1500px

`show_legend` <- `TRUE`/`FALSE` to show a legend for the contaminant drivers. `TRUE` by default.

`contaminant_shapes` <- contaminant shape mappinsg from the `assign_contaminant_mappings()` function

`contaminant_colours` <- contaminant colour mappings from the `assign_contaminant_mappings()` function

As of 2025-06-03, I have not figured out a way to cleanly integrate the contaminant mappings directly into the function to allow the user to skip the step of defining them manually. Hopefully in a future update. Therefore, in the Master Report, the Tier 1 table is called as follows:

```
render_t1_table(
  df = t1_df,
  contaminant_shapes = contaminant_shapes,
  contaminant_colours = contaminant_colours,
  table_height = "1500px",
  show_legend = TRUE,
  interest_species = params$interest_species,
)
```

Where `contaminant_shapes` and `contaminant_colours` were defined as explained above, and `t1_df` being the output of `make_restrict_table()`.

### render_t2_table()
This function generates the Tier 2 table using Reactable. It will calculate the median value of your reference sites and compare them to your AOC advisory level. If a species/size class has 3 or fewer reference sites, the value will be coloured grey instead of red/green. This table also includes a dropdown to display the individual reference site values for each species, size class, and population type.

`cons__data` <- Defaults to the MECP advisory table

`aoc_id` <- Defaults to waterbody_group for AOC defined in parameters.

`length_levels` <- As above, defined by default from `setup.R`

`interest_species` <- A character string of your species of interest, defaulted to your report parameters

`reference_sites` <- A character vector of your reference site IDs, defaulted to the ones defined in parameters.

`table_height` <- You can define the default height of your table, defaulted to 1500px

`exclude_t1_passed` <- `TRUE`/`FALSE` This argument allows you to exclude species that passed Tier 1. `TRUE` by default.

Since this function's arguments all have reliable defaults, it can be called very simply with `render_t2_table()`. That's it!

### report_pass_fail_species()
This function will return a handy inline table to show whether each of your species of interest pass or fail one or both tiers.

`flag_df` <- A dataframe of pass/fail flags. I am actually not sure where this came from anymore, but setting this to `flags` somehow works.

`tier` <- Can be set to "t1", "t2", or "both". Will output species that pass respective tier

`output` <- Can be "list" or "table". Changes the output format.

In the master report:

```
report_pass_fail_species(flags, tier = "t1", output = "table")
```

```
report_pass_fail_species(flags, tier = "t2", output = "table")
```



# Index

## AOC ID values
### Below are the AOCs with their waterbody_group values and guide_locname_eng values in the MECP table.
| AOC | ID |
|-----|----|
|St. Lawrence River (Lake St. Francis)|45087425|
|Belleville Nearshore (Lake Ontario)|44087724|
|Trenton Nearshore (Lake Ontario)|44087724|
|Lake Ontario (Eastern Basin)|43457711|
|Hamilton Harbour (Lake Ontario)|43177950|
|Toronto Waterfront Area (Lake Ontario)|43397919|
|Niagara River (Upper reach)|42577857|
|Niagara River (Lower reach)|43077903|
|Detroit River (Upper reach)|42158307|
|Detroit River (Lower reach)|42048308|
|Thunder Bay (Lake Superior)|48228901|
|Nipigon Bay (Lake Superior)|48578814|
|Peninsula Harbour (Lake Superior)|48448625|
|St. Clair River|42498228|
|St. Marys River|46328421|
