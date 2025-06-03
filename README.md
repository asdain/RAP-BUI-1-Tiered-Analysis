# RAP BUI 1 Tiered Analysis
Analysis workflow and data visualizations for Remedial Action Plan Beneficial Use Impairment 1 - Restrictions on Fish and Wildlife Consumption


## This project is in a BETA phase. I am currently working to add clear documentation and a master file to be usable immediately by users.


# Features

=======
* Automatic generation of Tier 1 and Tier 2 analysis reports for the BUI 1 Assessment using parameterized R markdown report
* Render tables for Tier 1 (AOC advisory levels compared to restrictive threshold) and Tier 2 (AOC advisory levels compared to medians of suitable reference sites)
* Tables sorted by Species, Population (General/Sensitive), and size class
* Output list of species that pass/fail each tier
* Tier 2 table automatically excludes species that pass Tier 2 (optional)
* Configurable options for AOC, reference sites, threshold for restrictive meals/month, and species of interest
* Customizable tables including options for contaminant driver icons 





# Planned updates

* Full UI for configuration options
* Support for AOCs with multiple fishing zones (currently have to generate reports separately)
* Custom functions to be packaged into a... package
* Ongoing bug fixes and efficiency upgrades


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

### render_t1_table()
This function generates the Tier 1 table using Reactable. It will automatically detect the drivers of contaminants for any particular advisory and match them with a shape and colour for visualization. This function relies on an input table created from `make_restrict_table()`. It uses these arguments:

`df` <- Your `make_restrict_table()` output

`length_levels` <- As above, defined by default from `setup.R`

`interest_species` <- A character string of your species of interest, defaulted to your report parameters

`generate_shape_fn` <- The function used to generate contaminant driver shapes, defaulted to `generate_shape`, from the `contaminant_icons.R`

`table_height` <- You can define the default height of your table, defaulted to 1500px

`show_legend` <- `TRUE`/`FALSE` to show a legend for the contaminant drivers. `TRUE` by default.

Since the only argument without default is `df`, you can generally call this function simply with `render_t1_table(df)`


### render_t2_table()
This function generates the Tier 2 table using Reactable. It will calculate the median value of your reference sites and compare them to your AOC advisory level. If a species/size class has 3 or fewer reference sites, the value will be coloured grey instead of red/green. This table also includes a dropdown to display the individual reference site values for each species, size class, and population type.

`cons__data` <- Defaults to the MECP advisory table

`aoc_id` <- Defaults to waterbody_group for AOC defined in parameters.

`length_levels` <- As above, defined by default from `setup.R`

`interest_species` <- A character string of your species of interest, defaulted to your report parameters

`reference_sites` <- A character vector of your reference site IDs, defaulted to the ones defined in parameters.

`table_height` <- You can define the default height of your table, defaulted to 1500px

`exclude_t1_passed` <- `TRUE`/`FALSE` This argument allows you to exclude species that passed Tier 1. `TRUE` by default.
