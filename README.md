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

### `setup.R`
This file is a convenient and important tool to setup the project to use all of the included data and tools. By running `source(setup.R)`, the project will load all of the required packages, the MECP advisory dataset (which will be named `cons_data`), all of the custom functions, and other utilities. 

## Functions and helper tools
This repository uses a number of custom functions to generate the Tiered analysis outputs, so that they are portable, reusable, and configurable. At this time I have not created a convenient R package to bundle them all together, but the `setup.R` file will load them all automatically. All of the utility scripts can be found in the `R` folder. By running `source(setup.R)`, all of the necessary functions will be loaded for you to use. The first chunk in the Master Report runs `setup.R`. 

If you are not sure of your AOC or reference sites' ID values, you can use the included function `get_waterbody_ids()`. You can pass this function first the dataset (which in this project is by default named cons_data), then a waterbody name or list of waterbody names (in vector format,  `c("Name 1", "Name 2", etc...)`). Here is an example to find the St. Lawrence River AOC ID:

```
> get_waterbody_ids(cons_data, site_names = "St. Lawrence River (Lake St. Francis)")
[1] 45087425
```

There is also a reverse function to look up the site name from a given ID (`get_waterbody_names()`), which is used in the Master report Rmd file to correctly populate table fields.
