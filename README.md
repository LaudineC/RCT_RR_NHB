
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Investigating how administrative burden and search costs affect social inequalities in early childcare access, a randomised controlled trial

------------------------------------------------------------------------

**All this project’s materials are free and open**.

- [Replicate the findings](#replicate)
- [Check out the files glossary for this repository](#glossary)

![Open data](images/data_large_color.png)   ![Open
materials](images/materials_large_color.png)  
![Preregistration](images/preregistered_large_color.png)

------------------------------------------------------------------------

## Abstract

Access to high-quality early childcare for low socioeconomic status
(SES) households has the potential to mitigate socioeconomic
inequalities. Yet, there is an SES-based gap in early childcare
enrolment. While low-SES households would benefit the most from
attending early childcare, they access early childcare the least. This
study tackles cognitive and behavioural barriers behind this access gap.
We test the effectiveness of informational interventions and
personalised support to enhance early childcare application and access
for low-SES households through a multi-arm experiment. Results reveal
that the information-only treatment had minimal impact while adding
personalised support significantly bridged the SES-gap in early
childcare applications. However, despite large impacts on application
rates, we found limited impacts on access rates for low-SES households.
By identifying key obstacles to early childcare access for low-SES
households, our research underscores the need for effective strategies
to promote equal opportunities in early childhood education.

------------------------------------------------------------------------

This repository contains the code for our paper.

Our pre-print is online here:

> XX, XX, XX and XX. 2024. “Investigating how administrative burden and
> search costs affect social inequalities in early childcare access, a
> randomised controlled trial”“. Accessed avril 3, 2025. Online at
> <https://osf.io/rh7eb/?view_only=ef7409ac646544eaac946374de134892>

## How to download and replicate

To reproduce the findings and re-run the analysis, do the following:

1.  Download and install this repository.
2.  Open `RCT_RR_NHB.Rproj` to open an [RStudio
    Project](https://r4ds.had.co.nz/workflow-projects.html).
3.  Within the RStudio Project, open the `ManuscriptNHB.Rmd` file and
    run it to replicate the main paper
4.  Within the RStudio Project, open the `SupplementaryInformation.Rmd`
    file and run it to replicate the Supplementary Information file.

The source code can be found in the `.Rmd` and `.R` files.

Please find the [session info below](#session-info).

*Estimated installation time* : If you have R and Rstudio installed,
then adding additional packages required in our R scripts is quick (a
couple of minutes)

*Estimated run time*: Rendering the `ManuscriptNHB.Rmd` document takes
about 5mins on our machine, both on Mac and PC (see [session
info](#session-info))

## Files glossary

The `codebook.csv` contains all variables of our data set.

The `Data/` folder contains the main data sets.

- `Data/BaselineWithRefusals.csv` is the original data set with all
  baseline data entires, including refusals. It contains data collection
  dates.
- `Data/MainDatabaseWithoutIds.csv` is a cleaned, anonymised version of
  that data, containing both baseline and endline information variables
  necessary to replicate the analysis.
- `Data/Sapatial Data/txcouv_pe_com_EAJE_assmat.csv` contains data on
  early childcare access in France necessary for
- `Data/RECENSEMENT_COMMUNE_POPULATION.shp` is a shape file necessary to
  replicate the map.

The `RScripts/` folder contains the scripts necessary to that we rely
upon in our analysis scripts. To keep the `ManuscriptNHB.Rmd` document
neat and readable, we wrote all lengthy code bits as external functions.

- `RScripts/AllFunctions.R` contains all functions used in the analysis.
- `RScripts/LoadAllData.R` contains the code to load the data.
- `RScripts/LoadInstallPackages.R` contains the code to install and load
  all necessary packages.
- `RScripts/Appendix.R` contains the analysis code necessary to
  replicate Supplementary Information.

The `Section/` folder documents our all sections of the main manuscript.

- `S1_Introduction.Rmd` contains the introduction.
- `S2_Results.Rmd` contains the main results.
- `S3_Discussion.Rmd` contains the discussion.
- `S4_Methods.Rmd` contains the methods.
- `S5_ExtendedData.Rmd` contains the Extended Data.

`Figures/` contains the figures used in the manuscript.

- `Figures/diagram_NHB.png` contains the allocation diagram (Figure 1 in
  the paper).
- `Figures/toc.jpg` contains theory of change of the experimentation (
  see `RegistrationReportMainV2.Rmd`)

`RegistrationReportAnonymous.Rmd` contains the source file for our
pre-registration which can be found on the [AEA
RCT](www.socialscienceregistry.org/trials/9901).

`NHB.bib` contains all references for the manuscript and `nature.csl`
the citation formatting.

`PreRegBibTeX.bib` contains all references for the pre-registration
`RegistrationReportMainV2.Rmd`.

`images/` contains images used for this ReadME file.

## Session info

    #> R version 4.3.0 (2023-04-21 ucrt)
    #> Platform: x86_64-w64-mingw32/x64 (64-bit)
    #> Running under: Windows 10 x64 (build 19045)
    #> 
    #> Matrix products: default
    #> 
    #> 
    #> locale:
    #> [1] LC_COLLATE=French_France.utf8  LC_CTYPE=French_France.utf8   
    #> [3] LC_MONETARY=French_France.utf8 LC_NUMERIC=C                  
    #> [5] LC_TIME=French_France.utf8    
    #> 
    #> time zone: Europe/Paris
    #> tzcode source: internal
    #> 
    #> attached base packages:
    #> [1] stats     graphics  grDevices utils     datasets  methods   base     
    #> 
    #> loaded via a namespace (and not attached):
    #>  [1] compiler_4.3.0    fastmap_1.1.1     cli_3.6.2         tools_4.3.0      
    #>  [5] htmltools_0.5.5   rstudioapi_0.16.0 yaml_2.3.7        rmarkdown_2.22   
    #>  [9] knitr_1.43        xfun_0.39         digest_0.6.31     rlang_1.1.4      
    #> [13] evaluate_0.21
