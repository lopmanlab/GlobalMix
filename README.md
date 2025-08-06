# GlobalMix study
**DOI**: [10.5281/zenodo.16754436] (https://doi.org/10.5281/zenodo.16754437)

**Contributors** <br/>
*Kristin N Nelson <sup>1</sup> and Moses C Kiti <sup>1</sup>, Machi Shiiba <sup>1</sup>, Sara S Kim <sup>1</sup>, Pragati V. Prasad <sup>1</sup>, Charfudin Sacoor <sup>2</sup>, Azucena Bardají <sup>2</sup> <sup>3</sup>, Ivalda Macicame <sup>4</sup>, Edgar Jamisse <sup>2</sup>, Corssino Tchavana <sup>2</sup>, Americo José <sup>4</sup>, Nilzio Cavele <sup>2</sup>, Herberth Maldonado <sup>5</sup>, Claudia Jarquin <sup>5</sup>, H. María Ajsivinac <sup>5</sup>, Venkata Raghava <sup>6</sup>, Prasanna Samuel <sup>6</sup>, Rajan Srinivasan <sup>6</sup>, Momin Kazi <sup>7</sup>, Raheel Allana <sup>7</sup>, Dehao Chen <sup>1</sup>, Carol Liu <sup>1</sup>, Samuel M Jenness <sup>1</sup>, Noureen Ahmed <sup>8</sup>, Obianuju Aguolu <sup>9</sup>, Maria A. Sundaram <sup>10</sup>, Inci Yildirim <sup>11</sup>, Fauzia Malik <sup>8</sup>, Alessia Melegaro <sup>12</sup>, Benjamin A Lopman <sup>1</sup> and Saad B Omer <sup>8</sup>*

*<sup>1</sup> Emory University Rollins School of Public Health, Atlanta, GA, USA* <br/>
*<sup>2</sup> Manhiça Health Research Institute, Manhiça, Mozambique* <br/>
*<sup>3</sup> Barcelona Institute for Global Health ISGlobal, Barcelona, Spain* <br/>
*<sup>4</sup> National Institute of Health, Maputo, Mozambique* <br/>
*<sup>5</sup> Universidad del Valle de Guatemala, Guatemala City, Guatemala* <br/>
*<sup>6</sup> Christian Medical College, Vellore, Tmil Nadu, India* <br/>
*<sup>7</sup> The Aga Khan University, Karachi, Pakistan* <br/>
*<sup>8</sup> Peter O'Donnell Jr. School of Public Health, UT Southwestern Medical Center, Dallas, TX, USA* <br/>
*<sup>9</sup> The Ohio State University, Columbus, Ohio, USA* <br/>
*<sup>10</sup> Marshfield Research Institute, Marshfield, WI, USA* <br/>
*<sup>11</sup> Yale University, New Haven, CT, USA* <br/>
*<sup>12</sup> University of Milan, Milan, Italy*

*Correspondence to Ben Lopman (blopman@emory.edu)*

# Description of study
**Aim**
1. Characterize the patterns of social contact across the age range in urban and rural settings in low- and middle-income settings using standardized social contact diaries.

The protocol that contains the detailed data collection procedure is explained in Aguolu et al (2024).<sup>1</sup>

# Description of repository
This repository contains data, scripts, and questionnaires.
Folders are arranged as follows.
1. Guatemala
2. India
3. Mozambique
4. Pakistan
5. Other
6. Scripts
7. Codebook

Each country folder contains 'participants' and 'contact diary' datasets, which contain information about the study participants and their reported contacts, respectively. The Other folder contains the clean datasets used for the analysis.
The scripts folder has scripts used for the analysis of the "Characterizing social behavior relevant for infectious disease transmission in four countries, 2021-2023". The Codebook folder contains codebooks explaining the variables in the datasets.

The participant and contact diary datasets are named as follows;
- country-code_participant_data_aim.RDS
- country-code_contact_data_aim.RDS

Country codes are **gt** (Guatemala), **ind** (India), **moz** (Mozambique), **pak** (Pakistan).

The datasets from previous studies used for this analysis are as follows;
1. Oxford Covid-19 Government Response Tracker <sup>2</sup>
2. Synthetic contact matrices from Prem et al (2021) <sup>3</sup>

# System requirements
- The code is written in R version 4.4.1 using RStudio version 2023.06.1. <br/>
- The following packages are used to run the code: [cowplot, dplyr, EpiEstim, EpiModel, ggplot2, ggpubr, ggtext, grid, gridExtra, here, lme4, lmerTest, lubridate, patchwork, plotly, reshape2, scales, shadowtext, srvyr, survey, tidyr, viridis]. <br/>
- The code has been developed and tested on Windows 11. The code should be compatible with Windows and Mac operating systems. <br/>
- No non-standard hardware is required to run the code. 

# Installation guide
**Installing the latest version of R**
1. Go to the Comprehensive R Archive Network: https://cran.r-project.org/
2. Download the version for your operating system (e.g. Click Download R for Windows/macOS)
3. Follow the instructions provided

This should take about 3 minutes.

**Installing RStudio**
1. Go to the download RStudio website: https://posit.co/downloads/
2. Download RStudio for your operating system.
3. Follow the instructions provided

This should take about 2 minutes.

**Installing the R packages**
After installing R and RStudio, you can install packages using the following code.
```
install.packages(c("cowplot", "dplyr", "EpiEstim", "EpiModel", "ggplot2", "ggpubr", "ggtext", "grid", "gridExtra", "here", "lme4", "lmerTest", "lubridate", "patchwork", "plotly", "reshape2", "scales", "shadowtext,"srvyr", "survey", "tidyr", "viridis"))
```
This should take about 5 minutes.

# Instructions for running the scripts
1. Load the packages and run the functions in the "Summary figures and functions" file.
2. Run the code in the "(countrycode)_main_analysis_script" file.
3. If needed, code for creating multipanel summary figures is in the "Summary figures and functions" file, which can be used after running all the country's main analysis code.
4. For modeling code and outputs, please run the code in "Modeling" file.
5. For supplemental figures 9-12, please run the code in "Supp9-12" file. 
6. For regression analysis code and outputs, please run the code in "regression_analysis" file, and change the country name/code at the first few lines as needed.

Each file should take about 20 seconds to run all the code. All the outputs corresponding to the Figure numbers and table numbers are available in main text and supplemental materials.

## Reference
1. Aguolu OG, Kiti MC, Nelson K, et al. Comprehensive profiling of social mixing patterns in resource poor countries: A mixed methods research protocol. PLOS ONE. 2024;19(6):e0301638. doi:10.1371/journal.pone.0301638
2. Hale T, Angrist N, Goldszmidt R, et al. A global panel database of pandemic policies (Oxford COVID-19 Government Response Tracker). Nat Hum Behav. 2021;5(4):529-538. doi:10.1038/s41562-021-01079-8
3. Prem K, Zandvoort K van, Klepac P, et al. Projecting contact matrices in 177 geographical regions: An update and comparison with empirical data for the COVID-19 era. PLOS Computational Biology. 2021;17(7):e1009098. doi:10.1371/journal.pcbi.1009098
