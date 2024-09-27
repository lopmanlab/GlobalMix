# GlobalMix study
*Contributors: ... Saad B. Omer <sup>2</sup> Ben Lopman <sup>1</sup>*

*<sup>1</sup> Emory University*
*<sup>2</sup> University of Texas SouthWestern*

*Correspondence to: blopman@emory.edu;*

# Description of study
**Aim**
1. Characterize the patterns of social contact across the age range in urban and rural settings in low- and middle-income settings using standardized social contact diaries.

The protocol that contains the detailed data collection procedure is explained in Aguolu et al (2024).<sup>1</sup>

# Description of repository
This repository contains data, scripts, and a summary of questionnaires.
Folders are arranged as follows.
1. Guatemala
2. India
3. Mozambique
4. Pakistan
5. Other
6. Scripts
7. Codebook

Each country contains participants and contact diary datasets. The other folder has datasets from previous studies used for the analysis.
The scripts folder has scripts used for the analysis of published papers. The Codebook folder has codebooks explaining the variables in the datasets.

The GlobalMix datasets are named as follows;
- country-code_participant_data_aim.RDS
- country-code_contact_data_aim.RDS

Country codes are **gt** (Guatemala), **ind** (India), **moz** (Mozambique), **pak** (Pakistan).

The datasets from previous studies used for this analysis are as follows;
1. Oxford Covid-19 Government Response Tracker <sup>2</sup>
2. Synthetic contact matrices from Prem et al (2021) <sup>3</sup>

# Instructions for running the scripts
**Characterizing social behavior relevant for infectious disease transmission in four countries**
1. Load the packages and run the functions in the "Summary figures and functions" file.
2. Run the code in the "(countrycode)_main_analysis_script" file.
3. If needed, code for creating multipanel summary figures is in the "Summary figures and functions" file, which can be used after running all the country's main analysis code.
4. For modeling outputs and figure 3, please see "(countrycode)_model" file, and for supplemental figures 4-8, please see "Supp4-8" file.

## Reference
1. Aguolu OG, Kiti MC, Nelson K, et al. Comprehensive profiling of social mixing patterns in resource poor countries: A mixed methods research protocol. PLOS ONE. 2024;19(6):e0301638. doi:10.1371/journal.pone.0301638
2. Hale T, Angrist N, Goldszmidt R, et al. A global panel database of pandemic policies (Oxford COVID-19 Government Response Tracker). Nat Hum Behav. 2021;5(4):529-538. doi:10.1038/s41562-021-01079-8
3. Prem K, Zandvoort K van, Klepac P, et al. Projecting contact matrices in 177 geographical regions: An update and comparison with empirical data for the COVID-19 era. PLOS Computational Biology. 2021;17(7):e1009098. doi:10.1371/journal.pcbi.1009098
