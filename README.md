# GlobalMix study
*Contributors: ... Saad B. Omer <sup>2</sup> Ben Lopman <sup>1</sup>*

*<sup>1</sup> Emory University*
*<sup>2</sup> University of Texas SouthWestern*

*Correspondence to: blopman@emory.edu;*

# Description of study
**Aim**
1. Characterize the patterns of social contact across the age range in urban and rural settings in low- and middle-income settings using standardized social contact diaries.
2. Comprehensively profile the social contacts of young infants with their household members using wearable proximity sensing devices.

# Description of repository
This repository contains data, scripts, and a summary of questionnaires.
Folders are arranged as follows.
1. Guatemala
2. India
3. Mozambique
4. Pakistan
5. Scripts
6. Codebook

Each country has subfolders of **Aim 1** and **Aim 2**. The Aim 1 folder contains participants and contact diary datasets from the Aim 1 study.
The Aim 2 folder contains participants, contact diary, and sensor datasets from the Aim 2 study (to be updated).

The scripts folder has scripts used for the analysis of published papers. The Codebook folder has each country's files explaining the variables in the datasets.

The datasets are named as follows;
- country-code_participant_data_aim.RDS
- country-code_contact_data_aim.RDS

Country codes are **gt** (Guatemala), **ind** (India), **moz** (Mozambique), **pak** (Pakistan).

# Instructions for running the scripts
**Characterizing social behavior relevant for infectious disease transmission in four countries**
1. Load the packages and run the functions in the "Summary figures and functions" file.
2. Run the code in the "(countrycode)_main_analysis_script" file.
3. If needed, code for creating multipanel summary figures is in the "Summary figures and functions" file, which can be used after running all the country's main analysis code.
4. For modeling outputs and figure 3, please see "(countrycode)_model" file, and for supplemental figures 4-8, please see "Supp4-8" file.
