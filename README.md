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
5. Scripts
6. Codebook

Each country contains participants and contact diary datasets.
The scripts folder has scripts used for the analysis of published papers. The Codebook folder has codebooks explaining the variables in the datasets.

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

## Reference
<sup>1</sup> Aguolu OG, Kiti MC, Nelson K, Liu CY, Sundaram M, Gramacho S, Jenness S, Melegaro A, Sacoor C, Bardaji A, Macicame I, Jose A, Cavele N, Amosse F, Uamba M, Jamisse E, Tchavana C, Briones HGM, Jarquín C, Ajsivinac M, Pischel L, Ahmed N, Mohan VR, Srinivasan R, Samuel P, John G, Ellington K, Joaquim OA, Zelaya A, Kim S, Chen H, Kazi M, Malik F, Yildirim I, Lopman B, Omer SB. Comprehensive profiling of social mixing patterns in resource poor countries: a mixed methods research protocol. medRxiv [Preprint]. 2023 Dec 5:2023.12.05.23299472. doi: 10.1101/2023.12.05.23299472. Update in: PLoS One. 2024 Jun 24;19(6):e0301638. doi: 10.1371/journal.pone.0301638.
