zoop data readme file

Original data and first QC:
R scripts:FASTR_zoop_analysis_plots_and_NMDS.RMD, Mallory B. code QC's original data to create zoop_NDFA.csv;
Raw data files: LT_RAW_ZoopThru2018_20200825.csv, LT_zoop_RAW, NDFA_2019_zoop_CPUE_allsites_alldates.csv
Output file: Zoop_NDFA

Next manipulation and QC:
R Scripts: AddBiomass.R, Nicole K. code does additional QC to the zoop_NDFA.csv, adds biomass, creates zoop_NDFA_v2,
This dataset is used in final analyses with additional QC from the code therein;
Raw data file: zoop_NDFA.csv, Biomass.csv
Output file: zoop_NDFA_v2

Additional data subset for contaminants
R script: Zoop_2019_SHR_STTD.R, Jesse A. code to query STTD and SHR only data for 2019, creates zoop_2019_SHR_STTD.csv;
Raw data files:zoop_NDFA_v2_copy (includes additional SHR data for 2019 from the raw data file NDFA_2019_zoop_CPUE_allsites_alldates.csv that was queried out of zoop_NDFA_v2
Output file: Zoop_2019_SHR_STTD.csv


