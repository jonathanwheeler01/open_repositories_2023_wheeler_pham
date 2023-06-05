# -*- coding: utf-8 -*-
"""
Created on Thu Feb  2 13:44:28 2023

@author: jwheel01

This script aggregates per-month CSV files of RAMP data into a single
CSV file for the period of study (Jan 1, 2019 - Dec 31, 2021)
used in the analysis described in Wheeler and Pham's 
2023 Open Repositories presentation.

The data are large and cannot be uploaded or synced to GitHub. To use this
script, it is recommended to download the data from the following Dryad
repositories:
  
  * 2019 RAMP data: Wheeler, Jonathan; Arlitsch, Kenning (2021), 
  Repository Analytics and Metrics Portal (RAMP) 2019 data, Dryad, 
  Dataset, https://doi.org/10.5061/dryad.crjdfn342
  
  * 2020 RAMP data: Wheeler, Jonathan; Arlitsch, Kenning (2021), 
  Repository Analytics and Metrics Portal (RAMP) 2020 data, Dryad, 
  Dataset, https://doi.org/10.5061/dryad.dv41ns1z4
  
  * 2021 RAMP data: Wheeler, Jonathan; Arlitsch, Kenning (2023), 
  Repository Analytics and Metrics Portal (RAMP) 2021 data, Dryad, 
  Dataset, https://doi.org/10.5061/dryad.1rn8pk0tz
  
The script will unzip the data as part of the aggregation process, so there
is no need to unzip downloaded datasets. The paths to the zip files in the
script expect each year's data to be in a single directory. Data should be
stored accordingly:
  
  * Zipped data for 2019 should be saved to ./raw_data/2019/
  * Zipped data for 2020 should be saved to ./raw_data/2020/
  * Zipped data for 2021 should be saved to ./raw_data/2021/

The output of this script is a subset of the data limited to repositories 
that were participating in RAMP as of January 1, 2019. The output file will 
be saved to the ./raw_data/ directory and used as the input file to the
or23_analysis.R script as well as the R Markdown version of the analysis
and presentation, open_repositories2023.Rmd.

"""
#%% Import libraries
import pandas as pd
import numpy as np
from zipfile import ZipFile
import glob


#%% Initialize functions

def extract_ramp_data(zip_file):
    with ZipFile(zip_file) as rampzip:
        with rampzip.open(rampzip.namelist()[0]) as rampfile:
            # read date col as datetime and make index 
            month_df = pd.read_csv(rampfile, infer_datetime_format=True)
    return month_df

def dt_resample_aggregate(ramp_data, cols, per, aggregation):
    dt_reindex = ramp_data.set_index(pd.to_datetime(ramp_data['date'])).copy()
    dt_reindex.drop(columns=cols, inplace=True)
    ramp_dt_resample = dt_reindex.resample(per)
    ramp_aggregated = ramp_dt_resample.aggregate([aggregation])
    return ramp_aggregated

#%% Read data

zipfile_list = glob.glob("../raw_data/*/*country-device-info.zip")
print(zipfile_list[0])
ramp_data = extract_ramp_data(zipfile_list[0])

for f in zipfile_list[1:]:
    print(f)
    mo_data = extract_ramp_data(f)
    ramp_data = pd.concat([ramp_data, mo_data])

print(ramp_data.info()) # 44840781 rows

#%% Filter repositories

# Some IR joined during the 2019-2021 time period
# Limit to IR that were in already RAMP January 2019

# set date as datetime index
ramp_data.set_index(pd.to_datetime(ramp_data['date']), inplace=True)
ramp_data = ramp_data.sort_index()

# get January 2019 subset
ramp_jan19 = ramp_data.loc['2019-01']
sample_repos = ramp_jan19['repository_id'].unique()

# filter ramp_data on these IR
ramp_sample = ramp_data[ramp_data['repository_id'].isin(sample_repos)].copy()
repo_check = ramp_sample['repository_id'].unique()
set(repo_check) == set(sample_repos)

print(ramp_sample.info())

#%% Save sample as CSV
ramp_sample.to_csv("../results/ramp_sample_2019-2021.csv", index=False)












































