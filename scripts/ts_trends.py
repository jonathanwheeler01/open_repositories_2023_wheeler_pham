# -*- coding: utf-8 -*-
"""
Created on Thu Feb  2 13:44:28 2023

@author: jwheel01
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

#%% Resample by period and aggregate

cols = ['position', 'clickThrough', 'country', 'device', 'impressions', 'date', 'index', 'repository_id']
click_data  = dt_resample_aggregate(ramp_sample, cols, 'D', np.sum)

#%% Plot monthly clicksums

ax = click_data.plot(legend=False)
ax.set_title('Daily clicksums on all URLs for all IR in sample')

click_data.rolling(window=14).mean().plot(ax=ax)
click_data.rolling(window=30).mean().plot(ax=ax)
click_data.rolling(window=90).mean().plot(ax=ax)

ax.legend(['Daily total', 'Two week avg', 'Monthly avg', 'Quarterly avg'])

#%% Use raw data: Drop cols from sample for rolling means

ramp_sample_clicks = ramp_sample.drop(columns=cols)

#%% Check data

ramp_sample_clicks.head()
ramp_sample_clicks.info()

#%% Rolling averages on raw data

ax = ramp_sample['clicks'].plot(legend=False)

ramp_sample['clicks'].rolling(window=7).mean().plot(ax=ax)

ax.legend(['Daily clicksums', 'Weekly avg'])












































