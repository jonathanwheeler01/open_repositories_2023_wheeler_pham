# -*- coding: utf-8 -*-
"""
Created on Thu Mar 16 12:29:25 2023

@author: jwheel01
"""

#%% Import libraries
import pandas as pd
import numpy as np


#%% Define functions

def dt_resample_aggregate(ramp_data, cols, per, aggregation):
    dt_reindex = ramp_data.set_index(pd.to_datetime(ramp_data['date'])).copy()
    dt_reindex.drop(columns=cols, inplace=True)
    ramp_dt_resample = dt_reindex.resample(per)
    ramp_aggregated = ramp_dt_resample.aggregate([aggregation])
    return ramp_aggregated

#%% read data

ramp = pd.read_csv("../raw_data/ramp_country_coded_2019-2021.csv", 
                   encoding='latin1')


#%% Set dt index and sort
# note we could do this on import
ramp.set_index(pd.to_datetime(ramp['date']), inplace=True)
ramp = ramp.sort_index()

#%% Resample for time series visualization

# these cols will be dropped on resampled data
cols = ['position', 'clickThrough', 'country', 'device', 
        'impressions', 'date', 'index', 'repository_id',
        'Country', 'Location']

# resample
click_data  = dt_resample_aggregate(ramp, cols, 'D', np.sum)

#%% Plot monthly clicksums

ax = click_data.plot(legend=False)
ax.set_title('Daily clicksums on all URLs for all IR in sample')

click_data.rolling(window=14).mean().plot(ax=ax)
click_data.rolling(window=30).mean().plot(ax=ax)
click_data.rolling(window=90).mean().plot(ax=ax)

ax.legend(['Daily total', 'Two week avg', 'Monthly avg', 'Quarterly avg'])

#%% Subset by global region
# Location codes: 1 == "Global North," 0 == "Global South"

gn_ramp = ramp[ramp["Location"] == 1].copy()
gs_ramp = ramp[ramp["Location"] == 0].copy()

#%% Resample subsets

gn_click_data  = dt_resample_aggregate(gn_ramp, cols, 'D', np.sum)
gs_click_data  = dt_resample_aggregate(gs_ramp, cols, 'D', np.sum)

#%% Plot GN monthly clicksums

ax = gn_click_data.plot(legend=False)
ax.set_title('Daily clicksums from the global north on all URLs for all IR in sample')

gn_click_data.rolling(window=14).mean().plot(ax=ax)
gn_click_data.rolling(window=30).mean().plot(ax=ax)
gn_click_data.rolling(window=90).mean().plot(ax=ax)

ax.legend(['Daily total', 'Two week avg', 'Monthly avg', 'Quarterly avg'])

#%% Plot GS monthly clicksums

ax = gs_click_data.plot(legend=False)
ax.set_title('Daily clicksums from the global south on all URLs for all IR in sample')

gs_click_data.rolling(window=14).mean().plot(ax=ax)
gs_click_data.rolling(window=30).mean().plot(ax=ax)
gs_click_data.rolling(window=90).mean().plot(ax=ax)

ax.legend(['Daily total', 'Two week avg', 'Monthly avg', 'Quarterly avg'])

#%% Combine 2 week rolling average

ax = click_data.plot(legend=False)
ax.set_title('Daily and 14 day rolling mean clicksums')

click_data.rolling(window=14).mean().plot(ax=ax)
gn_click_data.rolling(window=14).mean().plot(ax=ax)
gs_click_data.rolling(window=14).mean().plot(ax=ax)

ax.legend(['Daily total',
           'Two week average',
           'Two week global north average', 
           'Two week global south average'])

#%% Combine 30 day rolling average

ax = click_data.plot(legend=False)
ax.set_title('Daily and 30 day rolling mean clicksums')

click_data.rolling(window=30).mean().plot(ax=ax)
gn_click_data.rolling(window=30).mean().plot(ax=ax)
gs_click_data.rolling(window=30).mean().plot(ax=ax)

ax.legend(['Daily total',
           '30 day average',
           '30 day global north average', 
           '30 day global south average'])

#%% Combine 90 day rolling average

ax = click_data.plot(legend=False)
ax.set_title('Daily and 90 day rolling mean clicksums')

click_data.rolling(window=90).mean().plot(ax=ax)
gn_click_data.rolling(window=90).mean().plot(ax=ax)
gs_click_data.rolling(window=90).mean().plot(ax=ax)

ax.legend(['Daily total',
           '90 day average',
           '90 day global north average', 
           '90 day global south average'])

#%% All data subset by device

ramp_desktop = ramp[ramp["device"] == "DESKTOP"].copy()
ramp_mobile = ramp[ramp["device"] == "MOBILE"].copy()
ramp_tablet = ramp[ramp["device"] == "TABLET"].copy()

#%% Resample all data by device

ramp_desktop_click_data  = dt_resample_aggregate(ramp_desktop, cols, 'D', np.sum)
ramp_mobile_click_data  = dt_resample_aggregate(ramp_mobile, cols, 'D', np.sum)
ramp_tablet_click_data  = dt_resample_aggregate(ramp_tablet, cols, 'D', np.sum)

#%% Subset by global region and device

gn_desktop = gn_ramp[gn_ramp["device"] == "DESKTOP"].copy()
gn_mobile = gn_ramp[gn_ramp["device"] == "MOBILE"].copy()
gn_tablet = gn_ramp[gn_ramp["device"] == "TABLET"].copy()

gs_desktop = gs_ramp[gs_ramp["device"] == "DESKTOP"].copy()
gs_mobile = gs_ramp[gs_ramp["device"] == "MOBILE"].copy()
gs_tablet = gs_ramp[gs_ramp["device"] == "TABLET"].copy()

#%% Resample region-device subsets

gn_desktop_click_data  = dt_resample_aggregate(gn_desktop, cols, 'D', np.sum)
gn_mobile_click_data  = dt_resample_aggregate(gn_mobile, cols, 'D', np.sum)
gn_tablet_click_data  = dt_resample_aggregate(gn_tablet, cols, 'D', np.sum)

gs_desktop_click_data  = dt_resample_aggregate(gs_desktop, cols, 'D', np.sum)
gs_mobile_click_data  = dt_resample_aggregate(gs_mobile, cols, 'D', np.sum)
gs_tablet_click_data  = dt_resample_aggregate(gs_tablet, cols, 'D', np.sum)

#%% Plot device clicksums on full dataset

ax = ramp_desktop_click_data.plot(legend=False)
ax.set_title('Daily and 14 day rolling mean clicksums: Desktop')

ramp_desktop_click_data.rolling(window=14).mean().plot(ax=ax)
ramp_desktop_click_data.rolling(window=30).mean().plot(ax=ax)
ramp_desktop_click_data.rolling(window=90).mean().plot(ax=ax)

ax.legend(['Daily total',
           'Two week average',
           '30 day average', 
           '90 day average'])

ax = ramp_mobile_click_data.plot(legend=False)
ax.set_title('Daily and 14 day rolling mean clicksums: Mobile')

ramp_mobile_click_data.rolling(window=14).mean().plot(ax=ax)
ramp_mobile_click_data.rolling(window=30).mean().plot(ax=ax)
ramp_mobile_click_data.rolling(window=90).mean().plot(ax=ax)

ax.legend(['Daily total',
           'Two week average',
           '30 day average', 
           '90 day average'])

ax = ramp_tablet_click_data.plot(legend=False)
ax.set_title('Daily and 14 day rolling mean clicksums: Tablet')

ramp_tablet_click_data.rolling(window=14).mean().plot(ax=ax)
ramp_tablet_click_data.rolling(window=30).mean().plot(ax=ax)
ramp_tablet_click_data.rolling(window=90).mean().plot(ax=ax)

ax.legend(['Daily total',
           'Two week average',
           '30 day average', 
           '90 day average'])

#%% Plot device clicksums from global north

ax = gn_desktop_click_data.plot(legend=False)
ax.set_title('Daily and 14 day rolling mean clicksums: GN Desktop')

gn_desktop_click_data.rolling(window=14).mean().plot(ax=ax)
gn_desktop_click_data.rolling(window=30).mean().plot(ax=ax)
gn_desktop_click_data.rolling(window=90).mean().plot(ax=ax)

ax.legend(['Daily total',
           'Two week average',
           '30 day average', 
           '90 day average'])

ax = gn_mobile_click_data.plot(legend=False)
ax.set_title('Daily and 14 day rolling mean clicksums: GN Mobile')

gn_mobile_click_data.rolling(window=14).mean().plot(ax=ax)
gn_mobile_click_data.rolling(window=30).mean().plot(ax=ax)
gn_mobile_click_data.rolling(window=90).mean().plot(ax=ax)

ax.legend(['Daily total',
           'Two week average',
           '30 day average', 
           '90 day average'])

ax = gn_tablet_click_data.plot(legend=False)
ax.set_title('Daily and 14 day rolling mean clicksums: GN Tablet')

gn_tablet_click_data.rolling(window=14).mean().plot(ax=ax)
gn_tablet_click_data.rolling(window=30).mean().plot(ax=ax)
gn_tablet_click_data.rolling(window=90).mean().plot(ax=ax)

ax.legend(['Daily total',
           'Two week average',
           '30 day average', 
           '90 day average'])

#%% Plot device clicksums from global south

ax = gs_desktop_click_data.plot(legend=False)
ax.set_title('Daily and 14 day rolling mean clicksums: GS Desktop')

gs_desktop_click_data.rolling(window=14).mean().plot(ax=ax)
gs_desktop_click_data.rolling(window=30).mean().plot(ax=ax)
gs_desktop_click_data.rolling(window=90).mean().plot(ax=ax)

ax.legend(['Daily total',
           'Two week average',
           '30 day average', 
           '90 day average'])

ax = gs_mobile_click_data.plot(legend=False)
ax.set_title('Daily and 14 day rolling mean clicksums: GS Mobile')

gs_mobile_click_data.rolling(window=14).mean().plot(ax=ax)
gs_mobile_click_data.rolling(window=30).mean().plot(ax=ax)
gs_mobile_click_data.rolling(window=90).mean().plot(ax=ax)

ax.legend(['Daily total',
           'Two week average',
           '30 day average', 
           '90 day average'])

ax = gs_tablet_click_data.plot(legend=False)
ax.set_title('Daily and 14 day rolling mean clicksums: GS Tablet')

gs_tablet_click_data.rolling(window=14).mean().plot(ax=ax)
gs_tablet_click_data.rolling(window=30).mean().plot(ax=ax)
gs_tablet_click_data.rolling(window=90).mean().plot(ax=ax)

ax.legend(['Daily total',
           'Two week average',
           '30 day average', 
           '90 day average'])

#%% Plot region and desktop

ax = ramp_desktop_click_data.plot(legend=False)
ax.set_title('Daily and 30 day rolling mean clicksums: Desktop')

ramp_desktop_click_data.rolling(window=30).mean().plot(ax=ax)
gn_desktop_click_data.rolling(window=30).mean().plot(ax=ax)
gs_desktop_click_data.rolling(window=30).mean().plot(ax=ax)

ax.legend(["Daily clicks total",
           "Total 30 day average clicks",
           "Global north 30 day average clicks",
           "Global south 30 day average clicks"])

#%% Plot region and tablet

ax = ramp_tablet_click_data.plot(legend=False)
ax.set_title('Daily and 30 day rolling mean clicksums: Tablets')

ramp_tablet_click_data.rolling(window=30).mean().plot(ax=ax)
gn_tablet_click_data.rolling(window=30).mean().plot(ax=ax)
gs_tablet_click_data.rolling(window=30).mean().plot(ax=ax)

ax.legend(["Daily clicks total",
           "Total 30 day average clicks",
           "Global north 30 day average clicks",
           "Global south 30 day average clicks"])

#%% Plot region and mobile

ax = ramp_mobile_click_data.plot(legend=False)
ax.set_title('Daily and 30 day rolling mean clicksums: Mobile')

ramp_mobile_click_data.rolling(window=30).mean().plot(ax=ax)
gn_mobile_click_data.rolling(window=30).mean().plot(ax=ax)
gs_mobile_click_data.rolling(window=30).mean().plot(ax=ax)

ax.legend(["Daily clicks total",
           "Total 30 day average clicks",
           "Global north 30 day average clicks",
           "Global south 30 day average clicks"])