# FASTR - WQ Station Map
# purpose: create a map of the WQ stations for FASTR
# author: Sarah Perry
# contact: seperry83@gmail.com

# import packages
import os
import numpy as np
import pandas as pd
import cartopy.crs as ccrs
import matplotlib.pyplot as plt
from adjustText import adjust_text
import cartopy.io.img_tiles as cimgt
import matplotlib.patches as mpatches

# -- Import Data --
# define main FASTR filepath (assumes sync'd with Sharepoint)
fp_fastr = '/California Department of Water Resources/Office of Water Quality and Estuarine Ecology - North Delta Flow Action/'

# define rel/abs filepaths
fp_rel_wq = fp_fastr + 'WQ_Subteam/Processed_Data/NDFA_WQ_Stations.csv'
fp_abs_wq = os.getenv('USERPROFILE') + fp_rel_wq

# import station data
df_stations = pd.read_csv(fp_abs_wq)

# -- Set Variables --
uni_regions = sorted(df_stations['Region'].unique())
num_regions = len(uni_regions)

# -- Modify Data --
# create a "region color" column that's a numeric equivalent for the regions
df_stations['Region_Color'] = np.nan

for i, region in enumerate(uni_regions):
    df_stations['Region_Color'].loc[df_stations['Region'] == region] = i

# -- Create Base Map --
# create figure and use Stamen terrain
stamen_terrain = cimgt.Stamen('terrain-background')
fig = plt.figure()
fig.set_size_inches(15, 15)

# create a GeoAxes in the tile's projection
ax = fig.add_subplot(1, 1, 1, projection=stamen_terrain.crs)

# set coord ref sys
crsGeo = ccrs.PlateCarree()

# limit map extent to area of the run
coordDict = {
    'minLat': df_stations['Latitude'].min() - .04,
    'maxLat': df_stations['Latitude'].max() + .05,
    'minLon': df_stations['Longitude'].min() - .05,
    'maxLon': df_stations['Longitude'].max() + .05
    }
    
ax.set_extent([coordDict['minLon'], coordDict['maxLon'], coordDict['minLat'], coordDict['maxLat']], crs=crsGeo)

# add data at correct zoom level
ax.add_image(stamen_terrain, 12)

# -- Set Parameters for Plotting --
# define box to outline text on map
bboxProps = dict(fc="white", ec="k", lw=1, alpha=.1)

# set colormap
cmap = plt.cm.get_cmap('Set1_r', num_regions)
# del cmap.colors[5]

# set legend borders in the interval [0, 1]
bound = np.linspace(0, 1, num_regions)

# -- Plot Data --
# define parameters
lat_param = np.array(df_stations['Latitude'])
lon_param = np.array(df_stations['Longitude'])
region_param = np.array(df_stations['Region_Color'])

# plot points
ax.scatter(lon_param, lat_param,
            s=165,
            marker='o',
            c=region_param,
            cmap=cmap,
            edgecolors='k',
            linewidth=1.7,
            transform=crsGeo)

# create and plot legend
leg = fig.legend(
    [mpatches.Patch(color=cmap(b), lw=1.0, ec='k') for b in bound],
    [i for i in uni_regions],
    loc=1,
    bbox_to_anchor=(0.55,0.55),
    edgecolor='k'
    )

leg.set_title('Regions',prop={'size':14,'weight':'bold'})

# add annotations to map
for i in range(len(df_stations)):
   annotations = [
       ax.annotate(
           '%s' % (df_stations['StationCode'][i]),
            xy=(df_stations['Longitude'][i], df_stations['Latitude'][i]),
            size=14,
            xycoords=ccrs.Geodetic()._as_mpl_transform(ax),
            xytext=(13, 14),
            textcoords='offset points',
            ha='center',
            bbox=bboxProps
            ) for i in range(len(df_stations['Longitude']))
   ]

   # adjust labels so they don't overlap as much
   adjust_text(
       annotations,
       expand_text=(0.3, 0.3)
       )
   
# -- Save Figure --
# define rel/abs filepaths
fp_rel_save = fp_fastr + 'WQ_Subteam/Raw_Plots/wq_station_map.png'
fp_abs_save = os.getenv('USERPROFILE') + fp_rel_save

# save fig
fig.savefig(fp_abs_save, dpi=150)

# close fig
plt.close()