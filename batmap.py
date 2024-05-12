import csv
import pandas as pd 
import matplotlib.pyplot as plt
import cartopy.crs as ccrs
import cartopy.feature as cfeature
import matplotlib.pyplot as plt
import matplotlib.patches as patches
import numpy as np

def main():
	mnfev = 0 # Manufacturing - EVs and parts - no chargers

	# SETUP MAP
	projection = ccrs.LambertConformal(
		central_longitude=-95,
		central_latitude=38,
		standard_parallels=(33, 45)
	)
	fig, ax = plt.subplots(subplot_kw={'projection': projection})
	fig.set_size_inches(12, 8)
	ax.add_feature(cfeature.BORDERS)
	ax.add_feature(cfeature.STATES)
	ax.add_feature(cfeature.LAND)
	ax.add_feature(cfeature.OCEAN)
	ax.add_feature(cfeature.LAKES)
	ax.coastlines()
	ax.set_extent([-121, -72, 19, 52], ccrs.Geodetic())
	ax.set_title("IRA-Affected Investments, Electric Vehicle Battery Supply Chain, North America", fontsize=14)

	# PROCESS DATAPOINTS
	df = pd.read_csv("SI. IRA Incentives - S9. Post-IRA EV Supply Chain Investments.csv")

	df_filt = df.loc[(df["timeline"]=="Post-Inflation Reduction Act") & # Get the right timeline
		((df["technology"] == "Electric Vehicles") | (df["technology"] == "Batteries")) & # Get the right technologies
		(df["product"] != "Electric Vehicles: Chargers") & # Filter out electric vehicle chargers
		(df["latitude"] > 15) # valid locations
		]
	# Create dummy variables to assign dot sizes and colors to investment amount and product
	investment_map = {
		'Less than 10M': 18,      
		'10M - 100M': 54,
		'100M - 1B': 120,
		'Over 1 Billion': 240,
		'Not Specified': 18
	}
	df_filt["by_amount_cat"] = df['by_amount'].map(investment_map)

	product_map = {
		'Electric Vehicles: Components': 'white', 												# 39
		'Electric Vehicles: Assembly': 'white',													# 29
		'Batteries: Minerals Extraction': 'maroon',												# 11
		'Batteries: Minerals Extraction; Batteries: Materials Processing': 'mediumvioletred',	#  8
		'Batteries: Materials Processing': 'blue',												# 21
		'Batteries: Recycling': 'navy',															# 15
		'Batteries: Recycling; Batteries: Materials Processing': 'navy',						#  3
		'Batteries: Components': 'darkorange', 													# 41
		'Batteries: Cells': 'gold',																# 30
		'Batteries: Packs': 'gold',																# 38
		'Batteries: Cells; Batteries: Packs': 'gold',											# 15
		'Batteries: Minerals Extraction; Batteries: Recycling': 'black',						#  1
		'Batteries: Recycling; Batteries: Cells': 'black',										#  1
		'Batteries: Materials Processing; Batteries: Components': 'black', 						#  4
		'Batteries: Components; Batteries: Cells': 'black', 									#  1
		'Batteries: Components; Batteries: Cells; Batteries: Recycling': 'black',				#  1
	}
	df_filt["product_color"] = df['product'].map(product_map)

	# print(df_filt["product"].unique())
	# print(df_filt["product"].value_counts())

	# Plot each data point
	ax.scatter(df_filt['longitude'],df_filt['latitude'], s=df_filt["by_amount_cat"], facecolor=df_filt['product_color'], alpha=0.7, transform=ccrs.PlateCarree(),zorder=2)

	if SHOWFIG:
		plt.show()
	if SAVEFIG:
		fig.savefig('post-IRA_investments.png', format='png', dpi=1200)

if __name__ == '__main__':
	SHOWFIG = False
	SAVEFIG = True
	main()
