import pandas as pd
import matplotlib.pyplot as plt
import numpy as np


# Games data
#game_df_test = game_df_1[['x','y','endX','endY','type','outcomeType']]
#game_df_test.to_csv("game_df_test.csv")

game_df_test = pd.read_csv("game_df_test.csv")

#Adding xT to data
xT_grid = pd.read_csv("https://raw.githubusercontent.com/mckayjohns/xT/main/xT_Grid.csv",header=None)
xT=np.array(xT_grid)
xT_rows, xT_cols = xT.shape

# Create bins based on xT np array
game_df_test['x1_bin'] = pd.cut(game_df_test['x'], bins=xT_cols, labels=False)
game_df_test['y1_bin'] = pd.cut(game_df_test['y'], bins=xT_rows, labels=False)
game_df_test['x2_bin'] = pd.cut(game_df_test['endX'], bins=xT_cols, labels=False)
game_df_test['y2_bin'] = pd.cut(game_df_test['endY'], bins=xT_rows, labels=False)

# Calculate xT for only successful passes
#
# if game_df_test['outcomeType']=='Successful' and game_df_test['type']=='Pass':
# 	game_df_test['start_zone_value'] = game_df_test[['x1_bin', 'y1_bin']].apply(lambda x: xT[x[1]][x[0]], axis=1)
# 	game_df_test['end_zone_value'] = game_df_test[['x2_bin', 'y2_bin']].apply(lambda x: xT[x[1]][x[0]], axis=1)
# 	game_df_test['xT'] = game_df_test['end_zone_value'] - game_df_test['start_zone_value']
# else:
# 	game_df_test['start_zone_value'] = np.nan
# 	game_df_test['end_zone_value'] = np.nan
# 	game_df_test['xT'] = np.nan


# Error is as a results of code 25. You cannot perform iteration as suggested. Possbile solution is to iterate all via rows of the column
# to perform computation as below.
# Also we need to set an empty columns perfilled with Null values before starting our solution.


game_df_test['start_zone_value'] = np.nan
game_df_test['end_zone_value'] = np.nan
game_df_test['xT'] = np.nan

for i in range(game_df_test.shape[0]):
    if (game_df_test.loc[i,'outcomeType'] == "Successful") & (game_df_test.loc[i,'type'] == 'Pass'):

        # Values to match at xT
        x1, y1 = int(game_df_test.loc[i, "x1_bin"]), int(game_df_test.loc[i, "y1_bin"])
        x2, y2 = int(game_df_test.loc[i, "x2_bin"]), int(game_df_test.loc[i, "y2_bin"])
        
        
        # matching values 
        game_df_test.loc[i, "start_zone_value"] = xT[y1, x1] # based on the values of x1_bin and y1_bin in game_df_test, find the matching xT value from the previously defined np array, xT
        game_df_test.loc[i, "end_zone_value"] = xT[y2, x2] # based on the values of x2_bin and y2_bin in game_df_test, find the matching xT value from the previously defined np array, xT
        game_df_test.loc[i, "xT"] = game_df_test.loc[i, "end_zone_value"] - game_df_test.loc[i, "start_zone_value"]
    else:
        pass