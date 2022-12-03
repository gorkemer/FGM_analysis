
#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
27  Nov re-running motion experiment

preparing the data.  

"""

import csv
import pandas as pd
import numpy as np
import datetime
import os
import matplotlib.pyplot as plt
import seaborn as sns # for plotting

import sys
print(sys.version)


from sklearn.linear_model import LogisticRegression

os.chdir('/Users/gorkem.er/Desktop/21Projects/Single_FG_Motion')

print(os.getcwd())


d = pd.read_csv('Data/Motion_finalData_01.csv')  



# check distance between shapes per id #
d["distBtwShapes"] = np.where(d.aperture_configuration==0, abs(d.aperture1_center_y - d.aperture2_center_y), abs(d.aperture1_center_x - d.aperture2_center_x) )#this is when organization is vertical, getting y values
distPerID = d.groupby("trial_participant_id")['distBtwShapes'].mean()
d["shapeDistID"] = np.where(((d.distBtwShapes >= 350) & (d.distBtwShapes <= 522)), 1, 0) #if shape dist is between 350 and 572, I call this the right dist, hence referred as 1



# re-code global_org so that "1" indicates vertical organization
d["globalOrg"] = (d.aperture_configuration-1)*-1
d["respError"] = d.selected_ellipse_logAR-d.cued_ellipse_logAR
d["arDiff"] = d.uncued_ellipse_logAR - d.cued_ellipse_logAR

# Are the two shapes in the same category of aspect ratio? i.e., both tall?

conditions_SameCat  = [
    ((d.cued_ellipse_logAR < -0.00000048)  & (d.uncued_ellipse_logAR < -0.00000048)),
    ((d.cued_ellipse_logAR > -0.00000048) & (d.uncued_ellipse_logAR > -0.00000048)),
    (d.cued_ellipse_logAR == d.uncued_ellipse_logAR)
    ]
choices_SameCat     = [1, 1, 1]

d["sameARCat"] = np.select(conditions_SameCat, choices_SameCat, 0 )

#tag identical pairs 
d["sameAR"] = np.where(d.cued_ellipse_logAR == d.uncued_ellipse_logAR, 1, 0)

d["sameMotion"] = np.where(d.ellipse1_move_direction == d.ellipse2_move_direction, 1, 0)
# populate cued_motion direction column -- use "cued_ellipse" and e1/e2 motion directions
d["cuedMotDir"] = np.where(d.cueType == 1, d.ellipse1_move_direction, d.ellipse2_move_direction)


#rename column names
d.rename({'ellipse1_move_direction': 'e1MotDir',
          'ellipse2_move_direction': 'e2MotDir',
          'selected_ellipse_logAR': 'responseAR',
          'uncued_ellipse_logAR': 'uncuedAR',
          'cued_ellipse_logAR': 'cuedAR'
          },
         axis=1, inplace=True)

### write the resulting dataframe to local
d.to_csv('motion_RA.csv')


