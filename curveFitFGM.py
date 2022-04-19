import pandas as pd
import os
import pathlib
import numpy as np
import matplotlib.pyplot as plt
from scipy.optimize import curve_fit
import math

pathlib.Path.cwd()

path = os.getcwd()

print(path)
# /Users/mbp/Documents/my-project/python-snippets/notebook

path="/Users/gorkem.er/Desktop/21Projects/Single_FG_Motion/"
os.chdir(path)

print(path)
df = pd.read_csv('aggTogetBGM.csv')
fgmdata = pd.read_csv('fgmdata.csv')

df_agg = pd.read_csv('aggTogetBGM.csv')
df_agg["response_error"] = df_agg['responseAR'] - df_agg['cuedAR']
#dGauss = pd.read_csv('dGauss.csv')

test1 = df[df["cuedAR"] == 0.00000]
test1_agg = df_agg[df_agg['barkod']==11]

#Recast xdata and ydata into numpy arrays so we can use their handy features
xdata = np.asarray(fgmdata["cuedAR"])
ydata = np.asarray(fgmdata["responseAR"])
plt.plot(xdata, ydata, 'o')

""" xdata = np.array([-2,-1.64,-1.33,-0.7,0,0.45,1.2,1.64,2.32,2.9,3.1, 3.5, 3.9])
ydata = np.array([-0.919576,-0.730975,-1.42001,1.03905,1.97389,2.41143,1.91091,0.919576,0.05,0.3, 0.5, 0.730975,1.42001])
plt.plot(xdata, ydata, 'o')
   """
""" xdata = np.asarray(dGauss.x)
ydata = np.asarray(dGauss.y)
plt.plot(xdata, ydata, 'o')
 """
 
""" 
xdata = np.asarray(df['cuedAR'])
ydata = np.asarray(df['responseAR'])
plt.plot(xdata, ydata, 'o') """



""" # Define the Gaussian function
def Gauss(x, A, B):
    y = A*np.exp(-1*B*x**2)
    return y
 """
def firstDerGauss(x,a,w):
    c = np.sqrt(2)/np.exp(-(0.5))
    y = x * a * w * c *np.exp(-(w*x**2))
    return y


parameters, covariance = curve_fit(firstDerGauss, xdata, ydata)
  
fit_a = parameters[0]
fit_w = parameters[1]
print("fit_a:", fit_a)
print("fit_w:", fit_w)

fit_y = firstDerGauss(xdata, fit_a, fit_w)
plt.plot(xdata, ydata, 'o', label='data')
plt.plot(xdata, fit_y, label='fit')
plt.legend()


# get the unique cued AR
df.cuedAR.unique()

def runAllCued():
    
    for i in range(max(df_agg.barkod)-1): #max(df_agg.barkod+1)
        print(i)
        #selectedCued = df_agg[df_agg['barkod']==i]
        selectedCued = df_agg.cuedAR.unique()[i]
        selectedCued = df_agg[df_agg['cuedAR']==selectedCued]
        #Recast xdata and ydata into numpy arrays so we can use their handy features
        xdata = np.asarray(selectedCued["uncuedAR"])
        ydata = np.asarray(selectedCued["response_error"])
        #plt.plot(xdata, ydata, 'o')


        parameters, covariance = curve_fit(firstDerGauss, xdata, ydata, maxfev=5000)
        
        fit_a = parameters[0]
        fit_w = parameters[1]
        print("fit_a:", fit_a)
        print("fit_w:", fit_w)

        fit_y = firstDerGauss(xdata, fit_a, fit_w)
        #plt.plot(xdata, ydata, 'o', label='data')
        t = i/100 + 0.07
        print(t)
        plt.plot(xdata, fit_y, '-', label=df_agg.cuedAR.unique()[i+1], color=(0.5+t,0.5+t,0.5+t)) #selectedCued["cuedAR"]
        plt.legend()

runAllCued()




""" from scipy.optimize import curve_fit
from scipy.interpolate import *   
def func(x, a, b, c, d):
    return a*x**3 + b*x**2 + c * x + d

Xdata1 = np.array([10, 20, 30, 60])
Ydata1 = np.array([3, 5, 4, 3.5])

plt.plot(Xdata1, Ydata1, 'bo', label='Raw Data')

popt, pcov = curve_fit(func, Xdata1, Ydata1)
a, b, c, d= popt
plt.plot(Xdata1, func(Xdata1, *popt), 'r--', label='fit') """