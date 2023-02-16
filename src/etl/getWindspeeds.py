#!/usr/bin/env python
# coding: utf-8

# In[1]:


import tabula
import os
import sys
import pandas as pd
from PyPDF2 import PdfFileReader
import time
import re
from decimal import Decimal
from IPython.core.display import clear_output


# In[2]:


def update_progress(progress):
    bar_length = 20
    if isinstance(progress, int):
        progress = float(progress)
    if not isinstance(progress, float):
        progress = 0
    if progress < 0:
        progress = 0
    if progress >= 1:
        progress = 1
    block = int(round(bar_length * progress))
    clear_output(wait = True)
    text = "Progress: [{0}] {1:.1f}%".format( "#" * block + "-" * (bar_length - block), progress * 100)
    print(text)


# In[3]:


pdfPath = os.path.join('..', '..', 'dat', 'raw', 'pdf', 'windspeedData.pdf')


# In[4]:


#get number of pages
with open(pdfPath, "rb") as p:
    n = PdfFileReader(p).numPages


# In[5]:


start = time.time()

#define interval for managing Java heap space
#keeping it small so we can watch a progress bar
interval = 10

dfs = []
k = 0
for i in range(1,n,interval):
    i = i + k
    j = i + interval
    if j > n:
        j = n
    k += 0
        
    try:
        #read tables
        tables = tabula.read_pdf(pdfPath,
                                 pages=(str(i)+'-'+str(j)))

        #drop first row (it's part of a column name) and concat
        tables = [d.iloc[1:] for d in tables]
        df = pd.concat(tables,
                       ignore_index=True)

        #clean up column name issue cited above
        df = df.rename(columns={'Outdoor': 'Outdoor Temperature (ºF)'})

        #append to large df
        dfs.append(df)
    
    except:
        print('Error around page ', i)
        sys.exit(0)
        
    update_progress(i / n)

dfs = pd.concat(dfs,
               ignore_index=True)
dfs = dfs.iloc[:,:-2]
end = time.time()

update_progress(1) 
print("Done! " + str(end-start) + " seconds elapsed.")


# In[6]:


dfs


# In[7]:


csvPath = os.path.join('..', '..', 'dat', 'clean', 'csv', 'windspeedData.csv')
dfs.to_csv(csvPath, index=False)


# In[5]:


import session_info
session_info.show()


# In[ ]:




