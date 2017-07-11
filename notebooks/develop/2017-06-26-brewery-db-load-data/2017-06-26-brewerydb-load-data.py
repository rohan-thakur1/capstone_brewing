
# coding: utf-8

# # In this Notebook

# Extract data from BreweryDB using the site's premium API.

# # Setup

# In[1]:


import csv
from datetime import datetime
import json
from Queue import Queue
import string
from threading import Thread

from ipywidgets import FloatProgress
from IPython.display import display
import numpy as np
import pandas as pd

from creds import apikey


# In[2]:


import requests

DEFAULT_BASE_URI = "http://api.brewerydb.com/v2"
BASE_URI = ""
API_KEY = ""

simple_endpoints = ["beers", "breweries", "categories", "events",
                    "featured", "features", "fluidsizes", "glassware",
                    "locations", "guilds", "heartbeat", "ingredients",
                    "search", "search/upc", "socialsites", "styles"]

single_param_endpoints = ["beer", "brewery", "category", "event",
                          "feature", "glass", "guild", "ingredient",
                          "location", "socialsite", "style", "menu"]

class BreweryDb:

    @staticmethod
    def __make_simple_endpoint_fun(name):
        @staticmethod
        def _function(options={}):
            return BreweryDb._get("/" + name, options)
        return _function

    @staticmethod
    def __make_singlearg_endpoint_fun(name):
        @staticmethod
        def _function(id, options={}):
            return BreweryDb._get("/" + name + "/" + id, options)
        return _function

    @staticmethod
    def _get(request, options):
        options.update({"key" : BreweryDb.API_KEY})
        r = requests.get(BreweryDb.BASE_URI + request, params=options)
        #print r.request.url
        return r.json()

    @staticmethod
    def configure(apikey, baseuri=DEFAULT_BASE_URI):
        BreweryDb.API_KEY = apikey
        BreweryDb.BASE_URI = baseuri
        for endpoint in simple_endpoints:
            fun = BreweryDb.__make_simple_endpoint_fun(endpoint)
            setattr(BreweryDb, endpoint.replace('/', '_'), fun)
        for endpoint in single_param_endpoints:
            fun = BreweryDb.__make_singlearg_endpoint_fun(endpoint)
            setattr(BreweryDb, endpoint.replace('/', '_'), fun)


# # Extract Data

# In[3]:


BreweryDb.configure(apikey)


# ## Brewery Data

# In[ ]:


# get number of pages
pages = BreweryDb.breweries()[u'numberOfPages']

# get breweries data
breweries = []
for page in range(1, pages+1):
    b = BreweryDb.breweries({'p': page})
    breweries.extend(b['data'])


# In[ ]:


len(breweries)


# In[ ]:


breweries = pd.DataFrame(breweries)
breweries.reset_index()


# In[ ]:


breweries.to_csv('../../../data/brewery-db/breweries.csv', quoting=csv.QUOTE_NONNUMERIC, encoding='utf-8', index=False)


# ## Beer Data

# In[4]:


# global variables
num_fetch_threads = 10
errors = []
queue = Queue()

# get number of pages
pages = BreweryDb.beers()[u'numberOfPages']

# build queue
for page in range(pages):
    q = page + 1
    queue.put(q)
    
# encode strings with utf-8
def encode_str(x):
    return str(x).encode('utf-8')

# encode all string columns
def clean_row(x):
    for c in x.keys():
        if isinstance(c, str):
            x[c] = encode_str(x[c])
    return x


# set up beers download progress bar
p = FloatProgress(min=0, max=queue.qsize(), description="Downloading beers")
display(p)

# define worker
def get_beers(i, q):
    while True:
        try:
            beers = []
            page = q.get() 
            # get beer data
            beers = BreweryDb.beers({'p': page, 'withBreweries': 'Y', 'withSocialAccounts': 'Y', 'withIngredients': 'Y'})
            with open('../../../data/brewery-db/%s.json' % page, 'w') as f:
                json.dump(beers, f, indent=4)
            
        except Exception as e: 
            errors.append(dict(page=page, error=str(e)))
            pass
        p.value += 1
        q.task_done()

# open threads
for i in range(num_fetch_threads):
    worker = Thread(target=get_beers, args=(i, queue,))
    worker.setDaemon(True)
    worker.start()

# start task queue
queue.join()

# send errors to csv
errors = pd.DataFrame(errors)
errors.to_csv('../../../data/brewery-db/beer-errors-1.csv', index=False, quoting=csv.QUOTE_NONNUMERIC)

print '*** Done'


# In[ ]:




