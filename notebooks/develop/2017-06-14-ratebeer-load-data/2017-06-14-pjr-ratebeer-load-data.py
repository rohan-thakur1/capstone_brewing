
# coding: utf-8

# # In this Notebook

# This notebook extracts the following data from the RateBeer website (<a href='https://www.ratebeer.com/'>link</a>) using the ratebeer (<a href='https://github.com/alilja/ratebeer'>link</a>) package. 
# 
# * Beer - all data related to specific beers
# 
# The extracted data is saved in a .csv file for future analysis. 

# # Setup

# In[1]:


#!pip3 install requests beautifulsoup4 lxml
#!git clone https://github.com/alilja/ratebeer.git
#!cd ratebeer
#!python setup.py install


# In[2]:


import numpy as np
import pandas as pd

import ratebeer


# In[3]:


# connect to ratebeer API
rb = ratebeer.RateBeer()


# # Beer Data

# ```
# >>> rb.beer("/beer/new-belgium-tour-de-fall/279122/")
# {'_has_fetched': True,
#  'abv': 6.0,
#  'brewed_at': None,
#  'brewery': <Brewery('/brewers/new-belgium-brewing-company/77/')>,
#  'calories': 180,
#  'description': "New Belgium's love for beer, bikes and benefits is best "
#                 'described by being at Tour de Fat. Our love for Cascade and '
#                 'Amarillo hops is best tasted in our Tour de Fall Pale Ale. '
#                 "We're cruising both across the country during our favorite "
#                 'time of year. Hop on and find Tour de Fall Pale Ale in fall '
#                 '2014.',
#  'ibu': 38,
#  'img_url': 'https://res.cloudinary.com/ratebeer/image/upload/w_120,c_limit/beer_279122.jpg',
#  'mean_rating': None,
#  'name': 'New Belgium Tour de Fall',
#  'num_ratings': 261,
#  'overall_rating': 72,
#  'retired': False,
#  'seasonal': 'Autumn',
#  'style': 'American Pale Ale',
#  'style_rating': 70,
#  'style_url': '/beerstyles/american-pale-ale/18/',
#  'tags': ['cascade', 'amarillo'],
#  'url': '/beer/new-belgium-tour-de-fall/279122/',
#  'weighted_avg': 3.34}
#  ```

# ## Download via API

# In[4]:


# get list of beer styles for purposes of extracting beers
beer_styles = rb.beer_style_list()


# In[5]:


from Queue import Queue
from threading import Thread

from ipywidgets import FloatProgress
from IPython.display import display

# global variables
num_fetch_threads = 5
beers = []
queue = Queue()

# set up beer_styles download progress bar
p1 = FloatProgress(min=0, max=len(beer_styles), description='Getting URLs')
display(p1)

# build queue
for k, bs in beer_styles.items():
    p1.value += 1
    for b in rb.beer_style(bs):
        queue.put(b.url)

# set up beers download progress bar
p2 = FloatProgress(min=0, max=queue.qsize(), description="Downloading beer data")
display(p2)

# define worker
def get_beer(i, q):
    while True:
        try:
            url = q.get()
            beer = rb.beer(url)
            beers.append(beer)
            p2.value += 1
        except:
            pass
        q.task_done()

# open threads
for i in range(num_fetch_threads):
    worker = Thread(target=get_beer, args=(i, queue,))
    worker.setDaemon(True)
    worker.start()

# start task queue
queue.join()

print '*** Done'


# ## Export to CSV File

# In[6]:


df = pd.DataFrame(beers)


# In[7]:


# encode strings with utf-8
def encode_str(x):
    return str(x).encode('utf-8')

# encode all string columns
def clean_row(x):
    for c in x.keys():
        if isinstance(c, str):
            x[c] = encode_str(x[c])
    return x

# perform transformations
df = df.apply(lambda x: clean_row(x))


# In[9]:


# save beers to csv file
df.to_csv('../../data/beer.csv', encoding='utf-8')


# # Brewery Data

# In[ ]:





# # Beer Review Data

# ```
# >>> rb.brewery("/brewers/deschutes-brewery/233/")
# {'_has_fetched': True,
#  'city': 'Bend',
#  'country': 'USA',
#  'name': 'Deschutes Brewery',
#  'postal_code': '97702',
#  'state': 'Oregon',
#  'street': '901 SW Simpson Ave',
#  'telephone': '(541) 385-8606',
#  'type': 'Microbrewery',
#  'url': '/brewers/deschutes-brewery/233/',
#  'web': 'https://www.facebook.com/deschutes.brewery'}
#  ```

# In[ ]:




