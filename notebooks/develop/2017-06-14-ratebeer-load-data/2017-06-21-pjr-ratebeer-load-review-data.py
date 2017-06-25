
# coding: utf-8

# # In this Notebook

# This notebook extracts the following data from the RateBeer website (<a href='https://www.ratebeer.com/'>link</a>) using the ratebeer (<a href='https://github.com/alilja/ratebeer'>link</a>) package. 
# 
# * Review - all review data related to all beers
# 
# The extracted data is saved in a .csv file for future analysis. 

# # Setup

# In[ ]:


#!pip3 install requests beautifulsoup4 lxml
#!git clone https://github.com/alilja/ratebeer.git
#!cd ratebeer
#!python setup.py install


# In[1]:


from datetime import datetime
import numpy as np
import pandas as pd
import re
import requests
import string
import csv
from Queue import Queue
import string
from threading import Thread

from ipywidgets import FloatProgress
from IPython.display import display

from bs4 import BeautifulSoup
import ratebeer
from ratebeer import soup as soup_helper
from ratebeer import RateBeer as RateBeerBase
from ratebeer.models import Beer as BeerBase


# In[2]:


# connect to ratebeer API
rb = ratebeer.RateBeer()


# # Beer Review Data

# In[3]:


class Review(object):
    """
    Args:
        review_soup (soup): the soup of the review
    Returns:
        appearance (int): rating for appearance (out of 5)
        aroma (int): aroma rating (out of 10)
        date (datetime): review date
        overall (int): overall rating (out of 20, for some reason)
        palate (int): palate rating (out of 5)
        rating (float): another overall rating provided in the review. Not sure how this different from overall.
        taste (int): taste rating (out of 10)
        text (string): actual text of the review.
        user_location (string): writer's location
        user_name (string): writer's username
    """

    def __init__(self, review_soup):
        # get ratings
        # gets every second entry in a list
        self.rating = float(review_soup.find(style=re.compile('color: #036;')).text)

        # get user information
        userinfo = review_soup.next_sibling
        self.text = userinfo.next_sibling.next_sibling.text.strip()
        self.user_name = re.findall(r'(.*?)\xa0\(\d*?\)', userinfo.a.text)[0]
        self.user_location = re.findall(r'-\s(.*?)\s-', userinfo.a.next_sibling)[0]

        # get date it was posted
        date = re.findall(r'-(?:\s.*?\s-)+\s(.*)', userinfo.a.next_sibling)[0]
        self.date = datetime.strptime(date.strip(), '%b %d, %Y').date()

    def __str__(self):
        """Provide a nicely formatted representation"""
        return self.text
    
    def __dict__(self):
        return dict(rating=self.rating, 
                    date=self.date,
                    text=self.text, 
                    user_name=self.user_name,
                    user_location=self.user_location)
    
    
class Beer(BeerBase):
    
    def get_reviews(self, review_order="most recent"):
        """Returns reviews for a specific beer.
        Args:
            url (string): The specific url of the beer. Looks like:
                "/beer/deschutes-inversion-ipa/55610/"
            review_order (string): How to sort reviews. Three inputs:
                most recent: Newer reviews appear earlier.
                top raters: RateBeer.com top raters appear earlier.
                highest score: Reviews with the highest overall score appear
                earlier.
        Returns:
            A generator of dictionaries, containing the information about the review.
        """

        if not self._has_fetched:
            self._populate()

        review_order = review_order.lower()
        url_codes = {
            "most recent": 1,
            "top raters": 2,
            "highest score": 3
        }
        url_flag = url_codes.get(review_order)
        if not url_flag:
            raise ValueError("Invalid ``review_order``.")

        page_number = 1
        while True:
            complete_url = u'{0}{1}/{2}/'.format(self.url, url_flag, page_number)
            soup = soup_helper._get_soup(complete_url)
            content = soup.find('div', class_='reviews-container')
            reviews = content.find_all('div', style='padding: 0px 0px 0px 0px;')
            if len(reviews) < 1:
                raise StopIteration

            for review_soup in reviews:
                yield Review(review_soup)

            page_number += 1


class RateBeer(RateBeerBase):
    def get_beer(self, ident, fetch=None):
            """Returns a Beer object for the requested ID"""
            if fetch is None:
                fetch = False
            return Beer(ident, fetch)


# In[4]:


# connect to ratebeer API
rb = RateBeer()


# In[5]:


# Load beer data
beers = pd.read_csv('../../../data/beer.csv')


# ## First Run

# In[ ]:


# global variables
num_fetch_threads = 10
errors = []
queue = Queue()


# encode strings with utf-8
def encode_str(x):
    return str(x).encode('utf-8')

# encode all string columns
def clean_row(x):
    for c in x.keys():
        if isinstance(c, str):
            x[c] = encode_str(x[c])
    return x


# build queue
for beer in beers.to_dict(orient='records'):
    queue.put(beer)
  
# set up beers download progress bar
p = FloatProgress(min=0, max=queue.qsize(), description="Downloading reviews")
display(p)

# define worker
def get_reviews(i, q):
    while True:
        try:
            beer = q.get()
            b = rb.get_beer(beer['url'])
            reviews = []
            for review in b.get_reviews():
                review = review.__dict__()
                review['beer_id'] = beer['id']
                reviews.append(review)
            # create DataFrame
            df = pd.DataFrame(reviews)

            # perform transformations
            df = df.apply(lambda x: clean_row(x))

            # save reviews to csv file
            df.to_csv('../../../data/reviews/%s.csv' % beer['id'], quoting=csv.QUOTE_NONNUMERIC, encoding='utf-8', index=False)
        except Exception as e: 
            errors.append(dict(beer_id=beer['id'], name=beer['name'], error=str(e)))
            pass
        p.value += 1
        q.task_done()

# open threads
for i in range(num_fetch_threads):
    worker = Thread(target=get_reviews, args=(i, queue,))
    worker.setDaemon(True)
    worker.start()

# start task queue
queue.join()

# send errors to csv
errors = pd.DataFrame(errors)
errors.to_csv('../../../data/review-errors-1.csv', index=False, quoting=csv.QUOTE_NONNUMERIC)

print '*** Done'


# ## Retry Errors

# In[6]:


# get beer_ids with errors in first run
errors = pd.read_csv('../../../data/review-errors-1.csv',  quoting=csv.QUOTE_NONNUMERIC)


# In[7]:


## Retry Errors
import csv
from Queue import Queue
import string
from threading import Thread

from ipywidgets import FloatProgress
from IPython.display import display

# global variables
num_fetch_threads = 10
queue = Queue()


# encode strings with utf-8
def encode_str(x):
    return str(x).encode('utf-8')

# encode all string columns
def clean_row(x):
    for c in x.keys():
        if isinstance(c, str):
            x[c] = encode_str(x[c])
    return x


# build queue
errors = errors['beer_id'].tolist()
for beer in beers.to_dict(orient='records'):
    if beer['id'] in errors:
        queue.put(beer)
errors = []
  
# set up beers download progress bar
p = FloatProgress(min=0, max=queue.qsize(), description="Downloading reviews")
display(p)

# define worker
def get_reviews(i, q):
    while True:
        try:
            beer = q.get()
            b = rb.get_beer(beer['url'])
            reviews = []
            for review in b.get_reviews():
                review = review.__dict__()
                review['beer_id'] = beer['id']
                reviews.append(review)
            # create DataFrame
            df = pd.DataFrame(reviews)

            # perform transformations
            df = df.apply(lambda x: clean_row(x))

            # save reviews to csv file
            df.to_csv('../../../data/reviews/%s.csv' % beer['id'], quoting=csv.QUOTE_NONNUMERIC, encoding='utf-8', index=False)
        except Exception as e: 
            errors.append(dict(beer_id=beer['id'], name=beer['name'], error=str(e)))
            pass
        p.value += 1
        q.task_done()

# open threads
for i in range(num_fetch_threads):
    worker = Thread(target=get_reviews, args=(i, queue,))
    worker.setDaemon(True)
    worker.start()

# start task queue
queue.join()

# send errors to csv
errors = pd.DataFrame(errors)
errors.to_csv('../../../data/review-errors-2.csv', index=False, quoting=csv.QUOTE_NONNUMERIC)

print '*** Done'


# In[ ]:




