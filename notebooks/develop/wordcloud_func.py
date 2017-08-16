import pandas as pd
from gensim import corpora, models, similarities

#Always load
#dictionary = corpora.Dictionary.load('../../data/beerwords_v5.dict')

#For Beer:
#corpus = corpora.MmCorpus('../../data/beercorpus_v5.mm')
#corpus_indices = pd.read_csv('../../data/correct_corpus_ids.csv')['corpus_ids'].tolist()

#For Brewery
#corpus = corpora.MmCorpus('../../data/brewerycorpus.mm')
#corpus_indices = pd.read_csv('../../data/brewery_corpus_ids.csv')['corpus_ids'].tolist()

def get_cloud_inputs(b_id, dictionary, corpus, corpus_indices, num_words=200):
    '''
    b_id: ID of the beer OR brewery (from wrk.beer/wrk.brewery) for which you want the word cloud.
    dictionary: type gensim.corpora.dictionary -> load this from saved files
    corpus: type gensim.corpora.MMCorpus -> load this from saved files
    corpus_indices: Mapping list for corpus to beer / brewery
    num_words: Number of highest frequency words to be returned 
    '''
    for i, corp in zip(corpus_indices, corpus):
        if i==b_id:
            correct_corp = corp
            break
    sorted_corpus =  sorted(correct_corp, key = lambda x:x[1], reverse=True)[:num_words]
    return [(dictionary[id_], count_) for id_, count_ in sorted_corpus]