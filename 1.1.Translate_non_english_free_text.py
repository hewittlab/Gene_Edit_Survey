
# coding: utf-8

# #Translator
# 
# This notebook is used to translate the other language free text entries into english via the Python module [Goslate](https://pypi.python.org/pypi/goslate).
# 
# You should install this module via pip before running this notebook.
# 
# *Note: This has only been tested on Python 3.4*

# In[1]:

import json
import goslate
from progressbar import *
import argparse

parser = argparse.ArgumentParser(description='Translates non-english free text questions into english')
parser.add_argument('-i', '--input', help="Input file (json only)", required=True)
parser.add_argument('-o', '--output', help="Output file (will become json)", required=True)
args = vars(parser.parse_args())

# ####Used to convert the language encodings from the input file into ones that the Goslate module understands.

# In[2]:

langs = {"ENGLISH": "en", "FRENCH":"fr", "CHINESE":"zh-TW", "ARABIC":"ar", "RUSSIAN":"ru", "GERMAN":"de", "HINDI":"hi", "JAPANESE":"ja", "PORTUGUESE":"pt", "SPANISH":"es", "TURKISH":"tr" }


# ####Change to your input file

# In[3]:

with open(args['input']) as f:
    answers = json.load(f)


# ####Actual Translation
# Works by calling the Goslate library on the free text boxes that need translating. Filters out the non-english and blank answers to not waste time.
# 
# I added the `timeout=100` to the constructor of Goslate as I was getting a timeout error a few minutes in to processing. You may or may not need to tweak this. [YMMV](http://dictionary.cambridge.org/dictionary/british/ymmv).

# In[4]:
filtered = [answer for answer in answers if answer['language'] != "ENGLISH"]
gs = goslate.Goslate(timeout=100)
widgets = ['Translate Progress: ', Percentage(), ' ', Bar(marker='#',left='[',right=']'), ' ', SimpleProgress()] 
pbar = ProgressBar(widgets = widgets, maxval = len(filtered)) 
pbar.start()

free_text_questions = ['reason', 'other_traits_alter']
for index, answer in enumerate(filtered):
    pbar.update(index)
    for question in free_text_questions:
        if question in answer:
            if answer[question] != "":
                result = gs.translate(answer[question], "en")
                answer[question] = result

pbar.finish()

# ####Write translated data to file

# In[5]:

with open(args['output'], "w") as f:
    json.dump(answers, f, indent=4)

