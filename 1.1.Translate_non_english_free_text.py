
# coding: utf-8

# #Translator
# 
# This notebook is used to translate the other language free text entries into english via the Python module [Goslate](https://pypi.python.org/pypi/goslate).
# 
# You should install this module via pip before running this notebook.
# 
# *Note: This has only been tested on Python 3.4*

# In[25]:

import json
import sys
import string
import goslate


# ####Used to convert the language encodings from the input file into ones that the Goslate module understands.

# In[26]:

langs = {"ENGLISH": "en", "FRENCH":"fr", "CHINESE":"zh-TW", "ARABIC":"ar", "RUSSIAN":"ru", "GERMAN":"de", "HINDI":"hi", "JAPANESE":"ja", "PORTUGUESE":"pt", "SPANISH":"es", "TURKISH":"tr" }


# ####Change to your input file

# In[36]:

with open("../data/data_with_free_text.json") as f:
    answers = json.load(f)


# ####Actual Translation
# Works by calling the Goslate library on the free text boxes that need translating. Filters out the non-english and blank answers to not waste time.
# 
# I added the `timeout=100` to the constructor of Goslate as I was getting a timeout error a few minutes in to processing. You may or may not need to tweak this. YMMV.

# In[44]:

gs = goslate.Goslate(timeout=100)

free_text_questions = ['question_25', 'other_traits_alter']
for answer in [answer for answer in answers if answer['language'] != "ENGLISH"]:
    for question in free_text_questions:
        if question in answer:
            if answer[question] != "":
                result = gs.translate(answer[question], "en")
                answer[question] = result


# ####Write translated data to file

# In[ ]:

with open("../data/data_with_free_text_translated.json", "w") as f:
    json.dump(answers, f, indent=4)

