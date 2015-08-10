'''
Spelling correction

Reference: http://norvig.com/spell-correct.html
'''

import re, collections, string

def words(text): return re.findall('[a-z]+', text.lower())

def train(features):
    model = collections.defaultdict(lambda: 1)
    for f in features:
        model[f] += 1
    return model

NWORDS = train(words(open('dict_and_big.txt', encoding="latin-1").read()))

alphabet = string.ascii_lowercase

def edits1(word):
    splits     = [(word[:i], word[i:]) for i in range(len(word) + 1)]
    deletes    = [a + b[1:] for a, b in splits if b]
    transposes = [a + b[1] + b[0] + b[2:] for a, b in splits if len(b)>1]
    replaces   = [a + c + b[1:] for a, b in splits for c in alphabet if b]
    inserts    = [a + c + b     for a, b in splits for c in alphabet]
    return set(deletes + transposes + replaces + inserts)

def known_edits2(word):
    return set(e2 for e1 in edits1(word) for e2 in edits1(e1) in NWORDS)

def known(words): return set(w for w in words if w in NWORDS)

def correct(word):
    candidates = known([word]) or known(edits1(word)) or known_edits2(word) or [word]
    return max(candidates, key=NWORDS.get)


## PRAGMA MARK - hewittlab code

import argparse
import json
from progressbar import *
import enchant

d = enchant.Dict("en_US")

slang_dict = {'lol': 'laugh out loud', 'idk': 'i dont know', 'idc': 'i dont care'}

parser = argparse.ArgumentParser(description='Performs spelling correction on the input dataset')
parser.add_argument('-i', '--input', help="Input file (json only)", required=True)
parser.add_argument('-o', '--output', help="Output file (will become json)", required=True)
args = vars(parser.parse_args())

with open(args['input']) as f:
    answers = json.load(f)

widgets = ['SpellCheck Progress: ', Percentage(), ' ', Bar(marker='#',left='[',right=']'), ' ', SimpleProgress()] 
pbar = ProgressBar(widgets = widgets, maxval = len(answers)) 
pbar.start()

free_text_questions = ['reason', 'other_traits_alter']
for index, answer in enumerate(answers):
    pbar.update(index)
    for question in free_text_questions:
        if question in answer:
            if answer[question] != "":
                for char in string.punctuation:
                    if char in answer[question]:
                        answer[question] = answer[question].replace(char, '')
                words = answer[question].lower().split(" ")
                i = 0
                while i < len(words):
                    if words[i] in slang_dict:
                        words = words[:i] + slang_dict[words[i]].split(" ") + words[i+1:]
                    i+= 1
                print(words)
                for i in range(len(words)):
                    word = words[i]
                    word = re.findall("[a-z]+", word)
                    if len(word) > 0:
                        word = word[0]
                        if not d.check(word):
                            try:
                                corr = correct(word)
                                word = corr
                            except:
                                continue
                        words[i] = word
                print(words)
                words = " ".join(words)
                print(words)
                answers[index][question] = words
pbar.finish()

with open(args['output'], "w") as f:
    json.dump(answers, f, indent=4)
