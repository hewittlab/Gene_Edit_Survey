import csv
import sys
import string
from translate import translator

data = {} 

langs = {"ENGLISH": "en", "FRENCH":"fr", "CHINESE":"zh-TW", "ARABIC":"ar", "RUSSIAN":"ru", "GERMAN":"de", "HINDI":"hi", "JAPANESE":"ja", "PORTUGUESE":"pt", "SPANISH":"es", "TURKISH":"tr" }

with open(sys.argv[1], "r") as f:
    reader = csv.reader(f)
    temp_data = [row for row in reader][1:]
    for lang in langs:
        data[lang] = [row for row in temp_data if row[6] == lang]

for key in data:
    print(key, len(data[key]))

print("")

for lang in data:
    print(lang, langs[lang])

for lang in [key for key in data if key is not "ENGLISH"]:
    print(lang)
    for row in data[lang]:
        for col in [-1]:
            if "NA" not in row[col] and row[col] is not "":
                result = translator(langs[lang], "en", row[col])
                print("Translating: ", row[col])
                print(result)
                if isinstance(result[0], list): 
                    translation = "\n".join([res[0].strip() for res in result[0]])
                    print(translation)

