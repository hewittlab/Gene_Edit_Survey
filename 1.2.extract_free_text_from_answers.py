import json
import csv
import argparse

parser = argparse.ArgumentParser(description="Pulls out free text fields from the input json file and puts it into the output file as a csv") 
parser.add_argument('-i', '--input', help="Input file (json only)", required=True)
parser.add_argument('-o', '--output', help="Output file (will become csv)", required=True)
args = vars(parser.parse_args())

answers = json.load(open(args['input']))

writer = csv.writer(open(args['output'], "w"))

for answer in [answer for answer in answers if 'question_25' in answer and answer['question_25'] != "" and answer['question_25'] != " "]:
    writer.writerow([answer['objectId'], answer['question_25']])

