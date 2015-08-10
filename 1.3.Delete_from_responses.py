import json, csv, argparse, time

parser = argparse.ArgumentParser(description='Performs spelling correction on the input dataset')
parser.add_argument('-i', '--input', help="Input file (json only)", required=True)
parser.add_argument('-d', '--delete', help="CSV of items to delete. Needs created,updated and ip", required=True)
parser.add_argument('-o', '--output', help="Output file (will become json)", required=True)
args = vars(parser.parse_args())

with open(args['input']) as f:
    answers = json.load(f)

with open(args['delete'], encoding='latin-1') as f:
    to_delete = [row for row in csv.reader(f)]

deleters = []
for index, answer in enumerate(answers):
    for row in to_delete:
        if 'ip' not in answer:
            answer['ip'] = ""
        answer_create = time.strftime("%d/%m/%Y %-H:%M", time.strptime( ":".join(answer['createdAt'].split(":")[:-1]), "%Y-%m-%d %H:%M"))
        answer_update = time.strftime("%d/%m/%Y %-H:%M", time.strptime( ":".join(answer['updatedAt'].split(":")[:-1]), "%Y-%m-%d %H:%M"))
        if answer_create in row[0] and answer_update in row[1] and answer['ip'] in row[2]:
            deleters.append(index)

for d in deleters[::-1]:
    del answers[d]


with open(args['output'], "w") as f:
    json.dump(answers, f, indent=4)

