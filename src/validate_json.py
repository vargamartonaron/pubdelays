import json

def CorrectSingleQuoteJSON(s):
    rstr = ""
    escaped = False

    for c in s:
        if c == "'" and not escaped:
            c = '"'
        elif c == "'" and escaped:
            rstr = rstr[:-1]
        elif c == '"':
          c = '\\' + c
        escaped = (c == '\\')
        rstr += c

    return rstr

with open("/users/usumusu/pubmed_medline_articles_aff.json", "r") as f:
    data = f.read()

data = CorrectSingleQuoteJSON(data)
with open("/users/usumusu/pubmed_medline_articles_aff.json", "w") as f:
    f.write(data)
