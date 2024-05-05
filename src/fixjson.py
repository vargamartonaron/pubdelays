import json

def correctDoubleQuoteJSON(s):
    rstr = ""
    escaped = False

    for c in s:
        if c == '"' and not escaped:
            c = "'"
        elif c == '"' and escaped:
            rstr = rstr[:-1]
        elif c == '\\':
            escaped = True
        else:
            escaped = False
        rstr += c

    return rstr

with open('/users/usumusu/pubmed_medline_articles_aff.json', 'r') as file:
    lines = file.readlines()

corrected_lines = [correctDoubleQuoteJSON(line) for line in lines]

with open('/users/usumusu/pubmed_medline_articles_aff.json', 'w') as file:
    file.writelines(corrected_lines)
    print("done!")
