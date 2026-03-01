import sys
with open('bdd_stdout.txt','r',encoding='utf-8') as f:
    lines = f.readlines()
out = []
i = 0
sc = 0
while i < len(lines):
    line = lines[i].rstrip()
    if 'Scenario:' in line or 'Scenario Outline:' in line:
        j = i + 1
        has_fail = False
        slines = [line]
        while j < len(lines) and 'Scenario:' not in lines[j] and 'Scenario Outline:' not in lines[j]:
            slines.append(lines[j].rstrip())
            if '\u2718' in lines[j]:
                has_fail = True
            j += 1
        if has_fail:
            sc += 1
            out.append(f'--- #{sc} ---')
            for sl in slines:
                out.append(sl)
            out.append('')
        i = j
    else:
        i += 1
out.append(f'Total: {sc}')
with open('bdd_fails.txt','w',encoding='utf-8') as f:
    f.write('\n'.join(out))
print(f'Done: {sc} failures written')
