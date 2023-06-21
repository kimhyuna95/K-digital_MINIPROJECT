first = 'Hello World!'
second = 'Hi! Welcome!!'
f = set(first)
s = set(second)
total = f & s
total
for i in total:
    if i == i.isalpha():
        print(i)
