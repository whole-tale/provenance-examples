#!/usr/bin/python3
import matplotlib.pyplot as plt
import csv

x = []
y = []

with open('output2.txt','r') as csvfile:
    plots = csv.reader(csvfile, delimiter=',')
    for row in plots:
        x.append(int(row[0]))
        y.append(int(row[1]))

plt.plot(x,y, label='Output2')
plt.xlabel('x')
plt.ylabel('y')
plt.title('Graph of output2.txt')
plt.legend()
plt.savefig('figure.pdf')
