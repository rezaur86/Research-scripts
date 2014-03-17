#!/usr/bin/python
from networkx import *
import sys
import numpy
import random 
n = 1000000	# of nodes
m = 1000000000		# of edges		
s = 0.8		# % of overlapped edges
r = 4		# shrehold
p = 0.1 	# % of A0

def generateG12(n,m,s):
	G = gnm_random_graph(n,m)	# generate random graph
	G1 = nx.to_dict_of_lists(G)
	#print G1
	G2 = {}
	for key in G1:
		for i in range(0,len(G1[key])):
			pick_edge = numpy.random.binomial(1,s) # use s to calculate which edge to pick or not
			G1[key][i] = str(G1[key][i])		# change into string format
			if (pick_edge == 1):
				if key not in G2:
					G2[key] = [G1[key][i]]
				else:
					G2[key].append(G1[key][i])
		G1[str(key)] = G1.pop(key)
		if key in G2:
			G2[str(key)] = G2.pop(key)
	return (G1, G2)
my_G1, my_G2 = generateG12(n,m,s)

def generateA0(my_G1,p):
	A0 = []
	for key in my_G1:
		pick_A0 = numpy.random.binomial(1,p)
		if (pick_A0 == 1):
			A0 = A0 + [[key,key]]
	return A0
			
A0 = generateA0(my_G1,p)

def pgm(G1,G2,A0,r):
	A = A0			#node pairs
	Z = []			#used mapped pairs
	notConsider = []	# include nodes which has been paired
	for item in A0:
		for iitem in item:
			notConsider.append(iitem) 
	#print  notConsider
	tChoice = []
	M = { }			# amount of neighbors which are pairs
	t = 0
	while A != Z:
		unusedA = [i for i in A if i not in Z]
		#print unusedA
		if len(unusedA) == 0:
			break
		tChoice = random.choice(unusedA)
		#print tChoice
		Z = Z + [tChoice]
		#print Z
		neighbor1 = G1[tChoice[0]]
		neighbor2 = G2[tChoice[1]]
		for n1 in neighbor1:
			for n2 in neighbor2:
				if (n1 not in notConsider) and (n2 not in notConsider):
					if (n1 + "," + n2) not in M:
						M[n1+","+n2] = 1
					else:
						M[n1+","+n2] = M[n1+","+n2] + 1 
					if M[n1+","+n2] >= r:
						A = A + [[n1,n2]]
						notConsider.append(n1)
						notConsider.append(n2)
	return A
A = pgm(my_G1,my_G2,A0,r)
#print A
count = 0
for item in A :
	if item[0] == item[1]:
		count = count +1
	else:
		count = count
			
print (count)
print (len(A))
print 

