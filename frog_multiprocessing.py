#!/usr/bin/python
# frog_multiprocessing.py
# H. Muller
# 16.12.2019

# Analyse the COW corpus' morphological structure using frog in the LAMACHINE environment;
# Input: COW text in the tok format. The input will be cleaned before the analysis

#-------------------------------------------------
# Import modules

import sys, re, multiprocessing
from frog import Frog, FrogOptions
from itertools import chain

#-------------------------------------------------
# Text file input / output

def inputfile(filename):
    handle = open(filename,'r')
    linelist = handle.readlines()
    handle.close()
    return linelist

def outputtext(filename, text):
    with open(filename, 'w') as file:
        for word in text:
            file.write("%s\n" % word)

#-------------------------------------------------
# Cleaning

def cleaning(linelist):

    text = 'EOSTRUE'.join(linelist)

    return(text)

#-------------------------------------------------
# Prepare multiprocessing

def partitioning(linelist, cores):

    input_size = len(linelist)
    slice_size = input_size / cores
    slice_size = int(slice_size)
    remain = input_size % cores
    sliced_list = []
    iterator = iter(linelist)
    for i in range(cores):
        sliced_list.append([])
        for j in range(slice_size):
            sliced_list[i].append(iterator.__next__())
        if remain:
            sliced_list[i].append(iterator.__next__())
            remain -= 1

    textparts = []
    for i in sliced_list:
        part_i = 'EOSTRUE'.join(i)
        textparts.append(part_i)

    return(textparts)

#-------------------------------------------------
# Analysis
def analyse(textpart):

    part_analysis = []
    frog = Frog(FrogOptions(parser=False)) # initiate Frog
    print(str('$'*20 + '\ttext analyzed\t' + '$'*20))
    part_analysis.append(frog.process(textpart))

    return(part_analysis)


def analyse_paralell(textparts, cores):

    p = multiprocessing.Pool(cores) #Start a pool with x cores
    analysis = p.map(analyse,textparts)

    return(analysis)

def flatten_analysis(analysis, filename):

    analysis_flat = []
    for item in analysis:
        for subitem in item:
            analysis_flat = analysis_flat + subitem

    return(analysis_flat)


#-------------------------------------------------
# Main caller

def main():

    if len(sys.argv) < 3:
        print("Usage: frog_multiprocessing.py <txtfilename> <newfilename> <cores>")
        exit()
    filename = sys.argv[1]
    newfilename = str(sys.argv[2])
    cores = int(sys.argv[3])
    
    linelist = inputfile(filename) 
    textparts = partitioning(linelist, cores); print('\n\n\t\tpartitioning done\t\t\n\n')
    print(textparts)
    frog_output_fat = analyse_paralell(textparts, cores); print('frog analysis done')
    frog_output = flatten_analysis(frog_output_fat, newfilename)
    print(frog_output)

    outputtext(newfilename, frog_output)
    print("Output file: " + newfilename)

    return

#-------------------------------------------------
main()

#--- EOF -----------------------------------------
