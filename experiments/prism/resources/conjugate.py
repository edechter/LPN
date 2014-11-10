#!/usr/bin/env python

import sys
sys.path.append("../../misc")

from os.path import exists
from pattern.en import *
import ipa

conjugations = [
    {"tense" : PRESENT,
     "number" : SINGULAR},
    {"tense" : PAST,
     "number" : SINGULAR},
    {"tense" : PRESENT,
     "number" : PLURAL}, 
    {"tense" : PRESENT, 
     "aspect" : PROGRESSIVE}]

def mkConjugations(verbs):
    if not(isinstance(verbs, list)):
        verbs = [verbs]
    out = []
    for verb in verbs: 
        for conj in conjugations: 
            out.append(conjugate(verb, **conj))
    return out

def topHundredConjugations(useIPA=True):
    verbFile = "./verbs.txt"
    assert(exists(verbFile))
    
    fin = open("verbs.txt", "r")
    top_hundred = [line.strip() for line in fin.readlines()]
    fin.close()
    
    conjs = mkConjugations(top_hundred)

    if useIPA:
        return ipa.ipa(conjs)
    return conjs




