# coding: utf-8

import matplotlib.pyplot as pl
import numpy as np

if __name__ == "__main__":
    
    # carpet = tuple([] for i in range(4))

    # for l in open("allCarpets.txt", 'r'):
    #     l = l.strip()
    #     l = l.split(',')
    #     l = l[1:]

    #     carpet[0].append(l[0])
    #     carpet[1].append(l[1])
    #     carpet[2].append(l[2])
    #     carpet[3].append(l[3])

        
    # fig = pl.figure()
    # ax1 = fig.add_subplot(111)
    # ax1.plot(carpet[0], carpet[1], 'b', label=u"Temps de calcul du résultat")
    # ax1.set_ylabel(u"Temps (sec)")
    # ax1.set_xlabel("Taille du tapis de Sierpinski")
    # ax2 = ax1.twinx()
    # ax2.plot(carpet[0], carpet[3],'r', label=u"Nombre de noeuds créés pour le résultat")
    # ax2.set_ylabel(u"Nombre de noeuds")

    # handles, labels = ax1.get_legend_handles_labels()
    # handles2, labels2 = ax2.get_legend_handles_labels()
    # ax1.legend(handles + handles2, labels+labels2, loc=2)
    # pl.show()

    # ticker = ([], [], [])

    # for l in open("tests_ticker2.csv", 'r'):
        
    #     l = l.strip()
    #     l = l.split(',')
    #     # l = l[1:]

    #     ticker[0].append(l[0])
    #     ticker[1].append(l[1])
    #     ticker[2].append(l[2])
        


    # fig = pl.figure()
    # ax1 = fig.add_subplot(111)
    # ax1.plot(ticker[0], ticker[1], 'b', label=u"Nombre de noeuds créés")
    # ax1.set_ylabel(u"Nombre de noeuds")
    # ax1.set_xlabel("Pas de temps")
    # ax1.set_ylim([0, 1400000])
    # ax2 = ax1.twinx()
    # ax2.plot(ticker[0], ticker[2],'r', label=u"Temps de calcul du résultat")
    # ax2.set_ylabel(u"Temps (sec)")

    # handles, labels = ax1.get_legend_handles_labels()
    # handles2, labels2 = ax2.get_legend_handles_labels()
    # ax1.legend(handles + handles2, labels+labels2, loc=2)
    # pl.show()

    glider = ([], [], [], [])

    vals = [l.strip().split(',') for l in open("allGliders.csv", 'r')]
        
    comp = lambda x, y: cmp(int(x[1]), int(y[1]))

    for l in sorted(vals, cmp = comp):

        print l

        glider[0].append(l[0])
        glider[1].append(l[1])
        glider[2].append(l[2])
        glider[3].append(l[3])

    
    fig = pl.figure()

    # (Mcell.size m) t time id_diff
    
    ax1 = fig.add_subplot(111)
    
    la = u"Taille de la macro-cellule après t pas de temps"
    ax1.plot(glider[1], glider[0], 'b', label=la)
    ax1.set_xlabel("Pas de temps")
    ax1.set_ylabel("Taille de la macro-cellule en log2")

    ax2 = ax1.twinx()
    la = u'Temps de calcul du future en fonction du pas de temps'
    ax2.plot(glider[1], glider[2], 'r', label=la) 
    ax2.set_ylabel("Temps (sec)")
        
    handles, labels = ax1.get_legend_handles_labels()
    handles2, labels2 = ax2.get_legend_handles_labels()
    ax2.legend(handles + handles2, labels + labels2, loc=4)

    pl.show()

