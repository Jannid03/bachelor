Folgende Komandoaufrufe wurden verwendet:

******BAMBOO*******

Kompilierenvon BAMBOO:
g++ --std=c++23 bachelor_main.cpp SCITE_need/findBestTree_edit.cpp SCITE_need/matrices.cpp SCITE_need/scoreTree.cpp SCITE_need/trees.cpp SCITE_need/treelist.cpp SCITE_need/rand.cpp SCITE_need/output.cpp -o bachelor_main

Anschließend erfolgt der Aufruf wie folgt:
./bachelor_main [datei]

[datei] enthält dabei folgende Komponenten in dieser Reihenfolge
Berechneten Werte in der Forem: [X] [Y] [X->Y] [X<-Y] [X<->Y]
Datenmatrix
FN
FP
n
m
Graphviz-Datei des Originalbaumes
Graphviz-Datei von KS
Graphviz-Datei von SCITE

******DATENGENERIERUNG********

g++ --std=c++23 SCITE_need/NoisyDataMatrix.cpp SCITE_need/matrices.cpp -o generate_data

Aufruf:

./generate_date [Name] [n] [m] FN FP 0 0 1 0

Die 0 0 1 0 sorgt dafür, dass nur ein Baum ohne homozygote Mutationene erstellt wird.


**** DATEIERKLÄRUNG ******
scpript.R: Datengenerierung und Auswertung durch alle drei Methoden. Ergebnisse werden in txt Datei geschrieben
ausfuehren.R: Automatisierung für scpript.R, Eingabe kann angepasst werden mit n, m Fehlerraten und Anzahl der Durchläufe und ob die wahren Daten oder Noise Daten verwendet werden sollen





