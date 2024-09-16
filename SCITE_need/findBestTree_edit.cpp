/*
 * findBestTrees_noR.cpp
 *
 *  Created on: Mar 27, 2015
 *      Author: jahnka
 */

#include <stdbool.h>
#include <vector>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <float.h>
#include <iostream>
#include <fstream>
#include <sstream>
#include <time.h>
#include "matrices.h"
#include "treelist.h"
#include "trees.h"
// #include "output.h"
// #include "mcmc.h"
// #include "rand.h"
#include "scoreTree.h"

using namespace std;

// int** getDataMatrix(int n, int m, string fileName);
// double* getErrorRatesArray(double fd, double ad1, double ad2, double cc);
// int readParameters(int argc, char* argv[]);
// string getOutputFilePrefix(string fileName, string outFile);
// string getFileName(string prefix, string ending);
// string getFileName2(int i, string prefix, string ending, char scoreType);
// vector<string> getGeneNames(string fileName, int nOrig);
// vector<double> setMoveProbs();
// int* getParentVectorFromGVfile(string fileName, int n);
// int getMinDist(int* trueVector, std::vector<bool**> optimalTrees, int n);
// void printGeneFrequencies(int** dataMatrix, int n, int m, vector<string> geneNames);


double defaultMoveProbs[] = {0.55, 0.4, 0.05};     // moves: change beta / prune&re-attach / swap node labels / swap subtrees
double defaultMoveProbsBin[] = {0.4, 0.6};    // moves: change beta / prune&re-attach / swap leaf labels


double errorRateMove = 0.0;
vector<double> treeMoves;
double chi = 10;
double priorSd = 0.1;
string fileName;      // data file
string outFile;       // the name of the outputfile, only the prefix before the dot
int n;                // number of genes
int m;                // number of samples
char scoreType = 'm';
int rep;            // number of repetitions of the MCMC
int loops;          // number of loops within a MCMC
double gamma = 1;
double fd;          // rate of false discoveries (false positives 0->1)
double ad1;          // rate of allelic dropout (false negatives 1->0)
double ad2 = 0.0;         // rate of allelic dropout (2->1)
double cc = 0.0;          // rate of falsely discovered homozygous mutations (0->2)
bool sample = false;
int sampleStep;
bool useGeneNames = false;        // use gene names in tree plotting
string geneNameFile;              // file where the gene names are listed.
bool trueTreeComp = false;      // set to true if true tree is given as parameter for comparison
string trueTreeFileName;        // optional true tree
bool attachSamples = false;       // attach samples to the tree
bool useFixedSeed = false;      // use a predefined seed for the random number generator
unsigned int fixedSeed = 1;   // default seed
bool useTreeList = true;
char treeType = 'm';        // the default tree is a mutation tree; other option is 't' for (transposed case), where we have a binary leaf-labeled tree
int maxTreeListSize = -1;  // defines the maximum size of the list of optimal trees, default -1 means no restriction

int** getDataMatrix(int n, int m, string fileName){

    int** dataMatrix = init_intMatrix(n, m, -1);

    ifstream in(fileName.c_str());

    if (!in) {
    	cout << "2 Cannot open file " << fileName << "\n";
      cout << fileName;
      cout << "\n";
      return NULL;
    }

    for (int i = 0; i < n; i++) {
        for (int j = 0; j < m; j++) {
            in >> dataMatrix[i][j];
        }
    }

    in.close();
    int** transposedMatrix = transposeMatrix(dataMatrix, n, m);
    free_intMatrix(dataMatrix);

    return transposedMatrix;
}

int* getParentVectorFromGVfile(string fileName, int n){
	int* parentVector = new int[n];

    for(int i = 0; i <n; i++) {
        parentVector[i] = n;
    }

	std::vector<std::string> lines;
	std::ifstream file(fileName.c_str());
	std::string line;
	while ( std::getline(file, line) ) {
	    if ( !line.empty() )
	        lines.push_back(line);
	}
	for(int i=0; i < lines.size(); i++){

		std::size_t found = lines[i].find(" -> ");
		if (found!=std::string::npos){
            int parent;
            int child;

            if (lines[i][1] == 'r') {
                // std::cout << "Root" << std::endl;
                parent = n+1;
            }
            else if (lines[i][2] == '_') {
                // std::cout << "Copy par" << std::endl;
                parent = n;
            }
            else {
                // std::cout << "else par" << std::endl;
                parent = atoi(lines[i].substr(0, found).c_str());
            }

            if (lines[i].substr(found+3)[1] == '"') {
                // std::cout << "Copy child" << std::endl;
                child = n;
            }
            else {
                // std::cout << "else child" << std::endl;
			    child = atoi(lines[i].substr(found+3).c_str());
            }
			
            // std::cout << "Vorher";
			parentVector[child-1] = parent-1;
            // std::cout << "    Nachher" << std::endl;

            // std::cout << "Parent, CHild: " << parent << ", " << child << std::endl;
	   }
	}
	return parentVector;
}