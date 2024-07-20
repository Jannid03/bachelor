#include <string>
#include <vector>
#include <memory>
#include <iostream>
#include <fstream>
//rcpp.h hier
#include <deque>
#include <algorithm>
#include <utility>
#include <cstdlib>
#include <cmath>

//Knoten Datenstruktur mit label und einen Vektor an pointer zu den jewieligen Kindern. Zwei Konstruktoren: mit name und für kopieren
struct node {
    node (std::string name) {
        label_ = name;
        konflikt_ = false;
    }

    node (bool kon) {
        konflikt_ = kon;
        label_ = "Konflikt";
    }

    node (const node& n1) {
        label_ = n1.label_;
        for (auto const & ptr : n1.children_) {
            children_.push_back(ptr);
        }
    }

    void ausgabe () const {
        std::cout << this -> label_;
    }


    bool konflikt_;
    std::string label_;
    std::vector<std::shared_ptr<node>> children_;
};

//prob = Wahrscheinlihckeiten, X -> Y, X <- Y, X <-> Y
//Gespeichert als Array der Größe 3, mehr nie benötigt
//maxmimum ist 0,1 oder 2, Stelle im Array
// Prio = Maximum - anderen beide
struct prob {

    prob (std::string x, std::string y, double xvory, double yvorx, double parallel) : x_(x), y_(y) {

        probs_[0] = xvory;
        probs_[1] = yvorx;
        probs_[2] = parallel;

        double max = std::max(std::max(probs_[0], probs_[1]), probs_[2]);

        if (max == probs_[0]) {
            prio_ = probs_[0] - probs_[1] - probs_[2];
            max_= 0;
        }
        else if (max == probs_[1]) {
            prio_ = probs_[1] - probs_[0] - probs_[2];
            max_ = 1;
        }
        else {
            prio_ = probs_[2] - probs_[1] - probs_[0];
            max_ = 2;
        }

    }

    std::string x_;
    std::string y_;
    double probs_ [3];
    double prio_;
    int max_; 

};

//datenstruktur für Konflikte
// Gespeichert werden:
//  Paare die nicht dem Maximum entsprechen
//  Das Maxmimum (eigentlich) und der Ersatz (stattdessen) gespeichert
//CLear Funktion für Funktion
struct conflict {

    void clear () {
        involved = std::pair<std::string, std::string> {"", ""};
        fall = "";
        eigentlich = 3;
        stattdessen = 3;
    }

    //std::vector<std::shared_ptr<node>> involved;
    std::pair<std::string, std::string> involved;
    int eigentlich;
    int stattdessen;

    std::string fall;
};

//Größer und kleiner Operator für probs
bool operator< (const prob & a, const prob & b) {
    return a.prio_ < b.prio_;
}

bool operator> (const prob & a, const prob & b) {
    return a.prio_ > b.prio_;
}

//Suchfunktion, fängt bei root an und sucht nach query. Tiefensuche
//Returned pointer auf den node
std::shared_ptr<node> find_node (const std::shared_ptr<node> & root, std::string query) {

    std::deque<std::shared_ptr<node>> stack {root};

    while (!stack.empty()) {
        std::shared_ptr<node> curr = stack[0];
        stack.pop_front();

        if (curr -> label_ == query) {
            return curr;
        }
        else {
            for (auto const & child : curr -> children_) {
                stack.push_back(child);
            }
        }
    }

    return nullptr;
}

//Ausgabefunktion, erstellt .dot file für grpahviz und führt aus
//digraph = directed graph
//Auflistung alle Eltern-Kind-Paare mit -> dazwischen
void tree_ausgabe (const std::shared_ptr<node> & root, const std::string name) {

    std::ofstream dotfile (name + ".dot");
    std::deque<std::shared_ptr<node>> stack {root};


    dotfile << "digraph G { " << std::endl;

    while (!stack.empty()) {
        std::shared_ptr<node> curr = stack[0];
        stack.pop_front();

        for (auto const & child : curr -> children_) {
            stack.push_back(child);

            dotfile << curr -> label_ << " -> " << child -> label_ << std::endl;
        }
    }

    dotfile << "}";

    dotfile.close();

    system("dot -Tsvg graph.dot > graph.svg");

}

//Hilfsfunktion, findet einen Knoten x in dem Deque visited, wird für Durchlauf gebraucht
bool finden (const std::deque<std::shared_ptr<node>> & visited, const std::string & x) {

    for (size_t i = 0; i < visited.size(); i++) {
        if ((visited[i] -> label_) == x) {
            return true;
        }
    }

    return false;
}

std::deque<std::shared_ptr<node>> subtree (const std::shared_ptr<node> & sub_root) {
    std::deque<std::shared_ptr<node>> stack {sub_root};

    for (size_t i = 0; i < stack.size(); i++) {
        for(size_t j = 0; j < stack[i] -> children_.size(); j++) {
            stack.push_back(stack[i] -> children_[j]);
        }
    }

    return stack;
}

std::shared_ptr<node> shared_ancestor(const std::shared_ptr<node> & root, const std::string a, const std::string b) {
    std::shared_ptr<node> ancestor = root;
    std::deque<std::shared_ptr<node>> stack {ancestor};

    while (!stack.empty()) {
        std::deque<std::shared_ptr<node>> subtree_list = subtree(stack[0]);
        

        if(finden(subtree_list, a) && finden(subtree_list, b)) {
            ancestor = stack[0];
            stack.clear();
            for(size_t i = 0; i < ancestor -> children_.size(); i++) {
                stack.push_back(ancestor -> children_[i]);
            } 
        }
        else {
            stack.pop_front();
        }
    }

    return ancestor;

}

std::shared_ptr<node> find_parent(const std::shared_ptr<node> & child, const std::shared_ptr<node> & root) {
    std::deque<std::shared_ptr<node>> stack {root};

    if (root == child) {
        return root;
    }

    while(!stack.empty()) {
        std::shared_ptr<node> current = stack[0];
        stack.pop_front();

        if(std::find(current -> children_.begin(), current -> children_.end(), child) != current -> children_.end()) {
            return current;
        }
        else {
            for (size_t i = 0; i < current -> children_.size(); i++) {
                stack.push_back(current -> children_[i]);
            }
        }

    }

    return nullptr;
}

//Decimal -> binary, als Vector zurückgegeben
std::vector<size_t> to_binary (size_t i) {

    std::vector<size_t> bin;
    while(i > 0) {
        bin.push_back(i % 2);
        i /= 2;
    }

    return bin;
}

//Erstellen aller Möglichkeiten der Kantenkombinationen
//result ist Vektor von Vektoren, mit allen Kombinationen an children von curr
std::vector<std::vector<std::shared_ptr<node>>> possibilities (const std::shared_ptr<node> & curr) {

    std::vector<std::vector<std::shared_ptr<node>>> result;
    size_t n = curr -> children_.size();

    size_t complete = std::pow(2,n) -1;

    for(size_t i = 0; i <= complete; i++) {
        std::vector<size_t> bin = to_binary(i);

        std::vector<std::shared_ptr<node>> in;
        for(size_t j = 0; j < bin.size(); j++) {
            if(bin[j] == 1) {
                in.push_back(curr -> children_[j]);
            }
            else {
                continue;
            }
        }

        result.push_back(in);
    }

    return result;

} 

//Ausprobieren aller möglichen Stellen an denen neu eingefügt werden könnte, wahrscheinlichste wird zurückgegeben
//Paar: 1. Elternknoten 2. mögliche Kinder
//2. ist initalisiert mit nullptr, ansonsten ersetzt durch die Möglichkeit aus possibilities die am wahrscheinlichsten ist
std::pair<std::shared_ptr<node>, std::vector<std::shared_ptr<node>>> find_place(std::vector<conflict> & all_conflicts, 
                                                                    const std::shared_ptr<node> & root,
                                                                    const std::vector<prob> & probs,
                                                                    const size_t anfang,
                                                                    const std::vector<prob> & ignored,
                                                                    std::vector<std::string> & in,
                                                                    const std::string & neu) {

    std::deque<std::shared_ptr<node>> stack {root};
    std::deque<std::shared_ptr<node>> visited;
    double max {0};
    std::pair<std::shared_ptr<node>, std::vector<std::shared_ptr<node>>> place {nullptr, std::vector<std::shared_ptr<node>> {nullptr}};
    bool confli = false;
    conflict conflicts;
    std::string fall;
    // std::cout << "Probs anfnag: " << probs[anfang].x_ << " " << probs[anfang].y_ << " " << probs[anfang].probs_[2] << std::endl;
    // std::cout << "Neu: " << neu << std::endl;

    //Tiefensuche
    while (!stack.empty()) {
        std::shared_ptr<node> curr = stack[0];
        stack.pop_front();
        bool local_con = false;
        std::pair<std::string, std::string> local_conflicts;
        int local_eigent;
        int local_statt;
        fall.clear();

        for (size_t i = 0; i < curr -> children_.size(); i++) {
            stack.push_front(curr -> children_[i]);
        }

        if (curr -> label_ != "root") {
            visited.push_back(curr);
        } 

        std::cout << "Visited: ";
        for (size_t i = 0; i < visited.size(); i++) {
            std::cout << visited[i] -> label_ << ", ";
        }
        std::cout << std::endl;

        double value {1};
        std::cout << "Max vorher: " << max << std::endl;
        std::cout << neu << " -> " << curr -> label_ << std::endl;  
        
        std::vector<std::vector<std::shared_ptr<node>>> possi = possibilities(curr);
        local_con = false;
        // local_conflicts.clear();
        local_eigent=0;
        local_statt=0;
        fall.clear();

        for (const auto & child : possi) {
            local_con = false;
        // local_conflicts.clear();
        local_eigent=0;
        local_statt=0;
        fall.clear();
        
            value = 1;
            std::vector<std::string> nach;
            std::deque<std::shared_ptr<node>> lil_stack;
            for (size_t i = 0; i < child.size(); i++) {
                lil_stack.push_back(child[i]);
            }

            std::cout << "Nach: ";
            while (!lil_stack.empty()) {
                std::shared_ptr<node> lil_curr = lil_stack[0];
                lil_stack.pop_front();

                std::cout << lil_curr -> label_ << ", ";
                nach.push_back(lil_curr -> label_);

                for (const auto & grandchild : lil_curr -> children_) {
                    lil_stack.push_back(grandchild);
                }
            }

            std::cout << std::endl;

            for (size_t i = anfang; i < probs.size(); i++) {
                // std::cout << "i: " << i << std::endl;
                // if((probs[i].x_ == "B")) {
                //     std::cout << "B da?: " << finden(visited, probs[i].x_) << std::endl;
                // }

                if((probs[i].x_ == neu) && (finden(visited, probs[i].y_))) {
                    value *= probs[i].probs_[1];    
                    if (probs[i].max_ != 1) {
                        local_con = true;
                        local_conflicts = std::pair<std::string, std::string> {probs[i].x_, probs[i].y_};
                        local_eigent = probs[i].max_;
                        local_statt = 1;
                        fall = "E1";
                    }
                }
                else if ((probs[i].y_ == neu) && (finden(visited, probs[i].x_))) {
                    value *= probs[i].probs_[0];
                    if (probs[i].max_ != 0) {
                        local_con = true;
                        local_conflicts = std::pair<std::string, std::string> {probs[i].x_, probs[i].y_};
                        local_eigent = probs[i].max_;
                        local_statt = 0;
                        fall = "E2";
                    }
                }
                else if((probs[i].x_ == neu) && (std::find(nach.begin(), nach.end(), probs[i].y_) != nach.end())) {
                    value *= probs[i].probs_[0];   
                    if (probs[i].max_ != 0) {
                        local_con = true;
                        local_conflicts = std::pair<std::string, std::string> {probs[i].x_, probs[i].y_};
                        local_eigent = probs[i].max_;
                        local_statt = 0;
                        fall = "E3";
                    } 
                }
                else if ((probs[i].y_ == neu) && (std::find(nach.begin(), nach.end(), probs[i].x_) != nach.end())) {
                    value *= probs[i].probs_[1];
                    if (probs[i].max_ != 1) {
                        local_con = true;
                        local_conflicts = std::pair<std::string, std::string> {probs[i].x_, probs[i].y_};
                        local_eigent = probs[i].max_;
                        local_statt = 1;
                        fall = "E4";

                        // std::cout << "Bei: " << probs[i].x_ << " und " << probs[i].y_ << " mit value: " << value << " und max: " << max << std::endl;
                    }
                }
                else if (((probs[i].x_ == neu) && (std::find(in.begin(), in.end(), probs[i].y_) != in.end())) || ((probs[i].y_ == neu) && (std::find(in.begin(), in.end(), probs[i].x_) != in.end()))) {
                    value *= probs[i].probs_[2];
                    if (probs[i].max_ != 2) {
                        local_con = true;
                        local_conflicts = std::pair<std::string, std::string> {probs[i].x_, probs[i].y_};
                        local_eigent = probs[i].max_;
                        local_statt = 2;
                        fall = "E5";
                        std::cout << "E5" << std::endl;
                    }
                }
                else {
                    continue;
                }
            }

            for (size_t i = 0; i < ignored.size(); i++) {
                if((ignored[i].x_ == neu) && (finden(visited, ignored[i].y_))) {
                    value *= ignored[i].probs_[1];  
                    if (ignored[i].max_ != 1) {
                        local_con = true;
                        local_conflicts = std::pair<std::string, std::string> {ignored[i].x_, ignored[i].y_};
                        local_eigent = ignored[i].max_;
                        local_statt = 1;
                        fall = "EI1";
                    }  
                }
                else if ((ignored[i].y_ == neu) && (finden(visited, ignored[i].x_))) {
                    value *= ignored[i].probs_[0];
                    if (ignored[i].max_ != 0) {
                        local_con = true;
                        local_conflicts = std::pair<std::string, std::string> {ignored[i].x_, ignored[i].y_};
                        local_eigent = ignored[i].max_;
                        local_statt = 0;
                        fall = "EI2";
                    }
                }
                else if((ignored[i].x_ == neu) && (std::find(nach.begin(), nach.end(), ignored[i].y_) != nach.end())) {
                    value *= ignored[i].probs_[0];  
                    if (ignored[i].max_ != 0) {
                        local_con = true;
                        local_conflicts = std::pair<std::string, std::string> {ignored[i].x_, ignored[i].y_};
                        local_eigent = ignored[i].max_;
                        local_statt = 0;
                        fall = "EI3";
                    }  
                }
                else if ((ignored[i].y_ == neu) && (std::find(nach.begin(), nach.end(), ignored[i].x_) != nach.end())) {
                    value *= ignored[i].probs_[1];
                    if (ignored[i].max_ != 1) {
                        local_con = true;
                        local_conflicts = std::pair<std::string, std::string> {ignored[i].x_, ignored[i].y_};
                        local_eigent = ignored[i].max_;
                        local_statt = 1;
                        fall = "EI4";
                    }
                }
                else if (((ignored[i].x_ == neu) && (std::find(in.begin(), in.end(), ignored[i].y_) != in.end())) || ((ignored[i].y_ == neu) && (std::find(in.begin(), in.end(), ignored[i].x_) != in.end()))) {
                    value *= ignored[i].probs_[2];
                    if (ignored[i].max_ != 2) {
                        local_con = true;
                        local_conflicts = std::pair<std::string, std::string> {ignored[i].x_, ignored[i].y_};
                        local_eigent = ignored[i].max_;
                        local_statt = 2;
                        fall = "EI5";
                    }
                }
                else {
                    continue;
                }
            }

            // std::cout << "Value before new Kante :" << value << std::endl;

            if (value > max) {
                // std::cout << neu << " curr: "<< curr -> label_ << " mit value: " << value << " und max: " << max << std::endl;
                max = value;
                place.first = curr;
                place.second = child;

                if (local_con) {
                    confli = local_con;
                    std::cout << "NEW CONFLICT" << std::endl;
                    std::cout << "involved: " << local_conflicts.first << " " << local_conflicts.second << std::endl;
                    std::cout << "Eig: " << local_eigent << " Statt: " << local_statt << " Fall: " << fall << std::endl;
                    conflicts.involved = local_conflicts;
                    conflicts.eigentlich = local_eigent;
                    conflicts.stattdessen = local_statt;

                    conflicts.fall = fall;
                }
                else {
                    confli = false;
                    conflicts.clear();
                }
                
            }
            std::cout << "Max nachher: " << max << std::endl;
        }

        // std::cout << "Visited: ";
        // for (size_t i = 0; i < visited.size(); i++) {
        //     std::cout << visited[i] -> label_ << ", ";
        // }
        // std::cout << std::endl;

        // std::cout << "Stack: ";
        // for (size_t i = 0; i < stack.size(); i++) {
        //     std::cout << stack[i] -> label_ << ", ";
        // }
        // std::cout << std::endl;

        //  std::cout << "In: ";
        // for (size_t i = 0; i < in.size(); i++) {
        //     std::cout << in[i] << ", ";
        // }
        // std::cout << std::endl;

        //Geht durch visited durch und wenn am Ende eines Astes, geht bis zur nächsten Verzeigung zurück
        bool go = true;
        while((!visited.empty()) && (!stack.empty()) && go) {
            if ((std::find(visited[visited.size()-1] -> children_.begin(), visited[visited.size()-1] -> children_.end(), stack[0]) == visited[visited.size()-1] -> children_.end())) {
                visited.pop_back();
                // std::cout << "Entfernt" << std::endl;
            }
            else {
                go = false;
            }
        }
        // std::cout << "test" << std::endl;

    }

    if (confli) {
        std::cout << "Es gibt einen Konflikt bei: " << std::endl;

        std::cout << conflicts.involved.first << ", " << conflicts.involved.second << "     Eigentlich: " << conflicts.eigentlich << "     Stattdessen: " << conflicts.stattdessen  << " Bei: " << conflicts.fall << std::endl;
        all_conflicts.push_back(conflicts);
    }

    // if (neu == "F") {
    //     std::cout << place.first -> label_ << " Kinder: " << place.second[0] <<std::endl;
    // }
    return place;
}

// [Rcpp::export]
void make_tree (const std::vector<prob> & probs) {
    std::shared_ptr<node> root (new node (std::string ("root")));

    //Initialiserung mit erstem Paar
    std::shared_ptr<node> x_ptr (new node (probs[0].x_));
    std::shared_ptr<node> y_ptr (new node (probs[0].y_));

    if (probs[0].max_ == 0) {
        x_ptr -> children_.push_back(y_ptr);
        root -> children_.push_back(x_ptr);
    } 
    else if (probs[0].max_ == 1) {
        y_ptr -> children_.push_back(x_ptr);
        root -> children_.push_back(y_ptr);
    }
    else {
        root -> children_.push_back(x_ptr);
        root -> children_.push_back(y_ptr);
    }

    std::vector<std::string> in {probs[0].x_, probs[0].y_};
    std::vector<prob> ignored;
    std::vector<conflict> conflicts;

    // size_t curr_place = 1;

    //Geht probs durhc und schut ob x oder y schon im Tree vorhanden sind
    //Wenn beide ja -> continue
    //Wenn nur eins ja -> wird an find_place übergeben und in den Tree eingefügt
    //Wenn kein ja -> continue und Wahrscheinlichkeit wird in ginored eingefügt
    for (size_t i = 1; i < probs.size(); i++) {
        std::pair<std::shared_ptr<node>, std::vector<std::shared_ptr<node>>> place;

        if ((std::find(in.begin(), in.end(), probs[i].x_) != in.end()) && (std::find(in.begin(), in.end(), probs[i].y_) != in.end())) {
            //std::cout << "Fall 1" << std::endl;
            continue;
        }
        else if((std::find(in.begin(), in.end(), probs[i].x_) != in.end())) {
            //std::cout << "Fall 2" << std::endl;
            place = find_place(conflicts, root, probs, i, ignored, in, probs[i].y_);
            in.push_back(probs[i].y_);
        }
        else if ((std::find(in.begin(), in.end(), probs[i].y_) != in.end())) {
            //std::cout << "Fall 3" << std::endl;
            place = find_place(conflicts, root, probs, i, ignored, in, probs[i].x_);
            in.push_back(probs[i].x_);
        }
        else {
            //std::cout << "Fall 4" << std::endl;
            ignored.push_back(probs[i]);
            continue;
        }
        
        //Neuer node
        std::shared_ptr<node> new_node (new node (in[in.size()-1]));

        //Wenn place.second nicht "leer" -> Kinder werden hinzugefügt und gelöscht aus vormaligen Elternknoten
        if (place.second[0] != nullptr) {
            // std::cout << "Kante mit: " << std::endl;
            for (size_t i = 0; i < place.second.size(); i++) { 
                // std::cout << place.second[i] -> label_ << ", ";
                new_node -> children_.push_back(place.second[i]);
                place.first -> children_.erase(std::find(place.first -> children_.begin(), place.first -> children_.end(), place.second[i]));
            }
            place.first -> children_.push_back(new_node);

            
        }
        else { //Ansonsten neuer Knoten

            // std::cout << (place.first == nullptr) << std::endl;
            place.first -> children_.push_back(new_node);
        }

        // tree_ausgabe(root);
    }
    std::cout << "Konflikt size: " << conflicts.size() << std::endl;
    //Konfliktlösung

    // for (size_t i = 0; i < conflicts.size(); i++) {
    //     std::shared_ptr<node> conflict_node (new node (true));

    //     // if(((conflicts[i].eigentlich == 0) && (conflicts[i].stattdessen == 1)) || ((conflicts[i].eigentlich == 1) && (conflicts[i].stattdessen == 0))) {
    //     //     //Pfad zwsichen den beiden zu Konflik
    //     // }
    //     if (conflicts[i].stattdessen == 2) {
    //         //Gemeinsame Elternknoten und dann bis zu den Knoten
    //         //Gesmater subbaum

    //         std::shared_ptr<node> sub_root = shared_ancestor(root, conflicts[i].involved.first, conflicts[i].involved.second);
    //         std::deque<std::shared_ptr<node>> sub_tree = subtree(sub_root);
    //         std::shared_ptr<node> parent = find_parent(sub_root, root);

    //         if (sub_root != root) {
    //             parent -> children_.erase(std::find(parent -> children_.begin(), parent -> children_.end(), sub_root));
    //         }
    //         else {
    //             sub_tree.pop_front();
    //         }

    //         std::cout << "Subtree size: " << sub_tree.size() << std::endl;

            
    //         for (size_t j = 0; j < sub_tree.size(); j++) {
    //             conflict_node -> children_.push_back(sub_tree[j]);
    //             sub_tree[j] -> children_.clear();

    //             auto position = std::find(root -> children_.begin(), root -> children_.end(), sub_tree[j]);
    //             if (position != root -> children_.end()) {
    //                 root -> children_.erase(position);
    //             }
    //         }

    //         parent -> children_.push_back(conflict_node);
    //     }
    //     // else if ((conflicts[i].eigentlich == 1) && (conflicts[i].stattdessen == 0)) {
    //     //     //Pfad zwischen beiden
    //     // }
    //     // else if ((conflicts[i].eigentlich == 1) && (conflicts[i].stattdessen == 2)) {
    //     //     //Gemeinsame Elternknoten und dann bis zu den Knoten
    //     //     //Gesmater subbaum
    //     // }
    //     else if (conflicts[i].eigentlich == 2) {
    //         //nächsthöhere Abzweigung
    //     }
    //     // else if ((conflicts[i].eigentlich == 2) && (conflicts[i].stattdessen == 1)) {
    //     //     //nächsthöhere abzweigung
    //     // }
    //     else {
    //         //Pfad zwsichen den beiden zu Konflik
    //     }
    // }

    //std::cout << "Ancestor of E and G: " << shared_ancestor(root, "C", "G") -> label_ << std::endl;
    std::cout << root -> children_[0] -> children_.size() << std::endl;

    tree_ausgabe(root, "graph");
    std::cout << "Test nach" << std::endl;
}

int main (int argc, char* argv[]) {

    std::vector<prob> vec;

    // std::cout << argv[1];

    std::ifstream file;

    file.open(argv[1]);

    //Einlesen der Datei aus R mit Daten und konvertieren zu struct prob
    while (file.good()) {
        std::string x,y;
        double a,b,c;

        file >> x >> y >> a >> b >> c;
        vec.push_back(prob (x,y,a,b,c));
    }

    //Sortierung nach Priorität, extra implementiert für struct prob
    std::sort(vec.begin(), vec.end(), std::greater());

    for (size_t i = 0; i < vec.size(); i++) {
        std::cout << i << ": " << vec[i].x_ << " " << vec[i].y_ << " " << vec[i].probs_[0] << " " << vec[i].probs_[1] << " " << vec[i].probs_[2] << std::endl;
    }
    
    
    // //std::cout << vec[1].x_ << " " << vec[1].y_ << std::endl;
    // //std::cout << vec[2].x_ << " " << vec[2].y_ << std::endl;


    //Tree funktion
    make_tree(vec);

}