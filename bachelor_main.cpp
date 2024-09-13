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
#include <chrono>
#include "SCITE_need/scoreTree.h"

int** getDataMatrix(int n, int m, std::string fileName);
int* getParentVectorFromGVfile(std::string fileName, int n);

struct node {

    virtual std::string ausgabe () const {}
    virtual void involved_add (std::shared_ptr<node>& a) {} 
    virtual bool check_conflict () const {}
    virtual std::vector<std::shared_ptr<node>> get_involved () const {}

    void children_add (const std::vector<std::shared_ptr<node>>& new_child) {
        for (const auto & child : new_child) {
            this -> children_.push_back(child);
        }
    }

    void add_depth(const int depth) {
        depth_ = depth;
    }

    int depth_;
    std::string label_;
    std::vector<std::shared_ptr<node>> children_;
};
//Knoten Datenstruktur mit label und einen Vektor an pointer zu den jewieligen Kindern. Zwei Konstruktoren: mit name und für kopieren
struct normal_node : public node {
    normal_node (std::string name) {
        label_ = name;
        depth_ = 0;
    }

    normal_node (const normal_node& n1) {
        label_ = n1.label_;
        for (auto const & ptr : n1.children_) {
            children_.push_back(ptr);
        }
    }

    std::string ausgabe () const {
        return label_;
    }

    bool check_conflict () const {
        return false;
    }

    std::vector<std::shared_ptr<node>> get_involved () const {
        return std::vector<std::shared_ptr<node>> {nullptr};
    }
};

struct conflict_node : public node {
    conflict_node (int name) {
        label_ = "Konflikt_" + std::to_string(name);
    }

    conflict_node (const conflict_node& n1) {
        label_ = n1.label_;
        for (auto const & ptr : n1.children_) {
            children_.push_back(ptr);
        }
        for (auto const & st : n1.involved_) {
            involved_.push_back(st);
        }
    }

    std::string ausgabe () const {
        std::string ergebnis = "Konflikt_";
        for (auto const & in : involved_) {
            ergebnis = ergebnis + in -> label_ + "_";
        }

        return ergebnis;
    }

    void involved_add (std::shared_ptr<node>& a) {
        involved_.push_back(a);
    }

    bool check_conflict () const {
        return true;
    }

    std::vector<std::shared_ptr<node>> get_involved () const {
        return involved_;
    }

    std::vector<std::shared_ptr<node>> involved_;
};

int to_num (const std::shared_ptr<node> & node) {
    if (node -> label_ == "root") {
        return 0;
    }
    else {
        return atoi(node -> label_.c_str());
    }
}

int find_num (const std::shared_ptr<node>& root) {
    std::deque<std::shared_ptr<node>> stack {root};
    int ergebnis {0};

    while(!stack.empty()) {
        std::shared_ptr curr = stack[0];
        stack.pop_front();
        ergebnis++;

        for (auto const & child : curr -> children_) {
            stack.push_back(child);
        }
    }
    return ergebnis;
}

//Suchfunktion, fängt bei root an und sucht nach query. Tiefensuche
//Returned pointer auf den node
std::shared_ptr<node> find_node (const std::shared_ptr<node> & root, std::string query) {

    std::deque<std::shared_ptr<node>> stack {root};

    while (!stack.empty()) {
        std::shared_ptr<node> curr = stack[0];
        stack.pop_front();
        // std::cout << "Curr: " << curr -> label_ << std::endl;

        if (curr -> check_conflict()) {
            std::vector<std::shared_ptr<node>> inv = curr -> get_involved();
            // std::cout << "Inv size: " << inv.size() << std::endl;
            for (auto const & child : inv) {
                if (child -> label_ == query) {
                    return curr;
                }
            }
        }
        else if (curr -> label_ == query) {
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

int* to_int_stern (std::vector<int> parent_vec) {
    int* vec = new int [parent_vec.size()];

    for (int i = 0; i < parent_vec.size(); i++) {
        vec[i] = parent_vec[i];
    }

    return vec;
}

std::shared_ptr<node> to_tree (std::vector<int> parent_vec) {

    int num = parent_vec.size();
    std::vector<std::vector<int>> nodes (num+1, std::vector<int> {-1});
    // nodes[num];

    for(int i = 0; i < num; i++) {
        if(parent_vec[i] == -1) {
            continue;
        }
        else if (nodes[parent_vec[i]][0] == -1) {
            nodes[parent_vec[i]].clear();
            nodes[parent_vec[i]].push_back(i);
        }
        else {
            nodes[parent_vec[i]].push_back(i);
        }
    }


    std::shared_ptr<node> root = std::shared_ptr<normal_node> (new normal_node ("root"));
    std::deque<int> stack;
    std::shared_ptr<node> curr_point = root;

    for(auto const i : nodes[num]) {
        stack.push_back(i);
        std::shared_ptr<node> new_node (new normal_node (std::to_string(i+1)));
        curr_point -> children_.push_back(new_node);
    }

    while(!stack.empty()) {
        int curr_int = stack[0];
        stack.pop_front();

        curr_point = find_node(root, std::to_string(curr_int+1));

        for (auto const & i : nodes[curr_int]) {
            if(i == -1) {
                continue;
            }
            stack.push_back(i);
            std::shared_ptr<node> new_node (new normal_node (std::to_string(i+1)));
            curr_point -> children_.push_back(new_node);
        }
    }
    return root;
}

std::vector<int> to_parent_vec (const std::shared_ptr<node>& root, int num) {
    std::deque<std::shared_ptr<node>> stack;
    // int num = find_num(root) -1 ;
    std::vector<int> parent_vec (num, -1);

    for(auto const & child : root -> children_) {
        parent_vec[atoi(child->label_.c_str())-1] = num;

        stack.push_back(child);
    }
    
    while (!stack.empty()) {
        std::shared_ptr<node> curr = stack[0];
        stack.pop_front();
        for (auto const & child : curr -> children_) {
            stack.push_back(child);
            parent_vec[atoi(child->label_.c_str())-1] = atoi(curr->label_.c_str())-1;
        }
    }

    return parent_vec;
}

//prob = Wahrscheinlihckeiten, X -> Y, X <- Y, X <-> Y
//Gespeichert als Array der Größe 3, mehr nie benötigt
//maxmimum ist 0,1 oder 2, Stelle im Array
// Prio = Maximum - anderen beide
struct prob {

    prob (std::string x, std::string y, double xvory, double yvorx, double parallel, double prio) : x_(x), y_(y) {

        probs_[0] = xvory;
        probs_[1] = yvorx;
        probs_[2] = parallel;
        prio_ = prio;

        double max = std::max(std::max(probs_[0], probs_[1]), probs_[2]);

        if (max == probs_[0]) {
            if(max == probs_[1]) {
                max_ = 10;
            }
            else if (max == probs_[2]) {
                max_ = 20;
            }
            else {
                max_= 0;
            }
            
        }
        else if (max == probs_[1]) {
            if (max == probs_[2]) {
                max_ = 21;
            }
            else {
                max_ = 1;
            }
            
        }
        else {
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

    conflict (std::string a, std::string b, int eig, int statt) {
        involved = std::pair<std::string, std::string> {a,b};
        eigentlich = eig;
        stattdessen = statt;
    }

    void clear () {
        involved = std::pair<std::string, std::string> {"", ""};
        eigentlich = 3;
        stattdessen = 3;
    }

    //std::vector<std::shared_ptr<node>> involved;
    std::pair<std::string, std::string> involved;
    int eigentlich;
    int stattdessen;
};

void konflikt_ausgabe (const std::vector<conflict> & vec) {

    if(vec.empty()) {
        std::cout << "Konflikte: Keine" << std::endl;
    }
    else {
        std::cout << "Konflikte: " << std::endl;
        for (auto const & confi : vec) {
            std::cout << confi.involved.first << ", " << confi.involved.second << "     Eigentlich: " << confi.eigentlich << "     Stattdessen: " << confi.stattdessen  << std::endl;
        }
    }
}

//Größer und kleiner Operator für probs
bool operator< (const prob & a, const prob & b) {
    return a.prio_ < b.prio_;
}

bool operator> (const prob & a, const prob & b) {
    return a.prio_ > b.prio_;
}

bool operator== (const prob & a, const prob & b) {
    return ((a.x_ == b.x_) && (a.y_ == b.y_));
}

bool operator< (const conflict & a, const conflict & b) {
    if (a.involved.first != b.involved.first) {return a.involved.first < b.involved.first;}
    else {return a.involved.second < b.involved.second;}
}

bool operator== (const conflict & a, const conflict & b) {
    return ((a.involved.first == b.involved.first) && (a.involved.second == b.involved.second));
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
        for (size_t i = 0; i < curr -> children_.size(); i++) {
            stack.push_back(curr -> children_[i]);
            dotfile << curr -> ausgabe() << " -> " << curr -> children_[i] -> ausgabe() << std::endl;
        }
    }
    dotfile << "}";

    dotfile.close();

    std::string command = "dot -Tsvg " + name + ".dot > " + name + ".svg";

    // const char * command = "dot -Tsvg " + dotfile + ""

    system(command.c_str());

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

std::shared_ptr<node> abzweigung (const std::shared_ptr<node> & root, const std::string a, const std::string b) {
    std::deque<std::shared_ptr<node>> stack {root};
    std::pair<std::pair<bool, bool>, std::shared_ptr<node>> zweig {{false, false}, root};
    bool a_found = false;
    bool b_found = false;

    while (!stack.empty()) {

        std::shared_ptr<node> curr = stack[0];
        stack.pop_front();

        for (size_t i = 0; i < curr -> children_.size(); i++) {
            stack.push_front(curr -> children_[i]);
        }

        a_found = (curr -> label_ == a);
        b_found = (curr -> label_ == b);

        if (curr -> children_.size() > 0) {
            zweig.second = curr;
            zweig.first.first = a_found;
            zweig.first.second = b_found;
        }

        if (a_found && b_found) {
            return zweig.second;
        }
        else if (zweig.first.first) {
            return find_node(root, a);
        }
        else if (zweig.first.second) {
            return find_node(root,b);
        }
    }

    return nullptr;
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

std::vector<std::shared_ptr<node>> pfad(const std::shared_ptr<node> & root, const std::string a, const std::string b) {
    std::deque<std::shared_ptr<node>> stack {root};
    bool a_found = false;
    bool b_found = false;
    std::vector<std::shared_ptr<node>> result;

    while (!stack.empty()) {
        std::shared_ptr<node> curr = stack[0];
        stack.pop_front();

        for(size_t i = 0; i < curr -> children_.size(); i++) {
            stack.push_front(curr -> children_[i]);
        }

        if((!finden(subtree(curr),a) && !finden(subtree(curr),b)) || (a_found && !finden(subtree(curr),b)) || (b_found && !finden(subtree(curr),a))) {
            continue;
        }

        a_found = curr -> label_ == a || a_found;
        b_found = curr -> label_ == b || b_found;

        if(a_found && b_found) {
            result.push_back(curr);
            return result;
        }
        else if (!a_found && !b_found) {
            continue;
        }
        else {
            result.push_back(curr);
        }

    }

}

void collapse_conflict (const std::shared_ptr<node>& root, const std::string in, const std::string out, int eigentlich, int statt, std::deque<std::shared_ptr<node>>& in_conflicts) {
    std::shared_ptr<node> conflict_found = find_node(root, in);
    // tree_ausgabe(root, "test_collapse_conflict");
    // std::cout << "Collapse, vor if" << std::endl;
    // std::cout << "Statt: " << statt << std::endl;
    // std::cout << "Eig: " << eigentlich << std::endl;
    // std::cout << "Out: " << out << std::endl;
    // std::cout << "Konflikt: " << (conflict_found == nullptr) << std::endl;
    if (statt == 2) {
        //Gemeinsame Elternknoten und dann bis zu den Knoten
        //Gesmater subbaum
        // std::cout << "Test 1" << std::endl;
        std::shared_ptr<node> sub_root = shared_ancestor(root, conflict_found -> label_, out);
        // std::cout << "Test 2" << std::endl;
        std::deque<std::shared_ptr<node>> sub_tree = subtree(sub_root);
        // std::cout << "Subtree size: " << sub_tree.size() << std::endl;
        std::shared_ptr<node> parent = find_parent(sub_root, root);

        if (sub_root != root) {
            parent -> children_.erase(std::find(parent -> children_.begin(), parent -> children_.end(), sub_root));
        }
        else {
            sub_tree.pop_front();
        }
        
        for (size_t j = 0; j < sub_tree.size(); j++) {
            // conflictnode -> children_.push_back(sub_tree[j]);
            auto position = std::find(root -> children_.begin(), root -> children_.end(), sub_tree[j]);
            if (position != root -> children_.end()) {
                root -> children_.erase(position);
            }

            if (sub_tree[j] == conflict_found) {
                continue;
            }
            conflict_found -> involved_add(sub_tree[j]);
            sub_tree[j] -> children_.clear();
            in_conflicts.push_back(sub_tree[j]);     
        }

        parent -> children_.push_back(conflict_found);
    }
    else if (eigentlich == 2) {
        //nächsthöhere Abzweigung
        std::shared_ptr<node> sub_root = abzweigung(root, out, conflict_found -> label_);
        std::deque<std::shared_ptr<node>> sub_tree = subtree(sub_root);
        std::shared_ptr<node> parent = find_parent(sub_root, root);

        if (sub_root != root) {
            parent -> children_.erase(std::find(parent -> children_.begin(), parent -> children_.end(), sub_root));
        }
        else {
            sub_tree.pop_front();
        }

        
        for (size_t j = 0; j < sub_tree.size(); j++) {
            conflict_found -> involved_add(sub_tree[j]);
            sub_tree[j] -> children_.clear();
            in_conflicts.push_back(sub_tree[j]);

            auto position = std::find(root -> children_.begin(), root -> children_.end(), sub_tree[j]);
            if (position != root -> children_.end()) {
                root -> children_.erase(position);
            }
        }

        parent -> children_.push_back(conflict_found);
    }
    else {
        //Pfad zwsichen den beiden zu Konflik
        std::vector<std::shared_ptr<node>> path = pfad(root, out, conflict_found -> label_);
        std::shared_ptr<node> parent = find_parent(path[0], root);

        for (size_t j = 0; j < path.size(); j++) {
            conflict_found -> involved_add(path[j]);
            in_conflicts.push_back(path[j]);

            if (j == path.size()-1) {
                conflict_found -> children_add(path[j] -> children_);
                path[j] -> children_.clear();
            }
            else {
                for(auto const child : path[j] -> children_) {
                    if (child != path[j+1]) {
                        conflict_found -> children_.push_back(child);
                    }
                }
                path[j] -> children_.clear();
            }

            auto position = std::find(root -> children_.begin(), root -> children_.end(), path[j]);
            if (position != root -> children_.end()) {
                root -> children_.erase(position);
            }
        }

        parent -> children_.push_back(conflict_found);
    }
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

// std::deque<std::shared_ptr<node>> get_ancestors(const std::shared_ptr<node> & curr, const std::shared_ptr<node> & root) {

//     std::deque<std::shared_ptr<node>> stack {find_parent(curr, root)};

//     for(int i = 0; i < stack.size(); i++) {
//         std::shared_ptr<node> current = stack[i];
//         stack.push_back(find_parent(current, root));
//     }

//     return stack;
    
// }

// std::pair<std::shared_ptr<node>, std::vector<std::shared_ptr<node>>> find_place_para(std::vector<conflict> & all_conflicts, 
//                                                                     const std::shared_ptr<node> & root,
//                                                                     const std::vector<prob> & probs,
//                                                                     std::vector<std::string> & in,
//                                                                     const std::string & neu) {

//     std::deque<std::shared_ptr<node>> stack = subtree(root);
//     double max {-10000000000};
//     bool confli = false;
//     std::vector<std::vector<conflict>> conflicts (in.size()+1); 
//     std::vector<std::pair<std::shared_ptr<node>, std::vector<std::shared_ptr<node>>>> possible_places (in.size()+1, std::pair<std::shared_ptr<node>, std::vector<std::shared_ptr<node>>> {nullptr, std::vector<std::shared_ptr<node> {nullptr}});
//     std::vector<double> max_from_thread (in.size()+1);

//     #pragma omp parallel for num_threads(10)
//     for (int i = 0; i < stack.size(); i++) {
//         std::deque<std::shared_ptr<node>> visited = get_ancestors(stack[i], root);

//         std::vector<std::vector<std::shared_ptr<node>>> possi = possibilities(stack[i]);
//         bool local_con;
//         std::vector<conflict> local_conflicts;
//         double local_max {-10000000000};

//         for (const auto & child : possi) {
//             local_con = false;
//             local_conflicts.clear();

//             double value = 1;
//             std::vector<std::string> nach;
//             for (size_t i = 0; i < child.size(); i++) {
//                 std::deque<std::shared_ptr<node>> sub_tree_local = subtree(child[i]);

//                 for(auto const & inn : sub_tree_local) {
//                     nach.push_back(inn -> label_);
//                 }
//             }

//             for (size_t i = 0; i < probs.size(); i++) {

//                 if((probs[i].x_ == neu) && (finden(visited, probs[i].y_))) {
//                     value += probs[i].probs_[1];    
//                     if (probs[i].max_ != 1 && probs[i].max_ != 10 && probs[i].max_ != 21) {
//                         local_con = true;
//                         local_conflicts.push_back(conflict (probs[i].x_, probs[i].y_, probs[i].max_, 1));
//                     }
//                 }
//                 else if ((probs[i].y_ == neu) && (finden(visited, probs[i].x_))) {
//                     value += probs[i].probs_[0];
//                     if (probs[i].max_ != 0 && probs[i].max_ != 10 && probs[i].max_ != 20) {
//                         local_con = true;
//                         local_conflicts.push_back(conflict (probs[i].x_, probs[i].y_, probs[i].max_, 0));
//                     }
//                 }
//                 else if((probs[i].x_ == neu) && (std::find(nach.begin(), nach.end(), probs[i].y_) != nach.end())) {
//                     value += probs[i].probs_[0];   
//                     if (probs[i].max_ != 0 && probs[i].max_ != 10 && probs[i].max_ != 20) {
//                         local_con = true;
//                         local_conflicts.push_back(conflict (probs[i].x_, probs[i].y_, probs[i].max_, 0));
//                     } 
//                 }
//                 else if ((probs[i].y_ == neu) && (std::find(nach.begin(), nach.end(), probs[i].x_) != nach.end())) {
//                     value += probs[i].probs_[1];
//                     if (probs[i].max_ != 1 && probs[i].max_ != 10 && probs[i].max_ != 21) {
//                         local_con = true;
//                         local_conflicts.push_back(conflict (probs[i].x_, probs[i].y_, probs[i].max_, 1));
//                     }
//                 }
//                 else if (((probs[i].x_ == neu) && (std::find(in.begin(), in.end(), probs[i].y_) != in.end())) || ((probs[i].y_ == neu) && (std::find(in.begin(), in.end(), probs[i].x_) != in.end()))) {
//                     value += probs[i].probs_[2];
//                     if (probs[i].max_ != 2 && probs[i].max_ != 20 && probs[i].max_ != 21) {
//                         local_con = true;
//                         local_conflicts.push_back(conflict (probs[i].x_, probs[i].y_, probs[i].max_, 2));
//                     }
//                 }
//                 else {
//                     continue;
//                 }
//             }


//             if (value > local_max) {
//                 local_max = value;
//                 possible_places[to_num(stack[i])].first = stack[i];
//                 possible_places[to_num(stack[i])].second = child;

//                 if (local_con) {
//                     confli = local_con;
//                     conflicts[to_num(stack[i])] = local_conflicts;
//                 }
//                 else {
//                     confli = false;
//                     conflicts[to_num(stack[i])].clear();
//                 }
                
//             }
//         }

//         max_from_thread[to_num(stack[i])] = local_max;

//     }

//     int pos {0};
//     for(int i = 0; i < max_from_thread.size(); i++) {
//         if(max_from_thread[i] > max) {
//             pos=i;
//         }
//     }


//     return possible_places[pos];
// }


//Ausprobieren aller möglichen Stellen an denen neu eingefügt werden könnte, wahrscheinlichste wird zurückgegeben
//Paar: 1. Elternknoten 2. mögliche Kinder
//2. ist initalisiert mit nullptr, ansonsten ersetzt durch die Möglichkeit aus possibilities die am wahrscheinlichsten ist
std::pair<std::shared_ptr<node>, std::vector<std::shared_ptr<node>>> find_place(std::vector<conflict> & all_conflicts, 
                                                                    const std::shared_ptr<node> & root,
                                                                    const std::vector<prob> & probs,
                                                                    std::vector<std::string> & in,
                                                                    const std::string & neu) {

    std::deque<std::shared_ptr<node>> stack {root};
    std::deque<std::shared_ptr<node>> visited;
    double max {-10000000000};
    std::pair<std::shared_ptr<node>, std::vector<std::shared_ptr<node>>> place {nullptr, std::vector<std::shared_ptr<node>> {nullptr}};
    bool confli = false;
    std::vector<conflict> conflicts; 
    // std::cout << "Probs anfnag: " << probs[anfang].x_ << " " << probs[anfang].y_ << " " << probs[anfang].probs_[2] << std::endl;
    // std::cout << "Neu: " << neu << std::endl;

    //Tiefensuche
    while (!stack.empty()) {
        std::shared_ptr<node> curr = stack[0];
        stack.pop_front();
        bool local_con = false;
        std::vector<conflict> local_conflicts;
        // int local_eigent {0};
        // int local_statt {0};

        for (size_t i = 0; i < curr -> children_.size(); i++) {
            stack.push_front(curr -> children_[i]);
        }

        if (curr -> label_ != "root") {
            visited.push_back(curr);
        } 

        // std::cout << "Visited: ";
        // for (size_t i = 0; i < visited.size(); i++) {
        //     std::cout << visited[i] -> label_ << ", ";
        // }
        // std::cout << std::endl;

        // double value {1};
        // std::cout << "Max vorher: " << max << std::endl;
        // std::cout << neu << " -> " << curr -> label_ << std::endl;  
        
        std::vector<std::vector<std::shared_ptr<node>>> possi = possibilities(curr);

        for (const auto & child : possi) {
            local_con = false;
            local_conflicts.clear();
            // local_eigent=0;
            // local_statt=0;

            double value = 1;
            std::vector<std::string> nach;
            std::deque<std::shared_ptr<node>> lil_stack;
            for (size_t i = 0; i < child.size(); i++) {
                lil_stack.push_back(child[i]);
            }

            // std::cout << "Nach: ";
            while (!lil_stack.empty()) {
                std::shared_ptr<node> lil_curr = lil_stack[0];
                lil_stack.pop_front();

                // std::cout << lil_curr -> label_ << ", ";
                nach.push_back(lil_curr -> label_);

                for (const auto & grandchild : lil_curr -> children_) {
                    lil_stack.push_back(grandchild);
                }
            }

            // std::cout << std::endl;

            for (size_t i = 0; i < probs.size(); i++) {
                // std::cout << "i: " << i << std::endl;
                // if((probs[i].x_ == "B")) {
                //     std::cout << "B da?: " << finden(visited, probs[i].x_) << std::endl;
                // }

                if((probs[i].x_ == neu) && (finden(visited, probs[i].y_))) {
                    value += probs[i].probs_[1];    
                    if (probs[i].max_ != 1 && probs[i].max_ != 10 && probs[i].max_ != 21) {
                        local_con = true;
                        local_conflicts.push_back(conflict (probs[i].x_, probs[i].y_, probs[i].max_, 1));
                    }
                }
                else if ((probs[i].y_ == neu) && (finden(visited, probs[i].x_))) {
                    value += probs[i].probs_[0];
                    if (probs[i].max_ != 0 && probs[i].max_ != 10 && probs[i].max_ != 20) {
                        local_con = true;
                        local_conflicts.push_back(conflict (probs[i].x_, probs[i].y_, probs[i].max_, 0));
                    }
                }
                else if((probs[i].x_ == neu) && (std::find(nach.begin(), nach.end(), probs[i].y_) != nach.end())) {
                    value += probs[i].probs_[0];   
                    if (probs[i].max_ != 0 && probs[i].max_ != 10 && probs[i].max_ != 20) {
                        local_con = true;
                        local_conflicts.push_back(conflict (probs[i].x_, probs[i].y_, probs[i].max_, 0));
                    } 
                }
                else if ((probs[i].y_ == neu) && (std::find(nach.begin(), nach.end(), probs[i].x_) != nach.end())) {
                    value += probs[i].probs_[1];
                    if (probs[i].max_ != 1 && probs[i].max_ != 10 && probs[i].max_ != 21) {
                        local_con = true;
                        local_conflicts.push_back(conflict (probs[i].x_, probs[i].y_, probs[i].max_, 1));
                        // std::cout << "Bei: " << probs[i].x_ << " und " << probs[i].y_ << " mit value: " << value << " und max: " << max << std::endl;
                    }
                }
                else if (((probs[i].x_ == neu) && (std::find(in.begin(), in.end(), probs[i].y_) != in.end())) || ((probs[i].y_ == neu) && (std::find(in.begin(), in.end(), probs[i].x_) != in.end()))) {
                    value += probs[i].probs_[2];
                    if (probs[i].max_ != 2 && probs[i].max_ != 20 && probs[i].max_ != 21) {
                        local_con = true;
                        local_conflicts.push_back(conflict (probs[i].x_, probs[i].y_, probs[i].max_, 2));
                        // std::cout << "E5" << std::endl;
                    }
                }
                else {
                    continue;
                }
            }

            // if(anfang == 35) {std::cout << "Value before new Kante: " << value << std::endl;}

            if (value > max) {
                // std::cout << neu << " curr: "<< curr -> label_ << " mit value: " << value << " und max: " << max << std::endl;
                max = value;
                place.first = curr;
                place.second = child;

                if (local_con) {
                    confli = local_con;
                    // std::cout << "NEW CONFLICT" << std::endl;
                    // std::cout << "involved: " << local_conflicts.first << " " << local_conflicts.second << std::endl;
                    // std::cout << "Eig: " << local_eigent << " Statt: " << local_statt << " Fall: " << fall << std::endl;
                    conflicts = local_conflicts;
                }
                else {
                    confli = false;
                    conflicts.clear();
                }
                
            }
            // std::cout << "Max nachher: " << max << std::endl;
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
        // std::cout << "Es gibt einen Konflikt bei: " << std::endl;

        for (auto const & confi : conflicts) {
            // std::cout << confi.involved.first << ", " << confi.involved.second << "     Eigentlich: " << confi.eigentlich << "     Stattdessen: " << confi.stattdessen  << std::endl;
            all_conflicts.push_back(confi);
        }
        
    }
    return place;
}

// [Rcpp::export]
std::shared_ptr<node> make_tree (std::vector<prob> probs, std::vector<conflict> & conflicts, 
                                std::vector<std::string>& in,
                                std::string zuletzt, std::vector<std::vector<int>>& parent_vec, int n) {
    
    std::shared_ptr<node> root (new normal_node (std::string ("root")));
    std::vector<prob> needed {};
    // std::vector<std::string> in;

    int zahler {0};
    if (!zuletzt.empty()) {
        
        std::vector<std::string> local_in;
        std::vector<std::vector<int>> local_parent_vec;

        for (; zahler < in.size(); zahler++) {
            if(in[zahler] == zuletzt) {
                break;
            }
            
            else {
                local_in.push_back(in[zahler]);
                local_parent_vec.push_back(parent_vec[zahler]);
            }
        }
        in = local_in;
        parent_vec = local_parent_vec;

        if (zahler >= 2) {
            root = to_tree(parent_vec[zahler-1]);  
        }
    }

    if (zuletzt.empty() || zahler < 2) {
        in.clear();
        //Initialiserung mit erstem Paar
        for (int i = 0; i < probs.size(); i++) {

            if (probs[i].x_ == zuletzt || probs[i].y_ == zuletzt) {
                needed.push_back(probs[i]);
                probs.erase(std::find(probs.begin(), probs.end(), probs[i]));
                continue;
            }
            else {
                std::shared_ptr<node> x_ptr (new normal_node (probs[i].x_));
                std::shared_ptr<node> y_ptr (new normal_node (probs[i].y_));

                if (probs[i].max_ == 0 || probs[0].max_ == 10 || probs[i].max_ == 20) {
                    x_ptr -> add_depth(1);
                    y_ptr -> add_depth(2);

                    x_ptr -> children_.push_back(y_ptr);
                    root -> children_.push_back(x_ptr);
                } 
                else if (probs[i].max_ == 1 || probs[i].max_ == 11 || probs[i].max_ == 21) {
                    x_ptr -> add_depth(2);
                    y_ptr -> add_depth(1);

                    y_ptr -> children_.push_back(x_ptr);
                    root -> children_.push_back(y_ptr);
                }
                else {
                    x_ptr -> add_depth(1);
                    y_ptr -> add_depth(1);

                    root -> children_.push_back(x_ptr);
                    root -> children_.push_back(y_ptr);
                }

                in.push_back(probs[i].x_);
                in.push_back(probs[i].y_);
                
                probs.erase(std::find(probs.begin(), probs.end(), probs[i]));

                break;
            }
        }
        //Zwei damit einfacher
        parent_vec.clear();
        parent_vec.push_back(to_parent_vec(root,n));
        parent_vec.push_back(to_parent_vec(root,n));
    }
    
    // std::vector<int*> parent_vecs {to_parent_vec(root)};
    // std::vector<conflict> conflicts;


    //Geht probs durhc und schut ob x oder y schon im Tree vorhanden sind
    //Wenn beide ja -> continue
    //Wenn nur eins ja -> wird an find_place übergeben und in den Tree eingefügt
    //Wenn kein ja -> continue und Wahrscheinlichkeit wird erstmal ignoriert

    // int anfang {1};
    int i {0};
    while(i < probs.size()) {
        std::pair<std::shared_ptr<node>, std::vector<std::shared_ptr<node>>> place;

        if ((probs[i].x_ == zuletzt) || (probs[i].y_ == zuletzt)) {
            // std::cout << "Fall0" << std::endl;
            needed.push_back(probs[i]);
            probs.erase(std::find(probs.begin(), probs.end(), probs[i]));
            i = 0;
            continue;
        }
        else if ((std::find(in.begin(), in.end(), probs[i].x_) != in.end()) && (std::find(in.begin(), in.end(), probs[i].y_) != in.end())) {
            // std::cout << "Fall 1" << std::endl;
            probs.erase(std::find(probs.begin(), probs.end(), probs[i]));
            i = 0;
            continue;
        }
        else if((std::find(in.begin(), in.end(), probs[i].x_) != in.end())) {
            // std::cout << "Fall 2" << std::endl;
            place = find_place(conflicts, root, probs, in, probs[i].y_);
            in.push_back(probs[i].y_);
            probs.erase(std::find(probs.begin(), probs.end(), probs[i]));
            i = 0;
        }
        else if ((std::find(in.begin(), in.end(), probs[i].y_) != in.end())) {
            // std::cout << "Fall 3" << std::endl;
            place = find_place(conflicts, root, probs, in, probs[i].x_);
            in.push_back(probs[i].x_);
            probs.erase(std::find(probs.begin(), probs.end(), probs[i]));
            i = 0;
        }
        else {
            // std::cout << "Fall 4" << std::endl;
            i++;
            continue;
        }
        
        //Neuer node
        std::shared_ptr<node> new_node (new normal_node (in[in.size()-1]));

        std::cout << "New Node: " << in[in.size()-1] << std::endl;
        new_node -> add_depth(place.first -> depth_+1);
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
            // std::cout << "i: " << i << " mit else" << std::endl;
            // std::cout << "First: " << (place.first == nullptr) << " und second: " << place.second.size() << std::endl;
            // std::cout << (place.first == nullptr) << std::endl;
            place.first -> children_.push_back(new_node);
            // std::cout << "Test in else" << std::endl;
        }
        // std::cout << "Test nach einfugen" << std::endl;
        // tree_ausgabe(root);
        parent_vec.push_back(to_parent_vec(root,n));
    }

    // std::cout << "Konflikt size: " << conflicts.size() << std::endl;
    // tree_ausgabe(root, "graph_vorher");
    // std::vector<std::pair<int,std::string>> parent_vec = to_parent_vec(root);

    // bool konflikte = !(conflicts.size() == 0);

    // std::vector<int> parent_vec = to_parent_vec(root);
    // std::cout << "Große: " << parent_vec.size() << std::endl;
    // for (auto const & ins : parent_vec) {
    //     std::cout << ins << ", ";
    // }
    // std::cout << std::endl;

    // std::deque<std::shared_ptr<node>> in_conflicts;
    // int num_con {0};

    // for (size_t i = 0; i < conflicts.size(); i++) {
    //     // std::string name = "graph_vorher" + std::to_string(i);
    //     // tree_ausgabe(root,name);
    //     if (finden(in_conflicts, conflicts[i].involved.first) && finden(in_conflicts, conflicts[i].involved.second)) {
    //         // std::cout << "Übersprungen" << std::endl;
    //         continue;
    //     }
    //     else if (finden(in_conflicts, conflicts[i].involved.first)) {
    //         // std::cout << "Collapse 1" << std::endl;
    //         collapse_conflict(root, conflicts[i].involved.first, conflicts[i].involved.second, conflicts[i].eigentlich, conflicts[i].stattdessen, in_conflicts);
    //     }
    //     else if (finden(in_conflicts, conflicts[i].involved.second)) {
    //         // std::cout << "Collapse2" << std::endl;
    //         collapse_conflict(root, conflicts[i].involved.second, conflicts[i].involved.first, conflicts[i].eigentlich, conflicts[i].stattdessen, in_conflicts);
    //         // std::cout << "Collaps 2 danach" << std::endl;
    //     }
    //     else { 
    //         std::shared_ptr<node> conflictnode (new conflict_node (num_con));
    //         num_con++;

    //         if (conflicts[i].stattdessen == 2) {
    //             //Gemeinsame Elternknoten und dann bis zu den Knoten
    //             //Gesmater subbaum

    //             std::shared_ptr<node> sub_root = shared_ancestor(root, conflicts[i].involved.first, conflicts[i].involved.second);
    //             std::deque<std::shared_ptr<node>> sub_tree = subtree(sub_root);
    //             std::shared_ptr<node> parent = find_parent(sub_root, root);

    //             if (sub_root != root) {
    //                 parent -> children_.erase(std::find(parent -> children_.begin(), parent -> children_.end(), sub_root));
    //             }
    //             else {
    //                 sub_tree.pop_front();
    //             }

    //             // std::cout << "Subtree size: " << sub_tree.size() << std::endl;

                
    //             for (size_t j = 0; j < sub_tree.size(); j++) {
    //                 // conflictnode -> children_.push_back(sub_tree[j]);
    //                 conflictnode -> involved_add(sub_tree[j]);
    //                 sub_tree[j] -> children_.clear();
    //                 in_conflicts.push_back(sub_tree[j]);

    //                 auto position = std::find(root -> children_.begin(), root -> children_.end(), sub_tree[j]);
    //                 if (position != root -> children_.end()) {
    //                     root -> children_.erase(position);
    //                 }
    //             }

    //             parent -> children_.push_back(conflictnode);
    //         }
    //         else if (conflicts[i].eigentlich == 2) {
    //             //nächsthöhere Abzweigung
    //             std::shared_ptr<node> sub_root = abzweigung(root, conflicts[i].involved.first, conflicts[i].involved.second);
    //             std::deque<std::shared_ptr<node>> sub_tree = subtree(sub_root);
    //             std::shared_ptr<node> parent = find_parent(sub_root, root);

    //             if (sub_root != root) {
    //                 parent -> children_.erase(std::find(parent -> children_.begin(), parent -> children_.end(), sub_root));
    //             }
    //             else {
    //                 sub_tree.pop_front();
    //             }

    //             // std::cout << "Subtree size: " << sub_tree.size() << std::endl;

                
    //             for (size_t j = 0; j < sub_tree.size(); j++) {
    //                 conflictnode -> involved_add(sub_tree[j]);
    //                 sub_tree[j] -> children_.clear();
    //                 in_conflicts.push_back(sub_tree[j]);

    //                 auto position = std::find(root -> children_.begin(), root -> children_.end(), sub_tree[j]);
    //                 if (position != root -> children_.end()) {
    //                     root -> children_.erase(position);
    //                 }
    //             }

    //             parent -> children_.push_back(conflictnode);
    //         }
    //         else {
    //             //Pfad zwsichen den beiden zu Konflik
    //             std::vector<std::shared_ptr<node>> path = pfad(root, conflicts[i].involved.first, conflicts[i].involved.second);
    //             std::shared_ptr<node> parent = find_parent(path[0], root);
    //             auto position_par = std::find(parent -> children_.begin(), parent -> children_.end(), path[0]);
    //             parent -> children_.erase(position_par);

    //             for (size_t j = 0; j < path.size(); j++) {
    //                 conflictnode -> involved_add(path[j]);
    //                 in_conflicts.push_back(path[j]);

    //                 if (j == path.size()-1) {
    //                     conflictnode -> children_add(path[j] -> children_);
    //                     path[j] -> children_.clear();
    //                 }
    //                 else {
    //                     for(auto const child : path[j] -> children_) {
    //                         if (child != path[j+1]) {
    //                             conflictnode -> children_.push_back(child);
    //                         }
    //                     }
    //                     path[j] -> children_.clear();
    //                 }

    //                 auto position = std::find(root -> children_.begin(), root -> children_.end(), path[j]);
    //                 if (position != root -> children_.end()) {
    //                     root -> children_.erase(position);
    //                 }
    //             }

    //             parent -> children_.push_back(conflictnode);
    //         }
    //     }
    // }

    // double** logscores = getLogScores(fp,fn,0,0);

    // Eventuell dynamsiche Anpassung der Datenmatrix
    // std::string datamat;
    // std::string param = 
    // if (noise) {
    //    datamat = "D:/Uni/Sommersemester_24/Bachelorarbeit/code/data/data_noise/" + param;
    // } 
    // else {
    //     datamat = "D:/Uni/Sommersemester_24/Bachelorarbeit/code/data/data_norm/" + param;
    // }

    // int** datamatrix = getDataMatrix(mut,cells,datamat);

    // if (konflikte) {
    //     double score = scoreTreeAccurate(mut,cells,logscores,datamatrix,'m',to_int_stern(parent_vec));
    //     //vorne Mutation, hinten Zellen
    //     double score_vorher = scoreTreeAccurate(mut,cells,logscores,datamatrix,'m',to_int_stern(parent_vec));
    //     std::cout << "Score vorher: " << score_vorher << std::endl;
    // }
    // else {
    //     // int* parent_vec = to_parent_vec(root);

    //     //vorne Mutation, hinten Zellen
    //     double score = scoreTreeAccurate(mut,cells,logscores,datamatrix,'m',to_int_stern(parent_vec));
    //     std::cout << "Score: " << score << std::endl;
    // }

    // double score = scoreTreeAccurate(mut,cells,logscores,datamatrix,'m',to_int_stern(parent_vec));
    // std::cout << "Score: " << score;

    //Zuletzt hinzufügen

    if (!zuletzt.empty()) {
        std::pair<std::shared_ptr<node>, std::vector<std::shared_ptr<node>>> place = find_place(conflicts, root, needed, in, zuletzt);
        in.push_back(zuletzt);

        // std::cout << "Needed: " << std::endl;
        // for(int i =0; i < needed.size(); i++) {
        //     std::cout << needed[i].x_ << ", " << needed[i].y_ << std::endl;
        // }

        // std::cout << "Place child size: " << (place.first -> label_) << std::endl;

        std::shared_ptr<node> new_node (new normal_node (in[in.size()-1]));
        new_node -> add_depth(place.first -> depth_+1);
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
            // std::cout << "i: " << i << " mit else" << std::endl;
            // std::cout << "First: " << (place.first == nullptr) << " und second: " << place.second.size() << std::endl;
            // std::cout << (place.first == nullptr) << std::endl;
            place.first -> children_.push_back(new_node);
            // std::cout << "Test in else" << std::endl;
        }
    }
    return root;
}

int main (int argc, char* argv[]) {

    std::vector<prob> vec;

    std::ifstream filenames;
    filenames.open(argv[1]);
    std::string data;
    std::string datamat;
    double fn;
    double fp;
    int n;
    int m;
    std::string ori_file;
    std::string kim_simon_file;
    std::string scite_file;

    filenames >> data;
    filenames >> datamat;
    filenames >> fn;
    filenames >> fp;
    filenames >> n;
    filenames >> m;
    filenames >> ori_file;
    filenames >> kim_simon_file;
    filenames >> scite_file;
    
    // std::cout << "FN: " << fn << ", FP: " << fp << ", Mutationen: " << n << ", Zellen: " << m << std::endl;

    // std::vector<int> fehlend;
    // while (filenames.good()) {
    //     int fehl;

    //     filenames >> fehl;
    //     fehlend.push_back(fehl);
    // }

    // if (fehlend.empty()) {
    //     std::cout << "Keine Mutationen entfernt" << std::endl;
    // } 
    // else {
    //     std::cout << "Entfernte Mutationen: ";
    //     for (auto const & fehl : fehlend) {
    //         std::cout << fehl << ", ";
    //     }
    //     std::cout << std::endl;
    // }

    std::ifstream file;

    file.open(data);

    //Einlesen der Datei aus R mit Daten und konvertieren zu struct prob
    while (file.good()) {
        std::string x,y;
        double a,b,c,d;

        file >> x >> y >> a >> b >> c >> d;
        vec.push_back(prob (x,y,a,b,c,d));
    }

    //Sortierung nach Priorität, extra implementiert für struct prob
    std::sort(vec.begin(), vec.end(), std::greater());

    // for (size_t i = 0; i < vec.size(); i++) {
    //     std::cout << i << ": " << vec[i].x_ << " " << vec[i].y_ << " " << vec[i].probs_[0] << " " << vec[i].probs_[1] << " " << vec[i].probs_[2] << " " << vec[i].prio_ << std::endl;
    // }
    
    
    std::vector<conflict> conflicts;
    std::string zuletzt;
    std::vector<std::vector<int>> first_parent_vec;
    std::vector<std::string> in;
    //Tree funktion
    auto start = std::chrono::high_resolution_clock::now();
    std::shared_ptr<node> root = make_tree(vec, conflicts, in, zuletzt, first_parent_vec, n);

    tree_ausgabe(root, "graph_first");
    std::cout << "Tree korrekt erstellt" << std::endl;
    // std::cout << "Original root size: " << find_num(root) << std::endl;
    std::vector<int> parent_vec = to_parent_vec(root, n);

    std::shared_ptr<node> testroot = to_tree(parent_vec);
    tree_ausgabe(testroot, "test");

    // for(int i = 0; i < n; i++) {
    //     std::cout << i << ": " << parent_vec[i] << std::endl;
    // }

    int** datamatrix = getDataMatrix(n,m,datamat);
    double** logscores = getLogScores(fp,fn,0,0);

    
    double score = scoreTreeAccurate(n,m,logscores,datamatrix,'m',to_int_stern(parent_vec));
    std::cout << "First Score: " << score << std::endl;

    std::sort(conflicts.begin(), conflicts.end(), std::less());

    // konflikt_ausgabe(conflicts);

    std::cout << "Konfliktgröße: " << conflicts.size() << std::endl;
    //Konfliktlösung
    bool going = !conflicts.empty();
    while (going) {
        std::vector<std::pair<std::string, int>> anzahl;

        for(int i = 0; i < conflicts.size(); i++) {
            std::string suche = conflicts[i].involved.first;
            auto finden = [&suche] (auto const & a) {return a.first == suche;};

            auto ite = std::find_if(anzahl.begin(), anzahl.end(), finden);
            if(ite == anzahl.end()) {
                anzahl.push_back(std::pair<std::string,int> {conflicts[i].involved.first, 1});
            }
            else {
                ite -> second++;
            }

            suche = conflicts[i].involved.second;
            auto ite2 = std::find_if(anzahl.begin(), anzahl.end(), finden);
            
            if (ite2 == anzahl.end()) {
                anzahl.push_back(std::pair<std::string,int> {conflicts[i].involved.second, 1});
            }
            else {
                ite2 -> second++;
            }
        }
        
        auto sortierung = [&] (const std::pair<std::string,int>& a, const std::pair<std::string,int>& b) 
        {if (a.second != b.second) {return a.second > b.second;}
         else {return ((find_node(root, a.first) -> depth_) < (find_node(root, b.first) -> depth_));}
        };

        std::sort(anzahl.begin(), anzahl.end(), sortierung);

        // std::cout << "Anzahl in Konflikt: " << std::endl;
        // for(auto i : anzahl) {
        //     std::cout << i.first << ", " << i.second << std::endl;
        // }


        for (int i = 0; i < anzahl.size(); i++) {
            zuletzt = (anzahl[i].first);


            // std::cout << "Zuletzt: " << zuletzt << std::endl;

            std::vector<conflict> new_conflicts;
            std::vector<std::string> new_in = in;
            std::vector<std::vector<int>> new_parent_vecs = first_parent_vec;
            std::shared_ptr<node> new_root = make_tree(vec, new_conflicts, new_in, zuletzt, new_parent_vecs, n);
            // std::cout << "New root size: " << find_num(new_root) << std::endl;

            std::vector<int> new_parent_vec = to_parent_vec(new_root, n);

            // std::cout << "Old Parent Vec: " << std::endl;

            // for(int i = 0; i < parent_vec.size(); i++) {
            //     std::cout << i << ": " << parent_vec[i].first << ", " << parent_vec[i].second << std::endl;
            // }

            // std::cout << "New Parent Vec: " << std::endl;

            // for(int i = 0; i < new_parent_vec.size(); i++) {
            //     std::cout << i << ": " << new_parent_vec[i] << std::endl;
            // }

            double new_score = scoreTreeAccurate(n,m,logscores,datamatrix,'m',to_int_stern(new_parent_vec));
            // std::cout << "New Score: " << new_score << std::endl;

            std::sort(new_conflicts.begin(), new_conflicts.end(), std::less());

            
            if(new_score > score) {
                score = new_score;
                conflicts = new_conflicts;
                root = new_root;
                going = !new_conflicts.empty();
                in = new_in;
                first_parent_vec = new_parent_vecs;

                // std::cout << "New conflicts: " << std::endl;
                // konflikt_ausgabe(new_conflicts);
                break;
            }
            else if (new_score == score) {
                continue;
            }
            else if (conflicts == new_conflicts) {
                going = false;
                break;
            }
        }

        going = false;
    }
    auto end = std::chrono::high_resolution_clock::now();
    auto diff = duration_cast<std::chrono::milliseconds>(end - start);

    bool ausgabe = argv[2];
    if(ausgabe) {tree_ausgabe(root, "graph_final");}
    std::cout << diff.count() << " ms" << std::endl;
    std::cout << "Final Score: " << score << std::endl;

    // std::ifstream test;
    // std::cout << "Name: " << scite_file << std::endl;
    // test.open(scite_file);
    // if(test.good()) {
    //     std::cout << "Test" << std::endl;
    // }

    // int* par_ori = getParentVectorFromGVfile(scite_file,n);

    // for (int i = 0; i < n; i++) {
    //     std::cout << parent_vec[i] << std::endl;
    // }
    

    double score_ori = scoreTreeAccurate(n,m,logscores,datamatrix,'m',getParentVectorFromGVfile(ori_file,n));
    double score_kim_simon = scoreTreeAccurate(n,m,logscores,datamatrix,'m',getParentVectorFromGVfile(kim_simon_file,n));
    double score_scite = scoreTreeAccurate(n,m,logscores,datamatrix,'m',getParentVectorFromGVfile(scite_file,n));

    std::cout << "Ori: " << score_ori << "   KS: " << score_kim_simon << "   SCITE: " << score_scite << std::endl;
}