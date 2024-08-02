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

    std::string label_;
    std::vector<std::shared_ptr<node>> children_;
};
//Knoten Datenstruktur mit label und einen Vektor an pointer zu den jewieligen Kindern. Zwei Konstruktoren: mit name und für kopieren
struct normal_node : public node {
    normal_node (std::string name) {
        label_ = name;
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

std::vector<int> to_parent_vec (const std::shared_ptr<node>& root) {
    std::deque<std::shared_ptr<node>> stack;
    std::vector<int> parent_vec;
    int pos = find_num(root);

    for(auto const & child : root -> children_) {
        parent_vec.push_back(pos);

        stack.push_back(child);
    }
    
    pos = 1;
    while (!stack.empty()) {
        std::shared_ptr<node> curr = stack[0];
        stack.pop_front();
        for (auto const & child : curr -> children_) {
            stack.push_back(child);
            parent_vec.push_back(pos);
        }

        pos++;

    }

    return parent_vec;
}

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
            if(max == probs_[1]) {
                prio_ = - probs_[2];
                max_ = 10;
            }
            else if (max == probs_[2]) {
                prio_ = - probs_[1];
                max_ = 20;
            }
            else {
                prio_ = probs_[0] - probs_[1] - probs_[2];
                max_= 0;
            }
            
        }
        else if (max == probs_[1]) {
            if (max == probs_[2]) {
                prio_ = - probs_[0];
                max_ = 21;
            }
            else {
                prio_ = probs_[1] - probs_[0] - probs_[2];
                max_ = 1;
            }
            
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
    // std::cout << "Probs anfnag: " << probs[anfang].x_ << " " << probs[anfang].y_ << " " << probs[anfang].probs_[2] << std::endl;
    // std::cout << "Neu: " << neu << std::endl;

    //Tiefensuche
    while (!stack.empty()) {
        std::shared_ptr<node> curr = stack[0];
        stack.pop_front();
        bool local_con = false;
        std::pair<std::string, std::string> local_conflicts;
        int local_eigent {0};
        int local_statt {0};
        std::string fall;

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
            // local_conflicts.clear();
            local_eigent=0;
            local_statt=0;
            fall.clear();

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

            for (size_t i = anfang; i < probs.size(); i++) {
                // std::cout << "i: " << i << std::endl;
                // if((probs[i].x_ == "B")) {
                //     std::cout << "B da?: " << finden(visited, probs[i].x_) << std::endl;
                // }

                if((probs[i].x_ == neu) && (finden(visited, probs[i].y_))) {
                    value *= probs[i].probs_[1];    
                    if (probs[i].max_ != 1 && probs[i].max_ != 10 && probs[i].max_ != 21) {
                        local_con = true;
                        local_conflicts = std::pair<std::string, std::string> {probs[i].x_, probs[i].y_};
                        local_eigent = probs[i].max_;
                        local_statt = 1;
                        fall = "E1";
                    }
                }
                else if ((probs[i].y_ == neu) && (finden(visited, probs[i].x_))) {
                    value *= probs[i].probs_[0];
                    if (probs[i].max_ != 0 && probs[i].max_ != 10 && probs[i].max_ != 20) {
                        local_con = true;
                        local_conflicts = std::pair<std::string, std::string> {probs[i].x_, probs[i].y_};
                        local_eigent = probs[i].max_;
                        local_statt = 0;
                        fall = "E2";
                    }
                }
                else if((probs[i].x_ == neu) && (std::find(nach.begin(), nach.end(), probs[i].y_) != nach.end())) {
                    value *= probs[i].probs_[0];   
                    if (probs[i].max_ != 0 && probs[i].max_ != 10 && probs[i].max_ != 20) {
                        local_con = true;
                        local_conflicts = std::pair<std::string, std::string> {probs[i].x_, probs[i].y_};
                        local_eigent = probs[i].max_;
                        local_statt = 0;
                        fall = "E3";
                    } 
                }
                else if ((probs[i].y_ == neu) && (std::find(nach.begin(), nach.end(), probs[i].x_) != nach.end())) {
                    value *= probs[i].probs_[1];
                    if (probs[i].max_ != 1 && probs[i].max_ != 10 && probs[i].max_ != 21) {
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
                    if (probs[i].max_ != 2 && probs[i].max_ != 20 && probs[i].max_ != 21) {
                        local_con = true;
                        local_conflicts = std::pair<std::string, std::string> {probs[i].x_, probs[i].y_};
                        local_eigent = probs[i].max_;
                        local_statt = 2;
                        fall = "E5";
                        // std::cout << "E5" << std::endl;
                    }
                }
                else {
                    continue;
                }
            }

            for (size_t i = 0; i < ignored.size(); i++) {
                if((ignored[i].x_ == neu) && (finden(visited, ignored[i].y_))) {
                    value *= ignored[i].probs_[1];  
                    if (ignored[i].max_ != 1 && ignored[i].max_ != 10 && ignored[i].max_ != 21) {
                        local_con = true;
                        local_conflicts = std::pair<std::string, std::string> {ignored[i].x_, ignored[i].y_};
                        local_eigent = ignored[i].max_;
                        local_statt = 1;
                        fall = "EI1";
                    }  
                }
                else if ((ignored[i].y_ == neu) && (finden(visited, ignored[i].x_))) {
                    value *= ignored[i].probs_[0];
                    if (ignored[i].max_ != 0 && ignored[i].max_ != 10 && ignored[i].max_ != 20) {
                        local_con = true;
                        local_conflicts = std::pair<std::string, std::string> {ignored[i].x_, ignored[i].y_};
                        local_eigent = ignored[i].max_;
                        local_statt = 0;
                        fall = "EI2";
                    }
                }
                else if((ignored[i].x_ == neu) && (std::find(nach.begin(), nach.end(), ignored[i].y_) != nach.end())) {
                    value *= ignored[i].probs_[0];  
                    if (ignored[i].max_ != 0 && ignored[i].max_ != 10 && ignored[i].max_ != 20) {
                        local_con = true;
                        local_conflicts = std::pair<std::string, std::string> {ignored[i].x_, ignored[i].y_};
                        local_eigent = ignored[i].max_;
                        local_statt = 0;
                        fall = "EI3";
                    }  
                }
                else if ((ignored[i].y_ == neu) && (std::find(nach.begin(), nach.end(), ignored[i].x_) != nach.end())) {
                    value *= ignored[i].probs_[1];
                    if (ignored[i].max_ != 1 && ignored[i].max_ != 10 && ignored[i].max_ != 21) {
                        local_con = true;
                        local_conflicts = std::pair<std::string, std::string> {ignored[i].x_, ignored[i].y_};
                        local_eigent = ignored[i].max_;
                        local_statt = 1;
                        fall = "EI4";
                    }
                }
                else if (((ignored[i].x_ == neu) && (std::find(in.begin(), in.end(), ignored[i].y_) != in.end())) || ((ignored[i].y_ == neu) && (std::find(in.begin(), in.end(), ignored[i].x_) != in.end()))) {
                    value *= ignored[i].probs_[2];
                    if (ignored[i].max_ != 2 && ignored[i].max_ != 20 && ignored[i].max_ != 21) {
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
                    // std::cout << "NEW CONFLICT" << std::endl;
                    // std::cout << "involved: " << local_conflicts.first << " " << local_conflicts.second << std::endl;
                    // std::cout << "Eig: " << local_eigent << " Statt: " << local_statt << " Fall: " << fall << std::endl;
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
    std::shared_ptr<node> root (new normal_node (std::string ("root")));

    //Initialiserung mit erstem Paar
    std::shared_ptr<node> x_ptr (new normal_node (probs[0].x_));
    std::shared_ptr<node> y_ptr (new normal_node (probs[0].y_));

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
        std::shared_ptr<node> new_node (new normal_node (in[in.size()-1]));

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
    tree_ausgabe(root, "graph_vorher");

    // std::vector<int> parent_vec = to_parent_vec(root);
    // std::cout << "Große: " << parent_vec.size() << std::endl;
    // for (auto const & ins : parent_vec) {
    //     std::cout << ins << ", ";
    // }
    // std::cout << std::endl;

    std::deque<std::shared_ptr<node>> in_conflicts;
    int num_con {0};

    for (size_t i = 0; i < conflicts.size(); i++) {
        if (finden(in_conflicts, conflicts[i].involved.first) && finden(in_conflicts, conflicts[i].involved.second)) {
            // std::cout << "Übersprungen" << std::endl;
            continue;
        }
        else if (finden(in_conflicts, conflicts[i].involved.first)) {
            // std::cout << "Collapse 1" << std::endl;
            collapse_conflict(root, conflicts[i].involved.first, conflicts[i].involved.second, conflicts[i].eigentlich, conflicts[i].stattdessen, in_conflicts);
        }
        else if (finden(in_conflicts, conflicts[i].involved.second)) {
            // std::cout << "Collapse2" << std::endl;
            collapse_conflict(root, conflicts[i].involved.second, conflicts[i].involved.first, conflicts[i].eigentlich, conflicts[i].stattdessen, in_conflicts);
            // std::cout << "Collaps 2 danach" << std::endl;
        }
        else { 
            std::shared_ptr<node> conflictnode (new conflict_node (num_con));
            num_con++;

            if (conflicts[i].stattdessen == 2) {
                //Gemeinsame Elternknoten und dann bis zu den Knoten
                //Gesmater subbaum

                std::shared_ptr<node> sub_root = shared_ancestor(root, conflicts[i].involved.first, conflicts[i].involved.second);
                std::deque<std::shared_ptr<node>> sub_tree = subtree(sub_root);
                std::shared_ptr<node> parent = find_parent(sub_root, root);

                if (sub_root != root) {
                    parent -> children_.erase(std::find(parent -> children_.begin(), parent -> children_.end(), sub_root));
                }
                else {
                    sub_tree.pop_front();
                }

                // std::cout << "Subtree size: " << sub_tree.size() << std::endl;

                
                for (size_t j = 0; j < sub_tree.size(); j++) {
                    // conflictnode -> children_.push_back(sub_tree[j]);
                    conflictnode -> involved_add(sub_tree[j]);
                    sub_tree[j] -> children_.clear();
                    in_conflicts.push_back(sub_tree[j]);

                    auto position = std::find(root -> children_.begin(), root -> children_.end(), sub_tree[j]);
                    if (position != root -> children_.end()) {
                        root -> children_.erase(position);
                    }
                }

                parent -> children_.push_back(conflictnode);
            }
            else if (conflicts[i].eigentlich == 2) {
                //nächsthöhere Abzweigung
                std::shared_ptr<node> sub_root = abzweigung(root, conflicts[i].involved.first, conflicts[i].involved.second);
                std::deque<std::shared_ptr<node>> sub_tree = subtree(sub_root);
                std::shared_ptr<node> parent = find_parent(sub_root, root);

                if (sub_root != root) {
                    parent -> children_.erase(std::find(parent -> children_.begin(), parent -> children_.end(), sub_root));
                }
                else {
                    sub_tree.pop_front();
                }

                // std::cout << "Subtree size: " << sub_tree.size() << std::endl;

                
                for (size_t j = 0; j < sub_tree.size(); j++) {
                    conflictnode -> involved_add(sub_tree[j]);
                    sub_tree[j] -> children_.clear();
                    in_conflicts.push_back(sub_tree[j]);

                    auto position = std::find(root -> children_.begin(), root -> children_.end(), sub_tree[j]);
                    if (position != root -> children_.end()) {
                        root -> children_.erase(position);
                    }
                }

                parent -> children_.push_back(conflictnode);
            }
            else {
                //Pfad zwsichen den beiden zu Konflik
                std::vector<std::shared_ptr<node>> path = pfad(root, conflicts[i].involved.first, conflicts[i].involved.second);
                std::shared_ptr<node> parent = find_parent(path[0], root);

                for (size_t j = 0; j < path.size(); j++) {
                    conflictnode -> involved_add(path[j]);
                    in_conflicts.push_back(path[j]);

                    if (j == path.size()-1) {
                        conflictnode -> children_add(path[j] -> children_);
                        path[j] -> children_.clear();
                    }
                    else {
                        for(auto const child : path[j] -> children_) {
                            if (child != path[j+1]) {
                                conflictnode -> children_.push_back(child);
                            }
                        }
                        path[j] -> children_.clear();
                    }

                    auto position = std::find(root -> children_.begin(), root -> children_.end(), path[j]);
                    if (position != root -> children_.end()) {
                        root -> children_.erase(position);
                    }
                }

                parent -> children_.push_back(conflictnode);
            }
        }
    }

    tree_ausgabe(root, "graph");
    std::cout << "Tree korrekt erstellt" << std::endl;
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