#include <iostream>
#include <vector>
#include <fstream>
#include <sstream>
#include "bits-stdc++.h"
#include <unordered_map>
#include <unordered_set>

using namespace std;

class node {

public:

    float Average;
    float MSE;
    float Threshold;
    vector<vector<float>> Tdatareg;
    string Attr;
    int colnum;
    pair<int, string> path;
    vector<vector<string>> Tdata;

};

vector<vector<string>> adj_rules;
vector<node> nodes;
typedef vector<pair<vector<node>, vector<vector<string>>>> root;

string InttoString(float a)
{
    ostringstream temp;
    temp << a;
    return temp.str();
}

vector<string> split(const string& str, const string& delim)
{
    vector<string> tokens;
    size_t prev = 0, pos = 0;
    do
    {
        pos = str.find(delim, prev);
        if (pos == string::npos)
        {
            pos = str.length();
        }
        string token = str.substr(prev, pos - prev);
        if (!token.empty())
        {
            tokens.push_back(token);
        }
        prev = pos + delim.length();
    } while (pos < str.length() && prev < str.length());

    return tokens;
}

string& remove_charss(string& s, const string& chars)
{
    s.erase(remove_if(s.begin(), s.end(), [&chars](const char& c) {

        return chars.find(c) != string::npos;

        }), s.end());

    return s;
}

void add_rules(const int& start, const int& end, const string& rule)
{
    adj_rules[start][end] = rule;
}

void add_node(const string& attr, vector<vector<string>>& tdata, pair<int, string>& path, const float& threshold, const float& average, const float& mse, vector<vector<float>>& tdatareg)
{
    node* newnode = new node();
    newnode->Attr = attr;
    newnode->Tdata = tdata;
    newnode->colnum = NULL;
    newnode->path = path;
    newnode->Threshold = threshold;
    newnode->Average = average;
    newnode->MSE;
    newnode->Tdatareg = tdatareg;

    nodes.push_back(*newnode);
}
template<typename T>
void drop_rows(vector<vector<T>>& tdata, const int& i)
{
    tdata[i].clear();
}

template<typename T>
void shuffle_rows(vector<vector<T>>& tdata, time_t seed)
{
    srand((unsigned)seed);
    vector<T> saved;
    for (int i = 1; i < tdata.size(); i++)
    {
        int r = rand() % tdata.size();
        if (r != i && r != 0)
        {
            for (int j = 0; j < tdata[i].size(); j++)
            {
                saved.push_back(tdata[i][j]);
            }
            drop_rows(tdata, i);
            for (int j = 0; j < saved.size(); j++)
            {
                tdata[i].push_back(tdata[r][j]);
            }
            drop_rows(tdata, r);
            for (int j = 0; j < saved.size(); j++)
            {
                tdata[r].push_back(saved[j]);
            }
            saved.clear();

        }

    }
}

float GiniimpuritySub(const string& sub, const int& j, const int& index, set<string>& targets)
{
    vector<float> firstcountarr;

    float val = 1;
    for (auto& x : targets)
    {
        float firstcount = 0;
        for (int i = 1; i < nodes[index].Tdata.size(); i++)
        {
            if (nodes[index].Tdata[i].size() != 0)
            {
                if (nodes[index].Tdata[i][j] == sub)
                {
                    if (nodes[index].Tdata[i][nodes[index].Tdata[0].size() - 1] == x)
                    {
                        firstcount++;
                    }

                }
            }


        }
        firstcountarr.push_back(firstcount);
    }

    float total = accumulate(firstcountarr.begin(), firstcountarr.end(), 0.0);
    for (int i = 0; i < firstcountarr.size(); i++)
    {
        val -= (pow(firstcountarr[i] / (total), 2));
    }

    return val;

}

unordered_map<string, float> count_Features(const int& j, const int& index, int& tcount)
{
    set<string> subfeature;
    vector<string> copyfeature;
    for (int i = 1; i < nodes[index].Tdata.size(); i++)
    {
        if (nodes[index].Tdata[i].size() != 0)
        {
            subfeature.insert(nodes[index].Tdata[i][j]);
            copyfeature.push_back(nodes[index].Tdata[i][j]);
        }

    }

    unordered_map<string, float> Umap;

    for (auto& x : subfeature)
    {
        Umap.insert(make_pair(x, count(copyfeature.begin(), copyfeature.end(), x)));

    }

    tcount = copyfeature.size();
    return Umap;
}

float cal_gini_impurity(const int& j, const int& index, set<string>& targets)
{
    int tcount;
    unordered_map<string, float> Umap = count_Features(j, index, tcount);

    float SumofGini = 0;
    for (auto& x : Umap)
    {
        SumofGini += (GiniimpuritySub(x.first, j, index, targets) * (x.second / tcount));
    }

    return SumofGini;

}

template<typename T>
void column_drop(vector<int> drops, vector<vector<T>>& tdata)
{
     sort(drops.begin(), drops.end());
     for (int k = 0; k < drops.size(); k++)
     {
         if (k > 0)
         {
             drops[k] = drops[k] - k;
             
         }

         for (int i = 0; i < tdata.size(); i++)
         {
             tdata[i].erase(tdata[i].begin() + drops[k]);
         }
         
     }
       
}

void header_col_Drop(vector<int> drops, vector<string>& header)
{
    sort(drops.begin(), drops.end());
    for (int k = 0; k < drops.size(); k++)
    {
        if (k > 0)
        {
            drops[k] = drops[k] - 1;
        }

        header.erase(header.begin() + drops[k]);

    }

}

string set_node(int& colnum, const int& index, set<string>& targets, const float& single_node_accuracy_per)
{
    vector<float> collected;
    float low = 10000;
    int feature_index = 0;
    for (int j = 0; j < nodes[index].Tdata[0].size() - 1; j++)
    {
        float Gini = cal_gini_impurity(j, index, targets);
        collected.push_back(Gini);
        if (low > Gini)
        {
            low = Gini;
            feature_index = j;
        }
    }

    collected.erase(unique(collected.begin(), collected.end()), collected.end());
    colnum = feature_index;
    if (collected.size() == 1)
    {
        map<string, int> mp;
        for (int h = 1; h < nodes[index].Tdata.size(); h++)
        {
            mp[nodes[index].Tdata[h][nodes[index].Tdata[h].size() - 1]]++;
        }
        int maxv = 0;
        string maxt = "";
        for (auto& x : mp)
        {
            if (maxv < x.second)
            {
                maxv = x.second;
                maxt = x.first;
            }
        }
        colnum = 10000;
        return maxt;
    }
    else
    {
        map<string, float> freqmp;
        for (int i = 1; i < nodes[index].Tdata.size(); i++)
        {
            freqmp[nodes[index].Tdata[i][nodes[index].Tdata[i].size() - 1]]++;
        }

        for (auto& x : freqmp)
        {
            float per = (x.second / nodes[index].Tdata.size()) * 100;
            if (per >= single_node_accuracy_per)
            {
                colnum = 10000;
                return x.first;
            }
        }

        return nodes[index].Tdata[0][feature_index];

    }

}

unordered_set<string> Get_featurearr(const int& colnum, const int& origin)
{
    unordered_set<string> subfeature;
    for (int i = 1; i < nodes[origin].Tdata.size(); i++)
    {
        if (nodes[origin].Tdata[i].size() != 0)
        {
            subfeature.insert(nodes[origin].Tdata[i][colnum]);
        }

    }

    return subfeature;
}

vector<vector<string>> getTdata(pair<int, string>& path, const int& origin)
{
    vector<vector<string>> tdata;
    tdata.resize(nodes[origin].Tdata.size());
    int k = 0;

    for (int j = 0; j < nodes[origin].Tdata[0].size(); j++)
    {
        tdata[k].push_back(nodes[origin].Tdata[0][j]);

    }

    k++;

    for (int i = 0; i < nodes[origin].Tdata.size(); i++)
    {
        if (nodes[origin].Tdata[i].size() != 0)
        {
            if (path.second == nodes[origin].Tdata[i][path.first])
            {
                for (int j = 0; j < nodes[origin].Tdata[i].size(); j++)
                {
                    tdata[k].push_back(nodes[origin].Tdata[i][j]);
                }
                k++;
            }
        }

    }

    tdata.resize(k);

    return tdata;
}

void setTdata_for_new_branches(const int& colnum, const int& origin, bool shuffleagain, time_t seed)
{
    unordered_set<string> subfeature = Get_featurearr(colnum, origin);
    vector<vector<float>> tdatareg;
    for (auto& x : subfeature)
    {
        pair<int, string> path;

        path = make_pair(colnum, x);

        vector<vector<string>> tdata = getTdata(path, origin);
 
        column_drop({ colnum }, tdata);
        if (shuffleagain)
        {
            shuffle_rows(tdata, seed);
        }

        add_node("", tdata, path, 0, 0, 0, tdatareg);

        add_rules(origin, nodes.size() - 1, x);

    }

}

void MakeDecisions(vector<vector<string>>& tdata, set<string>& targets, const float& single_node_accuracy_per, bool shuffleagain, time_t seed)
{
    if (nodes.empty())
    {
        vector<vector<float>> tdatareg;
        pair<int, string> path;
        add_node("", tdata, path, 0, 0, 0, tdatareg);
        int colnum;
        string attr = set_node(colnum, 0, targets, 100);
        nodes[0].Attr = attr;
        nodes[0].colnum = colnum;
        setTdata_for_new_branches(colnum, 0, shuffleagain, seed);
        nodes[0].Tdata.clear();

        MakeDecisions(tdata, targets, single_node_accuracy_per, shuffleagain, seed);

    }
    else
    {
        for (int i = 0; i < nodes.size(); i++)
        {
            if (nodes[i].Attr == "")
            {
                int colnum;
                string attr = set_node(colnum, i, targets, single_node_accuracy_per);

                nodes[i].Attr = attr;
                nodes[i].colnum = colnum;

                if (colnum != 10000)
                {
                    setTdata_for_new_branches(colnum, i, shuffleagain, seed);
                }

                nodes[i].Tdata.clear();

            }

        }

    }


}

set<string> findTargets(vector<vector<string>>& tdata)
{
    set<string> targets;

    for (int i = 1; i < tdata.size(); i++)
    {
        if (tdata[i][tdata[i].size() - 1] != " " && tdata[i][tdata[i].size() - 1] != "")
        {
            targets.insert(tdata[i][tdata[i].size() - 1]);
        }

    }

    return targets;
}

float calGniimpurityforNumericaldata(vector<vector<string>>& tdata, const float& threshold, const int& j, set<string>& targets)
{
    vector<float> Hfirstcountarr;
    vector<float> Lfirstcountarr;

    for (auto& x : targets)
    {
        float Hfirstcount = 0;
        float Lfirstcount = 0;
        for (int i = 1; i < tdata.size(); i++)
        {
            if (atof(tdata[i][j].c_str()) >= threshold)
            {
                if (tdata[i][tdata[0].size() - 1] == x)
                {
                    Hfirstcount++;
                }

            }
            else
            {
                if (tdata[i][tdata[0].size() - 1] == x)
                {
                    Lfirstcount++;
                }

            }

        }
        Hfirstcountarr.push_back(Hfirstcount);
        Lfirstcountarr.push_back(Lfirstcount);
    }

    float Hcounttotal = accumulate(Hfirstcountarr.begin(), Hfirstcountarr.end(), 0.0);
    float Lcounttotal = accumulate(Lfirstcountarr.begin(), Lfirstcountarr.end(), 0.0);
    float valH = 1;
    float valL = 1;
    for (int i = 0; i < Hfirstcountarr.size(); i++)
    {
        valH -= pow((Hfirstcountarr[i] / Hcounttotal), 2);
    }
    for (int i = 0; i < Lfirstcountarr.size(); i++)
    {
        valL -= pow((Lfirstcountarr[i] / Lcounttotal), 2);
    }

    float sumofGini = (valH * ((Hcounttotal) / (tdata.size() - 1))) + (valL * ((Lcounttotal) / (tdata.size() - 1)));

    return sumofGini;

}

pair<float, float> find_the_threshold(vector<vector<string>>& tdata, const int& j, vector<float>& vec, set<string>& targets)
{
    map<float, float> thresholds;

    for (int i = 0; i < vec.size(); i++)
    {
        float gini = calGniimpurityforNumericaldata(tdata, vec[i], j, targets);
        if (!isnan(gini))
        {
            thresholds.insert(make_pair(gini, vec[i]));
        }

    }

    return *next(thresholds.begin(), 0);
}

vector<pair<int, float>> seperate_num_cols_find_threshold(vector<vector<string>>& tdata, set<string>& targets, vector<int>& colwithnums)
{
    typedef vector<pair<int, vector<float>>> Dtype;

    Dtype allpairs;
    vector<float> vec;
    for (auto& x : colwithnums)
    {
        for (int i = 1; i < tdata.size(); i++)
        {
            vec.push_back(atof(tdata[i][x].c_str()));
        }
        sort(vec.begin(), vec.end());
        allpairs.push_back(make_pair(x, vec));
        vec.clear();
    }
    vector<pair<int, float>> relation;
    for (auto& x : allpairs)
    {
        pair<float, float> threshold = find_the_threshold(tdata, x.first, x.second, targets);

        for (int i = 1; i < tdata.size(); i++)
        {
            if (atof(tdata[i][x.first].c_str()) >= threshold.second)
            {
                tdata[i][x.first] = "1";
            }
            else
            {
                tdata[i][x.first] = "0";
            }
        }
        relation.push_back(make_pair(x.first, threshold.second));
    }
    return relation;

}
template<typename T>
void cal_Accuracy(vector<T>& prediction, vector<T> Actuals)
{
    if (Actuals.size() == prediction.size())
    {
        float correct = 0;
        for (int i = 1; i < prediction.size(); i++)
        {
            if (prediction[i] == Actuals[i])
            {
                correct++;
            }
        }
        cout << "Accuracy Score: " << ((correct / prediction.size()) * 100) << " %" << endl;
    }
    else
    {
        cout << "size of predicted values and size of actual values not a match!";
    }



}

vector<string> Predict_outcome(vector<vector<string>>& input, set<string>& targets, vector<string>& header, bool showR, root& Treecollections)
{
    srand((unsigned)time(0));
    vector<vector<string>> allpredictions;
    allpredictions.resize(Treecollections.size());
    vector<string> verdiction;
    int m = 0;
    for (auto& z : Treecollections)
    {
        for (int q = 0; q < input.size(); q++)
        {
            string rule = "";
            int index = 0;
            for (int i = 0; i < header.size(); i++)
            {
                if (header[i] == z.first[0].Attr)
                {
                    rule = input[q][i];
                    break;
                }
            }

            while (1)
            {
                bool check = false;
                for (int j = 0; j < z.second.size(); j++)
                {
                    if (rule == z.second[index][j])
                    {
                        index = j;
                        check = true;
                        break;
                    }
                }
                bool check2 = false;
                for (auto& x : targets)
                {
                    if (z.first[index].Attr == x)
                    {
                        if (showR)
                        {
                            cout << "Prediction: " << x << endl;
                        }
                        check2 = true;
                        allpredictions[m].push_back(x);
                        break;
                    }
                }
                if (check2)
                {
                    break;
                }

                if (!check)
                {
                    if (showR)
                    {
                        cout << "No Prediction!" << endl;
                    }

                    allpredictions[m].push_back(" ");

                    break;
                }
                for (int i = 0; i < header.size(); i++)
                {
                    if (header[i] == z.first[index].Attr)
                    {
                        rule = input[q][i];
                        break;
                    }
                }

            }
        }

        m++;

    }
    allpredictions.resize(m);
    allpredictions.shrink_to_fit();
    for (int j = 0; j < input.size(); j++)
    {
        map<string, int> fremap;
        for (int c = 0; c < allpredictions.size(); c++)
        {
            fremap[allpredictions[c][j]]++;
        }
        bool check4 = false;
        if (fremap.size() == 1)
        {
            if ((*next(fremap.begin(), 0)).first == " ")
            {
                int in = rand() % (targets.size() - 1);
                string pre = *next(targets.begin(), in);
                cout << "No final prediction!,but chose randomly: " << pre << endl;
                check4 = true;
                verdiction.push_back(pre);
            }
        }
        if (!check4)
        {
            int large = 0;
            string vote = "";
            for (auto& p : fremap)
            {
                if (large < p.second && p.first != " ")
                {
                    large = p.second;
                    vote = p.first;
                }
            }
            verdiction.push_back(vote);
        }

    }

    return verdiction;
}

template<typename T>
vector<vector<T>> row_feature_sampling(vector<vector<T>> tdata, const int featurelimit)
{
    vector<vector<T>> newdata;
    newdata.resize(tdata.size());
    srand(time(0));
    int prev = NULL;
    for (int i = 0; i < featurelimit; i++)
    {
        int r = rand() % (tdata[0].size() - 1);

        if (r == prev)
        {
            i--;
        }
        else
        {
            for (int e = 0; e < tdata.size(); e++)
            {
                newdata[e].push_back(tdata[e][r]);
            }
        }

        prev = r;
    }
    for (int e = 0; e < tdata.size(); e++)
    {
        newdata[e].push_back(tdata[e][tdata[e].size() - 1]);
    }

    return newdata;
}

vector<pair<int, float>> build_Random_forest(vector<vector<string>>& tdata, vector<int>& colwithnums, const float& split_train_per, const float& single_node_accuracy_per, bool shuffleagain, time_t seed, const int& sizeofmatrix, const int& numofTrees, int featurelimit)
{
    root Treecollections;

    adj_rules.resize(sizeofmatrix);
    for (int i = 0; i < adj_rules.size(); i++)
    {
        adj_rules[i].resize(sizeofmatrix);
    }

    set<string> targets = findTargets(tdata);
    vector<pair<int, float>> rel = seperate_num_cols_find_threshold(tdata, targets, colwithnums);

    vector<vector<string>> testdatasplited;
    vector<string> actuals;
    const int testdataN = (tdata.size()) - ((split_train_per * (tdata.size())) / 100);
    testdatasplited.resize(testdataN);
    int y = 0;

    vector<string> header;
    for (int j = 0; j < tdata[0].size(); j++)
    {
        header.push_back(tdata[0][j]);
    }

    for (int i = tdata.size() - testdataN; i < tdata.size(); i++)
    {
        actuals.push_back(tdata[i][tdata[i].size() - 1]);
        for (int j = 0; j < tdata[i].size(); j++)
        {
            testdatasplited[y].push_back(tdata[i][j]);
        }
        y++;

    }

    tdata.resize(tdata.size() - testdataN);
    tdata.shrink_to_fit();
    testdatasplited.shrink_to_fit();
    column_drop({ int(testdatasplited[0].size()) - 1 }, testdatasplited);
    if (featurelimit == 0)
    {
        featurelimit = ceil(sqrt(header.size() - 1));
    }

    while (1)
    {
        vector<vector<string>> partialtdata = row_feature_sampling(tdata, featurelimit);
        set<string> partialtarget = findTargets(partialtdata);
        MakeDecisions(partialtdata, partialtarget, single_node_accuracy_per, shuffleagain, seed);
    
        Treecollections.push_back(make_pair(nodes, adj_rules));

        cout << "Tree " << Treecollections.size() << " trained!" << '\n';
        if (Treecollections.size() == numofTrees)
        {
            break;
        }
        nodes.clear();
        adj_rules.clear();

        adj_rules.resize(sizeofmatrix);
        for (int i = 0; i < adj_rules.size(); i++)
        {
            adj_rules[i].resize(sizeofmatrix);
        }

    }

    cout << "model is ready to predict!...." << endl;
    vector<string> outcomes = Predict_outcome(testdatasplited, targets, header, true, Treecollections);

    cal_Accuracy(outcomes, actuals);

    return rel;
}


bool isinclude(const string& str)
{
    for (int k = 0; k < str.length(); k++)
    {
        if (str[k] == '\"')
        {
            return true;
        }
    }
    return false;
}


void drop_Nan_row(vector<vector<string>>& tdata)
{
    for (int i = 0; i < tdata.size(); i++)
    {
        for (int j = 0; j < tdata[i].size(); j++)
        {
            if (tdata[i][j] == "Nan" || tdata[i][j] == "nan" || tdata[i][j] == "NAN" || tdata[i][j] == "-")
            {
                tdata[i].clear();
     
            }
        }
    }
}

pair<int, string> get_each_freq(const int& k, vector<vector<string>>& tdata)
{
    map<string, int> mp;

    for (int t = 0; t < tdata.size(); t++)
    {
        mp[tdata[t][k]]++;

    }
    map<int, string> vecp;

    for (auto& x : mp)
    {
        vecp.insert(make_pair(x.second, x.first));
    }

    return *next(vecp.begin(), vecp.size() - 1);
}

void replace_columnwithfreq(vector<int>& freqedtion, vector<vector<string>>& tdata)
{
    for (int k = 0; k < freqedtion.size(); k++)
    {
        pair<int, string> mostfreq = get_each_freq(freqedtion[k], tdata);

        for (int t = 0; t < tdata.size(); t++)
        {
            if (tdata[t][freqedtion[k]] == "B28" || tdata[t][freqedtion[k]] == " ")
            {
                tdata[t][freqedtion[k]] = mostfreq.second;
            }
        }
    }
}

void featurize_data(vector<vector<string>>& tdata, vector<int>& alphafeature)
{
    for (int j = 0; j < alphafeature.size(); j++)
    {
        vector<string> data;
        for (int i = 1; i < tdata.size(); i++)
        {
            data.push_back(tdata[i][alphafeature[j]]);
        }
        sort(data.begin(), data.end());
        data.erase(unique(data.begin(), data.end()), data.end());

        for (int c = 0; c < data.size(); c++)
        {
            for (int i = 1; i < tdata.size(); i++)
            {
                if (tdata[i][alphafeature[j]] == data[c])
                {
                    tdata[i][alphafeature[j]] = InttoString(c);
                }
            }
        }

    }

}

void sumX(const float& x, float& SXv)
{
    SXv += x;
}

void sumY(const float& y, float& SYv)
{
    SYv += y;
}

void sumXY(const float& x, const float& y, float& SXYv)
{
    SXYv += x * y;
}

void sumX2(const float& x, float& SX2v)
{
    SX2v += pow(x, 2);
}

void sumY2(const float& y, float& SY2v)
{
    SY2v += pow(y, 2);
}

float get_cor_coe(int t, const int& x, vector<vector<string>>& tdata, const int& y, vector<int>& alphafeature)
{
    if (t == 0)
    {
        featurize_data(tdata, alphafeature);
    }

    float sumXv = 0, sumYv = 0, sumX2v = 0, sumY2v = 0, sumXYv = 0;
    for (int i = 1; i < tdata.size(); i++)
    {
        sumX(atof(tdata[i][x].c_str()), sumXv);
        sumY(atof(tdata[i][y].c_str()), sumYv);
        sumX2(atof(tdata[i][x].c_str()), sumX2v);
        sumY2(atof(tdata[i][y].c_str()), sumY2v);
        sumXY(atof(tdata[i][x].c_str()), atof(tdata[i][y].c_str()), sumXYv);

    }
    float upper = (tdata.size() * sumXYv) - (sumXv * sumYv);
    float lower = sqrt(((tdata.size() * sumX2v) - pow(sumXv, 2)) * ((tdata.size() * sumY2v) - pow(sumYv, 2)));
    float corcoe = upper / lower;

    return corcoe;
}

vector<pair<int, int>> get_best_corrcoecolms(vector<int>& tocorcoe, vector<vector<string>> tdata, vector<int>& alphafeature)
{
    vector<pair<int, int>> mp;
    float minval = 10000;
    int mincolx = 0;
    int mincoly = 0;
    int t = 0;
    for (auto& x : tocorcoe)
    {
        for (int y = 0; y < tdata[0].size(); y++)
        {
            float corcoe = get_cor_coe(t++, x, tdata, y, alphafeature);

            if (corcoe >= 0)
            {
                float abslD = 1 - corcoe;
                if (minval > abslD && abslD != 0)
                {
                    minval = abslD;
                    mincolx = x;
                    mincoly = y;
                }
            }
            else
            {
                float abslD = 1 - abs(corcoe);
                if (minval > abslD & abslD != 0)
                {
                    minval = abslD;
                    mincolx = x;
                    mincoly = y;
                }
            }

        }
        mp.push_back(make_pair(mincolx, mincoly));

    }

    return mp;

}

vector<vector<string>> fixmissingnumvalue(vector<int>& tocorcoe, vector<vector<string>> tdata, vector<int>& alphafeature)
{
    vector<pair<int, int>> mp = get_best_corrcoecolms(tocorcoe, tdata, alphafeature);
    vector<pair<int, vector<string>>> data;

    for (auto& x : mp)
    {
        vector<string> fixed;
        vector<string> mis;
        for (int i = 1; i < tdata.size(); i++)
        {
            if (tdata[i][x.first] == " ")
            {
                if (tdata[i][x.second] != " ")
                {
                    mis.push_back(tdata[i][x.second]);
                }

            }
        }
        for (int r = 0; r < mis.size(); r++)
        {
            float sum = 0;
            int v = 0;
            for (int i = 1; i < tdata.size(); i++)
            {
                if (tdata[i][x.second] == mis[r])
                {
                    if (tdata[i][x.first] != " ")
                    {
                        sum += atof(tdata[i][x.first].c_str());
                        v++;
                    }
                }
            }

            fixed.push_back(InttoString(sum / v));

        }

        map<string, int> freq;
        for (int u = 0; u < fixed.size(); u++)
        {
            freq[fixed[u]]++;
        }
        map<int, string> collected;
        for (auto& z : freq)
        {
            collected.insert(make_pair(z.second, z.first));
        }

        for (int u = 0; u < fixed.size(); u++)
        {
            if (fixed[u] == "nan" || fixed[u] == "Nan" || fixed[u] == "0")
            {
                fixed[u] = (*next(collected.begin(), collected.size() - 1)).second;
            }
        }
        data.push_back(make_pair(x.first, fixed));
    }


    for (auto& x : data)
    {
        int h = 1;
        for (int j = 0; j < x.second.size(); j++)
        {
            for (int i = h; i < tdata.size(); i++)
            {
                if (tdata[i][x.first] == " ")
                {
                    tdata[i][x.first] = x.second[j];
                    h = i;
                    break;
                }
            }
        }

    }

    return tdata;

}
template<typename T>
void reposition_target_col(vector<vector<T>>& tdata, const int& current)
{
    for (int i = 0; i < tdata.size(); i++)
    {
        string td = tdata[i][current];
        tdata[i].erase(tdata[i].begin() + current);
        tdata[i].push_back(td);
    }

}

vector<vector<string>> readprepareTraindataset(const char* fname)
{
    vector<string> data;
    vector<vector<string>> tdata;
    ifstream file(fname);
    string line = "";
    
    while (getline(file, line))
    {
        const std::string s = ", ";
        const std::string t = " ";

        std::string::size_type n = 0;
        while ((n = line.find(s, n)) != std::string::npos)
        {
            line.replace(n, s.size(), t);
            n += t.size();
        }
        if (line[line.length() - 1] == ',')
        {
            line = line + ',';
          
        }
        const std::string s1 = ",,";
        const std::string t1 = ",nan,";

        std::string::size_type n1 = 0;
        while ((n1 = line.find(s1, n1)) != std::string::npos)
        {
            line.replace(n1, s1.size(), t1);
            n1 += t1.size();
        }
        
        data.push_back(line);
        
    }
    
    file.close();

    tdata.resize(data.size());
   
    for (int i = 0; i < data.size(); i++)
    {
        vector<string> str = split(data[i], ",");
        
        for (int j = 0; j < str.size(); j++)
        {
           
             tdata[i].push_back(str[j]);
         
        }
    }


    //vector<int> freqedtion = { 8 };
    //replace_columnwithfreq(freqedtion, tdata);
    vector<int> drops = { 0, 2, 3, 6, 7, 8, 10, 11};
    column_drop(drops, tdata);
   
    
    reposition_target_col(tdata, 0);
    vector<int> tocorcoe = { 1 };
    vector<int> alphafeature = { 0 };
    tdata = fixmissingnumvalue(tocorcoe, tdata, alphafeature);
    drop_Nan_row(tdata);
 
    int q = 0;
    for (int i = 0; i < tdata.size(); i++)
    {
        if (tdata[i].size() != tdata[0].size())
        {
            drop_rows(tdata, i);
            q++;
        }
    }
    vector<vector<string>> tdatanew;
    tdatanew.resize(tdata.size() - q);

    int h = 0;
    for (int i = 0; i < tdata.size(); i++)
    {
        if (tdata[i].size() != 0)
        {
            for (int j = 0; j < tdata[i].size(); j++)
            {
                tdatanew[h].push_back(tdata[i][j]);
            }
            h++;
        }

    }

    tdata.clear();
    tdatanew.shrink_to_fit();
    return tdatanew;

}

vector<vector<string>> readprepareTestdataset(const char* fname, vector<pair<int, float>>& relation)
{
    vector<string> data;
    vector<vector<string>> tdata;
    ifstream file(fname);
    string line = "";

    while (getline(file, line))
    {
        const std::string s = ", ";
        const std::string t = " ";

        std::string::size_type n = 0;
        while ((n = line.find(s, n)) != std::string::npos)
        {
            line.replace(n, s.size(), t);
            n += t.size();
        }
        if (line[line.length() - 1] == ',')
        {
            line = line + ',';

        }
        const std::string s1 = ",,";
        const std::string t1 = ",nan,";

        std::string::size_type n1 = 0;
        while ((n1 = line.find(s1, n1)) != std::string::npos)
        {
            line.replace(n1, s1.size(), t1);
            n1 += t1.size();
        }

        data.push_back(line);

    }

    file.close();

    tdata.resize(data.size());

    for (int i = 0; i < data.size(); i++)
    {
        vector<string> str = split(data[i], ",");

        for (int j = 0; j < str.size(); j++)
        {

            tdata[i].push_back(str[j]);

        }
    }


    //vector<int> freqedtion = { 8 };
    //replace_columnwithfreq(freqedtion, tdata);
    vector<int> drops = { 0, 2, 3, 6, 7, 8, 10, 11 };
    column_drop(drops, tdata);


    reposition_target_col(tdata, 0);
    vector<int> tocorcoe = { 1 };
    vector<int> alphafeature = { 0 };
    tdata = fixmissingnumvalue(tocorcoe, tdata, alphafeature);
    drop_Nan_row(tdata);

    int q = 0;
    for (int i = 0; i < tdata.size(); i++)
    {
        if (tdata[i].size() != tdata[0].size())
        {
            drop_rows(tdata, i);
            q++;
        }
    }
    vector<vector<string>> tdatanew;
    tdatanew.resize(tdata.size() - q);

    int h = 0;
    for (int i = 0; i < tdata.size(); i++)
    {
        if (tdata[i].size() != 0)
        {
            for (int j = 0; j < tdata[i].size(); j++)
            {
                tdatanew[h].push_back(tdata[i][j]);
            }
            h++;
        }

    }

    tdata.clear();

    for (auto& x : relation)
    {
        for (int i = 1; i < tdatanew.size(); i++)
        {
            if (atof(tdatanew[i][x.first].c_str()) >= x.second)
            {
                tdatanew[i][x.first] = "1";
            }
            else
            {
                tdatanew[i][x.first] = "0";
            }
        }

    }

    return tdatanew;
}

vector<float> CalculateThresholdReg(const int& index, const int& j)
{
    vector<float> thresholds;
    vector<float> colvals;

    for (int i = 0; i < nodes[index].Tdatareg.size(); i++)
    {
        colvals.push_back(nodes[index].Tdatareg[i][j]);
    }
    sort(colvals.begin(), colvals.end());

    float reductoin = 0;
    for (int i = 0; i + 1 < colvals.size(); i++)
    {
        reductoin += colvals[i + 1] - colvals[i];
    }

    reductoin = reductoin / (colvals.size() - 1);

    float value = colvals[0];
    for (int i = 0; i < colvals.size(); i++)
    {
        thresholds.push_back(value);
        if (colvals[colvals.size() - 1] < value)
        {
            break;
        }

        value += reductoin;

    }

    return thresholds;
}

float cal_Sum_mse(const int& index, const int& j, const float& threshold)
{
    vector<float> lowvals;
    vector<float> highvals;
    for (int i = 0; i < nodes[index].Tdatareg.size(); i++)
    {
        if (threshold >= nodes[index].Tdatareg[i][j])
        {
            lowvals.push_back(nodes[index].Tdatareg[i][nodes[index].Tdatareg[i].size() - 1]);
        }
        else
        {
            highvals.push_back(nodes[index].Tdatareg[i][nodes[index].Tdatareg[i].size() - 1]);
        }
    }
    float Lsum = 0, Hsum = 0, LsumAve = 0, HsumAve = 0, Lsmse = 0, Hsmse = 0;

    Lsum = accumulate(lowvals.begin(), lowvals.end(), 0.0);
    LsumAve = Lsum / lowvals.size();
    Hsum = accumulate(highvals.begin(), highvals.end(), 0.0);
    HsumAve = Hsum / highvals.size();

    for (int i = 0; i < lowvals.size(); i++)
    {
        Lsmse += pow((lowvals[i] - LsumAve), 2);
    }

    for (int i = 0; i < highvals.size(); i++)
    {
        Hsmse += pow((highvals[i] - HsumAve), 2);
    }

    return Lsmse + Hsmse;

}

float proper_Mse(const int& index, const int& j, vector<float>& Lthresholds, float& bestthreshold)
{
    map<float, float> hold;

    for (int i = 0; i < Lthresholds.size(); i++)
    {
        float Smse = cal_Sum_mse(index, j, Lthresholds[i]);

        hold.insert(make_pair(Smse, Lthresholds[i]));
    }

    bestthreshold = (*next(hold.begin(), 0)).second;

    return (*next(hold.begin(), 0)).first;
}

void setTdata_for_new_branches_reg(const int& colnum, const int& origin, const float& threshold)
{
    vector<vector<string>> tdata;
    pair<int, string> path;
    vector<vector<float>> Truetdatareg;
    vector<vector<float>> Falsetdatareg;

    Truetdatareg.resize(nodes[origin].Tdatareg.size());
    Falsetdatareg.resize(nodes[origin].Tdatareg.size());

    int k = 0;
    int n = 0;
    bool check = false;
    for (int i = 0; i < nodes[origin].Tdatareg.size(); i++)
    {
        for (int j = 0; j < nodes[origin].Tdatareg[i].size(); j++)
        {
            if (nodes[origin].Tdatareg[i][colnum] <= threshold)
            {
                Truetdatareg[k].push_back(nodes[origin].Tdatareg[i][j]);
                check = true;
            }
            else
            {
                Falsetdatareg[n].push_back(nodes[origin].Tdatareg[i][j]);
                check = false;
            }
        }
        if (check)
        {
            k++;
        }
        else
        {
            n++;
        }
    }

    Truetdatareg.resize(k);
    Falsetdatareg.resize(n);

    add_node("", tdata, path, 0, 0, 0, Truetdatareg);
    add_rules(origin, nodes.size() - 1, "True");

    add_node("", tdata, path, 0, 0, 0, Falsetdatareg);
    add_rules(origin, nodes.size() - 1, "False");




}

string set_node_reg(int& colnum, const int& index, float& threshold, float& mse, float& average, vector<string>& header, const int& NumberofCases)
{
    map<float, pair<int, float>> hold;

    for (int j = 0; j < nodes[index].Tdatareg[0].size() - 1; j++)
    {
        vector<float> Lthresholds = CalculateThresholdReg(index, j);

        float bestthreshold;
        float Lmse = proper_Mse(index, j, Lthresholds, bestthreshold);
        pair<int, float> co = make_pair(j, bestthreshold);
        hold.insert(make_pair(Lmse, co));

    }
    mse = (*next(hold.begin(), 0)).first;
    threshold = ((*next(hold.begin(), 0)).second).second;

    vector<float> vals;
    for (int i = 0; i < nodes[index].Tdatareg.size(); i++)
    {
        vals.push_back(nodes[index].Tdatareg[i][nodes[index].Tdatareg[i].size() - 1]);
    }

    average = accumulate(vals.begin(), vals.end(), 0.0) / vals.size();
    colnum = ((*next(hold.begin(), 0)).second).first;
    if (vals.size() <= NumberofCases)
    {
        colnum = 10000;
        return "result";
    }
    else
    {
        return header[((*next(hold.begin(), 0)).second).first];
    }

}

void MakeregressionDecision(vector<vector<float>>& tdatareg, vector<string>& header, const int& NumberofCases)
{
    if (nodes.empty())
    {
        vector<vector<string>> tdata;
        pair<int, string> path;

        add_node("", tdata, path, 0, 0, 0, tdatareg);

        int colnum;
        float threshold;
        float mse;
        float average;

        string attr = set_node_reg(colnum, 0, threshold, mse, average, header, 0);

        nodes[0].Attr = attr;
        nodes[0].colnum = colnum;
        nodes[0].Threshold = threshold;
        nodes[0].Average = average;
        nodes[0].MSE = mse;

        setTdata_for_new_branches_reg(colnum, 0, threshold);

        nodes[0].Tdatareg.clear();

        MakeregressionDecision(tdatareg, header, NumberofCases);

    }
    else
    {
        for (int i = 0; i < nodes.size(); i++)
        {
            if (nodes[i].Attr == "")
            {
                int colnum;
                float threshold;
                float mse;
                float average;

                string attr = set_node_reg(colnum, i, threshold, mse, average, header, NumberofCases);

                nodes[i].Attr = attr;
                nodes[i].colnum = colnum;
                nodes[i].Threshold = threshold;
                nodes[i].Average = average;
                nodes[i].MSE = mse;

                if (colnum != 10000)
                {
                    setTdata_for_new_branches_reg(colnum, i, threshold);

                }

                nodes[i].Tdatareg.clear();

            }

        }

    }
}

vector<float> Predict_outcome_reg(vector<vector<float>>& input, vector<string>& header, bool showR, root& Treecollections)
{
    vector<vector<float>> allpredictions;
    allpredictions.resize(Treecollections.size());
    vector<float> verdiction;
    int m = 0;
    for (auto& z : Treecollections)
    {
        for (int q = 0; q < input.size(); q++)
        {
            string rule = "";
            int index = 0;
            for (int i = 0; i < header.size(); i++)
            {
                if (header[i] == z.first[0].Attr)
                {
                    if (input[q][i] <= z.first[0].Threshold)
                    {
                        rule = "True";
                    }
                    else
                    {
                        rule = "False";
                    }
                    break;
                }
            }
            while (1)
            {
                bool check = false;
                for (int j = 0; j < z.second.size(); j++)
                {
                    if (rule == z.second[index][j])
                    {
                        index = j;
                        check = true;
                        break;
                    }

                }

                if (z.first[index].Attr == "result")
                {
                    if (showR)
                    {
                        cout << "Prediction: " << z.first[index].Average << endl;
                    }

                    allpredictions[m].push_back(z.first[index].Average);
                    break;
                }

                if (!check)
                {
                    if (showR)
                    {
                        cout << "No Prediction! " << endl;
                    }
                    allpredictions[m].push_back(0.0);
                    break;
                }
                for (int i = 0; i < header.size(); i++)
                {
                    if (header[i] == z.first[index].Attr)
                    {
                        if (input[q][i] <= z.first[index].Threshold)
                        {
                            rule = "True";
                        }
                        else
                        {
                            rule = "False";
                        }
                        break;
                    }
                }

            }
        }
        m++;
    }
    allpredictions.resize(m);
    allpredictions.shrink_to_fit();
    for (int j = 0; j < input.size(); j++)
    {
        float sum = 0;
        for (int c = 0; c < allpredictions.size(); c++)
        {
            sum += allpredictions[c][j];
        }
        float ave = sum / allpredictions.size();

        verdiction.push_back(ave);
    }

    return verdiction;
}

void cal_Rsquared_Outcomes(vector<float>& prediction, vector<float>& actuals)
{
    if (prediction.size() == actuals.size())
    {
        float RSS = 0, TSS = 0, R2 = 0;
        float ybar = accumulate(actuals.begin(), actuals.end(), 0.0) / actuals.size();
        for (int i = 0; i < prediction.size(); i++)
        {
            RSS += pow((log(actuals[i]) - log(prediction[i])), 2);
            TSS += pow((log(actuals[i]) - log(ybar)), 2);
        }

        R2 = 1 - (RSS / TSS);
        cout << "R2: " << R2 << endl;
    }
    else
    {
        cout << "size of predicted values and size of actual values not a match!";
    }
}

void cal_RMSE_Outcomes(vector<float>& prediction, vector<float>& actuals)
{
    if (prediction.size() == actuals.size())
    {
        float rmse = 0;

        for (int i = 0; i < prediction.size(); i++)
        {
            rmse += pow((log(prediction[i]) - log(actuals[i])), 2);
        }

        cout << "RMSE: " << sqrt(rmse / prediction.size()) << endl;
    }
    else
    {
        cout << "size of predicted values and size of actual values not a match!";
    }

}

void BuildRandomforestRegression(vector<vector<float>>& tdata, vector<string>& header, const int& NumberofCases, const float& split_train_per, const int& sizeofmatrix, const int& numofTrees, int featurelimit)
{
    root Treecollections;
    adj_rules.resize(sizeofmatrix);
    for (int i = 0; i < adj_rules.size(); i++)
    {
        adj_rules[i].resize(sizeofmatrix);
    }
    vector<vector<float>> testdatasplited;
    vector<float> actuals;
    const int testdataN = (tdata.size()) - ((split_train_per * (tdata.size())) / 100);
    testdatasplited.resize(testdataN);
    int y = 0;

    for (int i = tdata.size() - testdataN; i < tdata.size(); i++)
    {
        actuals.push_back(tdata[i][tdata[i].size() - 1]);
        for (int j = 0; j < tdata[i].size(); j++)
        {
            testdatasplited[y].push_back(tdata[i][j]);
        }
        y++;

    }

    tdata.resize(tdata.size() - testdataN);
    tdata.shrink_to_fit();

    column_drop({ int(testdatasplited[0].size()) - 1 }, testdatasplited);

    if (featurelimit == 0)
    {
        featurelimit = ceil(header.size() - 1) / 3;
    }

    while (1)
    {
        vector<vector<float>> partialtdata = row_feature_sampling(tdata, featurelimit);

        MakeregressionDecision(partialtdata, header, NumberofCases);
        Treecollections.push_back(make_pair(nodes, adj_rules));

        cout << "Tree " << Treecollections.size() << " trained!" << '\n';
        if (Treecollections.size() == numofTrees)
        {
            break;
        }
        nodes.clear();
        adj_rules.clear();

        adj_rules.resize(sizeofmatrix);
        for (int i = 0; i < adj_rules.size(); i++)
        {
            adj_rules[i].resize(sizeofmatrix);
        }

    }

    cout << "Model is ready to predict!......" << endl;
    vector<float> predicitons = Predict_outcome_reg(testdatasplited, header, false, Treecollections);

    cout << "Model_";
    cal_RMSE_Outcomes(predicitons, actuals);
    cout << "Model_";
    cal_Rsquared_Outcomes(predicitons, actuals);

}


vector<vector<float>> readprepareTraindatasetReg(const char* fname, vector<string>& header)
{
    vector<string> data;
    vector<vector<float>> tdatareg;
    ifstream file(fname);
    string line = "";
    int u = 0;
    while (getline(file, line))
    {
        if (line != "")
        {
            for (int i = 0; i + 1 < line.length(); i++)
            {
                if (line[i] == ',' && line[i + 1] == ',')
                {
                    line.insert(i + 1, " ");
                }
            }
            data.push_back(line);
            u++;
        }
        if (u == 6000)
        {
            break;
        }

    }

    file.close();

    tdatareg.resize(data.size() - 1);

    for (int i = 0; i < data.size(); i++)
    {
        vector<string> str = split(data[i], ",");

        for (int j = 0; j < str.size(); j++)
        {
            if (i == 0)
            {
                header.push_back(str[j]);
            }
            else
            {
                tdatareg[i - 1].push_back(atof(str[j].c_str()));

            }

        }
    }

    column_drop({0}, tdatareg);
    vector<int> drops = { 0 };
    header_col_Drop(drops, header);
    shuffle_rows(tdatareg, 4);

    return tdatareg;
}

float cal_stdsub(const string& sub, const int& j, const int& index)
{
    float Tstd = 0;
    vector<float> vals;
    for (int i = 0; i < nodes[index].Tdata.size(); i++)
    {
        if (nodes[index].Tdata[i].size() != 0)
        {
            if (nodes[index].Tdata[i][j] == sub)
            {
                vals.push_back(atof(nodes[index].Tdata[i][nodes[index].Tdata[0].size() - 1].c_str()));

            }
        }

    }

    float ave = accumulate(vals.begin(), vals.end(), 0.0) / vals.size();

    for (int i = 0; i < vals.size(); i++)
    {
        Tstd += pow(vals[i] - ave, 2);
    }

    return sqrt(Tstd / vals.size());

}

float cal_Wstd(const int& j, const int& index)
{
    int tcount;
    unordered_map<string, float> Umap = count_Features(j, index, tcount);

    float sumstd = 0;
    for (auto& x : Umap)
    {
        sumstd += (cal_stdsub(x.first, j, index) * (x.second / tcount));
    }

    return sumstd;

}

float cal_IndexTstd(const int& index)
{
    vector<float> ave;
    float Tstd = 0;
    for (int i = 0; i < nodes[index].Tdata.size(); i++)
    {
        ave.push_back(atof(nodes[index].Tdata[i][nodes[index].Tdata[i].size() - 1].c_str()));
    }
    float val = accumulate(ave.begin(), ave.end(), 0.0) / ave.size();

    for (int i = 0; i < ave.size(); i++)
    {
        Tstd += pow(ave[i] - val, 2);
    }

    return sqrt(Tstd / ave.size());
}

string set_node_reg_std(int& colnum, const int& index, const float& numofcases, vector<string>& header)
{
    float high = 0;
    int feature_index = 0;
    for (int j = 0; j < nodes[index].Tdata[0].size() - 1; j++)
    {
        float stdR = cal_IndexTstd(index) - cal_Wstd(j, index);

        if (high < stdR)
        {
            high = stdR;
            feature_index = j;
        }
    }

    colnum = feature_index;
    if (numofcases >= nodes[index].Tdata.size())
    {
        vector<float> lcol;
        colnum = 10000;
        for (int i = 0; i < nodes[index].Tdata.size(); i++)
        {
            lcol.push_back(atof(nodes[index].Tdata[i][nodes[index].Tdata[i].size() - 1].c_str()));
        }

        return InttoString((accumulate(lcol.begin(), lcol.end(), 0.0) / lcol.size()));
    }
    else
    {
        return header[feature_index];
    }

}

void MakeregressionDecision_std(vector<vector<string>>& tdata, vector<string>& header, const int& NumberofCases, bool shuffleagain, time_t seed)
{
    if (nodes.empty())
    {
        vector<vector<float>> tdatareg;
        pair<int, string> path;

        add_node("", tdata, path, 0, 0, 0, tdatareg);

        int colnum;
        string attr = set_node_reg_std(colnum, 0, 0, header);

        nodes[0].Attr = attr;
        nodes[0].colnum = colnum;

        setTdata_for_new_branches(colnum, 0, shuffleagain, seed);

        nodes[0].Tdata.clear();

        MakeregressionDecision_std(tdata, header, NumberofCases, shuffleagain, seed);

    }
    else
    {
        for (int i = 0; i < nodes.size(); i++)
        {
            if (nodes[i].Attr == "")
            {
                int colnum;
                string attr = set_node_reg_std(colnum, i, NumberofCases, header);

                nodes[i].Attr = attr;
                nodes[i].colnum = colnum;

                if (colnum != 10000)
                {
                    setTdata_for_new_branches(colnum, i, shuffleagain, seed);

                }

                nodes[i].Tdata.clear();

            }

        }

    }
}

vector<float> Predict_outcome_reg_std(vector<vector<string>>& input, vector<string>& header, bool showR)
{
    vector<float> verdiction;
    for (int q = 0; q < input.size(); q++)
    {
        int colnum = nodes[0].colnum;
        string rule = input[q][colnum];

        int index = 0;

        while (1)
        {
            bool check = false;
            for (int j = 0; j < adj_rules.size(); j++)
            {
                if (rule == adj_rules[index][j])
                {
                    index = j;
                    check = true;
                    break;
                }

            }

            if (isdigit(nodes[index].Attr[0]) && isdigit(nodes[index].Attr[nodes[index].Attr.length() - 1]))
            {
                if (showR)
                {
                    cout << "Prediction: " << nodes[index].Attr << endl;
                }

                verdiction.push_back(atof(nodes[index].Attr.c_str()));
                break;
            }
            if (!check)
            {
                if (showR)
                {
                    cout << "No Prediction!" << endl;
                }
                verdiction.push_back(0);
                break;
            }
            for (int i = 0; i < header.size(); i++)
            {
                if (header[i] == nodes[index].Attr)
                {
                    rule = input[q][i];
                    break;
                }
            }

        }
    }

    return verdiction;
}

void BuildRegressionTree_using_std(vector<vector<string>>& tdata, vector<string>& header, const int& NumberofCases, const float& split_train_per, bool shuffleagain, time_t seed, const int& sizeofmatrix)
{
    adj_rules.resize(sizeofmatrix);
    for (int i = 0; i < adj_rules.size(); i++)
    {
        adj_rules[i].resize(sizeofmatrix);
    }
    vector<vector<string>> testdatasplited;
    vector<float> actuals;
    const int testdataN = (tdata.size()) - ((split_train_per * (tdata.size())) / 100);
    testdatasplited.resize(testdataN);
    int y = 0;

    for (int i = tdata.size() - testdataN; i < tdata.size(); i++)
    {
        actuals.push_back(atof(tdata[i][tdata[i].size() - 1].c_str()));
        for (int j = 0; j < tdata[i].size(); j++)
        {
            testdatasplited[y].push_back(tdata[i][j]);
        }
        y++;

    }

    tdata.resize(tdata.size() - testdataN);
    tdata.shrink_to_fit();

    column_drop({ int(testdatasplited[0].size()) - 1 }, testdatasplited);

    MakeregressionDecision_std(tdata, header, NumberofCases, shuffleagain, seed);
    cout << "Model is ready to predict!......" << endl;
    vector<float> predicitons = Predict_outcome_reg_std(testdatasplited, header, true);
    cout << "Model_";
    cal_RMSE_Outcomes(predicitons, actuals);
    cout << "Model_";
    cal_Rsquared_Outcomes(predicitons, actuals);


}


void encode_target_variable(vector<vector<string>>& tdata)
{
    set<string> mp;
    for (int i = 1; i < tdata.size(); i++)
    {
        mp.insert(tdata[i][tdata[i].size() - 1]);
    }
    float y = 1;
    for (auto& x : mp)
    {
        for (int i = 1; i < tdata.size(); i++)
        {
            if (tdata[i][tdata[i].size() - 1] == x)
            {
                tdata[i][tdata[i].size() - 1] = InttoString(y);
            }
        }
        y++;
    }

}

vector<vector<string>> readIrisdata(const char* fname)
{
    vector<string> data;
    vector<vector<string>> tdata;
    ifstream file(fname);
    string line = "";

    while (getline(file, line))
    {
        if (line != "")
        {
            for (int i = 0; i + 1 < line.length(); i++)
            {
                if (line[i] == ',' && line[i + 1] == ',')
                {
                    line.insert(i + 1, " ");
                }
            }
            data.push_back(line);
        }

    }

    file.close();

    tdata.resize(data.size());

    for (int i = 0; i < data.size(); i++)
    {
        vector<string> str = split(data[i], ",");
        for (int j = 0; j < str.size(); j++)
        {
            tdata[i].push_back(str[j]);
        }
    }

    encode_target_variable(tdata);
    shuffle_rows(tdata, 1593018700);

    return tdata;
}

int main()
{

    /**
        vector<string> header;
        vector<vector<float>> train_data = readprepareTraindatasetReg("housing.csv",header);

        BuildRandomforestRegression(train_data,header,250,70,1000,10,1);

    **/
    vector<vector<string>> train_data = readprepareTraindataset("train.csv");
    vector<int> colwithnums = { 1, 2 };
    
    //Note: lowering single_node_accuracy(to prune the tree/reduce overfitting) will give a better outcome for test dataset,but with a low model_accuracyscore
    vector<pair<int, float>> relation = build_Random_forest(train_data, colwithnums, 80, 80, false, 2, 1000, 10, 0);

    return 0;

}
