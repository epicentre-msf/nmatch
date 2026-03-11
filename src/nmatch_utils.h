#pragma once
#include <string>
#include <vector>

std::vector<std::string> tokenize_name(const std::string& name, int nchar_min);
int osa_distance(const std::string& s1, const std::string& s2);
bool match_eval_token_cpp(int nchar_x, int nchar_y, int dist);
