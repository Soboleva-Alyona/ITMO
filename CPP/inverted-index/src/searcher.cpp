#include "searcher.h"

#include <algorithm>
#include <iostream>

void to_word(std::string & word)
{
    size_t begin = 0;
    while (std::ispunct(word[begin])) {
        begin++;
    }

    size_t end = word.size();
    while (end > begin && std::ispunct(word[end - 1])) {
        end--;
    }

    word = word.substr(begin, end - begin);

    for (size_t i = 0; i < word.size(); ++i) {
        word[i] = std::tolower(word[i]);
    }
}

std::vector<std::size_t> phrases_sections(const std::string & query)
{
    std::vector<std::size_t> result;
    for (std::size_t i = 0; i < query.size(); i++) {
        if (query[i] == '\"') {
            result.emplace_back(i);
        }
    }
    return result;
}

void delete_from_vector(std::vector<std::string> & vec, const std::string & filename)
{
    auto it = std::remove(vec.begin(), vec.end(), filename);
    vec.erase(it, vec.end());
}

std::vector<std::string> split_query(const std::string & query)
{
    std::vector<std::string> res;

    std::string new_word;
    for (const char & cur : query) {
        if (std::isspace(static_cast<unsigned char>(cur)) || cur == '\"') {
            to_word(new_word);
            if (!new_word.empty()) {
                res.emplace_back(new_word);
                new_word.clear();
            }
        }
        else {
            new_word.append(1, cur);
        }
    }
    // last word
    to_word(new_word);
    if (!new_word.empty()) {
        res.emplace_back(new_word);
    }

    return res;
}

void Searcher::add_document(const Filename & filename, std::istream & strm)
{
    if (std::find(files_list.begin(), files_list.end(), filename) != files_list.end()) {
        remove_document(filename);
    }
    if (!strm.good()) {
        return;
    }
    std::string tmp;
    std::size_t pos = 0;
    while (!strm.eof()) {
        strm >> tmp;
        to_word(tmp);
        if (!tmp.empty()) {
            m_index[tmp][filename].insert(pos);
            pos++;
        }
    }
    files_list.emplace_back(filename);
}

void Searcher::remove_document(const Filename & filename)
{
    delete_from_vector(files_list, filename);
    for (auto cur : m_index) {
        cur.second.erase(filename);
    }
}

std::pair<Searcher::DocIterator, Searcher::DocIterator> Searcher::search(const std::string & query) const
{
    if (query.empty()) {
        throw BadQuery("Empty query");
    }
    DocVector result = files_list;
    Words words = split_query(query);

    if (words.empty()) {
        throw BadQuery("Empty query");
    }

    for (const Filename & file : files_list) {
        if (!contains_all_words(words, file)) {
            delete_from_vector(result, file);
        }
    }

    std::vector<std::size_t> indexes = phrases_sections(query);
    if (indexes.size() % 2 != 0) {
        throw BadQuery("Wrond number of quotes", query);
    }

    for (std::size_t i = 0; i < indexes.size(); i += 2) {
        DocVector ph_files;
        auto start = indexes[i];
        auto end = indexes[i + 1];
        auto s = query.substr(start, end - start);
        Words new_phrase = split_query(s);

        for (const Filename & cur_f : result) {
            for (std::size_t pose : m_index.at(new_phrase[0]).at(cur_f)) {

                std::size_t begin = pose;
                for (std::size_t j = 1; j < new_phrase.size(); j++) {
                    if (m_index.at(new_phrase[j]).at(cur_f).find(pose + 1) == m_index.at(new_phrase[j]).at(cur_f).end()) {
                        break;
                    }
                    pose++;
                }

                if (pose == begin + new_phrase.size() - 1) {
                    ph_files.emplace_back(cur_f);
                    break;
                }
            }
        }
        result = ph_files;
    }

    auto answ = std::make_shared<DocVector>(std::move(result));
    return {answ, answ->end()};
}

bool Searcher::contains_all_words(const Searcher::Words & words, const Filename & file) const
{
    for (const std::string & w : words) {
        if (m_index.find(w) == m_index.end() || m_index.at(w).find(file) == m_index.at(w).end()) {
            return false;
        }
    }
    return true;
}
