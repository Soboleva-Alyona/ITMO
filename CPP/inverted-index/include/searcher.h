#pragma once

#include <iostream>
#include <map>
#include <memory>
#include <set>
#include <string>
#include <unordered_map>
#include <utility>
#include <vector>

class Searcher
{
public:
    using Filename = std::string; // or std::filesystem::path
    using DocVector = std::vector<std::string>;
    using Words = std::vector<std::string>;
    using Indexes = std::vector<std::size_t>;

    // index modification
    void add_document(const Filename & filename, std::istream & strm);

    void remove_document(const Filename & filename);

    // queries
    class DocIterator
    {
    public:
        using iterator_category = std::forward_iterator_tag;
        using difference_type = std::ptrdiff_t;
        using value_type = const Filename;
        using pointer = const value_type *;
        using reference = const value_type &;

        DocIterator() = default;

        DocIterator(const std::shared_ptr<DocVector> & shared_t)
            : doc_ptr(shared_t)
            , doc_it(doc_ptr->begin())
        {
        }

        DocIterator(const DocVector::iterator & iterator)
            : doc_it(iterator)
        {
        }

        reference operator*() const
        {
            return *doc_it;
        }

        pointer operator->() const
        {
            return doc_it.operator->();
        }

        DocIterator & operator++()
        {
            ++doc_it;
            return *this;
        }

        DocIterator operator++(int)
        {
            DocIterator temp = *this;
            operator++();
            return temp;
        }

        friend bool operator==(const DocIterator & a, const DocIterator & b)
        {
            return a.doc_it == b.doc_it;
        }

        friend bool operator!=(const DocIterator & a, const DocIterator & b)
        {
            return !(a == b);
        }

    private:
        std::shared_ptr<DocVector> doc_ptr;
        DocVector::iterator doc_it;
    };

    class BadQuery : public std::exception
    {
    public:
        explicit BadQuery(const std::string & error_message)
            : m_error_message(error_message)
        {
        }

        explicit BadQuery(const std::string & error_message, const std::string & query)
            : m_error_message(error_message + " in query \"" + query + '\"')
        {
        }

        const char * what() const noexcept
        {
            return m_error_message.c_str();
        }

    private:
        const std::string m_error_message;
    };

    std::pair<DocIterator, DocIterator> search(const std::string & query) const;

private:
    bool contains_all_words(const Words & words, const Filename & file) const;

    DocVector files_list;
    std::unordered_map<std::string, std::unordered_map<Filename, std::set<std::size_t>>> m_index;
};
