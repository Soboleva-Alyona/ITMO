#include "searcher.h"

#include <fstream>
#include <iostream>
#include <iterator>

int main(int argc, char ** argv)
{
    Searcher s;
    
    for (int i = 1; i < argc; ++i) {
        std::ifstream f(argv[i]);
        s.add_document(argv[i], f);
    }
    /*
    std::string line;
    while (std::getline(std::cin, line)) {
        const auto [begin, end] = s.search(line);
        std::ostream_iterator<Searcher::Filename> out(std::cout, ", ");
        std::copy(begin, end, out);
        std::cout << std::endl;
    }
    */
    //s.print();
    //std::cout << '\n';
    //s.remove_document("txt.txt");
    auto [begin, end] = s.search("");
    std::cout << std::distance(begin, end);
    //s.print();
    return 0;
}
