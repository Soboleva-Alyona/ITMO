#include <cstring>
#include <string>
#include <iostream>
#include <fstream>
#include <utility>

namespace {
    void print_out(std::string s, const bool count, int cnt, const bool unique, const bool repeated) {
        if (!count) {
            if ((!unique && repeated && cnt > 1) || (!repeated && unique && cnt == 1) || (!unique && !repeated)) {
                std::cout << s << "\n";
            }
        } else {
            if ((!unique && repeated && cnt > 1) || (!repeated && unique && cnt == 1) || (!unique && !repeated)) {
                std::cout << "      " << cnt << " " << s << "\n";
            }
        }
    }

    void uniq_stream(std::istream &input, const bool unique, const bool count, const bool repeated) {

        std::string prev;
        std::string line;
        if (std::getline(input, line)) {
            prev = line;
        } else {
            return;
        }
        int cnt = 1;
        while (std::getline(input, line)) {
            if (prev == line) {
                cnt++;
            }

            if (prev != line) {
                print_out(prev, count, cnt, unique, repeated);
                cnt = 1;
            }
            prev = std::move(line);
        }

        print_out(prev, count, cnt, unique, repeated);

    }
}

int main(int argc, char **argv) {
    bool count = false;
    bool repeated = false;
    bool unique = false;

    const char *FILE = nullptr;

    for (int i = 1; i < argc; i++) {
        if (argv[i][0] == '-') {
            if (argv[i][1] != '-') {
                const size_t len = std::strlen(argv[i]);
                for (size_t j = 1; j < len; j++) {
                    switch (argv[i][j]) {
                        case 'u':
                            unique = true;
                            break;
                        case 'c':
                            count = true;
                            break;
                        case 'd':
                            repeated = true;
                            break;
                        default:
                            break;
                    }
                }
            } else if (!std::strcmp(argv[i], "--unique")) {
                unique = true;
            } else if (!std::strcmp(argv[i], "--count")) {
                count = true;
            } else if (!std::strcmp(argv[i], "--repeated")) {
                repeated = true;
            }

        } else {
            FILE = argv[i];
        }
    }

    if (FILE) {
        std::ifstream fin(FILE);
        uniq_stream(fin, unique, count, repeated);
    } else {
        uniq_stream(std::cin, unique, count, repeated);
    }

    return 0;
}
