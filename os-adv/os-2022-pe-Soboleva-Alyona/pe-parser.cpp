#include <iostream>
#include <cstdio>
#include "pe-parser.h"

int main(int argc, char *argv[]) {
    if (argc < 2) {
        return 0;
    }

    map_available_commands();
    std::string command = argv[1];
    if (argc < 3) {
        std::cout << "File name expected";
        return 0;
    }
    char *file = argv[2];
    switch (command_mapping[command]) {
        case IS_PE_COMMAND:
            if (is_pe_function(file)) {
                std::cout << "PE\n";
                return 0;
            } else {
                std::cout << "Not PE\n";
                return 1;
            }
        case IMPORT_FUNCTIONS_COMMAND:
            print_dependencies_of_executable(file);
            break;
        case EXPORT_FUNCTIONS_COMMAND:
            print_export_functions(file);
            break;
        default:
            std::cout << "Unknown command\n";
    }
    return 0;
}

bool is_pe_function(char *file) {
    FILE *ptrFile = std::fopen(file, "r");
    if (!get_offset_to_pe_flag(ptrFile)) {
        return false;
    }
    char pe_flag[4];
    std::size_t read_bytes = fread(pe_flag, sizeof(char), 4, ptrFile);
    fclose(ptrFile);
    if (read_bytes == 4) {
        return pe_flag[0] == 'P' && pe_flag[1] == 'E' && pe_flag[2] == '\0' && pe_flag[3] == '\0';
    }
    return false;
}

void from_pe_to_optional_header(FILE *file) {
    fseek(file, 4 + COFF_SIZE, SEEK_CUR); // 4 - to skip PE signature
}

int find_section_and_raw(FILE *file, int rva, int &section_rva, int &section_raw, long sections_offset) {
    long pos = ftell(file);
    int section_virtual_size;
    fseek(file, sections_offset, SEEK_SET);
    do {
        fseek(file, 0x8, SEEK_CUR);
        fread(&section_virtual_size, sizeof(int), 1, file);
        fread(&section_rva, sizeof(int), 1, file);
        fseek(file, 0x4, SEEK_CUR);
        fread(&section_raw, sizeof(int), 1, file);
        fseek(file, 16, SEEK_CUR);
    } while (rva < section_rva || rva > section_rva + section_virtual_size);
    fseek(file, pos, SEEK_SET);
    return section_raw + rva - section_rva;
}

void print_dependencies_of_executable(char *file_name) {
    FILE *file = std::fopen(file_name, "r");
    if (!get_offset_to_pe_flag(file)) {
        std::cout << "Problems while reading file";
        return;
    }
    from_pe_to_optional_header(file);

    fseek(file, TABLE_ADDR_OFFSET, SEEK_CUR);
    int import_table_rva, import_table_size;
    fread(&import_table_rva, sizeof(int), 1, file);
    fread(&import_table_size, sizeof(int), 1, file);

    int section_rva, section_raw;
    fseek(file, 112, SEEK_CUR); // to skip optional header
    long sections_offset = ftell(file);


    int import_raw = find_section_and_raw(file, import_table_rva, section_rva, section_raw, sections_offset);
    fseek(file, import_raw, SEEK_SET);

    char cur_dependency[20];
    fread(cur_dependency, 20, 1, file);
    do {
        long pos = ftell(file);
        int lookup_rva = *((int *) cur_dependency);
        int lookup_raw = find_section_and_raw(file, lookup_rva, section_rva, section_raw, sections_offset);

        int table_name_rva = *((int *) &cur_dependency[0xC]);
        int table_name_raw = find_section_and_raw(file, table_name_rva, section_rva, section_raw, sections_offset);
        fseek(file, table_name_raw, SEEK_SET);

        char name[64];
        fread(name, 64, 1, file);
        printf("%s\n", name);

        print_import_functions(file, sections_offset, lookup_raw, section_rva, section_raw);

        fseek(file, pos, SEEK_SET);
        fread(cur_dependency, 20, 1, file);

    } while (!cmp_char_buffs(cur_dependency, TWENTY_ZEROES, 20));

}

void print_import_functions(FILE *file, long sections_offset, int lookup_raw, int &section_rva, int &section_raw) {
    lookup_table_entry lookup_entry = {};
    fseek(file, lookup_raw, SEEK_SET);
    fread(&lookup_entry, 8, 1, file);
    do {
        if (lookup_entry.name_flag == 0) {
            long lookup_pos = ftell(file);

            int func_name_raw = find_section_and_raw(file, lookup_entry.name_rva, section_rva, section_raw, sections_offset);
            func_name_raw += 2; // offset to string
            fseek(file, func_name_raw, SEEK_SET);

            char func_name[64];
            fread(func_name, 64, 1, file);
            printf("    %s\n", func_name);
            fseek(file, lookup_pos, SEEK_SET);
        }
        fread(&lookup_entry, 8, 1, file);
    } while (!cmp_char_buffs((char*) &lookup_entry, EIGHT_ZEROES, 8));
}

void print_export_functions(char *file_name) {
    FILE *file = std::fopen(file_name, "r");
    if (!get_offset_to_pe_flag(file)) {
        std::cout << "Problems while reading file";
        return;
    }
    from_pe_to_optional_header(file);

    fseek(file, EXPORT_TABLE_ADDR_OFFSET, SEEK_CUR);
    int export_table_rva, export_table_size;
    fread(&export_table_rva, sizeof(int), 1, file);
    fread(&export_table_size, sizeof(int), 1, file);

    int section_rva, section_raw;
    fseek(file, 120, SEEK_CUR); // to skip optional header
    long sections_offset = ftell(file);

    int export_raw = find_section_and_raw(file, export_table_rva, section_rva, section_raw, sections_offset);
    fseek(file, export_raw + 24, SEEK_SET); // to get number of name pointers

    int number_of_name_pointers;
    fread(&number_of_name_pointers, sizeof (int), 1, file);

    int name_pointers_rva;
    fseek(file, export_raw + 32, SEEK_SET); // rva of name pointers
    fread(&name_pointers_rva, sizeof (int), 1, file);

    int name_pointers_raw = find_section_and_raw(file, name_pointers_rva, section_rva, section_raw, sections_offset);
    fseek(file, name_pointers_raw, SEEK_SET);

    for (int i = 1; i <= number_of_name_pointers; i++) {
        int export_name_rva;
        fread(&export_name_rva, sizeof(int), 1, file);
        int export_name_raw = find_section_and_raw(file, export_name_rva, section_rva, section_raw, sections_offset);
        fseek(file, export_name_raw, SEEK_SET);
        char name[64];
        fread(name, 64, 1, file);
        printf("%s\n", name);
        fseek(file, name_pointers_raw + i * 4, SEEK_SET);
    }
}


bool get_offset_to_pe_flag(FILE *ptrFile) {
    if (!ptrFile) {
        std::cout << "Can't open file\n";
        return false;
    }
    fseek(ptrFile, PE_FLAG_OFFSET, SEEK_SET);

    int pe_pos;
    fread(&pe_pos, sizeof(int), 1, ptrFile);

    fseek(ptrFile, pe_pos, SEEK_SET);
    return true;
}
