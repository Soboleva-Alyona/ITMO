#ifndef PE_HW_SOBOLEVA_ELENA
#define PE_HW_SOBOLEVA_ELENA

#include <string>
#include <map>

const std::string IS_PE_COMMAND_STR = "is-pe";
const std::string IMPORT_FUNCTIONS_COMMAND_STR = "import-functions";
const std::string EXPORT_FUNCTIONS_COMMAND_STR = "export-functions";

const int IS_PE_COMMAND = 0;
const int IMPORT_FUNCTIONS_COMMAND = 1;
const int EXPORT_FUNCTIONS_COMMAND = 2;

std::map<std::string, int> command_mapping;

const int PE_FLAG_OFFSET = 0x3C;
const int COFF_SIZE = 20;
const int EXPORT_TABLE_ADDR_OFFSET = 0x70;
const int TABLE_ADDR_OFFSET = 0x78;

char *TWENTY_ZEROES = "\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0";
char *EIGHT_ZEROES = "\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0";

struct lookup_table_entry {
    int name_rva : 31;
    int blank: 32;
    int name_flag: 1; // 0 - by name
};

bool cmp_char_buffs(char *buf1, char *buf2, int size) {
    for (int i = 0; i < size; i++) {
        if (buf1[i] != buf2[i]) {
            return false;
        }
    }
    return true;
}

void map_available_commands() {
    command_mapping[IS_PE_COMMAND_STR] = IS_PE_COMMAND;
    command_mapping[IMPORT_FUNCTIONS_COMMAND_STR] = IMPORT_FUNCTIONS_COMMAND;
    command_mapping[EXPORT_FUNCTIONS_COMMAND_STR] = EXPORT_FUNCTIONS_COMMAND;
}

bool get_offset_to_pe_flag(FILE *ptrFile);

void from_pe_to_optional_header(FILE *file);

int find_section_and_raw(FILE *file, int rva, int &section_rva, int &section_raw, int sections_offset);

bool is_pe_function(char *file);

void print_import_functions(FILE *file, long sections_offset, int lookup_raw, int &section_rva, int &section_raw);

void print_dependencies_of_executable(char *file_name);

void print_export_functions(char *file);

#endif