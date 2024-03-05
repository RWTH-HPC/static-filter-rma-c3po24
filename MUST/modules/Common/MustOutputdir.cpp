#include "MustOutputdir.h"
#include <cstdio>
#include <sys/stat.h> // for mkdir
#include <fstream>

static std::string get_base_output_dir_impl()
{
    char const* from_env = getenv("MUST_OUTPUT_PATH");
    return from_env ? from_env : ".";
}

std::string const& get_base_output_dir()
{
    static std::string const path = get_base_output_dir_impl();
    return path;
}

std::string must_output_dir_with_file(char const* file_name)
{
    std::string result = get_base_output_dir() + "/MUST_Output-files/" + file_name;

    return result;
}

std::ofstream must_output_open_file(char const* file_name)
{
    std::string const path = must_output_dir_with_file(file_name);

    return std::ofstream(path);
}

void must_ensure_dir_exists(char const* path)
{
    struct stat sd;
    if (stat(path, &sd) != 0) {
        mkdir(path, 0755);
    }
}

void must_ensure_output_dir_exists()
{
    static std::string output_dir = get_base_output_dir() + "/MUST_Output-files";
    must_ensure_dir_exists(output_dir.c_str());
}
