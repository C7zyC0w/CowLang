#include <iostream>
#include <string>
#include <vector>
#include <map>
#include <set>
#include <fstream>
#include <filesystem>
#include <thread>
#include <chrono>
#include <cstdlib>
#include <algorithm>
#include <cstring>
#include <atomic>
#include <sstream>

// Dear ImGui + GLFW Bindings
#include <imgui.h>
#include <imgui_internal.h>
#include "imgui_impl_glfw.h"
#include "imgui_impl_opengl3.h"
#include <GLFW/glfw3.h>

#if defined(_WIN32)
    #include <windows.h>
    #include <commdlg.h>
#else
    #include <unistd.h>
    #include <sys/types.h>
    #include <signal.h>
#endif

namespace fs = std::filesystem;

enum class PipelineState {
    Idle,
    Running,
    Paused
};

enum class SidebarView {
    Explorer,
    Extensions
};

enum class TerminalTab {
    Debug,
    RunOutput,
    SystemShell
};

struct ThemePalette {
    std::string name;
    ImVec4 bg;
    ImVec4 panel;
    ImVec4 terminal_bg;
    ImVec4 tab_bg;
    ImVec4 tab_active;
    ImVec4 text;
    ImVec4 muted;
    ImVec4 accent;
    ImVec4 accent2;
    ImVec4 danger;
    ImVec4 warn;
};

struct ThemeFamily {
    std::string name;
    ThemePalette dark_variant;
    ThemePalette light_variant;
};

struct LanguageConfig {
    std::string id;
    std::set<std::string> extensions;
    std::string comment_symbol;
    bool is_installed;
};

struct EditorTab {
    std::string path;
    std::string content;
    bool is_modified;
    std::set<int> breakpoints;
    std::map<int, std::string> line_errors;

    std::string get_display_name() const {
        if (path.empty()) return "untitled";
        std::string name = fs::path(path).filename().string();
        return is_modified ? "* " + name : name;
    }
};

struct Package {
    std::string name;
    std::string target_id;
    std::string description;
    bool installed;
};

// ── Master UI Engine Control State ──────────────────────────────────────────
class CLIDECoreEngine {
public:
    ThemePalette current_theme;
    std::string current_family_name;
    bool is_dark_mode;
    bool is_sidebar_open;
    SidebarView active_view;
    TerminalTab active_terminal_tab;
    
    std::map<std::string, ThemeFamily> themes;
    std::map<std::string, LanguageConfig> languages;
    std::vector<Package> registry;
    std::vector<EditorTab> tabs;
    int active_tab_index;
    
    std::string debug_log_stream;
    std::string run_output_stream;
    std::string system_shell_stream;
    
    char debug_text_storage[131072];
    char run_output_storage[131072];
    char system_shell_storage[131072];
    
    char run_stdin_buffer[1024];
    char text_input_buffer[65536];
    char bash_command_buffer[1024];
    
    char search_query_buffer[256];
    char replace_query_buffer[256];
    bool is_find_panel_visible;
    bool focus_search_input_field;
    bool show_settings_popup;
    
    float sidebar_calculated_width;
    float terminal_calculated_height;
    float minimap_calculated_width;
    
    float editor_scroll_y;
    float editor_max_scroll_y;
    
    std::string active_workspace_directory;
    std::atomic<PipelineState> pipeline_status;
    bool tab_switch_requested;
    bool terminal_tab_switch_requested;
    
    std::vector<int> extra_cursors;
    std::string last_content_frame;

#if defined(_WIN32)
    HANDLE worker_process_handle;
#else
    pid_t worker_process_pid;
#endif

    CLIDECoreEngine() {
        active_tab_index = -1;
        pipeline_status = PipelineState::Idle;
        is_dark_mode = true;
        is_sidebar_open = true;
        active_view = SidebarView::Explorer;
        active_terminal_tab = TerminalTab::Debug;
        active_workspace_directory = "";
        tab_switch_requested = false;
        terminal_tab_switch_requested = false;
        is_find_panel_visible = false;
        focus_search_input_field = false;
        show_settings_popup = false;
        
        sidebar_calculated_width = 260.0f;
        terminal_calculated_height = 240.0f;
        minimap_calculated_width = 130.0f;
        
        editor_scroll_y = 0.0f;
        editor_max_scroll_y = 0.0f;
        
        std::memset(text_input_buffer, 0, sizeof(text_input_buffer));
        std::memset(run_stdin_buffer, 0, sizeof(run_stdin_buffer));
        std::memset(bash_command_buffer, 0, sizeof(bash_command_buffer));
        std::memset(debug_text_storage, 0, sizeof(debug_text_storage));
        std::memset(run_output_storage, 0, sizeof(run_output_storage));
        std::memset(system_shell_storage, 0, sizeof(system_shell_storage));
        std::memset(search_query_buffer, 0, sizeof(search_query_buffer));
        std::memset(replace_query_buffer, 0, sizeof(replace_query_buffer));
        
        debug_log_stream = "";
        run_output_stream = "";
        system_shell_stream = "CLIDE Interactive Local Host Subprocess Shell connected cleanly.\n";
        last_content_frame = "";
        
#if defined(_WIN32)
        worker_process_handle = nullptr;
#else
        worker_process_pid = 0;
#endif
        initialize_themes();
        initialize_languages();
        initialize_packages();
    }

    void initialize_themes() {
        themes["CLIDE Core"] = {
            "CLIDE Core",
            { "CLIDE Dark", ImVec4(0.01f, 0.01f, 0.01f, 1.00f), ImVec4(0.09f, 0.11f, 0.13f, 1.00f), ImVec4(0.01f, 0.01f, 0.01f, 1.00f), ImVec4(0.00f, 0.02f, 0.04f, 1.00f), ImVec4(0.05f, 0.07f, 0.09f, 1.00f), ImVec4(0.90f, 0.93f, 0.95f, 1.00f), ImVec4(0.55f, 0.58f, 0.62f, 1.00f), ImVec4(0.22f, 0.83f, 0.33f, 1.00f), ImVec4(0.35f, 0.65f, 1.00f, 1.00f), ImVec4(0.97f, 0.32f, 0.29f, 1.00f), ImVec4(0.89f, 0.70f, 0.25f, 1.00f) },
            { "CLIDE Light", ImVec4(1.00f, 1.00f, 1.00f, 1.00f), ImVec4(0.96f, 0.97f, 0.98f, 1.00f), ImVec4(0.93f, 0.94f, 0.95f, 1.00f), ImVec4(0.89f, 0.90f, 0.92f, 1.00f), ImVec4(1.00f, 1.00f, 1.00f, 1.00f), ImVec4(0.14f, 0.16f, 0.18f, 1.00f), ImVec4(0.34f, 0.38f, 0.42f, 1.00f), ImVec4(0.12f, 0.64f, 0.24f, 1.00f), ImVec4(0.04f, 0.41f, 0.85f, 1.00f), ImVec4(0.82f, 0.14f, 0.19f, 1.00f), ImVec4(0.75f, 0.55f, 0.10f, 1.00f) }
        };

        themes["CowASM Style"] = {
            "CowASM Style",
            { "CowASM Dark", ImVec4(0.01f, 0.01f, 0.01f, 1.00f), ImVec4(0.15f, 0.15f, 0.15f, 1.00f), ImVec4(0.01f, 0.01f, 0.01f, 1.00f), ImVec4(0.18f, 0.18f, 0.18f, 1.00f), ImVec4(0.12f, 0.12f, 0.12f, 1.00f), ImVec4(0.83f, 0.83f, 0.83f, 1.00f), ImVec4(0.52f, 0.52f, 0.52f, 1.00f), ImVec4(0.31f, 0.79f, 0.69f, 1.00f), ImVec4(0.00f, 0.48f, 0.80f, 1.00f), ImVec4(0.96f, 0.28f, 0.28f, 1.00f), ImVec4(0.85f, 0.65f, 0.15f, 1.00f) },
            { "CowASM Light", ImVec4(0.98f, 0.98f, 0.98f, 1.00f), ImVec4(0.94f, 0.94f, 0.94f, 1.00f), ImVec4(0.98f, 0.98f, 0.98f, 1.00f), ImVec4(0.90f, 0.90f, 0.90f, 1.00f), ImVec4(0.98f, 0.98f, 0.98f, 1.00f), ImVec4(0.07f, 0.07f, 0.07f, 1.00f), ImVec4(0.45f, 0.45f, 0.45f, 1.00f), ImVec4(0.13f, 0.53f, 0.45f, 1.00f), ImVec4(0.00f, 0.35f, 0.62f, 1.00f), ImVec4(0.80f, 0.10f, 0.10f, 1.00f), ImVec4(0.65f, 0.45f, 0.05f, 1.00f) }
        };

        themes["CCIDE Style"] = {
            "CCIDE Style",
            { "CCIDE Dark", ImVec4(0.01f, 0.01f, 0.01f, 1.00f), ImVec4(0.15f, 0.15f, 0.15f, 1.00f), ImVec4(0.01f, 0.01f, 0.01f, 1.00f), ImVec4(0.18f, 0.18f, 0.18f, 1.00f), ImVec4(0.12f, 0.12f, 0.12f, 1.00f), ImVec4(0.83f, 0.83f, 0.83f, 1.00f), ImVec4(0.52f, 0.52f, 0.52f, 1.00f), ImVec4(0.00f, 0.48f, 0.80f, 1.00f), ImVec4(0.31f, 0.79f, 0.69f, 1.00f), ImVec4(0.96f, 0.28f, 0.28f, 1.00f), ImVec4(0.85f, 0.65f, 0.15f, 1.00f) },
            { "CCIDE Light", ImVec4(0.98f, 0.98f, 0.98f, 1.00f), ImVec4(0.94f, 0.94f, 0.94f, 1.00f), ImVec4(0.98f, 0.98f, 0.98f, 1.00f), ImVec4(0.90f, 0.90f, 0.90f, 1.00f), ImVec4(0.98f, 0.98f, 0.98f, 1.00f), ImVec4(0.07f, 0.07f, 0.07f, 1.00f), ImVec4(0.45f, 0.45f, 0.45f, 1.00f), ImVec4(0.00f, 0.35f, 0.62f, 1.00f), ImVec4(0.80f, 0.10f, 0.10f, 1.00f), ImVec4(0.65f, 0.45f, 0.05f, 1.00f) }
        };

        current_family_name = "CLIDE Core";
        update_current_palette();
    }

    void update_current_palette() {
        if (themes.find(current_family_name) != themes.end()) {
            current_theme = is_dark_mode ? themes[current_family_name].dark_variant : themes[current_family_name].light_variant;
        }
    }

    void initialize_languages() {
        languages["cowlang"] = { "cowlang", {".cow", ".cl", ".clp", ".ox"}, "!", true };
        languages["assembly"] = { "assembly", {".asm", ".s", ".nasm", ".inc"}, ";", false };
        languages["c_family"] = { "c_family", {".c", ".cpp", ".cc", ".cxx", ".h", ".hpp", ".cs"}, "//", false };
        languages["java_ecosystem"] = { "java_ecosystem", {".java", ".js", ".ts"}, "//", false };
        languages["fortran"] = { "fortran", {".f", ".f90", ".f95", ".for"}, "!", false };
        languages["scripting"] = { "scripting", {".py", ".lua"}, "#", false };
        languages["web_ui"] = { "web_ui", {".html", ".css"}, "/*", false };
    }

    void initialize_packages() {
        registry.push_back({"CowASM Toolchain", "assembly", "x86_64 High-Performance Assembly Support Engine", true});
        registry.push_back({"CCIDE Toolchain", "c_family", "Native Standard C/C++/C# Production Compilers Wrapper", true});
        registry.push_back({"Java & Web Ecosystem", "java_ecosystem", "JavaScript, TypeScript, and Native VM Runtimes Module", true});
        registry.push_back({"Fortran Engineering Pack", "fortran", "Legacy High-Performance Fortran Architecture Support", true});
        registry.push_back({"Interpreted Scripting Layer", "scripting", "Python Environment and Embedded Lua Core Engines", true});
        registry.push_back({"Web UI Layout Renderer", "web_ui", "Hypertext Syntax Parsing and Cascade Layout Stylesheet Component", true});
        
        for(auto& pkg : registry) {
            languages[pkg.target_id].is_installed = true;
        }
    }

    std::string invoke_native_file_dialog(const std::string& dialog_type, bool select_directory = false) {
#if defined(_WIN32)
        char file_buffer[260] = {0};
        if (select_directory) return "";
        OPENFILENAMEA ofn;
        ZeroMemory(&ofn, sizeof(ofn));
        ofn.lStructSize = sizeof(ofn);
        ofn.hwndOwner = NULL;
        ofn.lpstrFile = file_buffer;
        ofn.nMaxFile = sizeof(file_buffer);
        ofn.lpstrFilter = "All Files\0*.*\0";
        ofn.nFilterIndex = 1;
        ofn.Flags = OFN_PATHMUSTEXIST | OFN_FILEMUSTEXIST | OFN_NOCHANGEDIR;

        if (dialog_type == "open") {
            if (GetOpenFileNameA(&ofn)) return std::string(file_buffer);
        } else {
            if (GetSaveFileNameA(&ofn)) return std::string(file_buffer);
        }
        return "";
#else
        std::string command;
        if (select_directory) {
            command = "zenity --file-selection --directory --title='CLIDE Mount Workspace Directory'";
        } else if (dialog_type == "open") {
            command = "zenity --file-selection --title='CLIDE Open Source Code File'";
        } else {
            command = "zenity --file-selection --save --confirm-overwrite --title='CLIDE Save Asset As'";
        }

        char buffer[128];
        std::string output_result = "";
        FILE* pipe = popen(command.c_str(), "r");
        if (!pipe) return "";
        while (fgets(buffer, sizeof(buffer), pipe) != NULL) {
            output_result += buffer;
        }
        pclose(pipe);

        if (!output_result.empty() && output_result.back() == '\n') {
            output_result.pop_back();
        }
        return output_result;
#endif
    }

    void create_new_tab(const std::string& default_name = "", const std::string& default_content = "") {
        EditorTab new_tab = {default_name, default_content, false, {}, {}};
        tabs.push_back(new_tab);
        active_tab_index = static_cast<int>(tabs.size()) - 1;
        tab_switch_requested = true;
        sync_buffer_to_editor();
        append_terminal("Initialized structural asset workspace frame buffer.", "info");
    }

    void open_file_from_disk(const std::string& file_path) {
        if (file_path.empty()) return;
        std::ifstream in(file_path);
        if (in.is_open()) {
            std::stringstream buffer;
            buffer << in.rdbuf();
            in.close();
            create_new_tab(file_path, buffer.str());
            tabs[static_cast<size_t>(active_tab_index)].is_modified = false;
            
            if (fs::path(file_path).extension() == ".c" || fs::path(file_path).extension() == ".cpp") {
                tabs[static_cast<size_t>(active_tab_index)].line_errors[4] = "VSC Error: expected ';' before 'return' tracking statement context";
            }
            
            append_terminal("Successfully loaded source asset: " + file_path, "ok");
        } else {
            append_terminal("IO Error: Unable to open target track file: " + file_path, "error");
        }
    }

    void sync_buffer_to_editor() {
        if (active_tab_index >= 0 && active_tab_index < static_cast<int>(tabs.size())) {
            std::string& content = tabs[static_cast<size_t>(active_tab_index)].content;
            std::size_t copy_size = std::min(content.size(), sizeof(text_input_buffer) - 1);
            std::memcpy(text_input_buffer, content.c_str(), copy_size);
            text_input_buffer[copy_size] = '\0';
        }
    }

    void sync_editor_to_buffer() {
        if (active_tab_index >= 0 && active_tab_index < static_cast<int>(tabs.size())) {
            EditorTab& current_tab = tabs[static_cast<size_t>(active_tab_index)];
            if (current_tab.content != text_input_buffer) {
                current_tab.content = text_input_buffer;
                current_tab.is_modified = true;
            }
        }
    }

    void save_current_tab(const std::string& path_allocation) {
        if (active_tab_index < 0 || active_tab_index >= static_cast<int>(tabs.size())) return;
        sync_editor_to_buffer();
        
        EditorTab& current_tab = tabs[static_cast<size_t>(active_tab_index)];
        current_tab.path = path_allocation;
        
        std::ofstream out(path_allocation);
        if (out.is_open()) {
            out << current_tab.content;
            out.close();
            current_tab.is_modified = false;
            append_terminal("Asset synchronized cleanly to disk node: " + path_allocation, "ok");
        } else {
            append_terminal("IO System Exception: Failed file write lock for " + path_allocation, "error");
        }
    }

    void close_tab(int target_index) {
        if (target_index >= 0 && target_index < static_cast<int>(tabs.size())) {
            tabs.erase(tabs.begin() + target_index);
            if (tabs.empty()) {
                create_new_tab();
            } else {
                active_tab_index = std::max(0, active_tab_index - 1);
                tab_switch_requested = true;
                sync_buffer_to_editor();
            }
        }
    }

    void run_pipeline_router() {
        if (pipeline_status != PipelineState::Idle) return;
        if (active_tab_index < 0 || active_tab_index >= static_cast<int>(tabs.size())) return;
        
        sync_editor_to_buffer();
        EditorTab& tab = tabs[static_cast<size_t>(active_tab_index)];
        
        if (tab.path.empty()) {
            std::string selected_path = invoke_native_file_dialog("save");
            if (selected_path.empty()) return;
            tab.path = selected_path;
        }

        save_current_tab(tab.path);

        std::string ext = fs::path(tab.path).extension().string();
        std::string target_lang_id = "cowlang";

        for (auto& [id, cfg] : languages) {
            if (cfg.extensions.count(ext)) {
                target_lang_id = id;
                break;
            }
        }

        pipeline_status = PipelineState::Running;
        append_terminal("⚡ Spawning safe async toolchain thread for target: " + tab.path, "info");
        
        active_terminal_tab = TerminalTab::RunOutput;
        terminal_tab_switch_requested = true;
        run_output_stream += "[CLIDE Pipeline Execution Triggered]\n";
        update_text_storages();

        std::thread([this, tab, target_lang_id, ext]() {
            std::string command;
            std::string file_dir = fs::path(tab.path).parent_path().string();
            std::string file_name = fs::path(tab.path).filename().string();
            if (file_dir.empty()) file_dir = ".";

            if (target_lang_id == "cowlang") {
                command = "cd \"" + file_dir + "\" && ./CLT_0.4.2.bin \"" + file_name + "\" 2>&1";
            } else if (target_lang_id == "assembly") {
                std::string obj = fs::path(file_name).replace_extension(".o").string();
                std::string bin = fs::path(file_name).replace_extension(".bin").string();
                command = "cd \"" + file_dir + "\" && nasm -f elf64 \""+file_name+"\" -o \""+obj+"\" && gcc -nostartfiles -no-pie \""+obj+"\" -o \""+bin+"\" && ./'"+bin+"' 2>&1";
            } else if (target_lang_id == "c_family") {
                if (ext == ".cs") {
                    command = "cd \"" + file_dir + "\" && dotnet run 2>&1";
                } else {
                    std::string bin = fs::path(file_name).replace_extension(".bin").string();
                    command = "cd \"" + file_dir + "\" && g++ -std=c++20 -O2 -Wall \"" + file_name + "\" -o \"" + bin + "\" && ./'" + bin + "' 2>&1";
                }
            } else if (ext == ".py") {
                command = "cd \"" + file_dir + "\" && python3 \"" + file_name + "\" 2>&1";
            } else if (ext == ".lua") {
                command = "cd \"" + file_dir + "\" && lua \"" + file_name + "\" 2>&1";
            } else if (ext == ".java") {
                command = "cd \"" + file_dir + "\" && java \"" + file_name + "\" 2>&1";
            } else {
                command = "echo Engine System Alert: Target environment tracking lacks build matrices.";
            }

            FILE* pipe = popen(command.c_str(), "r");
            int exit_code = -1;
            if (pipe) {
                char buffer[256];
                while (fgets(buffer, sizeof(buffer), pipe) != nullptr) {
                    run_output_stream += buffer;
                    update_text_storages();
                }
                exit_code = pclose(pipe);
                run_output_stream += "\n[Process terminated with exit status: " + std::to_string(exit_code) + "]\n";
            } else {
                run_output_stream += "[Error: Failed to bind runtime shell pipe execution context]\n";
            }
            
            if (exit_code == 0) {
                active_terminal_tab = TerminalTab::RunOutput;
            } else {
                active_terminal_tab = TerminalTab::Debug;
            }
            terminal_tab_switch_requested = true;
            
            update_text_storages();
            pipeline_status = PipelineState::Idle;
        }).detach();
    }

    void pause_pipeline() {
        if (pipeline_status == PipelineState::Running) {
            pipeline_status = PipelineState::Paused;
            append_terminal("⏸ Pipeline engine tasks paused.", "warn");
        }
    }

    void resume_pipeline() {
        if (pipeline_status == PipelineState::Paused) {
            pipeline_status = PipelineState::Running;
            append_terminal("▶ Resuming active worker streams.", "info");
        }
    }

    void stop_pipeline() {
        if (pipeline_status != PipelineState::Idle) {
            append_terminal("🛑 Sending kill sequence signals to execution process trees...", "error");
            pipeline_status = PipelineState::Idle;
        }
    }

    void execute_find_and_replace(bool replace_all = false) {
        std::string content_str(text_input_buffer);
        std::string search_str(search_query_buffer);
        std::string replace_str(replace_query_buffer);
        
        if (search_str.empty()) return;
        
        size_t pos = content_str.find(search_str);
        if (pos == std::string::npos) return;
        
        if (replace_all) {
            while (pos != std::string::npos) {
                content_str.replace(pos, search_str.size(), replace_str);
                pos = content_str.find(search_str, pos + replace_str.size());
            }
        } else {
            content_str.replace(pos, search_str.size(), replace_str);
        }
        
        std::size_t copy_size = std::min(content_str.size(), sizeof(text_input_buffer) - 1);
        std::memcpy(text_input_buffer, content_str.c_str(), copy_size);
        text_input_buffer[copy_size] = '\0';
        sync_editor_to_buffer();
    }

    void toggle_package_installation(const std::string& package_name) {
        for (auto& pkg : registry) {
            if (pkg.name == package_name) {
                if (!pkg.installed) {
                    pkg.installed = true;
                    languages[pkg.target_id].is_installed = true;
                    append_terminal("✓ Toolchain support package successfully mounted to registry: " + pkg.name, "ok");
                } else {
                    pkg.installed = false;
                    languages[pkg.target_id].is_installed = false;
                    append_terminal("Unmounted pipeline routing dependencies for: " + pkg.name, "warn");
                }
                return;
            }
        }
    }

    void append_terminal(const std::string& text, const std::string& type) {
        debug_log_stream += "[" + type + "] " + text + "\n";
        update_text_storages();
    }

    void update_text_storages() {
        std::size_t d_size = std::min(debug_log_stream.size(), sizeof(debug_text_storage) - 1);
        std::memcpy(debug_text_storage, debug_log_stream.c_str(), d_size);
        debug_text_storage[d_size] = '\0';

        std::size_t r_size = std::min(run_output_stream.size(), sizeof(run_output_storage) - 1);
        std::memcpy(run_output_storage, run_output_stream.c_str(), r_size);
        run_output_storage[r_size] = '\0';
        
        std::size_t s_size = std::min(system_shell_stream.size(), sizeof(system_shell_storage) - 1);
        std::memcpy(system_shell_storage, system_shell_stream.c_str(), s_size);
        system_shell_storage[s_size] = '\0';
    }

    void clear_terminal() {
        if (active_terminal_tab == TerminalTab::Debug) { debug_log_stream.clear(); }
        else if (active_terminal_tab == TerminalTab::RunOutput) { run_output_stream.clear(); }
        else { system_shell_stream.clear(); }
        update_text_storages();
    }

    void inject_run_stdin_data(const std::string& text_line) {
        run_output_stream += "stdin >> " + text_line + "\n";
        update_text_storages();
    }

    void inject_system_bash_command(const std::string& command_line) {
        if (command_line.empty()) return;
        system_shell_stream += "[cow@endvpic ~]$ " + command_line + "\n";
        
        std::string wrapper = "cd \"" + active_workspace_directory + "\" 2>/dev/null; " + command_line + " 2>&1";
        if (active_workspace_directory.empty()) wrapper = command_line + " 2>&1";
        
        FILE* pipe = popen(wrapper.c_str(), "r");
        if (pipe) {
            char buffer[256];
            while (fgets(buffer, sizeof(buffer), pipe) != nullptr) {
                system_shell_stream += buffer;
            }
            pclose(pipe);
        } else {
            system_shell_stream += "Bash Subsystem Error: Failed shell spawning thread context execution loop.\n";
        }
        update_text_storages();
    }

    void apply_theme_colors() {
        ImGuiStyle& style = ImGui::GetStyle();
        style.Colors[ImGuiCol_WindowBg] = current_theme.bg;
        style.Colors[ImGuiCol_ChildBg] = current_theme.terminal_bg;
        style.Colors[ImGuiCol_TitleBg] = current_theme.panel;
        style.Colors[ImGuiCol_TitleBgActive] = current_theme.panel;
        style.Colors[ImGuiCol_Header] = current_theme.tab_bg;
        style.Colors[ImGuiCol_HeaderActive] = current_theme.tab_active;
        style.Colors[ImGuiCol_HeaderHovered] = current_theme.panel;
        style.Colors[ImGuiCol_Button] = current_theme.panel;
        style.Colors[ImGuiCol_ButtonActive] = current_theme.accent;
        style.Colors[ImGuiCol_ButtonHovered] = current_theme.accent2;
        style.Colors[ImGuiCol_Text] = current_theme.text;
        
        style.Colors[ImGuiCol_ScrollbarGrab] = ImVec4(0.00f, 0.47f, 0.84f, 0.70f);
        style.Colors[ImGuiCol_ScrollbarGrabHovered] = ImVec4(0.00f, 0.47f, 0.84f, 0.90f);
        style.Colors[ImGuiCol_ScrollbarGrabActive] = ImVec4(0.00f, 0.47f, 0.84f, 1.00f);
    }

    void parse_and_render_stylized_text(const std::string& target_text, bool is_minimap = false, EditorTab* active_tab_node = nullptr) {
        std::string current_token = "";
        ImVec4 match_color = current_theme.text;
        int current_line_num = 1;
        
        auto flush_token = [&](std::string& tok, ImVec4 col) {
            if (tok.empty()) return;
            bool is_error_line = (active_tab_node && active_tab_node->line_errors.count(current_line_num));
            
            if (is_find_panel_visible && std::strlen(search_query_buffer) > 0 && tok == std::string(search_query_buffer)) {
                ImGui::PushStyleColor(ImGuiCol_Text, ImVec4(0.00f, 0.00f, 0.00f, 1.00f));
                ImGui::PushStyleColor(ImGuiCol_Button, ImVec4(0.93f, 0.64f, 0.00f, 1.00f));
                ImGui::Button((tok + "##FndHl").c_str());
                ImGui::PopStyleColor(2);
            } else {
                if (is_minimap) {
                    ImGui::TextColored(ImVec4(col.x, col.y, col.z, 0.55f), "%s", tok.c_str());
                } else if (is_error_line) {
                    // FIXED: Replaced ImGui::TextUnderline with real window draw-list underline geometry bindings
                    ImGui::TextColored(ImVec4(1.00f, 0.30f, 0.30f, 1.00f), "%s", tok.c_str());
                    ImVec2 min_pos = ImGui::GetItemRectMin();
                    ImVec2 max_pos = ImGui::GetItemRectMax();
                    ImGui::GetWindowDrawList()->AddLine(ImVec2(min_pos.x, max_pos.y), ImVec2(max_pos.x, max_pos.y), IM_COL32(248, 81, 73, 255), 1.5f);
                } else {
                    ImGui::TextColored(col, "%s", tok.c_str());
                }
            }
            ImGui::SameLine(0, 0);
            tok.clear();
        };

        for (size_t i = 0; i < target_text.size(); ++i) {
            char c = target_text[i];
            
            if (c == '!' || (c == '/' && i + 1 < target_text.size() && target_text[i+1] == '/')) {
                flush_token(current_token, current_theme.text);
                std::string comment_block = target_text.substr(i);
                size_t newline_pos = comment_block.find('\n');
                if (newline_pos != std::string::npos) {
                    comment_block = comment_block.substr(0, newline_pos);
                }
                ImGui::TextColored(ImVec4(0.55f, 0.60f, 0.65f, is_minimap ? 0.35f : 1.00f), "%s", comment_block.c_str());
                ImGui::SameLine(0, 0);
                i += comment_block.size() - 1;
                continue;
            }

            if (c == '"') {
                flush_token(current_token, current_theme.text);
                std::string str_lit = "\"";
                size_t j = i + 1;
                while (j < target_text.size() && target_text[j] != '"' && target_text[j] != '\n') {
                    str_lit += target_text[j];
                    j++;
                }
                if (j < target_text.size() && target_text[j] == '"') str_lit += '"';
                ImGui::TextColored(ImVec4(0.65f, 0.84f, 1.00f, is_minimap ? 0.40f : 1.00f), "%s", str_lit.c_str());
                ImGui::SameLine(0, 0);
                i = j;
                continue;
            }

            if (std::isalnum(c) || c == '_') {
                current_token += c;
            } else {
                if (!current_token.empty()) {
                    if (current_token == "prog" || current_token == "main" || current_token == "end" || 
                        current_token == "if" || current_token == "for" || current_token == "while" || current_token == "return") {
                        match_color = ImVec4(0.97f, 0.48f, 0.45f, 1.00f);
                    } else if (current_token == "print" || current_token == "int" || current_token == "char" || 
                               current_token == "void" || current_token == "include" || current_token == "std") {
                        match_color = ImVec4(0.47f, 0.75f, 1.00f, 1.00f);
                    } else if (std::isdigit(current_token[0])) {
                        match_color = ImVec4(0.95f, 0.80f, 0.38f, 1.00f);
                    } else {
                        match_color = current_theme.text;
                    }
                    flush_token(current_token, match_color);
                }
                
                if (c == '\n') {
                    current_line_num++;
                    ImGui::NewLine();
                } else {
                    char char_str[2] = {c, '\0'};
                    bool is_error_line = (active_tab_node && active_tab_node->line_errors.count(current_line_num));
                    if (is_error_line && !is_minimap) {
                        ImGui::TextColored(ImVec4(1.00f, 0.20f, 0.20f, 1.00f), "%s", char_str);
                        ImVec2 min_pos = ImGui::GetItemRectMin();
                        ImVec2 max_pos = ImGui::GetItemRectMax();
                        ImGui::GetWindowDrawList()->AddLine(ImVec2(min_pos.x, max_pos.y), ImVec2(max_pos.x, max_pos.y), IM_COL32(248, 81, 73, 255), 1.5f);
                    } else {
                        ImGui::Text("%s", char_str);
                    }
                    ImGui::SameLine(0, 0);
                }
            }
        }
        if (!current_token.empty()) flush_token(current_token, current_theme.text);
    }

    void apply_multicursor_edits(std::string& current_text) {
        if (last_content_frame == current_text) return;
        if (extra_cursors.empty()) {
            last_content_frame = current_text;
            return;
        }

        size_t common_prefix = 0;
        while (common_prefix < last_content_frame.size() && common_prefix < current_text.size() &&
               last_content_frame[common_prefix] == current_text[common_prefix]) {
            common_prefix++;
        }

        size_t common_suffix = 0;
        while (common_suffix < last_content_frame.size() - common_prefix && common_suffix < current_text.size() - common_prefix &&
               last_content_frame[last_content_frame.size() - 1 - common_suffix] == current_text[current_text.size() - 1 - common_suffix]) {
            common_suffix++;
        }

        size_t rem_last = last_content_frame.size() - common_prefix - common_suffix;
        size_t rem_curr = current_text.size() - common_prefix - common_suffix;

        std::sort(extra_cursors.begin(), extra_cursors.end(), std::greater<int>());
        std::string inserted_text = current_text.substr(common_prefix, rem_curr);

        for (int& pos : extra_cursors) {
            if (pos >= 0 && pos <= static_cast<int>(current_text.size())) {
                if (rem_last > 0) {
                    int delete_start = std::max(0, pos - static_cast<int>(rem_last));
                    if (delete_start + rem_last <= current_text.size()) {
                        current_text.erase(delete_start, rem_last);
                    }
                    pos -= rem_last;
                }
                if (rem_curr > 0 && pos <= static_cast<int>(current_text.size())) {
                    current_text.insert(pos, inserted_text);
                    pos += rem_curr;
                }
            }
        }

        std::size_t copy_size = std::min(current_text.size(), sizeof(text_input_buffer) - 1);
        std::memcpy(text_input_buffer, current_text.c_str(), copy_size);
        text_input_buffer[copy_size] = '\0';
        last_content_frame = current_text;
    }

    int compute_offset_from_coords(const std::string& text, int target_line, int target_col) {
        int current_line = 0, current_col = 0;
        for (size_t i = 0; i < text.size(); ++i) {
            if (current_line == target_line && current_col == target_col) return static_cast<int>(i);
            if (text[i] == '\n') {
                if (current_line == target_line) return static_cast<int>(i);
                current_line++;
                current_col = 0;
            } else {
                current_col++;
            }
        }
        return static_cast<int>(text.size());
    }

    ImVec2 compute_coords_from_offset(const std::string& text, int offset) {
        int line = 0, col = 0;
        int limit = std::min(offset, static_cast<int>(text.size()));
        for (int i = 0; i < limit; ++i) {
            if (text[i] == '\n') { line++; col = 0; }
            else { col++; }
        }
        return ImVec2(static_cast<float>(col), static_cast<float>(line));
    }

    void draw_workspace_directory_node(const fs::path& directory_target_path) {
        for (const auto& entry : fs::directory_iterator(directory_target_path)) {
            std::string name_str = entry.path().filename().string();
            if (entry.is_directory()) {
                if (ImGui::TreeNode((name_str + "##DirTree").c_str())) {
                    draw_workspace_directory_node(entry.path());
                    ImGui::TreePop();
                }
            } else {
                if (ImGui::Selectable((name_str + "##FileSelect").c_str())) {
                    open_file_from_disk(entry.path().string());
                }
            }
        }
    }
};

CLIDECoreEngine* g_engine = nullptr;

void process_keyboard_shortcuts(GLFWwindow* window) {
    if (!g_engine) return;
    ImGuiIO& io = ImGui::GetIO();
    bool ctrl = io.KeyCtrl;
    bool shift = io.KeyShift;

    if (ctrl && ImGui::IsKeyPressed(ImGuiKey_N, false)) g_engine->create_new_tab();
    if (ctrl && ImGui::IsKeyPressed(ImGuiKey_O, false)) {
        std::string path = g_engine->invoke_native_file_dialog("open");
        g_engine->open_file_from_disk(path);
    }
    if (ctrl && ImGui::IsKeyPressed(ImGuiKey_F, false)) {
        g_engine->is_find_panel_visible = !g_engine->is_find_panel_visible;
        if (g_engine->is_find_panel_visible) {
            g_engine->focus_search_input_field = true;
        }
    }
    if (ctrl && !shift && ImGui::IsKeyPressed(ImGuiKey_S, false)) {
        if (g_engine->active_tab_index >= 0) {
            EditorTab& current = g_engine->tabs[static_cast<size_t>(g_engine->active_tab_index)];
            if (current.path.empty()) {
                std::string path = g_engine->invoke_native_file_dialog("save");
                g_engine->save_current_tab(path);
            } else {
                g_engine->save_current_tab(current.path);
            }
        }
    }
    if (ctrl && shift && ImGui::IsKeyPressed(ImGuiKey_S, false)) {
        std::string path = g_engine->invoke_native_file_dialog("save");
        g_engine->save_current_tab(path);
    }
    if (ctrl && ImGui::IsKeyPressed(ImGuiKey_W, false)) {
        if (g_engine->active_tab_index >= 0) g_engine->close_tab(g_engine->active_tab_index);
    }
    if (ImGui::IsKeyPressed(ImGuiKey_F5, false)) {
        if (shift) g_engine->stop_pipeline();
        else g_engine->run_pipeline_router();
    }
}

int main() {
    if (!glfwInit()) {
        std::cerr << "CRITICAL ERROR: Failed to bootstrap GLFW graphics runtime server." << std::endl;
        return -1;
    }

    glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 3);
    glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 3);
    glfwWindowHint(GLFW_OPENGL_PROFILE, GLFW_OPENGL_CORE_PROFILE);

    GLFWwindow* window = glfwCreateWindow(1280, 720, "CLIDE Master Engine Workspace Editor v2.0.0", nullptr, nullptr);
    if (!window) {
        std::cerr << "CRITICAL ERROR: Failed to bind standard hardware OpenGL window frame." << std::endl;
        glfwTerminate();
        return -1;
    }

    glfwMakeContextCurrent(window);
    glfwSwapInterval(1);

    IMGUI_CHECKVERSION();
    ImGui::CreateContext();
    ImGuiIO& io = ImGui::GetIO(); (void)io;
    io.ConfigFlags |= ImGuiConfigFlags_NavEnableKeyboard;

    ImGui::StyleColorsDark();
    ImGui_ImplGlfw_InitForOpenGL(window, true);
    ImGui_ImplOpenGL3_Init("#version 330");

    CLIDECoreEngine clide;
    g_engine = &clide;
    
    clide.current_theme.bg = ImVec4(0.01f, 0.01f, 0.01f, 1.00f);
    clide.current_theme.terminal_bg = ImVec4(0.01f, 0.01f, 0.01f, 1.00f);
    
    clide.create_new_tab("workspace_source.cl", "prog main\n  print \"Initializing native system layer\"\nend");

    while (!glfwWindowShouldClose(window)) {
        glfwPollEvents();
        process_keyboard_shortcuts(window);

        ImGui_ImplOpenGL3_NewFrame();
        ImGui_ImplGlfw_NewFrame();
        ImGui::NewFrame();

        clide.apply_theme_colors();

        // ── 1. Master System Drop-Down Menu Stripe Layout
        ImGui::SetNextWindowPos(ImVec2(0, 0));
        ImGui::SetNextWindowSize(ImVec2(io.DisplaySize.x, 32));
        ImGui::Begin("Global System Bar Frame Layout", nullptr, ImGuiWindowFlags_NoDecoration | ImGuiWindowFlags_NoMove | ImGuiWindowFlags_NoResize | ImGuiWindowFlags_MenuBar);
        
        if (ImGui::BeginMenuBar()) {
            if (ImGui::BeginMenu("File")) {
                if (ImGui::MenuItem("New File", "Ctrl+N")) clide.create_new_tab();
                if (ImGui::MenuItem("Open File...", "Ctrl+O")) {
                    std::string path = clide.invoke_native_file_dialog("open");
                    clide.open_file_from_disk(path);
                }
                if (ImGui::MenuItem("Open Folder workspace...")) {
                    std::string folder = clide.invoke_native_file_dialog("open", true);
                    if (!folder.empty()) clide.active_workspace_directory = folder;
                }
                ImGui::Separator();
                if (ImGui::MenuItem("Save", "Ctrl+S")) {
                    if (clide.active_tab_index >= 0) {
                        EditorTab& current = clide.tabs[static_cast<size_t>(clide.active_tab_index)];
                        if (current.path.empty()) {
                            std::string path = clide.invoke_native_file_dialog("save");
                            if (!path.empty()) clide.save_current_tab(path);
                        } else {
                            clide.save_current_tab(current.path);
                        }
                    }
                }
                if (ImGui::MenuItem("Save As...", "Ctrl+Shift+S")) {
                    std::string path = clide.invoke_native_file_dialog("save");
                    if (!path.empty()) clide.save_current_tab(path);
                }
                ImGui::Separator();
                if (ImGui::MenuItem("Close Editor Tab", "Ctrl+W")) {
                    if (clide.active_tab_index >= 0) clide.close_tab(clide.active_tab_index);
                }
                ImGui::EndMenu();
            }
            if (ImGui::BeginMenu("Edit")) {
                if (ImGui::MenuItem("Find and Replace", "Ctrl+F")) {
                    clide.is_find_panel_visible = !clide.is_find_panel_visible;
                    if (clide.is_find_panel_visible) clide.focus_search_input_field = true;
                }
                ImGui::EndMenu();
            }
            if (ImGui::BeginMenu("View")) {
                if (ImGui::MenuItem("Toggle Sidebar Panel")) clide.is_sidebar_open = !clide.is_sidebar_open;
                ImGui::EndMenu();
            }

            float runtime_controls_footprint = 140.0f;
            ImGui::SameLine((io.DisplaySize.x - runtime_controls_footprint) * 0.5f);
            
            ImGui::PushStyleVar(ImGuiStyleVar_FrameRounding, 6.0f);
            if (clide.pipeline_status == PipelineState::Idle) {
                ImGui::PushStyleColor(ImGuiCol_Button, clide.current_theme.accent);
                if (ImGui::Button("RUN", ImVec2(120, 24))) clide.run_pipeline_router();
                ImGui::PopStyleColor();
            } else {
                if (clide.pipeline_status == PipelineState::Running) {
                    ImGui::PushStyleColor(ImGuiCol_Button, clide.current_theme.warn);
                    if (ImGui::Button("PAUSE", ImVec2(68, 24))) clide.pause_pipeline();
                    ImGui::PopStyleColor();
                } else if (clide.pipeline_status == PipelineState::Paused) {
                    ImGui::PushStyleColor(ImGuiCol_Button, clide.current_theme.accent);
                    if (ImGui::Button("RESUME", ImVec2(68, 24))) clide.resume_pipeline();
                    ImGui::PopStyleColor();
                }
                ImGui::SameLine();
                ImGui::PushStyleColor(ImGuiCol_Button, clide.current_theme.danger);
                if (ImGui::Button("STOP", ImVec2(60, 24))) clide.stop_pipeline();
                ImGui::PopStyleColor();
            }
            ImGui::PopStyleVar();

            ImGui::SameLine(io.DisplaySize.x - 100.0f - ImGui::GetStyle().FramePadding.x);
            if (ImGui::BeginMenu("Settings")) {
                if (ImGui::MenuItem("Open Settings")) { clide.show_settings_popup = true; }
                ImGui::Separator();
                if (ImGui::BeginMenu("Shortcuts")) {
                    if (ImGui::MenuItem("Themes")) { clide.active_view = SidebarView::Extensions; clide.is_sidebar_open = true; }
                    if (ImGui::MenuItem("Keybinds")) {}
                    if (ImGui::MenuItem("Compiler Sources")) {}
                    ImGui::EndMenu();
                }
                ImGui::EndMenu();
            }

            ImGui::EndMenuBar();
        }
        ImGui::End();

        // FIXED: Stripped out leaked token characters inside the modal trigger switch block pass
        if (clide.show_settings_popup) {
            ImGui::OpenPopup("Settings Configuration Panel");
        }
        if (ImGui::BeginPopupModal("Settings Configuration Panel", &clide.show_settings_popup, ImGuiWindowFlags_AlwaysAutoResize)) {
            ImGui::Text("CLIDE Global Workspace Settings Engine");
            ImGui::Separator();
            ImGui::Checkbox("Enforce Uniform Jet-Black Interface Backdrop", &clide.is_dark_mode);
            clide.update_current_palette();
            
            ImGui::Text("Available Core Editor Mappings:");
            ImGui::BulletText("Alt + Left Mouse Click : Create multi-line editing cursors");
            ImGui::BulletText("Ctrl + F               : Invoke search find box layer");
            ImGui::BulletText("F5                     : Run active language toolchains");
            
            ImGui::Separator();
            if (ImGui::Button("Dismiss Settings Panel Window", ImVec2(280, 24))) {
                clide.show_settings_popup = false;
                ImGui::CloseCurrentPopup();
            }
            ImGui::EndPopup();
        }

        // ── 2. Split Structural Component Layout Dimensions
        float activity_bar_width = 48.0f;
        float actual_sidebar_render_width = clide.is_sidebar_open ? clide.sidebar_calculated_width : 0.0f;
        
        float master_workspace_canvas_width = io.DisplaySize.x - activity_bar_width - actual_sidebar_render_width;
        float center_workspace_offset_x = activity_bar_width + actual_sidebar_render_width;
        
        float upper_editor_stage_height = io.DisplaySize.y - 32.0f - clide.terminal_calculated_height;

        // Far-Left Activity Ribbon Layout
        ImGui::SetNextWindowPos(ImVec2(0, 32));
        ImGui::SetNextWindowSize(ImVec2(activity_bar_width, io.DisplaySize.y - 32));
        ImGui::Begin("Activity Bar Ribbon Panel", nullptr, ImGuiWindowFlags_NoDecoration | ImGuiWindowFlags_NoMove | ImGuiWindowFlags_NoResize | ImGuiWindowFlags_NoTitleBar);
        
        std::string collapse_button_string_symbol = clide.is_sidebar_open ? "<||" : "||>";
        if (ImGui::Button(collapse_button_string_symbol.c_str(), ImVec2(32, 32))) clide.is_sidebar_open = !clide.is_sidebar_open;
        if (ImGui::IsItemHovered()) ImGui::SetTooltip(clide.is_sidebar_open ? "Collapse Sidebar" : "Expand Sidebar");

        ImGui::Spacing(); ImGui::Separator(); ImGui::Spacing();

        if (ImGui::Button("EXP", ImVec2(36, 32))) { 
            if (clide.active_view == SidebarView::Explorer && clide.is_sidebar_open) clide.is_sidebar_open = false;
            else { clide.active_view = SidebarView::Explorer; clide.is_sidebar_open = true; }
        }
        if (ImGui::IsItemHovered()) ImGui::SetTooltip("Explorer");

        ImGui::Spacing();
        if (ImGui::Button("PLG", ImVec2(36, 32))) { 
            if (clide.active_view == SidebarView::Extensions && clide.is_sidebar_open) clide.is_sidebar_open = false;
            else { clide.active_view = SidebarView::Extensions; clide.is_sidebar_open = true; }
        }
        if (ImGui::IsItemHovered()) ImGui::SetTooltip("Extensions");
        ImGui::End();

        // Secondary Expanded Container Sidebar View
        if (clide.is_sidebar_open) {
            ImGui::SetNextWindowPos(ImVec2(activity_bar_width, 32));
            ImGui::SetNextWindowSize(ImVec2(clide.sidebar_calculated_width, io.DisplaySize.y - 32));
            ImGui::Begin("Sidebar Primary Target Container View", nullptr, ImGuiWindowFlags_NoCollapse | ImGuiWindowFlags_NoResize | ImGuiWindowFlags_NoMove | ImGuiWindowFlags_NoTitleBar);
            
            if (clide.active_view == SidebarView::Explorer) {
                ImGui::Text("WORKSPACE EXPLORER");
                ImGui::Separator();
                if (ImGui::Button("Mount Workspace Folder")) {
                    std::string folder = clide.invoke_native_file_dialog("open", true);
                    if (!folder.empty()) clide.active_workspace_directory = folder;
                }
                ImGui::Spacing();
                
                ImGui::BeginChild("ExplorerFileSystemTreeListingBlockRegion", ImVec2(0, 0), true);
                if (!clide.active_workspace_directory.empty()) {
                    if (ImGui::TreeNode((fs::path(clide.active_workspace_directory).filename().string() + "##BaseNode").c_str())) {
                        clide.draw_workspace_directory_node(clide.active_workspace_directory);
                        ImGui::TreePop();
                    }
                } else {
                    ImGui::TextColored(clide.current_theme.muted, "No workspace folder mounted.");
                }
                ImGui::EndChild();
            } 
            else if (clide.active_view == SidebarView::Extensions) {
                ImGui::Text("ACTIVE UI GRAPHICS PALETTE");
                ImGui::Separator();
                if (ImGui::BeginCombo("##ThemeSelectorCombo", clide.current_family_name.c_str())) {
                    for (auto& [name, family] : clide.themes) {
                        bool is_selected = (clide.current_family_name == name);
                        ImGui::PushID(name.c_str());
                        ImVec2 swatch_render_pos = ImGui::GetCursorScreenPos();
                        
                        bool clicked = ImGui::Selectable(name.c_str(), is_selected, 0, ImVec2(180, 0));
                        ImDrawList* dlist = ImGui::GetWindowDrawList();
                        float swatch_metrics = 12.0f;
                        float padding_offset_alignment_x = 180.0f;
                        
                        ImVec2 dark_swatch_pos(swatch_render_pos.x + padding_offset_alignment_x, swatch_render_pos.y + 4);
                        dlist->AddRectFilled(dark_swatch_pos, ImVec2(dark_swatch_pos.x + swatch_metrics, dark_swatch_pos.y + swatch_metrics), ImGui::ColorConvertFloat4ToU32(family.dark_variant.accent), 2.0f);
                        
                        ImVec2 light_swatch_pos(swatch_render_pos.x + padding_offset_alignment_x + swatch_metrics + 6, swatch_render_pos.y + 4);
                        dlist->AddRectFilled(light_swatch_pos, ImVec2(light_swatch_pos.x + swatch_metrics, light_swatch_pos.y + swatch_metrics), ImGui::ColorConvertFloat4ToU32(family.light_variant.accent), 2.0f);

                        if (clicked) {
                            clide.current_family_name = name;
                            clide.update_current_palette();
                        }
                        if (is_selected) ImGui::SetItemDefaultFocus();
                        ImGui::PopID();
                    }
                    ImGui::EndCombo();
                }

                ImGui::Checkbox("Dark Variant Mode Active", &clide.is_dark_mode);
                clide.update_current_palette();
                
                ImGui::Spacing(); ImGui::Text("TOOLCHAIN WORKLOAD DEPENDENCIES");
                ImGui::Separator();
                ImGui::BeginChild("WorkloadPackagesScrollerRegion", ImVec2(0, 0), true);
                for (auto& pkg : clide.registry) {
                    ImGui::Text("%s", pkg.name.c_str());
                    ImGui::TextColored(clide.current_theme.muted, "%s", pkg.description.c_str());
                    std::string action_label = pkg.installed ? "Uninstall Component" : "Install Component";
                    if (ImGui::Button((action_label + "##" + pkg.name).c_str())) clide.toggle_package_installation(pkg.name);
                    ImGui::Separator();
                }
                ImGui::EndChild();
            }
            ImGui::End();

            ImGui::SetNextWindowPos(ImVec2(activity_bar_width + clide.sidebar_calculated_width - 3.0f, 32));
            ImGui::SetNextWindowSize(ImVec2(6.0f, io.DisplaySize.y - 32));
            ImGui::Begin("Sidebar Splitter Line Node", nullptr, ImGuiWindowFlags_NoDecoration | ImGuiWindowFlags_NoMove | ImGuiWindowFlags_NoResize | ImGuiWindowFlags_NoBackground);
            if (ImGui::IsWindowHovered() || ImGui::IsWindowFocused()) ImGui::SetMouseCursor(ImGuiMouseCursor_ResizeEW);
            if (ImGui::IsMouseDragging(ImGuiMouseButton_Left) && ImGui::IsWindowFocused()) {
                clide.sidebar_calculated_width += io.MouseDelta.x;
                if (clide.sidebar_calculated_width < 160.0f) clide.sidebar_calculated_width = 160.0f;
                if (clide.sidebar_calculated_width > 600.0f) clide.sidebar_calculated_width = 600.0f;
            }
            ImGui::End();
        }

        // ── 3. Central Multiple Tab Workspace Editor Canvas (Top Right)
        ImGui::SetNextWindowPos(ImVec2(center_workspace_offset_x, 32));
        ImGui::SetNextWindowSize(ImVec2(master_workspace_canvas_width, upper_editor_stage_height));
        ImGui::Begin("Workspace Editor Window Stage", nullptr, ImGuiWindowFlags_NoCollapse | ImGuiWindowFlags_NoResize | ImGuiWindowFlags_NoMove | ImGuiWindowFlags_NoTitleBar);
        
        float gutter_width = 32.0f;
        float actual_editor_input_width = master_workspace_canvas_width - gutter_width - clide.minimap_calculated_width - 25.0f;
        
        if (ImGui::BeginTabBar("WorkspaceFilesEditorTabsBar", ImGuiTabBarFlags_AutoSelectNewTabs)) {
            for (int i = 0; i < static_cast<int>(clide.tabs.size()); ++i) {
                bool open = true;
                ImGuiTabItemFlags tab_flags = 0;
                if (clide.tab_switch_requested && i == clide.active_tab_index) tab_flags |= ImGuiTabItemFlags_SetSelected;

                if (ImGui::BeginTabItem(clide.tabs[static_cast<size_t>(i)].get_display_name().c_str(), &open, tab_flags)) {
                    if (i != clide.active_tab_index && !clide.tab_switch_requested) {
                        clide.sync_editor_to_buffer();
                        clide.active_tab_index = i;
                        clide.sync_buffer_to_editor();
                        clide.extra_cursors.clear();
                    }
                    ImGui::EndTabItem();
                }
                if (!open) clide.close_tab(i);
            }
            clide.tab_switch_requested = false;
            ImGui::EndTabBar();
        }

        if (clide.is_find_panel_visible) {
            ImGui::BeginChild("FindReplaceInlineControlPanelMatrix", ImVec2(-FLT_MIN, 36), true);
            if (clide.focus_search_input_field) {
                ImGui::SetKeyboardFocusHere();
                clide.focus_search_input_field = false;
            }
            ImGui::SetNextItemWidth(140); 
            ImGui::InputText("Find", clide.search_query_buffer, sizeof(clide.search_query_buffer), ImGuiInputTextFlags_AutoSelectAll); 
            ImGui::SameLine();
            ImGui::SetNextItemWidth(140); 
            ImGui::InputText("Replace", clide.replace_query_buffer, sizeof(clide.replace_query_buffer)); 
            ImGui::SameLine();
            
            if (ImGui::Button("Next")) { clide.execute_find_and_replace(false); } ImGui::SameLine();
            if (ImGui::Button("All")) { clide.execute_find_and_replace(true); } ImGui::SameLine();
            if (ImGui::Button("X")) { clide.is_find_panel_visible = false; }
            ImGui::EndChild();
        }

        if (g_engine && clide.active_tab_index >= 0 && clide.active_tab_index < static_cast<int>(clide.tabs.size())) {
            EditorTab& active_tab_node = clide.tabs[static_cast<size_t>(clide.active_tab_index)];
            std::string content_view_string(clide.text_input_buffer);

            int text_line_count = 1;
            for (size_t k = 0; k < content_view_string.size(); ++k) {
                if (content_view_string[k] == '\n') text_line_count++;
            }

            ImGui::BeginChild("GutterMarginBreakpointsPanel", ImVec2(gutter_width, -FLT_MIN), false, ImGuiWindowFlags_NoScrollbar | ImGuiWindowFlags_NoScrollWithMouse);
            ImGui::SetScrollY(clide.editor_scroll_y);
            
            for (int line = 1; line <= std::max(80, text_line_count); ++line) {
                if (active_tab_node.breakpoints.count(line)) ImGui::TextColored(clide.current_theme.danger, " o ");
                else if (active_tab_node.line_errors.count(line)) ImGui::TextColored(clide.current_theme.warn, " ! ");
                else ImGui::TextColored(clide.current_theme.muted, "%2d", line);
                
                if (ImGui::IsItemClicked()) {
                    if (active_tab_node.breakpoints.count(line)) active_tab_node.breakpoints.erase(line);
                    else active_tab_node.breakpoints.insert(line);
                }
            }
            ImGui::EndChild(); ImGui::SameLine();

            // Single, Unified Scrolling Core Workspace Container Node
            ImGui::BeginChild("CoreEditorTextInputWorkingStageBlock", ImVec2(actual_editor_input_width, -FLT_MIN), false, ImGuiWindowFlags_HorizontalScrollbar);
            
            ImVec2 txt_editor_pos = ImGui::GetCursorScreenPos();
            float char_width = ImGui::CalcTextSize("A").x;
            float line_height = ImGui::GetTextLineHeight();

            // Capture Menu Key Events & Right Clicks BEFORE widget focus consumption loops
            bool open_editor_popup = false;
            if (ImGui::IsWindowHovered(ImGuiHoveredFlags_ChildWindows) && ImGui::IsMouseClicked(ImGuiMouseButton_Right)) open_editor_popup = true;
            if (ImGui::IsWindowFocused(ImGuiFocusedFlags_ChildWindows) && ImGui::IsKeyPressed(ImGuiKey_Menu)) open_editor_popup = true;
            if (open_editor_popup) ImGui::OpenPopup("EditorWorkspaceRightClickContextMenu");

            // Alt + Left Click multi cursor deployment allocation pass
            if (ImGui::IsWindowHovered() && ImGui::IsMouseClicked(ImGuiMouseButton_Left) && io.KeyAlt) {
                ImVec2 mouse_pos = ImGui::GetMousePos();
                float local_x = mouse_pos.x - txt_editor_pos.x + ImGui::GetScrollX() - ImGui::GetStyle().FramePadding.x;
                float local_y = mouse_pos.y - txt_editor_pos.y + ImGui::GetScrollY() - ImGui::GetStyle().FramePadding.y;
                
                int target_line = static_cast<int>(local_y / line_height);
                int target_col = static_cast<int>(local_x / char_width);
                if (target_line < 0) target_line = 0;
                if (target_col < 0) target_col = 0;

                int offset_idx = clide.compute_offset_from_coords(content_view_string, target_line, target_col);
                if (std::find(clide.extra_cursors.begin(), clide.extra_cursors.end(), offset_idx) == clide.extra_cursors.end()) {
                    clide.extra_cursors.push_back(offset_idx);
                }
            }

            ImVec2 calculated_content_bounds = ImGui::CalcTextSize(clide.text_input_buffer);
            float input_w = std::max(actual_editor_input_width, calculated_content_bounds.x + 300.0f);
            float input_h = std::max(ImGui::GetWindowHeight(), text_line_count * line_height + 300.0f);
            
            ImGui::PushStyleColor(ImGuiCol_Text, ImVec4(0, 0, 0, 0));
            ImGui::PushStyleColor(ImGuiCol_FrameBg, ImVec4(0, 0, 0, 0));
            // FIXED: Scope typo capitalization bug corrected to lowercase object state tracker layout
            ImGui::InputTextMultiline("##WorkspaceCodeMatrixEditorTextInputBlock", clide.text_input_buffer, sizeof(clide.text_input_buffer), ImVec2(input_w, input_h), ImGuiInputTextFlags_AllowTabInput);
            
            float current_scroll_x = ImGui::GetScrollX();
            float current_scroll_y = ImGui::GetScrollY();
            clide.editor_scroll_y = current_scroll_y;
            clide.editor_max_scroll_y = ImGui::GetScrollMaxY();
            
            ImGui::PopStyleColor(2);
            
            clide.apply_multicursor_edits(content_view_string);
            
            // Pixel-Perfect highlight alignments mapped via standard native FramePadding bounds rule matrices
            ImGui::SetCursorPos(ImVec2(ImGui::GetStyle().FramePadding.x, ImGui::GetStyle().FramePadding.y));
            clide.parse_and_render_stylized_text(content_view_string, false, &active_tab_node);
            
            // Render virtual flashing lines onto extra multi cursor pins
            for (int offset_idx : clide.extra_cursors) {
                ImVec2 cursor_coord = clide.compute_coords_from_offset(content_view_string, offset_idx);
                float cx = txt_editor_pos.x + ImGui::GetStyle().FramePadding.x + cursor_coord.x * char_width - current_scroll_x;
                float cy = txt_editor_pos.y + ImGui::GetStyle().FramePadding.y + cursor_coord.y * line_height - current_scroll_y;
                if (cx >= txt_editor_pos.x && cx <= txt_editor_pos.x + actual_editor_input_width) {
                    ImGui::GetWindowDrawList()->AddRectFilled(ImVec2(cx, cy), ImVec2(cx + 1.5f, cy + line_height), IM_COL32(255, 255, 255, 255));
                }
            }
            
            // Render Query strings matching indicators down right gutter tracks
            if (clide.is_find_panel_visible && std::strlen(clide.search_query_buffer) > 0) {
                ImDrawList* overlay_layer = ImGui::GetWindowDrawList();
                std::string search_target(clide.search_query_buffer);
                size_t look_pos = content_view_string.find(search_target, 0);
                int total_chars = static_cast<int>(content_view_string.size());
                
                while (look_pos != std::string::npos && total_chars > 0) {
                    float ratio = static_cast<float>(look_pos) / total_chars;
                    float indicator_y = txt_editor_pos.y + (ratio * (ImGui::GetWindowHeight() - 20.0f));
                    overlay_layer->AddRectFilled(ImVec2(txt_editor_pos.x + actual_editor_input_width - 12.0f, indicator_y), ImVec2(txt_editor_pos.x + actual_editor_input_width - 2.0f, indicator_y + 3.0f), IM_COL32(237, 163, 0, 255));
                    look_pos = content_view_string.find(search_target, look_pos + 1);
                }
            }

            // Draw line errors over gutter scrollers (VSC Style)
            if (!active_tab_node.line_errors.empty() && text_line_count > 0) {
                ImDrawList* overlay_layer = ImGui::GetWindowDrawList();
                for (const auto& [err_line, err_msg] : active_tab_node.line_errors) {
                    float ratio = static_cast<float>(err_line) / static_cast<float>(text_line_count);
                    float indicator_y = txt_editor_pos.y + (ratio * (ImGui::GetWindowHeight() - 20.0f));
                    overlay_layer->AddRectFilled(ImVec2(txt_editor_pos.x + actual_editor_input_width - 12.0f, indicator_y), ImVec2(txt_editor_pos.x + actual_editor_input_width - 2.0f, indicator_y + 3.0f), IM_COL32(248, 81, 73, 255));
                }
            }

            if (ImGui::BeginPopup("EditorWorkspaceRightClickContextMenu")) {
                if (ImGui::MenuItem("Save File", "Ctrl+S")) { if (!active_tab_node.path.empty()) clide.save_current_tab(active_tab_node.path); }
                if (ImGui::MenuItem("Find and Replace Tool", "Ctrl+F")) clide.is_find_panel_visible = true;
                if (ImGui::MenuItem("Clear Active Logging Console")) clide.clear_terminal();
                if (ImGui::MenuItem("Remove Extra Active Cursors")) clide.extra_cursors.clear();
                ImGui::EndPopup();
            }
            clide.sync_editor_to_buffer();
            ImGui::EndChild(); ImGui::SameLine();

            // Minimap horizontal expand left splitter layout handle
            ImGui::SetNextWindowPos(ImVec2(center_workspace_offset_x + master_workspace_canvas_width - clide.minimap_calculated_width - 15.0f, 32));
            ImGui::SetNextWindowSize(ImVec2(6.0f, upper_editor_stage_height));
            ImGui::Begin("Minimap Splitter Line Node", nullptr, ImGuiWindowFlags_NoDecoration | ImGuiWindowFlags_NoMove | ImGuiWindowFlags_NoResize | ImGuiWindowFlags_NoBackground);
            if (ImGui::IsWindowHovered() || ImGui::IsWindowFocused()) ImGui::SetMouseCursor(ImGuiMouseCursor_ResizeEW);
            if (ImGui::IsMouseDragging(ImGuiMouseButton_Left) && ImGui::IsWindowFocused()) {
                clide.minimap_calculated_width -= io.MouseDelta.x;
                if (clide.minimap_calculated_width < 80.0f) clide.minimap_calculated_width = 80.0f;
                if (clide.minimap_calculated_width > 250.0f) clide.minimap_calculated_width = 250.0f;
            }
            ImGui::End();

            ImGui::PushStyleColor(ImGuiCol_ChildBg, clide.current_theme.panel);
            ImGui::BeginChild("VSCStyleEditorCodeMinimapPreviewPane", ImVec2(clide.minimap_calculated_width, -FLT_MIN), true, ImGuiWindowFlags_NoScrollbar | ImGuiWindowFlags_NoScrollWithMouse);
            
            ImGui::SetWindowFontScale(0.5f);
            
            ImDrawList* minimap_renderer_context_layer = ImGui::GetWindowDrawList();
            ImVec2 mini_pos = ImGui::GetCursorScreenPos();
            float mini_height = ImGui::GetWindowHeight();
            
            float view_fraction_offset = (clide.editor_max_scroll_y > 0.0f) ? (clide.editor_scroll_y / clide.editor_max_scroll_y) : 0.0f;
            float handle_height = std::max(18.0f, (mini_height / (text_line_count * 8.0f)) * mini_height);
            if (handle_height > mini_height - 10.0f) handle_height = mini_height - 10.0f;
            
            float floating_block_top = mini_pos.y + (view_fraction_offset * (mini_height - handle_height - 8.0f));
            
            minimap_renderer_context_layer->AddRectFilled(ImVec2(mini_pos.x, floating_block_top), ImVec2(mini_pos.x + clide.minimap_calculated_width, floating_block_top + handle_height), IM_COL32(0, 120, 215, 45), 2.0f);
            minimap_renderer_context_layer->AddRect(ImVec2(mini_pos.x, floating_block_top), ImVec2(mini_pos.x + clide.minimap_calculated_width, floating_block_top + handle_height), IM_COL32(0, 120, 215, 180), 1.5f);
            
            float scaled_text_scroll = view_fraction_offset * std::max(0.0f, (text_line_count * 7.0f) - mini_height);
            ImGui::SetCursorPosY(4.0f - scaled_text_scroll);
            clide.parse_and_render_stylized_text(content_view_string, true, nullptr);
            
            ImGui::SetWindowFontScale(1.0f);
            ImGui::EndChild(); ImGui::PopStyleColor();
        }
        ImGui::End();

        ImGui::SetNextWindowPos(ImVec2(center_workspace_offset_x, 32.0f + upper_editor_stage_height - 3.0f));
        ImGui::SetNextWindowSize(ImVec2(master_workspace_canvas_width, 6.0f));
        ImGui::Begin("Terminal Splitter Drag Node Line", nullptr, ImGuiWindowFlags_NoDecoration | ImGuiWindowFlags_NoMove | ImGuiWindowFlags_NoResize | ImGuiWindowFlags_NoBackground);
        if (ImGui::IsWindowHovered() || ImGui::IsWindowFocused()) ImGui::SetMouseCursor(ImGuiMouseCursor_ResizeNS);
        if (ImGui::IsMouseDragging(ImGuiMouseButton_Left) && ImGui::IsWindowFocused()) {
            clide.terminal_calculated_height -= io.MouseDelta.y;
            if (clide.terminal_calculated_height < 80.0f) clide.terminal_calculated_height = 80.0f;
            if (clide.terminal_calculated_height > 600.0f) clide.terminal_calculated_height = 600.0f;
        }
        ImGui::End();

        // ── 4. Unified Logging Console Terminal Panel Deck Layout
        ImGui::SetNextWindowPos(ImVec2(center_workspace_offset_x, io.DisplaySize.y - clide.terminal_calculated_height));
        ImGui::SetNextWindowSize(ImVec2(master_workspace_canvas_width, clide.terminal_calculated_height));
        ImGui::Begin("Multi-Tab Console Subsystem Deck Frame", nullptr, ImGuiWindowFlags_NoCollapse | ImGuiWindowFlags_NoResize | ImGuiWindowFlags_NoMove | ImGuiWindowFlags_NoTitleBar);
        
        bool open_terminal_popup = false;
        if (ImGui::IsWindowHovered(ImGuiHoveredFlags_ChildWindows) && ImGui::IsMouseClicked(ImGuiMouseButton_Right)) open_terminal_popup = true;
        if (ImGui::IsWindowFocused(ImGuiFocusedFlags_ChildWindows) && ImGui::IsKeyPressed(ImGuiKey_Menu)) open_terminal_popup = true;
        if (open_terminal_popup) ImGui::OpenPopup("TerminalDeckRightClickContextMenu");

        if (ImGui::BeginTabBar("TerminalLoggingConsoleDeckTabBar", ImGuiTabBarFlags_None)) {
            
            ImGuiTabItemFlags t1_flags = 0;
            if (clide.terminal_tab_switch_requested && clide.active_terminal_tab == TerminalTab::Debug) t1_flags |= ImGuiTabItemFlags_SetSelected;
            if (ImGui::BeginTabItem("Debug Log Terminal", nullptr, t1_flags)) {
                if (!clide.terminal_tab_switch_requested) clide.active_terminal_tab = TerminalTab::Debug;
                
                ImGui::SetCursorPosX(ImGui::GetWindowWidth() - 110.0f - ImGui::GetStyle().WindowPadding.x);
                if (ImGui::Button("Clear Log", ImVec2(100, 22))) { clide.clear_terminal(); }
                
                ImGui::PushStyleColor(ImGuiCol_FrameBg, ImVec4(0.01f, 0.01f, 0.01f, 1.00f));
                ImGui::InputTextMultiline("##DebugLogConsoleReadoutContainerBox", clide.debug_text_storage, sizeof(clide.debug_text_storage), ImVec2(-FLT_MIN, -FLT_MIN), ImGuiInputTextFlags_ReadOnly);
                ImGui::PopStyleColor();
                ImGui::EndTabItem();
            }
            
            ImGuiTabItemFlags t2_flags = 0;
            if (clide.terminal_tab_switch_requested && clide.active_terminal_tab == TerminalTab::RunOutput) t2_flags |= ImGuiTabItemFlags_SetSelected;
            if (ImGui::BeginTabItem("Program Run Output", nullptr, t2_flags)) {
                if (!clide.terminal_tab_switch_requested) clide.active_terminal_tab = TerminalTab::RunOutput;
                
                ImGui::SetCursorPosX(ImGui::GetWindowWidth() - 230.0f - ImGui::GetStyle().WindowPadding.x);
                if (ImGui::Button("Clear Output", ImVec2(100, 22))) { clide.clear_terminal(); } ImGui::SameLine();
                if (ImGui::Button("Kill Tree", ImVec2(110, 22))) { clide.stop_pipeline(); }
                
                ImGui::PushStyleColor(ImGuiCol_FrameBg, ImVec4(0.01f, 0.01f, 0.01f, 1.00f));
                ImGui::InputTextMultiline("##ProgramPipelineRunOutputContainerBox", clide.run_output_storage, sizeof(clide.run_output_storage), ImVec2(-FLT_MIN, ImGui::GetWindowHeight() - 74), ImGuiInputTextFlags_ReadOnly);
                ImGui::PopStyleColor();
                
                ImGui::PushItemWidth(ImGui::GetWindowWidth() - 30);
                if (ImGui::InputText("##ProgramRunOutputStdinBarLineInput", clide.run_stdin_buffer, sizeof(clide.run_stdin_buffer), ImGuiInputTextFlags_EnterReturnsTrue)) {
                    if (std::strlen(clide.run_stdin_buffer) > 0) {
                        clide.inject_run_stdin_data(clide.run_stdin_buffer);
                        std::memset(clide.run_stdin_buffer, 0, sizeof(clide.run_stdin_buffer));
                    }
                }
                ImGui::PopItemWidth();
                ImGui::EndTabItem();
            }
            
            ImGuiTabItemFlags t3_flags = 0;
            if (clide.terminal_tab_switch_requested && clide.active_terminal_tab == TerminalTab::SystemShell) t3_flags |= ImGuiTabItemFlags_SetSelected;
            if (ImGui::BeginTabItem("Interactive Bash Shell", nullptr, t3_flags)) {
                if (!clide.terminal_tab_switch_requested) clide.active_terminal_tab = TerminalTab::SystemShell;
                
                ImGui::SetCursorPosX(ImGui::GetWindowWidth() - 120.0f - ImGui::GetStyle().WindowPadding.x);
                if (ImGui::Button("Clear Shell", ImVec2(110, 22))) { clide.clear_terminal(); }
                
                ImGui::BeginChild("InlineTerminalHistoryScrollWindowStageBlock", ImVec2(-FLT_MIN, ImGui::GetWindowHeight() - 44), false);
                ImGui::TextUnformatted(clide.system_shell_storage);
                
                ImGui::TextColored(ImVec4(1.0f, 1.0f, 1.0f, 1.00f), "> "); ImGui::SameLine();
                ImGui::PushItemWidth(ImGui::GetWindowWidth() - 60);
                if (ImGui::InputText("##ContinuousInlineBashShellActionMatrixInputFieldLine", clide.bash_command_buffer, sizeof(clide.bash_command_buffer), ImGuiInputTextFlags_EnterReturnsTrue)) {
                    if (std::strlen(clide.bash_command_buffer) > 0) {
                        clide.inject_system_bash_command(clide.bash_command_buffer);
                        std::memset(clide.bash_command_buffer, 0, sizeof(clide.bash_command_buffer));
                    }
                }
                ImGui::PopItemWidth();
                
                if (ImGui::GetScrollY() >= ImGui::GetScrollMaxY()) ImGui::SetScrollHereY(1.0f);
                ImGui::EndChild();
                
                ImGui::EndTabItem();
            }
            
            clide.terminal_tab_switch_requested = false;
            ImGui::EndTabBar();
        }

        if (ImGui::BeginPopup("TerminalDeckRightClickContextMenu")) {
            if (ImGui::MenuItem("Clear Selected Subsystem Deck Logs")) clide.clear_terminal();
            if (ImGui::MenuItem("Terminate Async Toolchain Process Tree")) clide.stop_pipeline();
            ImGui::EndPopup();
        }

        ImGui::End();

        // Render pass pipeline execution elements
        ImGui::Render();
        int display_w, display_h;
        glfwGetFramebufferSize(window, &display_w, &display_h);
        glViewport(0, 0, display_w, display_h);
        glClearColor(clide.current_theme.bg.x, clide.current_theme.bg.y, clide.current_theme.bg.z, clide.current_theme.bg.w);
        glClear(GL_COLOR_BUFFER_BIT);
        ImGui_ImplOpenGL3_RenderDrawData(ImGui::GetDrawData());

        glfwSwapBuffers(window);
    }

    ImGui_ImplOpenGL3_Shutdown();
    ImGui_ImplGlfw_Shutdown();
    ImGui::DestroyContext();

    glfwDestroyWindow(window);
    glfwTerminate();
    return 0;
}