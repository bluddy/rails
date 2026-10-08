/* Runs before main() and before OCaml module initialization.
   Sets LIBSDL2_PATH to the executable's directory so tsdl-mixer
   can find SDL2_mixer.dll on Windows. */
#ifdef _WIN32
#include <windows.h>
#include <stdlib.h>

__attribute__((constructor))
static void preload_sdl_mixer(void) {
    char exe_path[MAX_PATH];
    char *last_slash;
    GetModuleFileNameA(NULL, exe_path, MAX_PATH);
    last_slash = strrchr(exe_path, '\\');
    if (last_slash) {
        *last_slash = '\0';
        SetEnvironmentVariableA("LIBSDL2_PATH", exe_path);
    }
}
#endif
