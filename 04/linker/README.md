## Linker Implementation

A linker converts object files (`.o` & `.a`) into executables and shared libraries.

1. Parse all the objects and static libraries and put their symbols into cache. Symbols are named addresses of functions and global variables.
2. Search for all unresolved symbol references in the `.o` files and match it up with a symbol from cache, recursively doing this for any code in `.a` referenced during this process. This forms a dependency graph between sections. This step is called symbol resolution.
3. Throw out any code that is not referenced by the input files by tracing the dependency graph from the entry-point symbol (e.g., _start on Linux). This step is called garbage collection.
4. Execute the linker script to figure out how to stitch the final binary together. This includes discovering the offsets at which everything will go.
5. Resolve relocations, “holes” in the binary that require knowing the final runtime address of the section. Relocations are instructions placed in the object file for the linker to execute.
6. Write out the executable.

## Sources
- [Linker Essay](https://lwn.net/Articles/276782/)
