from enum import Enum

# Structure:
# ----
# ELF header -> indentify as an elf type + specify the arch
#   * 52/64 bytes long for 32/64 bit binaries
#   * https://en.wikipedia.org/wiki/Executable_and_Linkable_Format#File_header
# Program header table -> execution information
# ----
# Code -> executable information
# Data -> information used by the code
# Section names
# ----
# Section header table -> linking (connecting program objects) information
# ----
