.global _reset

_reset:
    ldr sp, =stack_top
    bl c_entry
    b .
