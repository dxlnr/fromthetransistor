volatile unsigned int * const UART0DR = (unsigned int *)0x101f1000;
 
void print_uart0(const char *s) {
  while(*s != '\0') {
    *UART0DR = (unsigned int)(*s); /* Transmit char */
    s++;
  }
}
 
void c_entry() {
  print_uart0("\nBOOT PROCESS\n\nmoloch, moloch!\n");
}
