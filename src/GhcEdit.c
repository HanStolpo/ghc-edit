// StartEnd.c
#include <Rts.h>

void GhcEditStart()
{
   int argc = 1;
   char* argv[] = {"ghc-edit-dll", NULL}; // argv must end with NULL

   // Initialize Haskell runtime
   char** args = argv;
   hs_init(&argc, &args);
}

void GhcEditEnd()
{
   hs_exit();
}
