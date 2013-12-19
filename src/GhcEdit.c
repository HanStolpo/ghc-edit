// StartEnd.c
#include <Rts.h>

int GhcEditStart()
{
   int argc = 1;
   char* argv[] = {"ghc-edit-dll", NULL}; // argv must end with NULL

   // Initialize Haskell runtime
   char** args = argv;
   hs_init(&argc, &args);
   return 1;
}

int GhcEditEnd()
{
   hs_exit();
   return 1;
}
