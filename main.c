#include <stdio.h>
#include <stdlib.h>
#include "CStruct_stub.h"
#include "foo.h"

int main(int argc, char *argv[]) {  
  hs_init(&argc, &argv);
  
  foo *f;
  f = malloc(sizeof(foo));
  f->a = "Hello";
  f->b = "World";
  f->c = 55555; 
  
  printf("foo has been set in C:\n  a: %s\n  b: %s\n  c: %d\n",f->a,f->b,f->c);
  setFoo(f);
  printf("foo has been set in Haskell:\n  a: %s\n  b: %s\n  c: %d\n",f->a,f->b,f->c);
  
  attribute *a;
  a = malloc(sizeof(attribute));
  a->name = "width";
  a->val  = "12";
  
  printf("a has been set in C:\n  name: %s\n  val: %s\n",a->name,a->val);
  setAttribute(a);
  printf("a has been set in Haskell:\n  name: %s\n  val: %s\n",a->name,a->val);
  
  free_HaskellPtr(f->a);
  free_HaskellPtr(f->b);
  free(f);
  
  free_HaskellPtr(a->name);
  free_HaskellPtr(a->val);
  free(a);
  
  hs_exit();
}

/*
  Compile files
  $ hsc2hs HsFoo.hsc
  $ ghc -c HsFoo.hs foo.c
  $ ghc -no-hs-main foo.c HsFoo.o main.c -o main
  $ ./main
    move main.c to dist build
    move foo.h to dist build
    move CStruct_stub.h to dist.build
  $ ghc -no-hs-main Kiosk/Backend/Form/Rendering/CStruct.o cbits/foo.o main.c -o main
*/