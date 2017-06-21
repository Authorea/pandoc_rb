#include <stdio.h>
#include "HsFFI.h"
#include "PandocRb_stub.h"

int main(int argc, char *argv[])
{
  int i;

  hs_init(&argc, &argv);

  // for (i = 0; i < 5; i++) {
  //   printf("%d\n", foo(2500));
  // }

  // if(foo(5) && bar(5)){
  //   hs_exit();
  //   return 0;
  // } else {
  //   hs_exit();
  //   return 1;
  // }

  hs_exit();
  return 0;
}

