
#include <stdio.h>

int main(){
  char filename[] = "../input";
  FILE *fp = fopen(filename,"r");
  if (!fp){
    fprintf(stderr,"cannot open [%s] ", filename);
    return 1;    
  }
  while(!feof(fp)){
    int ch = fgetc(fp);
    if (ch == '\n' || ch < 0){
      printf("\n");
    }
    else {
      printf("%d ",ch);
    }
  }  
  fclose(fp);
  return 0;
}


