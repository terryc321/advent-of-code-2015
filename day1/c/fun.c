
#include <stdio.h>

int main(int argc,char **argv){
  FILE *fp = fopen("../input","r");
  int level = 0;
  while(!feof(fp)){
    int ch = fgetc(fp);    
    if (ch == '('){
      level ++;
    }
    else if (ch ==')'){
      level--;
    }    
  }
  printf("level is %d \n",level);
  fclose(fp);

  fp = fopen("../input","r");
  level = 0;
  int position = 1;
  while(!feof(fp)){
    int ch = fgetc(fp);    
    if (ch == '('){
      level ++;
      position ++;
    }
    else if (ch ==')'){
      level--;
      if (level < 0){
        printf("postion when level < 0 is %d \n",position);        
        break;
      }
      position ++;
    }    
  }

  fclose(fp);

  return 0;
}


