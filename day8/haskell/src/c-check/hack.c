

#include <stdio.h>


int main(int argc , char *args[] ){
  char *filename = args[1];
  printf("reading file [%s] \n" , filename);
  printf("got [character] : TOT_C : TOT_M : expect mem > char count \n");
  
  FILE *fp = fopen(filename,"r");
  if (!fp){
    fprintf(stderr,"cannot open [%s] ", filename);
    return 1;    
  }

  int tot_c = 0;
  int tot_m = 0;
  int loc_c = 0;
  int loc_m = 0;
  
  while(!feof(fp)){
    int ch = fgetc(fp);

    if (ch == '\n'){
      // ignore
    }
    else {
      //  \\
      //  \xAB
      //  \"
      if (ch == '\\'){
	int ch2 = fgetc(fp);
	if (ch2 == '\\' || ch2 == '"'){
	  tot_c += 1;
	  tot_m += 2;
	  printf("got [%c] [%c] : %d : %d \n",ch , ch2 , tot_c , tot_m);
	  continue;
	}
	else if (ch2 == 'x'){
	  int ch3 = fgetc(fp);
	  int ch4 = fgetc(fp);
	  tot_c += 1;
	  tot_m += 4;
	  printf("got [%c] [%c] [%c] [%c] : %d : %d \n",ch , ch2 , ch3 , ch4 , tot_c , tot_m);
	  continue;
	}
      }
      else {
	tot_c += 1;
	tot_m += 1;
	printf("got [%c] : %d : %d \n",ch , tot_c , tot_m);
	continue;
      }      
    }    
  }

  printf("\nsolution via C code says char %d and mem %d  -> %d \n" , tot_c , tot_m , tot_m - tot_c);
  
  fclose(fp);
  return 0;
}


