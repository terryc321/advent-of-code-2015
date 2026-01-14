/* dart - fun.dart 
we need a main
dart does not have a repl ? oh poor boy...

*/

import 'dart:io';

var str;

int height() {
  int i ;
  int floor = 0;
  for (i =0 ; i < str.length ; i ++){
    if (str[i] == '('){
      floor ++;
    }
    else if (str[i] ==')' ){
      floor --;
    }
  }  
  return floor;
}

int basement() {
  int i ;
  int ch = 0;
  int floor = 0;
  for (i =0 ; i < str.length ; i ++){
    if (str[i] == '('){
      floor ++;
      ch ++;
    }
    else if (str[i] ==')' ){
      floor --;
      ch ++;
      if (floor < 0){
        return ch;
      }
    }
  }
  //not found
  return -1;
}

void main() async {
  try {
    final file = File('../input');
    str = await file.readAsString();
    print(str);
    print("santa reached floor ${height()}");
    print("santa basement was at character ${basement()}");
  } catch (e) {
    print('Error reading file: $e');
  }
}


/*
how does dart compare to javascript ?

santa reached floor 280
santa basement was at character 1797

*/
