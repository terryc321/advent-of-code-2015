
// read input file in directory above where we run the antlr4 command
// parses it , dumps resulting parse tree to output file , again one directory above
// antlr4 Holiday.g4 && javac *.java && grun Holiday s -tree < ../input > ../output

// in one big schebang ...
// > antlr4 Holiday.g4 && javac *.java && grun Holiday s -gui

//   notice on grun 's' is the root of the 
// > grun Holiday s -gui
// alpha beta 123
// charlie delta 345
// CTRL-d

// title of grammer MUST match the file name 
grammar Holiday;		// Define a grammer called Hello
s : r ( r )*  ;   
r : ID 'to' ID '=' INT ;		// match keyword hello followed by an identifier
ID : [A-Za-z]+ ;			// match lower case identifiers
INT : [0-9]+ ; 			// an int
to : 'to' ;
eq : '='  ;
WS : [ \t\r\n]+ -> skip ; 	// skip whitespac tabs newlines returns




