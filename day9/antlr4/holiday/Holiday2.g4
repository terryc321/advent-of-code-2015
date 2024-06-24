

// > grun Holiday s -gui
// alpha beta 123
// charlie delta 345
// CTRL-d

// title of grammer MUST match the file name 
grammar Holiday;		// Define a grammer called Hello
s : r ( r )*  ;   
r : ID ID INT ;		// match keyword hello followed by an identifier
ID : [a-z]+ ;			// match lower case identifiers
INT : [0-9]+ ; 			// an int
to : 'to'  ;
eq : '='  ;
WS : [ \t\r\n]+ -> skip ; 	// skip whitespac tabs newlines returns




