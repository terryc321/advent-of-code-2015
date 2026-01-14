
/* javascript fun.js
   how do we read contents of a file into variable ?
   backticks to interpolate a string -- assume backticks optimized away
   
*/


const fs = require('fs')

let str = fs.readFileSync('../input').toString()

console.log(str);
console.log(str.length);
   
str = str.replace(/[^()]/g, '');
console.log(str.length);
const strlen = str.length;

function height (){
    let floor = 0;
    for(let i=0;i< strlen ;i++){
	if (str[i] === '('){
	    floor ++;
	}
	else if (str[i] === ')'){
	    floor --;
	}	
    }
    return floor
}

console.log(`santa gets off on floor ${height()}`);

function basement (){
    let floor = 0;
    let ch = 0;
    for(let i=0;i< strlen ;i++){
	if (str[i] === '('){
	    floor ++;
	    ch++;
	}
	else if (str[i] === ')'){
	    floor --;
	    ch++;
	    if (floor < 0){
		return ch;
	    }
	}	
    }
    return -1;
}

console.log(`santa finds the basement at character ${basement()}`);



