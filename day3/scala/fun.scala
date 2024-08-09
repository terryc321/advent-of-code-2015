
import scala.collection.mutable.Set as MSet


def partOne () = 
 val set: MSet[(Int,Int)] = MSet((1,1))
 var x = 1 
 var y = 1 
 val lines = scala.io.Source.fromFile("../input").mkString
 // println(lines)
 for (c <- lines) yield {
   if c == '^' then
     y = y - 1
     set.add((x,y))
   else if c == 'v' then
     y = y + 1
     set.add((x,y))
   else if c == '<' then
     x = x - 1
     set.add((x,y))
   else if c == '>' then
     x = x + 1
     set.add((x,y))
 }
 // println(set)
 val solution = set.count( (x) => true)
 println(s"solution to part1 = ${solution}")
 


def partTwo () = 
 val set1: MSet[(Int,Int)] = MSet((1,1))
 val set2: MSet[(Int,Int)] = MSet((1,1))

 var x1 = 1 
 var y1 = 1 
 var x2 = 1
 var y2 = 1
 var z = 0
 val lines = scala.io.Source.fromFile("../input").mkString
 // println(lines)

 for (c <- lines) yield {
   if (z % 2) == 0 then {
     if c == '^' then
       y1 = y1 - 1
       set1.add((x1,y1))
     else if c == 'v' then
       y1 = y1 + 1
       set1.add((x1,y1))
     else if c == '<' then
       x1 = x1 - 1
       set1.add((x1,y1))
     else if c == '>' then
       x1 = x1 + 1
       set1.add((x1,y1))

     // if c == '^' || c == '>' || c == '<' || c == 'v' then 
     //   z = z + 1     

   }
   else {
     if c == '^' then
       y2 = y2 - 1
       set2.add((x2,y2))
     else if c == 'v' then
       y2 = y2 + 1
       set2.add((x2,y2))
     else if c == '<' then
       x2 = x2 - 1
       set2.add((x2,y2))
     else if c == '>' then
       x2 = x2 + 1
       set2.add((x2,y2))
   }

   if c == '^' || c == '>' || c == '<' || c == 'v' then
     z = z + 1


 }

 // println(set1)
 // println(set1.count( (x) => true))
 // println(set2)
 // println(set2.count( (x) => true))
 
 val solution = (set1 ++ set2).count( (x) => true)
 println(s"solution part2  = ${solution}")
 /*
 val tot2 = set1.count( (x) => true ) 
 println(s"tot2 = ${tot2}")
 val tot3 = set1.count( (x) => true) + set2.count( (x) => true)
 println(s"tot3 = ${tot3}")
 val inter1 = set1.intersect(set2) 
 println(s"intersection = ${inter1}")
 println(s"count = ${inter1.count( (x) => true)}")
  */



@main
def main() = 
  partOne()
  partTwo()




