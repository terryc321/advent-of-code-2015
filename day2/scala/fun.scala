
import scala.util.matching.Regex

def partOne () = 
 val content = scala.io.Source.fromFile("../input.txt").mkString
 // val lines = content.split("\\\\n").flatMap(_.split("\\\\t")).filter(_.nonEmpty).toList
 val lines = content.split("\\n").filter(_.nonEmpty).toList
 
 var tot = 0
 // println(lines)
 for (str <- lines) yield {
  val pattern = "^([0-9]+)x([0-9]+)x([0-9]+)".r
  //val desc = "a20x30x40" match {
  str match {
    case pattern(sl,sw,sh) => { 
      val (l,w,h) = (sl.toInt , sw.toInt, sh.toInt)
      val prism = 2*l*w + 2*w*h + 2*h*l
      val extra = List(l*w,l*h,w*h).min()
      val present = prism + extra 
      tot = tot + present
      //println(s"It's box shape is $l $w $h : $prism : $extra : $present : $tot ")
    }
    case _ => "ok"
  }
  // val pattern = "([0-9]+)x([0-9]+)x([0-9]+)".r
  // str match {
  //   case pattern(l,w, h) => s"It's box shape is $l $w $h "
  // }
  // println(pattern findFirstIn str)
  //println(s"[${str}]")
 }
 println(s"part one solution $tot")
 tot
 // true
 // val description = "11:34:01.411" match {
 //  case timestamp(hour, minutes, _, _) => s"It's $minutes minutes after $hour"
 // }

/* 
  val pattern = "^([0-9]+)x([0-9]+)x([0-9]+)".r
  val desc = "20x30x40" match {
    case pattern(a,b,c) => println(s"It's box shape is $a $b $c  ")
    case _ => "ok"
  }
 */

def partTwo () = 
 val content = scala.io.Source.fromFile("../input.txt").mkString
 // val lines = content.split("\\\\n").flatMap(_.split("\\\\t")).filter(_.nonEmpty).toList
 val lines = content.split("\\n").filter(_.nonEmpty).toList
 
 var tot = 0
 // println(lines)
 for (str <- lines) yield {
  val pattern = "^([0-9]+)x([0-9]+)x([0-9]+)".r
  //val desc = "a20x30x40" match {
  str match {
    case pattern(sl,sw,sh) => { 
      val (l,w,h) = (sl.toInt , sw.toInt, sh.toInt)
      val cubic = l * w * h 
      val ribbon = List(l+l+w+w,l+l+h+h,w+w+h+h).min()
      val present = cubic + ribbon
      tot = tot + present
      //println(s"It's box shape is $l $w $h : $cubic : $ribbon : $present : $tot ")
    }
    case _ => "ok"
  }
  // val pattern = "([0-9]+)x([0-9]+)x([0-9]+)".r
  // str match {
  //   case pattern(l,w, h) => s"It's box shape is $l $w $h "
  // }
  // println(pattern findFirstIn str)
  //println(s"[${str}]")
 }
 println(s"part two solution $tot")
 tot
 // val description = "11:34:01.411" match {



@main
def main() =   
  partOne()
  partTwo()





