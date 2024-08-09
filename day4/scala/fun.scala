

import java.security.MessageDigest

def md5(s: String) = {
    MessageDigest.getInstance("MD5").digest(s.getBytes).map("%02X".format(_)).mkString + "-" + s
}


def solve5(secret : String) : Int = {
  var n = 0 
  while( true) {
    var s = secret + n.toString()
    val m = md5(s)
    val sub = m.substring(0,5)   
    if (sub == "00000"){
      println(s"solution : s = ${s} -> ${md5(s)} -> ${sub} -> ${n}")
      return n
    }
    n = n + 1
  }
  return n
}

def solve6(secret : String) : Int = {
  var n = 0 
  while( true) {
    var s = secret + n.toString()
    val m = md5(s)
    val sub = m.substring(0,6)   
    if (sub == "000000"){
      println(s"solution : s = ${s} -> ${md5(s)} -> ${sub} -> ${n}")
      return n
    }
    n = n + 1
  }
  return n
}


@main
def main() = 
  println(md5("hello"))
  println(md5("abcdef609043"))
  println(md5("pqrstuv1048970"))
  solve5("abcdef")
  solve5("pqrstuv")

  println("part one > ")
  solve5("bgvyzdsv")
  
  println("part two > ")
  solve6("bgvyzdsv")








