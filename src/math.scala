object Conversions {
 import scala.language.implicitConversions
 implicit val _rt:Int = 1
 implicit def _Int2Value(i: Int):Value = new Value(i)
 implicit def _Char2Ch(c: Char):Ch = new Ch(c)
 implicit def _Int2Num(i:Int):Num = Num( new Value(i) )
 implicit def _Char2Num(c: Char): Num = Num(new Ch(c) )
 implicit def _String2Ch(s: String):Ch = {
   require( s.length == 1 )
   new Ch(s.head)
 }
 implicit def _String2Num(s:String):Num = {
   require( s.length == 1)
   Num(new Ch(s.head))
 }
}

class Term(val lbox: List[Num] ) {
  val length: Int = lbox.length
  def *(that: Num): Term = new Term(this.lbox.init ::: (this.lbox.last * that).lbox)
  def *(that: Term): Term = new Term(this.lbox.init ::: (this.lbox.last * that.lbox.head).lbox ::: that.lbox.tail )

  override def toString() : String = {
    def _toString( l:List[Num] ): String = {
      if (l == Nil ) ""
      else if (l.tail == Nil ) l.head.toString
           else l.head.toString + _toString(l.tail)
    }
    _toString(lbox)
  }
}

class Ch(val c:Char) {
  override def equals(that:Any): Boolean = {
    that match {
      case ch: Ch => this.c == ch.c
      case _ => false
    }
  }

  override def toString(): String = c.toString
}

class Value( _n:Int)(implicit _rt: Int) {
  private def root(i: Int):(Int, Int) = {
    def fact(i:Int):List[Int] ={
      def _fact(i:Int):List[Int] = {
        if (i == 1) List()
        else __fact(i , 2) :: _fact(i / __fact(i, 2))
      }
      def __fact(i:Int, d:Int): Int = {
        if (i < d) i
        else {
          if ( i % d == 0 ) d
          else __fact(i, d+1)
        }
      }
  
      if (i == 0 || i == 1) List(i)
      else _fact(i)
    }

    def remove(l:List[Int], key:Int): List[Int] = {
       if (l == Nil) Nil
       else {
         if (l.head == key) l.tail
         else l.head :: remove(l.tail, key)
       }
    }
    if ( fact(i).length == 1 ) (1, i)
    else if (fact(i).length == 2) {
      if (fact(i).head == fact(i).tail.head) ( fact(i).head, 1)
      else (1, i)
    }
    else {
      val target = fact(i).head
      if ( fact(i).tail.contains(target) ) (target * root( remove(fact(i).tail, target).reduceLeft(_*_) )._1 , root(remove(fact(i).tail, target).reduceLeft(_*_))._2)
      else (1 * root(fact(i).tail.reduceLeft(_*_))._1, target * root(fact(i).tail.reduceLeft(_*_))._2)
    }
  }

  val n:Int = _n * root(_rt)._1
  val rt: Int = root(_rt)._2

  def *(that: Value):Value = new Value( this.n * that.n )( this.rt * that.rt )
  
  override def equals(that: Any): Boolean = {
    that match {
      case a:Value => (this.n == a.n) & (this.rt == a.rt)
      case _ => false
    }
  }
  
  override def toString(): String = {
    if ( rt == 1 ) n.toString
    else {
      if ( n == 1 ) "√(" + rt.toString + ")"
      else n.toString + "√(" + rt.toString + ")"
    }
  }
}

class Num(val num:Option[Value], val ch:Option[Ch]) extends Box{
  require( num.isDefined || ch.isDefined )
  require( !(num.isDefined && ch.isDefined) )
  def *(that:Num): Term = {
    if (this.num.isDefined) {
      if (that.num.isDefined) {
        new Term( Num( this.num.get * that.num.get ) :: Nil )
      }
      else{
        new Term( this :: that :: Nil )
      }
    }
    else{
      new Term( this :: that :: Nil)
    }
  }
  
  override def equals(that:Any): Boolean = {
    that match {
      case num: Num => {
        if (this.num.isDefined){
          if (num.num.isDefined) {
            this.num.get == num.num.get
          }
          else{
            false
          }
        }
        else {
          if (num.num.isDefined){
            false
          }
          else{
            this.ch.get == num.ch.get
          }
        }
      }
      case _ => false
    }
  }
  
  override def toString:String = {
    if ( num.isDefined ) num.get.toString
    else ch.get.toString
  }
}

object Num {
  val _num:Option[Value] = None
  val _ch: Option[Ch] = None
  def apply(num: Value): Num = new Num(Some(num), _ch)
  def apply(ch: Ch): Num = new Num(_num, Some(ch))
}

class Root(val n:Num){

  override def equals(that:Any) = {
    that match {
      case r: Root => this.n == r.n
      case _ => false
    }
  }
  override def toString(): String = "√(" + n.toString + ")" 
}

object Root {
  def apply(v: Int, r: Int): Num = new Num( Some( new Value(v)(r) ), None)
  def apply(r: Int): Num = new Num( Some( new Value(1)(r) ), None)
}

class Box


object Tool {
def greek(str:String):Char = {
  str match {
    case "\\alpha" => 'α'
    case "\\beta" => 'β'
    case "\\gamma" => 'γ'
    case "\\delta" => 'δ'
    case "\\epsilon" => 'ε'
    case "\\zeta" => 'ζ'
    case "\\eta" => 'η'
    case "\\theta" => 'θ'
    case "\\iota" => 'ι'
    case "\\kappa" => 'κ'
    case "\\lambda" => 'λ'
    case "\\mu" => 'μ'
    case "\\nu" => 'ν'
    case "\\xi" => 'ξ'
    case "\\omicron" => 'ο'
    case "\\pi" => 'π'
    case "\\rho" => 'ρ'
    case "\\sigma" => 'σ'
    case "\\tau" => 'τ'
    case "\\upsilon" => 'υ'
    case "\\phi" => 'φ'
    case "\\chi" => 'χ'
    case "\\psi" => 'ψ'
    case "\\omega" => 'ω'
    case "\\Alpha" => 'Α'
    case "\\Beta" => 'Β'
    case "\\Gamma" => 'Γ'
    case "\\Delta" => 'Δ'
    case "\\Epsilon" => 'Ε'
    case "\\Zeta" => 'Ζ'
    case "\\Eta" => 'Η'
    case "\\Theta" => 'Θ'
    case "\\Iota" => 'Ι'
    case "\\Kappa" => 'Κ'
    case "\\Lambda" => 'Λ'
    case "\\Mu" => 'Μ'
    case "\\Nu" => 'Ν'
    case "\\Xi" => 'Ξ'
    case "\\Omicron" => 'Ο'
    case "\\Pi" => 'Π'
    case "\\Rho" => 'Ρ'
    case "\\Sigma" => 'Σ'
    case "\\Tau" => 'Τ'
    case "\\Upsilon" => 'Υ'
    case "\\Phi" => 'Φ'
    case "\\Chi" => 'Χ'
    case "\\Psi" => 'Ψ'
    case "\\Omega" => 'Ω'
    case _ => throw new Exception()
  }
}

def equal(a:String, b:String):Boolean = a == b

// if gt(a, b) => true then, a > b
def gt(a:String, b:String) : Boolean = {
  if (a.isEmpty && b.isEmpty) false
  else {
    if (a.isEmpty) false
    else if (b.isEmpty) true
    else {
      if (a.length > b.length) true
      else if (a.length < b.length) false
      else {
        if (a.head > b.head) true
        else if (a.head < b.head) false
        else gt(a.tail, b.tail)
      }
    }
  }
}

def add(a:String, b:String): String = {
  def ____add(a:Char, b:Char): (Char, Char) = {
    ( ((((a - '0') + (b - '0'))/10)+'0').toChar, ((((a - '0') + (b - '0'))%10)+'0').toChar )
  }

  def ___add(a:Char, b: Char, c: Char) : (Char, Char) = {
    (____add( ____add(a, b)._1, ____add(____add(a, b)._2, c)._1)._2, ____add(____add(a , b)._2 , c)._2)
  }

  def __add(a:String, b: Char): String = {
    if (a.reverse.isEmpty) {
      if (b == '0') ""
      else b.toString
    }
    else  __add(a.init, ____add(a.last, b)._1) + ____add(a.last, b)._2.toString
  }

  def _add(a: String, b:String, c:Char):String = {
     if (a == "" && b == "") {
       if (c == '0') ""
       else c.toString
     }
     else if ( a == "") __add(b, c)
     else if ( b == "") __add(a, c)
     else _add(a.init, b.init, ___add(a.last, b.last, c)._1) + ___add( a.last, b.last, c)._2.toString
  }

_add(a, b, '0')
}


def mul(a: String, b:String): String = {
  def __mul(a:Char, b:Char): (Char, Char) = {
    ( ((((a - '0') * (b - '0'))/10)+'0').toChar, ((((a - '0') * (b - '0'))%10)+'0').toChar )
  }

  def _mul(a:String, b:Char) : String = {
    if (a.isEmpty) ""
    else {
      if (__mul(a.last, b)._1 == '0') add( _mul(a.init, b) + "0",  __mul(a.last , b)._2.toString)
      else add( _mul(a.init, b) + "0",  __mul(a.last, b)._1.toString + __mul(a.last , b)._2.toString)
    }
  }

  if (b.isEmpty) ""
  else add( mul(a, b.init) + "0", _mul(a, b.last) )
}


def sub(a:String, b:String):String = {
  def ___sub(a:Char, b:Char): (Char, Char) = {
    if ( a >= b ) ( '0' , (((a - '0') - (b - '0'))+'0').toChar)
    else ( '1' , ((((a - '0') + 10) - (b - '0'))+'0').toChar)
  }

  def __sub(a:String, b:Char): String = {
    if (a.isEmpty) {
      if ( b != '0') throw new Error()
      else ""
    } else {
      if (__sub(a.init, ___sub(a.last, b)._1) == "" ) {
        if ( ___sub(a.last, b)._2 == '0' ) ""
        else ___sub(a.last, b)._2.toString
      }
      else __sub(a.init, ___sub(a.last, b)._1) + ___sub(a.last, b)._2.toString  
    }
  }

  def _sub(a:String, b:String): String = {
    if (b.isEmpty) a
    else if (a.isEmpty) throw new Error
    else if (__sub(a, b.last).isEmpty) ""
    else if ( (_sub( __sub(a, b.last).init , b.init) == "") && (__sub(a, b.last).last == '0') ) ""
    else _sub( __sub(a, b.last).init , b.init) + __sub(a, b.last).last
  }

  if (equal(a, b)) "0"
  else _sub(a, b)
}

def div(a:String, b:String): (String, String) = {
  if ( gt(a, b) ){ // a > b
    if ( gt( mul(b, "2"), a) ) ("1", sub(a, b))
    else if ( gt( mul(b, "3"), a) ) ("2", sub(a, mul("2", b)))
    else if ( gt( mul(b, "4"), a) ) ("3", sub(a, mul("3", b)))
    else if ( gt( mul(b, "5"), a) ) ("4", sub(a, mul("4", b)))
    else if ( gt( mul(b, "6"), a) ) ("5", sub(a, mul("5", b)))
    else if ( gt( mul(b, "7"), a) ) ("6", sub(a, mul("6", b)))
    else if ( gt( mul(b, "8"), a) ) ("7", sub(a, mul("7", b)))
    else if ( gt( mul(b, "9"), a) ) ("8", sub(a, mul("8", b)))
    else if ( gt( mul(b, "10"), a) ) ("9", sub(a, mul("9", b)))
    else {
      if ( div(a.init, b)._2 == "0") ( div(a.init, b)._1 + div(a.last.toString, b)._1 , div(a.last.toString, b)._2)
      else ( div(a.init, b)._1 + div( div(a.init, b)._2 + a.last.toString, b)._1 , div(div(a.init, b)._2 + a.last.toString, b)._2)      
    }
  }
  else if (equal(a, b)) ("1", "0")
  else ( "0", a )
}

}