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
    case _ => throw new Exception()
  }
}
}