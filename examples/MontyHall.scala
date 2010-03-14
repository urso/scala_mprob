
object MontyHall {
    import probability._
    import Probability._

    sealed abstract class Door
    object A extends Door { override def toString = "A" }
    object B extends Door { override def toString = "B" }
    object C extends Door { override def toString = "C" }

    sealed abstract class Winning
    object Looser extends Winning { override def toString = "Looser"}
    object Winner extends Winning { override def toString = "Winner"}

    val doors = Array(A,B,C)

    final case class State(prize:Door, chosen:Door, open:Door)
    
    val hide : Distribution[Door] = uniform(doors)
    val choose : Distribution[Door] = uniform(doors)
    def opened(hidden:Door, chosen:Door) : Distribution[Door] = 
        uniform( doors.filter {x => x != hidden && x != chosen} )

    def testWinner(s:State) : Boolean = s.prize == s.chosen
    def testWinner(d:Distribution[State]) : Distribution[Winning] = d.map {s : State =>
        if(testWinner(s)) Winner
        else              Looser
    }

    def stay(d:Distribution[State]) = d

    def switchDoor(d:Distribution[State]) = d.dep { s =>
            uniform( doors.filter {x => x != s.open && x != s.chosen }.map {door =>
                new State(s.prize, door, s.open)
            }
        )
    }

    def main(arg:Array[String]) = {
        val dist = hide.dep { h =>
                 choose.dep { c =>
                 opened(h,c).dep { o =>
                 single(new State(h,c,o))
             }}}
        println( "stay:\n" ++ testWinner(stay(dist) ).toString)
        println( "switch:\n" ++ testWinner(switchDoor(dist)).toString )
    }

}

