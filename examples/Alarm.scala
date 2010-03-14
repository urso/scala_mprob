
object Alarm {
    import probability._
    import Probability._

    case class Bulgary(s:Boolean)
    case class Earthquake(s:Boolean)
    case class Alarm(s:Boolean)
    case class John(s:Boolean)
    case class Mary(s:Boolean)

    case class State(b:Bulgary, e:Earthquake, a:Alarm, j:John, m:Mary) 

    def mkProb[A](p:Double, mk: Boolean => A) = choose(p, mk(true), mk(false))

    val pBulgary : Distribution[Bulgary] = mkProb(0.001, Bulgary)

    val pEarthquake : Distribution[Earthquake] = mkProb( 0.002, Earthquake )

    def alarm(b:Bulgary, e:Earthquake) = {
        val p = (b.s, e.s) match {
            case (true, true)  => 0.95
            case (true, false) => 0.94
            case (false, true) => 0.29
            case (false, false) => 0.001
        }
        mkProb(p, Alarm)
    }

    def john(a:Alarm) = mkProb(if(a.s) 0.9 else 0.05, John)
    def mary(a:Alarm) = mkProb(if(a.s) 0.7 else 0.01, Mary)

    def PJoint[A]( mkState: (Bulgary, Earthquake, Alarm, John, Mary) => A)   = 
        pBulgary.dep     { b =>
        pEarthquake.dep  { e =>
        alarm(b,e).dep   { a =>
        john(a).dep      { j =>
        mary(a).dep      { m =>
            single(mkState(b,e,a,j,m))
    }}}}}

    def main(args:Array[String]) =
        println(PJoint(State).filter { s:State =>
            !s.e.s && s.j.s && s.m.s && s.a.s
        })
}

