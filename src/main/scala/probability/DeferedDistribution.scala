
package probability
import Utils._

abstract sealed class DeferedDistribution[A] {
    def flatMap[B](f: A => DeferedDistribution[B]) : DeferedDistribution[B] =
        this match {
            case PNone() => PNone[B]()
            case _     => PBind(this, f)
        }

    def map[B](f: A => B) : DeferedDistribution[B] = 
        this match {
            case PNone() => PNone[B]()
            case _     => PFunctor(this, f)
        }

    def pick : Option[A]

    def reify : Distribution[Option[A]]

    def prob = reify

    def normalizedProb = Probability.normalize(reify)

    def collect(pred: () => Boolean) : Distribution[Option[A]] = {
        import scala.collection._
        new Distribution(
            block1(new mutable.HashMap[Option[A],Double]) { m =>
              while(pred()) {
                val x = pick
                m += x -> (m.getOrElse(x, 0.0) + 1.0)
              }
            })
    }

    def normalizedCollect(pred: () => Boolean) : Distribution[A] = 
        Probability.normalize(collect(pred))
}

private case class PChoice[A](d:Distribution[A]) extends DeferedDistribution[A] {
    def pick = Some(d.pick._1)

    def reify = d.map(Some(_))
}

private case class PValue[A](a:A) extends DeferedDistribution[A] {
    def pick = Some(a)

    def reify = Probability.single(Some(a))
}

private case class PNone[A]() extends DeferedDistribution[A] {
    def pick = None

    def reify = Probability.single(None)
}

private case class PBind[A,B](a:DeferedDistribution[A],
                              cont: A => DeferedDistribution[B])
  extends DeferedDistribution[B] 
{
    def pick = a.pick.flatMap { cont(_).pick }

    def reify = a.reify.flatMap { _ match {
            case None    => Probability.single(None)
            case Some(a) => cont(a).reify
        }
    }
}

private case class PFunctor[A,B](a:DeferedDistribution[A],
                            f: A => B)
  extends DeferedDistribution[B]
{
    def pick = a.pick.map(f)

    def reify = a.reify.map( _.map(f) )
}

object defered extends ProbabilityLang[DeferedDistribution] {

    def dist[A](d:Distribution[A]) : DeferedDistribution[A] = PChoice(d)

    def single[A](a:A) : DeferedDistribution[A] = PValue[A](a)

    def uniform[A](d:Iterable[A]) = dist(Probability.uniform(d))

    def linear[A](d:Iterable[A]) = dist(Probability.linear(d))

    def negExp[A](d:Iterable[A]) = dist(Probability.negExp(d))

    def normal[A](data:Iterable[A]) = dist(Probability.normal(data))

    def flip[A](p:Double, a1:A, a2:A) = dist(Probability.flip(p,a1,a2))

    def condition[A](b:Boolean, f: => DeferedDistribution[A]) : DeferedDistribution[A] = 
        if (b) f else PNone()

    def pnull[A] : DeferedDistribution[A] = PNone[A]()

    def loopK(k:Int) : (() => Boolean) = {
        var tmp = k;
        { () => val r = tmp > 0; tmp -= 1; r }
    }

    def loopMaxMs(k:Long) : (() => Boolean) = {
        import java.lang.System
        val start = System.currentTimeMillis()
            return {() => (System.currentTimeMillis() - start) < k }
    }
}

