
package probability

private[probability] object Utils {
    def block1[A](x:A)(blk: A => Unit) = {blk(x); x}

    def K[A](value:A)(tmp:A) = value
}

