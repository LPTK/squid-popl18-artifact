package popl18

class Array[T](val size: Int) {
  //val underlying = new Array[AnyRef](size)
  val underlying = new scala.Array[AnyRef](size).asInstanceOf[scala.Array[T]]
  def apply(i: Int) = underlying(i)
  def update(i: Int, v: T) = underlying(i) = v
  //def size = underlying.size
}
