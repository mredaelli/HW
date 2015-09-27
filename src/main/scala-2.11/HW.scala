import breeze.linalg._

class HWData(val data: DenseMatrix[Double]) {
  val x = data(::, 0)
  val y = data(::, 1)
  val p = data(::, 2)
  val a1 = data(::, 3)
  val a2 = data(::, 4)
}

class Trait(val penDown: Boolean, override val data: DenseMatrix[Double]) extends HWData(data) {
  val penUp = !penDown
}

class HW(override val data: DenseMatrix[Double]) extends HWData(data) {
  val traits: List[Trait] = partition()

  override val toString = data.toString()

  private def partition(): List[Trait] = {
    import collection.mutable
    val down: mutable.Buffer[Int] = mutable.Buffer()
    val up: mutable.Buffer[Int] = mutable.Buffer()

    var last = p(0)
    (if( last == 0 ) up else down) += 0

    val rows = data.rows - 1

    p(1 to -1).foreachPair( (idx, p) => { // idx is index - 1
      if ( (p==0) != (last==0) ) {
        (if (p == 0) up else down) += idx + 1
        (if (p == 0) down else up) += idx
      }
      last = p
    })

    (if( p(-1) == 0 ) up else down) += rows

    val upTrait = up.grouped(2).map{ b => new Trait(false, data(b.head to b.tail.head, ::)) }.toList
    val downTrait = down.grouped(2).map{ b => new Trait(true, data(b.head to b.tail.head, ::)) }.toList
    upTrait ++ downTrait
  }


}

object HW {

  def fromHWR(fn: String) : Option[HW] = {
    import resource._
    import scala.io.Source

    for(file <- managed(Source.fromFile(fn))) {
      val f = file.getLines()
      val header = f.next()
      val size = header.split("\\s+")(1).toInt
      val res = DenseMatrix.zeros[Double](size, 5)
      f.map(_.trim.split("\\s+").map(_.toDouble)).zipWithIndex.foreach {
        case (el, idx) => el.zipWithIndex.foreach {
          case (value, col) => res(idx, col) = value
        }
      }
      return Some(new HW(res))
    }
    None
  }

}

