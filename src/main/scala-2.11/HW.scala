import breeze.linalg._
import breeze.numerics.{sqrt, pow}

class HWData(val data: DenseMatrix[Double]) {
  val t = data(::, 0)

  val x = data(::, 1)
  val y = data(::, 2)
  val p = data(::, 3)
  val a1 = data(::, 4)
  val a2 = data(::, 5)

   val ddata = NumUtils.diff(data, t)
   val xt = ddata(::, 1)
   val yt = ddata(::, 2)
   val v = sqrt(pow(xt, 2) + pow(yt, 2))
   val pt = ddata(::, 3)
   val V = sqrt(pow(xt, 2) + pow(yt, 2) + pow(pt, 2))
   val a1t = ddata(::, 4)
   val a2t = ddata(::, 5)

  val xtt = NumUtils.diff(xt, t)
  val ytt = NumUtils.diff(yt, t)
}

class Trait(val penDown: Boolean, override val data: DenseMatrix[Double]) extends HWData(data) {
  val penUp = !penDown
}

class HW(override val data: DenseMatrix[Double]) extends HWData(data) {
  val traits: Array[Trait] = partition()

  override val toString = data.toString()

  private def partition(): Array[Trait] = {
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

    val upTrait = up.grouped(2).map{ b => (b.head, new Trait(false, data(b.head to b.tail.head, ::))) }.toArray
    val downTrait = down.grouped(2).map{ b => (b.head, new Trait(true, data(b.head to b.tail.head, ::))) }.toArray
    (upTrait ++ downTrait).sortBy( f => f._1 ).map( f => f._2 )
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
      val t = DenseVector.rangeD(0.0, size, 1).asDenseMatrix.t
      f.map(_.trim.split("\\s+").map(_.toDouble)).zipWithIndex.foreach {
        case (el, idx) => el.zipWithIndex.foreach {
          case (value, col) => res(idx, col) = value
        }
      }
      return Some(new HW(DenseMatrix.horzcat(t, res)))
    }
    None
  }

}

