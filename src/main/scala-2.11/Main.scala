import breeze.interpolation.CubicInterpolator
import breeze.linalg.{norm, DenseVector, DenseMatrix}
import breeze.numerics.{sin, cos}
import breeze.plot.{Figure, plot}

object Main extends App  {

  val f = Figure()
  f.rows = 2
  val p1 = f.subplot(0)
  val p2 = f.subplot(1)
  val p3 = f.subplot(2)

  val hw = HW.fromHWR("test.hwr").get

  hw.traits.filter( _.penDown ).foreach { t =>
    //p1 += plot(t.x, t.y)
  }

  val tr = hw.traits(2)
  /*val ev = NumUtils.evolute(hw)
  println(ev._1)
  println(ev._2)
  p2 += plot(ev._1, ev._2)*/
  //p3 += plot(t.t, t.v)



  val m = DenseMatrix.tabulate(300, 3)( (i, j) => j match {
    case 0 => i/50.0
    case 1 => sin(i/50.0)
    case _ => cos(i/50.0)
  })

  //println(m)
  //p1 += plot(m(::,0), m(::,1))


  //var ev = NumUtils.evolute(m(::, 1), m(::, 2), m(::, 0))
  val t = m(::, 0)
  val xt = NumUtils.diff(m(::,1), t)
  val yt = NumUtils.diff(m(::,2), t)

  val fun = CubicInterpolator(m(::,1), t)
  val res = fun(.5)
  println(res)
  //val fun1 = CubicInterpolator(xt, t)
  //val res1 = fun1(t)
  //p1 += plot(t, m(::,1)-res)
  //p2 += plot(t, xt-res1)

  val xtt = NumUtils.diff(xt, t)
  val ytt = NumUtils.diff(yt, t)
  println(yt)
  println(ytt)

  f.refresh()
  //val m = DenseMatrix((1.0, 1.0), (1.0, 1.0), (1.0, 1.0))
}

