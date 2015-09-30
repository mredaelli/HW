import breeze.linalg._
import breeze.numerics.{sqrt, abs, pow}


object NumUtils {
  def diff(m: DenseMatrix[Double], t: DenseVector[Double]): DenseMatrix[Double] = {
    import breeze.interpolation._
    val res = DenseMatrix.zeros[Double](m.rows, m.cols)
    val delta: Double = (t(1)-t(0))/5
    (0 until m.cols).foreach { c =>
      val f = LinearInterpolator(t, m(::, c))
      t.foreachPair{ (idx, t_) => res(idx, c) = (f(t_ +delta)-f(t_ -delta))/(2.0*delta) }
    }
    res
  }

  def diff(m: DenseVector[Double], t: DenseVector[Double]): DenseVector[Double] = {
    import breeze.interpolation._
    val res = DenseVector.zeros[Double](m.length)
    val delta: Double = (t(1)-t(0))/10
    val f = CubicInterpolator(t, m)
    t.foreachPair{ (idx, t_) =>  { if( idx > 0 && idx < m.length-1) res(idx) = (f(t_ +delta)-f(t_ -delta))/(2.0*delta) } }
    res(0) = (f(t(0) + delta)-f(t(0)))/delta
    res(m.length-1) = (f(t(m.length-1))-f(t(m.length-1)-delta))/delta
    res
  }
  def diff2(m: DenseVector[Double], t: DenseVector[Double]): DenseVector[Double] = {
    import breeze.interpolation._
    val res = DenseVector.zeros[Double](m.length)
    val delta: Double = (t(1)-t(0))/10
    val f = CubicInterpolator(t, m)
    t.foreachPair{ (idx, t_) =>  { if( idx > 0 && idx < m.length-1 )
      res(idx) = (f(t_ +delta)+2*f(t_)-f(t_ -delta))/(delta*delta) } }
    res(0) = (f(t(0) + delta)-f(t(0)))/delta
    res(m.length-1) = (f(t(m.length-1))-f(t(m.length-1)-delta))/delta
    res
  }
  def diff1(m: DenseVector[Double], t: DenseVector[Double]): DenseVector[Double] = {
    val res = DenseVector.zeros[Double](m.length)
    t.foreachPair{ (idx, t_) => if( idx > 0 && idx < m.length-1 ) res(idx) = (m(idx+1)-m(idx-1))/(t(idx+1)-t(idx-1)) }
    res(0) = (m(1)-m(0))/(t(1)-t(0))
    res(m.length-1) = (m(m.length-1)-m(m.length-2))/(t(m.length-1)-t(m.length-2))
    //res(m.length-1) = (f(t(m.length-1))-f(t(m.length-1)-delta))/delta
    res
  }
    //
  // diff(m.asDenseMatrix, t)(::, 1)

  def evolute(x: DenseVector[Double], y: DenseVector[Double], t: DenseVector[Double]): (DenseVector[Double], DenseVector[Double]) = {
    // Calculate the normal vector
    //  nv=[dy, -dx];
    val xt = diff(x, t)
    val yt = diff(y, t)
    val xtt = diff(xt, t)
    val ytt = diff(yt, t)

    //% % determine radius of curvature
    val den = abs(xt :* ytt - yt :* xtt)
    //val idx = den :== DenseVector.zeros[Double](den.length)
    //println(idx)
    //den.foreachPair( (i, b) => { if( b < 1.0 ) den(i) = 1 } )
    //println(den(idx) //:= DenseVector.ones[Double](idx.length) :* 0.00001
    //println(den)
    val num = pow(xt,2)+pow(yt, 2)
    println("den/num")
    println(num)
    println(den)
    val R = num :/ den
    println("R:")
    println(R)
/*
        % % For inner normal curve
          x_inner=x-unv(:, 1).*R;
        y_inner=y-unv(:, 2).*R;

        % % For outer normal curve
          x_outer=x+unv(:, 1).*R;
        y_outer=y+unv(:, 2).*R;
        */

    var cx: DenseVector[Double] =  x - yt :* R
    var cy: DenseVector[Double] = y + xt :* R
    //println(x)

    (xtt, ytt)
  }
}
