import breeze.plot.{Figure, plot}

object Main extends App  {
  val hw = HW.fromHWR("test.hwr").get

  val f = Figure()
  val p = f.subplot(0)

  hw.traits.filter( _.penDown ).foreach { t =>
    //println(t.data)
    p += plot(t.x, t.y)
  }
  f.refresh()

}

