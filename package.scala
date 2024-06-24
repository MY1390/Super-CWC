package object scwc {
  def time[A](a: => A) = {
    System.gc
    val now = System.nanoTime
    val result = a
    val micros = (System.nanoTime - now) / 1000
    (result, micros)
  }

  val HIDDEN = 'hidden
  type Attr = Int //Symbol
  type Value = Int //Symbol
  //type Row = Seq[(Attr, Value)]
}
