import scwc._
import scala.collection.mutable.{ListBuffer, ArrayBuffer, HashMap, Map}
import java.text.DecimalFormat
import java.io.{OutputStreamWriter, FileOutputStream}
import scala.math

case class Case(var row: ArrayBuffer[(Attr,Value)], val classLabel: Value, val frq: Int) {
  //  rowの中は特徴番号の順に昇順にソートされていると仮定する。
  //  以下のコードをいれて、ソートを仮定しなくてもよい。
  //
  //  row = row.sortWith {_._1 < _._1}
  //
  val size = row.size

  def apply(i: Int) = row(i)

  def attr(f: Attr) = { // feature_f
//    val i = row.indexWhere(_._1 == f)
    var i = this.indexOf(f)
    if (i == -1) 0 else row(i)._2
  }

  def <(that: Case): Boolean = {
    var f = 0    
    val m = (this.size - that.size) match {
      case 0 => this.size
      case d if d > 0 =>
        f = 1
        that.size
      case _ =>
        f = -1
        this.size
    }

    (0 to m-1).foreach {j =>
      val i = this(j)
      val a = that(j)
      (i._1 - a._1) match {
        case 0 =>
          (i._2 - a._2) match {
            case d if d < 0 => return true
            case d if d > 0 => return false
            case _ => //何もしない
          }
        case d if d > 0 => return true
        case _ => return false
      }
    }

    return f match {
      case -1 => true
      case 1 => false
      case _ => this.classLabel < that.classLabel
    }
  }

  def indexOf(x: Int): Int = {
    // xがfeatureの番号になっているところのindex
    var l = 0
    var u = this.size - 1
    if(u < 0 || row(l)._1 > x || row(u)._1 < x) return -1
    if(row(u)._1 == x) return u
    while(true) {
      if(l + 1 == u) return if(row(l)._1 == x) l else -1
      val j = (l + u)/2
      row(j)._1 match {
        case y if y <= x => l = j
        case y if y > x => u = j
      }
    }
    0 // Syntax Errorを回避するためのダミー
  }

  //   def indexOf(x: Int): Int = {
  //   // xがfeatureの番号になっているところのindex
  //   var l = 0
  //   var u = this.size - 1
  //   if(u < 0 || row(0)._1 > x) return -1
  //   if(row(u)._1 <= x) return u
  //   while(true) {
  //     if(l + 1 == u) return l
  //     val j = (l + u)/2
  //     row(j)._1 match {
  //       case row if row <= x => l = j
  //       case row if row > x => u = j
  //     }
  //   }
  //   0 // Syntax Errorを回避するためのダミー
  // }

  def isPartiallyIdenticalTo(that: Case, x: Attr): Boolean = {
    // Instanceが辞書順にソートされていることを前提に、
    // i <= x のAttrが全て一致していればtrue、そうでなければfalseを返す。
    var i = 0
    val lim = this.size.min(that.size)
    while(i < lim && this(i) == that(i) && this(i)._1 < x) i += 1
    return if(i == this.size) {
      if(i == that.size) true
      else if(that(i)._1 <= x) false
      else true
    } else if(i == that.size) {
      if(this(i)._1 <= x) false
      else true
    } else {
      if(this(i) == that(i)) true
      else if(this(i)._1 > x && that(i)._1 > x) true
      else false
    }
  }

  // def isPartiallyIdenticalTo(that: Case, x: Attr): Boolean = {
  //   // Instanceが辞書順にソートされていることを前提に、
  //   // i <= x のAttrが全て一致していればtrue、そうでなければfalseを返す。
  //   val s = this.indexOf(x)
  //   val t = that.indexOf(x)
  //   if(s != t) {
  //     false
  //   } else {
  //     (s to 0 by -1).foreach {i =>
  //       if(this(i) != that(i)) return false
  //     }
  //     true
  //   }
  // }
  
  def isIdenticalTo(that: Case, prefix: List[Int], x: Int): Boolean = {
    val s = this.indexOf(x)
    val t = that.indexOf(x)
    if(s != t) {
      false
    } else {
      if(prefix.size > 0 && !prefix.forall(i => this.attr(i) == that.attr(i)))
        return false
      (0 to s).foreach {i =>
        if(this(i) != that(i)) return false
      }
      true
    }
  }

  // 以下はでバグ用関数

  def display(attrs: Seq[Attr]): String = {
    attrs.map(a => a + ">" + this.attr(a)).mkString(" ") + " label>" + classLabel + " freq>" + frq
  }
  
  def serialize(attrs: Seq[Attr]): String = {
    row.filter(a => attrs.contains(a._1)).map{x =>x._1 + ">" + x._2}.mkString(":")
  }

  def signature: (String, String) = {
    val s = row.map{x => x._1 + ":" + x._2}.mkString(" ")
    return (s, s + " " + classLabel)
  }
}


case class Dataset(
  val data: ArrayBuffer[(ArrayBuffer[(Symbol, Value)], Value)],
  val algorithm: String, 
  val sl: Int,
    // The measure to sort features
    // 0 -> Symmetric uncertainty
    // 1 -> Mutual entropy
    // 2 -> Bayesian riek
    // 3 -> Matthew correlation coefficient
  val tutorial: Boolean,
  val verbose: Boolean
) {

  val f = new DecimalFormat("0.0000")
  val fns = new DecimalFormat("#,### nsec")
  
  // The upperbound of allowed inconsistent instances
  var tr = 0
  // データセットのConsistency
  var isConsistent = true
  // クラスの最大値＋１（クラスが連続なら、クラスの総数を想定）
  var cn = 0
  // サンプルの数
  val sn = data.size
  // クラス毎の生起数、総和は cn に等しい
  val oc = collection.mutable.Map[Value, Int]()
  // 特徴毎の生起数
  val of = scala.collection.mutable.Map[Symbol, scala.collection.mutable.Map[Value,Int]]()
  // 特徴とクラスの組み毎の生起数
  val ofc = scala.collection.mutable.Map[Symbol, scala.collection.mutable.Map[(Value,Value),Int]]()
  // The set of features
  var fs = collection.mutable.Set[Symbol]()
  // vm(symbol)は特徴 symbol の値の最大値＋１（特徴の値が連続なら、特徴の値の総数）
  val vm = collection.mutable.Map[Symbol, Int]()
  // 特徴とクラスの間のTP (Truely Positive)
  val tpc = collection.mutable.Map[Symbol, Int]()
  // 特徴とクラスの間のTN (Truely Negative)
  val tnc = collection.mutable.Map[Symbol, Int]()
  // 特徴とクラスの間のFP (Falsely Positive)
  val fpc = collection.mutable.Map[Symbol, Int]()
  // 特徴とクラスの間のFN (Falsely Negative)
  val fnc = collection.mutable.Map[Symbol, Int]()
  
  if(verbose) print("computing statistics ... ")

  data.foreach {x =>
    cn = cn.max(x._2 + 1)
    if(oc.isDefinedAt(x._2)) oc(x._2) += 1 else oc(x._2) = 1
    x._1.foreach {y =>
      if(of.isDefinedAt(y._1)) {
        if(of(y._1).isDefinedAt(y._2))
          of(y._1)(y._2) += 1
        else
          of(y._1)(y._2) = 1
        if(ofc(y._1).isDefinedAt((y._2, x._2)))
          ofc(y._1)((y._2, x._2)) += 1
        else
          ofc(y._1)((y._2, x._2)) = 1
      } else {
        of(y._1) = collection.mutable.Map[Value,Int](y._2 -> 1)
        ofc(y._1) = collection.mutable.Map[(Value,Value),Int]((y._2,x._2) -> 1)
      }
      fs += y._1
      vm(y._1) = if(vm.isDefinedAt(y._1)) vm(y._1).max(y._2 + 1) else y._2 + 1
    }
  }

  fs.foreach {x =>
    of(x)(0) = sn - of(x).foldLeft(0)(_ + _._2)
    oc.foreach {y =>
      ofc(x)((0, y._1)) = y._2 - ofc(x).foldLeft(0)((z,u) => z + (if(u._1._2 == y._1) u._2 else 0))
    }
  }

  fs.foreach {x =>
    tpc += x -> 0
    tnc += x -> 0
    fpc += x -> 0
    fnc += x -> 0
  }

  ofc.foreach {x =>
    x._2.foreach {y =>
      if(y._1._1 > 0) {
        if(y._1._2 > 0) tpc(x._1) += y._2
        else fpc(x._1) += y._2
      } else {
        if(y._1._2 > 0) fnc(x._1) += y._2
        else tnc(x._1) += y._2
      }
    }
  }

  // 特徴の総数（隠れ特徴も含む）
  val fn = fs.size + 1

  // // 特徴とクラスの組み毎の生起数
  // val ofc = scala.collection.mutable.Map[Symbol, scala.collection.mutable.Map[(Value,Value),Int]]()
  // // 特徴毎の生起数
  // val of = scala.collection.mutable.Map[Symbol, scala.collection.mutable.Map[Value,Int]]()

  /*
   Statisticsy to be computed
   */

  // Entopy of each feature and the class 
  val hfc = scala.collection.mutable.Map[Symbol, Double]()
  // Entropy of each feature
  val hf = scala.collection.mutable.Map[Symbol, Double]()
  // Entropy of the class
  var hc = 0.0
  // Bayesian risk of the class
  var brc = 0.0
  // Mutual information of each feature to the class
  val mif = scala.collection.mutable.Map[Symbol, Double]()
  // Symmetric Uncertainty of each feature to the class
  val suf = scala.collection.mutable.Map[Symbol, Double]()
  // The complement of Bayesian risk of each feature to the class (the difference from 1.0)
  val brf = scala.collection.mutable.Map[Symbol, Double]()
  // Mathew's correlation coefficient of each feature to the class 
  // when interpreting the value 0 as 0 and the other values collectively as 1
  val mcf = scala.collection.mutable.Map[Symbol, Double]()
  // Entropy of the entire features
  var entropyEntire = 0.0
  // Join entropy of the entire features and the class
  var entropyEntireLabel = 0.0
  // Mutual information of the entire features to the class
  var miEntireLabel = 0.0
  // Bayesian risk of the entire features to the class
  var bayesEntire = 0.0

  /*
   Runtime to be measured
   */
  var timeSortFeatures: Long = 0L
  var timeSortInstances: Long = 0L
  var timeSelectFeatures: Long = 0L

  // 統計量を計算する

  def xlog2(x: Double): Double = if(x == 0) 0 else x * math.log(x) / math.log(2)
  def log2(x: Double): Double = math.log(x) / math.log(2)

  hc = oc.foldLeft(0.0)((x,y) => x - xlog2(y._2))/sn + log2(sn)
  brc = 1 - oc.values.max.toDouble / sn

  fs.foreach {x =>
    hfc(x) = ofc(x).foldLeft(0.0) {(y,z) =>
      y - (if(z._2 > 0) z._2/sn.toDouble * math.log(z._2/sn.toDouble) else 0)}
    hf(x) = of(x).foldLeft(0.0) {(y,z) =>
      y - (if(z._2 > 0) z._2/sn.toDouble * math.log(z._2/sn.toDouble) else 0)}
    mif(x) = hf(x) + hc - hfc(x)
    suf(x) = mif(x)/(hf(x) + hc)
    val mx = Map[Value, Int]()
    ofc(x).foreach {y => mx(y._1._1) = if(mx.isDefinedAt(y._1._1)) mx(y._1._1).max(y._2) else y._2} 
    brf(x) = mx.foldLeft(0)(_ + _._2)/sn.toDouble
    val dnm1 = math.sqrt(tpc(x) + fpc(x)) * math.sqrt(tpc(x) + fnc(x))
    val dnm2 = math.sqrt(tnc(x) + fpc(x)) * math.sqrt(tnc(x) + fnc(x))
    mcf(x) =
      if(dnm1 == 0 || dnm2 == 0) 0
      else tpc(x)/dnm1*tnc(x)/dnm2 - fpc(x)/dnm1*fnc(x)/dnm2
  }


  if(verbose) print("computed .. ")

  // 特徴をソートする指標
  val msr = Seq(suf, mif, brf, mcf)
  val slm = List("symmetrical uncertainty", "mutual information", "Bayesian risk", "Matthew's correlation coefficient")

  def displayStats(hdl: Option[OutputStreamWriter]) {
    hdl foreach {out =>
      out.write("# Statistics\n")
      out.write("## Entropy of Classes = " + f.format(hc) + "\n")
      out.write("## Entropy\n")
      fs.foreach {x => out.write(x + ":" + f.format(hf(x)) + " ")}
      out.write("## Symmetrical uncertainty \n")
      fs.foreach {x => out.write(x + ":" + f.format(suf(x)) + " ")}
      out.write("\n")
      out.write("## Mutual information \n")
      fs.foreach {x => out.write(x + ":" + f.format(mif(x)) + " ")}
      out.write("\n")
      out.write("## Bayesian risk \n")
      fs.foreach {x => out.write(x + ":" + f.format(brf(x)) + " ")}
      out.write("\n")
      out.write("## Matthew's correlation coefficient \n")
      fs.foreach {x => out.write(x + ":" + f.format(mcf(x)) + " ")}
      out.write("\n")
    }
      print("# Statistics\n")
      print("## Entropy of Classes = " + f.format(hc) + "\n")
      print("## Entropy\n")
      fs.foreach {x => print(x + ":" + f.format(hf(x)) + " ")}
      print("\n")
      print("## Symmetrical uncertainty \n")
      fs.foreach {x => print(x + ":" + f.format(suf(x)) + " ")}
      print("\n")
      print("## Mutual information \n")
      fs.foreach {x => print(x + ":" + f.format(mif(x)) + " ")}
      print("\n")
      print("## Bayesian risk \n")
      fs.foreach {x => print(x + ":" + f.format(brf(x)) + " ")}
      print("\n")
      print("## Matthew's correlation coefficient \n")
      fs.foreach {x => print(x + ":" + f.format(mcf(x)) + " ")}
      print("\n")
  }
  
  // 特徴名(Symbol)と特徴番号(Attr)との対応表

  if(verbose) print("sorting attributes by " + slm(sl) + "...")
  val rn = this.renameFeatures
  val nr = rn.map {x => x._2 -> x._1}
  if(verbose) {
    print("sorted .. ")
    print("(" + fns.format(timeSortFeatures) + ")")
  }
  //
  val vn = vm.map(x => (rn(x._1) -> x._2)) + ((fn - 1) -> (cn + 1))
  // dataを、rnに従ってリネームし、入力から冗長な行を取り除き、
  // 出現頻度と隠れ特徴を追加し、辞書順でソート
  val src = this.aggregate
  // c はサンプルのcollectionを格納
  // 各サンプルのcollectionはprefixの値が同じサンプルの集合
  var c = ArrayBuffer(src.map {x => Case(x._1, x._2, x._3)})
  //　パーティション
  // var p = List(-1, c.size-1)

  if(tutorial) {
    println("\n********")
    println("A tutorial starts.")
    println("The specified dataset includes:")
    println("# "+ sn + " instances;")
    println("# "+ cn + " classes;")
    println("# "+ (fn - 1) + " features.")
    println("\nThe features are numbered in a decreasing order of their " + slm(sl) + " (values in parenthesis).")
    println((0 until fn-1).map(i => i + ">" + nr(i) + "(" + f.format(msr(sl)(nr(i))) + ")").mkString(" "))
    println("From now, the features are refferenced by their numbers.")
  }

  /*
   Computing H(Entire), H(Entire, C) and I(Entire; C)
   */

  def count_per_partition(x: Int): Array[ArrayBuffer[Array[Int]]] = {
    c.map{cases =>
      val n = cases.size
      var i = 0
      var count_partition = ArrayBuffer[Array[Int]]()
      var count_label = Array[Int]()
      while(i <  n) {
        count_label = Array.fill(cn)(0)
        val curr = cases(i)
        count_label(curr.classLabel) += curr.frq
        i += 1
        while(i < n && cases(i).isPartiallyIdenticalTo(curr, x)) {
          count_label(cases(i).classLabel) += cases(i).frq
          i += 1
        }
        count_partition += count_label
      }
      count_partition
    }.toArray
  }

  val count = count_per_partition(fn-1)

  entropyEntire = count(0).par.map(b => - xlog2(b.sum)).sum/sn + log2(sn)
  entropyEntireLabel = count(0).par.map(b => b.map(x => - xlog2(x)).sum).sum/sn + log2(sn)
  miEntireLabel = entropyEntire + hc - entropyEntireLabel
  bayesEntire = 1 - count(0).par.map(b => b.max).sum/sn

  if(tutorial) {
    println("\nSome statistics of the dataset are computed as:")
    println("# H(C) = " + f.format(hc))
    println("# H(Entire) = " + f.format(entropyEntire)) 
    println("# H(Entire, C) = " + f.format(entropyEntireLabel)) 
    println("# I(Entire; C) = " + f.format(miEntireLabel)) 
    println("# Br(Empty) = " + f.format(brc))
    println("# Br(Entire) = " + f.format(bayesEntire))
    println
    println("The instances of the dataset are aggregated into " + c(0).size + " case objects:")
    c(0).foreach{x => println(x.display(0 until fn - 1))}
    println
    println("SLCC iterates selecting a feature.")
    println("A single feature is selected at each iteration.")
    println("Also, a search range is associated with an iteration.")
    println("The search range determines the features from which the algorithm selects a feature at the iteration.")
    println("When executing an iteration, case objects are partitioned into one or more groups so that each group consists of case objects with the same values for the features selected so far.")
    println("In a partition, the case objects are sorted in a lexicographical order of the feature values of the features in the search range.")
    println("The search range of iterations shrinks as selecting features proceeds.")
    println("The algorithm terminates by outputing the features selected so far when the search range vanishes.")
  }

/* 以下のコードは遅すぎる
  val signatures = c(0).par.map{x => (x.signature, x.frq)}
  val countEntire = HashMap[String, Int]()
  val countEntireLabel = HashMap[String, Int]()
  for(s <- signatures) {
    if(countEntire.isDefinedAt(s._1._1)) {
      countEntire(s._1._1) += s._2
      if(countEntireLabel.isDefinedAt(s._1._2)) {
        countEntireLabel(s._1._2) += s._2
      }else{
        countEntireLabel(s._1._2) = s._2
      }
    }else{
      countEntire(s._1._1) = s._2
      countEntireLabel(s._1._2) = s._2
    }
  }

  val entropyEntire = countEntire.values.par.map{x => -x*log2(x)}.sum/sn + log2(sn)
  val entropyEntireLabel = countEntireLabel.values.par.map{x => -x*log2(x)}.sum/sn + log2(sn)
 */


  var entropySelected = 0.0
  var entropySelectedLabel = 0.0

  // ここまで

  def reset: Dataset = {
    c = ArrayBuffer(src.map {x => Case(x._1, x._2, x._3)})
    this
  }


  def renameFeatures: Map[Symbol, Attr] = {
    val start = System.nanoTime()
    val result = Map(fs.toSeq.sortWith {(x, y) => math.abs(msr(sl)(x)) > math.abs(msr(sl)(y))}.zipWithIndex: _*) + ('hidden -> (fn - 1))
    timeSortFeatures = System.nanoTime() - start
    result
  }

  def aggregate: ArrayBuffer[(ArrayBuffer[(Attr,Value)],Value,Int)] = {
    //
    // ソートして、同じものをまとめて、出現度数を添えて返す
    //

    val renamed_data = data.map{x => (x._1.map{y => (rn(y._1), y._2)}.sortWith(_._1 < _._1), x._2)}

    if(verbose) print("sorting instances .. ")
    var start = System.nanoTime()
    val sorted_data = renamed_data.sortWith((x,y) => x_younger_y(x, y))
    timeSortInstances = System.nanoTime() - start
    if(verbose) print("sorted (" + fns.format(timeSortInstances) + ") ... ")
    var res =ArrayBuffer[(ArrayBuffer[(Attr,Value)],Value,Int)]()
    var t = sorted_data(0)
    var i = 1
    var count = 1
    val n = sorted_data.size

    if(verbose) print("aggregating instances .. ")

    while(true) {
      if(i == n) {

        if(verbose) {
          print("aggregated (" + res.size + " instances: ")
          print(fns.format(System.nanoTime() - start) + ") .. ")
          print("making consistency .. ")
          start = System.nanoTime()
        }

        res = this.addHiddenFeature(res :+ (t._1, t._2, count))

        if(verbose) print("made (" + fns.format(System.nanoTime() - start) + ") ... ")

        return res
      }
      while(i < n && sorted_data(i) == t) {
        count += 1
        i += 1
      }
      if(i < n) {
        res = res :+ (t._1, t._2, count)
        t = sorted_data(i)
        i += 1
        count = 1
      }
    }
    res // Syntax Errorを避けるためのダミー
  }

  def addHiddenFeature(sorted_data: ArrayBuffer[(ArrayBuffer[(Attr,Value)],Value, Int)]):
      ArrayBuffer[(ArrayBuffer[(Attr,Value)],Value, Int)] = {
    var res = ArrayBuffer(sorted_data(0))
    (1 to sorted_data.size-1).foreach {i =>
      val curr = sorted_data(i)
      val prev = sorted_data(i-1)
      if(curr._1 == prev._1 && curr._2 != prev._2) {
        isConsistent = false
        res = res :+ ((curr._1 :+ (fn-1, curr._2 + 1)), curr._2, curr._3)
      } else {
        res = res :+ curr
      }
    }
    res
  }

  def x_younger_y(x: (ArrayBuffer[(Attr,Value)],Value),
    y: (ArrayBuffer[(Attr,Value)],Value)):
      Boolean = {
    var f = 0
    val m = (x._1.size - y._1.size) match {
      case 0 => x._1.size
      case d if d > 0 =>
        f = 1
        y._1.size
      case _ =>
        f = -1
        x._1.size
    }

    (0 to m-1).foreach {j =>
      val xx = x._1(j)
      val yy = y._1(j)
      (xx._1 - yy._1) match {
        case 0 =>
          (xx._2 - yy._2) match {
            case d if d < 0 => return true
            case d if d > 0 => return false
            case _ => //何もしない
          }
        case d if d > 0 => return true
        case _ => return false
      }
    }

    return f match {
      case -1 => true
      case 1 => false
      case _ => x._2 < y._2
    }
  }

  def findBorder(x: Int): Int = {
    // prefix + [0, x]はconsistentであることを前提
    if(!this.isConsistent(x-1)) return x
    else if(x == 0) return -1
    if(this.isConsistent(-1)) return -1
    if(this.isConsistent(0)) return 0
    var l = 0
    var u = x
    while(true) {
      if(l + 1 >= u) return u
      val j = (l + u)/2

      if(verbose) print(".")

      if(this.isConsistent(j)) {
        u = j
      } else {
        l = j
      }
    }
    0 // Syntax Errorを回避するためのダミー
  }

  def findBorderLcc(x: Int): Int = {
    if(!this.isUnderThreshold(x-1)) return x
    else if(x == 0) return -1
    if(this.isUnderThreshold(-1)) return -1
    if(this.isUnderThreshold(0)) return 0
    var l = 0
    var u = x
    while(true) {
      if(l + 1 >= u) return u
      val j = (l + u)/2

      if(verbose) print(".")

      if(this.isUnderThreshold(j)) {
        u = j
      } else {
        l = j
      }
    }
    0 // Syntax Errorを回避するためのダミー
  }


  def display = {
    var i = 0
    c.foreach {x =>
      println
      x.foreach {y =>
        print("    c(" + i +") = ")
        (0 to fn-1).foreach {j => print(y.attr(j) + " ") }
        println(": " + y.classLabel + " : " + y.frq)
        i += 1
      }
    }
    print("    Rename : ")
    println(rn.foldLeft("")((x,y) => x + y._1 + " = " + y._2 + " "))
  }

  // def displayStat = {
  //   val f = new DecimalFormat("0.000")
  //   println("Statistics")
  //   println("** Entropy of Classes = " + f.format(hc))
  //   println("** Feature : SU : MI : 1 - BR : Entropy")
  //   fs.foreach {x =>
  //     print("   " + x + " : " + f.format(suf(x)) + " : " + f.format(mif(x)) + " : ")
  //     println(f.format(brf(x)) + " : " + f.format(hf(x)))
  //   }
  // }

  def isConsistent(x: Int): Boolean = {
    // 以下のループは並列化できる
    c.foreach {y =>
      if(y.size > 1 && !this.isPartiallyConsistent(x, y)) return false
    }
    true
  }


  def isPartiallyConsistent(x: Int, y: ArrayBuffer[Case]): Boolean = {

    var prev = y(0)

    y.foreach {curr =>
      if(curr.classLabel != prev.classLabel) {
        if(curr.isPartiallyIdenticalTo(prev, x)) return false
      }
      prev = curr
    }
    true
  }


  def isUnderThreshold(x: Int): Boolean = {
    // 以下のループは並列化できる
    var count = 0
    c.foreach {y =>
      if(y.size > 1) {   //この条件分岐は不要のはず
        count += this.diff_br(x, y)
        if(count > tr) return false
      }
    }
    true
  }


  def diff_br(x: Int, y: ArrayBuffer[Case]): Int = {

    var count_br = 0
    var i = 0
    while(i < y.size) {
      var count_label = Array.fill(cn)(0)
      val curr = y(i)
      count_label(y(i).classLabel) += y(i).frq
      i += 1
      while(i < y.size && y(i).isPartiallyIdenticalTo(curr, x)) {
        count_label(y(i).classLabel) += y(i).frq
        i += 1
      }
      count_br += count_label.sum - count_label.max

      if(i >= y.size || count_br > tr) return count_br
    }
    count_br
  }

  def select(trd: Double): List[Symbol] = {
    tr = math.round((brc - trd * (brc - bayesEntire)) * sn).toInt

    val start = System.nanoTime()

    val selected = if(algorithm == "cwc")
      this.select_cwc.map(nr(_))
    else
      this.select_lcc.map(nr(_))

    timeSelectFeatures = System.nanoTime() - start

    entropySelected = c.par.map(b => - xlog2(b.map(_.frq).sum)).sum/sn + log2(sn)

    entropySelectedLabel = c.par.map{b =>
      val count = Array.fill(cn)(0.0)
      for(i <- b) count(i.classLabel) += i.frq
      count.foldLeft(0.0){(x, y) => x - xlog2(y)}
    }.sum/sn + log2(sn)

    if(verbose) {
      println("finished.")
      println("Needed "+ fns.format(timeSelectFeatures) + ".")
    }

    return selected
  }


  def select_cwc: List[Int] = {

    if(verbose) print("Selecting features with CWC ")

    var prefix:List[Int] = List()
    var x = fn - 1

    // var prefix:List[Int] = if(isConsistent) List() else List(fn - 1)
    // var x = fn - 2

    while (x >= 0) {
      //
      // トレースモード始まり
      //
      // this.display
      // print("  prefix = " + prefix + " , x = " + x + ": Select ")
      //
      // トレースモード終わり
      //
      x = this.findBorder(x)
      //
      // バイナリーサーチを使わないなら以下のコード 
      //

      // x = (-1 to x).indexWhere(this.isConsistent(prefix, _)) - 1

      //      
      // トレースモード始まり
      //
      // println(x)
      //
      // トレースモード終わり
      //

      if(x >= 0) {
        this.reSort(x)
        prefix = x::prefix
        x -= 1
      }
    }

    prefix
  }

  def select_lcc: List[Int] = {

    val start = System.nanoTime()

    if(verbose)  println("Selecting features with LCC (allowed inconsistency = " + tr + " instances) ... ")

    var prefix:List[Int] = List()
    var x = fn - 2 // 元々のコードでは fn - 1

    while (x >= 0) {

      if(tutorial) {
        println("\nA new iteration starts.")
        println("The maximum number of allowed inconsistent instances is " + tr + ".")
        if(prefix.isEmpty) {
          println("No features have been selected so far.")
        } else {
          println("The currently selected feature indices are " + prefix.mkString("", " ", "."))
        }
        println("The search range is [0, " + x + "].")
        println("The current order and partitioning of case objects are:")
        for(b <- c) {
          println(">> Partition")
          for(cs <- b) {
            println(cs.display(0 until fn-1))
          }
        }
        println("We let i in [0, " + x + "].")
        println("The number of inconsistent instances that the union of the features in [0, i-1] and those selected so far yields:")
        // for(y <- (0 to x)) {
        //   print(y + ">")
        //   println(count_per_partition(y-1).map(_.map(_.mkString("["," ","]")).mkString("["," ","]")).mkString(" "))
        // }
        println((0 to x).map{y => y + ">" +
          (sn - count_per_partition(y-1).par.map(p => p.map(_.max).sum).sum)}.mkString(" "))
      }


      //
      // トレースモード始まり
      //
      // this.display
      // print("  prefix = " + prefix + " , x = " + x + ": Select ")
      //
      // トレースモード終わり
      //
      x = this.findBorderLcc(x)

      //
      // バイナリーサーチを使わないなら以下のコード 
      //

      // x = (-1 to x).indexWhere(this.isConsistent(prefix, _)) - 1

      //      
      // トレースモード始まり
      //
      // println(x)
      //
      // トレースモード終わり
      //


      if(x >= 0) {
        this.reSort(x)
        prefix = x::prefix

        if(tutorial) {
          println("The Algorithm selects the last feature whose number of inconsistent instances is greater than " + tr + ".")
          println("Therefore, the algorithm selects the attribute of " + x + ".")
        }

        x -= 1
      }
    }

    if(tutorial) {
      println("\nThe algorithm finally has selected features of: " + prefix.mkString(" "))
      println("\nThe final sequence of instances are:")
      for(b <- c) {
        println(">> Partition")
        for(cs <- b) {
          println(cs.display(0 until fn-1))
        }
      }
      println("\nThe tutorial finishes.  Thank you.")
      println("*******")
      println
    }

    // entropySelected = c.par.map(b => - xlog2(b.map(_.frq).sum)).sum/sn + log2(sn)

    // entropySelectedLabel = c.par.map{b =>
    //   val count = Array.fill(cn)(0.0)
    //   for(i <- b) count(i.classLabel) += i.frq
    //   count.foldLeft(0.0){(x, y) => x - xlog2(y)}
    // }.sum/sn + log2(sn)

    prefix
  }

  def reSort(x: Int) = {

    // F_x の valueの数 vn(x)
    // t_c case
    // t_p partition

    c = c.par.map{b =>
      val buf = Array.fill(vn(x))(ArrayBuffer[Case]())
      b.foreach {z =>
        buf(z.attr(x)) += z
      }
      buf.filter(_.size > 0)
    }.flatten.to[ArrayBuffer]

    if(verbose) print("." + x + ".")
  }

  // def reSort(x: Int) = {

  //   // F_x の valueの数 vn(x)
  //   // t_c case
  //   // t_p partition

  //   val t_c = ArrayBuffer(ArrayBuffer[Case]())

  //   c.foreach {y =>
  //     val buf = Array.fill(vn(x))(ArrayBuffer[Case]())
  //     y.foreach {z =>
  //       buf(z.attr(x)) += z
  //     }
  //     buf.foreach {z =>
  //       if(z.size > 0) t_c += z
  //     }
  //   }

  //   c = t_c

  //   if(!tutorial) print("." + x + ".")
  // }
}

// object test {

//   def main(args: Array[String]) {
//     // args(0) -> Selector of the measure to sort features
//     // args(1) -> Threshold
//     //
//     // Caseクラスのテスト
//     //

//     val src = Seq(
//       (ArrayBuffer((0,1), (3,2), (5,2)), -1),
//       (ArrayBuffer((0,1), (3,1), (5,2), (6,1)), -1),
//       (ArrayBuffer((0,1), (3,1), (5,2)), 1),
//       (ArrayBuffer((0,1), (4,1), (5,2)), 1))

//     val c = src.map {x => Case(x._1, x._2, 1)}

//     // val c = List(
//     //   Case(ArrayBuffer((0,1), (3,2),        (5,2)),        'a),
//     //   Case(ArrayBuffer((0,1), (3,1),        (5,2), (6,1)), 'a),
//     //   Case(ArrayBuffer((0,1), (3,1),        (5,2)),        'b),
//     //   Case(ArrayBuffer((0,1),        (4,1), (5,2)),        'b)
//     // )

//     println("#")
//     println("# Test of Case Class")
//     println("#")
//     println("### Sammples to use")

//     (0 to c.size-1).foreach {j =>
//       print("c(" + j +") = ")
//       (0 to 6).foreach{i => print(c(j).attr(i) + " ")}
//       print(": " + c(j).classLabel + " : ")
//       println(c(j).row)
//     }


//     print("### Test of comparing c(i) (rows) and c(j) (columns)\n")

//     def sgn(x: Case, y: Case): String = {
//       if(x < y) {
//         "<"
//       } else if(y < x) {
//         ">"
//       } else {
//         "="
//       }
//     }

//     c.foreach {x =>
//       c.foreach {y =>
//         print(sgn(x, y) + " ")
//       }
//       print("\n")
//     }

//       print("### Test of identity of cases\n")

//     val cases = List((0,1), (1,2), (2,3))
//     val conds = List((List(),-1),(List(),4),(List(6,5),4),(List(6,5),3),(List(6,5),2),(List(5),4),(List(5),3),(List(5),2),(List(5,4),4),(List(5,4),3),(List(5,4),2),(List(5,4,3),2),(List(5,4,3),1))

//     cases.foreach {x =>
//       print("Between c(" + x._1 +") and c(" + x._2 + ")\n")
//       conds.foreach {y =>
//         print("  prefix = " + y._1.foldLeft("")(_ + _ + " ") + ": x = " + y._2 + ": ")
//         print(c(x._1).isIdenticalTo(c(x._2), y._1, y._2) + "\n")
//       }
//     }

//     println("# ")
//     println("# Test of Dataset Class")
//     println("# ")

//     val src2 =  ArrayBuffer(
//       (ArrayBuffer(('f0,1), ('f5,2), ('f4,1)), 1),
//       (ArrayBuffer(('f0,1), ('f5,2), ('f4,1)), 0),
//       (ArrayBuffer(('f3,1), ('f5,2), ('f0,1)), 1),
//       (ArrayBuffer(('f3,2), ('f0,1), ('f5,2)), 0),
//       (ArrayBuffer(('f3,1), ('f0,1), ('f6,1), ('f5,2)), 0),
//       (ArrayBuffer(('f3,1), ('f5,2), ('f0,1)), 1),
//       (ArrayBuffer(('f3,1), ('f0,1), ('f6,1), ('f5,2)), 0),
//       (ArrayBuffer(('f3,1), ('f0,1), ('f6,1), ('f5,2)), 1),
//       (ArrayBuffer(('f0,1), ('f5,2), ('f4,1)), 1)
//     )

//     println("Input data")

//     src2.foreach {x => println("    " + x._1 + " : " + x._2)}

//     println("Generated dataset")

//     val vm = Map('f0 -> 2, 'f3 -> 3, 'f4 -> 2, 'f5 -> 3, 'f6 -> 2)

//     val ds = Dataset(src2, args(0).toInt)

//     ds.display
//     ds.displayStats(None)
//     println(ds.tpc('f3) + " " + ds.fpc('f3) + " " + ds.fnc('f3) + " " + ds.tnc('f3))

//     println("    Rename = " + ds.rn)
//     println("    # Value = " + ds.vn) 

//     print("### Consistency for prefix = List()\n")

//     (ds.fn - 1 to -1 by -1). foreach {y =>
//       print("  " + y +" : " + ds.isConsistent(y) + "\n")
//     }

//     print("### Selection\n")
//     print("#### Emulation\n")

//     val hist = List((List(), 5), (List(5), 4), (List(3, 5), 2))
//     ds.display
//     hist.foreach {x =>
//       print("  prefix = " + x._1 + ", x = " + x._2 + ": Select ")
//       val n = (-1 to x._2).indexWhere(ds.isConsistent(_)) - 1
//       print(n + "\n")

//       if(n >= 0) {
//         ds.reSort(n)
//         ds.display
//       }
//     }

//     println("#### Trace of \"select\"")
//     // selectのトレースモードのコードをアクティベート
//     println("Selected fetures are " + ds.reset.select(args(1).toDouble))
//   }
// }
