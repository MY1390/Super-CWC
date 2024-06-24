import scwc._

import weka.core.Instances
import weka.core.converters.ArffSaver
import weka.core.converters.ConverterUtils.DataSource
import weka.core.Instances
import weka.filters.Filter
import weka.filters.unsupervised.attribute.Remove

import scala.collection.mutable.{HashMap,HashSet,ArrayBuffer}
import scala.collection.JavaConversions._

import java.text.DecimalFormat
import java.io.{File, OutputStreamWriter, FileOutputStream}

import scopt.OptionParser

case class MainOption(
  sort: String = "su",
  threshold: Double = 1.0,
  in: String = "",
  out: String = "",
  algorithm: String = "lcc",
  log: String = "low",
  tutorial: Boolean = false,
  verbose: Boolean = true
)

case class ARFFReader(filename: String) {
  var instances = new DataSource(filename).getDataSet

  // println
  // println("DEBUG>>")
  // println("instances.enumerateInstances = ")
  // // instances.enumerateInstances.foreach(println(_))
  // println(instances.enumerateInstances.toList(0))
  // println("<<DEBUG")

  val attr2index = HashMap[Symbol,Int]()
  val index2attr = HashMap[Int,Symbol]()
  (0 until instances.numAttributes).foreach {index =>
    attr2index += Symbol(instances.attribute(index).name) -> index
    index2attr += index -> Symbol(instances.attribute(index).name)
  }

  val numInstances  = instances.numInstances
  val numAttrs = instances.numAttributes
  val sparse_instances = sparseInstances

  def sparseInstances = {
    instances.enumerateInstances.map { instance =>
      val n = instance.numValues
      val body: ArrayBuffer[(Symbol, Int)] =
        for(i <- (0 until n - 1 ).to[ArrayBuffer]
          if instance.value(instance.index(i)).toInt != 0) yield {
          val attr_sym = Symbol(instance.attribute(instance.index(i)).name)
          Pair(attr_sym, instance.value(instance.index(i)).toInt)
        }
      val temp_sym = Symbol(instance.attribute(instance.index(n - 1)).name)
      if(temp_sym == index2attr(instances.numAttributes - 1)) {
        (body, instance.value(instance.index(n - 1)).toInt)
      } else {
        body += Pair(temp_sym, instance.value(instance.index(n - 1)).toInt)
        (body, 0)
      }
    }
  }

  // def sparseInstances = {
  //   instances.enumerateInstances.map { instance =>
  //     val n = instance.numValues
  //     val body: ArrayBuffer[(Symbol, Int)] =
  //       (0 until n - 1 ).to[ArrayBuffer].map { i:Int =>
  //         val attr_sym = Symbol(instance.attribute(instance.index(i)).name)
  //         //        attr2index(attr_sym)=i
  //         Pair(attr_sym, instance.value(instance.index(i)).toInt)
  //       }
  //     val temp_sym = Symbol(instance.attribute(instance.index(n - 1)).name)
  //     if(temp_sym == index2attr(instances.numAttributes - 1)) {
  //       (body, instance.value(instance.index(n - 1)).toInt)
  //     } else {
  //       body += Pair(temp_sym, instance.value(instance.index(n - 1)).toInt)
  //       (body, 0)
  //     }
  //   }
  // }

  def removeUnselectedAttrs(selected_attrs: List[Symbol]) {
    val remove_list =
      ((for (attr <- selected_attrs if attr != HIDDEN) yield (attr2index(attr))) ::: List(instances.numAttributes - 1)).toArray

    val filter = new Remove()
    filter.setAttributeIndicesArray(remove_list)


    filter.setInvertSelection(true)
    filter.setInputFormat(instances)
    instances = Filter.useFilter(instances, filter)
  }

  def saveArffFile (output_file_name: String) {
    val arff_saver = new ArffSaver()
    arff_saver.setInstances(instances)
    arff_saver.setFile(new File(output_file_name))
    arff_saver.writeBatch()
  }
}

object Main {

  val f = new DecimalFormat("0.0000")
  val fns = new DecimalFormat("#,### nsec")

  def main(args: Array[String]) {

    val parser = new OptionParser[MainOption]("Super LCC & Super CWc") {
      opt[String]('i', "input") required() valueName("<path>") action { (x, o) =>
        o.copy(in = x)
      } text("A path to an input ARFF file")

      opt[String]('o', "output") valueName("<path>") action { (x, o) =>
        o.copy(out = x)
      } text("A path to an output ARFF file")

      opt[String]('a', "algorithm") valueName("<lcc (default), cwc>") action { (x, o) =>
        o.copy(algorithm = x)
      } text("An algorithm to select features: SLCC or SCWC")

      opt[String]('s', "sort") valueName("<su,mi,br,mc>") action { (x, o) =>
        o.copy(sort = x)
      } text("A statistical measure to sort features: su (symmetric uncertainty, default), mi (mutual information), br (Bayesian risk) or mc (Matthew's correlation coefficient)")

      opt[Double]('t', "threshold") valueName("<value>") action { (x, o) =>
        o.copy(threshold = x)
      } text("Threshold: 1.0 (default)")

      opt[String]('l', "log") valueName("<high,low,none>") action { (x, o) =>
        o.copy(log = x)
      } text("Level of detail of log: high, low (default) or none")

      opt[Boolean]('T', "tutorial") valueName("<true, false>") action { (x, o) =>
        o.copy(tutorial = x)
      } text("Tutorial mode: true or false (default)")

      opt[Boolean]('v', "verbose") valueName("<true, false>") action { (x, o) =>
        o.copy(verbose = x)
      } text("Verbose: true (default) or false")
    }

    var sort = "su"
    var threshold = 1.0
    var in = ""
    var out = ""
    var algorithm = "lcc"
    var log = "low"
    var tutorial = false
    var verbose = true

    parser.parse(args, MainOption()) map { option =>
      // 引数解析に成功した場合
      sort = option.sort
      sort match {
        case "su" => // Do nothing
        case "mi" => // Do nothing
        case "br" => // Do nothing
        case "mc" => // Do nothing
        case _ => //
          println("Wrong measure name " + sort)
          println(parser.usage)
          return
      }
      algorithm = option.algorithm
      algorithm match {
        case "lcc" => // Do nothing
        case "cwc" => // Do nothing
        case _ => //
          println("Wrong measure name " + algorithm)
          println(parser.usage)
          return
      }
      threshold = option.threshold
      in = option.in
      out = option.out
      log = option.log
      log match {
        case "high" => // Do nothing
        case "low" => // Do nothing
        case "none" => // Do nothing
        case _ => //
          println("Wrong specification " + log)
          println(parser.usage)
          return
      }
      tutorial = option.tutorial
      verbose = option.verbose
    } getOrElse {
      // 引数解析に失敗した場合
      // sys.exit(1)
      // println(parser.usage)
      return
    }

    println("\n*** Super LCC\n")

    println("Input file = " + in)
    if(out != "") println("Output file = " + out)
    println("Algorithm = " + algorithm)
    println("Sorting measure = " + sort)
    if(algorithm == "lcc") println("Threshold = " + threshold)
    println("Level of detail of log = " + log)
    println("Tutorial mode = " + tutorial)
    println("Verbose = " + verbose)
    println
    print("Reading file ... ")
    val db = ARFFReader(in)
    val data = db.sparse_instances.to[ArrayBuffer]
    println("finished.")
    println("Found "+db.numInstances+" instances and "+db.numAttrs+" features.")

    if(tutorial) {
      if(algorithm == "cwc") {
        println("Tutorial mode is canceled because CWC is selected.")
        tutorial = false
      } else if(db.numInstances > 20 || db.numAttrs > 10){
        println("Tutorial mode is cancelled because the dataset is too large.")
        tutorial = false
      }
      println("Verbose mode is cancelled because tutorial mode is selected.")
      verbose = false
    }

    val sort_selector = Map("su" -> 0, "mi" -> 1, "br" -> 2, "mc" -> 3)

    print("Creating a dataset object...")
    var start = System.nanoTime()

    val ds = Dataset(data, algorithm, sort_selector(sort), tutorial, verbose)

    val timeGenerateObject = System.nanoTime() - start
    println("finished.")
    println("Needed " + fns.format(timeGenerateObject) + ".")

    // println("DEBUG>>")
    // println("data = ")
    // // data.foreach(println(_))
    // println(data(1))
    // println("<<DEBUG")

    start = System.currentTimeMillis()
    
    val result = ds.select(threshold)

    val select_t = System.currentTimeMillis() - start

    // println("DEBUG>>")
    // println("Count the occurences of (attr_value, class_label)")
    // ds.ofc.foreach(x => println(x))
    // println("Count the occurences of attribute values")
    // ds.of.foreach(x => println(x))
    // println("Count the occurences of class labels")
    // ds.oc.foreach(x => println(x))
    // ds.displayStats(None)
    // println("The result of attribute sorting")
    // // val f = new DecimalFormat("0.00")
    // //   (0 until ds.rn.size).foreach {i =>
    // //     if(ds.nr(i) != HIDDEN)
    // //       print(ds.nr(i) + "(" + f.format(ds.msr(sl)(ds.nr(i))) + ") ")
    // //   }
    // // println
    // val f = new DecimalFormat("0.00")
    //   (0 until 30).foreach {i =>
    //     if(ds.nr(i) != HIDDEN)
    //       print(ds.nr(i) + "(" + f.format(ds.msr(sl)(ds.nr(i))) + ") ")
    //   }
    // println
    // println("<<DEBUG")

    println
    println(result.size + " features have been selected.")
    println
    print("Selected features are: ")
    println(result.mkString(" "))


    // for (attr <- result if attr!=HIDDEN) {
    //   print(f"$attr(${ds.msr(sort_selector(sort))(attr)}%.3f, ${ds.rn(attr) + 1}) ")
    // }
    // println

    if(out != "") {
      db.removeUnselectedAttrs(result)
      db.saveArffFile(out)
      println
      println("The modified dataset has been output to " + out + ".")
    }

    val log_file_name = if(algorithm == "lcc") {
      in + "-" + sort + "-" + threshold + "-lcc.log"
    }else{
      in + "-" + sort + "- cwc.log"
    }

    println
    println("Log has been output to " + log_file_name + ".")

    println
    println("Statistics")
    for(x <- 0 to 3) {
      println("Scores in　" + ds.slm(x) + ":")
      for (attr <- result if attr!=HIDDEN) {
        print(f"$attr(${ds.msr(x)(attr)}%.3f) ")
      }
      println
      println
    }

    if(log == "high" || log == "low") {
      val log_file = new OutputStreamWriter(new FileOutputStream(log_file_name), "utf-8")
      val sl = sort_selector(sort)

      log_file.write("# Parameters\n")
      log_file.write("## Input file = " + in + "\n")
      if(out != "") log_file.write("## Output file = " + out + "\n")
      log_file.write("## Algorithm = " + algorithm + "\n")
      log_file.write("## Sorting measure = " + sort + "\n")
      if(algorithm == "lcc") log_file.write("## Threshold = " + threshold + "\n")
      log_file.write("## Level of detail of log = " + log + "\n")

      log_file.write("\n# Run-time\n")
      log_file.write("## Creating a dataset object = " + fns.format(timeGenerateObject) + "\n")
      log_file.write("## Sorting features = " + fns.format(ds.timeSortFeatures) + "\n")
      log_file.write("## Sorting instances = " + fns.format(ds.timeSortInstances) + "\n")
      log_file.write("## Selecting features = " + fns.format(ds.timeSelectFeatures) + "\n")

      log_file.write("\n# " + result.size + "features selected.\n")
      log_file.write("# Selected features\n")
      log_file.write(result.mkString(""," ","\n"))

      // for (attr <- result if attr!=HIDDEN) {
      //   log_file.write(f"$attr(${ds.msr(sort_selector(sort))(attr)}%.4f, ${ds.rn(attr) + 1}) ")
      // }

      log_file.write("\n# Statistics\n")
      log_file.write("## Number of instances = " + db.numInstances + "\n")
      log_file.write("## Number of features = " + db.numAttrs + "\n")
      log_file.write("## H(C) = " + f.format(ds.hc) + "\n")
      log_file.write("## H(Entire) = " + f.format(ds.entropyEntire) + "\n")
      log_file.write("## H(Entire, C) = " + f.format(ds.entropyEntireLabel) + "\n")
      log_file.write("## I(Entire; C) = " + f.format(ds.miEntireLabel) + "\n")
      log_file.write("## H(Selected) = " + f.format(ds.entropySelected) + "\n")
      log_file.write("## H(Selected, C) = " + f.format(ds.entropySelectedLabel) + "\n")
      val mis = ds.entropySelected + ds.hc - ds.entropySelectedLabel
      log_file.write("## I(Selected; C) = " + f.format(mis) + "\n")
      log_file.write("## H(Selected | C) = " + f.format(ds.entropySelected - mis) + "\n")
      val muh = 2*mis/(ds.miEntireLabel + ds.entropySelected)
      val mug = mis/math.sqrt(ds.miEntireLabel * ds.entropySelected)
      log_file.write("## mu_H = " + f.format(muh) + "\n")
      log_file.write("## mu_G = " + f.format(mug) + "\n")

      if(log == "high") {
        log_file.write("\n# Miscellany\n")

        log_file.write("\n## Selected features: feature name (score and rank in ")
        log_file.write(ds.slm(sort_selector(sort)) + ")\n")
        log_file.write((for (attr <- result if attr!=HIDDEN) yield {
          attr+"("+f.format(ds.msr(sl)(attr))+","+(ds.rn(attr)+1)+")"}).mkString(","))

        for(x <- (0 to 3) if x != sl) {
          log_file.write("\n\n## Selected features: feature name (score in ")
          log_file.write(ds.slm(x) + ")\n")
          log_file.write((for (attr <- result if attr!=HIDDEN) yield {
            attr + "(" + f.format(ds.msr(x)(attr)) + ")"}).mkString("",",","\n"))
        }

        log_file.write("\n## Entropy of features\n")
        ds.fs.foreach {x => log_file.write(x + ":" + f.format(ds.hf(x)) + " ")}
        log_file.write("\n\n")
        log_file.write("\n## Symmetric uncertainty between features and class\n")
        ds.fs.foreach {x => log_file.write(x + ":" + f.format(ds.suf(x)) + " ")}
        log_file.write("\n\n")
        log_file.write("\n## Mutual information between features and class\n")
        ds.fs.foreach {x => log_file.write(x + ":" + f.format(ds.mif(x)) + " ")}
        log_file.write("\n\n")
        log_file.write("\n## Bayesian risk between features and class\n")
        ds.fs.foreach {x => log_file.write(x + ":" + f.format(ds.brf(x)) + " ")}
        log_file.write("\n\n")
        log_file.write("\n## Matthew's correlation coefficient between features and class\n")
        ds.fs.foreach {x => log_file.write(x + ":" + f.format(ds.mcf(x)) + " ")}
        log_file.write("\n")
      }
      log_file.close()
    }
  }
}


