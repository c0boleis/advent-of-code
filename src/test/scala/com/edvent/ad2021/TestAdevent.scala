package com.edvent.ad2021

import org.scalatest.freespec.AsyncFreeSpec

import java.time
import java.time.Instant
import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, Map}
import scala.concurrent.ExecutionContext.global
import scala.concurrent.{Await, Future}
import scala.io.Source
import scala.util.control.Breaks._
import scala.concurrent.duration._

class TestAdevent extends AsyncFreeSpec {

  val numberOfDay = 256

  "advent 2021" - {
    "day 1 part 1.1" in {
      val filename = "src/main/resources/numbers_1.txt"
      val listTest = Source.fromFile(filename).getLines.map(line => line.toInt)
      var test = 0
      var old = Int.MaxValue;
      for (value <- listTest) {
        //        println(value)
        if (value > old) {
          test = test + 1
        }
        old = value
      }
      assert(test == 1715)
    }
    "day 1 part 1.2" in {
      val filename = "src/main/resources/numbers_1.txt"
      var test = 0
      val listTest = Source.fromFile(filename).getLines.foldLeft(Int.MaxValue.toString)((l1: String, l2: String) => {
        val tmp1 = l1.toInt
        val tmp2 = l2.toInt
        if (tmp2 > tmp1) {
          test = test + 1
        }
        l2
      })
      assert(test == 1715)
    }
    "day 1 part 2" in {
      val filename = "src/main/resources/numbers_1.txt"
      val listTest = Source.fromFile(filename).getLines.map(line => line.toInt).toList
      var test = 0
      var old = Int.MaxValue;
      for (te <- 2 to (listTest.length - 1)) {
        var value = listTest(te - 2) + listTest(te - 1) + listTest(te)
        if (value > old) {
          test = test + 1
        }
        old = value
      }
      assert(test == 1739)
    }
    "day 2 part 1" in {
      val filename = "src/main/resources/numbers_2.txt"
      val listTest = Source.fromFile(filename).getLines.map(line => Move(line)).toList
      var test = 0
      var depth = 0;
      var dir = 0;
      for (value <- listTest) {
        value.moveType match {
          case 1 => dir = dir + value.value
          case 2 => depth = depth + value.dir * value.value
          case 3 => depth = depth + value.dir * value.value
        }
      }
      assert(dir * depth == 1499229)
    }
    "day 2 part 2" in {
      val filename = "src/main/resources/numbers_2.txt"
      val listTest = Source.fromFile(filename).getLines.map(line => Move(line)).toList
      var test = 0
      var aim = 0;
      var depth = 0;
      var dir = 0;
      for (value <- listTest) {
        value.moveType match {
          case 1 => {
            dir = dir + value.value
            depth = depth + value.value * aim
          }
          case 2 => aim = aim + value.dir * value.value
          case 3 => aim = aim + value.dir * value.value
        }
      }
      assert(dir * depth == 1340836560)
    }
    "day 3 part 1" in {
      val filename = "src/main/resources/numbers_3.txt"
      val lines = Source.fromFile(filename).getLines().toList
      val res = lines.foldLeft(ArrayBuffer.fill(12)(0))((acc: ArrayBuffer[Int], y: String) => {
        val tabChar = y.toCharArray
        for (indexX <- 0 to (tabChar.length - 1)) {
          if (tabChar(indexX) == '1') {
            acc(indexX) = acc(indexX) + 1
          }
        }
        acc
      })
      println(res)
      var gammaRate = ""
      var epsilonRate = ""
      for (k <- 0 to (res.length - 1)) {
        if (res(k) > 500) {
          gammaRate = s"${gammaRate}1"
          epsilonRate = s"${epsilonRate}0"
        } else {
          gammaRate = s"${gammaRate}0"
          epsilonRate = s"${epsilonRate}1"
        }
      }

      val gammaInt = Integer.parseInt(gammaRate, 2)
      val epsilonInt = Integer.parseInt(epsilonRate, 2)

      assert(gammaRate == "011101111100")
      assert(epsilonRate == "100010000011")
      assert(gammaInt * epsilonInt == 4174964)
    }
    "day 3 part 2" in {
      val filename = "src/main/resources/numbers_3.txt"
      val lines = Source.fromFile(filename).getLines().toList

      val res = lines.foldLeft(ArrayBuffer.fill(12)(0))((acc: ArrayBuffer[Int], y: String) => {
        val tabChar = y.toCharArray
        for (indexX <- 0 to (tabChar.length - 1)) {
          if (tabChar(indexX) == '1') {
            acc(indexX) = acc(indexX) + 1
          }
        }
        acc
      })
      /*
       * calcul oxygen generator rating
       */
      var oxygenGeneratorRating: Option[Int] = None
      var indexBit = 0
      var tmpLines = lines
      //      println("START SEARCH oxygen generator rating: "+tmpLines.size)
      while (oxygenGeneratorRating.isEmpty) {
        //        println("SEARCH oxygen generator rating: "+tmpLines.size)
        val tmp = reduceListAxugenGeneratorRating(tmpLines, indexBit)
        if (tmp._1.size == tmp._2.size) {
          //          println("oxygen generator rating: "+tmp._1.size)
          if (tmp._1(0).charAt(indexBit) == '1') {
            oxygenGeneratorRating = Some(Integer.parseInt(tmp._1(0), 2))
          } else {
            oxygenGeneratorRating = Some(Integer.parseInt(tmp._2(0), 2))
          }
        } else if (tmp._1.size > tmp._2.size) {
          tmpLines = tmp._1
        } else {
          tmpLines = tmp._2
        }
        indexBit = indexBit + 1
      }
      println("oxygen generator rating: " + oxygenGeneratorRating.get)
      /*
       * calcul CO2 scrubber rating
       */
      var co2ScrubberRating: Option[Int] = None
      indexBit = 0
      tmpLines = lines
      while (co2ScrubberRating.isEmpty) {
        //        println("SEARCH oxygen generator rating: "+tmpLines.size)
        val tmp = reduceListAxugenGeneratorRating(tmpLines, indexBit)
        if (tmp._1.size == tmp._2.size) {
          //          println("oxygen generator rating: "+tmp._1.size)
          if (tmp._1(0).charAt(indexBit) == '0') {
            co2ScrubberRating = Some(Integer.parseInt(tmp._1(0), 2))
          } else {
            co2ScrubberRating = Some(Integer.parseInt(tmp._2(0), 2))
          }
        } else if (tmp._1.size < tmp._2.size) {
          tmpLines = tmp._1
        } else {
          tmpLines = tmp._2
        }
        indexBit = indexBit + 1
      }
      println("CO2 scrubber rating: " + co2ScrubberRating.get)

      var gammaRate = ""
      var epsilonRate = ""
      for (k <- 0 to (res.length - 1)) {
        if (res(k) > 500) {
          gammaRate = s"${gammaRate}1"
          epsilonRate = s"${epsilonRate}0"
        } else {
          gammaRate = s"${gammaRate}0"
          epsilonRate = s"${epsilonRate}1"
        }
      }

      val gammaInt = Integer.parseInt(gammaRate, 2)
      val epsilonInt = Integer.parseInt(epsilonRate, 2)

      assert(gammaRate == "011101111100")
      assert(epsilonRate == "100010000011")
      assert(gammaInt * epsilonInt == 4174964)
      assert(oxygenGeneratorRating.get * co2ScrubberRating.get == 4474944)
    }
    "day 4 part 1" in {
      val filename = "src/main/resources/numbers_4.txt"
      val lines = Source.fromFile(filename).getLines().toList
      val numberList = lines(0).split(",").toList.map(num => num.toInt)
      var listGrid = ArrayBuffer[Grid]();
      for (index <- 2 to 600 by 6) {

        val grid = Grid(ArrayBuffer(
          ArrayBuffer[(Int, Boolean)](lines(index).trim.split("\\s+").toList.map(num => (num.toInt, false)): _*),
          ArrayBuffer[(Int, Boolean)](lines(index + 1).trim.split("\\s+").toList.map(num => (num.toInt, false)): _*),
          ArrayBuffer[(Int, Boolean)](lines(index + 2).trim.split("\\s+").toList.map(num => (num.toInt, false)): _*),
          ArrayBuffer[(Int, Boolean)](lines(index + 3).trim.split("\\s+").toList.map(num => (num.toInt, false)): _*),
          ArrayBuffer[(Int, Boolean)](lines(index + 4).trim.split("\\s+").toList.map(num => (num.toInt, false)): _*)))
        listGrid.append(grid)
      }
      var result = -1
      breakable {
        for (number <- numberList) {
          for (grid <- listGrid) {
            result = grid.addNumber(number)
            if (result >= 0) {
              println(s"Grid WIN($number) sum($result): \n" + grid.toString)
              result = result * number
              //check if all win
                break
            }
          }
        }
      }
      assert(result == 35711)
    }
    "day 4 part 2" in {
      val filename = "src/main/resources/numbers_4.txt"
      val lines = Source.fromFile(filename).getLines().toList
      val numberList = lines(0).split(",").toList.map(num => num.toInt)
      var listGrid = ArrayBuffer[Grid]();
      for (index <- 2 to 600 by 6) {
        val grid = Grid(ArrayBuffer(
          ArrayBuffer[(Int, Boolean)](lines(index).trim.split("\\s+").toList.map(num => (num.toInt, false)): _*),
          ArrayBuffer[(Int, Boolean)](lines(index + 1).trim.split("\\s+").toList.map(num => (num.toInt, false)): _*),
          ArrayBuffer[(Int, Boolean)](lines(index + 2).trim.split("\\s+").toList.map(num => (num.toInt, false)): _*),
          ArrayBuffer[(Int, Boolean)](lines(index + 3).trim.split("\\s+").toList.map(num => (num.toInt, false)): _*),
          ArrayBuffer[(Int, Boolean)](lines(index + 4).trim.split("\\s+").toList.map(num => (num.toInt, false)): _*)))
        listGrid.append(grid)
      }
      var result = -1
      breakable {
        for (number <- numberList) {
          for (grid <- listGrid) {
            result = grid.addNumber(number)
            if (result >= 0) {
              println(s"Grid WIN($number) sum($result): \n" + grid.toString)
              result = result * number
              //check if all win
              if(listGrid.foldLeft(true)((acc,gridTmp) =>{if(gridTmp.win()<0 || !acc){false}else{acc}})){
                break
              }

            }
          }
        }
      }
      assert(result == 5586)
    }
    "day 5 part 1" in {
      val filename = "src/main/resources/numbers_5.txt"
      val lines = Source.fromFile(filename).getLines().toList.map(WindVect(_))
      val maxX = lines.foldLeft(0){(acc,value) =>{
          Math.max(acc,Math.max(value.start._2,value.end._2))
      }}
      val maxY = lines.foldLeft(0){(acc,value) =>{
        Math.max(acc,Math.max(value.start._1,value.end._1))
      }}
      //create Map
      var grid: Map[(Int,Int),Int] = Map()
      val tmpX = (1 to maxX).toList
      val tmpY = (1 to maxY).toList

      //init grid
      for(x <- 0 to maxX){
        for(y <- 0 to maxY){
          grid.put((x,y),0)
        }
      }

      lines.map(obj =>obj.processGrid(grid))

//      //print result
//      for(x <- 0 to maxX){
//        for(y <- 0 to maxY){
//          print(s"${grid(x,y)}-")
//        }
//        print("\n")
//      }

      val res = grid.foldLeft(0){(acc,value) =>{
        if(value._2>=2){
          acc+1
        }else{
          acc
        }
      }}
      assert(res == 6564)
    }
    "day 5 part 2" in {
      val filename = "src/main/resources/numbers_5.txt"
      val lines = Source.fromFile(filename).getLines().toList.map(WindVect(_))
      val maxX = lines.foldLeft(0) { (acc, value) => {
        Math.max(acc, Math.max(value.start._2, value.end._2))
      }
      }
      val maxY = lines.foldLeft(0) { (acc, value) => {
        Math.max(acc, Math.max(value.start._1, value.end._1))
      }
      }
      //create Map
      var grid: Map[(Int, Int), Int] = Map()
      val tmpX = (1 to maxX).toList
      val tmpY = (1 to maxY).toList

      //init grid
      for (x <- 0 to maxX) {
        for (y <- 0 to maxY) {
          grid.put((x, y), 0)
        }
      }

      lines.map(obj => obj.processGridDiag(grid))

      //print result
      for(x <- 0 to maxX){
        for(y <- 0 to maxY){
          print(s"${grid(x,y)}-")
        }
        print("\n")
      }

      val res = grid.foldLeft(0) { (acc, value) => {
        if (value._2 >= 2) {
          acc + 1
        } else {
          acc
        }
      }
      }
      assert(res == 19172)
    }
    "day 6 part 1" in {
      val filename = "src/main/resources/numbers_6.txt"
      val initLine = Source.fromFile(filename).getLines().next()
      val intNumbers = initLine.split(",").toList.map(_.toInt)
      var listTmp = intNumbers
      for(index <- 1 to 80){
        //decrement
        var count6 = 0
        listTmp = listTmp.map(_-1).map(value => if(value<0){count6=count6+1;6}else{value})
        listTmp = List.concat(listTmp,List.fill(count6)(8))
      }

      assert(listTmp.size == 360268)
    }
    "day 6 part 2" in {
      val filename = "src/main/resources/numbers_6.txt"
      val initLine = Source.fromFile(filename).getLines().next()

      //    Await.result(p2(2),30.minutes)
//      val references = Await.result(Future.sequence((1 to 5).map(p2)),10.minutes)
      val references = (1 to 5).map(p21)

      val intNumbers = initLine.split(",").toList.map(_.toInt)
      val res = intNumbers.map(value =>{references(value-1)}).sum
      assert(res == 1632146183902l)
    }
    "day 7 part 1" in {
      val filename = "src/main/resources/numbers_7.txt"
      val initLine = Source.fromFile(filename).getLines().next()
      val intNumbers = initLine.split(",").toList.map(_.toInt).sortWith(_>_)
      val max = intNumbers.max
      val min = intNumbers.min
      val res = (min to max).map(value => intNumbers.map(dist => Math.abs(dist-value)).sum).min
      assert(res == 340056)
    }
    "day 7 part 2" in {
      val filename = "src/main/resources/numbers_7.txt"
      val initLine = Source.fromFile(filename).getLines().next()
      val intNumbers = initLine.split(",").toList.map(_.toInt).sortWith(_>_)
      val max = intNumbers.max
      val min = intNumbers.min
      val res = (min to max).map(value => intNumbers.map(dist => {
        (0 to Math.abs(dist-value)).foldLeft(0)((acc,valueArth)=>{
          acc+valueArth
        })
      }).sum).min
      assert(res == 96592275)
    }
    "day 8 part 1" in {
      val filename = "src/main/resources/numbers_8.txt"
      val initLine = Source.fromFile(filename).getLines().toList
      val intNumbers1 = initLine.map(line =>line.split("\\|")(1).trim)
      val intNumbers2 = intNumbers1.flatMap(line => line.split("\\s+").toList)
      val intNumbers = intNumbers2.map(charsToDigitSize)
      val res = intNumbers.fold(0)((acc,value)=>{
        if(value<0){
          acc
        }else{
          acc+1
        }
      })
      assert(res == 543)
    }
    "day 8 part test" in {
      val filename = "src/main/resources/numbers_8-test.txt"
      val initLine = Source.fromFile(filename).getLines().toList.map(charOrganize)
      for(i1 <- 0 to 9){
        for(i2 <- 0 to 9){
          println(s"$i1 * $i2  => ${reduceCode(initLine(i1),initLine(i2))}")
        }
        println(s"############")
      }
      assert(true)
    }
    "day 8 part 2" in {
      val filename = "src/main/resources/numbers_8.txt"
      val initLine = Source.fromFile(filename).getLines().toList
      val intNumbers1 = initLine.map(line =>(line.split("\\|")(0).trim,line.split("\\|")(1).trim))
      val intNumbers2 = intNumbers1.map(line => (line._1.split("\\s+").toList,line._2.split("\\s+").toList))
      val intNumbers = intNumbers2.map(value => charsToDigitSizeDecoder(
        value._1.map(value => value.trim.toCharArray.sortWith(_<_).foldLeft("")((acc,value)=> acc+value)),
        value._2.map(value => value.trim.toCharArray.sortWith(_<_).foldLeft("")((acc,value)=> acc+value))))
      val res = intNumbers.sum
      assert(res == 994266)
    }
  }

  def charsToDigitSizeDecoder(decoder: List[String],code: List[String]): Int = {
    val decoderList = decode(decoder)
    var codeOut = "";
    for(text <- code){
      val k = decoderList.indexOf(text)
      if(k<0){
        println("error index not found: "+text)
        decoder.map(value => print(value+" "))
        println("")
      }
      codeOut = codeOut + k.toString
    }
    if(codeOut.isBlank){
      -1
    }else{
      println(codeOut)
      codeOut.toInt
    }
  }


  def decode(decoder: List[String]): List[String] = {
    var listTmp = ArrayBuffer.fill(10)("")
    // step 0 search 1, 4, 7 & 8
    var list5 = mutable.Map[String,List[Int]]()
    var list6 = mutable.Map[String,List[Int]]()
    for(text <- decoder){
      val k = text.length
      k match {
        case 2 => listTmp(1) = text
        case 4 => listTmp(4) = text
        case 3 => listTmp(7) = text
        case 7 => listTmp(8) = text
        case 5 => list5.put(text,charOrganize(text))
        case 6 => list6.put(text,charOrganize(text))
        case _ => throw new RuntimeException(s"size note definie: $text")
      }
    }
    assert(list5.size == 3)
    assert(list6.size == 3)
    // search 3
    val org1 = charOrganize(listTmp(1))
    val org4 = charOrganize(listTmp(4))
    listTmp(3) = list5.foldLeft("")((acc,value) =>{
      val t = reduceCode(value._2,org1)
      if(t==2){
        value._1
      }else{
        acc
      }
    })
    assert(!list5.remove(listTmp(3)).isEmpty)
    listTmp(3)

    //search 2
    list5.foreach(value =>{
      if(reduceCode(value._2,org4)==2){
        listTmp(2) = value._1
      }
    })
    assert(!list5.remove(listTmp(2)).isEmpty)

    //search 5
    listTmp(5) = list5.keysIterator.next()

    //search 6
    list6.foreach(value =>{
      if(reduceCode(value._2,org1)==1){
        listTmp(6) = value._1
      }
    })
    list6.remove(listTmp(6))
    assert(list6.size == 2)
    //search 9
    list6.foreach(value =>{
      val t = reduceCode(value._2,org4)
      if(t==4){
        listTmp(9) = value._1
      }
    })
    assert(!list6.remove(listTmp(9)).isEmpty)

    //search 0
    listTmp(0) = list6.keysIterator.next()
    listTmp.toList
  }

  def charOrganize(text: String): List[Int] ={
    val out = ArrayBuffer.fill(7)(0)
    for(charT <- text){
      charT match {
        case 'a' => out(0) = 1
        case 'b' => out(1) = 1
        case 'c' => out(2) = 1
        case 'd' => out(3) = 1
        case 'e' => out(4) = 1
        case 'f' => out(5) = 1
        case 'g' => out(6) = 1
        case _ => throw  new RuntimeException(s"error char unknown: $charT")
      }
    }
    out.toList
  }

  def reduceCode(l1: List[Int],l2: List[Int]): Int ={
    (0 to 6).map(index => l1(index)*l2(index)).sum
  }


  def charsToDigitSize(text: String): Int = {
    text.length match {
      case 2 => 1
      case 4 => 4
      case 3 => 7
      case 7 => 8
      case _ => {
        println(s"$text")
        -1
      }
    }
  }


  def p2(number: Int): Future[Long] ={
    Future {
      println(s"Process start: " + Thread.currentThread().getName)
      var mapAcc = ArrayBuffer.fill[Long](numberOfDay+1)(0)
      mapAcc(0)=1
      addAcc(mapAcc,number)
      for (index <- 1 to numberOfDay) {
        println(s"day: $index")
        val numToAdd = mapAcc(index)
        if(numToAdd>0) {
          addAcc8(mapAcc,index,numToAdd)
        }
      }
      //      mapAcc(numberOfDay) = 0
      mapAcc.sum
    }
  }

  def p21(number: Int): Long ={
    println(s"Process start: " + Thread.currentThread().getName)
    var mapAcc = ArrayBuffer.fill[Long](numberOfDay+1)(0)
    mapAcc(0)=1
    addAcc(mapAcc,number)
    for (index <- 1 to numberOfDay) {
//      println(s"day: $index")
      val numToAdd = mapAcc(index)
      if(numToAdd>0) {
        addAcc8(mapAcc,index,numToAdd)
      }
    }
    //      mapAcc(numberOfDay) = 0
    mapAcc.sum
  }


  def addAcc(buffer: ArrayBuffer[Long],n: Long): Unit ={
    for (index <- n+1 to numberOfDay by 7) {
      buffer(index.toInt) = 1
    }
  }

  def addAcc8(buffer: ArrayBuffer[Long],start: Int,numToAdd: Long): Unit ={
    for (index <- (start + 9) to numberOfDay by 7) {
      buffer(index) = buffer(index) + numToAdd
    }
  }


  def stringToIntList(line: String): List[Int] ={
    line.split(",").toList.map(num => num.toInt)
  }


  def reduceListAxugenGeneratorRating(listIn: List[String], bitIndex: Int): (List[String],List[String]) = {
    val out = listIn.foldLeft((new ArrayBuffer[String](),new ArrayBuffer[String]()))((acc: (ArrayBuffer[String],ArrayBuffer[String]), line: String)=>{
      if(line.charAt(bitIndex) == '1'){
        acc._1.append(line)
      }else{
        acc._2.append(line)
      }
      acc
    })
    (out._1.toList,out._2.toList)
  }

  object WindVect{

    def apply(line: String): WindVect = {
      val vect = line.trim.split("\\s+")
      val vectStart = vect(0).split(",")
      val vectEnd = vect(2).split(",")
      WindVect((vectStart(0).toInt,vectStart(1).toInt),(vectEnd(0).toInt,vectEnd(1).toInt))
    }
  }

  case class WindVect(start: (Int,Int), end: (Int,Int)){

    override def toString(): String = {
      s"${start._1},${start._2} -> ${end._1},${end._2}"
    }

    def isVertical():Boolean ={
      {start._1} =={end._1}
    }

    def isDiagonal():Boolean ={
      if(isHorizontal() || isVertical()){
        false
      }else{
        Math.abs(end._1-start._1)==Math.abs(end._2-start._2)
      }
    }

    def isHorizontal():Boolean ={
      {start._2} =={end._2}
    }

    def processGrid(grid: Map[(Int,Int),Int]): Unit ={
      if(isVertical()){
        val valY = start._1
        for(valX <- Math.min({start._2},{end._2}) to Math.max({start._2},{end._2})){
            val tmp = grid(valX,valY)+1
            grid.update((valX,valY),tmp)
        }
      }else if(isHorizontal()){
        val valX = start._2
        for(valY <- Math.min({start._1},{end._1}) to Math.max({start._1},{end._1})){
          val tmp = grid(valX,valY)+1
          grid.update((valX,valY),tmp)
        }
      }
      ()
    }

    def processGridDiag(grid: Map[(Int,Int),Int]): Unit ={
      if(isVertical()){
        val valY = start._1
        for(valX <- Math.min({start._2},{end._2}) to Math.max({start._2},{end._2})){
          val tmp = grid(valX,valY)+1
          grid.update((valX,valY),tmp)
        }
      }else if(isHorizontal()){
        val valX = start._2
        for(valY <- Math.min({start._1},{end._1}) to Math.max({start._1},{end._1})){
          val tmp = grid(valX,valY)+1
          grid.update((valX,valY),tmp)
        }
      }else if(isDiagonal()){
        val size = Math.abs(end._1-start._1)
        val dir1 = if(end._1>start._1){1}else{-1}
        val dir2 = if(end._2>start._2){1}else{-1}
        for(index <-0 to size){
          val valY= start._1+dir1*index
          val valX = start._2+dir2*index
          val tmp = grid(valX,valY)+1
          grid.update((valX,valY),tmp)
        }
      }
      ()
    }
  }

  case class Grid(rows : ArrayBuffer[ArrayBuffer[(Int,Boolean)]]){

    def addNumber(number: Int): Int ={
      for(row <- rows){
        for(index <-0 to 4){
          if(row(index)._1 == number){
            row(index) = (row(index)._1,true)
          }
        }
      }
      win()
    }
    def sum(): Int ={
      var sum = 0
      for(row <- rows){
        for(number <- row){
          if(!number._2){
            sum = sum +number._1
          }

        }
      }
      sum
    }

    def win(): Int ={
      var result = -1
      // cheks rows
      breakable{
        for(row <- rows){
          if(row.foldLeft(true)((acc,tmp)=>{if(!acc){false}else{tmp._2}})){
            result = sum()
            break
          }
        }
      }
      if(result>=0){
        result
      }else{
        //check columns
        breakable{
          for(index <-0 to 4){
            if(rows.foldLeft(true)((acc,tmp)=>{if(!acc){false}else{tmp(index)._2}})){
              result = sum()
              break
            }
          }
        }
        result
      }
    }

    override def toString(): String ={
      var out = "";
      for(row <- rows){
        for(number <-row){
          if(number._2){
            out = out+s"(${number._1}) "
          }else{
            out = out+s"${number._1} "
          }
        }
        out = out+"\n"
      }
      out
    }
  }

  object Move{
    def apply(line: String): Move = {
      val table = line.split(" ");
      val ty = table(0) match{
        case "forward" => 1
        case "up" => 2
        case "down" => 3
        case _ => 4
      }
      val dir = table(0) match{
        case "forward" => 1
        case "up" => -1
        case "down" => 1
        case _ => 0
      }
      Move(ty,dir,table(1).toInt)
    }
  }

  case class Move(moveType: Int,dir: Int,value: Int){
    override def toString(): String = {
      val dir = moveType match {
        case 1 => "forward"
        case 2 => "up"
        case 3 => "down"
        case _ => "ERROR!!!"
      }
      s"$dir $value"
    }
  }

}
