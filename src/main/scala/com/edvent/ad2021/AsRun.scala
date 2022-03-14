package com.edvent.ad2021

import java.time
import java.time.Instant
import scala.collection.mutable.{ArrayBuffer,Map}
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext
import scala.io.Source
import scala.util.control.Breaks.{break, breakable}

object AsRun {

  implicit val ec = ExecutionContext.global


  var buffer = ArrayBuffer[Future[Int]]()

  val referencesFull = List(6,5,4,3,2,1,0)
  val references1 = List.concat(List(8,7),(0 to 300).map(value => referencesFull(value%7)))
  val mapRes = Map[(Int,Int),Int]()

  def main(args: Array[String]): Unit = {
    val i1 = Instant.now()
    val resBuffer = ArrayBuffer.fill(256)(0)
    val filename = "src/main/resources/numbers_6.txt"
    val initLine = Source.fromFile(filename).getLines().next()

    val references = Await.result(Future.sequence((0 to 8).map(p1)),10.minutes)

    val intNumbers = initLine.split(",").toList.map(_.toInt)
    val res = intNumbers.map(references).sum

    val i2 = Instant.now()
    val delta = time.Duration.between(i1, i2).toMillis / 1000f
    println(s"Resultat: $res in ${delta}s")
  }

  def r0(buffer : ArrayBuffer[Int]): Unit ={

  }

  def p1(number: Int): Future[Int] ={
    var listTmp = List(number)
    Future {
      println(s"Process start: " + Thread.currentThread().getName)
      for (index <- 1 to 80) {
        println(s"day: $index")
        //decrement
        var count6 = 0
        listTmp = listTmp.map(value => if (value == 0) {
          count6 = count6 + 1;
          6
        } else {
          value - 1
        })
        listTmp = List.concat(listTmp, List.fill(count6)(8))
      }
      listTmp.size
    }
  }

  def process(listIn: List[Int], startIndex: Int): Future[Int] = {
    if (listIn.size == 20) {
      println("coucou")
    }
    var listTmp = listIn
    val fs = ArrayBuffer[Future[Int]]()
    Future {
      println(s"Process start: " + Thread.currentThread().getName)
      var continueIndex = -1
      breakable {
        for (index <- 1 to 256) {
          println(s"day: $index")
          //decrement
          var count6 = 0
          listTmp = listTmp.map(value => if (value == 0) {
            count6 = count6 + 1;
            6
          } else {
            value - 1
          })
          listTmp = List.concat(listTmp, List.fill(count6)(8))
          if (listTmp.size > 300) {
            continueIndex = index + 1
            break
          }
        }
      }
      if (continueIndex > 0) {
        val res = for {
          res1 <- listTmp.grouped(20).map(value => process(value, continueIndex))
        } yield {
          res1
        }
        //val waitRes = Await.result(Future.sequence(res),10.minutes).sum
        println(s"Process end: " + Thread.currentThread().getName)
        0
      } else {
        println(s"Process end: " + Thread.currentThread().getName)
        listTmp.size
      }

    }
    //val res  = Await.result(Future.sequence(fs.toList),30.seconds)
    //Future.successful(listTmp.size)
  }

}
