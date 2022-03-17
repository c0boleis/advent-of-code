package com.edvent.ad2021

import com.edvent.ad2021.AsRun.{addAcc, addAcc8}

import java.time
import java.time.Instant
import scala.collection.mutable.{ArrayBuffer, Map}
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext
import scala.io.Source
import scala.util.control.Breaks.{break, breakable}

object AsRun {

  implicit val ec = ExecutionContext.global

  val numberOfDay = 256

  def main(args: Array[String]): Unit = {
    val i1 = Instant.now()
    val filename = "src/main/resources/numbers_6.txt"
    val initLine = Source.fromFile(filename).getLines().next()

    //    Await.result(p2(2),30.minutes)
//    val references = Await.result(Future.sequence((1 to 5).map(p2)),10.minutes)
    val references = (1 to 5).map(p21)

    val intNumbers = initLine.split(",").toList.map(_.toInt)
    val res = intNumbers.map(value =>{references(value-1)}).sum

    val i2 = Instant.now()
    val delta = time.Duration.between(i1, i2).toMillis / 1000f
    println(s"Resultat: $res in ${delta}s")
  }

  def p2(number: Int): Future[Long] = {
    Future {
      println(s"Process start: " + Thread.currentThread().getName)
      var mapAcc = ArrayBuffer.fill[Long](numberOfDay + 1)(0)
      mapAcc(0) = 1
      addAcc(mapAcc, number)
      for (index <- 1 to numberOfDay) {
        println(s"day: $index")
        val numToAdd = mapAcc(index)
        if (numToAdd > 0) {
          addAcc8(mapAcc, index, numToAdd)
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
        println(s"day: $index")
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

}
