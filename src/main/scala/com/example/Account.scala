package com.example

import akka.actor._
import com.example._

object AccountDriver extends CompletableApp(17) {
}

case class Money(value: Double) {
  def +(money: Money): Money = Money(value + money.value)
  def -(money: Money): Money = Money(value - money.value)
  def negative(): Money = Money(0 - value)
}
