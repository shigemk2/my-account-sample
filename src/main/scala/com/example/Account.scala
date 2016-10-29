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

object AccountId {
  var currentId = 0
  def apply(): AccountId = {
    currentId = currentId + 1
    AccountId(currentId.toString)
  }
}

case class AccountId(id: String)