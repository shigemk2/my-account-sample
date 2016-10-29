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

object TransactionId {
  var currentId = 0
  def apply(): TransactionId = {
    currentId = currentId + 1
    TransactionId(currentId.toString)
  }
}

case class TransactionId(id: String)

case class Transaction(transactionId: TransactionId, amount: Money)

case class AccountBalance(accountId: AccountId, amount: Money)
case class Deposit(transactionId: TransactionId, amount: Money)
case class QueryBalance()
case class Withdraw(transactionId: TransactionId, amount: Money)