package com.example

import akka.actor._
import com.example._

import scala.collection.mutable.Map

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

class Account(accountId: AccountId) extends Actor {
  val transactions = Map.empty[TransactionId, Transaction]

  def receive = {
    case deposit: Deposit =>
      val transaction = Transaction(deposit.transactionId, deposit.amount)
      println(s"Deposit: $transaction")
      transactions += (deposit.transactionId -> transaction)
      AccountDriver.completedStep()
    case withdraw: Withdraw =>
      val transaction = Transaction(withdraw.transactionId, withdraw.amount.negative)
      println(s"Withdraw: $transaction")
      transactions += (withdraw.transactionId -> transaction)
      AccountDriver.completedStep()
    case query: QueryBalance =>
      sender ! calculateBalance()
      AccountDriver.completedStep()
  }

  def calculateBalance(): AccountBalance = {
    var amount = Money(0)

    transactions.values map { transaction =>
      amount = amount + transaction.amount
    }

    println(s"Balance: $amount")

    AccountBalance(accountId, amount)
  }
}