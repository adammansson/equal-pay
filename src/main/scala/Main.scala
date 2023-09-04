import java.util.UUID

case class Transaction(amount: Double, from: String, to: String):
  override def toString: String = 
    s"$from -> $to: $amount"

case class Participant(name: String, balance: Double)

def getTransactions(xs: Vector[Participant]): Vector[Transaction] =
  xs match
    case Vector() => Vector()
    case _ =>
      val average = xs.foldLeft(0.0)((acc, p) => acc + p.balance) / xs.length
      val averageRemoved = xs.map(p => Participant(p.name, p.balance - average))
      getTransactionsRecursive(averageRemoved)

def getTransactionsRecursive(xs: Vector[Participant]): Vector[Transaction] =
  xs.sortBy(_.balance) match
    case Vector(first, last) => Vector(Transaction(last.balance, first.name, last.name))
    case first +: middle :+ last =>
      val difference = first.balance + last.balance
      if difference < 0 then
        Transaction(last.balance, first.name, last.name) +: getTransactionsRecursive(Participant(first.name, difference) +: middle)
      else
        Transaction(-first.balance, first.name, last.name) +: getTransactionsRecursive(middle :+ Participant(last.name, difference))
    case _ => Vector()

def participantsFromInput: Vector[Participant] =
  io.StdIn.readLine().split(" ").toVector match
    case Vector(name, balance) => 
      Participant(name, balance.toDouble) +: participantsFromInput
    case _ => Vector()

@main def run: Unit = 
  getTransactions(participantsFromInput).foreach(println)
