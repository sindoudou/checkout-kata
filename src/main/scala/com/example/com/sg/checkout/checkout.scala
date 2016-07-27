package com.example.com.sg.checkout

trait CheckoutApp extends App {
  def printResult(value: Int) = print(value)

  val rulesMap = Map(
    "A" -> Map(1 -> 50, 3 -> 130),
    "B" -> Map(1 -> 30, 2 -> 45),
    "C" -> Map(1 -> 20),
    "D" -> Map(1 -> 15)
  )

  val checkout = new CheckoutCalculator(rulesMap)
  val price = checkout.calculate(args.toList)
  printResult(price)
}

class CheckoutCalculator(val rulesMap: Map[SKU, Map[Count, Price]]) {

  def calculate(skus: List[SKU]): Price = {
    val groupedSkus = skus.groupBy(identity).mapValues(_.size)
    groupedSkus.foldRight(0) {(sp, acc) =>
      acc + calculateBySku(sp._1, sp._2)
    }
  }

  private def rules(sku: SKU): Map[Count, Price] =
    rulesMap.getOrElse(sku, throw new IllegalArgumentException(s"Unknown sku $sku"))

  private def calculateBySku(sku: SKU, count: Count): Price = {
    def calculate(count: Count, skuRules: Seq[(Count, Price)], acc: Price): Price = {
      skuRules
        .headOption
        .map(r => calculate(count % r._1, skuRules.tail, acc + (count / r._1) * r._2))
        .getOrElse(acc)
    }
    calculate(count, rules(sku).toSeq.sortBy(_._1).reverse, 0)
  }
}
