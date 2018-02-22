val romanNumerals = Map("I" -> 1, "V" -> 5, "X" -> 10)
val CapitalOfCountry = Map("US" -> "Washington", "Switzerland" -> "Bern")

val fruit = List("apple","pear","orange","pineapple")
fruit.sorted
fruit
fruit.sortWith(_.length < _.length)
fruit.groupBy(_.head)