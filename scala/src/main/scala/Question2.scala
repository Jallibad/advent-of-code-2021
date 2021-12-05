package adventofcode

import scala.io.Source

@main def question2() = {
    val measurements = Source.fromResource("question1-input.txt")
        .getLines
        .toList
        .map(_.toInt)
    val sums = 
    val increases = measurements.zip(measurements.drop(1)).map(_ < _).count(x => x)
    print(increases)
}
