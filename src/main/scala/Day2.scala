/**
  * https://adventofcode.com/2017/day/2
  */

object Day2 {
  def main(args: Array[String]) {
    val lines: Iterator[String] = scala.io.Source.fromFile("src/main/resources/day2.txt").getLines()
    val matrix: Array[Array[Int]] = lines.map(_.split("\t").map(_.toInt)).toArray
    println("Checksum = " + checksum(matrix))
    println("Matrix division = " + matrixDivision(matrix))
  }

  /**
    * The spreadsheet consists of rows of apparently-random numbers. To make sure the recovery process is on the right track,
    * they need you to calculate the spreadsheet's checksum. For each row, determine the difference between the largest value
    * and the smallest value; the checksum is the sum of all of these differences.
    *
    * For example, given the following spreadsheet:
    * 5 1 9 5
    * 7 5 3
    * 2 4 6 8
    * The first row's largest and smallest values are 9 and 1, and their difference is 8.
    * The second row's largest and smallest values are 7 and 3, and their difference is 4.
    * The third row's difference is 6.
    * In this example, the spreadsheet's checksum would be 8 + 4 + 6 = 18.
    */
  def checksum(matrix: Array[Array[Int]]): Int = {
    matrix
      .map(row => row.max - row.min)
      .sum
  }

  /**
    * Based on what we're seeing, it looks like all the User wanted is some information about the evenly divisible values in the spreadsheet.
    * Unfortunately, none of us are equipped for that kind of calculation - most of us specialize in bitwise operations."
    *
    * It sounds like the goal is to find the only two numbers in each row where one evenly divides the other - that is,
    * where the result of the division operation is a whole number. They would like you to find those numbers on each line,
    * divide them, and add up each line's result.
    *
    * For example, given the following spreadsheet:
    *
      * 5 9 2 8
      * 9 4 7 3
      * 3 8 6 5
    * In the first row, the only two numbers that evenly divide are 8 and 2; the result of this division is 4.
    * In the second row, the two numbers are 9 and 3; the result is 3.
    * In the third row, the result is 2.
    * In this example, the sum of the results would be 4 + 3 + 2 = 9.
  */
  def matrixDivision(matrix: Array[Array[Int]]) = {
    matrix
      .map(rowNumbers => rowNumbers.sorted.reverse)
      .map(sortedNumbers =>
        (for (
          no1 <- sortedNumbers;
          no2 <- sortedNumbers.tail
          if (no1 != no2 && no1.%(no2) == 0)
        )
        yield no1 / no2).head
      )
      .sum

  }
}
