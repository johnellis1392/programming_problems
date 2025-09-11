object Main {
  val tests = Array(
    ("Hello World", 5),
    ("   fly me   to   the moon  ", 4),
    ("luffy is still joyboy", 6),
  )

  def lengthOfLastWord(s: String): Int = s.trim().split(" +").last.length()

  def main(args: Array[String]): Unit = {
    println("Running...")
    tests foreach {
      case (s, o) =>
        val res = lengthOfLastWord(s)
        if (res == o) {
          println("Success")
        } else {
          println(s"Failure: ${res} != ${o}")
        }
    }
  }
}