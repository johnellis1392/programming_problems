package org.example.common

import okhttp3.OkHttpClient
import okhttp3.Request
import java.nio.file.FileSystem
import java.nio.file.FileSystems
import java.nio.file.Path
import java.nio.file.Paths
import kotlin.io.path.absolute
import kotlin.io.path.createParentDirectories
import kotlin.io.path.div
import kotlin.io.path.writeText

val inputDir = Paths.get("").absolute() / "input"

val sessionId = ""
val years = (2015..2025).map { it.toString() }
val days = (1..25).map { it.toString() }

fun inputPathFor(day: String, year: String): Path =
  inputDir / year / "day$day.input.txt"

fun fetchInputForDay(day: String, year: String) {
  val url = "https://adventofcode.com/$year/day/$day/input"
  val request = Request.Builder()
    .url(url)
    .header("Cookie", "session=$sessionId")
    .build()
  val input = OkHttpClient().newCall(request).execute().use { it.body.string() }

  val path = inputPathFor(day, year)
  path.createParentDirectories()
  path.writeText(input)
}

fun fetchInputForYear(year: String) {
  days.forEach { day ->
    fetchInputForDay(day, year)
  }
}

fun fetchAllInputs() {
  years.forEach { year ->
    fetchInputForYear(year)
  }
}

fun main() {
  fetchInputForDay("1", "2015")
  println("Done.")
}
