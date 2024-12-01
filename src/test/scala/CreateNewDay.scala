import TestRunner.dayString

import java.io.{FileNotFoundException, InputStream}
import java.net.{HttpURLConnection, URL}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths, StandardCopyOption}
import java.time.{LocalDate, LocalTime}

object CreateNewDay {
  def main(args: Array[String]): Unit = {
    val currentDate = LocalDate.now()
    if (currentDate.getYear <= 2024 && currentDate.getMonthValue < 12) {
      println("🎄 It's not December 2024 yet.")
      return
    }
    val afterDecember: Boolean = currentDate.getYear > 2024

    val inputDirPath = Paths.get("input")
    if (!Files.exists(inputDirPath)) Files.createDirectory(inputDirPath)

    val maxDay = if (afterDecember) 25 else
      math.min(25, currentDate.getDayOfMonth - (if (LocalTime.now().getHour < 5) 1 else 0))
    val day = (1 to maxDay).find { day =>
      val filePath = Paths.get(s"input/${dayString(day)}.txt")
      !Files.exists(filePath)
    }

    day match {
      case Some(day) =>
        createDayScript(day)
        downloadInput(2024, day)
      case None =>
        println("👍 All input for December 2024 have been downloaded, or it's too early to download next input.")
    }
  }

  def downloadInput(year: Int, day: Int): Unit = {
    val filePath = Paths.get(s"input/${dayString(day)}.txt")
    if (!Files.exists(filePath)) {
      val url = new URL(s"https://adventofcode.com/$year/day/$day/input")

      val connection = url.openConnection().asInstanceOf[HttpURLConnection]
      connection.setRequestMethod("GET")
      val cookieFilePath = Paths.get(".cookie")
      if (!Files.exists(cookieFilePath)) {
        throw new FileNotFoundException("❌ Could not find .cookie file for session cookie.")
      }
      val sessionCookie = Files.readString(cookieFilePath).stripSuffix("\n")
      connection.setRequestProperty("Cookie", s"session=$sessionCookie")

      var in: InputStream = null
      try {
        in = connection.getInputStream
        Files.copy(in, filePath, StandardCopyOption.REPLACE_EXISTING)
        println(s"⏬ Downloaded input for day $day.")
      } catch {
        case e: Exception =>
          println(s"❌ Error during downloading the file for day $day: ${e.getMessage}")
      } finally {
        in.close()
        connection.disconnect()
      }
    } else {
      println(s"👍 Input file already exists for day $day")
    }
  }

  def createDayScript(day: Int): Unit = {
    val dayPath = Paths.get(s"src/main/scala/days/Day${dayString(day)}.scala")
    if (!Files.exists(dayPath)) {
      val templatePath = Paths.get("src/main/scala/utility/DayTemplate.scala")
      var fileContent = Files.readString(templatePath, StandardCharsets.UTF_8)
      fileContent = fileContent.replace("DayTemplate", s"Day${dayString(day)}")
        .replace("package utility", "package days\n\nimport utility.*")
      Files.writeString(dayPath, fileContent, StandardCharsets.UTF_8)
      println(s"📜 Created script for day $day. ($dayPath)")
    } else {
      println(s"👍 Script file already exists for day $day. ($dayPath)")
    }
  }
}
