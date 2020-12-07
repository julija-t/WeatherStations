import java.io.{File, PrintWriter}

import scala.xml.XML
import org.json.{XML => Jxml}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**Parses xml file, produces report for each weather station in Latvia for each measurement available at that station.
 *Exports reports in TSV format.
 *Exports all metadata about each station into JSON formatted file.
 *Combines all small JSON files into stations_latvia_meta.json
 */
object WeatherStationsLV extends App {

  /** Parsing xml, producing a HashMap with data for report */
  val xmlSource = "./src/main/resources/LV_meta.xml"
  val xml = XML.loadFile(xmlSource)
  val stationNodes = xml \\ "station"

  stationNodes.foreach { n =>
    val stationName = (n \\ "station_name").text
    val stationEuCode = (n \\ "station_european_code").text
    val measurementConfigurationNodes = n \\ "measurement_configuration"

    var reportTableList = new ListBuffer[mutable.LinkedHashMap[String, String]]

    measurementConfigurationNodes.foreach { p =>

      var reportTable = scala.collection.mutable.LinkedHashMap[String, String]()
      val componentName = (p \\ "component_name").text
      val componentCaption = (p \\ "component_caption").text
      val measurementUnit = (p \\ "measurement_unit").text
      val measurementTechniquePrinciple = (p \\ "measurement_technique_principle").text

      reportTable += ("component_name" -> componentName)
      reportTable += ("component_caption" -> componentCaption)
      reportTable += ("measurement_unit" -> measurementUnit)
      reportTable += ("measurement_technique_principle" -> measurementTechniquePrinciple)

      val statisticNodes = p \\ "statistics"

      statisticNodes.foreach { q =>

        val year = (q \\ "@Year").text
        val statisticAverageGroupNodes = q \\ "statistics_average_group"
        val statisticsAverageGroup = (statisticAverageGroupNodes.head \\ "@value").text
        val p50Value = ((statisticAverageGroupNodes.head \\ "statistic_result" filter {
          _ \\ "statistic_shortname" exists (_.text == "P50")
        }) \ "statistic_value").text
        val meanValue = ((statisticAverageGroupNodes.head \\ "statistic_result" filter {
          _ \\ "statistic_shortname" exists (_.text == "Mean")
        }) \ "statistic_value").text

        reportTable += ("statistics_average_group" -> statisticsAverageGroup)
        if (!meanValue.isEmpty) reportTable += (s"year_${year}_mean" -> meanValue)
        if (!p50Value.isEmpty) reportTable += (s"year_${year}_median(P50)" -> p50Value)
      }

      reportTableList += reportTable
    }

    /** Printing report to file in tsv format */
    new File("./src/main/resources/reports").mkdirs()
    val reportPath = s"./src/main/resources/reports/${stationName}_${stationEuCode}_yearly.tsv"
    val reportFile = new PrintWriter(new File(reportPath))
    reportTableList.foreach(el => reportFile.write(s"${el.keys.mkString("\t")} \n ${el.values.mkString("\t")} \n\n"))
    reportFile.close()

    /** Printing report to console */
    println(Console.BOLD + s"Station: $stationName ($stationEuCode)" + Console.RESET)
    reportTableList.foreach { l =>
      println()
      for ((k, v) <- l) println(s"$k:${" " * (40 - k.length)}$v")
    }
    println()

    /**Saving each station's metadata as json */
    new File("./src/main/resources/meta").mkdirs()
    val metaDestPath = s"./src/main/resources/meta/${stationName}_${stationEuCode}_meta.json"
    val stationInfo = (n \ "station_info").toString()

    val myJson = Jxml.toJSONObject(stationInfo)
    val metaFile = new PrintWriter(new File(metaDestPath))
    val prettyJson = ujson.read(myJson.toString)
    ujson.writeTo(prettyJson, metaFile, indent = 4)
    metaFile.close()
  }

  /**Combining small jsons into one big json file */
  def getListOfFiles(dir: String):List[File] = {
    val d = new File(dir)
    if (d.exists && d.isDirectory) {
      d.listFiles.filter(_.isFile).toList
    } else {
      List[File]()
    }}

  val listOfFiles = getListOfFiles("./src/main/resources/meta")
  val ujsonArr = ujson.Arr()

  listOfFiles.foreach {n =>
    val dataJson = ujson.read(n)
    ujsonArr.arr.append(dataJson)
  }

  val combinedJsonPath = s"./src/main/resources/stations_latvia_meta.json"
  val combinedJsonFile = new PrintWriter(new File(combinedJsonPath))
  val prettyJson = ujson.read(ujsonArr)
  ujson.writeTo(prettyJson, combinedJsonFile, indent = 4)
  combinedJsonFile.close()

}
