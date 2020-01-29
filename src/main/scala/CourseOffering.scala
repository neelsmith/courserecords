package edu.holycross.classics

// AN offering of a course in a specific term.
case class CourseOffering(
  id: String,
  termCode: String,
  termString: String,
  section: String,
  subject: String,
  courseNumber: Int,
  courseId: String,
  classAbbr: String,
  classification: String,
  cap: Int,
  enrollment: Int,
  instructor: String,
  campus: String
) {

  def forReal: Boolean = {
    enrollment > 0
  }
  def year: Int = {
    val termParts = termString.split(" ")
    termParts(1).toInt
  }
  def semester: String = {
    val termParts = termString.split(" ")
    termParts(0)
  }

  def termSortable: String = {
    semester match {
      case "Spring" => s"${year}-01 (Spring)"
      case "Fall" => s"${year}-02 (Fall)"
      case _ => "__UNRECOGNIZED VALUE FOR SEMESTER: " + semester
    }
  }
}

object CourseOffering {
  
  def apply(s: String) : CourseOffering = {
      def columns = s.split("#")
      CourseOffering(
        columns(0),
        columns(1),
        columns(2),
        columns(3),
        columns(4),
        columns(5).replaceAll("[A-Z\\-]","").trim.toInt,
        columns(6),
        columns(7),
        columns(8),
        columns(9).toInt,
        columns(10).toInt,
        columns(11),
        columns(12)
      )
  }
}
