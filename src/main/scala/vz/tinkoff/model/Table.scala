package vz.tinkoff.model

import java.time.LocalDateTime

case class Table(var records: List[Record]) {

  def insert(): Unit = {
    val id        = generateId()
    val timestamp = LocalDateTime.now()

    records = records :+ Record(id, timestamp)
  }

  def backdatingInsert(timestamp: LocalDateTime): Unit = {
    val id           = generateId()
    val maxTimestamp = getMaxTimestamp(records.map(record => record.timestamp)).getOrElse(LocalDateTime.now())

    if (timestamp isAfter maxTimestamp) throw new IllegalArgumentException("Invalid Timestamp")

    records = records :+ Record(id, timestamp)
  }

  private def generateId() = records match {
    case Nil => 1
    case _   => records.last.id + 1
  }

  @scala.annotation.tailrec
  private def getMaxTimestamp(timestamps: List[LocalDateTime]): Option[LocalDateTime] = timestamps match {
    case Nil              => None
    case timestamp :: Nil => Some(timestamp)
    case timestampOne :: timestampTwo :: tail =>
      getMaxTimestamp((if (timestampOne isAfter timestampTwo) timestampOne else timestampTwo) :: tail)
  }

  def getBackdatingIds: List[Int] = records match {
    case Nil      => Nil
    case _ :: Nil => Nil
    case _ =>
      records
        .filter(
          record =>
            record.timestamp isBefore getMaxTimestamp(
              records.take(records.indexOf(record)).map(record => record.timestamp)).getOrElse(record.timestamp))
        .map(record => record.id)
  }
}
