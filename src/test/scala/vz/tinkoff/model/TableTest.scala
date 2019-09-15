package vz.tinkoff.model

import java.time.LocalDateTime

import org.scalatest._

class TableTest extends FlatSpec{

  "A Table" should "allow insertions" in {
    val table = Table(Nil)

    table.insert()
    table.insert()

    assert(table.records.size === 2)
  }

  it should "allow backdating insertions" in {
    val table = Table(Nil)
    val backdatingTimestamp = LocalDateTime.now().minusMinutes(1)

    table.insert()
    table.backdatingInsert(backdatingTimestamp)

    assert(table.records.size === 2)
  }

  it should "allow backdating insertions in first position" in {
    val table = Table(Nil)
    val backdatingTimestamp = LocalDateTime.now().minusMinutes(1)

    table.backdatingInsert(backdatingTimestamp)
    table.insert()

    assert(table.records.size === 2)
  }

  it should "throw IllegalArgumentException if backdating timestamp greater than max timestamp in table" in {
    val table = Table(Nil)
    val backdatingTimestamp = LocalDateTime.MAX

    table.insert()

    assertThrows[IllegalArgumentException] {
      table.backdatingInsert(backdatingTimestamp)
    }
  }

  it should "throw IllegalArgumentException if backdating timestamp in first position is from the future" in {
    val table = Table(Nil)
    val backdatingTimestamp = LocalDateTime.MAX

    assertThrows[IllegalArgumentException] {
      table.backdatingInsert(backdatingTimestamp)
    }
  }

  it should "generate records ids in increasing order (n + 1)" in {
    val table = Table(Nil)
    val backdatingTimestamp = LocalDateTime.now().minusMinutes(1)

    table.insert()
    table.backdatingInsert(backdatingTimestamp)
    table.insert()

    assert(table.records.map(record => record.id) === List(1, 2, 3))
  }

  it should "return an empty List of ids of backdating insertions if there are no records in the table" in {
    val table = Table(Nil)

    assert(table.getBackdatingIds === Nil)
  }

  it should "return an empty List of ids of backdating insertions if there are only one record in the table" in {
    val table = Table(Nil)

    table.insert()

    assert(table.getBackdatingIds === Nil)
  }

  it should "return an empty List of ids of backdating insertions if there are only one backdating record in the table" in {
    val table = Table(Nil)
    val backdatingTimestamp = LocalDateTime.now().minusMinutes(1)

    table.backdatingInsert(backdatingTimestamp)

    assert(table.getBackdatingIds === Nil)
  }

  it should "return an empty List of ids of backdating insertions if there are no backdating records in the table" in {
    val table = Table(Nil)

    table.insert()
    table.insert()
    table.insert()

    assert(table.getBackdatingIds === Nil)
  }

  it should "return a List of ids of backdating insertions if there are backdating records in the table" in {
    val table = Table(Nil)
    val backdatingTimestamp = LocalDateTime.now().minusMinutes(1)

    table.insert()
    table.backdatingInsert(backdatingTimestamp)
    table.insert()
    table.backdatingInsert(backdatingTimestamp)
    table.insert()

    assert(table.getBackdatingIds === List(2, 4))
  }
}
