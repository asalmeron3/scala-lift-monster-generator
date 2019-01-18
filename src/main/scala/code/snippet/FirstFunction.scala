package code.snippet 

import scala.xml.{NodeSeq, Text}
import net.liftweb.util._
import net.liftweb.common._
import java.util.Date
import code.lib._
import Helpers._
import scala.util.Random
import net.liftweb.http.SHtml
import net.liftweb.http.js.JsCmds.Alert
import scala.collection.mutable.ListBuffer
import net.liftweb.http.js.JE._
import net.liftweb.http.js.JsCmds._

object FirstFunction {
  case class Monster(name: String, level: Int, xp: Int, var id: String)
  case class CharVals(xpBudget: Int, charAdjustment: Int)

  val encounterBudget  = Map(
    "trivial" -> CharVals(40, 10),
    "low" -> CharVals(60, 15),
    "high" -> CharVals(80, 20),
    "severe" -> CharVals(120, 30),
    "extreme" -> CharVals(160, 40)
  )

  val partyLevel = 3;

  var monsterPool = ListBuffer(
    Monster("Goblin", 2, 30, ""), 
    Monster("Goblin Master", 4, 40, ""), 
    Monster("Young Dragon", 5, 50, ""), 
    Monster("Guard", 3, 15, ""),
    Monster("Wild Wolf", 2, 10, ""),
    Monster("Draugar", 4, 40, ""),
    Monster("Bear", 3, 30, "")
  )
  
    object creatureXpAndAdjusters {
    val partyLevelAdjuster: List[Int] = List(-4, -3, -2, -1, 0, 1, 2, 3, 4)
    val xp: List[Int] = List(10, 15, 20, 30, 40, 60, 80, 120, 160)
  }

  def totalXpBudget(encounterDifficulty: String, totalNumPlayers: Int): Int = {
    encounterBudget(encounterDifficulty).xpBudget + 
    ((totalNumPlayers - 4) * (encounterBudget(encounterDifficulty).charAdjustment))
  }

  def xpCost(creatureLevel: Int): Int = {
    val theAdjuster = creatureLevel - partyLevel
    val adjusterIndex = creatureXpAndAdjusters.partyLevelAdjuster.indexOf(theAdjuster)
    creatureXpAndAdjusters.xp(adjusterIndex)
  }

  def eligibleCreaturePool(xpBudgetLeft: Int, currentCreaturePool: ListBuffer[Monster]): ListBuffer[Monster] = {
    var creatureIndex = 0;
    if (xpBudgetLeft >= 160) {
      creatureIndex = 8
    } else if ( xpBudgetLeft >= 10 && xpBudgetLeft <= 160) {
      creatureIndex = creatureXpAndAdjusters.xp.indexWhere(_ > xpBudgetLeft) -1
    }
    
    if (creatureIndex >= 0) {
      val highestCreatureLevel = partyLevel + creatureXpAndAdjusters.partyLevelAdjuster(creatureIndex)
      currentCreaturePool.filter(eachMonster => eachMonster.level <= highestCreatureLevel && eachMonster.level >= partyLevel - 4)
    } else {
      ListBuffer[Monster]()
    }
  }

  def compileRandomCreatures(xpBudgetLeft: Int, currentCreaturePool: ListBuffer[Monster], randomCreatures: ListBuffer[Monster]): ListBuffer[Monster] = {
    if (currentCreaturePool.length > 0 && xpBudgetLeft >= 10) {
      var randNum = scala.util.Random
      var oneRandomCreature = currentCreaturePool(randNum.nextInt(currentCreaturePool.length))
      oneRandomCreature.id = oneRandomCreature.name.split("")(0).split(" ").mkString("")
      val newRandomPool = ListBuffer[Monster]()
      newRandomPool += oneRandomCreature
      val xpSpent = xpCost(oneRandomCreature.level)
      val newCreaturePool = eligibleCreaturePool(xpBudgetLeft - xpSpent, currentCreaturePool)
      compileRandomCreatures(xpBudgetLeft - xpSpent, newCreaturePool, newRandomPool)
    } else {
      randomCreatures
    }
  }
  
  def render = "button [onclick]" #>    
    SHtml.ajaxCall(ValById("xpToSpend"), xp => {
      val xpInt = xp.toInt
      if (xpInt >= 10 && xpInt <=160) {
        val theMonster = compileRandomCreatures(xpInt, monsterPool, ListBuffer[Monster]())
        val theText = "Monster: %s, Level: %d, XP: %d".format(theMonster(0).name, theMonster(0).level, theMonster(0).xp)
        JsRaw("$('#monsterText').text('" + theText + "')")
      } else {
        JsRaw("$('#monsterText').text('Please enter an xp between 10 and 160')")
      }
    })
}