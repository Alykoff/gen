/**
* "Hello World" Genetic Algoritm
* @autor: Alykoff Gali
* @date: 01.12.12
* @scalaVersion: 2.9.2
* @sbtVersion: 0.12.0
*/
import java.util.Random
import scala.annotation.tailrec

object Config {
	// размер популяции
	val POPULATION_SIZE = 2000
	// максимальное число иттераций
	val MAX_ITER = 950
	// процент выживания особей
	val ELIT_RATE = .80F
	// вероятность мутации
	val MUTATION_RATE = .85f
	// количество выживших особей
	val ELITE_SIZE: Int = (POPULATION_SIZE * ELIT_RATE).toInt
	// коэффициент мутации
	val MUTATION = Int.MaxValue * MUTATION_RATE
	// цель
	val TARGET = "Hello world!"
	// количество символов в цели
	val TARGET_SIZE = TARGET.length
	//
	val DELTA = 90

	val MIN_EL = 32

	val MAX_EL = MIN_EL + DELTA
}

/**
* @param value - значение строки.
* @param fitness - пригодность.
*/
class Persone(val value: String) {
	def calcFitness(
		count: Int = Config.TARGET_SIZE - 1, 
		acc: Int = 0): Int = {
			assert(value.length == Config.TARGET_SIZE)

			val newAcc = Helper.abs(this.value(count) - Config.TARGET(count)) + acc

			if (count < 0) 0
			else if (count == 0) newAcc
			else calcFitness(count - 1, newAcc)
	}

	val fitness = calcFitness()

	def fluctuation: Persone = {
		val pos = Rand.randPos
		val persVal = this.value
		val newPosValue = 
			if (new Random().nextInt < (Int.MaxValue / 2))
				persVal(pos) - 1
			else
				persVal(pos) + 1 
		val newValue = 
			Helper
				.setEl(persVal.toList, pos, newPosValue.asInstanceOf[Char])
				.mkString("")
		new Persone(newValue)
	} 

	def mutateX20: Persone = {
		this
			.mutate.mutate.mutate.mutate.mutate
			.mutate.mutate.mutate.mutate.mutate
			.mutate.mutate.mutate.mutate.mutate
			.mutate.mutate.mutate.mutate.mutate
			.mutate.mutate.mutate.mutate.mutate
			.mutate.mutate.mutate.mutate.mutate
			.mutate.mutate.mutate.mutate.mutate
			.mutate.mutate.mutate.mutate.mutate
			.mutate.mutate.mutate.mutate.mutate
			.mutate.mutate.mutate.mutate.mutate
			.mutate.mutate.mutate.mutate.mutate
	}

	def mutate: Persone = {
		val pos = Rand.randPos
		val persVal = this.value
		val newPosValue = (persVal(pos) + Config.DELTA) % Config.MAX_EL

		val newValue = 
			Helper
				.setEl(persVal.toList, pos, newPosValue.asInstanceOf[Char])
				.mkString("")
		new Persone(newValue)
	}
}

class Population(pers: List[Persone]) {
	private val _persones = 
		pers.sortWith(_.fitness < _.fitness)

	def persones = _persones

	@tailrec
	private def elitism(
		count: Int = 0, 
		acc: List[Persone] = List.empty): List[Persone] = {
			assert(count < this._persones.length)
			
			val newAcc = this._persones(count) :: acc
			val newCount = count + 1

			if (count >= Config.ELITE_SIZE - 1) newAcc
			else elitism(newCount, newAcc)
	}

	def mate: Population = {

		def crossing = {
			val pointerParent1 = Rand.randPointerParent
			val pointerParent2 = Rand.randPointerParent

			val delimer = Rand.randPos
			val newValue = 
				this.persones(pointerParent1).value.substring(0, delimer) + 
				this.persones(pointerParent2).value.substring(delimer, Config.TARGET_SIZE)
			
			if (new Random().nextInt < Config.MUTATION) 
				new Persone(newValue).mutateX20
			else 
				new Persone(newValue).fluctuation
		}

		val survivors = this.elitism()

		val deltaCrossing = (Config.ELITE_SIZE) to (Config.POPULATION_SIZE - 1)
		val generated = 
			(for (i <- deltaCrossing) yield crossing) 
				.toList
		new Population(survivors ++ generated)
	}

	

	def printBest = {
		println("Best: " + this.persones.head.value + " (" + this.persones.head.fitness + ")")
	}

	def printBest(prevBest: String) = {
		if (prevBest != this.persones.head.value) {
			println("Best: " + this.persones.head.value + " (" + this.persones.head.fitness + ")")
			this.persones.head.value
		} else {
			prevBest
		}
	}
}

object Population {
	def createPopulation: Population = {
		
		def createRandChar = 
			Rand.getElInDelta.asInstanceOf[Char]
		
		@tailrec
		def createRandString(
			count: Int = Config.TARGET_SIZE - 1, 
			acc: List[Char] = List.empty): String = {
				val newAcc = createRandChar :: acc

				if (count < 0) throw new Error()
				else if (count == 0) newAcc.foldLeft("")(_ + _)
				else createRandString(count - 1, newAcc)
		}

		@tailrec
		def newPopulation(
			count: Int = Config.POPULATION_SIZE, 
			acc: List[Persone] = List.empty): Population = {
				val newAcc = 
					new Persone(createRandString()) :: acc
				
				if (count < 0) new Population(acc)
				else if (count == 0) new Population(newAcc)
				else newPopulation(count - 1, newAcc)
		}

		newPopulation()
	}
}

object Rand {

	def getElInDelta: Int = {
		new Random().nextInt(Config.DELTA) + Config.MIN_EL
	}

	def randPos: Int = {
		new Random().nextInt(Config.TARGET_SIZE)
	}

	def randPointerParent: Int = {
		new Random().nextInt(Config.POPULATION_SIZE / 2)
	}	
}

object Helper {

	def abs(x: Int): Int =
		if (x < 0) -x
		else x

	def setEl(x: List[Char], pos: Int, value: Char): List[Char] = {
		assert(pos < x.length && pos >= 0)
		// println("value = " + value)
		x match {
			case xs :: xsl => 
				if (pos == 0) value :: xsl 
				else xs :: setEl(xsl, pos - 1, value)
			
			case Nil => 
				throw new Error("Bang!")
		}
	}
}

object Genetion {
	var currentBestVersion = ""

	def goLife(
		count: Int = 0, 
		population: Population = Population.createPopulation): Boolean = {
			currentBestVersion = population.printBest(currentBestVersion)
			if (population.persones.head.fitness == 0) true
			else if (count >= Config.MAX_ITER) false
			else goLife(count + 1, population.mate)
	}
}


object Main extends App {
	override def main(args: Array[String]): Unit = {
		Genetion.goLife()
	}
}