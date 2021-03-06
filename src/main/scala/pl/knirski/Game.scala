package pl.knirski

import akka.actor.ActorSystem
import cats._
import cats.data.State
import cats.effect.IO
import pl.knirski.Domain.GameScore
import pl.knirski.Protocol._

import scala.concurrent._
import scala.concurrent.duration._
import scala.language.postfixOps

object Protocol {

  sealed trait GameOps[A]
  case class Team1Scored(points: Int) extends GameOps[Unit]
  case class Team2Scored(points: Int) extends GameOps[Unit]
  case object AnnounceIntermediateResult extends GameOps[String]
  case object AnnounceFinalResult extends GameOps[String]
  case object Team1Cheated extends GameOps[IO[Unit]]
  case object ResetScore extends GameOps[Unit]

}

object Domain {

  case class GameScore(team1: Int, team2: Int)

  def team1Scored(score: GameScore, points: Int): GameScore = score.copy(team1 = score.team1 + points)

  def team2Scored(score: GameScore, points: Int): GameScore = score.copy(team2 = score.team2 + points)

  def team1Cheated(): GameScore = GameScore(100, 0)

  def announceIntermediateResult(score: GameScore): String = s"Result so far $score"

  def announceFinalResult(score: GameScore): String = {
    if (score.team1 > score.team2) {
      s"Team 1 won, result $score"
    } else if (score.team1 < score.team2) {
      s"Team 2 won, result $score"
    } else {
      s"It's a draw, result $score"
    }
  }

  val initialScore: GameScore = GameScore(0, 0)

}

object Effects {

  def reportCheating(score: GameScore): IO[Unit] =
    IO { println(s"reporting cheating to FBI, score $score was tampered with ") }

}

object GameInterpreter extends (GameOps ~> Lambda[A => State[GameScore, A]]) {
  /*_*/
  override def apply[A](fa: GameOps[A]): State[GameScore, A] = fa match {
    case Team1Scored(points) => State.modify { score => Domain.team1Scored(score, points) }
    case Team2Scored(points) => State.modify { score => Domain.team2Scored(score, points) }
    case Team1Cheated => State { _ =>
      val newScore = Domain.team1Cheated()
      (newScore, Effects.reportCheating(newScore))
    }
    case AnnounceIntermediateResult => State.inspect(Domain.announceIntermediateResult)
    case AnnounceFinalResult => State.inspect(Domain.announceFinalResult)
    case ResetScore => State.set(Domain.initialScore)
  }
  /*_*/
}

object Game extends App {

  implicit val executionContext = ExecutionContext.global
  implicit val actorSystem = ActorSystem("game")

  val gameActor: ActorInterpreter[GameOps, GameScore] =
    new ActorInterpreter[GameOps, GameScore](Domain.initialScore, GameInterpreter)

  val futureResult =
    for {
      _ <- gameActor(Team1Scored(1))
      _ <- gameActor(Team2Scored(2))
      intermediateAnnouncement1 <- gameActor(AnnounceIntermediateResult)
      _ <- gameActor(Team2Scored(1))
      cheatingEffect <- gameActor(Team1Cheated)
      intermediateAnnouncement2 <- gameActor(AnnounceIntermediateResult)
      _ <- gameActor(ResetScore)
      _ <- gameActor(Team2Scored(5))
      _ <- gameActor(Team1Scored(3))
      _ <- gameActor(Team2Scored(10))
      finalAnnouncement <- gameActor(AnnounceFinalResult)
    } yield {
      (List(intermediateAnnouncement1, intermediateAnnouncement2, finalAnnouncement), cheatingEffect)
    }

  val (announcements, cheatingEffect) = Await.result(futureResult, 1 second)

  announcements.foreach(println)
  cheatingEffect.unsafeRunSync()

  Await.ready(actorSystem.terminate(), 1 second)

}
