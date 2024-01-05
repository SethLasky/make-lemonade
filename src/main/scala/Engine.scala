import calico.*
import cats.effect.*
import cats.syntax.all.*
import fs2.concurrent.*
import fs2.dom.*
import calico.html.io.{*, given}
import calico.syntax.*
import cats.effect.std.{Queue, Random}

import calico.*
import calico.html.io.{*, given}
import calico.syntax.*
import calico.unsafe.given
import cats.effect.*
import cats.effect.std.Random
import cats.syntax.all.*
import fs2.*
import fs2.concurrent.*
import fs2.dom.*

import cats.effect.{IO, Ref, Resource}
import cats.implicits.*
import org.scalajs.dom.{Element, document}
import fs2.Stream
import fs2.concurrent.{Signal, SignallingRef}
import fs2.dom.HtmlElement

import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.util.{Random => SRandom}


import calico.*
import calico.frp.*
import calico.frp.given
import calico.html.io.*
import calico.html.io.given
import cats.data.*
import cats.effect.*
import cats.syntax.all.*
import fs2.concurrent.*
import fs2.dom.*
import org.scalajs.dom.KeyValue

import scala.collection.immutable.SortedMap
import cats.derived.auto.eq.*
import cats.derived.auto.eq.given


object Main extends IOWebApp {
    
  def getState(name: String, ref: SignallingRef[IO, Map[String, State]]) = 
    ref.get.map(_(name))  

  def getStateSignal(name: String, ref: SignallingRef[IO, Map[String, State]]) = 
    ref.map(_(name))    

  def updateStateEffectfully[X](name: String, ref: SignallingRef[IO, Map[String, State]], updateAndEffect: State => (State, IO[X])) =
    ref.flatModify{ states =>
      val(newState, effect) = updateAndEffect(states(name))
      (states + (name -> newState), effect)
    }

  def updateState(name: String, ref: SignallingRef[IO, Map[String, State]], update: State => State) ={
    ref.update(states => states + (name -> update(states(name))))
  }

  def autoBox(name: String, autoName: String, ref: SignallingRef[IO, Map[String, State]]) =
    getStateSignal(name, ref).map(_.autos(autoName)).changes.map {
      _.map { auto =>
        for {
          checkbox <- input(
            cls := "auto",
            typ := "checkbox",
            checked := auto.enabled,
            onInput --> (_.foreach { _ =>
              updateState(name, ref, state =>
                state.copy(autos = state.autos + (autoName -> state.autos(autoName).map { innerAuto =>
                  innerAuto.copy(enabled = !innerAuto.enabled)
                })))
            }))
        } yield checkbox
      }
    }



  override def render: Resource[IO, HtmlElement[IO]] = {
    def gameColumn(name: String, into: String, ref: SignallingRef[IO, Map[String, State]]) = {
      for {

        _ <- getStateSignal(name, ref).map(_.autos("autoPrepare")).changes.discrete.switchMap { autoOption =>
          autoOption.traverse { auto =>
            if (auto.enabled) Stream.fixedRate[IO](auto.interval).evalTap(_ => updateState(name, ref, state => state.copy(ready = state.ready + 1 * auto.multiplier)))
            else Stream.empty
          }
        }.holdOptionResource

        _ <- getStateSignal(name, ref).map(_.autos("autoCreate")).changes.discrete.switchMap { autoOption =>
          autoOption.traverse { auto =>
            if (auto.enabled)
              Stream.fixedRate[IO](auto.interval).evalTap(_ => for {
                created <- IO.monotonic.map(_.toMillis)
                id <- IO.randomUUID.map(_.toString)
                _ <- updateState(name, ref, state => {
                  if (state.maxHeight > state.elements.size + 1 && state.ready != 0) {
                    state.copy(elements = state.elements + (id -> ElementState(created, state.ready)), ready = 0)
                  } else state
                })
              } yield ())
            else Stream.empty
          }
        }.holdOptionResource

        _ <- getStateSignal(name, ref).map(_.autos("autoCollect")).changes.discrete.switchMap { autoOption =>
          autoOption.traverse { auto =>
            if (auto.enabled) Stream.fixedRate[IO](auto.interval).evalTap(_ =>
              IO.monotonic.map(_.toMillis).flatMap { now =>
                updateStateEffectfully(name, ref, state => {
                  val toBeCollected = state.elements.filter {
                    case (_, element) => (now - element.created) > auto.interval.toMillis
                  }
                  val sum = toBeCollected.values.map(_.number).sum
                  (state.copy(state.elements -- toBeCollected.keySet),
                    updateState(into, ref, state => state.copy(ready = state.ready + sum)))
                })
              })
            else Stream.empty
          }
        }.holdOptionResource





        finalDiv <- div(cls := "game",
          children[String] { id =>
            getState(name, ref).map(_.elements(id)).toResource
              .flatMap(element => div(cls := "block", element.number.toString))
          } <-- ref.map(_(name).elements.toList.sortBy(_._2.created).map(_._1)),
          p("autoprepare"),
          autoBox(name, "autoPrepare", ref),
          p("autocreate"),
          autoBox(name, "autoCreate", ref),
          p("autocollect"),
          autoBox(name, "autoCollect", ref),
          p("ready"),
          ref.map(_(name).ready).map(number => Option(div(number.toString))),
          p("gold"),
          ref.map(_(into).ready).map(number => Option(div(number.toString)))

        )
      } yield finalDiv
    }

    for{
      ref <- SignallingRef[IO].of(Map(
          "lemonade" ->
            State(Map.empty, 10, 0, Map(
              "autoPrepare" -> Some(Auto(2.seconds, false)),
              "autoCreate" -> Some(Auto(2.seconds, false)),
              "autoCollect" -> Some(Auto(2.seconds, false)))),
          "gold" -> State(Map.empty, 0, 0, Map.empty)
        )).toResource
      div <- gameColumn("lemonade", "gold", ref)

    } yield div
  }
}

case class State(elements: Map[String, ElementState],
                 maxHeight: Long,
                 ready: Long,
                 autos: Map[String, Option[Auto]])
case class ElementState(created: Long, number: Long)
case class Auto(interval: FiniteDuration, enabled: Boolean, multiplier: Long = 1)
