package com.whatsapp.analyzer

import erlang.forms.AbstractExpr._
import erlang.forms.AbstractForm._
import erlang.forms.AbstractPattern._

import scala.util.Using

object GenServerFeatures {

  /*
   * See https://erlang.org/doc/man/gen_server.html for key functions
   *
   * Here we model the ways in which an Erlang implementation of a gen_server
   * can respond to various kinds of messages - e.g. does a call return a reply with
   * some state, or noreply with some state or even stop? Sometimes we can't easily
   * work this out statically (else we'd already have a type system!) so we record
   * that we don't know.
   */
  case class Module(
    name: String,
    hasGenServerBehaviourAttribute: Boolean,
    hasOtherBehaviourAttribute: Boolean,
    hasCodeChangeDefined: Boolean,
    handleCallReturns: List[CallClauseReturn],
    handleCastReturns: List[CastLikeClauseReturn],
    handleInfoClauses: List[HandleInfoClause],
    handleContinueReturns: List[CastLikeClauseReturn],
    initReturns: List[InitClauseReturn],
    terminatePatterns: List[TerminateClausePattern],
  )

  // See Result case of https://erlang.org/doc/man/gen_server.html#Module:handle_call-3
  sealed trait CallClauseReturn
  case class CallReplyNewState() extends CallClauseReturn
  case class CallReplyNewStateTimeout() extends CallClauseReturn
  case class CallReplyNewStateHibernate() extends CallClauseReturn
  case class CallReplyNewStateContinue() extends CallClauseReturn
  case class CallNoReplyNewState() extends CallClauseReturn
  case class CallNoReplyNewStateTimeout() extends CallClauseReturn
  case class CallNoReplyNewStateHibernate() extends CallClauseReturn
  case class CallNoReplyNewStateContinue() extends CallClauseReturn
  case class CallStopReasonReplyNewState() extends CallClauseReturn
  case class CallStopReasonNewState() extends CallClauseReturn
  case class CallRecurses() extends CallClauseReturn
  case class CallImmediatelyDelegatesRemotely() extends CallClauseReturn
  case class CallImmediatelyDelegatesLocally() extends CallClauseReturn
  case class CallErrors() extends CallClauseReturn
  case class CallIncorrectReturnType() extends CallClauseReturn
  case class CallUnknownReturn() extends CallClauseReturn

  // TODO Handle these more neatly, e.g. cases with a catch-call that just return noreply/ok or error(badarg), perhaps after counting stats
  case class HandleInfoClause(param: HandleInfoParam, returns: CastLikeClauseReturn)

  sealed trait HandleInfoParam
  case class HandleInfoTimeout() extends HandleInfoParam
  case class HandleInfoTimer() extends HandleInfoParam
  case class HandleInfoSystemOrLibraryMessage() extends HandleInfoParam
  case class HandleInfoCatchAll() extends HandleInfoParam
  case class HandleInfoUnknown() extends HandleInfoParam

  // See Result case of https://erlang.org/doc/man/gen_server.html#Module:handle_cast-2
  sealed trait CastLikeClauseReturn
  case class CastLikeNoReplyNewState(approxEquivalentToDefaultImpl: Boolean) extends CastLikeClauseReturn
  case class CastLikeNoReplyNewStateTimeout() extends CastLikeClauseReturn
  case class CastLikeNoReplyNewStateHibernate() extends CastLikeClauseReturn
  case class CastLikeNoReplyNewStateContinue() extends CastLikeClauseReturn
  case class CastLikeStopReasonReplyNewState() extends CastLikeClauseReturn
  case class CastLikeStopReasonNewState() extends CastLikeClauseReturn
  case class CastLikeRecurses() extends CastLikeClauseReturn
  case class CastLikeImmediatelyDelegatesRemotely() extends CastLikeClauseReturn
  case class CastLikeImmediatelyDelegatesLocally() extends CastLikeClauseReturn
  case class CastLikeErrors() extends CastLikeClauseReturn
  case class CastLikeIncorrectReturnType() extends CastLikeClauseReturn
  case class CastLikeUnknownReturn() extends CastLikeClauseReturn

  // See Result case of https://erlang.org/doc/man/gen_server.html#Module:init-1
  sealed trait InitClauseReturn
  case class InitOkState() extends InitClauseReturn
  case class InitOkStateTimeout() extends InitClauseReturn
  case class InitOkStateHibernate() extends InitClauseReturn
  case class InitOkStateContinue() extends InitClauseReturn
  case class InitStopReason() extends InitClauseReturn
  case class InitIgnore() extends InitClauseReturn
  case class InitRecurses() extends InitClauseReturn
  case class InitImmediatelyDelegatesRemotely() extends InitClauseReturn
  case class InitImmediatelyDelegatesLocally() extends InitClauseReturn
  case class InitErrors() extends InitClauseReturn
  case class InitIncorrectReturnType() extends InitClauseReturn
  case class InitUnknownReturn() extends InitClauseReturn

  // See https://erlang.org/doc/man/gen_server.html#Module:terminate-2
  sealed trait TerminateClausePattern
  case class TerminateJustReturnsOk() extends TerminateClausePattern
  case class TerminateIsCustom() extends TerminateClausePattern

  // All the functions that return the same type as handle_cast
  sealed trait WhichCastLike
  case class HandleCast() extends WhichCastLike
  case class HandleContinue() extends WhichCastLike
  case class HandleInfo() extends WhichCastLike

  case class AppInfo(app: String, modules: List[Module])

  /*
   * See:
   *   https://erlang.org/doc/man/gen_server.html#Module:handle_cast-2
   *   https://erlang.org/doc/man/gen_server.html#Module:handle_continue-2
   *   https://erlang.org/doc/man/gen_server.html#Module:handle_info-2
   */
  case class CastLikeClauseReturnCounts(
      whichCastLike: WhichCastLike,
      totalNoReplyNewState: Int,
      totalNoReplyNewStateTimeout: Int,
      totalNoReplyNewStateHibernate: Int,
      totalNoReplyNewStateContinue: Int,
      totalStopReasonReplyNewState: Int,
      totalStopReasonNewState: Int,
      totalRecurses: Int,
      totalImmediatelyDelegatesRemotely: Int,
      totalImmediatelyDelegatesLocally: Int,
      totalErrors: Int,
      totalIncorrectReturnType: Int,
      totalUnknownReturn: Int,
  ) {
    def +(that: CastLikeClauseReturnCounts): CastLikeClauseReturnCounts =
      CastLikeClauseReturnCounts(
        this.whichCastLike,
        this.totalNoReplyNewState + that.totalNoReplyNewState,
        this.totalNoReplyNewStateTimeout + that.totalNoReplyNewStateTimeout,
        this.totalNoReplyNewStateHibernate + that.totalNoReplyNewStateHibernate,
        this.totalNoReplyNewStateContinue + that.totalNoReplyNewStateContinue,
        this.totalStopReasonReplyNewState + that.totalStopReasonReplyNewState,
        this.totalStopReasonNewState + that.totalStopReasonNewState,
        this.totalRecurses + that.totalRecurses,
        this.totalImmediatelyDelegatesRemotely + that.totalImmediatelyDelegatesRemotely,
        this.totalImmediatelyDelegatesLocally + that.totalImmediatelyDelegatesLocally,
        this.totalErrors + that.totalErrors,
        this.totalIncorrectReturnType + that.totalIncorrectReturnType,
        this.totalUnknownReturn + that.totalUnknownReturn,
      )
    def totalClauses(): Int = {
      this.totalNoReplyNewState +
        this.totalNoReplyNewStateTimeout +
        this.totalNoReplyNewStateHibernate +
        this.totalNoReplyNewStateContinue +
        this.totalStopReasonReplyNewState +
        this.totalStopReasonNewState +
        this.totalRecurses +
        this.totalImmediatelyDelegatesRemotely +
        this.totalImmediatelyDelegatesLocally +
        this.totalErrors +
        this.totalIncorrectReturnType +
        this.totalUnknownReturn
    }
  }
  object CastLikeClauseReturnCounts {
    def empty(whichCastLike: WhichCastLike): CastLikeClauseReturnCounts =
      CastLikeClauseReturnCounts(whichCastLike, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
  }

  case class HandleInfoParamCounts(
    totalTimeout: Int,
    totalTimer: Int,
    totalSystemOrLibraryMessage: Int,
    totalCatchAll: Int,
    totalUnknown: Int,
   ) {
    def +(that: HandleInfoParamCounts): HandleInfoParamCounts =
      HandleInfoParamCounts(
        this.totalTimeout + that.totalTimeout,
        this.totalTimer + that.totalTimer,
        this.totalSystemOrLibraryMessage + that.totalSystemOrLibraryMessage,
        this.totalCatchAll + that.totalCatchAll,
        this.totalUnknown + that.totalUnknown,
      )
  }
  object HandleInfoParamCounts {
    def empty: HandleInfoParamCounts =
      HandleInfoParamCounts(0,0,0,0,0)
  }

  case class TerminatePatternCounts(totalJustReturnsOk: Int, totalIsCustom: Int) {
    def +(that: TerminatePatternCounts): TerminatePatternCounts =
      TerminatePatternCounts(
        this.totalJustReturnsOk + that.totalJustReturnsOk,
        this.totalIsCustom + that.totalIsCustom,
      )
    def totalClauses(): Int = this.totalJustReturnsOk + this.totalIsCustom
  }
  object TerminatePatternCounts {
    def empty: TerminatePatternCounts =
      TerminatePatternCounts(0, 0)
  }

  def looksSuspiciouslyLikeAGenServer(m: Module): Boolean = {
    val hasGenServeryFunctions =
      m.handleCallReturns.nonEmpty ||
        m.handleCastReturns.nonEmpty ||
        m.handleInfoClauses.nonEmpty ||
        m.handleContinueReturns.nonEmpty
    /* We don't check these because having these functions in your module isn't
     * a great indicator of whether it is intended to be used as a gen_server
        m.initReturns.length > 0 ||
        m.hasCodeChangeDefined ||
        m.hasTerminateDefined
     */

    hasGenServeryFunctions &&
    (!m.hasOtherBehaviourAttribute) &&
    (!m.hasGenServerBehaviourAttribute)
  }

  def countCastLikeClauseReturns(
      whichCastLike: WhichCastLike,
      clauseReturns: List[CastLikeClauseReturn],
  ): CastLikeClauseReturnCounts = {

    def sumClauseReturns(acc: CastLikeClauseReturnCounts, x: CastLikeClauseReturn): CastLikeClauseReturnCounts =
      x match {
        case CastLikeNoReplyNewState(_) =>
          acc.copy(totalNoReplyNewState = acc.totalNoReplyNewState + 1)
        case CastLikeNoReplyNewStateTimeout() =>
          acc.copy(totalNoReplyNewStateTimeout = acc.totalNoReplyNewStateTimeout + 1)
        case CastLikeNoReplyNewStateHibernate() =>
          acc.copy(totalNoReplyNewStateHibernate = acc.totalNoReplyNewStateHibernate + 1)
        case CastLikeNoReplyNewStateContinue() =>
          acc.copy(totalNoReplyNewStateContinue = acc.totalNoReplyNewStateContinue + 1)
        case CastLikeStopReasonReplyNewState() =>
          acc.copy(totalStopReasonReplyNewState = acc.totalStopReasonReplyNewState + 1)
        case CastLikeStopReasonNewState() =>
          acc.copy(totalStopReasonNewState = acc.totalStopReasonNewState + 1)
        case CastLikeRecurses() =>
          acc.copy(totalRecurses = acc.totalRecurses + 1)
        case CastLikeImmediatelyDelegatesRemotely() =>
          acc.copy(totalImmediatelyDelegatesRemotely = acc.totalImmediatelyDelegatesRemotely + 1)
        case CastLikeImmediatelyDelegatesLocally() =>
          acc.copy(totalImmediatelyDelegatesLocally = acc.totalImmediatelyDelegatesLocally + 1)
        case CastLikeErrors() =>
          acc.copy(totalErrors = acc.totalErrors + 1)
        case CastLikeIncorrectReturnType() =>
          acc.copy(totalIncorrectReturnType = acc.totalIncorrectReturnType + 1)
        case CastLikeUnknownReturn() =>
          acc.copy(totalUnknownReturn = acc.totalUnknownReturn + 1)
      }

    clauseReturns.foldLeft(CastLikeClauseReturnCounts.empty(whichCastLike))(sumClauseReturns)
  }

  def countTerminateClausePatterns(
      clausePatterns: List[TerminateClausePattern]
  ): TerminatePatternCounts = {

    def sumClauseReturns(acc: TerminatePatternCounts, clause: TerminateClausePattern): TerminatePatternCounts =
      clause match {
        case TerminateJustReturnsOk() =>
          acc.copy(totalJustReturnsOk = acc.totalJustReturnsOk + 1)
        case TerminateIsCustom() =>
          acc.copy(totalIsCustom = acc.totalIsCustom + 1)
      }

    clausePatterns.foldLeft(TerminatePatternCounts.empty)(sumClauseReturns)
  }

  def countHandleInfoParams(params: List[HandleInfoParam]): HandleInfoParamCounts = {

    def sumParams(acc: HandleInfoParamCounts, param: HandleInfoParam): HandleInfoParamCounts = {
      param match {
        case HandleInfoTimeout() => acc.copy(totalTimeout = acc.totalTimeout + 1)
        case HandleInfoTimer() => acc.copy(totalTimer = acc.totalTimer + 1)
        case HandleInfoSystemOrLibraryMessage() => acc.copy(totalSystemOrLibraryMessage = acc.totalSystemOrLibraryMessage + 1)
        case HandleInfoCatchAll() => acc.copy(totalCatchAll = acc.totalCatchAll + 1)
        case HandleInfoUnknown() => acc.copy(totalUnknown = acc.totalUnknown + 1)
      }
    }

    params.foldLeft(HandleInfoParamCounts.empty)(sumParams)
  }

  def main(args: Array[String]): Unit = {
    val infos = Using.resource(RPC.connect())(loadData)
    var totalModules = 0
    var totalGenServers = 0
    var totalPossibleGenServersWithoutAttribute = 0
    var totalHandleCallsDefined, totalHandleCallClauses = 0
    var totalInitsDefined, totalInitClauses = 0
    var totalTerminatesDefined, totalCodeChangesDefined = 0
    var totalHandleCallReplyNewState, totalHandleCallReplyNewStateTimeout, totalHandleCallReplyNewStateHibernate,
        totalHandleCallReplyNewStateContinue, totalHandleCallNoReplyNewState, totalHandleCallNoReplyNewStateTimeout,
        totalHandleCallNoReplyNewStateHibernate, totalHandleCallNoReplyNewStateContinue,
        totalHandleCallStopReasonReplyNewState, totalHandleCallStopReasonNewState, totalHandleCallRecurses,
        totalHandleCallImmediatelyDelegatesRemotely, totalHandleCallImmediatelyDelegatesLocally, totalHandleCallErrors,
        totalHandleCallIncorrectReturnType, totalHandleCallUnknownReturn = 0
    var totalInitOkState, totalInitOkStateTimeout, totalInitOkStateHibernate, totalInitOkStateContinue,
        totalInitStopReason, totalInitIgnore, totalInitRecurses, totalInitImmediatelyDelegatesRemotely,
        totalInitImmediatelyDelegatesLocally, totalInitErrors, totalInitIncorrectReturnType, totalInitUnknownReturn = 0
    var totalHandleCastClauseReturns = CastLikeClauseReturnCounts.empty(HandleCast())
    var totalHandleInfoClauseReturns = CastLikeClauseReturnCounts.empty(HandleInfo())
    var totalHandleInfoClauseParams = HandleInfoParamCounts.empty
    var totalHandleContinueClauseReturns = CastLikeClauseReturnCounts.empty(HandleContinue())
    var totalTerminateClausePatterns = TerminatePatternCounts.empty
    var totalHandleCastsDefined, totalHandleInfosDefined, totalHandleContinuesDefined = 0
    var totalHandleInfosThatAreEquivalentToTheDefaultImplementation = 0
    var totalTerminatesThatAreEquivalentToTheDefaultImplementation = 0

    for {
      info <- infos
      module <- info.modules
    } {

      totalModules = totalModules + 1

      if (!module.hasGenServerBehaviourAttribute) {

        if (looksSuspiciouslyLikeAGenServer(module)) {
          println("Possibly missing `gen_server` attribute: " + module.name)
          totalPossibleGenServersWithoutAttribute += 1
        }

      } else {

        totalGenServers += 1

        totalCodeChangesDefined += (if (module.hasCodeChangeDefined) 1 else 0)

        totalInitsDefined += (if (module.initReturns.nonEmpty) 1 else 0)
        totalInitClauses += module.initReturns.length
        for {
          initReturnValue <- module.initReturns
        } initReturnValue match {
          case InitOkState()                      => totalInitOkState += 1
          case InitOkStateTimeout()               => totalInitOkStateTimeout += 1
          case InitOkStateHibernate()             => totalInitOkStateHibernate += 1
          case InitOkStateContinue()              => totalInitOkStateContinue += 1
          case InitStopReason()                   => totalInitStopReason += 1
          case InitIgnore()                       => totalInitIgnore += 1
          case InitRecurses()                     => totalInitRecurses += 1
          case InitImmediatelyDelegatesRemotely() => totalInitImmediatelyDelegatesRemotely += 1
          case InitImmediatelyDelegatesLocally()  => totalInitImmediatelyDelegatesLocally += 1
          case InitErrors()                       => totalInitErrors += 1
          case InitIncorrectReturnType()          => totalInitIncorrectReturnType += 1
          case InitUnknownReturn()                => totalInitUnknownReturn += 1
        }

        totalHandleCallsDefined += (if (module.handleCallReturns.nonEmpty) 1 else 0)
        totalHandleCallClauses += module.handleCallReturns.length
        for {
          clauseReturnValue <- module.handleCallReturns
        } clauseReturnValue match {
          case CallReplyNewState()                => totalHandleCallReplyNewState += 1
          case CallReplyNewStateTimeout()         => totalHandleCallReplyNewStateTimeout += 1
          case CallReplyNewStateHibernate()       => totalHandleCallReplyNewStateHibernate += 1
          case CallReplyNewStateContinue()        => totalHandleCallReplyNewStateContinue += 1
          case CallNoReplyNewState()              => totalHandleCallNoReplyNewState += 1
          case CallNoReplyNewStateTimeout()       => totalHandleCallNoReplyNewStateTimeout += 1
          case CallNoReplyNewStateHibernate()     => totalHandleCallNoReplyNewStateHibernate += 1
          case CallNoReplyNewStateContinue()      => totalHandleCallNoReplyNewStateContinue += 1
          case CallStopReasonReplyNewState()      => totalHandleCallStopReasonReplyNewState += 1
          case CallStopReasonNewState()           => totalHandleCallStopReasonNewState += 1
          case CallRecurses()                     => totalHandleCallRecurses += 1
          case CallImmediatelyDelegatesRemotely() => totalHandleCallImmediatelyDelegatesRemotely += 1
          case CallImmediatelyDelegatesLocally()  => totalHandleCallImmediatelyDelegatesLocally += 1
          case CallErrors()                       => totalHandleCallErrors += 1
          case CallIncorrectReturnType()          => totalHandleCallIncorrectReturnType += 1
          case CallUnknownReturn()                => totalHandleCallUnknownReturn += 1
        }

        val additionalHandleCastClauses = countCastLikeClauseReturns(HandleCast(), module.handleCastReturns)
        val additionalHandleInfoReturns = countCastLikeClauseReturns(HandleInfo(), module.handleInfoClauses.map(_.returns))
        val additionalHandleInfoParams = countHandleInfoParams(module.handleInfoClauses.map(_.param))
        val additionalHandleContinueClauses = countCastLikeClauseReturns(HandleContinue(), module.handleContinueReturns)
        val additionalTerminateClausePatterns = countTerminateClausePatterns(module.terminatePatterns)

        totalTerminatesThatAreEquivalentToTheDefaultImplementation +=
          (if (module.terminatePatterns.forall { case TerminateJustReturnsOk() => true ; case _ => false }) 1 else 0)

        totalHandleInfosThatAreEquivalentToTheDefaultImplementation +=
          (if (module.handleInfoClauses.map(_.returns).forall{ case CastLikeNoReplyNewState(true) => true ; case _ => false }) 1 else 0)

        totalHandleCastsDefined += (if (module.handleCastReturns.nonEmpty) 1 else 0)
        totalHandleContinuesDefined += (if (module.handleContinueReturns.nonEmpty) 1 else 0)
        totalHandleInfosDefined += (if (module.handleInfoClauses.nonEmpty) 1 else 0)
        totalTerminatesDefined += (if (module.terminatePatterns.nonEmpty) 1 else 0)

        totalHandleCastClauseReturns += additionalHandleCastClauses
        totalHandleInfoClauseReturns += additionalHandleInfoReturns
        totalHandleInfoClauseParams += additionalHandleInfoParams
        totalHandleContinueClauseReturns += additionalHandleContinueClauses
        totalTerminateClausePatterns += additionalTerminateClausePatterns
      }
    }

    println(s"Total modules: $totalModules")
    println(s"  that look like a `gen_server`: $totalGenServers")
    println(
      s"  that look like a `gen_server` but don't have the `gen_server` attribute: $totalPossibleGenServersWithoutAttribute"
    )
    println()
    println(s"Total `gen_servers` that implement...")
    println(s"  `init`: $totalInitsDefined")
    println(s"  `handle_call`: $totalHandleCallsDefined")
    println(s"  `handle_cast`: $totalHandleCastsDefined")
    println(s"  `handle_info`: $totalHandleInfosDefined")
    println(s"  `terminate`: $totalTerminatesDefined")
    println(s"  `handle_continue`: $totalHandleContinuesDefined")
    println(s"  `code_change`: $totalCodeChangesDefined")
    println()
    println(s"Total `terminate` functions which could probably be deleted: $totalTerminatesThatAreEquivalentToTheDefaultImplementation")
    println(s"Total `handle_info` functions which could probably be deleted: $totalHandleInfosThatAreEquivalentToTheDefaultImplementation")
    println()
    println(s"Total `handle_call` clauses: $totalHandleCallClauses")
    println(s"  that return `{reply,Reply,NewState}`: $totalHandleCallReplyNewState")
    println(s"  that return `{reply,Reply,NewState,Timeout}`: $totalHandleCallReplyNewStateTimeout")
    println(s"  that return `{reply,Reply,NewState,hibernate}`: $totalHandleCallReplyNewStateHibernate")
    println(s"  that return `{reply,Reply,NewState,{continue,Continue}}`: $totalHandleCallReplyNewStateContinue")
    println(s"  that return `{noreply,NewState}`: $totalHandleCallNoReplyNewState")
    println(s"  that return `{noreply,NewState,Timeout}`: $totalHandleCallNoReplyNewStateTimeout")
    println(s"  that return `{noreply,NewState,hibernate}`: $totalHandleCallNoReplyNewStateHibernate")
    println(s"  that return `{noreply,NewState,{continue,Continue}}`: $totalHandleCallNoReplyNewStateContinue")
    println(s"  that return `{stop,Reason,NewState}`: $totalHandleCallStopReasonNewState")
    println(s"  that return `{stop,Reason,Reply,NewState}`: $totalHandleCallStopReasonReplyNewState")
    println(s"  that recurse back into handle_call: $totalHandleCallRecurses")
    println(s"  that delegate to a remote function entirely: $totalHandleCallImmediatelyDelegatesRemotely")
    println(s"  that delegate to another local function entirely: $totalHandleCallImmediatelyDelegatesLocally")
    println(s"  that call `error`, `exit` or `throw` in the tail position: $totalHandleCallErrors")
    println(s"  that immediately return something of the wrong type: $totalHandleCallIncorrectReturnType")
    println(s"  that do something we can't yet categorise: $totalHandleCallUnknownReturn")
    println()
    printCastLikeClauseReturnCounts(totalHandleCastClauseReturns)
    println()
    printCastLikeClauseReturnCounts(totalHandleInfoClauseReturns)
    println()
    println(s"Messages handled by `handle_info`:")
    println(s"  `timeout`: ${totalHandleInfoClauseParams.totalTimeout}")
    println(s"  `timer`: ${totalHandleInfoClauseParams.totalTimer}")
    println(s"  standard system or library messages: ${totalHandleInfoClauseParams.totalSystemOrLibraryMessage}")
    println(s"  catch-all: ${totalHandleInfoClauseParams.totalCatchAll}")
    println(s"  uncategorized: ${totalHandleInfoClauseParams.totalUnknown}")
    println()
    printCastLikeClauseReturnCounts(totalHandleContinueClauseReturns)
    println()
    println(s"Total `init` clauses: $totalInitClauses")
    println(s"  that return `{ok,State}`: $totalInitOkState")
    println(s"  that return `{ok,State,Timeout}`: $totalInitOkStateTimeout")
    println(s"  that return `{ok,State,Hibernate}`: $totalInitOkStateHibernate")
    println(s"  that return `{ok,State,Continue}`: $totalInitOkStateContinue")
    println(s"  that return `{stop,Reason}`: $totalInitStopReason")
    println(s"  that return `{ignore}`: $totalInitIgnore")
    println(s"  that recurse back into init: $totalInitRecurses")
    println(s"  that delegate to a remote function entirely: $totalInitImmediatelyDelegatesRemotely")
    println(s"  that delegate to another local function entirely: $totalInitImmediatelyDelegatesLocally")
    println(s"  that call `error`, `exit` or `throw` in the tail position: $totalInitErrors")
    println(s"  that immediately return something of the wrong type: $totalInitIncorrectReturnType")
    println(s"  that do something we can't yet categorise: $totalInitUnknownReturn")
    println()
    println(s"Total `terminate` clauses: ${totalTerminateClausePatterns.totalClauses()}")
    println(s"  that immediately return `ok`: ${totalTerminateClausePatterns.totalJustReturnsOk}")
    println(s"  that do some custom actions: ${totalTerminateClausePatterns.totalIsCustom}")
    println()
  }

  def printCastLikeClauseReturnCounts(clauseReturnCounts: CastLikeClauseReturnCounts): Unit = {

    val functionName = nameOfCastLike(clauseReturnCounts.whichCastLike)
    println(s"Total `$functionName` clauses: ${clauseReturnCounts.totalClauses()}")
    println(s"  that return `{noreply,NewState}`: ${clauseReturnCounts.totalNoReplyNewState}")
    println(s"  that return `{noreply,NewState,Timeout}`: ${clauseReturnCounts.totalNoReplyNewStateTimeout}")
    println(s"  that return `{noreply,NewState,hibernate}`: ${clauseReturnCounts.totalNoReplyNewStateHibernate}")
    println(
      s"  that return `{noreply,NewState,{continue,Continue}}`: ${clauseReturnCounts.totalNoReplyNewStateContinue}"
    )
    println(s"  that return `{stop,Reason,Reply,NewState}`: ${clauseReturnCounts.totalStopReasonReplyNewState}")
    println(s"  that return `{stop,Reason,NewState}`: ${clauseReturnCounts.totalStopReasonNewState}")
    println(s"  that recurse back into $functionName: ${clauseReturnCounts.totalRecurses}")
    println(
      s"  that delegate to a remote function entirely: ${clauseReturnCounts.totalImmediatelyDelegatesRemotely}"
    )
    println(
      s"  that delegate to another local function entirely: ${clauseReturnCounts.totalImmediatelyDelegatesLocally}"
    )
    println(s"  that call `error`, `exit` or `throw` in the tail position: ${clauseReturnCounts.totalErrors}")
    println(s"  that immediately return something of the wrong type: ${clauseReturnCounts.totalIncorrectReturnType}")
    println(s"  that do something we can't yet categorise: ${clauseReturnCounts.totalUnknownReturn}")
  }

  private def loadData(rpc: RPC): List[AppInfo] = {
    val infos = CodeDirs.projectEbinDirs.map(indexProjectDir(_, rpc))
    infos.filterNot { info => CodeDirs.thirdParty.contains(info.app) }
  }

  private def nameOfCastLike(whichCastLike: WhichCastLike): String =
    whichCastLike match {
      case HandleCast()     => "handle_cast"
      case HandleContinue() => "handle_continue"
      case HandleInfo()     => "handle_info"
    }

  private def isLog(expr: AbstractExpr) = expr match {
    case AF_RemoteCall(AF_LiteralAtom("logger"),_,_) | AF_RemoteCall(AF_LiteralAtom("error_logger"),_,_) => true
    case _ => false
  }

  private def determineCastLikeClauseReturnValue(thisFunction: WhichCastLike)(clause: AF_Clause): CastLikeClauseReturn =
    clause.body.last match {
      case AF_Tuple(AF_LiteralAtom("noreply") :: _newState :: AF_LiteralAtom("hibernate") :: Nil) =>
        CastLikeNoReplyNewStateHibernate()
      case AF_Tuple(
            AF_LiteralAtom("noreply") :: _newState :: AF_Tuple(AF_LiteralAtom("continue") :: _continue :: Nil) :: Nil
          ) =>
        CastLikeNoReplyNewStateContinue()
      case AF_Tuple(AF_LiteralAtom("noreply") :: _newState :: _timeout :: Nil) =>
        CastLikeNoReplyNewStateTimeout()
      case AF_Tuple(AF_LiteralAtom("noreply") :: AF_Variable(_) :: Nil) =>
        // TODO Check the variable is the state that was passed in
        val bodyJustLogs = clause.body.dropRight(1).forall(isLog)
        CastLikeNoReplyNewState(bodyJustLogs)
      case AF_Tuple(AF_LiteralAtom("noreply") :: _newState :: Nil) =>
        CastLikeNoReplyNewState(false)
      case AF_Tuple(AF_LiteralAtom("stop") :: _reason :: _newState :: Nil) =>
        CastLikeStopReasonNewState()
      case AF_LocalCall(AF_LiteralAtom(functionName), _) if functionName == nameOfCastLike(thisFunction) =>
        CastLikeRecurses()
      case AF_RemoteCall(AF_LiteralAtom("erlang"), AF_LiteralAtom("error"), _) |
          AF_LocalCall(AF_LiteralAtom("error"), _) |
          AF_RemoteCall(AF_LiteralAtom("erlang"), AF_LiteralAtom("exit"), _) | AF_LocalCall(AF_LiteralAtom("exit"), _) |
          AF_RemoteCall(AF_LiteralAtom("erlang"), AF_LiteralAtom("throw"), _) |
          AF_LocalCall(AF_LiteralAtom("throw"), _) =>
        CastLikeErrors()
      case AF_LocalCall(AF_LiteralAtom(_), _) if clause.body.length == 1 =>
        CastLikeImmediatelyDelegatesLocally()
      case AF_RemoteCall(AF_LiteralAtom(_), AF_LiteralAtom(_), _) if clause.body.length == 1 =>
        CastLikeImmediatelyDelegatesRemotely()
      case _: AF_Literal | AF_Tuple(_) | AF_Nil | AF_Cons(_, _) | AF_RecordCreation(_, _) | AF_RecordUpdate(_, _, _) |
          AF_MapCreation(_) | AF_MapUpdate(_, _) | AF_ListComprehension(_, _) | AF_BinaryComprehension(_, _) =>
        CastLikeIncorrectReturnType()
      case unknown =>
        println(s"Unknown cast-like: ${unknown.toString}")
        CastLikeUnknownReturn()
    }

  private def determineHandleInfoParamKind(clause: AF_Clause): HandleInfoParam = {
    clause.pats match {
      case PatternLiteral(AF_LiteralAtom("timeout"))::_state::Nil =>
        HandleInfoTimeout()
      case PatternLiteral(AF_LiteralAtom("timer"))::_state::Nil =>
        HandleInfoTimer()
      case PatternTuple(PatternLiteral(AF_LiteralAtom("EXIT"))::_::_::Nil)::_::Nil |
           // https://erlang.org/doc/reference_manual/processes.html#monitors
           PatternTuple(PatternLiteral(AF_LiteralAtom("DOWN"))::_::_::_::_::Nil)::_::Nil |
           // https://erlang.org/doc/man/gen_tcp.html
           PatternTuple(PatternLiteral(AF_LiteralAtom("tcp"))::_::_::Nil)::_::Nil |
           PatternTuple(PatternLiteral(AF_LiteralAtom("tcp_closed"))::_::Nil)::_::Nil |
           PatternTuple(PatternLiteral(AF_LiteralAtom("tcp_error"))::_::_::Nil)::_::Nil |
           PatternTuple(PatternLiteral(AF_LiteralAtom("tcp_passive"))::_::Nil)::_::Nil |
           // https://erlang.org/doc/man/gen_udp.html
           PatternTuple(PatternLiteral(AF_LiteralAtom("udp"))::_::_::_::_::Nil)::_::Nil |
           PatternTuple(PatternLiteral(AF_LiteralAtom("udp"))::_::_::_::_::_::Nil)::_::Nil |
           PatternTuple(PatternLiteral(AF_LiteralAtom("udp_passive"))::_::Nil)::_::Nil |
           // https://erlang.org/doc/man/ssl.html
           PatternTuple(PatternLiteral(AF_LiteralAtom("ssl"))::_::_::Nil)::_::Nil |
           PatternTuple(PatternLiteral(AF_LiteralAtom("ssl_closed"))::_::Nil)::_::Nil |
           PatternTuple(PatternLiteral(AF_LiteralAtom("ssl_error"))::_::_::Nil)::_::Nil |
           PatternTuple(PatternLiteral(AF_LiteralAtom("ssl_passive"))::_::Nil)::_::Nil |
           // https://erlang.org/doc/man/ets.html
           PatternTuple(PatternLiteral(AF_LiteralAtom("ETS-TRANSFER"))::_::_::_::Nil)::_::Nil =>
        HandleInfoSystemOrLibraryMessage()
      case PatternVariable(_)::PatternVariable(_)::Nil =>
        HandleInfoCatchAll()
      case unknown =>
        println("Unknown handle_info pattern: " ++ unknown.toString)
        HandleInfoUnknown()
    }
  }

  private def determineCallClauseReturnValue(clause: AF_Clause): CallClauseReturn =
    clause.body.last match {
      case AF_Tuple(AF_LiteralAtom("reply") :: _reply :: _newState :: AF_LiteralAtom("hibernate") :: Nil) =>
        CallReplyNewStateHibernate()
      case AF_Tuple(
            AF_LiteralAtom("reply") :: _reply :: _newState :: AF_Tuple(
              AF_LiteralAtom("continue") :: _continue :: Nil
            ) :: Nil
          ) =>
        CallReplyNewStateContinue()
      case AF_Tuple(AF_LiteralAtom("reply") :: _reply :: _newState :: Nil) =>
        CallReplyNewState()
      case AF_Tuple(AF_LiteralAtom("reply") :: _reply :: _newState :: _timeout :: Nil) =>
        CallReplyNewStateTimeout()
      case AF_Tuple(AF_LiteralAtom("noreply") :: _newState :: Nil) =>
        CallNoReplyNewState()
      case AF_Tuple(AF_LiteralAtom("noreply") :: _newState :: AF_LiteralAtom("hibernate") :: Nil) =>
        CallNoReplyNewStateHibernate()
      case AF_Tuple(
            AF_LiteralAtom("noreply") :: _newState :: AF_Tuple(AF_LiteralAtom("continue") :: _continue :: Nil) :: Nil
          ) =>
        CallNoReplyNewStateContinue()
      case AF_Tuple(AF_LiteralAtom("noreply") :: _newState :: _timeout :: Nil) =>
        CallNoReplyNewStateTimeout()
      case AF_Tuple(AF_LiteralAtom("stop") :: _reason :: _newState :: Nil) =>
        CallStopReasonNewState()
      case AF_Tuple(AF_LiteralAtom("stop") :: _reason :: _reply :: _newState :: Nil) =>
        CallStopReasonReplyNewState()
      case AF_LocalCall(AF_LiteralAtom("handle_call"), _) =>
        CallRecurses()
      case AF_RemoteCall(AF_LiteralAtom("erlang"), AF_LiteralAtom("error"), _) |
          AF_LocalCall(AF_LiteralAtom("error"), _) |
          AF_RemoteCall(AF_LiteralAtom("erlang"), AF_LiteralAtom("exit"), _) | AF_LocalCall(AF_LiteralAtom("exit"), _) |
          AF_RemoteCall(AF_LiteralAtom("erlang"), AF_LiteralAtom("throw"), _) |
          AF_LocalCall(AF_LiteralAtom("throw"), _) =>
        CallErrors()
      case AF_LocalCall(AF_LiteralAtom(_), _) if clause.body.length == 1 =>
        CallImmediatelyDelegatesLocally()
      case AF_RemoteCall(AF_LiteralAtom(_), AF_LiteralAtom(_), _) if clause.body.length == 1 =>
        CallImmediatelyDelegatesRemotely()
      case _: AF_Literal | AF_Tuple(_) | AF_Nil | AF_Cons(_, _) | AF_RecordCreation(_, _) | AF_RecordUpdate(_, _, _) |
          AF_MapCreation(_) | AF_MapUpdate(_, _) | AF_ListComprehension(_, _) | AF_BinaryComprehension(_, _) =>
        CallIncorrectReturnType()
      case unknown =>
        println(s"Unknown call: ${unknown.toString}")
        CallUnknownReturn()
    }

  private def determineInitReturnValue(clause: AF_Clause): InitClauseReturn = {
    clause.body.last match {
      case AF_Tuple(AF_LiteralAtom("ok") :: _state :: Nil) =>
        InitOkState()
      case AF_Tuple(AF_LiteralAtom("ok") :: _state :: AF_LiteralAtom("hibernate") :: Nil) =>
        InitOkStateHibernate()
      case AF_Tuple(
            AF_LiteralAtom("ok") :: _state :: AF_Tuple(AF_LiteralAtom("continue") :: _continue :: Nil) :: Nil
          ) =>
        InitOkStateContinue()
      case AF_Tuple(AF_LiteralAtom("ok") :: _state :: _timeout :: Nil) =>
        InitOkStateTimeout()
      case AF_Tuple(AF_LiteralAtom("stop") :: _reason :: Nil) =>
        InitStopReason()
      case AF_LiteralAtom("ignore") =>
        InitIgnore()
      case AF_LocalCall(AF_LiteralAtom("init"), _) =>
        InitRecurses()
      case AF_RemoteCall(AF_LiteralAtom("erlang"), AF_LiteralAtom("error"), _) |
          AF_LocalCall(AF_LiteralAtom("error"), _) |
          AF_RemoteCall(AF_LiteralAtom("erlang"), AF_LiteralAtom("exit"), _) | AF_LocalCall(AF_LiteralAtom("exit"), _) |
          AF_RemoteCall(AF_LiteralAtom("erlang"), AF_LiteralAtom("throw"), _) |
          AF_LocalCall(AF_LiteralAtom("throw"), _) =>
        InitErrors()
      case AF_LocalCall(AF_LiteralAtom(_), _) if clause.body.length == 1 =>
        InitImmediatelyDelegatesLocally()
      case AF_RemoteCall(AF_LiteralAtom(_), AF_LiteralAtom(_), _) if clause.body.length == 1 =>
        InitImmediatelyDelegatesRemotely()
      case _: AF_Literal | AF_Tuple(_) | AF_Nil | AF_Cons(_, _) | AF_RecordCreation(_, _) | AF_RecordUpdate(_, _, _) |
          AF_MapCreation(_) | AF_MapUpdate(_, _) | AF_ListComprehension(_, _) | AF_BinaryComprehension(_, _) =>
        InitIncorrectReturnType()
      case unknown =>
        println(s"Unknown init: ${unknown.toString}")
        InitUnknownReturn()
    }
  }

  private def determineTerminateClausePattern(clause: AF_Clause): TerminateClausePattern = {
    clause match {
      case AF_Clause(_, _, AF_LiteralAtom("ok") :: Nil) => TerminateJustReturnsOk()
      case AF_Clause(_, _, _)                           => TerminateIsCustom()
    }
  }

  private def updateModuleWithNewForm(module: Module, form: AbstractForm): Module = {
    form match {
      case AF_Behaviour("gen_server") =>
        module.copy(hasGenServerBehaviourAttribute = true)
      case AF_Behaviour(_) =>
        module.copy(hasOtherBehaviourAttribute = true)
      case AF_FunctionDecl("code_change", 3, _) =>
        module.copy(hasCodeChangeDefined = true)
      case AF_FunctionDecl("terminate", 2, clauses) =>
        module.copy(terminatePatterns = clauses.map(determineTerminateClausePattern))
      case AF_FunctionDecl("init", 1, clauses) =>
        module.copy(initReturns = clauses.map(determineInitReturnValue))
      case AF_FunctionDecl("handle_call", 3, clauses) =>
        module.copy(handleCallReturns = clauses.map(determineCallClauseReturnValue))
      case AF_FunctionDecl("handle_cast", 2, clauses) =>
        module.copy(handleCastReturns = clauses.map(determineCastLikeClauseReturnValue(HandleCast())))
      case AF_FunctionDecl("handle_continue", 2, clauses) =>
        module.copy(handleContinueReturns = clauses.map(determineCastLikeClauseReturnValue(HandleContinue())))
      case AF_FunctionDecl("handle_info", 2, clauses) =>
        val classifications =
          clauses.map(c =>
            HandleInfoClause(
              determineHandleInfoParamKind(c),
              determineCastLikeClauseReturnValue(HandleInfo())(c)))
        module.copy(handleInfoClauses = classifications)
      case _ =>
        module
    }
  }

  private def indexProjectDir(dir: String, rpc: RPC): AppInfo = {
    val start = dir.lastIndexOf("/lib/")
    val end = dir.lastIndexOf('/')
    val libName = dir.substring(start + 5, end)
    val dirFile = new java.io.File(dir)
    val beamFiles = dirFile.list().toList.filter(_.endsWith(".beam"))
    val moduleNames = beamFiles.map(s => s.substring(0, s.length - 5)).sorted

    val infos =
      for {
        mName <- moduleNames
        path = s"$dir/${mName}.beam"
        forms <- rpc.getAbstractForms(path)
      } yield {
        val emptyModule = Module(mName, false, false, false, List(), List(), List(), List(), List(), List())
        forms.foldLeft(emptyModule)(updateModuleWithNewForm)
      }
    AppInfo(libName, infos)
  }
}
