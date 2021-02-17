/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.whatsapp.analyzer

import erlang.forms.AbstractForm._
import erlang.forms.AbstractType._

object RecursiveTypes {
  import BeamDb.App

  var totalTypeCount = 0
  var recursiveTypeCount = 0
  var totalRecordCount = 0
  var recursiveRecordCount = 0

  var recTypes: Set[Id] = Set()
  var recRecords: Set[String] = Set()

  var sourcedRecTypes: Set[SourceTypeId] = Set()
  var sourcedRecRecords: Set[SourceRec] = Set()

  case class SourceTypeId(file: String, name: String, arity: Int) {
    override def toString: String = s"$file:$name/$arity"
  }

  case class SourceRec(file: String, name: String) {
    override def toString: String = s"$file:#$name{}"
  }

  case class Id(module: String, name: String, arity: Int) {
    override def toString: String = s"$module:$name/$arity"
    def shortId: (String, Int) = (name, arity)
  }
  sealed trait ChainElem
  case class TypeElem(id: Id) extends ChainElem {
    override def toString: String = id.toString
  }
  case class RecordElem(name: String) extends ChainElem {
    override def toString: String = s"#$name{}"
  }

  def main(args: Array[String]): Unit = {
    println("Loading beam DB")
    val otp = args sameElements Array("-otp")
    BeamDb.preload()
    assert(BeamDb.dupMod2App.isEmpty, s"Dupmodes: ${BeamDb.dupMod2App}")
    if (otp) {
      println("*** OTP")
      BeamDb.otpApps.values.toList.sortBy(_.name).foreach(analyzeApp)
    } else {
      println("*** PROJECT")
      BeamDb.projectApps.values.toList.sortBy(_.name).foreach(analyzeApp)
    }
    printStat()
    printRecTypes()
  }

  private def printStat(): Unit = {
    val stat =
      s"""
        |===========
        |TOTAL TYPES:                        $totalTypeCount
        |RECURSIVE TYPES:                    $recursiveTypeCount
        |RECURSIVE TYPES (UNIQUE NAMES):     ${recTypes.map(_.shortId).size}
        |RECURSIVE TYPES (UNIQUE SOURCE):    ${sourcedRecTypes.size}
        |
        |TOTAL RECORDS:                      $totalRecordCount
        |RECURSIVE RECORDS:                  $recursiveRecordCount
        |RECURSIVE RECORDS (UNIQUE NAMES):   ${recRecords.size}
        |RECURSIVE RECORDS (UNIQUE SOURCE):  ${sourcedRecRecords.size}
        |""".stripMargin
    println(stat)
  }

  private def printRecTypes(): Unit = {
    println("===========")
    println()
    println("REC TYPES:")
    sourcedRecTypes.map(_.toString).toList.sorted.foreach(println)

    println()
    println("REC RECORDS:")
    sourcedRecRecords.map(_.toString).toList.sorted.foreach(println)
  }

  private def analyzeApp(app: App): Unit = {
    println(s">>>> APP: ${app.name}")
    if (!CodeDirs.thirdParty.contains(app.name))
      app.modules.sorted.foreach(analyzeModule)
  }

  private def analyzeModule(m: String): Unit = {
    val Some(api) = BeamDb.getModuleApi(m)
    val sourceMap = SourceMap.get(m)
    api.types.foreach { typeDecl =>
      totalTypeCount += 1
      val typeId = Id(api.module, typeDecl.typeName, typeDecl.params.length)
      val file = sourceMap.types((typeDecl.typeName, typeDecl.params.length))
      val fileName = file.substring(file.lastIndexOf('/'))
      val sourceTypeId = SourceTypeId(fileName, typeDecl.typeName, typeDecl.params.length)
      val isRec = isRecursive(typeId, List())
      if (isRec) {
        recursiveTypeCount += 1
        recTypes += typeId
        sourcedRecTypes += sourceTypeId
      }
    }
    api.records.foreach { record =>
      totalRecordCount += 1
      val isRec = isRecursiveRecord(m, record.name, List())
      val file = sourceMap.records(record.name)
      val fileName = file.substring(file.lastIndexOf('/'))
      if (isRec) {
        recursiveRecordCount += 1
        recRecords += record.name
        sourcedRecRecords += SourceRec(fileName, record.name)
      }
    }
  }

  private def isRecursive(fqId: Id, context: List[ChainElem]): Boolean = {
    val thisElem = TypeElem(fqId)
    if (context.nonEmpty && context.last == thisElem) {
      println(s"Rec: $fqId")
      println(s"Context: ${context.reverse.mkString(" -> ")} -> $fqId")
      true
    } else if (context.contains(thisElem)) false
    else {
      val module = fqId.module
      val extendedContext = thisElem :: context
      val moduleApiOpt = BeamDb.getModuleApi(module)
      moduleApiOpt match {
        case Some(moduleApi) =>
          moduleApi.types.find(t => t.typeName == fqId.name && t.params.size == fqId.arity) match {
            case Some(typeDecl) =>
              isRecursiveType(module, typeDecl.abstractType, extendedContext)
            case None =>
              println(s"Cannot find $fqId. Context: $context")
              false
          }
        case None =>
          println(s"Cannot module $module. Context: $context")
          println(s"Cannot find $fqId. Context: $context")
          false
      }
    }
  }

  private def isRecursiveRecord(module: String, recordName: String, context: List[ChainElem]): Boolean = {
    val thisElem = RecordElem(recordName)
    if (context.nonEmpty && context.last == thisElem) {
      println(s"Rec: $module:$thisElem")
      println(s"Context: ${context.reverse.mkString(" -> ")} -> $thisElem")
      true
    } else if (context.contains(thisElem)) false
    else {
      val extendedContext = thisElem :: context
      val Some(moduleInfo) = BeamDb.getModuleApi(module)
      val Some(recordDecl) = moduleInfo.records.find(_.name == recordName)
      val fields = recordDecl.fields
      fields.exists(checkRecordField(module, _, extendedContext))
    }
  }

  private def checkRecordField(module: String, field: AF_FieldDecl, context: List[ChainElem]): Boolean =
    field.tp.exists(isRecursiveType(module, _, context))

  private def isRecursiveType(module: String, tp: AbstractType, context: List[ChainElem]): Boolean =
    tp match {
      case AF_AnnotatedType(_, tp1) =>
        isRecursiveType(module, tp1, context)
      case AF_AtomType(_) =>
        false
      case AF_BitstringType(_) =>
        false
      case AF_EmptyListType =>
        false
      case AF_FunTypeAny =>
        false
      case AF_FunTypeAnyArgs(resultType) =>
        isRecursiveType(module, resultType, context)
      case AF_FunctionType(params, resType) =>
        params.exists { t => isRecursiveType(module, t, context) } || isRecursiveType(module, resType, context)
      case AF_IntegerRangeType(_, _) =>
        false
      case AF_AnyMap =>
        false
      case AF_AssocMap(assocs) =>
        assocs.flatMap(_.types).exists(t => isRecursiveType(module, t, context))
      case _: AF_SingletonIntegerType =>
        false
      case AF_TypeVariable(_) =>
        false
      case AF_PredefinedType(_, params) =>
        params.exists { t => isRecursiveType(module, t, context) }
      case AF_RecordType(recordName, fieldTypes) =>
        if (isRecursiveRecord(module, recordName, context)) {
          true
        } else {
          val fTypes = fieldTypes.map(_.tp)
          val extendedContext = RecordElem(recordName) :: context
          fTypes.exists { t => isRecursiveType(module, t, extendedContext) }
        }
      case AF_RemoteType(remoteModule, tpName, params) =>
        val fqId = Id(remoteModule, tpName, params.length)
        if (isRecursive(fqId, context)) {
          true
        } else {
          params.exists { t => isRecursiveType(module, t, context) }
        }
      case AF_TupleTypeAny =>
        false
      case AF_TupleTypeTyped(params) =>
        params.exists { t => isRecursiveType(module, t, context) }
      case AF_TypeUnion(elems) =>
        elems.exists { t => isRecursiveType(module, t, context) }
      case AF_UserDefinedType(typeName, params) =>
        val typId = Id(module, typeName, params.length)
        if (isRecursive(typId, context)) true
        else params.exists { isRecursiveType(module, _, context) }
    }
}
