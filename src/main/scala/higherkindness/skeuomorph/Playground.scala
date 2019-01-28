/*
 * Copyright 2018-2019 47 Degrees, LLC. <http://www.47deg.com>
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

package higherkindness.skeuomorph

import cats.effect.IO
import com.google.protobuf.descriptor.DescriptorProto.ReservedRange
import com.google.protobuf.descriptor.FieldDescriptorProto.Label._
import com.google.protobuf.descriptor.FieldDescriptorProto.Type._
import com.google.protobuf.descriptor._
import higherkindness.skeuomorph.protobuf.ParseProto.ProtoSource
//import scalapb.descriptors.{BaseDescriptor, FileDescriptor}
import protobuf.Optimize._
import mu.Optimize._
import scalapb.UnknownFieldSet

object Playground extends App {
  // An example of the contract Skeuomorph will support
  import higherkindness.skeuomorph.mu.{MuF, Transform}
  import higherkindness.skeuomorph.protobuf._
  import qq.droste.data.Mu
  import qq.droste.data.Mu._
  import qq.droste.scheme

  val readFile: IO[FileDescriptor] = ParseProto
    .parseProto[IO]
    .parse(ProtoSource("sampleProto.proto", "/Users/rafaparadela/code/47/skeuomorph/src/main/resources"))

  val fileDescriptor: FileDescriptor = readFile.unsafeRunSync()

  // This step is new and is actually important for creating valid data
  val optimizeProtobufF: Mu[ProtobufF] => Mu[ProtobufF] = repeatedTypes

  val parseProto: BaseDescriptor => Mu[ProtobufF] =
    scheme.ana(ProtobufF.fromProtobuf) andThen optimizeProtobufF

  val printProto: Mu[ProtobufF] => String =
    print.printSchema.print _

  val roundTrip: String = printProto(parseProto(fileDescriptor))

  // Render Proto file
//  println(roundTrip)

  val protoToMu: Mu[ProtobufF] => Mu[MuF] =
    scheme.cata(Transform.transformProto.algebra) andThen nestedNamedTypes andThen knownCoproductTypes

  val transform: BaseDescriptor => Mu[MuF] = parseProto andThen protoToMu

  val printAsScala: Mu[MuF] => String =
    higherkindness.skeuomorph.mu.print.schema.print _

  // Render Scala
//  println(printAsScala(transform(fileDescriptor)))

  val a = new FileDescriptorProto(
    name = Some("sampleProto.proto"),
    `package` = Some("com.book"),
    dependency = Vector("author3.proto"),
    publicDependency = Vector(),
    weakDependency = Vector(),
    messageType = Vector(
      DescriptorProto(
        name = Some("Book"),
        field = Vector(
          FieldDescriptorProto(
            name = Some("isbn"),
            number = Some(1),
            label = Some(LABEL_OPTIONAL),
            `type` = Some(TYPE_INT64),
            typeName = None,
            extendee = None,
            defaultValue = None,
            oneofIndex = None,
            jsonName = Some("isbn"),
            options = None
          ),
          FieldDescriptorProto(
            name = Some("title"),
            number = Some(2),
            label = Some(LABEL_OPTIONAL),
            `type` = Some(TYPE_STRING),
            typeName = None,
            extendee = None,
            defaultValue = None,
            oneofIndex = None,
            jsonName = Some("title"),
            options = None
          ),
          FieldDescriptorProto(
            name = Some("author"),
            number = Some(3),
            label = Some(LABEL_REPEATED),
            `type` = Some(TYPE_MESSAGE),
            typeName = Some(".com.book.Author"),
            extendee = None,
            defaultValue = None,
            oneofIndex = None,
            jsonName = Some("author"),
            options = None
          ),
          FieldDescriptorProto(
            name = Some("binding_type"),
            number = Some(9),
            label = Some(LABEL_OPTIONAL),
            `type` = Some(TYPE_ENUM),
            typeName = Some(".com.book.BindingType"),
            extendee = None,
            defaultValue = None,
            oneofIndex = None,
            jsonName = Some("bindingType"),
            options = None
          )
        ),
        extension = Nil,
        nestedType = Vector(),
        enumType = Vector(),
        extensionRange = Vector(),
        oneofDecl = Vector(),
        options = None,
        reservedRange =
          Vector(ReservedRange(Some(4), Some(5)), ReservedRange(Some(8), Some(9)), ReservedRange(Some(12), Some(16))),
        reservedName = Vector()
      ),
      DescriptorProto(
        name = Some("GetBookRequest"),
        field = Vector(
          FieldDescriptorProto(
            name = Some("isbn"),
            number = Some(1),
            label = Some(LABEL_OPTIONAL),
            `type` = Some(TYPE_INT64),
            typeName = None,
            extendee = None,
            defaultValue = None,
            oneofIndex = None,
            jsonName = Some("isbn"),
            options = None
          )
        ),
        extension = Vector(),
        nestedType = Vector(),
        enumType = Vector(),
        extensionRange = Vector(),
        oneofDecl = Vector(),
        options = None,
        reservedRange = Vector(),
        reservedName = Vector()
      ),
      DescriptorProto(
        name = Some("GetBookViaAuthor"),
        field = Vector(
          FieldDescriptorProto(
            name = Some("author"),
            number = Some(1),
            label = Some(LABEL_OPTIONAL),
            `type` = Some(TYPE_MESSAGE),
            typeName = Some(".com.book.Author"),
            extendee = None,
            defaultValue = None,
            oneofIndex = None,
            jsonName = Some("author"),
            options = None
          )
        ),
        extension = Vector(),
        nestedType = Vector(),
        enumType = Vector(),
        extensionRange = Vector(),
        oneofDecl = Vector(),
        options = None,
        reservedRange = Vector(),
        reservedName = Vector()
      ),
      DescriptorProto(
        name = Some("BookStore"),
        field = Vector(
          FieldDescriptorProto(
            name = Some("name"),
            number = Some(1),
            label = Some(LABEL_OPTIONAL),
            `type` = Some(TYPE_STRING),
            typeName = None,
            extendee = None,
            defaultValue = None,
            oneofIndex = None,
            jsonName = Some("name"),
            options = None
          ),
          FieldDescriptorProto(
            name = Some("books"),
            number = Some(2),
            label = Some(LABEL_REPEATED),
            `type` = Some(TYPE_MESSAGE),
            typeName = Some(".com.book.BookStore.BooksEntry"),
            extendee = None,
            defaultValue = None,
            oneofIndex = None,
            jsonName = Some("books"),
            options = None
          ),
          FieldDescriptorProto(
            name = Some("genres"),
            number = Some(3),
            label = Some(LABEL_REPEATED),
            `type` = Some(TYPE_ENUM),
            typeName = Some(".com.book.Genre"),
            extendee = None,
            defaultValue = None,
            oneofIndex = None,
            jsonName = Some("genres"),
            options = None
          ),
          FieldDescriptorProto(
            name = Some("credit_card_number"),
            number = Some(4),
            label = Some(LABEL_OPTIONAL),
            `type` = Some(TYPE_INT64),
            typeName = None,
            extendee = None,
            defaultValue = None,
            oneofIndex = Some(0),
            jsonName = Some("creditCardNumber"),
            options = None
          ),
          FieldDescriptorProto(
            name = Some("cash"),
            number = Some(5),
            label = Some(LABEL_OPTIONAL),
            `type` = Some(TYPE_INT32),
            typeName = None,
            extendee = None,
            defaultValue = None,
            oneofIndex = Some(0),
            jsonName = Some("cash"),
            options = None
          ),
          FieldDescriptorProto(
            name = Some("iou_note"),
            number = Some(6),
            label = Some(LABEL_OPTIONAL),
            `type` = Some(TYPE_STRING),
            typeName = None,
            extendee = None,
            defaultValue = None,
            oneofIndex = Some(0),
            jsonName = Some("iouNote"),
            options = None
          ),
          FieldDescriptorProto(
            name = Some("barter"),
            number = Some(7),
            label = Some(LABEL_OPTIONAL),
            `type` = Some(TYPE_MESSAGE),
            typeName = Some(".com.book.Book"),
            extendee = None,
            defaultValue = None,
            oneofIndex = Some(0),
            jsonName = Some("barter"),
            options = None
          )
        ),
        extension = Vector(),
        nestedType = Vector(
          DescriptorProto(
            name = Some("BooksEntry"),
            field = Vector(
              FieldDescriptorProto(
                name = Some("key"),
                number = Some(1),
                label = Some(LABEL_OPTIONAL),
                `type` = Some(TYPE_INT64),
                typeName = None,
                extendee = None,
                defaultValue = None,
                oneofIndex = None,
                jsonName = Some("key"),
                options = None
              ),
              FieldDescriptorProto(
                name = Some("value"),
                number = Some(2),
                label = Some(LABEL_OPTIONAL),
                `type` = Some(TYPE_STRING),
                typeName = None,
                extendee = None,
                defaultValue = None,
                oneofIndex = None,
                jsonName = Some("value"),
                options = None
              )
            ),
            extension = Vector(),
            nestedType = Vector(),
            enumType = Vector(),
            extensionRange = Vector(),
            oneofDecl = Vector(),
            options = Some(MessageOptions(None, None, None, Some(true), Vector(), UnknownFieldSet(Map()))),
            reservedRange = Vector(),
            reservedName = Vector()
          )),
        enumType = Vector(),
        extensionRange = Vector(),
        oneofDecl = Vector(OneofDescriptorProto(Some("payment_method"), None)),
        options = None,
        reservedRange = Vector(),
        reservedName = Vector()
      )
    ),
    enumType = Vector(
      EnumDescriptorProto(
        name = Some("Genre"),
        value = Vector(
          EnumValueDescriptorProto(Some("UNKNOWN"), Some(0), None),
          EnumValueDescriptorProto(Some("SCIENCE_FICTION"), Some(1), None),
          EnumValueDescriptorProto(Some("SPECULATIVE_FICTION"), Some(1), None),
          EnumValueDescriptorProto(Some("POETRY"), Some(2), None),
          EnumValueDescriptorProto(Some("SCI_FI"), Some(1), None)
        ),
        options = Some(EnumOptions(Some(true), None, Vector(), UnknownFieldSet(Map())))
      ),
      EnumDescriptorProto(
        name = Some("BindingType"),
        value = Vector(
          EnumValueDescriptorProto(Some("HARDCOVER"), Some(0), None),
          EnumValueDescriptorProto(Some("PAPERBACK"), Some(1), None)),
        options = None
      )
    ),
    service = Vector(
      ServiceDescriptorProto(
        name = Some("BookService"),
        method = Vector(
          MethodDescriptorProto(
            name = Some("GetBook"),
            inputType = Some(".com.book.GetBookRequest"),
            outputType = Some(".com.book.Book"),
            options = Some(MethodOptions(None, None, Vector(), UnknownFieldSet(Map()))),
            clientStreaming = None,
            serverStreaming = None
          ),
          MethodDescriptorProto(
            name = Some("GetBooksViaAuthor"),
            inputType = Some(".com.book.GetBookViaAuthor"),
            outputType = Some(".com.book.Book"),
            options = Some(MethodOptions(None, None, Vector(), UnknownFieldSet(Map()))),
            clientStreaming = None,
            serverStreaming = Some(true)
          ),
          MethodDescriptorProto(
            name = Some("GetGreatestBook"),
            inputType = Some(".com.book.GetBookRequest"),
            outputType = Some(".com.book.Book"),
            options = Some(MethodOptions(None, None, Vector(), UnknownFieldSet(Map()))),
            clientStreaming = Some(true),
            serverStreaming = None
          ),
          MethodDescriptorProto(
            name = Some("GetBooks"),
            inputType = Some(".com.book.GetBookRequest"),
            outputType = Some(".com.book.Book"),
            options = Some(MethodOptions(None, None, Vector(), UnknownFieldSet(Map()))),
            clientStreaming = Some(true),
            serverStreaming = Some(true)
          )
        ),
        options = None
      )),
    extension = Vector(),
    options = None,
    sourceCodeInfo = None,
    syntax = Some("proto3")
  )

}
