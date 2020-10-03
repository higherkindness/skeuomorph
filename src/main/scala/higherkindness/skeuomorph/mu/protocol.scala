/*
 * Copyright 2018-2020 47 Degrees Open Source <https://www.47deg.com>
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

package higherkindness.skeuomorph.mu

import higherkindness.skeuomorph.protobuf
import higherkindness.skeuomorph.protobuf.ProtobufF
import higherkindness.skeuomorph.avro
import higherkindness.skeuomorph.avro.AvroF
import higherkindness.skeuomorph.mu.Service.OperationType
import higherkindness.skeuomorph.mu.Transform.transformAvro
import higherkindness.skeuomorph.mu.Transform.transformProto
import higherkindness.droste._

sealed trait SerializationType extends Product with Serializable
object SerializationType {
  case object Protobuf       extends SerializationType
  case object Avro           extends SerializationType
  case object AvroWithSchema extends SerializationType
}

sealed trait CompressionType extends Product with Serializable
object CompressionType {
  case object Gzip     extends CompressionType
  case object Identity extends CompressionType
}

final case class IdiomaticEndpoints(pkg: Option[String], value: Boolean)

final case class Protocol[T](
    name: Option[String],
    pkg: Option[String],
    options: List[(String, String)],
    declarations: List[T],
    services: List[Service[T]],
    imports: List[DependentImport[T]]
)
object Protocol {

  /**
   * create a [[higherkindness.skeuomorph.mu.Protocol]] from a [[higherkindness.skeuomorph.avro.Protocol]]
   */
  def fromAvroProtocol[T, U](compressionType: CompressionType, useIdiomaticEndpoints: Boolean)(
      proto: avro.Protocol[T]
  )(implicit T: Basis[AvroF, T], U: Basis[MuF, U]): Protocol[U] = {

    val toMu: T => U = scheme.cata(transformAvro[U].algebra)
    val toOperation: avro.Protocol.Message[T] => Service.Operation[U] =
      msg =>
        Service.Operation(
          msg.name,
          request = OperationType(toMu(msg.request), false),
          response = OperationType(toMu(msg.response), false)
        )
    val services = if(proto.messages.isEmpty) {
      Nil
    } else {
      List(
        Service(
          proto.name,
          SerializationType.Avro,
          compressionType,
          IdiomaticEndpoints(proto.namespace, useIdiomaticEndpoints),
          proto.messages.map(toOperation)
        )
      )
    }

    Protocol(
      name = None,
      pkg = proto.namespace,
      options = Nil,
      declarations = proto.types.map(toMu),
      services = services,
      imports = Nil
    )
  }

  def fromProtobufProto[T, U](compressionType: CompressionType, useIdiomaticEndpoints: Boolean)(
      protocol: protobuf.Protocol[T]
  )(implicit T: Basis[ProtobufF, T], U: Basis[MuF, U]): Protocol[U] = {
    val toMu: T => U = scheme.cata(transformProto[U].algebra)
    val toOperation: protobuf.Protocol.Operation[T] => Service.Operation[U] =
      msg =>
        Service.Operation(
          name = msg.name,
          request = OperationType(toMu(msg.request), msg.requestStreaming),
          response = OperationType(toMu(msg.response), msg.responseStreaming)
        )

    val toImports: DependentImport[T] => DependentImport[U] =
      imp => DependentImport(imp.pkg, imp.protocol, toMu(imp.tpe))

    new Protocol[U](
      name = Some(protocol.name),
      pkg = Option(protocol.pkg),
      options = protocol.options,
      declarations = protocol.declarations.map(toMu),
      services = protocol.services
        .map(s =>
          new Service[U](
            s.name,
            SerializationType.Protobuf,
            compressionType,
            IdiomaticEndpoints(Option(protocol.pkg), useIdiomaticEndpoints),
            s.operations.map(toOperation)
          )
        ),
      imports = protocol.imports.map(toImports)
    )

  }

}

final case class Service[T](
    name: String,
    serializationType: SerializationType,
    compressionType: CompressionType,
    idiomaticEndpoints: IdiomaticEndpoints,
    operations: List[Service.Operation[T]]
)
object Service {
  final case class OperationType[T](tpe: T, stream: Boolean)
  final case class Operation[T](name: String, request: OperationType[T], response: OperationType[T])
}

final case class DependentImport[T](pkg: String, protocol: String, tpe: T)
