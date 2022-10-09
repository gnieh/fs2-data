/*
 * Copyright 2022 Lucas Satabin
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

package fs2
package data
package xml
package internals

import cats._
import cats.syntax.all._

private case class ResolverEnv(parent: Option[ResolverEnv], nss: Map[String, String], depth: Int) {
  def resolve(prefix: String): Option[String] =
    nss.get(prefix).orElse(parent.flatMap(_.resolve(prefix)))
  def pop: Option[ResolverEnv] =
    if (depth <= 0)
      parent
    else
      Some(copy(depth = depth - 1))
  def push(env: Map[String, String]): ResolverEnv =
    if (env.isEmpty)
      copy(depth = depth + 1)
    else
      ResolverEnv(Some(this), env, 0)
}

private[xml] class NamespaceResolver[F[_]](implicit F: MonadError[F, Throwable]) {

  private val xmlNSURI = Some("http://www.w3.org/XML/1998/namespace")

  def pipe: Pipe[F, XmlEvent, XmlEvent] =
    _.evalMapAccumulate[F, ResolverEnv, XmlEvent](ResolverEnv(None, Map.empty, 0)) {
      case (env, evt @ XmlEvent.StartTag(name, attrs, _)) =>
        for {
          // create a new scoped env with the potentially declared namespaces
          mapping <- attrs.foldM(Map.empty[String, String]) { (env, attr) =>
            updateNS(env, attr)
          }
          env1 = env.push(mapping)
          // resolve attributes in the new env
          attrs <- attrs.traverse(attr => resolve(env1, attr.name, false).map(n => attr.copy(name = n)))
          // check attribute uiqueness
          _ <- checkDuplicates(attrs)
          // resolve tag name in the new env
          name <- resolve(env1, name, true)
        } yield (env1, evt.copy(name = name, attributes = attrs))
      case (env, evt @ XmlEvent.EndTag(name)) =>
        for {
          // resolve the tag name in the current env
          name <- resolve(env, name, true)
          // close the current environment scope
          env <- env.pop match {
            case Some(env) =>
              F.pure(env)
            case None =>
              F.raiseError(new XmlException(XmlSyntax("GIMatch"), s"unexpected closing tag '</${name.render}>'"))
          }
        } yield (env, evt.copy(name = name))
      case v =>
        F.pure(v)
    }.map(_._2)

  private def resolve(env: ResolverEnv, name: QName, withDefault: Boolean): F[QName] =
    name match {
      case QName(Some(pfx), _) if pfx.take(3).toLowerCase =!= "xml" =>
        // prefixes starting by `xml` are reserved
        env.resolve(pfx) match {
          case None => F.raiseError(new XmlException(NSCPrefixDeclared, s"undeclared namespace $pfx"))
          case uri  => F.pure(name.copy(prefix = uri))
        }
      case QName(None, _) if withDefault =>
        env.resolve("") match {
          case uri @ Some(_) => F.pure(name.copy(prefix = uri))
          case None          => F.pure(name.copy(prefix = xmlNSURI))
        }
      case _ =>
        F.pure(name)
    }

  private def checkDuplicates(attrs: List[Attr]): F[Unit] =
    attrs
      .foldM(Set.empty[QName]) { case (seen, Attr(name, _)) =>
        if (seen.contains(name))
          F.raiseError[Set[QName]](
            new XmlException(NSCAttributesUnique, s"duplicate attribute with resolved name ${name.render}"))
        else
          F.pure(seen + name)
      }
      .void

  private def updateNS(env: Map[String, String], attr: Attr): F[Map[String, String]] =
    attr match {
      case Attr(QName(None, "xmlns"), v) =>
        val uri = v.map(_.render).mkString
        F.pure(env.updated("", uri))
      case Attr(QName(Some("xmlns"), name), v) =>
        val uri = v.map(_.render).mkString
        if (uri.isEmpty)
          F.raiseError(new XmlException(NSCNoPrefixUndeclaring, s"undeclaring namespace $name is not allowed"))
        else
          F.pure(env.updated(name, uri))
      case _ =>
        F.pure(env)
    }

}

private[xml] object NamespaceResolver {
  def apply[F[_]](implicit F: MonadError[F, Throwable]): NamespaceResolver[F] =
    new NamespaceResolver[F]
}
