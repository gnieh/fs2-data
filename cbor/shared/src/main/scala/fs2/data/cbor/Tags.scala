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
package cbor

/** Contains the definition of tags described in [[https://tools.ietf.org/html/rfc7049#section-2.4 section 2.4 of the RFC]]
  */
object Tags {

  final val StandardDateTimeString: Long = 0L
  final val EpochDateTimeString: Long = 1L
  final val PositiveBigNum: Long = 2L
  final val NegativeBigNum: Long = 3L
  final val DecimalFraction: Long = 4L
  final val BigFloat: Long = 5L

  final val ExpectedBase64UrlEncoding: Long = 21L
  final val ExpectedBase64Encoding: Long = 22L
  final val ExpectedBase16Encoding: Long = 23L
  final val CborDataItem: Long = 24L

  final val URI: Long = 32L
  final val Base64URLEncoded: Long = 33L
  final val Base64Encoded: Long = 34L
  final val RegularExpression: Long = 35L
  final val MimeMessage: Long = 36L

  final val SelfDescribeCbor: Long = 55799L

}
