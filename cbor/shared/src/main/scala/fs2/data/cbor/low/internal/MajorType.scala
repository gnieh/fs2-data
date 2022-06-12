/*
 * Copyright 2019-2022 Lucas Satabin
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

package fs2.data.cbor.low.internal

import scodec.bits._

private[cbor] object MajorType {

  final val PositiveInteger = 0x00
  final val NegativeInteger = 0x01
  final val ByteString = 0x02
  final val TextString = 0x03
  final val Array = 0x04
  final val Map = 0x05
  final val SemanticTag = 0x06
  final val Simple = 0x07

  final val Zero1 = hex"18"
  final val Zero2 = hex"19"
  final val Zero4 = hex"1a"
  final val Zero8 = hex"1b"

  final val One0 = hex"20"
  final val One1 = hex"38"
  final val One2 = hex"39"
  final val One4 = hex"3a"
  final val One8 = hex"3b"

  final val Two0 = hex"40"
  final val Two1 = hex"58"
  final val Two2 = hex"59"
  final val Two4 = hex"5a"
  final val Two8 = hex"5b"
  final val TwoIndefinite = hex"5f"

  final val Three0 = hex"60"
  final val Three1 = hex"78"
  final val Three2 = hex"79"
  final val Three4 = hex"7a"
  final val Three8 = hex"7b"
  final val ThreeIndefinite = hex"7f"

  final val Four0 = hex"80"
  final val Four1 = hex"98"
  final val Four2 = hex"99"
  final val Four4 = hex"9a"
  final val Four8 = hex"9b"
  final val FourIndefinite = hex"9f"

  final val Five0 = hex"a0"
  final val Five1 = hex"b8"
  final val Five2 = hex"b9"
  final val Five4 = hex"ba"
  final val Five8 = hex"bb"
  final val FiveIndefinite = hex"bf"

  final val Six0 = hex"c0"
  final val Six1 = hex"d8"
  final val Six2 = hex"d9"
  final val Six4 = hex"da"
  final val Six8 = hex"db"

  final val Seven0 = hex"e0"
  final val Seven1 = hex"f8"
  final val Seven2 = hex"f9"
  final val Seven4 = hex"fa"
  final val Seven8 = hex"fb"
  final val SevenBreak = hex"ff"
  final val SevenFalse = hex"f4"
  final val SevenTrue = hex"f5"
  final val SevenNull = hex"f6"
  final val SevenUndefined = hex"f7"

}
