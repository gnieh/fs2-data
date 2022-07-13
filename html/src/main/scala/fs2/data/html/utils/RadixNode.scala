package fs2.data.html.utils

import cats.data.NonEmptyList

import scala.annotation.tailrec
import java.util.Arrays

class RadixNode(
    val fsts: Array[Char],
    val prefixes: Array[String],
    val children: Array[RadixNode],
    val word: Boolean
) {
  override def toString(): String =
    s"RadixNode(${fsts.mkString("[", ", ", "]")}, ${children.mkString("[", ", ", "]")}, $word)"

  final def isWord(s: String): Boolean = {
    val strLength = s.length()
    @tailrec
    def loop(offset: Int, tree: RadixNode): Boolean =
      if (offset < strLength) {
        val c = s.charAt(offset)
        val idx = Arrays.binarySearch(tree.fsts, c)
        if (idx >= 0) {
          val prefix = tree.prefixes(idx)
          if (s.startsWith(prefix, offset)) {
            // accept the prefix fo this character
            val children = tree.children(idx)
            loop(offset + prefix.length(), children)
          } else {
            false
          }
        } else {
          false
        }
      } else {
        tree.word
      }
    loop(0, this)
  }

  final def isPrefix(s: String): Boolean = {
    val strLength = s.length()
    @tailrec
    def loop(offset: Int, tree: RadixNode): Int =
      if (offset < strLength) {
        val c = s.charAt(offset)
        val idx = Arrays.binarySearch(tree.fsts, c)
        if (idx >= 0) {
          val prefix = tree.prefixes(idx)
          val sub = s.substring(offset)
          if (prefix.length() >= sub.length()) {
            // prefix is longer than the current string to match
            if (prefix.startsWith(sub))
              strLength
            else
              offset
          } else if (sub.startsWith(prefix)) {
            // accept the prefix fo this character
            val children = tree.children(idx)
            loop(offset + prefix.length(), children)
          } else {
            offset
          }
        } else {
          offset
        }
      } else {
        offset
      }
    loop(0, this) == strLength
  }
}

object RadixNode {

  val empty: RadixNode = new RadixNode(Array.emptyCharArray, Array.empty, Array.empty, false)

  def fromSortedStrings(strings: NonEmptyList[String]): RadixNode = {
    @tailrec
    def groupByNonEmptyPrefix(
        keys: List[String],
        prefix: String,
        current: NonEmptyList[String],
        acc: List[(Char, String, NonEmptyList[String])]
    ): List[(Char, String, NonEmptyList[String])] =
      keys match {
        case key :: keys =>
          val prefixSize = commonPrefix(prefix, key)
          if (prefixSize == 0) {
            // no common prefix, group current suffixes together sorted again
            groupByNonEmptyPrefix(
              keys,
              key,
              NonEmptyList.one(key),
              (prefix(0), prefix, current.map(_.drop(prefix.size)).reverse) :: acc
            )
          } else {
            // clip the prefix to the length, and continue
            groupByNonEmptyPrefix(keys, prefix.take(prefixSize), key :: current, acc)
          }
        case Nil =>
          (prefix(0), prefix, current.map(_.drop(prefix.size)).reverse) :: acc
      }
    NonEmptyList.fromList(strings.filter(_.nonEmpty)) match {
      case Some(nonEmpty) =>
        val grouped =
          groupByNonEmptyPrefix(
            nonEmpty.tail,
            nonEmpty.head,
            NonEmptyList.one(nonEmpty.head),
            Nil
          ).reverse.map { case (fst, prefix, v) => (fst, prefix, fromSortedStrings(v)) }
        val (fsts, prefixes, children) = grouped.unzip3
        new RadixNode(
          fsts.toArray,
          prefixes.toArray,
          children.toArray,
          nonEmpty.size < strings.size
        )
      case None =>
        leaf
    }
  }

  private val leaf = new RadixNode(Array.empty, Array.empty, Array.empty, true)

  private def commonPrefix(s1: String, s2: String): Int = {
    @tailrec
    def loop(idx: Int): Int =
      if (idx >= s1.size || idx >= s2.size) {
        idx
      } else {
        val c1 = s1(idx)
        val c2 = s2(idx)
        if (c1 == c2) {
          loop(idx + 1)
        } else {
          idx
        }
      }
    loop(0)
  }
}
