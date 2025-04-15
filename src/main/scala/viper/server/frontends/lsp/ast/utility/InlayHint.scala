// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2024 ETH Zurich.

package viper.silver.ast.utility.lsp

trait HasInlayHints {
  def getInlayHints: Seq[InlayHint]
}

case class InlayHint(
  /** The `_end` is ignored */
  position: RangePosition,
  label: Seq[InlayHintLabelPart],
  kind: Option[InlayHintKind.InlayHintKind],
  paddingLeft: Boolean = false,
  paddingRight: Boolean = false,
) extends HasRangePositions with BelongsToFile {
  override def rangePositions: Seq[RangePosition] = position +: label.flatMap(_.rangePositions)
  override def file = position.file
}

case class InlayHintLabelPart(
  /** The value of this label part. */
  value: String,
  /** The tooltip text when you hover over this label part. Formatted as markdown.
   * This is often not requires when `location` is set, since the tooltip will be
   * handled by a `HoverHint` request at the referenced location.
  */
  tooltip: Option[String] = None,
  location: Option[RangePosition] = None,
) extends HasRangePositions {
  override def rangePositions: Seq[RangePosition] = location.toSeq
}

object InlayHintKind extends Enumeration(1) {
    type InlayHintKind = Value
    val Type, Parameter = Value
}
