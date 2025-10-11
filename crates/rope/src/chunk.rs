use crate::{OffsetUtf16, Point, PointUtf16, TextSummary, Unclipped};
use arrayvec::ArrayString;
use bitvec::prelude::*;
use std::{cmp, ops::Range};
use sum_tree::Bias;
use unicode_segmentation::GraphemeCursor;
use util::debug_panic;

pub(crate) const MIN_BASE: usize = if cfg!(test) { 6 } else { 64 };
pub(crate) const MAX_BASE: usize = MIN_BASE * 2;

#[derive(Clone, Debug, Default)]
pub struct Chunk {
    /// If bit[i] is set, then the character at index i is the start of a UTF-8 character in the
    /// text.
    chars: u128,
    /// The number of set bits is the number of UTF-16 code units it would take to represent the
    /// text.
    ///
    /// Bit[i] is set if text[i] is the start of a UTF-8 character. If the character would
    /// take two UTF-16 code units, then bit[i+1] is also set. (Rust chars never take more
    /// than two UTF-16 code units.)
    chars_utf16: u128,
    /// If bit[i] is set, then the character at index i is an ascii newline.
    ///
    /// Only bits `0..text.len()` are valid.
    // Note: This creates an array of usizes. It can't currently be u128 due to
    // <https://github.com/ferrilab/bitvec/issues/76>
    newlines: BitArr!(for MAX_BASE, in usize),
    /// If bit[i] is set, then the character at index i is an ascii tab.
    pub(crate) tabs: u128,
    pub text: ArrayString<MAX_BASE>,
}

impl Chunk {
    #[inline(always)]
    pub fn new(text: &str) -> Self {
        let mut this = Chunk::default();
        this.push_str(text);
        this
    }

    #[inline(always)]
    pub fn push_str(&mut self, text: &str) {
        for (char_ix, c) in text.char_indices() {
            let ix = self.text.len() + char_ix;
            self.chars |= 1 << ix;
            self.chars_utf16 |= 1 << ix;
            self.chars_utf16 |= (c.len_utf16() as u128) << ix;
            self.newlines.set(ix, c == '\n');
            self.tabs |= ((c == '\t') as u128) << ix;
        }
        self.text.push_str(text);
    }

    #[inline(always)]
    pub fn append(&mut self, slice: ChunkSlice) {
        if slice.is_empty() {
            return;
        };

        let base_ix = self.text.len();
        self.chars |= slice.chars << base_ix;
        self.chars_utf16 |= slice.chars_utf16 << base_ix;
        self.newlines[base_ix..] |= &slice.newlines;
        self.tabs |= slice.tabs << base_ix;
        self.text.push_str(slice.text);
        debug_assert!(!self.newlines[self.text.len()..].any());
    }

    #[inline(always)]
    pub fn as_slice(&self) -> ChunkSlice<'_> {
        ChunkSlice {
            chars: self.chars,
            chars_utf16: self.chars_utf16,
            newlines: self.newlines,
            tabs: self.tabs,
            text: &self.text,
        }
    }

    #[inline(always)]
    pub fn slice(&self, range: Range<usize>) -> ChunkSlice<'_> {
        self.as_slice().slice(range)
    }

    #[inline(always)]
    pub fn chars(&self) -> u128 {
        self.chars
    }
}

#[derive(Clone, Copy, Debug)]
pub struct ChunkSlice<'a> {
    chars: u128,
    chars_utf16: u128,
    newlines: BitArr!(for MAX_BASE, in usize),
    tabs: u128,
    text: &'a str,
}

impl Into<Chunk> for ChunkSlice<'_> {
    fn into(self) -> Chunk {
        Chunk {
            chars: self.chars,
            chars_utf16: self.chars_utf16,
            newlines: self.newlines,
            tabs: self.tabs,
            text: self.text.try_into().unwrap(),
        }
    }
}

impl<'a> ChunkSlice<'a> {
    #[inline(always)]
    pub fn is_empty(&self) -> bool {
        self.text.is_empty()
    }

    #[inline(always)]
    pub fn is_char_boundary(self, offset: usize) -> bool {
        self.text.is_char_boundary(offset)
    }

    #[inline(always)]
    pub fn split_at(self, mid: usize) -> (ChunkSlice<'a>, ChunkSlice<'a>) {
        if mid == MAX_BASE {
            let left = self;
            let right = ChunkSlice {
                chars: 0,
                chars_utf16: 0,
                newlines: BitArray::ZERO,
                tabs: 0,
                text: "",
            };
            (left, right)
        } else {
            let mask = (1u128 << mid) - 1;
            let (left_text, right_text) = self.text.split_at(mid);
            let left = ChunkSlice {
                chars: self.chars & mask,
                chars_utf16: self.chars_utf16 & mask,
                newlines: BitArray::ZERO | &self.newlines[..mid],
                tabs: self.tabs & mask,
                text: left_text,
            };
            let right = ChunkSlice {
                chars: self.chars >> mid,
                chars_utf16: self.chars_utf16 >> mid,
                newlines: BitArray::ZERO | &self.newlines[mid..],
                tabs: self.tabs >> mid,
                text: right_text,
            };
            (left, right)
        }
    }

    #[inline(always)]
    pub fn slice(self, range: Range<usize>) -> Self {
        let mask = if range.end == MAX_BASE {
            u128::MAX
        } else {
            debug_assert!(
                self.is_char_boundary(range.end),
                "Invalid range end {} in {:?}",
                range.end,
                self
            );
            (1u128 << range.end) - 1
        };
        if range.start == MAX_BASE {
            Self {
                chars: 0,
                chars_utf16: 0,
                newlines: BitArray::ZERO,
                tabs: 0,
                text: "",
            }
        } else {
            debug_assert!(
                self.is_char_boundary(range.start),
                "Invalid range start {} in {:?}",
                range.start,
                self
            );
            Self {
                chars: (self.chars & mask) >> range.start,
                chars_utf16: (self.chars_utf16 & mask) >> range.start,
                newlines: BitArray::ZERO | &self.newlines[range.clone()],
                tabs: (self.tabs & mask) >> range.start,
                text: &self.text[range],
            }
        }
    }

    #[inline(always)]
    pub fn text_summary(&self) -> TextSummary {
        let mut chars = 0;
        let (longest_row, longest_row_chars) = self.longest_row(&mut chars);
        TextSummary {
            len: self.len(),
            chars,
            len_utf16: self.len_utf16(),
            lines: self.lines(),
            first_line_chars: self.first_line_chars(),
            last_line_chars: self.last_line_chars(),
            last_line_len_utf16: self.last_line_len_utf16(),
            longest_row,
            longest_row_chars,
        }
    }

    /// Get length in bytes
    #[inline(always)]
    pub fn len(&self) -> usize {
        self.text.len()
    }

    /// Get length in UTF-16 code units
    #[inline(always)]
    pub fn len_utf16(&self) -> OffsetUtf16 {
        OffsetUtf16(self.chars_utf16.count_ones() as usize)
    }

    /// Get point representing number of lines and length in bytes of last line
    #[inline(always)]
    pub fn lines(&self) -> Point {
        debug_assert!(
            !self.newlines[self.text.len()..].any(),
            "newline bits are set after end of text"
        );
        let row = self.newlines.count_ones();
        let column = self.last_line_range().len();
        Point::new(row as u32, column as u32)
    }

    /// Return the range of byte offsets after the last newline.
    #[inline(always)]
    fn last_line_range(&self) -> Range<usize> {
        debug_assert!(
            !self.newlines[self.text.len()..].any(),
            "newline bits are set after end of text"
        );
        let len = self.text.len();
        let line_start = self.newlines.iter_ones().last().map_or(0, |i| i + 1);
        line_start..len
    }

    /// Return the length in bytes of the first line, not including the newline.
    #[inline(always)]
    fn first_line_bytes(&self) -> usize {
        self.newlines[..self.text.len()].leading_zeros()
    }

    /// Get number of chars in first line
    #[inline(always)]
    pub fn first_line_chars(&self) -> u32 {
        if !self.newlines.any() {
            self.chars.count_ones()
        } else {
            let mask = (1u128 << self.first_line_bytes()) - 1;
            (self.chars & mask).count_ones()
        }
    }

    /// Get number of chars in last line
    #[inline(always)]
    pub fn last_line_chars(&self) -> u32 {
        if !self.newlines.any() {
            self.chars.count_ones()
        } else {
            let last_line_bytes = self.last_line_range();
            if last_line_bytes.is_empty() {
                return 0;
            }
            // mask off everything before the start, i.e. keep the higher bits
            // TODO: This is a good example of where testing with only small chunks
            // misses a case that can happen in real use: this will panic if
            // the last_line is 128 bytes long; it won't panic if MAX_BASE=6.
            debug_assert!(
                last_line_bytes.start < MAX_BASE,
                "last_line_bytes {last_line_bytes:?} too large for {self:?}"
            );
            let mask = u128::MAX << last_line_bytes.start;
            (self.chars & mask).count_ones()
        }
    }

    /// Get number of UTF-16 code units in last line
    #[inline(always)]
    pub fn last_line_len_utf16(&self) -> u32 {
        if !self.newlines.any() {
            self.chars_utf16.count_ones()
        } else {
            let last_line_bytes = self.last_line_range();
            if last_line_bytes.is_empty() {
                return 0;
            }
            debug_assert!(
                last_line_bytes.start < MAX_BASE,
                "last_line_bytes {last_line_bytes:?} too large for {self:?}"
            );
            let mask = u128::MAX << self.last_line_range().start;
            (self.chars_utf16 & mask).count_ones()
        }
    }

    /// Get the longest row in the chunk and its length in characters.
    /// Calculate the total number of characters in the chunk along the way.
    #[inline(always)]
    pub fn longest_row(&self, total_chars: &mut usize) -> (u32, u32) {
        let mut chars = self.chars;
        let mut newlines = &self.newlines[..self.text.len()];
        *total_chars = 0;
        let mut row = 0;
        let mut longest_row = 0;
        let mut longest_row_chars = 0;
        while let Some(newline_ix) = newlines.iter_ones().next() {
            let row_chars = (chars & ((1 << newline_ix) - 1)).count_ones();
            *total_chars += row_chars as usize;
            if row_chars > longest_row_chars {
                longest_row = row;
                longest_row_chars = row_chars;
            }

            chars >>= newline_ix;
            chars >>= 1;
            row += 1;
            *total_chars += 1;
            if newline_ix + 1 >= newlines.len() {
                break;
            } else {
                newlines = &newlines[newline_ix + 1..];
            }
        }

        let row_chars = chars.count_ones();
        *total_chars += row_chars as usize;
        if row_chars > longest_row_chars {
            (row, row_chars)
        } else {
            (longest_row, longest_row_chars as u32)
        }
    }

    /// Translate a byte-indexed offset into a row/column point.
    ///
    /// If the offset is at a newline, the point is at the end of the line.
    #[inline(always)]
    pub fn offset_to_point(&self, offset: usize) -> Point {
        let newlines = &self.newlines[..offset]; // don't include newline at offset
        if let Some(newline_ix) = newlines.iter_ones().last() {
            let row = newlines.count_ones();
            let column = offset - newline_ix - 1; // can't underflow because we excluded newline at offset, so newline_ix < offset
            Point::new(row as u32, column as u32)
        } else {
            Point::new(0, offset as u32)
        }
    }

    #[inline(always)]
    pub fn point_to_offset(&self, point: Point) -> usize {
        if point.row > self.lines().row {
            debug_panic!(
                "point {:?} extends beyond rows for string {:?}",
                point,
                self.text
            );
            return self.len();
        }

        let row_offset_range = self.offset_range_for_row(point.row);
        if point.column > row_offset_range.len() as u32 {
            debug_panic!(
                "point {:?} extends beyond row for string {:?}",
                point,
                self.text
            );
            row_offset_range.end
        } else {
            row_offset_range.start + point.column as usize
        }
    }

    #[inline(always)]
    pub fn offset_to_offset_utf16(&self, offset: usize) -> OffsetUtf16 {
        let mask = if offset == MAX_BASE {
            u128::MAX
        } else {
            (1u128 << offset) - 1
        };
        OffsetUtf16((self.chars_utf16 & mask).count_ones() as usize)
    }

    #[inline(always)]
    pub fn offset_utf16_to_offset(&self, target: OffsetUtf16) -> usize {
        if target.0 == 0 {
            0
        } else {
            let ix = nth_set_bit(self.chars_utf16, target.0) + 1;
            if ix == MAX_BASE {
                MAX_BASE
            } else {
                let utf8_additional_len = cmp::min(
                    (self.chars_utf16 >> ix).trailing_zeros() as usize,
                    self.text.len() - ix,
                );
                ix + utf8_additional_len
            }
        }
    }

    #[inline(always)]
    pub fn offset_to_point_utf16(&self, offset: usize) -> PointUtf16 {
        // NB: This currently tolerates offset not being on a char boundary,
        // but I'm not clear if that's desirable. -- @sourcefrog
        let mask = if offset == MAX_BASE {
            u128::MAX
        } else {
            (1u128 << offset) - 1
        };
        debug_assert!(offset <= self.text.len());
        // debug_assert!(
        //     self.is_char_boundary(offset),
        //     "Invalid offset {} in {:?}",
        //     offset,
        //     self.text
        // );
        let row = self.newlines[..offset].count_ones();
        let line_start = self.newlines[..offset]
            .iter_ones()
            .last()
            .map_or(0, |i| i + 1);
        debug_assert!(line_start <= offset);
        debug_assert!(line_start == 0 || self.text.as_bytes()[line_start - 1] == b'\n');
        let column = ((self.chars_utf16 & mask) >> line_start).count_ones();
        PointUtf16::new(row as u32, column)
    }

    #[inline(always)]
    pub fn point_to_point_utf16(&self, point: Point) -> PointUtf16 {
        self.offset_to_point_utf16(self.point_to_offset(point))
    }

    #[inline(always)]
    pub fn point_utf16_to_offset(&self, point: PointUtf16, clip: bool) -> usize {
        let lines = self.lines(); // TODO: We don't need the last line length?
        if point.row > lines.row {
            if !clip {
                debug_panic!(
                    "point {:?} is beyond this chunk's extent {:?}",
                    point,
                    self.text
                );
            }
            return self.len();
        }
        let row_offset_range = self.offset_range_for_row(point.row);
        let line = self.slice(row_offset_range.clone());
        if point.column > line.last_line_len_utf16() {
            if !clip {
                debug_panic!(
                    "point {point:?} is beyond the end of the line in chunk {:?}, line {line:?}, last line len_utf16 {}, row_offset_range {row_offset_range:?}",
                    self.text,
                    line.last_line_len_utf16()
                );
            }
            return line.len();
        }

        let mut offset = row_offset_range.start;
        if point.column > 0 {
            offset += line.offset_utf16_to_offset(OffsetUtf16(point.column as usize));
            if !self.text.is_char_boundary(offset) {
                offset -= 1;
                while !self.text.is_char_boundary(offset) {
                    offset -= 1;
                }
                if !clip {
                    debug_panic!(
                        "point {:?} is within character in chunk {:?}",
                        point,
                        self.text,
                    );
                }
            }
        }
        offset
    }

    #[inline(always)]
    pub fn unclipped_point_utf16_to_point(&self, point: Unclipped<PointUtf16>) -> Point {
        let max_point = self.lines();
        if point.0.row > max_point.row {
            return max_point;
        }

        let row_offset_range = self.offset_range_for_row(point.0.row);
        let line = self.slice(row_offset_range);
        if point.0.column == 0 {
            Point::new(point.0.row, 0)
        } else if point.0.column >= line.len_utf16().0 as u32 {
            Point::new(point.0.row, line.len() as u32)
        } else {
            let mut column = line.offset_utf16_to_offset(OffsetUtf16(point.0.column as usize));
            while !line.text.is_char_boundary(column) {
                column -= 1;
            }
            Point::new(point.0.row, column as u32)
        }
    }

    #[inline(always)]
    pub fn clip_point(&self, point: Point, bias: Bias) -> Point {
        let max_point = self.lines();
        if point.row > max_point.row {
            return max_point;
        }

        let line = self.slice(self.offset_range_for_row(point.row));
        if point.column == 0 {
            point
        } else if point.column >= line.len() as u32 {
            Point::new(point.row, line.len() as u32)
        } else {
            let mut column = point.column as usize;
            let bytes = line.text.as_bytes();
            if bytes[column - 1] < 128 && bytes[column] < 128 {
                return Point::new(point.row, column as u32);
            }

            let mut grapheme_cursor = GraphemeCursor::new(column, bytes.len(), true);
            loop {
                if line.is_char_boundary(column)
                    && grapheme_cursor.is_boundary(line.text, 0).unwrap_or(false)
                {
                    break;
                }

                match bias {
                    Bias::Left => column -= 1,
                    Bias::Right => column += 1,
                }
                grapheme_cursor.set_cursor(column);
            }
            Point::new(point.row, column as u32)
        }
    }

    #[inline(always)]
    pub fn clip_point_utf16(&self, point: Unclipped<PointUtf16>, bias: Bias) -> PointUtf16 {
        let max_point = self.lines();
        if point.0.row > max_point.row {
            PointUtf16::new(max_point.row, self.last_line_len_utf16())
        } else {
            let line = self.slice(self.offset_range_for_row(point.0.row));
            let column = line.clip_offset_utf16(OffsetUtf16(point.0.column as usize), bias);
            PointUtf16::new(point.0.row, column.0 as u32)
        }
    }

    #[inline(always)]
    pub fn clip_offset_utf16(&self, target: OffsetUtf16, bias: Bias) -> OffsetUtf16 {
        if target == OffsetUtf16::default() {
            OffsetUtf16::default()
        } else if target >= self.len_utf16() {
            self.len_utf16()
        } else {
            let mut offset = self.offset_utf16_to_offset(target);
            while !self.text.is_char_boundary(offset) {
                if bias == Bias::Left {
                    offset -= 1;
                } else {
                    offset += 1;
                }
            }
            self.offset_to_offset_utf16(offset)
        }
    }

    /// Return the byte offset range for the given row.
    ///
    /// The range does not include the newline character.
    #[inline(always)]
    fn offset_range_for_row(&self, row: u32) -> Range<usize> {
        let newlines = &self.newlines[..self.text.len()];
        let row_start = if row > 0 {
            newlines
                .iter_ones()
                .nth((row - 1) as usize)
                .expect("row out of range")
                + 1
        } else {
            0
        };
        let row_len = newlines[row_start..].leading_zeros() as u32;
        let range = row_start..row_start + row_len as usize;
        debug_assert!(!self.newlines[range.clone()].any());
        range
    }

    #[inline(always)]
    pub fn tabs(&self) -> Tabs {
        Tabs {
            tabs: self.tabs,
            chars: self.chars,
        }
    }
}

pub struct Tabs {
    tabs: u128,
    chars: u128,
}

#[derive(Debug, PartialEq, Eq)]
pub struct TabPosition {
    pub byte_offset: usize,
    pub char_offset: usize,
}

impl Iterator for Tabs {
    type Item = TabPosition;

    fn next(&mut self) -> Option<Self::Item> {
        if self.tabs == 0 {
            return None;
        }

        let tab_offset = self.tabs.trailing_zeros() as usize;
        let chars_mask = (1 << tab_offset) - 1;
        let char_offset = (self.chars & chars_mask).count_ones() as usize;

        // Since tabs are 1 byte the tab offset is the same as the byte offset
        let position = TabPosition {
            byte_offset: tab_offset,
            char_offset,
        };
        // Remove the tab we've just seen
        self.tabs ^= 1 << tab_offset;

        Some(position)
    }
}

/// Finds the n-th bit that is set to 1.
#[inline(always)]
fn nth_set_bit(v: u128, n: usize) -> usize {
    let low = v as u64;
    let high = (v >> 64) as u64;

    let low_count = low.count_ones() as usize;
    if n > low_count {
        64 + nth_set_bit_u64(high, (n - low_count) as u64) as usize
    } else {
        nth_set_bit_u64(low, n as u64) as usize
    }
}

#[inline(always)]
fn nth_set_bit_u64(v: u64, mut n: u64) -> u64 {
    let v = v.reverse_bits();
    let mut s: u64 = 64;

    // Parallel bit count intermediates
    let a = v - ((v >> 1) & (u64::MAX / 3));
    let b = (a & (u64::MAX / 5)) + ((a >> 2) & (u64::MAX / 5));
    let c = (b + (b >> 4)) & (u64::MAX / 0x11);
    let d = (c + (c >> 8)) & (u64::MAX / 0x101);

    // Branchless select
    let t = (d >> 32) + (d >> 48);
    s -= (t.wrapping_sub(n) & 256) >> 3;
    n -= t & (t.wrapping_sub(n) >> 8);

    let t = (d >> (s - 16)) & 0xff;
    s -= (t.wrapping_sub(n) & 256) >> 4;
    n -= t & (t.wrapping_sub(n) >> 8);

    let t = (c >> (s - 8)) & 0xf;
    s -= (t.wrapping_sub(n) & 256) >> 5;
    n -= t & (t.wrapping_sub(n) >> 8);

    let t = (b >> (s - 4)) & 0x7;
    s -= (t.wrapping_sub(n) & 256) >> 6;
    n -= t & (t.wrapping_sub(n) >> 8);

    let t = (a >> (s - 2)) & 0x3;
    s -= (t.wrapping_sub(n) & 256) >> 7;
    n -= t & (t.wrapping_sub(n) >> 8);

    let t = (v >> (s - 1)) & 0x1;
    s -= (t.wrapping_sub(n) & 256) >> 8;

    65 - s - 1
}

#[cfg(test)]
mod tests {
    use super::*;
    use rand::prelude::*;
    use util::RandomCharIter;

    #[gpui::test(iterations = 100)]
    fn test_random_chunks(mut rng: StdRng) {
        let text = random_string_with_utf8_len(&mut rng, MAX_BASE);
        log::info!("Chunk: {:?}", text);
        let chunk = Chunk::new(&text);
        verify_chunk(chunk.as_slice(), &text);

        // Verify Chunk::chars() bitmap
        let expected_chars = char_offsets(&text)
            .into_iter()
            .inspect(|i| assert!(*i < 128))
            .fold(0u128, |acc, i| acc | (1 << i));
        assert_eq!(chunk.chars(), expected_chars);

        for _ in 0..10 {
            let mut start = rng.random_range(0..=chunk.text.len());
            let mut end = rng.random_range(start..=chunk.text.len());
            while !chunk.text.is_char_boundary(start) {
                start -= 1;
            }
            while !chunk.text.is_char_boundary(end) {
                end -= 1;
            }
            let range = start..end;
            log::info!("Range: {:?}", range);
            let text_slice = &text[range.clone()];
            let chunk_slice = chunk.slice(range);
            verify_chunk(chunk_slice, text_slice);
        }
    }

    #[gpui::test(iterations = 100)]
    fn test_split_chunk_slice(mut rng: StdRng) {
        let text = &random_string_with_utf8_len(&mut rng, MAX_BASE);
        let chunk = Chunk::new(text);
        let offset = char_offsets_with_end(text)
            .into_iter()
            .choose(&mut rng)
            .unwrap();
        let (a, b) = chunk.as_slice().split_at(offset);
        let (a_str, b_str) = text.split_at(offset);
        verify_chunk(a, a_str);
        verify_chunk(b, b_str);
    }

    #[gpui::test(iterations = 1000)]
    fn test_nth_set_bit_random(mut rng: StdRng) {
        let set_count = rng.random_range(0..=128);
        let mut set_bits = (0..128).choose_multiple(&mut rng, set_count);
        set_bits.sort();
        let mut n = 0;
        for ix in set_bits.iter().copied() {
            n |= 1 << ix;
        }

        for (mut ix, position) in set_bits.into_iter().enumerate() {
            ix += 1;
            assert_eq!(
                nth_set_bit(n, ix),
                position,
                "nth_set_bit({:0128b}, {})",
                n,
                ix
            );
        }
    }

    /// Returns a (biased) random string whose UTF-8 length is no more than `len`.
    fn random_string_with_utf8_len(rng: &mut StdRng, len: usize) -> String {
        let mut str = String::new();
        let mut chars = RandomCharIter::new(rng);
        loop {
            let ch = chars.next().unwrap();
            if str.len() + ch.len_utf8() > len {
                break;
            }
            str.push(ch);
        }
        str
    }

    #[gpui::test(iterations = 1000)]
    fn test_append_random_strings(mut rng: StdRng) {
        let len1 = rng.random_range(0..=MAX_BASE);
        let len2 = rng.random_range(0..=MAX_BASE).saturating_sub(len1);
        let str1 = random_string_with_utf8_len(&mut rng, len1);
        let str2 = random_string_with_utf8_len(&mut rng, len2);
        let mut chunk1 = Chunk::new(&str1);
        let chunk2 = Chunk::new(&str2);
        let char_offsets = char_offsets_with_end(&str2);
        let start_index = rng.random_range(0..char_offsets.len());
        let start_offset = char_offsets[start_index];
        let end_offset = char_offsets[rng.random_range(start_index..char_offsets.len())];
        chunk1.append(chunk2.slice(start_offset..end_offset));
        verify_chunk(chunk1.as_slice(), &(str1 + &str2[start_offset..end_offset]));
    }

    /// Return the byte offsets for each character in a string.
    ///
    /// These are valid offsets to split the string.
    fn char_offsets(text: &str) -> Vec<usize> {
        text.char_indices().map(|(i, _c)| i).collect()
    }

    /// Return the byte offsets for each character in a string, plus the offset
    /// past the end of the string.
    fn char_offsets_with_end(text: &str) -> Vec<usize> {
        let mut v = char_offsets(text);
        v.push(text.len());
        v
    }

    fn verify_chunk(chunk: ChunkSlice<'_>, text: &str) {
        let mut offset = 0;
        let mut offset_utf16 = OffsetUtf16(0);
        let mut point = Point::zero();
        let mut point_utf16 = PointUtf16::zero();

        log::info!("Verifying chunk {:?}", text);
        assert_eq!(chunk.offset_to_point(0), Point::zero());

        assert_eq!(
            text.char_indices()
                .filter(|(_i, c)| *c == '\n')
                .map(|(i, _)| i)
                .collect::<Vec<usize>>(),
            chunk.newlines.iter_ones().collect::<Vec<usize>>(),
            "newline bits do not match in {text:?} and chunk {chunk:?}"
        );

        let mut expected_tab_positions = Vec::new();

        for (char_offset, c) in text.chars().enumerate() {
            assert_eq!(
                chunk.offset_to_point(offset),
                point,
                "wrong offset_to_point mismatch at offset {offset:?} in {text:?} and chunk {chunk:?}"
            );
            assert_eq!(
                chunk.point_to_offset(point),
                offset,
                "mismatch at point {:?}",
                point
            );
            assert_eq!(
                chunk.offset_to_offset_utf16(offset),
                offset_utf16,
                "mismatch at offset {}",
                offset
            );
            assert_eq!(
                chunk.offset_utf16_to_offset(offset_utf16),
                offset,
                "mismatch at offset_utf16 {:?}",
                offset_utf16
            );
            assert_eq!(
                chunk.point_to_point_utf16(point),
                point_utf16,
                "mismatch at point {:?}",
                point
            );
            assert_eq!(
                chunk.point_utf16_to_offset(point_utf16, false),
                offset,
                "mismatch at point_utf16 {:?}",
                point_utf16
            );
            assert_eq!(
                chunk.unclipped_point_utf16_to_point(Unclipped(point_utf16)),
                point,
                "mismatch for unclipped_point_utf16_to_point at {:?}",
                point_utf16
            );

            assert_eq!(
                chunk.clip_point(point, Bias::Left),
                point,
                "incorrect left clip at {:?}",
                point
            );
            assert_eq!(
                chunk.clip_point(point, Bias::Right),
                point,
                "incorrect right clip at {:?}",
                point
            );

            for i in 1..c.len_utf8() {
                let test_point = Point::new(point.row, point.column + i as u32);
                assert_eq!(
                    chunk.clip_point(test_point, Bias::Left),
                    point,
                    "incorrect left clip within multi-byte char at {:?}",
                    test_point
                );
                assert_eq!(
                    chunk.clip_point(test_point, Bias::Right),
                    Point::new(point.row, point.column + c.len_utf8() as u32),
                    "incorrect right clip within multi-byte char at {:?}",
                    test_point
                );
            }

            for i in 1..c.len_utf16() {
                let test_point = Unclipped(PointUtf16::new(
                    point_utf16.row,
                    point_utf16.column + i as u32,
                ));
                assert_eq!(
                    chunk.unclipped_point_utf16_to_point(test_point),
                    point,
                    "incorrect unclipped_point_utf16_to_point within multi-byte char at {:?}",
                    test_point
                );
                assert_eq!(
                    chunk.clip_point_utf16(test_point, Bias::Left),
                    point_utf16,
                    "incorrect left clip_point_utf16 within multi-byte char at {:?}",
                    test_point
                );
                assert_eq!(
                    chunk.clip_point_utf16(test_point, Bias::Right),
                    PointUtf16::new(point_utf16.row, point_utf16.column + c.len_utf16() as u32),
                    "incorrect right clip_point_utf16 within multi-byte char at {:?}",
                    test_point
                );

                let test_offset = OffsetUtf16(offset_utf16.0 + i);
                assert_eq!(
                    chunk.clip_offset_utf16(test_offset, Bias::Left),
                    offset_utf16,
                    "incorrect left clip_offset_utf16 within multi-byte char at {:?}",
                    test_offset
                );
                assert_eq!(
                    chunk.clip_offset_utf16(test_offset, Bias::Right),
                    OffsetUtf16(offset_utf16.0 + c.len_utf16()),
                    "incorrect right clip_offset_utf16 within multi-byte char at {:?}",
                    test_offset
                );
            }

            if c == '\n' {
                point.row += 1;
                point.column = 0;
                point_utf16.row += 1;
                point_utf16.column = 0;
            } else {
                point.column += c.len_utf8() as u32;
                point_utf16.column += c.len_utf16() as u32;
            }

            if c == '\t' {
                expected_tab_positions.push(TabPosition {
                    byte_offset: offset,
                    char_offset,
                });
            }

            offset += c.len_utf8();
            offset_utf16.0 += c.len_utf16();
        }

        let final_point = chunk.offset_to_point(offset);
        assert_eq!(
            point, final_point,
            "mismatch at final offset {offset} in {chunk:?} in {text:?}"
        );
        assert_eq!(
            chunk.point_to_offset(point),
            offset,
            "mismatch at point {:?}",
            point
        );
        assert_eq!(
            chunk.offset_to_offset_utf16(offset),
            offset_utf16,
            "mismatch at offset {}",
            offset
        );
        assert_eq!(
            chunk.offset_utf16_to_offset(offset_utf16),
            offset,
            "mismatch at offset_utf16 {:?}",
            offset_utf16
        );
        assert_eq!(
            chunk.point_to_point_utf16(point),
            point_utf16,
            "mismatch at final point {:?}",
            point
        );
        assert_eq!(
            chunk.point_utf16_to_offset(point_utf16, false),
            offset,
            "mismatch at final point_utf16 {:?}",
            point_utf16
        );
        assert_eq!(
            chunk.unclipped_point_utf16_to_point(Unclipped(point_utf16)),
            point,
            "mismatch for unclipped_point_utf16_to_point at final point {:?}",
            point_utf16
        );
        assert_eq!(
            chunk.clip_point(point, Bias::Left),
            point,
            "incorrect left clip at final point {:?}",
            point
        );
        assert_eq!(
            chunk.clip_point(point, Bias::Right),
            point,
            "incorrect right clip at final point {:?}",
            point
        );
        assert_eq!(
            chunk.clip_point_utf16(Unclipped(point_utf16), Bias::Left),
            point_utf16,
            "incorrect left clip_point_utf16 at final point {:?}",
            point_utf16
        );
        assert_eq!(
            chunk.clip_point_utf16(Unclipped(point_utf16), Bias::Right),
            point_utf16,
            "incorrect right clip_point_utf16 at final point {:?}",
            point_utf16
        );
        assert_eq!(
            chunk.clip_offset_utf16(offset_utf16, Bias::Left),
            offset_utf16,
            "incorrect left clip_offset_utf16 at final offset {:?}",
            offset_utf16
        );
        assert_eq!(
            chunk.clip_offset_utf16(offset_utf16, Bias::Right),
            offset_utf16,
            "incorrect right clip_offset_utf16 at final offset {:?}",
            offset_utf16
        );

        // Verify length methods
        assert_eq!(chunk.len(), text.len());
        assert_eq!(
            chunk.len_utf16().0,
            text.chars().map(|c| c.len_utf16()).sum::<usize>()
        );

        // Verify line counting
        let lines = chunk.lines();
        let mut newline_count = 0;
        let mut last_line_len = 0;
        for c in text.chars() {
            if c == '\n' {
                newline_count += 1;
                last_line_len = 0;
            } else {
                last_line_len += c.len_utf8() as u32;
            }
        }
        assert_eq!(
            lines,
            Point::new(newline_count, last_line_len),
            "Counted the wrong number of lines in {text:?}"
        );

        // Verify first/last line chars
        if !text.is_empty() {
            let first_line = text.split('\n').next().unwrap();
            assert_eq!(chunk.first_line_chars(), first_line.chars().count() as u32);

            let last_line = text.split('\n').next_back().unwrap();
            assert_eq!(
                chunk.last_line_chars(),
                last_line.chars().count() as u32,
                "wrong number of last_line_chars for {text:?}",
            );
            assert_eq!(
                chunk.last_line_len_utf16(),
                last_line.chars().map(|c| c.len_utf16() as u32).sum::<u32>(),
                "wrong number of last line UTF-16 code units for {text:?}",
            );
        }

        // Verify longest row
        let (longest_row, longest_chars) = chunk.longest_row(&mut 0);
        let mut max_chars = 0;
        let mut current_row = 0;
        let mut current_chars = 0;
        let mut max_row = 0;

        for c in text.chars() {
            if c == '\n' {
                if current_chars > max_chars {
                    max_chars = current_chars;
                    max_row = current_row;
                }
                current_row += 1;
                current_chars = 0;
            } else {
                current_chars += 1;
            }
        }

        if current_chars > max_chars {
            max_chars = current_chars;
            max_row = current_row;
        }

        assert_eq!(
            (max_row, max_chars as u32),
            (longest_row, longest_chars),
            "wrong longest row for {text:?}"
        );
        assert_eq!(
            chunk.tabs().collect::<Vec<_>>(),
            expected_tab_positions,
            "wrong tab positions for {text:?}"
        );
    }

    #[test]
    fn last_line_range() {
        let chunk = Chunk::new("");
        assert!(chunk.as_slice().last_line_range().is_empty());

        let chunk = Chunk::new("hello");
        assert_eq!(chunk.as_slice().last_line_range(), 0..5);

        let chunk = Chunk::new("hello\nworld");
        assert_eq!(chunk.as_slice().last_line_range(), 6..11);

        let chunk = Chunk::new("hello\nworld\n");
        assert_eq!(chunk.as_slice().last_line_range(), 12..12);
    }

    #[test]
    fn unaligned_offset_to_utf8_point() {
        // This tests behavior relied upon by diagnostic_tests.rs, although accepting
        // offsets not on char boundaries seems inconsistent with the rest of the API.
        let chunk = Chunk::new("a𐍈b");
        for offset in 0..=chunk.text.len() {
            let point = chunk.as_slice().offset_to_point_utf16(offset);
            println!("offset {offset} -> point {point:?}");
            // just check that it doesn't panic: it may not round trip precisely
        }
    }
}
