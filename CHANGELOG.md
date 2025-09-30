# Changelog

## v0.6.2 – 2025-09-30

Added:

- Support 15-bit AWIZ (such as the court images in Backyard Basketball)
- Allow newlines within expressions inside parens
- Improve the error messages for bmp files with the wrong format

Fixed:

- Correctly handle case blocks without an `else`
- Misc other tiny fixes

## v0.6.1 – 2025-08-01

- Add better reporting for music errors

## v0.6.0 – 2025-07-31

This release is a major overhaul.

- Baller now has a proper language with a proper parser
  - Improved error reporting, usually with a line/column pointing to the error
  - Source files are now ASCII instead of windows-1252. Within strings, you can use `\x00`-style escapes to represent characters from windows-1252.
  - CRLF is now supported smoothly. You're welcome, Windows users!
- It's now significantly faster. If you have enough CPU cores, extract and build both take under 0.25 seconds.
- Added high-level scripts instead of assembly
  - Perfect matching decompiler/compiler round trips for all supported games
  - Simple type inference for the decompiler
- Many opcodes have changed to more sensible and consistent names
- The compilation target is now a source directive, instead of being detected from the filename
  - The compiler and assembler now only accept opcodes valid for the current target
- Support music (.he4)
- symbols.ini changes
  - All glob types can have names, not just scripts
  - Extracted files are now named after their symbol
  - The locals and types for a script are now defined compactly all on one line
- Add `baller dump` low-level util for examining block structure
- Add `baller saveload dump` command for examining save files
- Add `baller extract --annotate=yes` flag which writes script addresses alongside the decompilation/disassembly
- Expanded block support
  - OBIM – newly supported
  - OBCD – newly supported
  - TALK – newly supported
  - TLKE – newly supported
  - RMIM
    - Support all compression types used across supported titles
    - Don't bail when z-planes are present
    - Extracted bmp files are now spec-compliant
  - AWIZ
    - Support uncompressed encoding
    - Add a flag `baller build --awiz=original` to precisely match the original RLE encoder. This is slightly less space-efficient, but allows 1:1 byte matching the original games.
    - Don't bail when unrecognized child blocks are seen
  - AKOS
    - Byte-identical encoding, same as for AWIZ
  - DIGI
    - Don't bail when unrecognized child blocks are seen

## v0.5.5 – 2025-02-19

Added:

- Support for 7-bit RMIMs

## v0.5.4 – 2025-01-08

Added:

- Support TRLE encoding for AKOS

## v0.5.3 – 2025-01-03

Added:

- Improved error messages for some resource encoding errors

## v0.5.2 – 2024-10-29

Added:

- Support AWIZ TRNS blocks
- Support uncompressed AWIZ
- Fix colors for MULTs with a DEFA RGBS block
- Add line numbers for most script errors
- Add cli arg `extract --akos=raw` to skip decoding AKOS

## v0.5.1 – 2024-10-22

Added:

- Support room var names in enter/exit scripts
- Print error on unknown instruction

## v0.5.0 – 2024-09-06

Added:

- Decode/encode AKOS images
- Emit comments with talkie tokens
- Add help/version commands

## v0.4.0 – 2024-08-21

Added:

- Support Backyard Soccer, Football, and Basketball
- Support AWIZ with embedded palette
- Allow naming scripts and variables in script assembler
- Allow comments in script assembler

Breaking changes:

- Renamed `in2` instruction to `in`

Fixed:

- Fix override instruction
- Make the AWIZ encoder work with SPUTM (previously it only worked with ScummVM)
- Fix incomplete pixel data for RMIM and AWIZ

## v0.3.0 – 2024-07-17

v0.3.0 is a rewrite from scratch. It _should_ support everything the previous version did, except for the script decompiler, which is replaced with a disassembler/assembler combo.

This release is dedicated to a mysterious sponsor who I hope to reveal one day!

New features:

- Support for Backyard Baseball 1997
- Encoding and decoding images
- Encoding and decoding room audio
- Encoding and decoding talkies
- Disassembling and assembling scripts
