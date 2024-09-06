# Changelog

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
