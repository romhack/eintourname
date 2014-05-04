deforleard
=========
NES "Teenage Mutant Ninja Turtles - Tournament Fighters" RLE tool. Game uses RLE encoded tile maps. This tool can decode/encode them.

Usage:
```
eintourname [-d | -e] file_name [offset]
```


***-d*** - Decode from ROM to binary file. Input ROM name and offset in it must be specified: -d file_name offset

***-e*** - Encode from raw binary. Input binary file with raw data must be specified: -e file_name
Please find sample bat file in release.

Short RLE scheme description:
It's a mixture of plain RLE/incremental RLE and raw codes.
```
First 2 bytes: start PPU address (not implemented, please provide eintourname offset to first control byte)
if ctl == ff, exit;
if ctl == 7f, next 2 bytes - new PPU address;(not implemented, looks like game doesn't use this)
if hibit is set, its RAW or incRle: 6 bits -> counter
	if 7 bit is set, it's incremental: next byte is starting byte;
	if not, its RLE: next byte is dataByte;
if hibit is cleared, it's plain RLE, count has no flags. Next byte is rleDataByte.
```
