# ⚾ Baller

Modding tools for Backyard Baseball 2001.

If you don't want to download anything, you can always [browse the scripts online](https://github.com/whatisaphone/baller/tree/browse).

## Tutorial

Let's extract the game scripts, modify one of them, and rebuild the game.

Before you start of course, [download the latest release](https://github.com/whatisaphone/baller/releases). Open a terminal and run `baller --help` to make sure it works.

1. Pick some directories to keep your project in. This tutorial uses:

   - `~/bb2001src` as the project directory, for the extracted source files.
   - `~/bb2001game` as the output directory, for the compiled game you can run.

2. Copy your original game files to `~/bb2001game`. Make sure to include all the original data files: `baseball 2001.he0`, `baseball 2001.(a)`, etc.

   Before you start, you could try running the game from this directory with ScummVM to make sure your untouched files are working.

3. Extract the assets to your project directory:

   ```sh
   baller extract ~/bb2001game/baseball\ 2001.he0 ~/bb2001src
   ```

   This will write project.txt, some supporting files, and one directory per room.

4. Modify a script. The file `baseball/RMDA_0002.bin` contains the hover text for the main lobby. Open it in a hex editor, and at offset 0xf945 change "Meet the players" to "Meet the modders".

5. Build the assets. This will **overwrite** `baseball 2001.he0` and other files in the output directory.

   ```sh
   baller build ~/bb2001src/project.txt ~/bb2001game/baseball\ 2001.he0
   ```

Now when you run the game, it will use your changed text:

![Screenshot of game after rebuilding](docs/tutorial-finished.webp)

## Contributing

Install prerequisites:

- [Zig] 0.13.0

[Zig]: https://ziglang.org/

Run the app:

```sh
zig build run
```

Run the tests:

```sh
zig build test
```
