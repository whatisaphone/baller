# ⚾ Inside Baseball

Modding tools for Backyard Baseball 2001.

If you don't want to download anything, you can always [browse the scripts online](https://github.com/whatisaphone/inside-baseball/tree/browse).

## A Quick Tour

Let's extract the game scripts, modify one of them, and rebuild the game.

Before you start of course, [download the latest release](https://github.com/whatisaphone/inside-baseball/releases). Open a terminal and run `inside-baseball --help` to make sure it works.

1. Locate `baseball 2001.he0` from your copy of the game. Make sure the disk files (`baseball 2001.(a)`, etc) are in the same directory.

   Extract the assets to a new project directory (this demo uses `~/Documents/bb2001`):

   ```sh
   inside-baseball project extract '/path/to/baseball 2001.he0' -o ~/Documents/bb2001
   ```

   This will write project.txt to the output directory, as well as one directory per room.

2. Modify a script. The file `baseball/RMDA/LSC2/2060.bin` contains the hover text for the main lobby. Open it in a hex editor and change "Meet the players" to "Meet the Fockers".

3. Rebuild the assets. This will **overwrite** `baseball 2001.he0` and other files in the same directory!

   You need to pass `-d` with a disk number. Remember in the previous step, we modified the room "baseball". Open project.txt from step 1. Next to "baseball" you'll see `disk=2`, so the disk number is 2.

   ```sh
   inside-baseball project update ~/Documents/bb2001 -o '/path/to/baseball 2001.he0' -d 2
   ```

Now when you run the game, it will use your changed text!

![Screenshot of game after rebuilding](docs/demo-finished.webp)

## Contributing

### Install prerequisites

- [Rust]
- [pre-commit]

[Rust]: https://www.rust-lang.org/
[pre-commit]: https://pre-commit.com/

### Install the pre-commit hook

```sh
pre-commit install
```

This installs a Git hook that runs a quick sanity check before every commit.

### Run the app

```sh
cargo run
```

### Run the tests

```sh
cargo test
```
