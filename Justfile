help:
  just --list

test:
  zig build
  zig build test

baseball1997: \
  baseball1997-extract \
  baseball1997-build \
  baseball1997-talkie-extract \
  baseball1997-talkie-build

baseball1997-extract:
  zig build run -- extract src/fixtures/baseball1997/BASEBALL.HE0 /tmp/bb1997

baseball1997-build:
  zig build run -- build /tmp/bb1997/project.txt /tmp/bb1997build/BASEBALL.HE0

baseball1997-talkie-extract:
  zig build run -- talkie extract src/fixtures/baseball1997/BASEBALL.HE2 /tmp/bb1997talk

baseball1997-talkie-build:
  zig build run -- talkie build /tmp/bb1997talk/talkies.txt /tmp/bb1997build/BASEBALL.HE2

baseball2001: \
  baseball2001-extract \
  baseball2001-build \
  baseball2001-talkie-extract \
  baseball2001-talkie-build

baseball2001-extract:
  zig build run -- extract src/fixtures/baseball2001/baseball\ 2001.he0 /tmp/bb2001

baseball2001-build:
  zig build run -- build /tmp/bb2001/project.txt /tmp/bb2001build/baseball\ 2001.he0

baseball2001-talkie-extract:
  zig build run -- talkie extract src/fixtures/baseball2001/baseball\ 2001.he2 /tmp/bb2001talk

baseball2001-talkie-build:
  zig build run -- talkie build /tmp/bb2001talk/talkies.txt /tmp/bb2001build/baseball\ 2001.he2

basketball: \
  basketball-extract \
  basketball-build \
  basketball-talkie-extract \
  basketball-talkie-build

basketball-extract:
  zig build run -- extract src/fixtures/basketball/Basketball.he0 /tmp/basketball

basketball-build:
  zig build run -- build /tmp/basketball/project.txt /tmp/basketballbuild/Basketball.he0

basketball-talkie-extract:
  zig build run -- talkie extract src/fixtures/basketball/Basketball.he2 /tmp/basketballtalk

basketball-talkie-build:
  zig build run -- talkie build /tmp/basketballtalk/talkies.txt /tmp/basketballbuild/Basketball.he2

football: \
  football-extract \
  football-build \
  football-talkie-extract \
  football-talkie-build

football-extract:
  zig build run -- extract src/fixtures/football/FOOTBALL.HE0 /tmp/football

football-build:
  zig build run -- build /tmp/football/project.txt /tmp/footballbuild/FOOTBALL.HE0

football-talkie-extract:
  zig build run -- talkie extract src/fixtures/football/FOOTBALL.HE2 /tmp/footballtalk

football-talkie-build:
  zig build run -- talkie build /tmp/footballtalk/talkies.txt /tmp/footballbuild/FOOTBALL.HE2

soccer: \
  soccer-extract \
  soccer-build \
  soccer-talkie-extract \
  soccer-talkie-build

soccer-extract:
  zig build run -- extract src/fixtures/soccer/SOCCER.HE0 /tmp/soccer

soccer-build:
  zig build run -- build /tmp/soccer/project.txt /tmp/soccerbuild/SOCCER.HE0

soccer-talkie-extract:
  zig build run -- talkie extract src/fixtures/soccer/SOCCER.HE2 /tmp/soccertalk

soccer-talkie-build:
  zig build run -- talkie build /tmp/soccertalk/talkies.txt /tmp/soccerbuild/SOCCER.HE2
