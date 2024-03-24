`rbdb` is a small tool that parses a [`Rockbox`](https://www.rockbox.org/) database and prints the played filenames.

# What problem does it solve?

I sync new podcasts from [`gPodder`](https://gpodder.github.io/) to an excellent [SanDisk Sansa Clip+](https://en.wikipedia.org/wiki/SanDisk_portable_media_players#Sansa_Clip+) player once in a while. Before each sync, I also need to mark played episodes as old (those that I want to keep) or delete played episodes. This program helps me recollect what I listened to because Rockbox stores a [database](https://www.rockbox.org/wiki/DataBase) and tracks various runtime information about the present tracks.

# Running the program

Currently, no binary releases are provided, so you need to install [Haskell Stack](https://docs.haskellstack.org/en/stable/GUIDE/) (for example, with [GHCup](https://www.haskell.org/ghcup/)) and build the project with `stack build`. Then `stack install` can install the executable into `~/.local/bin/`; if the directory is in your `$PATH`, you can simply run `rbdb`.

```bash
$ rbdb -h
Print played podcast episodes from the rockbox database

Usage: rbdb [--dump | [-f|--filename-only] [--color ARG]] ROCKBOX_PATH

Available options:
  --dump                   Dump valid entries from the parsed database
  -f,--filename-only       Show only filenames (default: false)
  --color ARG              Show colorized output: yes|no|auto (default: auto)
  ROCKBOX_PATH             Path to the rockbox database directory (with
                           `database_*.tcd`)
  --version                Show version
  -h,--help                Show this help text
```

The program requires the path to a rockbox database, which is a directory with `database_*.tcd` files. It's the `.rockbox/` directory in the root of your player. When it's connected to the computer and mounted, the path will be like `/Volumes/player/.rockbox`. So providing the path to the program would output something like this:

```bash
$ rbdb /Volumes/player/.rockbox
/podcasts/The Amp Hour Electronics Podcast/#661 – Blogging Electronics with Pallav Aggarwal.mp3: 27%, 1 plays, file 26% played
/podcasts/Darknet Diaries Bonus Episodes/Bonus Episode #1 - Wes.mp3: 100%, 1 plays, file 100% played
/podcasts/Hacker Public Radio/HPR4077_ FFMPEG Series_ Joining and Splitting files.mp3: 100%, 2 plays, file 100% played
/podcasts/Hacker Public Radio/HPR4078_ Learning to read music, part two_ pitch.mp3: 34%, 1 plays, file 32% played
/podcasts/Hanselminutes/Our Retro FPGA future powered by Jose Tejada (JOTEGO).mp3: 100%, 1 plays, file 99% played
/podcasts/The Haskell Interlude/42 _ Jezen Thomas.mp3: 100%, 2 plays, file 100% played
/podcasts/Headwaters/Climate and Community with Mike Durglo, Jr.mp3: 100%, 2 plays, file 96% played
/podcasts/Security Now/SN 966_ Morris The Second - Voyager 1, The Web Turns 35.mp3: 100%, 3 plays, file 100% played
/podcasts/Soft Skills Engineering/Episode 394_ Scrum master, weapons master and minimum tenure to not look bad.mp3: 100%, 1 plays, file 100% played
/podcasts/Software Engineering Radio/SE Radio 597_ Coral Calero Muñoz and Félix García on Green Software.mp3: 33%, 1 plays, file 33% played
```

The podcasts are sorted [the way gPodder does it](https://github.com/gpodder/gpodder/blob/74d73231d118caa52661fb16de870e971f6b8164/src/gpodder/model.py#L1094-L1096): case-insensitive, ignoring the "the " prefix if present and replacing certain letter with umlauts to those without (`ö => o`, `ü -> u`, `ä => a`). That's why "The Haskell Interlude" appears before "Headwaters" in the output above.

The `--dump` option shows more information about the valid entries from the parsed database:

```bash
$ rbdb --dump /Volumes/player/.rockbox
/podcasts/The Amp Hour Electronics Podcast/#661 – Blogging Electronics with Pallav Aggarwal.mp3: duration=3812.856s, 27% played (raw: 0.266603302091, autoscore=26.66033020916604) 1 play, playTime=1016.52s, playOrder=1042, lastOffset=15822883, lastElapsed=1001520, flags=4, file progress=0.26293896346045326
/podcasts/Darknet Diaries Bonus Episodes/Bonus Episode #1 - Wes.mp3: duration=2178.638s, 100% played (raw: 1.0, autoscore=100.0) 1 play, playTime=2178.638s, playOrder=1026, lastOffset=0, lastElapsed=0, flags=4, file progress=1.0
/podcasts/Hacker Public Radio/HPR4077_ FFMPEG Series_ Joining and Splitting files.mp3: duration=642.744s, 100% played (raw: 1.103481323824, autoscore=55.174066191205206) 2 plays, playTime=709.256s, playOrder=1023, lastOffset=0, lastElapsed=0, flags=4, file progress=1.0
/podcasts/Hacker Public Radio/HPR4078_ Learning to read music, part two_ pitch.mp3: duration=906.192s, 34% played (raw: 0.336220138778, autoscore=33.622013877853696) 1 play, playTime=304.68s, playOrder=1024, lastOffset=2319144, lastElapsed=289680, flags=4, file progress=0.31983005239425993
/podcasts/Hanselminutes/Our Retro FPGA future powered by Jose Tejada (JOTEGO).mp3: duration=1968.248s, 100% played (raw: 0.998006348793, autoscore=99.80063487934447) 1 play, playTime=1964.324s, playOrder=1032, lastOffset=31275037, lastElapsed=1949324, flags=4, file progress=0.9904434277276265
/podcasts/The Haskell Interlude/42 _ Jezen Thomas.mp3: duration=3062.831s, 100% played (raw: 1.318571935572, autoscore=65.92859677860123) 2 plays, playTime=4038.563s, playOrder=1020, lastOffset=36638569, lastElapsed=3051860, flags=4, file progress=0.9964414988077183
/podcasts/Headwaters/Climate and Community with Mike Durglo, Jr.mp3: duration=1895.131s, 100% played (raw: 1.871429996132, autoscore=93.57149980660968) 2 plays, playTime=3546.605s, playOrder=1041, lastOffset=43909505, lastElapsed=1827827, flags=4, file progress=0.964535300061506
/podcasts/Security Now/SN 966_ Morris The Second - Voyager 1, The Web Turns 35.mp3: duration=7668.36s, 100% played (raw: 1.65099291113, autoscore=55.03309703769776) 3 plays, playTime=12660.408s, playOrder=1030, lastOffset=0, lastElapsed=0, flags=4, file progress=1.0
/podcasts/Soft Skills Engineering/Episode 394_ Scrum master, weapons master and minimum tenure to not look bad.mp3: duration=1760.914s, 100% played (raw: 1.0, autoscore=100.0) 1 play, playTime=1760.914s, playOrder=1036, lastOffset=0, lastElapsed=0, flags=4, file progress=1.0
/podcasts/Software Engineering Radio/SE Radio 597_ Coral Calero Muñoz and Félix García on Green Software.mp3: duration=3829.237s, 33% played (raw: 0.328555270932, autoscore=32.855527093256434) 1 play, playTime=1258.116s, playOrder=1038, lastOffset=20193571, lastElapsed=1243116, flags=4, file progress=0.32797083508469216
```
