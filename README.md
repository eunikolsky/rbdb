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
/podcasts/99% Invisible/530- The Panopticon Effect.mp3: 26%, 1 plays
/podcasts/Accidental Tech Podcast/529_ The Cycles of Marco.mp3: 100%, 4 plays
/podcasts/CoRecursive w_ Adam Bell/Story_ JSON vs XML.mp3: 100%, 3 plays
/podcasts/Criminal/Episode 212_ The Fasting Cure.mp3: 100%, 2 plays
/podcasts/FLOSS Weekly/FLOSS Weekly 723_ Freedom to Fork - Open Source Communities & Democracy With Seth Frey.mp3: 99%, 1 plays
/podcasts/The FOSSA Engineering Podcast/Early-Stage Technology Decisions and Regrets.mp3: 100%, 3 plays
/podcasts/Hacker Public Radio/HPR3830_ Into New Mexico.mp3: 100%, 1 plays
/podcasts/Headwaters/Becoming _ Stained by History.mp3: 26%, 1 plays
/podcasts/MapScaping/Geospatial support for humanitarian emergencies.mp3: 75%, 2 plays
/podcasts/Regular Programming/About Tooling.mp3: 100%, 2 plays
/podcasts/This Week in Linux/220_ GNOME 44, Kali Linux, Red Hat 30 Years, Ubuntu Cinnamon, Trisquel 11 & more Linux news!.mp3: 100%, 2 plays
/podcasts/What Roman Mars Can Learn About Con Law/69- The Mar-a-Lago Warrant.mp3: 100%, 1 plays
```

The podcasts are sorted [the way gPodder does it](https://github.com/gpodder/gpodder/blob/74d73231d118caa52661fb16de870e971f6b8164/src/gpodder/model.py#L1094-L1096): case-insensitive, ignoring the "the " prefix if present and replacing certain letter with umlauts to those without (`ö => o`, `ü -> u`, `ä => a`). That's why "The FOSSA Engineering Podcast" appears before "Hacker Public Radio" in the output above.

The `--dump` option shows more information about the valid entries from the parsed database:

```bash
$ rbdb --dump /Volumes/player/.rockbox
/podcasts/99% Invisible/571- You Are What You Watch.mp3: duration=1875.957s, 97% played (raw: 0.965871285962, autoscore=96.58712859623115) 1 play, playTime=1811.933s, playOrder=996, lastOffset=28847279, lastElapsed=1796933, flags=4
/podcasts/Darknet Diaries/143_ Jim Hates Scams.mp3: duration=4195.892s, 100% played (raw: 1.272376886726, autoscore=63.61884433631752) 2 plays, playTime=5338.756s, playOrder=994, lastOffset=67199308, lastElapsed=4192382, flags=4
/podcasts/The Haskell Interlude/41_ Moritz Angermann.mp3: duration=3971.004s, 100% played (raw: 1.908688331716, autoscore=95.43441658583069) 2 plays, playTime=7579.409s, playOrder=979, lastOffset=47527874, lastElapsed=3959302, flags=4
/podcasts/MapScaping/Introduction To LIDAR & Point Clouds.mp3: duration=2932.741s, 100% played (raw: 1.085288472456, autoscore=54.264423622815656) 2 plays, playTime=3182.87s, playOrder=990, lastOffset=0, lastElapsed=0, flags=4
/podcasts/Regular Programming/About Developing Speed.mp3: duration=2285.766s, 100% played (raw: 1.0, autoscore=100.0) 1 play, playTime=2285.766s, playOrder=1002, lastOffset=0, lastElapsed=0, flags=4
/podcasts/Soft Skills Engineering/Episode 393_ Soft skills for interns and intern to QA.mp3: duration=1627.872s, 100% played (raw: 1.092113507695, autoscore=54.60567538479684) 2 plays, playTime=1777.821s, playOrder=985, lastOffset=0, lastElapsed=0, flags=4
/podcasts/Software Engineering Radio/SE Radio 589_ Zac Hatfield-Dodds on Property-Based Testing in Python.mp3: duration=3406.613s, 73% played (raw: 0.729546620059, autoscore=72.95466200592789) 1 play, playTime=2485.283s, playOrder=999, lastOffset=59501530, lastElapsed=2470283, flags=4
/podcasts/Радио-Т/Радио-Т 901.mp3: duration=6033.005s, 24% played (raw: 0.240602817335, autoscore=24.060281733563954) 1 play, playTime=1451.558s, playOrder=1004, lastOffset=23024032, lastElapsed=1436558, flags=4
/podcasts/Радио-Т Поток/2024-03-16 22_03_04 +0200 _ Радио-Т 901.mp3: duration=565.655s, 100% played (raw: 1.0, autoscore=100.0) 1 play, playTime=565.655s, playOrder=980, lastOffset=0, lastElapsed=0, flags=4
```
