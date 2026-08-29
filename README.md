# pdxparse
A parser for scripts used in Paradox Development Studios games, written in Haskell. It reads the game files and outputs text formatted into wiki tables and templates.

This branch only supports HoI4.

Examples of what it currently outputs on the HoI4 wiki
* https://hoi4.paradoxwikis.com/Ethiopian_national_focus_tree/scriptoutput for national focuses
* https://hoi4.paradoxwikis.com/Swiss_events/scriptoutput for events

## Building

The easiest way to get it running is to use [GHCup](https://www.haskell.org/ghcup/). Install it, then `cd` to the directory where you cloned `pdxparse` and type:

    $ stack install --install-ghc

This will automatically install the compiler and all dependencies. (If you already have GHC 8.10.7 installed, you can probably omit `--install-ghc`.)

You may also be able to just use `cabal-install` if you have it:

    $ cabal install --prefix=/path/to/install

## Usage

Rename settings_example.yml to settings.yml and optionally change game, version and the steam_ folders if you don't use HoI4 in the default steam location.

`pdxparse` should be run from the command line. It will create a directory `output` in the current directory. Its structure is the same as that of the game directory, except that the `.txt` files are directories. Each file in these directories is one "object": one event, one decision, etc. Normally it will wait for a user input at the end so that it can be used in a command window which closes automatically (use `--nowait` to change that).

The following command line options are supported:

    -h, --help      show a help about the command line options
    -p, --paths     show location of configuration files and exit
    -v, --version   show version information and exit
    -n, --nowait    don't wait for the user to press a key before exiting

Without command line options, pdxparse just processes everything it finds and puts the results in the directory `output`.

## Known Issues

* HoI4
    * Decisions don't have a proper format in the output
    * Multiple RHS scopes don't get parsed(e.g. PREV.PREV)
    * Various lines don't have custom messages yet
    * Alias tags aren't properly handled
    * The localization for the anti_tank_frontline doctrine does not resolve due to a Paradox typo

## To do
Feel extremely free to help with any of these or the issues.

* Clean up/optimize code
* Have time for event to be triggered added to the triggered only part in events
* Make random_list work properly
* Make on_action also add the limits/trigger other than just the action
* Rewrite text output for decison for HoI 4
* Find out how hostility_reason affects add_to_war
* deal with alias_tags
* maybe load history/units for load_OOB
* maybe load technology for has_tech
* Expand info on add_field_marshal_role ?
* Find cleaner solution for missing closing brackets in files
* Wiki links need to be added for the following:
    * Technology bonuses (needs mapping)
    * National Spirits/Dynamic Modifiers (verify source)
    * Events and decisions (possibly needs mapping or unified page renames)

## Thanks
* Thanks the functional programming discord community for helping me out and in specific:
    * Edmundnoble
    * skykanin سكايكانن
    * Cyrus T
    * let morrow = fix error
