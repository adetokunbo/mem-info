# mem-info

[![GitHub CI](https://github.com/adetokunbo/mem-info/actions/workflows/nix-ci.yml/badge.svg)](https://github.com/adetokunbo/mem-info/actions)
[![Stackage Nightly](http://stackage.org/package/mem-info/badge/nightly)](http://stackage.org/nightly/package/mem-info)
[![Hackage][hackage-badge]][hackage]
[![Hackage Dependencies][hackage-deps-badge]][hackage-deps]
[![BSD3](https://img.shields.io/badge/license-BSD3-green.svg?dummy)](https://github.com/adetokunbo/mem-info/blob/master/LICENSE)

A utility to accurately report the core memory usage of linux programs

This is a clone of [ps_mem], originally implemented in python. This
re-implementation allows its behaviour to be used as library code in haskell
programs.

It provides an executable command `printmem`, that mimics `ps_mem` while adding
[new features], and `mem-info`, a haskell library package.

## Rationale

`printmem` determines how much RAM (and optionally Swap) is used per *program*, not per
process, i.e, all the `httpd`, `postgres` or `firefox` processes are grouped
together.

Normally, it reports

```
sum (private RAM for all processes) + sum (shared RAM for all processes)
```

and optionally (when `-S` is specifed)

```
sum (swap for all processes)
```

Calculating the shared RAM accurately is a slightly complex task and one that is
less accurate on older linux kernels; `printmem` chooses the most accurate
method available.

When `(-d|--discriminate-by-pid)` option is specified, it switches to the more
common per-process breakdown of the RAM measurements.

## Installation

You can download a pre-built binary from [releases].  It is a statically-linked executable that should run on most recent Linux distributions.

  Download it, place it in a directory on your path and give it executable permissions

E.g, the following commands should suffice

```bash
$ # choose a directory on your PATH; this example uses ${HOME}/.local/bin
$ my_local_bin=${HOME}/.local/bin/printmem
$ release_url="https://github.com/adetokunbo/mem-info/releases/download/v0.1.0.1/printmem"
$ # download it
$ wget -O $my_local_bin $release_url
$ # make it executable
$ chmod u+x $my_local_bin
```

## Usage

```
Usage: printmem [-s|--split-args] [-t|--total] [-d|--discriminate-by-pid]
                [-S|--show_swap] [-r|--reverse] [-w|--watch N]
                [(-p|--pids <pid1> [ -p pid2 ... -p pidN ])]
                [-b|--order-by <Private | Swap | Shared | Count>]

Available options:
  -h,--help                Show this help text
  -s,--split-args          Show and separate by all command line arguments
  -t,--total               Only show the total value
  -d,--discriminate-by-pid Show by process rather than by program
  -S,--show_swap           Show swap information
  -r,--reverse             Reverses the order of output, making it descending
  -w,--watch N             Measure and show memory every N seconds (N > 0)
  -p,--pids <pid1> [ -p pid2 ... -p pidN ]
                           Only show memory usage of the specified PIDs
  -b,--order-by <Private | Swap | Shared | Count>
                           Orders the output by ascending values of the given
                           field
```

### Example output

You can run `printmem` *without* filtering; this will try to display data for
all running processes accessible by the current user

```printmem```

Often, you'll want to filter the results. which is supported by the `-p <pid>`
option. This can be specified multiple times to select multiple processes.
[pgrep] is great companion tool for obtaining the specific sets of pids for
filtering.

#### Example: breakdown the memory use of a different user

To restrict output to the specific user, you can obtain the user's process IDs using pgrep comme ca

```
  sudo printmem -S -p $(pgrep -d' -p ' -u <other-username>)
```

giving output like this:

```
  Private  +   Shared   =   RAM Used Swap Used  Program
  10.0 MiB +  393.0 KiB =   10.4 MiB   3.2 MiB  bash (2)
 975.4 MiB +   17.0 KiB =  975.5 MiB   0.0 KiB  emacs-29 (1)
   1.8 GiB +  294.9 MiB =    2.1 GiB   0.0 KiB  haskell-language-server (2)
   1.6 MiB +  901.0 KiB =    2.4 MiB   0.0 KiB  pipewire (1)
   1.8 MiB +  895.0 KiB =    2.7 MiB   0.0 KiB  pipewire-media-session (1)
   2.8 MiB +    1.1 MiB =    3.9 MiB   0.0 KiB  systemd (1)
----------------------------------------------
                             3.1 GiB   3.2 MiB
==============================================

```

#### Example: show all the memory used by postgres

`postgres` runs as multiple co-operating processes; use `pgrep` to determine the pids comme ca:

```
sudo printmem -S -p $(pgrep -d' -p ' postgres)
```

giving output like this:

```
  Private  +   Shared   =   RAM Used Swap Used  Program
  25.9 MiB +   39.5 MiB =   65.3 MiB 101.0 KiB  postgres (21)
----------------------------------------------
                            65.3 MiB 101.0 KiB
==============================================

```

## Building from source

Check out this repository, and then build using either [nix] or [cabal]

### Using cabal

You can build the `mem-info` haskell library and `printmem` executable by running

```
cabal build
```

### Using nix

You can build the `printmem` executable by running

```
$ nix-build static.nix
```


## System.MemInfo, a haskell library

The functions exported by `System.MemInfo` can be used to obtain memory usage
within haskell programs.

#### Example: print the program name and memory usage of a single process

```haskell
import System.MemInfo (
  readForOnePid,
  printUsage,
  ProcessID
)

showUsageOf :: ProcessID -> IO ()
showUsageOf pid = do
  orError <- readForOnePid pid
  case orError of
    Left err -> putStrLn $ show err
    Right usage -> printUsage usage

main :: IO ()
main = showUsageOf 96334 -- replace with your own process ID
```

#### Example: periodically check the memory of some processes

```haskell
import Data.List.NonEmpty (NonEmpty, (:|))
import System.MemInfo (
  mkReportBud,
  printUsage,
  unfoldMemUsageAfter,
  ProcessID
)

-- | Use 'unfoldMemUsageAfter' to periodically read memory usage
monitorRamOf :: NonEmpty ProcessID -> IO ()
monitorRamOf pids = do
  budMb <- mkReportBud pids
  case budMb of
    Nothing -> putStrLn $ "Failed to read the system info"
    Just bud -> do
      let gap = 10 :: Int -- print every 10 sec
          handleNext (Left _) = putStrLn "the process has stopped"
          handleNext (Right ((mu,  _), updated)) = do
            putStrLn $ show mu
            go updated
          go x = unfoldMemUsageAfter gap x >>= handleNext
      go bud

main :: IO ()
main = monitorRamOf $ 96334 :| [96335]-- replace with your own process IDs
```


[hackage-deps-badge]: <https://img.shields.io/hackage-deps/v/mem-info.svg>
[hackage-deps]:       <http://packdeps.haskellers.com/feed?needle=mem-info>
[hackage-badge]:      <https://img.shields.io/hackage/v/mem-info.svg>
[hackage]:            <https://hackage.haskell.org/package/mem-info>
[ps_mem]:             <https://github.com/pixelb/ps_mem/blob/master/README.md>
[pgrep]:              <https://www.man7.org/linux/man-pages/man1/pgrep.1.html>
[releases]:           <https://github.com/adetokunbo/mem-info/releases>
[nix]:                <https://nixos.org/manual/nix/stable/installation/installation>
[cabal]:              <https://cabal.readthedocs.io/en/stable/index.html>
[new features]:       <https://hackage.haskell.org/package/mem-info/changelog>
