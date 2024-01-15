# mem-info

[![GitHub CI](https://github.com/adetokunbo/mem-info/actions/workflows/nix-ci.yml/badge.svg)](https://github.com/adetokunbo/mem-info/actions)
[![Stackage Nightly](http://stackage.org/package/mem-info/badge/nightly)](http://stackage.org/nightly/package/mem-info)
[![Hackage][hackage-badge]][hackage]
[![Hackage Dependencies][hackage-deps-badge]][hackage-deps]
[![BSD3](https://img.shields.io/badge/license-BSD3-green.svg?dummy)](https://github.com/adetokunbo/mem-info/blob/master/LICENSE)

A utility to accurately report the core memory usage of programs

This is a clone of [ps_mem], the original command implemented in python, whose
functionality is also available for use as library code in haskell programs.

It provides an executable command `printmem`, that mimics `ps_mem`.

## Rationale

It determines how much RAM (and optionally Swap) is used per *program*, not per
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

## Usage

```
Usage: printmem [-s|--split-args] [-t|--total] [-d|--discriminate-by-pid]
                [-S|--show_swap] [-w|--watch N]
                [(-p|--pids <pid1> [ -p pid2 ... -p pidN ])]

Available options:
  -h,--help                Show this help text
  -s,--split-args          Show and separate by all command line arguments
  -t,--total               Only show the total value
  -d,--discriminate-by-pid Show by process rather than by program
  -S,--show_swap           Show swap information
  -w,--watch N             Measure and show memory every N seconds (N > 0)
  -p,--pids <pid1> [ -p pid2 ... -p pidN ]
                           Only show memory usage of the specified PIDs
```

### Example output

You can run `printmem` *without* filtering; this will try to display data for
all running processes, so sudo is required

```sudo printmem```

Usually, you'll want to filter the results. which is supported by the `-p <pid>`
option.  This can be specified multiple times to select multiple processes. [pgrep] is great companion tool for obtaining the specific sets of pids for filtering.

#### Example: breakdown the memory use of the current user

To restrict output to the current $USER, you can obtain the user's process IDs using pgrep comme ca

```
sudo printmem -S -p $(pgrep -d' -p ' -u $USER)
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

## Haskell Usage

The functions exported by `System.MemInfo` can be used to obtain memory usage
of programs from haskell code:

#### Example: print the program name and memory usage of single process

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

#### Example: monitor the memory of a single process continuously

```haskell
import System.MemInfo (
  mkReportBud,
  printUsage,
  unfoldMemUsageAfter,
  ProcessID
)

-- | Use 'unfoldMemUsageAfter' to periodically print out the memory usage
monitorRamOf :: ProcessID -> IO ()
monitorRamOf pid = do
  budMb <- mkReportBud $ NE.singleton pid
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
main = monitorRamOf 96334 -- replace with your own process ID
```


[hackage-deps-badge]: <https://img.shields.io/hackage-deps/v/mem-info.svg>
[hackage-deps]:       <http://packdeps.haskellers.com/feed?needle=mem-info>
[hackage-badge]:      <https://img.shields.io/hackage/v/mem-info.svg>
[hackage]:            <https://hackage.haskell.org/package/mem-info>
[ps_mem]:             <https://github.com/pixelb/ps_mem/blob/master/README.md>
[pgrep]:              <https://www.man7.org/linux/man-pages/man1/pgrep.1.html>
