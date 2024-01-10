# hs-mem-info

[![GitHub CI](https://github.com/adetokunbo/hs-mem-info/actions/workflows/nix-ci.yml/badge.svg)](https://github.com/adetokunbo/hs-mem-info/actions)
[![Stackage Nightly](http://stackage.org/package/hs-mem-info/badge/nightly)](http://stackage.org/nightly/package/hs-mem-info)
[![Hackage][hackage-badge]][hackage]
[![Hackage Dependencies][hackage-deps-badge]][hackage-deps]
[![BSD3](https://img.shields.io/badge/license-BSD3-green.svg?dummy)](https://github.com/adetokunbo/hs-mem-info/blob/master/LICENSE)

A utility to accurately report the core memory usage of a program

This a clone of [ps_mem], a command implemented in python, with its behavior
available as a library that may be used from haskell programs.

It provides an executable command `printmem`, that mimics the `ps_mem`.

## printmem usage

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

The [-p PID,...] option allows filtering the results.
For example to restrict output to the current $USER you could:

```
sudo ps_mem -S -p $(pgrep -d' -p ' -u $USER)
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


[hackage-deps-badge]: <https://img.shields.io/hackage-deps/v/hs-mem-info.svg>
[hackage-deps]:       <http://packdeps.haskellers.com/feed?needle=hs-mem-info>
[hackage-badge]:      <https://img.shields.io/hackage/v/hs-mem-info.svg>
[hackage]:            <https://hackage.haskell.org/package/hs-mem-info>
[ps_mem]:             <https://github.com/pixelb/ps_mem/blob/master/README.md>
