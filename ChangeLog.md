# Revision history for mem-info

`mem-info` uses [PVP Versioning][1].

## 0.3.0.1 -- 2025-01-17

- Fix test data generation in QuickCheck test

## 0.3.0.0 -- 2024-03-17

- Extended the dependency bounds to allow all bytestring 0.12.x

- Remove the check for __root__ when no pids are specified

  - previously, an error occurred if no pids were specified without sudo
  - after this, all processes of the current user are shown

- Add an option -y (--output-style) that allows the output to be formatted as
  CSV with all values in KiB

## 0.2.0.0 -- 2024-01-28

- Simplify the output when the -d (--discriminate-by-pid) flag is used

- Add options -b (--order-by) and -r (--reverse) to change the ordering of the
  output

## 0.1.0.1 -- 2024-01-17

- Adjusted dependency bounds to remove any stale dependencies

## 0.1.0.0 -- 2024-01-16

* Initial version.

[1]: https://pvp.haskell.org
