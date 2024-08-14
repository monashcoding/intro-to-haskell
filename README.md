# MAC Introduction to Haskell Workshop

## Setup

### Option 1: Code online using CodeSandbox

Fork [this CodeSandbox Devbox](https://codesandbox.io/p/devbox/mac-intro-to-haskell-workshop-g3ynvw).

### Option 2: Install Haskell locally

1. [Install GHCup](https://www.haskell.org/ghcup/). GHCup is an installer for Haskell and related tooling..
  - If you’re on macOS and have Homebrew, you can also do `brew install ghcup` instead to install it.
2. Run the following commands:
  ```sh
  ghcup install stack --set
  ghcup install hls --set
  ```
  This installs Stack (the tool we use to build our Haskell code and handle all our dependencies) and HLS (Haskell language server, which you will need to get IDE support).

<details>
  <summary>Optional: <code>ghcup tui</code></summary>

  If you would like, you can also GHCup’s TUI to interactively install Stack and HLS. Running `ghcup tui` will give you a terminal UI that looks something like this:

  ```none
  ┌───────────────────────GHCup──────────────────────┐
  │    Tool  Version         Tags                    │
  │──────────────────────────────────────────────────│
  │✔✔  GHCup 0.1.30.0   latest,recommended           │
  │──────────────────────────────────────────────────│
  │✔✔  Stack 3.1.1      latest,recommended           │
  │✗   Stack 2.15.7                                  │
  │──────────────────────────────────────────────────│
  │✗   HLS   2.9.0.1    latest                       │
  │✗   HLS   2.9.0.0                                 │
  │✗   HLS   2.8.0.0                                 │
  │✔✔  HLS   2.7.0.0    recommended                  │
  │✗   HLS   2.6.0.0                                 │
  │──────────────────────────────────────────────────│
  ```

  You can also use this interface instead of the `ghcup install` commands to install these tools.
  - Use the arrow keys or scroll to move up and down
  - Press `i` to install
  - Press `s` to set that as the version to use
  - Two ticks (`✔✔`) indicates that the version is currently being used
  - One tick (`✓`) indicates that the version has been installed but is not ‘set’ as the current version.
  - A cross (`✗`) indicates that the version has not been installed
</details>

## Test

Run `stack test` to test parts 1 to 3. It might take a while the first time you run this as Stack will install the appropriate version of GHC (the Haskell compiler) for you.

- `stack test --test-arguments 1`: only test part 1
- `stack test --test-arguments 2`: only test part 2
- `stack test --test-arguments 3`: only test part 3

For part 3, run `stack run` to try out the calculator.

If you want more things to do, there are extra bonus tasks in `Bonus.hs`.
Run `stack test --test-arguments bonus` to run the tests for these.

Solutions are available on the [solutions branch](https://github.com/monashcoding/intro-to-haskell/tree/solutions).

## Windows Issues

If you get an error like this

```none
Error: [S-7282]
       Stack failed to execute the build plan.

       While executing the build plan, Stack encountered the following errors:

       <stderr>: commitAndReleaseBuffer: invalid argument (invalid character)
```

<sup>(windows moment)</sup>

Go to Settings → Time and Language → Language and Region → Administrative
language Settings → Change System Locale… → and check Use Unicode UTF-8. You
might need to restart your computer.
