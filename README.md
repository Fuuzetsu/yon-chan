Yon-chan ![Build Status](https://travis-ci.org/davexunit/yon-chan.png)
========

An 4chan client for Emacs!

Usage
-----

To view a list of all boards:

```
M-x yon-chan
```

Use `M-x customize-group` to easily change various values associated
with the client.

Currently most key bindings can be found in `yon-chan.el` and they
should be consistent with what you're used to (`n` and `p` go up and
down, `g` refreshes buffers, `q` buries, etc.)


Contributing
-------------

See TODO if you want to contribute and for a slight overview of
features. Please add unit and feature tests for anything you
implement.

We use Ecukes (with espuds) and ert for testing. First install `Cask`
and make sure it's somewhere in your path and then run `cask` (might
need `cask --dev`) in the main directory to pull in various
dependencies. See `.travis.yml` file for how we automated this
process. Currently the client itself depends on the `dash` library so
make sure you have it in your load-path.

`make cukes` runs feature tests (i.e. the tests that ensure we get
the behaviour we want) and `make unit-tests` runs unit tests
(tests that ensure smaller functions do what we want to their inputs).
You can simply run `make` to run both unit tests and feature tests.

You can also install the Ruby `watchr` gem and run `watchr
watch-tests.watchr` to have all these ran every time you make a change
to the files.

License
-------

GNU GPL v3
