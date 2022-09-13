# Pixelblaze Emacs Lisp Library

[Pixelblaze](https://www.bhencke.com/pixelblaze) is a WiFi-based LED controller. You can hook various configurations (strips, matrices, spirals) of programmable LEDs (APA102, SK922, NeoPixel, WS28XX) to the board and then change and interact with colorful patterns generated on the board. In addition, you can write your own animations using the on-board Javascript environment.

This packages provides functions to interact with a Pixelblaze via its Websocket API.

* `pixelblaze-get-patterns`: get a list (hash) of all available patterns
* `pixelblaze-set-pattern`: change active pattern
* `pixelblaze-set-brightness`: change brightness
* `pixelblaze-get-vars` `pixelblaze-set-vars` `pixelblaze-get-controls` `pixelblaze-set-controls`: query and change variables and controls (UI elements)

There is an interactive command, `pixelblaze-choose-pattern`, to set the active pattern from a list.

## Installation and Usage

Install from MELPA using your favorite package manager or grab `pixelblaze.el`. This package requires the Websocket [library](https://github.com/ahyatt/emacs-websocket) and Emacs 27.1 or greater.

All functions require a Pixelblaze (actually, a Websocket) structure as returned from `pixelblaze-open`. You should always close this with `pixelblaze-close` after use to insure subsequent open calls will succeed. A typical usage might look like the following:

``` emacs-lisp
(let ((pb (pixelblaze-open ip-address)))
  ;; do stuff
  (pixelblaze-set-brightness pb 0.5)
  (pixelblaze-close(pb)))
```

A utility "with" function is provided for one-shot type operations:

``` emacs-lisp
(pixelblaze-with (lambda (pb) (pixelblaze-set-brightness pb 0.5))
                 (ip-address))
```

## Example

One common use case is to provide notifications for various events inside Emacs.

Compiling a large software project can take a while, so a visual reminder can be helpful to monitor progress and completion status.

``` emacs-lisp
(require 'pixelblaze)

(defun mg/compile ()
  (interactive)
  (pixelblaze-with (lambda (pb) (pixelblaze-set-pattern pb "rainbow fonts")))
  (call-interactively 'compile))

(defun mg/compile-done (buf status)
  (let ((pb (pixelblaze-open)))
    (pixelblaze-set-pattern pb (if (string-search "finished" status) "fireworks dust" "KITT"))
    (pixelblaze-close pb)))

(setq compilation-finish-functions 'mg/compile-done)
```
The `mg/compile` function will start the "rainbow fonts" pattern before running the standard `compile` command. After the compilation finishes, the hook function, `mg/compile-done`, checks the status and sets a corresponding pattern for success or failure.

## Development

The linting and testing tool, [makem.sh](https://github.com/alphapapa/makem.sh), is used for development. Tests are in `pixelblaze-test.el`.

``` shell
./makem.sh all
```
