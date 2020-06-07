# raycaster-untextured

This is a Common Lisp port of the very first part of [Lode Vandevenne](https://github.com/lvandeve)'s excellent [Computer Graphics Tutorial](https://lodev.org/cgtutor/raycasting.html), which covers the untextured raycaster example.

![raycaster-untextured2](https://user-images.githubusercontent.com/19293817/83981215-30bb9680-a91c-11ea-86df-fc8704b44425.gif)

My version here still has glitches (as can be seen above) and crashes sometimes as the translation was not as smooth as I foresaw. I hadn't had time to fix it yet because too many other things going on. Maybe (hopefully) I'll get back to it. In the meantime here it is. Have fun!

### To run the code:

I've used SBCL, so I have no clue if/how this will work on other Lisps. To run this you'll need quicklisp and lispbuilder-sdl. 

If you don't have quicklisp you can find instructions here:
https://golems.github.io/motion-grammar-kit/install.html

or here:
https://lispcookbook.github.io/cl-cookbook/getting-started.html

Then once into the REPL:

```
(load "~/quicklisp/setup.lisp")

(ql:quickload :lispbuilder-sdl)

(load "raycaster-untextured")

(raycaster-untextured-main)
```
