## What is it?

A port of the example sector/portal renderer [here][1].
  
After seeing the [video][2] that describes the renderer, 
I got interested in seeing how it accomplished so much
in a short amount of C code.  (and to be clear, this is the 
simple solid shaded renderer from the video, not the impressive
final one with textures, and lighting using lightmaps baked from
the author's own radiosity implementation).

I enjoy working with rendering code, and this style is
very different from other renderers I've looked at, which
makes it enticing.  

Porting the code to [Nim][4] is more of a sideshow. 
I like Nim, and something like this port is ideal because I already 
have graphics utility modules from other projects that I can use
for quick experimentation. 


## Files
[prender.c](prender.c) is the C version taken from [here][1]. 

[map-clear.txt](dist/map-clear.txt) is the map provided by original author 
for use by `prender.c`.

[sdl2](vendor/sdl2) is the pinned version of the Nim SDL2 bindings I'm using.

## Building

Get Nim from [here][4].  I'm using version 0.20.2, but earlier versions
back to 0.18 should probably work.

To pull in the external source dependencies under [vendor][vendor/], run: 

    $ git submodule update --init --recursive

### BSD, Linux

You need the development packages for both SDL 1.2 (for the original), 
and SDL 2 (for the port).

To compile the original:

    $ gcc -o dist/prender prender.c -lSDL -lm

The port:

    $ nim build

### Windows and MacOS

To compile the original, technically this works:

    $ gcc -o dist/prender prender.c -lSDL -lm

But as to how to get the GCC or Clang based compiler and the SDL 1.2/2 libraries and headers
for your platform, you're own your own.  I will note that for the port, on Windows the binding 
library expects the SDL2 library to be named `SDL2.dll`, and on MacOS, `libSDL2.dylib`. You might
have to rename them, depending on how you get/compile SDL2.  

To compile the port:

    $ nim build

## Running

All versions expect to be run from the [dist](dist/) directory, which
contains map data, and other files loaded at runtime.

### Key bindings

+ **W**: Move forward
+ **S**: Move backward
+ **A**: Sidestep left
+ **D**: Sidestep right
+ Move the mouse to look around
+ **T**: Go into debug mode where only one sector is drawn at a time.  When in this mode `TAB`
  will draw the next queued sector.  Prints debugging messages to 
  stdout for each sector, and when all sectors are done.
+ **Escape** or **Q** - quit

## Port information

The port is fairly straightforward, where I tried to keep the
C semantics, while staying true to Nim idioms. I copied over 
most comments.

Notes from the port:

+ Changed from SDL1 to SDL2. Both are up to the task.  This
  was only in anticipation of using some helper code I have
  for SDL2 in modified versions from the basic port. 
  
+ Changed the bottom wall base color from a magenta to a 
  shade of green for a contrast I can see better.

+ I changed the map parsing code from `scanf()`
  to parsing functions similar to and from the 
  Nim `parseutils` module. Requires noticably more code than
  the original.  You could probably get closer to original with the `strscans`
  module, but I couldn't be assed to do the conversion.

+ I changed growable arrays managed with `realloc()`
  to use Nim seqs instead. 

+ I included a `geom` module I wrote for vector math. This
  is the only obviously gratuitous change I made in the port, since its
  only use at the moment is to provide the definitions of
  2 and 3 element vectors.  No operations are used.
  I might try to change the inline operations to using the 
  module operations at some point, just to see if it makes
  a difference in runtime.

+ Changed the queue implementation in `DrawScreen` to 
  use indices into the backing array rather than pointers to the 
  entries. Nim makes pointer arithmetic verbose compared to C,
  so it was an easy choice.  Also made local templates for the
  queue functions, to avoid a little repetition.

+ Added mouse warping to center of the screen so the cursor won't stray 
  out of the window when looking around, and turned off the cursor.

+ I disabled numeric overflow/underflow checks in DrawScreen, 
  as they do happen, but are clamped by the code.  I may narrow this
  down just to the sections of the code where the overflows occur,  just to see what
  difference it makes rest of the rendering, speed-wise.

+ I left the globals as globals, even though that may pucker some rectums.
  It's a single file implementation of a renderer. It's fine.

+ There are a couple of glitches that are common to the original
  port.  Which I suppose is a good sign the port is correct.  You
  can step outside of the level at corners. And on the top floor, 
  at certain angles you can see a white wall appear and disappear
  when you're standing to the right of the stairs.

## Bugs

There's a bug in the port where black lines show through some floors that I need 
to track down.

## What's next?

I'll probably experiment with some changes to the port.  I'll put significant
changes in their own subdirectories, to avoid fouling up the basic port, which
I want to leave as a clean starting point.

Some possibilities:

+ Figure out what the rendering glitch on the top floor is coming from. 
  I see it in the original and the port.

+ OpenGL support, similar to what [Ken Silverman][3] did with Polymost
   for the Build engine, but I want to see if I can figure it out without looking
   at the code in the JonoF Build engine port. We'll see how long my resolve lasts.
   I suspect that getting this working simplifies things if you want to add sprites 
   later, since you can use the z-buffer to composite sprites in after the level is 
   rendered. With the usual complications for transparent walls and decals.

+ One thought I had looking at the data structures for the map sectors is that
   it has some passing similarities with a nav mesh.  It's not optimimzed
   for the least number of polygons, but it would be interesting to see how
   well you can put the level geometry to that use without adding any other 
   information. I haven't searched, but it wouldn't surprise me if the Build engine
   games did that.

[1]: https://bisqwit.iki.fi/jutut/kuvat/programming_examples/portalrendering.html 
[2]: https://www.youtube.com/watch?v=HQYsFshbkYw
[3]: http://advsys.net/ken/default.htm
[4]: https://nim-lang.org
