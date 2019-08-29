## What is it?

A playground version of [prender.nim](../prender.nim) that has an OpenGL
renderer added to it.  A place to experiment with ways to render 2d sectors
using OpenGL.

You can switch rendering modes between the original, and OpenGL by pressing **m**.  
Right now, the OpenGL renderer is just a stub that writes "OpenGL" in the 
bottom left corner of the screen.

## Building
From the glversion directory, run:

	$ nim buildGL
