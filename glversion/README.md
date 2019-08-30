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

## Background

The original rendering algorithm renders starts rendering at the sector the player is in, 
then visits any neighbors of the sector and draws them. Drawing from front to back. The 
`ytop` and `ybottom` arrays along with `sx1` and `sx2` fields in the queued sectors keep 
track of the areas of the screen that can still be rendered to for the current sector, 
allowing the renderer to reject sectors that can't possibly add anything to the scene. 
(or are behind the player). 

With OpenGL, it makes sense to use the z-buffer. In theory, if we submit the geometry in 
the same order the renderer does now, with the closest things first, early z-buffer 
rejection might save you your theoretically expensive fragment shader calculations for 
overdrawn areas. You end up also pushing the transformation of coordinates to the video
card, freeing up some CPU time.
 
The downside is it's not immediately obvious how to test sectors for visibility to determine where 
to stop sector traversals, beyond simple tests for sectors behind the player. Just 
submitting all of the geometry and hoping for the best will bottleneck on data transfers
for larger maps. One of the benefits of this type of level structure is it
allows some dynamic changes in the geometry, so uploading the geometry once and redrawing
it constantly misses the point. Can we maintain the same idea of whether a sector can be
visible from a scene without doing nearly all the work the original renderer does?

I suspect you can do something like project along the end points of sector edges that
have neighbors, and do some rough tests that are cheap enough,  and
are good enough so the failure mode is "sector is rendered even though it can't be seen", 
vs. "forgot to render a visible sector, which will pop in suddenly later." 
