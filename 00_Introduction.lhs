 ## Introduction

TODO: write something nice after reading.

Steps: 

1. Mandelbrot set with Haskell OpenGL
2. Mandelbrot edges
3. 3D Mandelbrot (because 2D is for pussies)
4. Clean the code from full impure and imperative to purer and purer.

From 1 to 3 it will be _dirtier_ and _dirtier_.
At 4, we will make some order in this mess!
Hopefuly for the best!

One of the goal of this article will be to show some good properties of Haskell and in particular how to make some real world application with it.

I know drawing a simple mandelbrot set isn't a "real world" application. 
But the idea is not to show you a real world application which would be hard to follows, but to give you a way to pass from the pure mindset to some real world application.

To this, I will show you how should progress an application.
It is not something easy to show.
This is why, I preferred work with a program that generate some image.

In a real world application, the first constraint would be to work with some framework.
And generally an imperative one.
Also, the imperative nature of OpenGL make it the perfect choice for an example.

