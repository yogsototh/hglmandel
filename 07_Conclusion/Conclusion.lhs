 ## Conclusion

In conclusion, if you add the courage to read this long article,
you should see how to organize your code in a functional way.

As we can use imperative style in a functional language,
know you can use functional style in imperative languages.

The usage of Haskell made it very simple to use our own data type.
The gigantic advantage is to code while being perfectly pure.

In the two last sections, the code is completely pure and functional.
Furthermore I don't use `GLfloat`, `Color3` or any other OpenGL type.
Therefore, if tomorrow I want to use another library I could do this simply by updating the `YGL` module.

The `YGL` module is a "wrapper". 
It is a clean separator between the imperative paradigm and functional paradigm.

Furthermore I demonstrated you can make user interaction and display 3D objects in real time using only a pure functional paradigm. 
