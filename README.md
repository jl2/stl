This is a small library for reading the triangle data in binary [STL files](https://en.wikipedia.org/wiki/STL_(file_format)).

Here is a small example showing how to read a file and compute the area of the triangles in it.

```commonlisp

    CL-USER> (ql:quickload :stl)
    To load "stl":
      Load 1 ASDF system:
        stl
    ; Loading "stl"
    ...
    (:STL)
    CL-USER> (defparameter *cube* (stl:read-stl "~/3d_models/cube.stl"))
    *CUBE*
    CL-USER> (stl:stl-area *cube*)
    24.000002
    CL-USER>
```
