# rohlang2
2nd iteration of rohlang

meant to be consumed by other programs (like shell scripts). you compose a lazy function that consumes an array and asynchronsly processes it, printing results to the output. instead of implementing unsafe IO through perhaps a monadic IO model the IO is abstracted away from the programmer. 

it's supposed to be pure in an impure world, all impurity should exist outside the application. in a perfect world, pure functions would be composed via a different meta programming language which handles impurity. 
