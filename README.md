# Buffered Output

Buffered output can be used to efficiently write output to a file, `stderr` or `stdout`. It does so
by not directly writing to the handle, but rather buffering in memory and then writing it
periodically to the handle. This ensures that multiple calls to write do not slow down computation.

It uses the `auto-update` module with default settings to implement the above mentioned
functionality.


# Example

Example how this might be used


# Note

Parts of this code are from the [`monad-logger`](https://github.com/snoyberg/monad-logger) package.
