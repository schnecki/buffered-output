# Buffered Output

Buffered output can be used to efficiently write output to a file, `stderr` or `stdout`. It does so
by not directly writing to the handle, but rather buffering in memory and then writing it
periodically to the handle. This ensures that multiple calls to write do not slow down computation.

It uses the `auto-update` module with default settings to implement the above mentioned
functionality.


# Example

Example how this might be used


    testFilepath :: FilePath
    testFilepath = "testfile.txt"

    testBufSize :: Int
    testBufSize = 1000 * 1024  -- 1 MB

    testFile :: IORef BufferedOutput
    testFile = unsafePerformIO $ openBufferedOutputFile testBufSize testFilepath >>= newIORef
    {-# NOINLINE testFile #-}

    openBuffer :: IO ()
    openBuffer = openBufferedOutputFile testBufSize testFilepath >>= writeIORef testFile

    getTestFile :: IO BufferedOutput
    getTestFile = readIORef testFile

    writeTest :: Int -> String -> String -> IO ()
    writeTest per info1 info2 = getTestFile >>= writeTest'
      where
        writeTest' buf = do
          when (per == 1) $ resetTest buf
          pushBufferedOutStrLn buf (info1 <> "\t" <> info2)
        resetTest buf = do
          closeBufferedOutput buf
          writeFile testFilepath "TestCol1\tTestCol2\n"
          openBuffer


# Note

Parts of this code are from the [`monad-logger`](https://github.com/snoyberg/monad-logger) package.
