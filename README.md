# h2c: Haskell bindings to Linux I2C API

H2C is a binding to the Linux i2c-tools/i2c-dev API.
It uses the I2C_RDWR ioctl for repeated-start communications between master and slave.

## Notes
 - You'll probably have to run as root. Getting regular users direct access to i2c busses on Linux is tricky.
 - The Linux i2c-stub driver that you might think would be useful for testing doesn't support the I2C_RDWR ioctl.
   This is why, if you try to use it, you'll get the "invalid argument" error.

## Documentation
` $ stack haddock --open`, find module `System.IO.I2C`

BE CAREFUL WITH I2C. It can be used to poke at things like your graphics card, fans, &c.

See my library [bno055-haskell](https://bitbucket.org/fmapE/bno055-haskell) for examples of h2c in use.

