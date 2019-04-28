# HDEVM

Haskell-based disassembler for the EVM machine.

Super simple project to disassemble EVM bytecode to Opcodes. Might add bytecode to source code if time permits (though the compiler optimization might fuck it up...)

![](https://i.imgur.com/k5B9TbU.png)

# Build
```
nix-shell

cabal build

./dist/build/hdevm/hdevm <bytecode>
```
