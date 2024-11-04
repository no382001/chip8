```
CHIP-8 Instruction Set
http://octo-ide.com

N is a number between 0 and 15.
NN is a number between 0 and 255.
NNN is an address between 0 and 4095.
vx and vy are registers (0-F).
I is the memory index register.
Instructions in gray rows may modify the VF register.

machinenode | ccto Instruction  | comments
------------------------------------------
00E0        | clear             | clear the display
00EE        | return            | exit a subroutine
1NNN        | jump NNN          | jump to address NNN
2NNN        | NNN               | call a subroutine
3XNN        | if vx != NN then  | skip next instruction if VX == NN
4XNN        | if vx != NN then  | skip next instruction if VX != NN
5XY0        | if vx == vy then  | skip next instruction if VX == VY
6XNN        | vx = NN           | set VX to NN
7XNN        | vx += NN          | add NN to VX
8XY0        | vx = vy           | set VX to VY
8XY1        | vx |= vy          | bitwise OR
8XY2        | vx &= vy          | bitwise AND
8XY3        | vx ^= vy          | bitwise XOR
8XY4        | vx += vy          | add VY to VX, VF = 1 on carry
8XY5        | vx -= vy          | subtract VY from VX, VF = 0 on borrow
8XY6        | vx >>= 1          | shift VX right by 1, VF = old LSB
8XY7        | vx = vy - vx      | set VX to VY - VX, VF = 0 on borrow
8XYE        | vx <<= 1          | shift VX left by 1, VF = old MSB
9XY0        | if vx != vy then  | skip next instruction if VX != VY
ANNN        | I = NNN           | set I to NNN
BNNN        | jump NNN + v0     | jump to address NNN + V0
CXNN        | vx = rand NN      | random number 0-255 AND NN
DXYN        | sprite vx vy N    | draw sprite at VX, VY, height N, VF = 1 on collision
EX9E        | if vx - key then  | skip next instruction if key in VX is pressed
EXA1        | if vx !- key then | skip next instruction if key in VX is not pressed
FX07        | vx = delay        | set VX to delay timer value
FX0A        | vx = key          | wait for a key press, store in VX
FX15        | delay = vx        | set delay timer to VX
FX18        | buzzer = vx       | set sound timer to VX
FX1E        | I += vx           | add VX to I
FX29        | I = hex vx        | set I to the location of sprite for digit VX
FX33        | bcd vx            | store BCD representation of VX at I, I+1, I+2
FX55        | save v0-vx        | store V0 to VX in memory starting at I
FX65        | load vx           | read memory at I into V0 to VX
```