% ----------------------------------------------------------------
% encode.pl
% ----------------------------------------------------------------

:- module(encode, [encode/2, encode_instruction/4, shift_and_mask/4]).

encode_instruction(Base, Parts, Shifts, [HighByte, LowByte]) :-
    foldl(shift_and_mask, Parts, Shifts, Base, Instruction),
    HighByte is (Instruction >> 8) /\ 0xFF,
    LowByte is Instruction /\ 0xFF.

shift_and_mask(Part, Shift, Acc, Result) :-
    Result is Acc + (Part << Shift).

% clear the display (00e0)
encode(clear_display, [0x00, 0xE0]).

% return from a subroutine (00ee)
encode(return_from_subroutine, [0x00, 0xEE]).

% jump to address nnn (1nnn)
encode(jump_to_address(nnn(Address)), Encoded) :-
    Address >= 0, Address =< 0xFFF,
    encode_instruction(0x1000, [Address], [0], Encoded).

% call subroutine at nnn (2nnn)
encode(call_subroutine(nnn(Address)), Encoded) :-
    Address >= 0, Address =< 0xFFF,
    encode_instruction(0x2000, [Address], [0], Encoded).

% skip next instruction if vx == nn (3xnn)
encode(skip_if_vx_eq_nn(v(X), nn(Value)), Encoded) :-
    X >= 0, X =< 15, Value >= 0, Value =< 255,
    encode_instruction(0x3000, [X, Value], [8, 0], Encoded).

% skip next instruction if vx != nn (4xnn)
encode(skip_if_vx_not_nn(v(X), nn(Value)), Encoded) :-
    X >= 0, X =< 15, Value >= 0, Value =< 255,
    encode_instruction(0x4000, [X, Value], [8, 0], Encoded).

% skip next instruction if vx == vy (5xy0)
encode(skip_if_vx_eq_vy(v(X), v(Y)), Encoded) :-
    X >= 0, X =< 15, Y >= 0, Y =< 15,
    encode_instruction(0x5000, [X, Y], [8, 4], Encoded).

% set vx to nn (6xnn)
encode(set_vx_nn(v(X), nn(Value)), Encoded) :-
    X >= 0, X =< 15, Value >= 0, Value =< 255,
    encode_instruction(0x6000, [X, Value], [8, 0], Encoded).

% add nn to vx (7xnn)
encode(add_nn_to_vx(v(X), nn(Value)), Encoded) :-
    X >= 0, X =< 15, Value >= 0, Value =< 255,
    encode_instruction(0x7000, [X, Value], [8, 0], Encoded).

% set vx to vy (8xy0)
encode(set_vx_vy(v(X), v(Y)), Encoded) :-
    X >= 0, X =< 15, Y >= 0, Y =< 15,
    encode_instruction(0x8000, [X, Y], [8, 4], Encoded).

% vx |= vy (8xy1)
encode(vx_or_vy(v(X), v(Y)), Encoded) :-
    X >= 0, X =< 15, Y >= 0, Y =< 15,
    encode_instruction(0x8001, [X, Y], [8, 4], Encoded).

% vx &= vy (8xy2)
encode(vx_and_vy(v(X), v(Y)), Encoded) :-
    X >= 0, X =< 15, Y >= 0, Y =< 15,
    encode_instruction(0x8002, [X, Y], [8, 4], Encoded).

% vx ^= vy (8xy3)
encode(vx_xor_vy(v(X), v(Y)), Encoded) :-
    X >= 0, X =< 15, Y >= 0, Y =< 15,
    encode_instruction(0x8003, [X, Y], [8, 4], Encoded).

% add vy to vx, set vf on carry (8xy4)
encode(vx_add_vy(v(X), v(Y)), Encoded) :-
    X >= 0, X =< 15, Y >= 0, Y =< 15,
    encode_instruction(0x8004, [X, Y], [8, 4], Encoded).

% vx -= vy, set vf on borrow (8xy5)
encode(vx_sub_vy(v(X), v(Y)), Encoded) :-
    X >= 0, X =< 15, Y >= 0, Y =< 15,
    encode_instruction(0x8005, [X, Y], [8, 4], Encoded).

% vx >>= 1, vf = old lsb (8xy6)
encode(vx_shift_right(v(X)), Encoded) :-
    X >= 0, X =< 15,
    encode_instruction(0x8006, [X], [8], Encoded).

% set vx to vy - vx, vf = 0 on borrow (8xy7)
encode(vx_set_vy_minus_vx(v(X), v(Y)), Encoded) :-
    X >= 0, X =< 15, Y >= 0, Y =< 15,
    encode_instruction(0x8007, [X, Y], [8, 4], Encoded).

% vx <<= 1, vf = old msb (8xye)
encode(vx_shift_left(v(X)), Encoded) :-
    X >= 0, X =< 15,
    encode_instruction(0x800E, [X], [8], Encoded).

% skip next instruction if vx != vy (9xy0)
encode(skip_if_vx_ne_vy(v(X), v(Y)), Encoded) :-
    X >= 0, X =< 15, Y >= 0, Y =< 15,
    encode_instruction(0x9000, [X, Y], [8, 4], Encoded).

% set i to nnn (annn)
encode(set_index_register(nnn(Address)), Encoded) :-
    Address >= 0, Address =< 0xFFF,
    encode_instruction(0xA000, [Address], [0], Encoded).

% jump to nnn + v0 (bnnn)
encode(jump_nnn_plus_v0(nnn(Address)), Encoded) :-
    Address >= 0, Address =< 0xFFF,
    encode_instruction(0xB000, [Address], [0], Encoded).

% set vx to random number and nn (cxnn)
encode(vx_rand_and_nn(v(X), nn(Value)), Encoded) :-
    X >= 0, X =< 15, Value >= 0, Value =< 255,
    encode_instruction(0xC000, [X, Value], [8, 0], Encoded).

% draw sprite at vx, vy with height n (dxyn)
encode(draw_sprite(v(X), v(Y), n(N)), Encoded) :-
    X >= 0, X =< 15, Y >= 0, Y =< 15, N >= 0, N =< 15,
    encode_instruction(0xD000, [X, Y, N], [8, 4, 0], Encoded).

% skip next instruction if key in vx is pressed (ex9e)
encode(kp_skip_if_vx(v(X)), Encoded) :-
    X >= 0, X =< 15,
    encode_instruction(0xE09E, [X], [8], Encoded).

% skip next instruction if key in vx is not pressed (exa1)
encode(kp_skip_if_not_vx(v(X)), Encoded) :-
    X >= 0, X =< 15,
    encode_instruction(0xE0A1, [X], [8], Encoded).

% set vx to delay timer value (fx07)
encode(set_vx_to_delay_timer(v(X)), Encoded) :-
    X >= 0, X =< 15,
    encode_instruction(0xF007, [X], [8], Encoded).

% wait for a key press, store in vx (fx0a)
encode(wait_for_kp_save_to_vx(v(X)), Encoded) :-
    X >= 0, X =< 15,
    encode_instruction(0xF00A, [X], [8], Encoded).

% set delay timer to vx (fx15)
encode(set_delay_timer_to_vx(v(X)), Encoded) :-
    X >= 0, X =< 15,
    encode_instruction(0xF015, [X], [8], Encoded).

% set sound timer to vx (fx18)
encode(set_sound_timer_to_vx(v(X)), Encoded) :-
    X >= 0, X =< 15,
    encode_instruction(0xF018, [X], [8], Encoded).

% add vx to i (fx1e)
encode(add_vx_to_i(v(X)), Encoded) :-
    X >= 0, X =< 15,
    encode_instruction(0xF01E, [X], [8], Encoded).

% set i to location of sprite for digit vx (fx29)
encode(set_i_to_sprite_location(v(X)), Encoded) :-
    X >= 0, X =< 15,
    encode_instruction(0xF029, [X], [8], Encoded).

% store bcd of vx at i, i+1, i+2 (fx33)
encode(store_bcd_vx(v(X)), Encoded) :-
    X >= 0, X =< 15,
    encode_instruction(0xF033, [X], [8], Encoded).

% store v0 to vx in memory starting at i (fx55)
encode(save_v0_vx(v(X)), Encoded) :-
    X >= 0, X =< 15,
    encode_instruction(0xF055, [X], [8], Encoded).

% load memory at i into v0 to vx (fx65)
encode(load_v0_vx(v(X)), Encoded) :-
    X >= 0, X =< 15,
    encode_instruction(0xF065, [X], [8], Encoded).

encode(rawbyte(Byte), [Byte]) :-
    Byte >= 0x00, Byte =< 0xFF.
