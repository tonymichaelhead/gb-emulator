const ZERO_FLAG_BYTE_POSITION: u8 = 7;
const SUBRACT_FLAG_BYTE_POSITION: u8 = 6;
const HALF_CARRY_FLAG_BYTE_POSITION: u8 = 5;
const CARRY_FLAG_BYTE_POSITION: u8 = 4;

struct Registers
{
    a: u8,
    b: u8,
    c: u8,
    d: u8,
    e: u8,
    f: FlagsRegister,
    h: u8,
    l: u8,
}

impl Registers
{
    fn get_bc(&self) -> u16
    {
        (self.b as u16) << 8
        | self.c as u16
    }

    fn set_bc(&mut self, value: u16)
    {
        self.b = ((value & 0xFF00) >> 8) as u8;
        self.c = (value & 0xFF) as u8;
    }

    fn get_de(&self) -> u16
    {
        (self.d as u16) << 8
        | self.e as u16
    }

    fn set_de(&mut self, value: u16)
    {
        self.d = ((value & 0xFF00) >> 8) as u8;
        self.e = (value & 0xFF) as u8;
    }

    fn get_hl(&self) -> u16
    {
        (self.h as u16) << 8
        | self.l as u16
    }

    fn set_hl(&mut self, value: u16)
    {
        self.h = ((value & 0xFF00) >> 8) as u8;
        self.l = (value & 0xFF) as u8;
    }

    fn get_af(&self) -> u16
    {
        (self.a as u16) << 8
        | u8::from(self.f) as u16
    }

    fn set_af(&mut self, value: u16)
    {
        self.a = ((value & 0xFF00) >> 8) as u8;
        self.f = FlagsRegister::from((value & 0xFF) as u8);
    }
}

#[derive(Copy, Clone)]
struct FlagsRegister
{
    zero: bool,
    subtract: bool,
    half_carry: bool,
    carry: bool,
}

impl std::convert::From<FlagsRegister> for u8
{
    fn from(flag: FlagsRegister) -> u8
    {
        (if flag.zero       { 1 } else { 0 }) << ZERO_FLAG_BYTE_POSITION |
        (if flag.subtract   { 1 } else { 0 }) << SUBRACT_FLAG_BYTE_POSITION |
        (if flag.half_carry { 1 } else { 0 }) << HALF_CARRY_FLAG_BYTE_POSITION |
        (if flag.carry      { 1 } else { 0 }) << CARRY_FLAG_BYTE_POSITION
    }
}

impl std::convert::From<u8> for FlagsRegister
{
    fn from(byte: u8) -> Self
    {
        let zero = ((byte >> ZERO_FLAG_BYTE_POSITION) & 0b1) != 0;
        let subtract = ((byte >> SUBRACT_FLAG_BYTE_POSITION) & 0b1) != 0;
        let half_carry = ((byte >> HALF_CARRY_FLAG_BYTE_POSITION) & 0b1) != 0;
        let carry = ((byte >> CARRY_FLAG_BYTE_POSITION) & 0b1) != 0;

        FlagsRegister
        {
            zero,
            subtract,
            half_carry,
            carry,
        }
    }

}

enum Instruction
{
    ADD(ArithmeticTarget),
}
 enum ArithmeticTarget
{
    A, B, C, D, E, H, L,
}

struct CPU {
    registers: Registers,
}

impl CPU
{
    fn execute(&mut self, instruction: Instruction)
    {
        match instruction
        {
            Instruction::ADD(target) =>
            {
                match target
                {
                    ArithmeticTarget::C =>
                    {
                        let value = self.registers.c;
                        let new_value = self.add(value);
                        self.registers.a = new_value;
                    }
                    _=> { /* TODO: support more targets */ }
                }
            }
            _=> { /* TODO: support more targets */ }
        }
    }

    fn add(&mut self, value: u8) -> u8
    {
        let (new_value, did_overflow) = self.registers.a.overflowing_add(value);

        // set flags
        self.registers.f.zero = new_value == 0;
        self.registers.f.subtract = false;
        self.registers.f.carry = did_overflow;
        // Half Carry is set if adding the lower nibbles of the value and register A
        // together result in a value bigger than 0xF. If the result is larger than 0xF
        // then the addition caused a carry from the lower nibble to the upper nibble.
        self.registers.f.half_carry = (self.registers.a &0xF) + (value & 0xF) > 0xF;
        new_value
    }
}

fn main()
{
    println!("Hello, Gameboy!");
}
