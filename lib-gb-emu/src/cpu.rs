const ZERO_FLAG_BYTE_POSITION: u8 = 7;
const SUBRACT_FLAG_BYTE_POSITION: u8 = 6;
const HALF_CARRY_FLAG_BYTE_POSITION: u8 = 5;
const CARRY_FLAG_BYTE_POSITION: u8 = 4;

const VRAM_BEGIN: usize = 0x8000;
const VRAM_END: usize = 0x9FFF;
const VRAM_SIZE: usize = VRAM_END - VRAM_BEGIN + 1;

pub struct Registers
{
    pub a: u8,
    pub b: u8,
    pub c: u8,
    pub d: u8,
    pub e: u8,
    pub f: FlagsRegister,
    pub h: u8,
    pub l: u8,
}

impl Registers
{
    pub fn new() -> Registers
    {
        Registers {
            a: 0,
            b: 0,
            c: 0,
            d: 0,
            e: 0,
            f: FlagsRegister::new(),
            h: 0,
            l: 0,
        }
    }

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

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct FlagsRegister
{
    // set if the last operation produced a result of 0
    pub zero: bool,
    // set if the last operation was a subtraction
    pub subtract: bool,
    // set if lower half of the result overflowed
    pub half_carry: bool,
    // set if the result overflowed
    pub carry: bool,
}

impl FlagsRegister
{
    pub fn new() -> FlagsRegister
    {
        FlagsRegister {
            zero: false,
            subtract: false,
            half_carry: false,
            carry: false,
        }
    }
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

#[derive(Copy, Clone, Debug, PartialEq)]
enum StackTarget
{
    AF,
    BC,
    DE,
    HL,
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum LoadByteTarget
{
    A, B, C, D, E, H, L, HLI
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum LoadByteSource
{
    A, B, C, D, E, H, L, D8, HLI
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum LoadWordTarget
{
    BC,
    DE,
    HL,
    SP,
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum LoadType
{
    Byte(LoadByteTarget, LoadByteSource),
    Word(LoadWordTarget),
}

#[derive(Debug)]
enum JumpTest
{
    NotZero,
    Zero,
    NotCarry,
    Carry,
    Always,
}

#[derive(Debug)]
enum PrefixTarget
{
    A,
    B,
    C,
    D,
    E,
    H,
    L,
    HLI,
}

#[derive(Debug)]
enum IncDecTarget
{
    BC,
    DE,
}

#[derive(Debug)]
enum Instruction
{
    ADD(ArithmeticTarget),
    JP(JumpTest),
    INC(IncDecTarget),
    LD(LoadType),
    RLC(PrefixTarget),

    // Prefix Instructions
    SWAP(PrefixTarget),

    // Stack Instructions
    PUSH(StackTarget),
    POP(StackTarget),
    CALL(JumpTest),
    RET(JumpTest),
}

impl Instruction
{
    fn from_byte(byte: u8, prefixed: bool) -> Option<Instruction>
    {
        if prefixed
        {
            Instruction::from_byte_prefixed(byte)
        }
        else
        {
            Instruction::from_byte_not_prefixed(byte)
        }
    }

    fn from_byte_prefixed(byte: u8) -> Option<Instruction>
    {
        match byte
        {
            0x00 => Some(Instruction::RLC(PrefixTarget::B)),

            0x31 => Some(Instruction::SWAP(PrefixTarget::C)),
                _=> /* TODO: Add mapping for rest of instructions*/ None
        }
    }

    fn from_byte_not_prefixed(byte: u8) -> Option<Instruction>
    {
        match byte
        {
            0x02 => Some(Instruction::INC(IncDecTarget::BC)),
            0x13 => Some(Instruction::INC(IncDecTarget::DE)),

            0x31 => Some(Instruction::LD(LoadType::Word(LoadWordTarget::SP))),

            _=> /* TODO: add mapping for rest of instructions */ None
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum ArithmeticTarget
{
    A,
    B,
    C,
    D,
    E,
    H,
    L,
    D8,
    HLI,
}

#[derive(Copy,Clone)]
pub enum TilePixelValue
{
    Zero,
    One,
    Two,
    Three,
}

type Tile = [[TilePixelValue; 8]; 8];
fn empty_tile() -> Tile
{
    [[TilePixelValue::Zero; 8]; 8]
}

const SCREEN_WIDTH: usize = 160;
const SCREEN_HEIGHT: usize = 144;
pub struct GPU
{
    pub canvas_buffer: [u8; SCREEN_WIDTH * SCREEN_HEIGHT * 4],
    pub vram: [u8; VRAM_SIZE],
    pub tile_set: [Tile; 384],
}

impl GPU
{
    pub fn new() -> GPU
    {
        GPU {
            canvas_buffer: [0; SCREEN_WIDTH * SCREEN_HEIGHT * 4],
            vram: [0; VRAM_SIZE],
            tile_set: [empty_tile(); 384],
        }
    }
    // TODO: probz delete
    // fn read_vram(&self, address: usize) -> u8
    // {
        // self.vram[address]
    // }

    fn write_vram(&mut self, index: usize, value: u8)
    {
        self.vram[index] = value;
        // If our index is greate than 0x1800, we're not writing to the tileset
        // storage, so we can just return.
        if index >= 0x1800 { return }

        // Tiles rows are encoded in two bytes with the first byte always
        // on an even address. Bitwise ANDing the address with 0xffe
        // gives us the address of the first byte.
        // For example: `12 & 0xFFFE == 12` and `13 & 0xFFFE == 12`
        let normalized_index = index & 0xFFFE;

        // First we need to get the two bytes that encode the tile row.
        let byte1 = self.vram[normalized_index];
        let byte2 = self.vram[normalized_index + 1];

        // A tiles is 8 rows tall.Since each row is encoded with two bytes,
        // a tile is therefore 16 bytes in total.
        let tile_index = index / 16;
        // Every two bytes is a new row
        let row_index = (index % 16) / 2;

        // Now we're going to loop 8 times to get the 8 pixels that make up a given row.
        for pixel_index in 0..8 {
            // To determine a pixel's value, we must first find the corresponding bit that encods
            // that pixels value:
            // 1111_1111
            // 0123 4567
            //
            // As you can see the bit that corresponds to the nth pixel is the bit in the nth
            // position *from the left*. Bits are normall indexed from the right.
            //
            // To find the first pixel (aka pixel 0) we find the left most bit (aka bit 7).For
            // the second pixel (aka pixel 1) we first find the second most left bit (aka bit 6) and
            // so on.
            //
            // We then create a mask with a 1 at that position and 0s everywhere else.
            //
            // Bitwise ANDing this mask with our bytes will leave that particular bit with its
            // original value and every other bit with a 0.
            let mask = 1 << (7 - pixel_index);
            let lsb = byte1 & mask;
            let msb = byte2 & mask;

            // If the masked values are not 0, the masked bit must be 1. If they are 0, the masked
            // bit must be 0.
            //
            // Finally we can tell which of the 4 values the pixel is. Fox example, if the least
            // significant byte's bit is 1 and the most significant byte's bit is also 1, then we
            // have tile value `Three`.
            let value = match(lsb != 0, msb != 0) {
                (true, true) => TilePixelValue::Three,
                (false, true) => TilePixelValue::Two,
                (true, false) => TilePixelValue::One,
                (false, false) => TilePixelValue::Zero,
            };

             self.tile_set[tile_index][row_index][pixel_index] = value;
        }
    }
}

// MEMORY BUS
// use crate::{
    // timer::{Frequency, Timer},
// };

pub const BOOT_ROM_BEGIN: usize = 0x00;
pub const BOOT_ROM_END: usize = 0xFF;
pub const BOOT_ROM_SIZE: usize = BOOT_ROM_END - BOOT_ROM_BEGIN + 1;

pub const ROM_BANK_0_BEGIN: usize = 0x0000;
pub const ROM_BANK_0_END: usize = 0x3FFF;
pub const ROM_BANK_0_SIZE: usize = ROM_BANK_0_END - ROM_BANK_0_BEGIN + 1;

pub const ROM_BANK_N_BEGIN: usize = 0x0000;
pub const ROM_BANK_N_END: usize = 0x3FFF;
pub const ROM_BANK_N_SIZE: usize = ROM_BANK_N_END - ROM_BANK_N_BEGIN + 1;

pub struct MemoryBus
{
    boot_rom: Option<[u8; BOOT_ROM_SIZE]>,
    memory: [u8; 0xFFFF],
    rom_bank_0: [u8; ROM_BANK_0_SIZE],
    pub gpu: GPU,
}

impl MemoryBus
{
    pub fn new(boot_rom_buffer: Option<Vec<u8>>, game_rom: Vec<u8>) -> MemoryBus
    {
        let boot_rom = boot_rom_buffer.map(|boot_rom_buffer| {
            if boot_rom_buffer.len() != BOOT_ROM_SIZE
            {
                panic!(
                    "Supplied boot ROM is the wrong size. Is {} bytes but should be {} bytes",
                    boot_rom_buffer.len(),
                    BOOT_ROM_SIZE
                );
            }
            let mut boot_rom = [0; BOOT_ROM_SIZE];
            boot_rom.copy_from_slice(&boot_rom_buffer);
            boot_rom
        });

        let mut rom_bank_0 = [0; ROM_BANK_0_SIZE];
        for i in 0..ROM_BANK_0_SIZE
        {
            rom_bank_0[i] = game_rom[i];
        }
        let mut rom_bank_n = [0; ROM_BANK_N_SIZE];
        for i in 0..ROM_BANK_N_SIZE
        {
            rom_bank_n[i] = game_rom[ROM_BANK_0_SIZE + i];
        }
        //TODO implement
        // let mut divider = Timer::new(Frequency::F16384);
        // divider.on = true;

        MemoryBus {
            // Note: instead of modeling memory as one array of length 0xFFFF, we'll
            // break memory up into it's logical parts
            // TODO: fix
            boot_rom,
            rom_bank_0,
            memory: [1; 65535],
            gpu: GPU::new(),
        }
    }

    pub fn read_byte(&self, address: u16) -> u8
    {
        let address = address as usize;
        match address {
            BOOT_ROM_BEGIN ..= BOOT_ROM_END => {
                if let Some(boot_rom) = self.boot_rom
                {
                    boot_rom[address]
                }
                else
                {
                    self.rom_bank_0[address]
                }
            }
            VRAM_BEGIN ..= VRAM_END => self.gpu.vram[address - VRAM_BEGIN],
            _ => panic!("TODO: support other areas of memory")
        }
    }

    pub fn write_byte(&mut self, address: u16, value: u8) {
        let address = address as usize;
        match address {
            ROM_BANK_0_BEGIN ..= ROM_BANK_0_END => {
                self.rom_bank_0[address] = value;
            }
            VRAM_BEGIN ..= VRAM_END => {
                self.gpu.write_vram(address - VRAM_BEGIN, value);
            }
            _ => panic!("TODO: support other areas of memory")
        }
    }
}

macro_rules! manipulate_8bit_register
{
    // Macro pattern for getting a value from a register and doing some work on that value
    //
    //  # Example usage:
    //  ``` rust
    //  manipulate_8bit_register!(self, a => print_register)
    //  ```
    //
    //  The above reads register `a` and then calls the method `print_register` with the
    //  value from `a`
    ( $self:ident : $getter:ident => $work:ident) => {
        {
            let value = $self.registers.$getter;
            $self.$work(value)
        }
    };

    // Macro pattern for getting a value from a register and doing some work on that value
    // and writing it back to the register
    //
    //  # Example usage:
    //  ``` rust
    //  manipulate_8bit_register!(self, a => increment => d)
    //  ```
    //
    //  The above reads register `a` and then calls the method `increment` with the
    //  value from `a` and then writes the result of `increment` into register `d`
    ( $self:ident : $getter:ident => $work:ident => $setter:ident) => {
        {
            let result = manipulate_8bit_register!($self: $getter => $work);
            $self.registers.$setter = result;
        }
    };
}

macro_rules! arithmetic_instruction
{
    // Macro pattern for matching a register and then manipulating the register
    //
    // # Example Usage:
    // ``` rust
    // arithmetic_instruction!(register, self.foo)
    // ```
    //
    // The above matches a register and then calls the function `foo` to do work on the
    // value in that register.
    ( $register:ident, $self:ident.$work:ident) => {
        {
            match $register
            {
                ArithmeticTarget::A => manipulate_8bit_register!($self: a => $work),
                ArithmeticTarget::B => manipulate_8bit_register!($self: b => $work),
                ArithmeticTarget::C => manipulate_8bit_register!($self: c => $work),
                ArithmeticTarget::D => manipulate_8bit_register!($self: d => $work),
                ArithmeticTarget::E => manipulate_8bit_register!($self: e => $work),
                ArithmeticTarget::H => manipulate_8bit_register!($self: h => $work),
                ArithmeticTarget::L => manipulate_8bit_register!($self: l => $work),
                ArithmeticTarget::D8 => {
                    let value = $self.read_next_byte();
                    $self.$work(value);
                }
                ArithmeticTarget::HLI => {
                    let value = $self.bus.read_byte($self.registers.get_hl());
                    $self.$work(value;)
                }
            };
            match $register
            {
                ArithmeticTarget::D8  => ($self.pc.wrapping_add(2), 8),
                ArithmeticTarget::HLI => ($self.pc.wrapping_add(1), 8),
                _                     => ($self.pc.wrapping_add(1), 4)
            }
        }
    };

    // Macro pattern for matching a register and then manipulating the register and
    // writing the value back to the register
    //
    // # Example Usage:
    // ``` rust
    // arithmetic_instruction!(register, self.foo => a)
    // ```
    //
    // The above matches a register and then calls the function `foo` to do work on the
    // value in that register and wirtes the result of `foo` into the a register.
    ( $register:ident, $self:ident.$work:ident => a) => {
        {
            match $register
            {
                ArithmeticTarget::A => manipulate_8bit_register!($self: a => $work => a),
                ArithmeticTarget::B => manipulate_8bit_register!($self: b => $work => a),
                ArithmeticTarget::C => manipulate_8bit_register!($self: c => $work => a),
                ArithmeticTarget::D => manipulate_8bit_register!($self: d => $work => a),
                ArithmeticTarget::E => manipulate_8bit_register!($self: e => $work => a),
                ArithmeticTarget::H => manipulate_8bit_register!($self: h => $work => a),
                ArithmeticTarget::L => manipulate_8bit_register!($self: l => $work => a),
                ArithmeticTarget::D8 => {
                    let value = $self.read_next_byte();
                    let result = $self.$work(value);
                    $self.registers.a = result;
                }
                ArithmeticTarget::HLI => {
                    let value = $self.bus.read_byte($self.registers.get_hl());
                    let result = $self.$work(value);
                    $self.registers.a = result;
                }
            };
            match $register
            {
                ArithmeticTarget::D8  => ($self.pc.wrapping_add(2), 8),
                ArithmeticTarget::HLI => ($self.pc.wrapping_add(1), 8),
                _                     => ($self.pc.wrapping_add(1), 4)
            }
        }
    };
}

macro_rules! prefix_instruction
{
    // Macro pattern for matching a register and then manipulating the register and writing the
    // value back to the register
    //
    //  # Example usage:
    //  ''' rust
    //  prefix_instruction!(register, self.foo => a)
    //  '''
    //
    //  The above mathes a register and then calls the function `foo` to do work the value
    //  in that register and writes the result of `foo` into the `a` register.
    ( $register:ident, $self:ident.$work:ident => reg) => {
        {
            match $register
            {
                PrefixTarget::A => manipulate_8bit_register!($self: a => $work => a),
                PrefixTarget::B => manipulate_8bit_register!($self: b => $work => b),
                PrefixTarget::C => manipulate_8bit_register!($self: c => $work => c),
                PrefixTarget::D => manipulate_8bit_register!($self: d => $work => d),
                PrefixTarget::E => manipulate_8bit_register!($self: e => $work => e),
                PrefixTarget::H => manipulate_8bit_register!($self: h => $work => h),
                PrefixTarget::L => manipulate_8bit_register!($self: l => $work => l),
                PrefixTarget::HLI => {
                    let hl = $self.registers.get_hl();
                    let value = $self.bus.read_byte(hl);
                    let result = $self.$work(value);
                    $self.bus.write_byte(hl, result);
                }
            }
            let cycles = match $register
            {
                PrefixTarget::HLI => 16,
                _                 => 8
            };
            ($self.pc.wrapping_add(2), cycles)
        }
    };
}

pub struct CPU
{
    pub registers: Registers,
    pc: u16,
    sp: u16,
    pub bus: MemoryBus,
    is_halted: bool,
    interrupts_enabled: bool,
}

impl CPU
{
    pub fn new(boot_rom: Option<Vec<u8>>, game_rom: Vec<u8>) -> CPU
    {
        CPU {
            registers: Registers::new(),
            pc: 0x0,
            sp: 0x00,
            bus: MemoryBus::new(boot_rom, game_rom),
            is_halted: false,
            interrupts_enabled: true,
        }
    }

    pub fn step(&mut self) -> u8
    {
        let mut instruction_byte = self.bus.read_byte(self.pc);
        let prefixed = instruction_byte == 0xCB;
        if prefixed
        {
            instruction_byte = self.bus.read_byte(self.pc + 1);
        }

        let next_pc = if let Some(instruction) = Instruction::from_byte(instruction_byte, prefixed)
        {
            self.execute(instruction)
        }
        else
        {
            let description = format!("0x{}{:x}", if prefixed { "cb" } else { "" }, instruction_byte);
            panic!("Unknown instruction found for: {}", description)
        };

        // TODO restore
        self.pc = next_pc;
    }

    fn execute(&mut self, instruction: Instruction) -> (u16, u8)
    {
        // OPCodes Map: http://pastraiser.com/cpu/gameboy/gameboy_opcodes.html
        // OPCodes Explanation: https://web.archive.org/web/20181009131634/http://www.chrisantonellis.com/files/gameboy/gb-instructions.txt

        // if self.is_halted {
            // // TODO: fix...
           // self.pc.wrapping_add(1)
        // }
        println!("instruction {:?}", instruction);

        match instruction
        {
            Instruction::ADD(register) =>
            {
                // DESCRIPTION: (add) - add the value stored in a specific register
                // with the value in the A register
                // WHEN: target is D8
                // PC:+2
                // Cycles: 8
                // WHEN: target is (HL)
                // PC:+1
                // Cycles: 8
                // ELSE:
                // PC: +1
                // Cycles: 4
                // Z:? S:0 H:? C:?
                arithmetic_instruction!(register, self.add_without_carry => a)
           },

           Instruction::SWAP(register) =>
           {
               println!("swap");
                // DESCRIPTION: switch upper and lower nibble of a specific register
                // PC:+2
                // WHEN: target is (HL):
                // Cycles: 16
                // ELSE:
                // Cycles: 8
                // Z:? S:0 H:0 C:0
                prefix_instruction!(register, self.swap_nibbles => reg)
           },

           Instruction::JP(test) =>
           {
               // DESCRIPTION: conditionally jump to the address stored in the next word in memory
               // PC:?/+3
               // Cycles: 16/12
               // Z:- N:- H:- C:-
               let jump_condition = match test
               {
                   JumpTest::NotZero => !self.registers.f.zero,
                   JumpTest::NotCarry => !self.registers.f.carry,
                   JumpTest::Zero => self.registers.f.zero,
                   JumpTest::Carry => self.registers.f.carry,
                   JumpTest::Always => true
               };
               self.jump(jump_condition)
           },

           Instruction::LD(load_type) =>
           {
               match load_type
               {
                   LoadType::Byte(target, source) =>
                   {
                       let source_value = match source
                       {
                           LoadByteSource::A => self.registers.a,
                           LoadByteSource::D8 => self.read_next_byte(),
                           LoadByteSource::HLI => self.bus.read_byte(self.registers.get_hl()),
                           _ => { panic!("TODO: implement other sources") }
                       };
                       match target
                       {
                           LoadByteTarget::A => self.registers.a = source_value,
                           LoadByteTarget::HLI => self.bus.write_byte(self.registers.get_hl(), source_value),
                           _ => { panic!("TODO: implement other targets") }
                       };
                       match source
                       {
                           LoadByteSource::D8 => (self.pc.wrapping_add(2), 8),
                           _                  => (self.pc.wrapping_add(1), 8),
                       }
                   }

                   // DESCRIPTION: load next word in memory into a particular register
                   // PC:+3
                   // Cycles: 12
                   // Z:- N:- H:- C:-
                   LoadType::Word(target) =>
                   {
                       let word = self.read_next_word();
                       match target
                       {
                           LoadWordTarget::BC => self.registers.set_bc(word),
                           LoadWordTarget::DE => self.registers.set_de(word),
                           LoadWordTarget::HL => self.registers.set_hl(word),
                           LoadWordTarget::SP => self.sp = word,
                       };
                       (self.pc.wrapping_add(3), 12)
                   }
               }
           },

           Instruction::PUSH(target) =>
           {
               // DESCRIPTION: push a value from a given register on to the stack
               // PC:+1
               // Cycles: 16
               // Z:- N:- H:- C:-
               let value = match target
               {
                   StackTarget::BC => self.registers.get_bc(),
                   _ => { panic!("TODO: support more targets") }
               };
               self.push(value);
               (self.pc.wrapping_add(1), 16)
           },

           Instruction::CALL(test) =>
           {
               // DESCRIPTION: Conditionally PUSH the would be instructino to the
               // stack and then jump to a specific address
               // PC:?/+3
               // Cycles: 24/12
               // Z:- N:- H:- C:-

               let jump_condition = match test
               {
                   JumpTest::NotZero => !self.registers.f.zero,
                   _ => { panic!("TODO: support mor conditions") }
               };
               self.call(jump_condition)
           }
            _=> { panic!("TODO: support more instructions") }
        }
    }

    #[inline(always)]
    fn add_without_carry(&mut self, value: u8) -> u8
    {
        self.add(value, false)
    }

    #[inline(always)]
    fn add(&mut self, value: u8, add_carry: bool) -> u8
    {
        let additional_carry = if add_carry && self.registers.f.carry
        {
            1
        }
        else
        {
            0
        };
        let (add, carry) = self.registers.a.overflowing_add(value);
        let (add2, carry2) = add.overflowing_add(additional_carry);

        // set flags
        self.registers.f.zero = add2 == 0;
        self.registers.f.subtract = false;
        self.registers.f.carry = carry || carry2;
        // Half Carry is set if adding the lower nibbles of the value and register A
        // together result in a value bigger than 0xF. If the result is larger than 0xF
        // then the addition caused a carry from the lower nibble to the upper nibble.
        self.registers.f.half_carry =
            ((self.registers.a & 0xF) + (value & 0xF) + additional_carry) > 0xF;
        add2
    }

    #[inline(always)]
    fn swap_nibbles(&mut self, value: u8) -> u8
    {
        let new_value = ((value & 0xf) << 4) | ((value & 0xf0) >> 4);
        self.registers.f.zero = new_value == 0;
        self.registers.f.subtract = false;
        self.registers.f.half_carry = false;
        self.registers.f.carry = false;
        new_value
    }

    #[inline(always)]
    fn jump(&self, should_jump: bool) -> (u16, u8)
    {
        if should_jump
        {
            (self.read_next_word(), 16)
        }
        else
        {
            (self.pc.wrapping_add(3), 12)
        }
    }

    fn push(&mut self, value: u16)
    {
        self.sp = self.sp.wrapping_sub(1);
        self.bus.write_byte(self.sp, ((value & 0xFF00) >> 8) as u8);

        self.sp = self.sp.wrapping_sub(1);
        self.bus.write_byte(self.sp, (value & 0xFF) as u8);
    }

    fn pop(&mut self) -> u16
    {
        let lsb = self.bus.read_byte(self.sp) as u16;
        self.sp = self.sp.wrapping_add(1);

        let msb = self.bus.read_byte(self.sp) as u16;
        self.sp = self.sp.wrapping_add(1);

        (msb << 8) | lsb
    }

    fn call(&mut self, should_jump: bool) -> (u16, u8)
    {
        let next_pc = self.pc.wrapping_add(3);
        if should_jump
        {
            self.push(next_pc);
            (self.read_next_word(), 24)
        }
        else
        {
            (next_pc, 12)
        }
    }

    fn return_(&mut self, should_jump: bool) -> u16
    {
        if should_jump
        {
            self.pop()
        }
        else
        {
            self.pc.wrapping_add(1)
        }
    }

    #[inline(always)]
    fn read_next_word(&self) -> u16
    {
        // Gameboy is little endian so read pc + 2 as most significant bit
        // and pc + 1 as least significant bit
        ((self.bus.read_byte(self.pc + 2) as u16) << 8) | (self.bus.read_byte(self.pc + 1) as u16)
    }

    #[inline(always)]
    fn read_next_byte(&self) -> u8
    {
        self.bus.read_byte(self.pc + 1)
    }
}

fn main()
{
    println!("Hello, Gameboy!");
}
