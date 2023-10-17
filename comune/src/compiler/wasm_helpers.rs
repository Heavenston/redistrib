use arrayvec::ArrayVec;
use num_enum::{ TryFromPrimitive, IntoPrimitive };
use vecmap::VecMap;

#[allow(dead_code)]
#[derive(Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord,
         TryFromPrimitive, IntoPrimitive)]
#[repr(u8)]
pub enum OpCode {
    Unreachable  = 0x00,
    Noop         = 0x01,
    Block        = 0x02,
    Loop         = 0x03,
    If           = 0x04,
    Else         = 0x05,

    End          = 0x0B,
    Br           = 0x0C,
    BrIf         = 0x0D,
    BrTable      = 0x0E,
    Return       = 0x0F,
    Call         = 0x10,
    CallIndirect = 0x11,

    RefNull      = 0xD0,
    RefIsNull    = 0xD1,
    RefFunc      = 0xD2,
    
    Drop         = 0x1A,
    Select       = 0x1B,
    SelectT      = 0x1C,

    LocalGet     = 0x20,
    LocalSet     = 0x21,
    LocalTee     = 0x22,
    GlobalGet    = 0x23,
    GlobalSet    = 0x24,

    TableGet,
    TableSet,

    I32Load = 0x28,
    I64Load,

    F32Load,
    F64Load,

    I32Load8s,
    I32Load8u,
    I32Load16s,
    I32Load16u,
    I64Load8s,
    I64Load8u,
    I64Load16s,
    I64Load16u,
    I64Load32s,
    I64Load32u,

    I32Store,
    I64Store,
    F32Store,
    F64Store,
    I32Store8,
    I32Store16,
    I64Store8,
    I64Store16,
    I64Store32,

    MemorySize,
    MemoryGrow,

    I32Const,
    I64Const,
    F32Const,
    F64Const,

    I32Eqz,
    I32Eq,
    I32Ne,
    I32LtS,
    I32LtU,
    I32GtS,
    I32GtU,
    I32LeS,
    I32LeU,
    I32GeS,
    I32GeU,

    I64Eqz,
    I64Eq,
    I64Ne,
    I64LtS,
    I64LtU,
    I64GtS,
    I64GtU,
    I64LeS,
    I64LeU,
    I64GeS,
    I64GeU,

    F32Eq,
    F32Ne,
    F32Lt,
    F32Gt,
    F32Le,
    F32Ge,

    F64Eq,
    F64Ne,
    F64Lt,
    F64Gt,
    F64Le,
    F64Ge,

    I32Clz,
    I32Ctz,
    I32Popcnt,
    I32Add,
    I32Sub,
    I32Mul,
    I32DivS,
    I32DivU,
    I32RemS,
    I32RemU,
    I32And,
    I32Or,
    I32Xor,
    I32Shl,
    I32ShrS,
    I32ShrU,
    I32RotL,
    I32RotR,
    
    I64Clz,
    I64Ctz,
    I64Popcnt,
    I64Add,
    I64Sub,
    I64Mul,
    I64DivS,
    I64DivU,
    I64RemS,
    I64RemU,
    I64And,
    I64Or,
    I64Xor,
    I64Shl,
    I64ShrS,
    I64ShrU,
    I64RotL,
    I64RotR,

    F32Abs,
    F32Neg,
    F32Ceil,
    F32Floor,
    F32Trunc,
    F32Nearest,
    F32Sqrt,
    F32Add,
    F32Sub,
    F32Mul,
    F32Div,
    F32Min,
    F32Max,
    F32Copysign,

    F64Abs,
    F64Neg,
    F64Ceil,
    F64Floor,
    F64Trunc,
    F64Nearest,
    F64Sqrt,
    F64Add,
    F64Sub,
    F64Mul,
    F64Div,
    F64Min,
    F64Max,
    F64Copysign,

    I32WrapI64,
    I32TruncF32S,
    I32TruncF32U,
    I32TruncF64S,
    I32TruncF64U,

    I64ExtendI32S,
    I64ExtendI32U,
    I64TruncF32S,
    I64TruncF32U,
    I64TruncF64S,
    I64TruncF64U,

    F32ConvertI32S,
    F32ConvertI32U,
    F32ConvertI64S,
    F32ConvertI64U,
    F32DemoteF64,

    F64ConvertI32S,
    F64ConvertI32U,
    F64ConvertI64S,
    F64ConvertI64U,
    F64PromoteF32,

    I32ReinterpretF32,
    I64ReinterpretF64,
    F32ReinterpretI32,
    F64ReinterpretI64,

    I32Extend8S,
    I32Extend16S,
    I64Extend8S,
    I64Extend16S,
    I64Extend32S,

    /// Prefix used for some instructions
    Prefix = 0xFC,
}

#[allow(dead_code)]
#[derive(Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord,
         TryFromPrimitive, IntoPrimitive)]
#[repr(u8)]
pub enum SectionId {
    Custom,
    Type,
    Import,
    Function,
    Table,
    Memory,
    Global,
    Export,
    Start,
    Element,
    Code,
    Data,
    DataCount,
}

#[allow(dead_code)]
#[derive(Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord,
         TryFromPrimitive, IntoPrimitive)]
#[repr(u8)]
pub enum ValType {
    I32       = 0x7F,
    I64       = 0x7E,
    F32       = 0x7D,
    F64       = 0x7C,
    V128      = 0x7B,
    FuncRef   = 0x70,
    ExternRef = 0x6F,
}

pub trait LEB128: Copy + PartialEq + PartialOrd {
    const RESULT_SIZE: usize;

    fn to_leb128(self) -> ArrayVec<u8, {Self::RESULT_SIZE}>;
    fn try_to_leb128<const CAP: usize>(
        self
    ) -> Result<ArrayVec<u8, CAP>, arrayvec::CapacityError>
        where [u8; Self::RESULT_SIZE]: AsRef<[u8]>
    {
        let mut new = ArrayVec::<u8, CAP>::new();
        new.try_extend_from_slice(self.to_leb128().as_ref())?;
        Ok(new)
    }
    fn ass_to_leb128<const CAP: usize>(
        self
    ) -> ArrayVec<u8, CAP>
        where [u8; Self::RESULT_SIZE]: AsRef<[u8]>
    {
        let mut new = ArrayVec::<u8, CAP>::new();
        new.try_extend_from_slice(self.to_leb128().as_ref()).expect("Number too big");
        new
    }
}

macro_rules! leb128 {
    ($ty: ty, $u: ty, $len: expr) => {
    impl LEB128 for $ty {
        const RESULT_SIZE: usize = $len;

        fn to_leb128(self) -> ArrayVec<u8, {Self::RESULT_SIZE}> {
            let mut v = <$u>::from_ne_bytes(self.to_ne_bytes());

            let mut buf = ArrayVec::new();
            // Add the bytes from least to most significants
            while v > 0 || buf.len() == 0 {
                // First 7 bits with msb set to 1
                buf.push((v & 0x7F) as u8 | 0x80);
                v >>= 7;
            }

            // Unset the first bit of the last byte
            assert!(buf.len() > 0);
            let len = buf.len() - 1;
            buf[len] &= 0x7F;

            buf
        }
    }
    };
}

leb128!(u32, u32, 5);
leb128!(i32, u32, 5);
leb128!(u64, u64, 10);
leb128!(i64, u64, 10);
leb128!(u128, u128, 19);
leb128!(i128, u128, 19);
leb128!(usize, usize, usize::BITS.div_ceil(7) as usize);
leb128!(isize, usize, usize::BITS.div_ceil(7) as usize);

#[derive(Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum BlockType {
    Unit,
    Valtype(ValType),
    X(u64),
}

impl BlockType {
    pub fn to_bytes(self) -> ArrayVec<u8, 5> {
        let mut v = ArrayVec::new();
        match self {
            Self::Unit => {
                v.push(0x40);
            },
            Self::Valtype(vt) => {
                v.push(vt.into());
            },
            Self::X(x) => {
                v = x.try_to_leb128().expect("Invalid block type")
            },
        }
        v
    }
}

#[derive(Default)]
pub struct ExprBuilder {
    data: Vec<u8>,
}

impl ExprBuilder {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn push_instr(&mut self, instr: OpCode) -> &mut Self {
        self.data.push(instr.into());
        self
    }

    pub fn push_blocktype(&mut self, bt: BlockType) -> &mut Self {
        self.data.extend_from_slice(&bt.to_bytes());
        self
    }

    pub fn push_byte(&mut self, val: u8) -> &mut Self {
        self.data.push(val);
        self
    }

    pub fn push_num<T: LEB128>(&mut self, val: T) -> &mut Self
        where [u8; T::RESULT_SIZE]: AsRef<[u8]>
    {
        self.data.extend_from_slice(&val.to_leb128());
        self
    }

    pub fn with_instr(mut self, instr: OpCode) -> Self {
        self.push_instr(instr);
        self
    }

    pub fn with_blocktype(mut self, bt: BlockType) -> Self {
        self.push_blocktype(bt);
        self
    }

    pub fn with_byte(mut self, val: u8) -> Self {
        self.push_byte(val);
        self
    }

    pub fn with_num<T: LEB128>(mut self, val: T) -> Self
        where [u8; T::RESULT_SIZE]: AsRef<[u8]>
    {
        self.push_num(val);
        self
    }

    pub fn build(self) -> Vec<u8> {
        self.data
    }
}

#[derive(Clone, Copy, Hash)]
pub enum ImportDescriptor {
    Func {
        idx: u32,
    },
    Mem {
        min: u32,
        max: Option<u32>,
    },
    Global {
        valtype: ValType,
        mut_: bool,
    },
}

#[derive(Default)]
pub struct WasmModuleBuilder {
    sections: VecMap<SectionId, Vec<Vec<u8>>>,
}

impl WasmModuleBuilder {
    pub fn new() -> Self {
        Self::default()
    }

    fn section_mut(&mut self, id: SectionId) -> &mut Vec<Vec<u8>> {
        self.sections.entry(id)
            .or_insert(vec![])
    }

    /// Appends a segment into the given section and returns its idx
    fn append_segment(&mut self, id: SectionId, data: Vec<u8>) -> u32 {
        let ss = self.section_mut(id);
        ss.push(data);
        (ss.len() - 1).try_into().expect("So much !?")
    }

    pub fn set_mem0(&mut self, min: u32, max: Option<u32>) {
        let section = self.section_mut(SectionId::Memory);

        section.clear();
        section.push(vec![]);
        let mut mem0 = &mut section[0];

        // Number of memories, here hardcoded to one
        // as no more memory is allowed
        if let Some(max) = max {
            mem0.push(0x01);
            mem0.extend_from_slice(min.to_leb128().as_slice());
            mem0.extend_from_slice(max.to_leb128().as_slice());
        }
        else {
            mem0.push(0x00);
            mem0.extend_from_slice(min.to_leb128().as_slice());
        }
    }

    /// Appends a passive data segment and return its Id
    pub fn add_data_passive(&mut self, bytes: &[u8]) -> u32 {
        let mut data = vec![];

        // Passive flag
        data.push(0x01);
        data.extend_from_slice(
            bytes.len().ass_to_leb128::<5>().as_slice()
        );
        data.extend_from_slice(bytes);

        self.append_segment(SectionId::Data, data)
    }

    /// Appends an active data segment and return its Id
    pub fn add_data_active(&mut self, bytes: &[u8], offset: ExprBuilder) -> u32 {
        let mut data = vec![];

        // Passive flag
        data.push(0x00);
        data.extend_from_slice(&offset.build());
        data.extend_from_slice(
            bytes.len().ass_to_leb128::<5>().as_slice()
        );
        data.extend_from_slice(bytes);

        self.append_segment(SectionId::Data, data)
    }

    /// Appends a global segment and return its idx
    pub fn add_global(&mut self, ty: ValType, mut_: bool, expr: ExprBuilder) -> u32 {
        let mut data = vec![];

        data.push(ty.into());
        data.push(if mut_ { 0x01 } else { 0x00 });
        data.extend_from_slice(&expr.build());
        data.push(OpCode::End.into());

        self.append_segment(SectionId::Global, data)
    }

    fn add_export(&mut self, name: &str, kind: u8, idx: u32) -> u32 {
        let mut data = vec![];

        data.extend_from_slice(
            &name.len().ass_to_leb128::<5>()
        );
        data.extend_from_slice(name.as_bytes());
        data.push(kind);
        data.extend_from_slice(&idx.to_leb128());

        self.append_segment(SectionId::Export, data)
    }

    /// Appends a Function export and returns its idx
    pub fn add_func_export(&mut self, name: &str, idx: u32) -> u32 {
        self.add_export(name, 0x00, idx)
    }

    /// Appends a Table export and returns its idx
    pub fn add_table_export(&mut self, name: &str, idx: u32) -> u32 {
        self.add_export(name, 0x01, idx)
    }

    /// Appends a Memory export and returns its idx
    pub fn add_mem_export(&mut self, name: &str, idx: u32) -> u32 {
        self.add_export(name, 0x02, idx)
    }

    /// Appends a Global export and returns its idx
    pub fn add_global_export(&mut self, name: &str, idx: u32) -> u32 {
        self.add_export(name, 0x03, idx)
    }

    /// Appends a type segment and returns its index
    pub fn add_type(&mut self, params: &[ValType], results: &[ValType]) -> u32 {
        let mut data = vec![];

        data.push(0x60);
        data.extend_from_slice(params.len().ass_to_leb128::<5>().as_slice());
        for &s in params {
            data.push(s.into());
        }
        data.extend_from_slice(results.len().ass_to_leb128::<5>().as_slice());
        for &s in results {
            data.push(s.into());
        }

        self.append_segment(SectionId::Type, data)
    }

    pub fn add_function(&mut self, type_idx: u32) -> u32 {
        self.append_segment(SectionId::Function, type_idx.to_leb128().to_vec())
    }

    pub fn add_code(
        &mut self,
        locals: &[(u32, ValType)], expr: ExprBuilder
    ) -> u32 {
        let mut data = vec![];

        let mut func = vec![];
        func.extend_from_slice(
            locals.len().ass_to_leb128::<5>().as_slice()
        );
        for (id, ty) in locals {
            func.extend_from_slice(id.to_leb128().as_slice());
            func.push((*ty).into());
        }
        func.extend_from_slice(&expr.build());
        func.push(OpCode::End.into());

        data.extend_from_slice(
            func.len().ass_to_leb128::<5>().as_slice()
        );
        data.append(&mut func);

        self.append_segment(SectionId::Code, data)
    }

    /// Appends an import segment and returns its id
    pub fn add_import(
        &mut self, module: &str, name: &str,
        desc: ImportDescriptor,
    ) -> u32 {
        let mut data = vec![];

        data.extend_from_slice(&module.len().ass_to_leb128::<5>());
        data.extend_from_slice(module.as_bytes());
        data.extend_from_slice(&name.len().ass_to_leb128::<5>());
        data.extend_from_slice(name.as_bytes());

        match desc {
            ImportDescriptor::Func { idx } => {
                data.push(0x00);
                data.extend_from_slice(&idx.to_leb128());
            },
            ImportDescriptor::Mem { min, max: None } => {
                data.push(0x02);
                data.push(0x00);
                data.extend_from_slice(&min.to_leb128());
            },
            ImportDescriptor::Mem { min, max: Some(max) } => {
                data.push(0x02);
                data.push(0x01);
                data.extend_from_slice(&min.to_leb128());
                data.extend_from_slice(&max.to_leb128());
            },
            ImportDescriptor::Global { valtype, mut_ } => {
                data.push(0x03);
                data.push(valtype.into());
                data.push(if mut_ { 0x00 } else { 0x01 });
            },
        }

        self.append_segment(SectionId::Import, data)
    }

    pub fn build(mut self) -> Vec<u8> {
        let section_order = [
            SectionId::Custom,
            SectionId::Type,
            SectionId::Import,
            SectionId::Function,
            SectionId::Table,
            SectionId::Memory,
            SectionId::Global,
            SectionId::Export,
            SectionId::Start,
            SectionId::Element,
            SectionId::DataCount,
            SectionId::Code,
            SectionId::Data,
        ];

        let mut result = vec![
            // \0asm
            0x00, 0x61, 0x73, 0x6D,
            // Version 1
            0x01, 0x00, 0x00, 0x00, 
        ];
        for (data, id) in section_order.into_iter()
            .filter_map(|id| self.sections.get(&id).zip(Some(id)))
        {
            result.push(id.into());

            let section_byte_size: usize = data.iter().map(|d| d.len())
                .sum();

            let vec_size_bytes = data.len().ass_to_leb128::<5>();

            // Content size
            result.extend_from_slice(
                // The vec size is part of the content
                (section_byte_size + vec_size_bytes.len())
                    .ass_to_leb128::<5>().as_slice()
            );
            // Almost all sections are vectors so just set theirs sizes
            result.extend_from_slice(
                vec_size_bytes.as_slice()
            );
            for d in data {
                result.extend_from_slice(d);
            }
        }
        result
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use std::error::Error;

    #[test]
    fn test_exported_functions() -> Result<(), Box<dyn Error>> {
        let mut b = WasmModuleBuilder::new();

        let glob = b.add_global(ValType::I32, true, ExprBuilder::new()
            .with_instr(OpCode::I32Const)
            .with_num(0u32)
        );

        let fty = b.add_type(&[ValType::I32, ValType::I32], &[ValType::I32]);
        let add_idx = b.add_function(fty);
        b.add_code(&[], ExprBuilder::new()
            .with_instr(OpCode::LocalGet).with_num(0u32)
            .with_instr(OpCode::LocalGet).with_num(1u32)
            .with_instr(OpCode::I32Add)
            .with_instr(OpCode::GlobalGet).with_num(glob)
            .with_instr(OpCode::I32Add)

            .with_instr(OpCode::GlobalGet).with_num(glob)
            .with_instr(OpCode::I32Const).with_num(1u32)
            .with_instr(OpCode::I32Add)
            .with_instr(OpCode::GlobalSet).with_num(glob)
        );
        b.add_func_export("add", add_idx);

        let complex_idx = b.add_function(fty);
        b.add_code(&[], ExprBuilder::new()
            .with_instr(OpCode::LocalGet).with_num(0u32)
            .with_instr(OpCode::LocalGet).with_num(1u32)
            .with_instr(OpCode::Call).with_num(add_idx)
            .with_instr(OpCode::LocalGet).with_num(0u32)
            .with_instr(OpCode::I32Mul)
        );
        b.add_func_export("complex", complex_idx);

        let mut store = wasmer::Store::default();
        let module = wasmer::Module::new(&store, b.build())?;

        let imports = wasmer::Imports::new();
        let instance = wasmer::Instance::new(&mut store, &module, &imports)?;

        let add = instance.exports.get_typed_function::<(i32, i32), i32>(&store, "add")?;
        assert_eq!(add.call(&mut store, 5, 5)?, 10);
        assert_eq!(add.call(&mut store, 5, 5)?, 11);
        assert_eq!(add.call(&mut store, 5, 5)?, 12);

        let complex = instance.exports.get_typed_function::<(i32, i32), i32>(
            &store, "complex"
        )?;
        assert_eq!(complex.call(&mut store, 10, 5)?, 180);

        Ok(())
    }
    
    #[test]
    fn test_imported_functions() -> Result<(), Box<dyn Error>> {
        let mut b = WasmModuleBuilder::new();

        let fty = b.add_type(&[ValType::I32, ValType::I32], &[ValType::I32]);
        let add_idx = b.add_import(
            "sys", "add", ImportDescriptor::Func { idx: fty }
        );

        let complex_idx = b.add_function(fty);
        b.add_code(&[], ExprBuilder::new()
            .with_instr(OpCode::LocalGet).with_num(0u32)
            .with_instr(OpCode::LocalGet).with_num(1u32)
            .with_instr(OpCode::Call).with_num(add_idx)
            .with_instr(OpCode::LocalGet).with_num(0u32)
            .with_instr(OpCode::I32Mul)
        );
        b.add_func_export("complex", complex_idx + 1);

        let mut store = wasmer::Store::default();
        let module = wasmer::Module::new(&store, b.build())?;

        let mut imports = wasmer::Imports::new();
        imports.define(
            "sys", "add",
            wasmer::Function::new_typed(&mut store, |a: i32, b: i32| a + b)
        );
        let instance = wasmer::Instance::new(&mut store, &module, &imports)?;

        let complex = instance.exports.get_typed_function::<(i32, i32), i32>(
            &store, "complex"
        )?;
        assert_eq!(complex.call(&mut store, 10, 5)?, 150);

        Ok(())
    }
}
