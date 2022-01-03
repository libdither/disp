
use std::fmt::LowerHex;
use std::io::BufRead;

use super::hasher::Hasher;

impl Hasher<{256 / 8}> for Sha256 {
	fn new() -> Self {
		Sha256::new()
	}
	fn update(&mut self, data: impl AsRef<[u8]>) {
		let data: &[u8] = data.as_ref();
		self.message_hash(data.len() as u64, data).unwrap();
	}
	fn finalize(self) -> [u8; 32] {
		self.hash as [u8; 32]
	}
}

pub type Sha256 = Hash<u32, 64, 64>;

impl Hash<u32, 64, 64> {
    pub fn new() -> Self {
        let iv: U256 = [
            0x6a09e667, 0xbb67ae85, 0x3c6ef372, 0xa54ff53a, 0x510e527f, 0x9b05688c, 0x1f83d9ab,
            0x5be0cd19,
        ];
        Hash {
            hash: iv,
            k_constants: Self::k_constants(),
            scramble_funcs: ScramblePool::<u32> {
                ch: Scramble::<u32>::Ch,
                maj: Scramble::<u32>::Maj,
                σ0: Scramble::<u32>::σ::<7, 18, 3>,
                σ1: Scramble::<u32>::σ::<17, 19, 10>,
                Σ0: Scramble::<u32>::Σ::<2, 13, 22>,
                Σ1: Scramble::<u32>::Σ::<6, 11, 25>,
            },
            block: [0u8; 64],
        }
    }

    pub fn k_constants() -> [u32; 64] {
        [
            0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5, 0x3956c25b, 0x59f111f1, 0x923f82a4,
            0xab1c5ed5, 0xd807aa98, 0x12835b01, 0x243185be, 0x550c7dc3, 0x72be5d74, 0x80deb1fe,
            0x9bdc06a7, 0xc19bf174, 0xe49b69c1, 0xefbe4786, 0x0fc19dc6, 0x240ca1cc, 0x2de92c6f,
            0x4a7484aa, 0x5cb0a9dc, 0x76f988da, 0x983e5152, 0xa831c66d, 0xb00327c8, 0xbf597fc7,
            0xc6e00bf3, 0xd5a79147, 0x06ca6351, 0x14292967, 0x27b70a85, 0x2e1b2138, 0x4d2c6dfc,
            0x53380d13, 0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85, 0xa2bfe8a1, 0xa81a664b,
            0xc24b8b70, 0xc76c51a3, 0xd192e819, 0xd6990624, 0xf40e3585, 0x106aa070, 0x19a4c116,
            0x1e376c08, 0x2748774c, 0x34b0bcb5, 0x391c0cb3, 0x4ed8aa4a, 0x5b9cca4f, 0x682e6ff3,
            0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208, 0x90befffa, 0xa4506ceb, 0xbef9a3f7,
            0xc67178f2,
        ]
    }
}

// Credit to https://github.com/dandyvica/sha, temporary implementation while I figure out how to do this

// hash is either 256 or 512 bits but always 8 u32 or u64 integers
// T is either u32 or u64
pub struct Hash<T, const BLOCKSIZE: usize, const ROUNDS: usize> {
    pub k_constants: [T; ROUNDS],   // ROUNDS = 64 or 80
    pub hash: [T; 8],
    pub scramble_funcs: ScramblePool<T>, // scrambling functions σ etc
    pub block: [u8; BLOCKSIZE], // BLOCKSIZE = 64 or 128
}

use std::convert::TryInto;
pub trait Modular<T> {
    fn add_modulo(&self, y: T) -> T;
    fn to_uint(buffer: &[u8]) -> T;
}

impl Modular<u32> for u32 {
    fn add_modulo(&self, y: u32) -> u32 {
        self.wrapping_add(y)
    }

    fn to_uint(buffer: &[u8]) -> u32 {
        #[cfg(target_endian = "little")]
        return u32::from_be_bytes(buffer.try_into().unwrap());

        #[cfg(target_endian = "big")]
        return u32::from_be_bytes(buffer.try_into().unwrap());
    }
}

impl Modular<u64> for u64 {
    fn add_modulo(&self, y: u64) -> u64 {
        self.wrapping_add(y)
    }

    fn to_uint(buffer: &[u8]) -> u64 {
        #[cfg(target_endian = "little")]
        return u64::from_be_bytes(buffer.try_into().unwrap());

        #[cfg(target_endian = "big")]
        return u64::from_be_bytes(buffer.try_into().unwrap());
    }
}


use std::ops::{BitAnd, BitXor, Not, Shr};

// functions used to rotate, shift etc
pub type FnScramble3<T> = fn(T, T, T) -> T;
pub type FnScramble1<T> = fn(T) -> T;

// need to implement this trait for u32/u64 rotate_right functions
pub trait Shifter<T> {
    fn right_rotate(&self, n: u8) -> T;
    fn right_shift(&self, n: u8) -> T;
}

impl Shifter<u32> for u32 {
    fn right_rotate(&self, n: u8) -> u32 {
        self.rotate_right(n as u32)
    }
    fn right_shift(&self, n: u8) -> u32 {
        self >> n as u32
    }
}

impl Shifter<u64> for u64 {
    fn right_rotate(&self, n: u8) -> u64 {
        self.rotate_right(n as u32)
    }
    fn right_shift(&self, n: u8) -> u64 {
        self >> n as u64
    }
}

pub struct Scramble<T> {
    _id: std::marker::PhantomData<T>,
}

// generic scrambling functions used in SHA calculations. Operate on u32 or u64
#[allow(non_snake_case)]
impl<T> Scramble<T> {
    // Ch(X,Y,Z) = (X ^ Y) ⊕ (!X ^ Z)
    #[allow(non_snake_case)]
    pub fn Ch(x: T, y: T, z: T) -> T
    where
        T: Not<Output = T>,
        T: BitAnd<Output = T>,
        T: BitXor<Output = T>,
        T: Copy,
    {
        (x & y) ^ (!x & z)
    }

    // Maj(X,Y,Z) = (X ^ Y) ⊕ (X ^ Z) ⊕ (Y ^ Z)
    #[allow(non_snake_case)]
    pub fn Maj(x: T, y: T, z: T) -> T
    where
        T: BitAnd<Output = T>,
        T: BitXor<Output = T>,
        T: Copy,
    {
        (x & y) ^ (x & z) ^ (y & z)
    }

    // Σ(X) = RotR(X,A) ⊕ RotR(X,B) ⊕ RotR(X,C)
    #[allow(non_snake_case)]
    pub fn Σ<const A: u8, const B: u8, const C: u8>(x: T) -> T
    where
        T: BitAnd<Output = T>,
        T: BitXor<Output = T>,
        T: Shifter<T>,
        T: Copy,
    {
        x.right_rotate(A) ^ x.right_rotate(B) ^ x.right_rotate(C)
    }

    // σ(X) = RotR(X,A) ⊕ RotR(X,B) ⊕ X >> C
    #[allow(non_snake_case)]
    pub fn σ<const A: u8, const B: u8, const C: u8>(x: T) -> T
    where
        T: Shr<Output = T>,
        T: BitXor<Output = T>,
        T: Shifter<T>,
        T: Copy,
    {
        x.right_rotate(A) ^ x.right_rotate(B) ^ x.right_shift(C)
    }
}

#[allow(non_snake_case)]
pub struct ScramblePool<T> {
    pub ch: FnScramble3<T>,
    pub maj: FnScramble3<T>,
    pub σ0: FnScramble1<T>,
    pub σ1: FnScramble1<T>,
    pub Σ0: FnScramble1<T>,
    pub Σ1: FnScramble1<T>,
}

pub trait Scrambler<T> {
    fn σ<const A: u8, const B: u8, const C: u8>(&self) -> T
    where
        T: Shifter<T>,
        T: Copy;
}

impl<T> Scrambler<T> for T {
    fn σ<const A: u8, const B: u8, const C: u8>(&self) -> T
    where
        T: Shifter<T>,
        T: Copy,
    {
        self.right_shift(C)
    }
}

#[allow(non_snake_case)]
impl<T, const BLOCKSIZE: usize, const ROUNDS: usize> Hash<T, BLOCKSIZE, ROUNDS> {
    pub fn message_hash<R: BufRead>(
        &mut self,
        message_length: u64,
        mut reader: R,
    ) -> Result<(), std::io::Error>
    where
        T: Default,
        T: Copy,
        T: Modular<T>,
        T: LowerHex,
    {
        let mut last_bytes_read = 0usize;

        loop {
            match reader.read(&mut self.block) {
                Ok(bytes_read) => {
                    // EOF: save last file address to restart from this address for next run
                    if bytes_read == 0 {
                        // the message is a multiple of BLOCKSIZE. It still need to be padded
                        if last_bytes_read == BLOCKSIZE {
                            let _ = self.block_padding(0, message_length);
                            self.block_hash();
                        }

                        break;
                    }

                    last_bytes_read = bytes_read;

                    // if bytes_read is exactly BLOCKSIZE, still need to hash
                    if bytes_read == BLOCKSIZE {
                        self.block_hash();
                    // padd buffer if block size is < BLOCKSIZE
                    } else if bytes_read < BLOCKSIZE {
                        // padd buffer if block size is < BLOCKSIZE
                        let additional_block = self.block_padding(bytes_read, message_length);

                        // anyway, hash the last or before last block
                        self.block_hash();

                        // if an additional block is created, use it
                        if let Some(new_block) = additional_block {
                            self.block = new_block;
                            self.block_hash();
                        }
                    } else {
                        panic!("bytes_read > BLOCKSIZE which shouldn't occur");
                    }
                }
                Err(err) => {
                    return Err(err);
                }
            };
        }

        Ok(())
    }

    fn message_schedule(&self) -> [T; ROUNDS]
    where
        T: Default,
        T: Copy,
        T: Modular<T>,
        T: LowerHex,
    {
        // these are W1 to W64
        let mut w = [T::default(); ROUNDS];

        // get the u32 or u64 integer bit size to split the block into chunks
        let size = std::mem::size_of::<T>();

        // get a 'window' of 4 or 8 bytes each
        let mut iter = self.block.chunks(size);

        // first 16 words are the same
        for i in 0..16 {
            w[i] = T::to_uint(iter.next().unwrap());
        }

        // remaining words are given by a formula
        for i in 16..ROUNDS {
            let s1 = w[i - 7].add_modulo((self.scramble_funcs.σ1)(w[i - 2]));
            let s2 = s1.add_modulo((self.scramble_funcs.σ0)(w[i - 15]));
            //w[i] = σ1(w[i - 2]) + w[i - 7] + σ0(w[i - 15]) + w[i - 16];
            w[i] = s2.add_modulo(w[i - 16]);
        }

        // for i in 0..N {
        //     print!("w[{}]={:0x} ", i, w[i]);
        // }
        w
    }

    // a round of sha256 calculation
    pub fn block_hash(&mut self)
    where
        T: Default,
        T: Copy,
        T: Modular<T>,
        T: LowerHex,
    {
        // decompose block
        let W = self.message_schedule();

        // build tmp variables
        let (mut a, mut b, mut c, mut d, mut e, mut f, mut g, mut h) = (
            self.hash[0],
            self.hash[1],
            self.hash[2],
            self.hash[3],
            self.hash[4],
            self.hash[5],
            self.hash[6],
            self.hash[7],
        );

        // 64 rounds
        for i in 0..ROUNDS {
            let s1 = h.add_modulo((self.scramble_funcs.Σ1)(e));
            let s2 = s1.add_modulo((self.scramble_funcs.ch)(e, f, g));
            let s3 = s2.add_modulo(self.k_constants[i]);
            let T1 = s3.add_modulo(W[i]);
            //let T1 = h + Sigma1(e) + Ch(e, f, g) + K[i] + W[i];

            let T2 = (self.scramble_funcs.Σ0)(a).add_modulo((self.scramble_funcs.maj)(a, b, c));

            h = g;
            g = f;
            f = e;
            e = d.add_modulo(T1);
            d = c;
            c = b;
            b = a;
            a = T1.add_modulo(T2);
            // println!(
            //     "{:08x} {:08x} {:08x} {:08x} {:08x} {:08x} {:08x} {:08x} ",
            //     a, b, c, d, e, f, g, h
            // );
        }

        // reallocate H
        self.hash[0] = self.hash[0].add_modulo(a);
        self.hash[1] = self.hash[1].add_modulo(b);
        self.hash[2] = self.hash[2].add_modulo(c);
        self.hash[3] = self.hash[3].add_modulo(d);
        self.hash[4] = self.hash[4].add_modulo(e);
        self.hash[5] = self.hash[5].add_modulo(f);
        self.hash[6] = self.hash[6].add_modulo(g);
        self.hash[7] = self.hash[7].add_modulo(h);
    }

    // pad block
    pub fn block_padding(&mut self, bytes_read: usize, length: u64) -> Option<[u8; BLOCKSIZE]> {
        debug_assert!(bytes_read <= BLOCKSIZE);

        // message length should be in bits
        let message_length = length * 8;

        // this will be added at the end of the message
        let length_as_bytes = message_length.to_be_bytes();

        // either 56 for 512-bit block size (SHA224/256), or 112 for 1024-bit block size (SHA384/512)
        let lower_bound = BLOCKSIZE - BLOCKSIZE / 8;

        // either 64 or 128
        let higher_bound = BLOCKSIZE;

        // in any case, there's enough space to add 0b10000000 (0x80)
        self.block[bytes_read] = 0x80;

        // how many bits to add depends on how enough room is left to let the 64-bit or 128-bit length
        // representation
        if bytes_read < lower_bound {
            for i in bytes_read + 1..higher_bound {
                self.block[i] = 0;
            }
            self.block[higher_bound - 8..higher_bound].clone_from_slice(&length_as_bytes);
            None
        } else {
            for i in bytes_read + 1..higher_bound {
                self.block[i] = 0;
            }
            let mut additional_block = [0u8; BLOCKSIZE];
            additional_block[higher_bound - 8..higher_bound].clone_from_slice(&length_as_bytes);
            Some(additional_block)
        }
    }

    // reset block values
    #[allow(dead_code)]
    pub fn clear(&mut self)
    where
        T: Default,
        T: Copy,
    {
        self.hash = [T::default(); 8];
    }
}

