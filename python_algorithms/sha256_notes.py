# Check out this stack overflow post for a python implementation of SHA256:
# https://stackoverflow.com/questions/11937192/sha-256-pseuedocode
#
# Official SHA Standards:
# http://csrc.nist.gov/publications/fips/fips180-4/fips-180-4.pdf

# Official python hash library:
import hashlib

# Example of hashing a string:
#
# Hash some binary string:
# h = hashlib.sha256(b'Something')
#
# Get the resultant string from the hash object:
# hash_value = h.hexdigest()

SIZE = 32
MAX = 2 ** SIZE
MASK = MAX - 1


# Parameters used:
# a, b, c, ..., h => Working variables representing w-bit words in the Hash value H_i
# H_i => The ith hash value
# H_i_j => The jth word in the ith hash value
# K_t => Constant used for iteration t of hash computation
# k => Number of zeroes to be appended to the message
# l => Length of message M in bits
# m => Number of bits in a message block M_i
# M => Message to be hashed
# M_i => Message block i with size of m bits
# M_i_j => jth word of the ith message block
# n => Number of bits to be rotated or shifted when a word is mutated
# N => Number of blocks in the padded message
# T => Temporary w-bit word used n the hash computation
# w => Number of bits in a word
# W_t => The tth w-bit word in the message schedule
#
# Operators:
# & => And (^)
# | => Or (Down caret)
# ^ => Xor (Plus circle)
# ~ => Not (Set theory complement operator)
# + => Addition modulo 2 ** w
# << => Left-shift
# >> => Right-shift
#
# Functions (where x is some w-bit word):
# ROTL_n(x) => Rotate left
# * 0 <= n < w
# * ROTL(x): (x << n) | (x >> w - n)
#
# ROTR_n(x) => Rotate right
# * 0 <= n < w
# * ROTR(x): (x >> n) | (x << w - n)
#
# SHR_n(x) => Right shift
# * 0 <= n < w
# * SHR(x) = x >> n

# Other functions:
# SHA-1:
# * Ch(x, y, z)     = (x & y) ^ (~x & z)            where 0 <= t <= 19
# * Parity(x, y, z) = x ^ y ^ z                     where 20 <= t <= 39
# * Maj(x, y, z)    = (x & y) ^ (x & z) ^ (y & z)   where 40 <= t <= 59
# * Parity(x, y, z) = x ^ y ^ z                     where 60 <= t <= 79
#
# SHA-224 & SHA-256:
# * Ch(x, y, z)  = (x & y) ^ (~x & z)
# * Maj(x, y, z) = (x & y) ^ (x & z) ^ (y & z)
# * S_0_256(x)   = ROTR(x, 2) ^ ROTR(x, 13) ^ ROTR(x, 22)
# * S_1_256(x)   = ROTR(x, 6) ^ ROTR(x, 11) ^ ROTR(x, 25)
# * s_0_256(x)   = ROTR(x, 7) ^ ROTR(x, 18) ^ SHR(x, 3)
# * s_1_256(x)   = ROTR(x, 17) ^ ROTR(x, 19) ^ SHR(x, 10)
#
# SHA-384, SHA-512, SHA-512/224, & SHA-512/256:
# * Ch(x, y, z)     = (x & y) ^ (~x & z)
# * Maj(x, y, z)    = (x & y) ^ (x & z) ^ (y & z)
# * S_0_512(x)      = ROTR(x, 28) ^ ROTR(x, 34) ^ ROTR(x, 39)
# * S_1_512(x)      = ROTR(x, 14) ^ ROTR(x, 18) ^ ROTR(x, 41)
# * s_0_512(x)      = ROTR(x, 1) ^ ROTR(x, 8) ^ SHR(x, 7)
# * s_1_512(x)      = ROTR(x, 19) ^ ROTR(x, 61) ^ SHR(x, 6)

# Constants:
# SHA-1:
# * K_t:
# ** 5a827999   where 0 <= t <= 19
# ** 6ed9eba1   where 20 <= t <= 39
# ** 8f1bbcdc   where 40 <= t <= 59
# ** ca62c1d6   where 60 <= t <= 79
#
# SHA-224 & SHA-256:
# * These algorithms use a sequence of 64 32-bit words, that are
#   the first 32 bits of the fractional parts of the cube roots
#   of the first sixty-four prime numbers (in hex):
# ** 428a2f98 71374491 b5c0fbcf e9b5dba5 3956c25b 59f111f1 923f82a4 ab1c5ed5
# ** d807aa98 12835b01 243185be 550c7dc3 72be5d74 80deb1fe 9bdc06a7 c19bf174
# ** e49b69c1 efbe4786 0fc19dc6 240ca1cc 2de92c6f 4a7484aa 5cb0a9dc 76f988da
# ** 983e5152 a831c66d b00327c8 bf597fc7 c6e00bf3 d5a79147 06ca6351 14292967
# ** 27b70a85 2e1b2138 4d2c6dfc 53380d13 650a7354 766a0abb 81c2c92e 92722c85
# ** a2bfe8a1 a81a664b c24b8b70 c76c51a3 d192e819 d6990624 f40e3585 106aa070
# ** 19a4c116 1e376c08 2748774c 34b0bcb5 391c0cb3 4ed8aa4a 5b9cca4f 682e6ff3
# ** 748f82ee 78a5636f 84c87814 8cc70208 90befffa a4506ceb bef9a3f7 c67178f2
#
# SHA-384, SHA-512, SHA-512/224, & SHA-512/256:
# * These algorithms use a sequence of 80 64-bit words, that are
#   the first 64 bits of the fractional parts of the cube roots
#   of the first 80 prime numbers:
# ** 428a2f98d728ae22 7137449123ef65cd b5c0fbcfec4d3b2f e9b5dba58189dbbc
# ** 3956c25bf348b538 59f111f1b605d019 923f82a4af194f9b ab1c5ed5da6d8118
# ** d807aa98a3030242 12835b0145706fbe 243185be4ee4b28c 550c7dc3d5ffb4e2
# ** 72be5d74f27b896f 80deb1fe3b1696b1 9bdc06a725c71235 c19bf174cf692694
# ** e49b69c19ef14ad2 efbe4786384f25e3 0fc19dc68b8cd5b5 240ca1cc77ac9c65
# ** 2de92c6f592b0275 4a7484aa6ea6e483 5cb0a9dcbd41fbd4 76f988da831153b5
# ** 983e5152ee66dfab a831c66d2db43210 b00327c898fb213f bf597fc7beef0ee4
# ** c6e00bf33da88fc2 d5a79147930aa725 06ca6351e003826f 142929670a0e6e70
# ** 27b70a8546d22ffc 2e1b21385c26c926 4d2c6dfc5ac42aed 53380d139d95b3df
# ** 650a73548baf63de 766a0abb3c77b2a8 81c2c92e47edaee6 92722c851482353b
# ** a2bfe8a14cf10364 a81a664bbc423001 c24b8b70d0f89791 c76c51a30654be30
# ** d192e819d6ef5218 d69906245565a910 f40e35855771202a 106aa07032bbd1b8
# ** 19a4c116b8d2d0c8 1e376c085141ab53 2748774cdf8eeb99 34b0bcb5e19b48a8
# ** 391c0cb3c5c95a63 4ed8aa4ae3418acb 5b9cca4f7763e373 682e6ff3d6b2b8a3
# ** 748f82ee5defb2fc 78a5636f43172f60 84c87814a1f0ab72 8cc702081a6439ec
# ** 90befffa23631e28 a4506cebde82bde9 bef9a3f7b2c67915 c67178f2e372532b
# ** ca273eceea26619c d186b8c721c0c207 eada7dd6cde0eb1e f57d4f7fee6ed178
# ** 06f067aa72176fba 0a637dc5a2c898a6 113f9804bef90dae 1b710b35131c471b
# ** 28db77f523047d84 32caab7b40c72493 3c9ebe0a15c9bebc 431d67c49c100d4c
# ** 4cc5d4becb3e42b6 597f299cfc657e2a 5fcb6fab3ad6faec 6c44198c4a475817



# The algorithms move in two phases:
# * Preprocessing - where various constants and parameters are established
#   and the message is initially padded.
# * Hash Computation - where the data is actually hashed.


# Function definitions:
# 0 <= t <= 19
def Ch(x, y, z):
    return (x & y) ^ (~x & z)

# 20 <= t <= 39
def Parity(x, y, z):
    return x ^ y ^ z

# 40 <= t <= 59
def Maj(x, y, z):
    return (x & y) ^ (x & z) ^ (y & z)

# 60 <= t <= 79
def Parity(x, y, z):
    return x ^ y ^ z


# def mod_add(x, y):
#     return (x + y) % (2 ** SIZE)
#
# def mod_add(x, y):
#     return (x + y) & (2 ** SIZE - 1)


def partition(l, n):
    return [l[i:i + n] for i in range(0, len(l), n)]

def pretty_print_binary(n):
    mask = 2 ** 64 - 1
    s = bin(n & mask)
    prefix = s[:2]
    suffix = s[2:]
    bit_partition = partition(suffix, 8)
    print('%s %s' % (prefix, ''.join(bit_partition)))


# def Ch(x, y, z):
#     return (x & y) ^ (~x & z)
#
# def Maj(x, y, z):
#     return ((x & y) ^ (x & z)) ^ (y & z)
#
# def rotate_right(x, n):
#     return ((x >> n) | (x << MAX - n)) & MASK
#
# def shift_right(x, n):
#     return x >> n
#
# def e_0(x):
#     return (rotate_right(x, 2) ^ rotate_right(x, 13)) ^ rotate_right(x, 22)
#
# def e_1(x):
#     return (rotate_right(x, 6) ^ rotate_right(x, 11)) ^ rotate_right(x, 25)
#
# def s_0(x):
#     return (rotate_right(x, 7) ^ rotate_right(x, 18)) ^ shift_right(x, 3)
#
# def s_1(x):
#     return (rotate_right(x, 17) ^ rotate_right(x, 19)) ^ shift_right(x, 10)
#
# def message_pad(bit_list):
#     pad_one = bit_list + '1'
#     pad_len = len(pad_one)
#     k = 0
#     while ((pad_len + k) - 448) % 512 != 0:
#         k += 1
#     back_append_0 = '0' * k
#     back_append_1 = bin_64bit(len(bit_list))
#     return pad_one + back_append_0 + back_append_1
#
# def message_bit_return(string_input):
#     bit_list = []
#     for i in range(len(string_input)):
#         bit_list.append(bin_8bit(ord(string_input[i])))
#     return ''.join(bit_list)
#
# def message_pre_pro(input_string):
#     bit_main = message_bit_return(input_string)
#     return message_pad(bit_main)
#
# def L_P(SET, n):
#     to_return = []
#     j = 0
#     k = n
#     while k < len(SET) + 1:
#         to_return.append(SET[j:k])
#         j = k
#         k += n
#     return to_return
#
# def message_parsing(input_string):
#     return L_P(message_pre_pro(input_string), 32)
#
# # def message_schedule(index, w_t):
# #     new_word =
