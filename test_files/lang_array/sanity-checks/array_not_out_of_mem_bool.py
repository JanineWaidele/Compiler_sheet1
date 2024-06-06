# We have 65536 bytes of mem
# So we have 65536 - 100 - 4 bytes available.
# This is enough for exactly 16358 elements

arr_bool = 16358 * [True]
print(len(arr_bool))
