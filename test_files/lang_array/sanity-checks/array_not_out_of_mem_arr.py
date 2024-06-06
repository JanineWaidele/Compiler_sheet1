# We have 65536 bytes of mem
# So we have 65536 - 100 - 4 bytes available.
# For the inner array, we need 8 bytes.
# Hence, we can store 16356 elements
arr_arr = 16356 * [[True]]
print(len(arr_arr))
