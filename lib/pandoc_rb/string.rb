require 'ffi'

module PandocRB
  class String < FFI::Struct
    layout :str_ptr, :pointer,
           :length,  :long

    def self.from_str(string)
      new_hs_string = self.new
      new_hs_string[:str_ptr] = FFI::MemoryPointer.from_string string
      new_hs_string[:length]  = string.size
      new_hs_string
    end
  end
end

