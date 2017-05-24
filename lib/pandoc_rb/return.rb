require 'ffi'

module PandocRB
  class Return < FFI::Struct
    layout :success, :int,
           :free_me, :pointer,
           :str_ptr, :pointer,
           :length,  :long
  end
end

