require 'test_helper'
require 'pry'
require 'json'
require 'open-uri'
require 'base64'


def valid_utf8?(char_num)
  !(/[^[:print:]]/ === [char_num].pack("U*")) rescue false
end
# if str.encode('UTF-8', 'binary', invalid: :replace, undef: :replace, replace: '') == str

class Enumerator
  def build_ranges
    Enumerator.new do |y|
      y << Range.new(*self.reduce([]) do |state, x|
        last_a, last_b = state
        if last_a.nil?
          [x     , x]
        elsif last_b + 1 == x || last_b == x
          [last_a, x]
        else
          y << (last_a..last_b)
          [x     , x]
        end
      end)
    end
  end
end

class Range
  def filter(&block)
    self.lazy.select(&block).build_ranges
  end
end

def big_list_of_naughty_strings
  @big_list_of_naughty_strings ||= JSON.parse(open('https://raw.githubusercontent.com/minimaxir/big-list-of-naughty-strings/master/blns.base64.json').read).map do |encoded|
    Base64.decode64(encoded).force_encoding('UTF-8')
  end
end

class PandocRbTest < Minitest::Test

#   def test_that_it_has_a_version_number
#     p 1
#     refute_nil ::PandocRb::VERSION
#   end

#   def test_that_it_has_a_readers_list
#     p 2
#     refute_nil ::PandocRb::Readers
#   end

#   def test_that_it_has_a_writers_list
#     p 3
#     refute_nil ::PandocRb::Writers
#   end

#   def test_unicode_preserved
#     p 4
#     # The upper bound of this range was determined by running on an infinite range and waiting for it to stop for a while
#     (1..1_048_573).filter{|x| valid_utf8?(x)}.each do |range|
#       range.each do |char_num|
#         str = [char_num].pack("U*")
#         success, result_str = PandocRb.convert 'html', 'html', str
#         assert success, "input was: #{[char_num]}, #{str.inspect}, error: #{result_str.unpack("U*")}, #{result_str.inspect}"
#         assert_equal Encoding::UTF_8, result_str.encoding
#       end
#     end
#   end

#   def test_consistent_results
#     p 5
#     100.times do
#       in_format_str = "markdown"
#       out_format_str = "latex"
#       input_str  = "hi there\nhow's it going?"
#       output_str = "hi there how's it going?"
#       assert_equal [true, output_str], PandocRb::convert(in_format_str, out_format_str, input_str)
#     end
#   end

#   def test_run_haskell_tests
#     p 6
#     assert system('stack test')
#   end

#   def test_argument_errors
#     p 7
#     reader_error = assert_raises ArgumentError do
#       PandocRb.convert('invalidReader', 'latex', "")
#     end
#     assert_equal 'Unknown reader: invalidReader', reader_error.message
#     writer_error = assert_raises ArgumentError do
#       PandocRb.convert('latex', 'invalidWriter', "")
#     end
#     assert_equal 'Unknown writer: invalidWriter', writer_error.message
#   end

#   def test_syntax_error
#     p 8
#     parse_error = assert_raises PandocRb::ParsecError do
#       PandocRb.convert('latex', 'latex', "}")
#     end
#     assert_equal "}"     , parse_error.input
#     assert_equal "source", parse_error.source_name
#     assert_equal 1       , parse_error.line
#     assert_equal 1       , parse_error.column
#     assert_equal 70      , parse_error.messages.length
#   end

  def test_convergent_conversions
    p 9
    (PandocRb::Readers & PandocRb::Writers).each do |format|
      big_list_of_naughty_strings.each do |str|
        puts "running with #{format}, #{str}"
        first_success, first_conversion = PandocRb.convert format, format, str
        assert_equal Encoding::UTF_8, first_conversion.encoding
        if first_success
          second_success, second_conversion = PandocRb.convert format, format, first_conversion
          assert       second_success  ,                           "second conversion failed"
          assert_equal Encoding::UTF_8 , first_conversion.encoding
          assert_equal first_conversion, second_conversion
        end
      end
    end
  end

  # def test_converting_back
    # reader x -> writer x -> reader x -> writer y -> reader y -> writer x
    #             A                       ==                      B
  # end

end

