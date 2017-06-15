require "test_helper"
require 'pry'

class PandocRbTest < Minitest::Test

  def test_that_it_has_a_version_number
    refute_nil ::PandocRb::VERSION
  end

  def test_unicode_preserved
    (1..1114109).each do |char_num|
      str = [char_num].pack("U*")
      if str.encode('UTF-8', 'binary', invalid: :replace, undef: :replace, replace: '') == str
        unless /[^[:print:]]/ === str
          success, result_str = PandocRB.convert 'html', 'html', str
          assert success, "input was: #{[char_num]}, #{str.inspect}, error: #{result_str.unpack("U*")}, #{result_str.inspect}"
          assert_equal Encoding::UTF_8, result_str.encoding
        end
      end
    end
  end

  def test_consistent_results
    100.times do
      in_format_str = "markdown"
      out_format_str = "latex"
      input_str  = "hi there\nhow's it going?"
      output_str = "hi there how's it going?"
      assert_equal [true, output_str], PandocRB::convert(in_format_str, out_format_str, input_str)
    end
  end

  def test_run_haskell_tests
    assert system('stack test')
  end

  def test_argument_errors
    reader_error = assert_raises ArgumentError do
      assert_equal nil, PandocRB.convert('invalidReader', 'latex', "")
    end
    assert_equal nil, reader_error.message
    writer_error = assert_raises ArgumentError do
      assert_equal nil, PandocRB.convert('latex', 'invalidWriter', "")
    end
    assert_equal nil, writer_error.message
  end

  def test_syntax_error
    assert_nil PandocRB.convert('latex', 'latex', "}")
  end

end

