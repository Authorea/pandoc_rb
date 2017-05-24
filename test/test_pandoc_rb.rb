require 'minitest/autorun'
require 'pandoc_rb'

class PandocRBTest < Minitest::Test
  def test_consistent_results
    100.times do
      in_format_str = "markdown"
      out_format_str = "latex"
      input_str  = "hi there\nhow's it going?"
      output_str = "hi there how's it going?"
      assert_equal output_str, PandocRB::convert(in_format_str, out_format_str, input_str)
    end
  end

  def test_run_haskell_tests
    assert system('stack test')
  end
end

