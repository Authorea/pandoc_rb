# Loads mkmf which is used to make makefiles for Ruby extensions
require 'mkmf'
require 'pry'
require 'open3'

def system_indent(command)
  puts "running #{command}"
  exit_status = system command
  puts "ran #{command}"
  exit_status
end

system_indent 'stack build'

def stack_path
  @stack_path ||= begin
    temp_stack_path = `stack path`.lines.map do |line|
      /^(?<var>[^:]+): (?<val>.*)$/ =~ line.chomp
      [var, val]
    end.to_h
    temp_stack_path['compiler-lib'] = temp_stack_path['compiler-bin'].sub(/bin$/, 'lib')
    temp_stack_path['compiler'] = temp_stack_path['compiler-bin'].sub(/^.*\/([^\/]+)\/bin$/, '\1')
    temp_stack_path
  end
end

# Give it a name
extension_name = 'pandoc_rb'

LIBDIR      = ::CONFIG['libdir']
INCLUDEDIR  = ::CONFIG['includedir']

HEADER_DIRS = [
  # /home/hsolo/.stack/programs/x86_64-linux/ghc-8.0.2/lib/ghc-8.0.2/include/
  File.join(stack_path['compiler-lib'], stack_path['compiler'], "include"),

  # .stack-work/dist/x86_64-osx/Cabal-1.24.2.0/build/PandocRb.dylib/PandocRb.dylib-tmp/
  File.join(stack_path['dist-dir'], "build/PandocRb.dylib/PandocRb.dylib-tmp"),

  # Then search /usr/local for people that installed from source
  '/usr/local/include',

  # Check the ruby install locations
  INCLUDEDIR,

  # Finally fall back to /usr
  '/usr/include',
]

LIB_DIRS = [
  # /home/hsolo/.stack/programs/x86_64-linux/ghc-8.0.2/lib/ghc-8.0.2/rts/
  File.join(stack_path['compiler-lib'], stack_path['compiler'], "rts"),
  File.join(stack_path['local-install-root'], "bin"),

  # First search /opt/local for macports
  '/opt/local/lib',

  # Then search /usr/local for people that installed from source
  '/usr/local/lib',

  # Check the ruby install locations
  LIBDIR,

  # Finally fall back to /usr
  '/usr/lib',
]

dir_config extension_name, HEADER_DIRS, LIB_DIRS

have_header 'stdio.h'
have_header 'ruby.h'
have_header 'HsFFI.h'
have_header 'PandocRb_stub.h', 'HsFFI.h'
find_library 'HSrts-ghc8.0.2', nil

build_command = [ "gcc",
                  File.join(stack_path['local-install-root'], "bin/PandocRb.dylib"),
                  "-I#{File.join(stack_path['compiler-lib'], stack_path['compiler'], "include")}", # HsFFI.h
                  "-I#{File.join(stack_path['dist-dir'], "build/PandocRb.dylib/PandocRb.dylib-tmp")}", # PandocRb_stub.h
                  "-L#{File.join(stack_path['compiler-lib'], stack_path['compiler'], "rts")}",
                  "-lHSrts-ghc8.0.2",
                  "#$INCFLAGS".gsub(/\$\(arch_hdrdir\)/, $arch_hdrdir).gsub(/\$\(hdrdir\)/, $hdrdir).gsub(/\$\(srcdir\)/, $srcdir),
                  "-fPIC",
                  "-O3",
                  "-fno-fast-math",
                  "-ggdb3",
                  "#$warnflags",
                  "-fPIC",
                  "-o",
                  "pandoc_rb.o",
                  "-c",
                  "pandoc_rb.c"].join(' ')

unless system_indent(build_command)
  raise "build failed"
end

clean_command = "rm -f pandoc_rb.so"

unless system_indent(clean_command)
  puts "nothing to clean"
end

prefix = mkintpath(CONFIG["prefix"])
possible_dest_dir = if destdir = prefix[$dest_prefix_pattern, 1]
  prefix = prefix[destdir.size..-1]
  destdir
else
 ''
end

link_command = [ "gcc",
                 "-shared",
                 "-o",
                 "pandoc_rb.so",
                 "pandoc_rb.o",
                 File.join(stack_path['local-install-root'], "bin/PandocRb.dylib"),
                 "-I#{File.join(stack_path['compiler-lib'], stack_path['compiler'], "include")}", # HsFFI.h
                 "-I#{File.join(stack_path['dist-dir'], "build/PandocRb.dylib/PandocRb.dylib-tmp")}", # PandocRb_stub.h
                 "-L#{File.join(stack_path['compiler-lib'], stack_path['compiler'], "rts")}",
                 $LIBPATH.map{|x| "-L" + x}.join(' ').gsub(/\$\(exec_prefix\)/, "$(prefix)").gsub(/\$\(prefix\)/, with_destdir(prefix).unspace).gsub(/\$\(DESTDIR\)/, possible_dest_dir),
                 $LDFLAGS,
                 "-L.",
                 File.join(stack_path['local-install-root'], "bin/PandocRb.dylib"),
                 "-I#{File.join(stack_path['compiler-lib'], stack_path['compiler'], "include")}", # HsFFI.h
                 "-I#{File.join(stack_path['dist-dir'], "build/PandocRb.dylib/PandocRb.dylib-tmp")}", # PandocRb_stub.h
                 "-L#{File.join(stack_path['compiler-lib'], stack_path['compiler'], "rts")}",
                 "-fstack-protector",
                 "-rdynamic",
                 "-Wl,-export-dynamic",
                 "-Wl,-rpath,'${ORIGIN}/../lib'",
                 "-Wl,-R'${ORIGIN}/../lib'",
                 "-Wl,-R'#{File.join(stack_path['compiler-lib'], stack_path['compiler'], "rts")}'",
                 "-Wl,-rpath,'#{File.join(stack_path['compiler-lib'], stack_path['compiler'], "rts")}'",
                 "-Wl,-R'#{File.join(stack_path['local-install-root'], "bin")}'",
                 "-Wl,-rpath,'#{File.join(stack_path['local-install-root'], "bin")}'",
                 "-lruby",
                 "-lHSrts-ghc8.0.2",
                 "-lpthread",
                 "-ldl",
                 "-lcrypt",
                 "-lm",
                 "-lc"].join(' ')

unless system_indent(link_command)
  raise "linking failed"
end

begin
	require File.expand_path('pandoc_rb')
  PandocRb.convert_init
	if y = PandocRb.convert_raw('markdown', 'latex', '- hi\n-there!', '')
    PandocRb.convert_exit
  	puts "build succeeded"
  else
    PandocRb.convert_exit
    raise "the build steps succeeded, but the resulting file does not work"
  end
end

create_makefile 'pandoc_rb/pandoc_rb'


