# CMake generated Testfile for 
# Source directory: /home/dead/CPPcourseITMO/calc-fold-Soboleva-Alyona
# Build directory: /home/dead/CPPcourseITMO/calc-fold-Soboleva-Alyona/build
# 
# This file includes the relevant testing commands required for 
# testing this directory and lists subdirectories to be tested as well.
add_test(tests "/home/dead/CPPcourseITMO/calc-fold-Soboleva-Alyona/build/test/runUnitTests")
set_tests_properties(tests PROPERTIES  _BACKTRACE_TRIPLES "/home/dead/CPPcourseITMO/calc-fold-Soboleva-Alyona/CMakeLists.txt;107;add_test;/home/dead/CPPcourseITMO/calc-fold-Soboleva-Alyona/CMakeLists.txt;0;")
subdirs("googletest")
subdirs("test")
