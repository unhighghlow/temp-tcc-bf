#supert_platform_executable(supert
#    supert.cpp
#)

project(tcc)

set(_TCC_Type WIN32)

add_executable(tcc ${_TCC_Type} tcc.c)
add_custom_command(TARGET tcc POST_BUILD COMMAND ${CMAKE_COMMAND} -E copy $<TARGET_FILE:tcc> "${CMAKE_SOURCE_DIR}/bin/")
if (WIN32)
set(TCC_INCS -I ${CMAKE_SOURCE_DIR}/tcc/win32/include -I ${CMAKE_SOURCE_DIR}/tcc/win32/include/winapi)
target_compile_definitions(tcc PRIVATE TCC_TARGET_PE)
target_compile_definitions(tcc PRIVATE TCC_TARGET_I386) 
else()

set(TCC_INCS -I ${CMAKE_SOURCE_DIR}/tcc/win32/include -I ${CMAKE_SOURCE_DIR}/tcc/win32/include/winapi)
target_compile_definitions(tcc PRIVATE TCC_TARGET_ELF)
target_compile_definitions(tcc PRIVATE TCC_TARGET_I386)
endif()
target_compile_definitions(tcc PRIVATE ONE_SOURCE=1) 
target_compile_definitions(tcc PRIVATE TCC_TARGET_BF) 

#target_include_directories(tcc PRIVATE ${CMAKE_SOURCE_DIR}/extern)

set(_TCC_BinDir ${CMAKE_SOURCE_DIR}/bin)
set(_TCC_ARGS -I ${CMAKE_SOURCE_DIR}/tcc/include ${TCC_INCS})

if (WIN32)
    set_target_properties(tcc PROPERTIES LINK_FLAGS "/SUBSYSTEM:CONSOLE")
    set_property(TARGET tcc PROPERTY MSVC_RUNTIME_LIBRARY "MultiThreaded$<$<CONFIG:Debug>:Debug>")
endif()

add_custom_command(TARGET tcc POST_BUILD
    #COMMAND chcp 65001
    COMMAND ${_TCC_BinDir}/${_TCC_BIN} ${_TCC_ARGS} -c ${CMAKE_CURRENT_SOURCE_DIR}/lib/libtcc1.c
    COMMAND ${_TCC_BinDir}/${_TCC_BIN} ${_TCC_ARGS} -c ${CMAKE_CURRENT_SOURCE_DIR}/win32/lib/crt1.c
    COMMAND ${_TCC_BinDir}/${_TCC_BIN} ${_TCC_ARGS} -c ${CMAKE_CURRENT_SOURCE_DIR}/win32/lib/crt1w.c
    COMMAND ${_TCC_BinDir}/${_TCC_BIN} ${_TCC_ARGS} -c ${CMAKE_CURRENT_SOURCE_DIR}/win32/lib/wincrt1.c
    COMMAND ${_TCC_BinDir}/${_TCC_BIN} ${_TCC_ARGS} -c ${CMAKE_CURRENT_SOURCE_DIR}/win32/lib/wincrt1w.c
    COMMAND ${_TCC_BinDir}/${_TCC_BIN} ${_TCC_ARGS} -c ${CMAKE_CURRENT_SOURCE_DIR}/win32/lib/dllcrt1.c
    COMMAND ${_TCC_BinDir}/${_TCC_BIN} ${_TCC_ARGS} -c ${CMAKE_CURRENT_SOURCE_DIR}/win32/lib/dllmain.c
    COMMAND ${_TCC_BinDir}/${_TCC_BIN} ${_TCC_ARGS} -c ${CMAKE_CURRENT_SOURCE_DIR}/win32/lib/chkstk.S
    COMMAND ${_TCC_BinDir}/${_TCC_BIN} ${_TCC_ARGS} -w -c ${CMAKE_CURRENT_SOURCE_DIR}/lib/bcheck.c
    COMMAND ${_TCC_BinDir}/${_TCC_BIN} -ar libtcc1-32.a libtcc1.o crt1.o crt1w.o wincrt1.o wincrt1w.o dllcrt1.o dllmain.o chkstk.o bcheck.o
    COMMAND ${CMAKE_COMMAND} -E make_directory "${CMAKE_SOURCE_DIR}/bin"
    COMMAND ${CMAKE_COMMAND} -E copy_if_different "$<TARGET_FILE:tcc>" "${CMAKE_SOURCE_DIR}/bin/"
    COMMAND ${CMAKE_COMMAND} -E copy_if_different "${_TCC_BinDir}/libtcc1-32.a" "${CMAKE_SOURCE_DIR}/bin/"
    COMMAND ${CMAKE_COMMAND} -E copy_if_different "${_TCC_BinDir}/crt1.o" "${CMAKE_SOURCE_DIR}/bin/"
    COMMAND ${CMAKE_COMMAND} -E copy_if_different "${CMAKE_SOURCE_DIR}/test/hello.c" "${CMAKE_SOURCE_DIR}/bin/"
    COMMAND ${CMAKE_COMMAND} -E copy_if_different "${CMAKE_SOURCE_DIR}/test/test.cmd" "${CMAKE_SOURCE_DIR}/bin/"
    COMMAND ${CMAKE_COMMAND} -E copy_directory "${CMAKE_SOURCE_DIR}/tcc/win32/include" "${CMAKE_SOURCE_DIR}/bin/include"
    COMMAND ${CMAKE_COMMAND} -E copy_directory "${CMAKE_SOURCE_DIR}/tcc/include" "${CMAKE_SOURCE_DIR}/bin/include"
    WORKING_DIRECTORY "${_TCC_BinDir}"
    COMMENT "\n~~~~ Compiling libc+crt with tcc.exe"
)
