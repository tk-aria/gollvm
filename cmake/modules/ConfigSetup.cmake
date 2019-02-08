
include(CheckTypeSize)

# Small atomic test case.
set(atomicstuff "
int i;
int main () {
  __atomic_load_n (&i, __ATOMIC_ACQUIRE);
  __atomic_store_n (&i, 1, __ATOMIC_RELEASE);
  __atomic_add_fetch (&i, 1, __ATOMIC_SEQ_CST);
 return 0;
}\n")

# Check for atomics
check_c_source_compiles("${atomicstuff}" HAVE_ATOMIC_FUNCTIONS)

# Assorted things needed by libbacktrace
check_symbol_exists(clock_gettime "time.h" HAVE_CLOCK_GETTIME)
check_symbol_exists(strnlen "string.h" HAVE_DECL_STRNLEN)
list(APPEND CMAKE_REQUIRED_DEFINITIONS "-D_GNU_SOURCE")
check_symbol_exists(dl_iterate_phdr "link.h" HAVE_DL_ITERATE_PHDR)
check_symbol_exists(fcntl "unistd.h;fcntl.h" HAVE_FCNTL)
check_symbol_exists(getexecname "stdlib.h" HAVE_GETEXECNAME)
check_symbol_exists(lstat "sys/types.h;sys/stat.h;unistd.h" HAVE_LSTAT)
check_symbol_exists(readlink "unistd.h" HAVE_READLINK)
check_symbol_exists(mmap "sys/mman.h" HAVE_MMAP)

# Tests for things that libgo needs.
check_symbol_exists(accept4 "sys/types.h;sys/socket.h" HAVE_ACCEPT4)
check_symbol_exists(dup3 "fcntl.h;unistd.h" HAVE_DUP3)
check_symbol_exists(epoll_create1 "sys/epoll.h" HAVE_EPOLL_CREATE1)
check_symbol_exists(faccessat "fcntl.h;unistd.h" HAVE_FACCESSAT)
check_symbol_exists(fallocate "fcntl.h" HAVE_FALLOCATE)
check_symbol_exists(fchmodat "fcntl.h;sys/stat.h" HAVE_FCHMODAT)
check_symbol_exists(fchownat "fcntl.h;unistd.h" HAVE_FCHOWNAT)
check_symbol_exists(futimesat "fcntl.h;sys/time.h" HAVE_FUTIMESAT)
check_symbol_exists(inotify_add_watch "sys/inotify.h" HAVE_INOTIFY_ADD_WATCH)
check_symbol_exists(inotify_init "sys/inotify.h" HAVE_INOTIFY_INIT)
check_symbol_exists(inotify_init1 "sys/inotify.h" HAVE_INOTIFY_INIT1)
check_symbol_exists(inotify_rm_watch "sys/inotify.h" HAVE_INOTIFY_RM_WATCH)
check_symbol_exists(listxattr "sys/types.h;sys/xattr.h" HAVE_LISTXATTR)
check_symbol_exists(mincore "unistd.h;sys/mman.h" HAVE_MINCORE)
check_symbol_exists(mkdirat "fcntl.h;sys/stat.h" HAVE_MKDIRAT)
check_symbol_exists(openat "fcntl.h" HAVE_OPENAT)
check_symbol_exists(removexattr "sys/types.h;sys/xattr.h" HAVE_REMOVEXATTR)
check_symbol_exists(renameat "fcntl.h;stdio.h" HAVE_RENAMEAT)
check_symbol_exists(setenv "stdlib.h" HAVE_SETENV)
check_symbol_exists(setxattr "sys/types.h;sys/xattr.h" HAVE_SETXATTR)
check_symbol_exists(splice "fcntl.h" HAVE_SPLICE)
check_symbol_exists(strerror_r "string.h" HAVE_STRERROR_R)
check_symbol_exists(sync_file_range "fcntl.h" HAVE_SYNC_FILE_RANGE)
check_symbol_exists(syscall "unistd.h" HAVE_SYSCALL)
check_symbol_exists(tee "fcntl.h" HAVE_TEE)
check_symbol_exists(unlinkat "fcntl.h;unistd.h" HAVE_UNLINKAT)
check_symbol_exists(unsetenv "stdlib.h" HAVE_UNSETENV)
check_symbol_exists(unshare "sched.h" HAVE_UNSHARE)
check_symbol_exists(utimensat "fcntl.h;sys/stat.h" HAVE_UTIMENSAT)
check_symbol_exists(wait4 "sys/types.h;sys/time.h;sys/resource.h;sys/wait.h" HAVE_WAIT4)
list(APPEND CMAKE_REQUIRED_DEFINITIONS "-D_LARGEFILE64_SOURCE=1")
check_symbol_exists(open64 "fcntl.h" HAVE_OPEN64)
check_symbol_exists(mknodat "sys/types.h;sys/stat.h;fcntl.h;unistd.h" HAVE_MKNODAT)
check_symbol_exists(pipe2 "unistd.h" HAVE_PIPE2)

# Checks for include files
check_include_file(dlfcn.h HAVE_DLFCN_H)
check_include_file(inttypes.h HAVE_INTTYPES_H)
check_include_file(link.h HAVE_LINK_H)
check_include_file(linux/ether.h HAVE_LINUX_ETHER_H)
check_include_file(linux/filter.h HAVE_LINUX_FILTER_H)
check_include_file(linux/fs.h HAVE_LINUX_FS_H)
check_include_file(linux/if_addr.h HAVE_LINUX_IF_ADDR_H)
check_include_file(linux/if_ether.h HAVE_LINUX_IF_ETHER_H)
check_include_file(linux/if_tun.h HAVE_LINUX_IF_TUN_H)
check_include_file(linux/netlink.h HAVE_LINUX_NETLINK_H)
check_include_file(linux/ptrace.h HAVE_LINUX_PTRACE_H)
check_include_file(linux/reboot.h HAVE_LINUX_REBOOT_H)
check_include_file(linux/rtnetlink.h HAVE_LINUX_RTNETLINK_H)
check_include_file(memory.h HAVE_MEMORY_H)
check_include_file(net/if.h HAVE_NET_IF_H)
check_include_file(net/if_arp.h HAVE_NET_IF_ARP_H)
check_include_file(net/route.h HAVE_NET_ROUTE_H)
check_include_file(netinet/icmp6.h HAVE_NETINET_ICMP6_H)
check_include_file(netinet/if_ether.h HAVE_NETINET_IF_ETHER_H)
check_include_file(netinet/in_systm.h HAVE_NETINET_IN_SYSTM_H)
check_include_file(netinet/ip.h HAVE_NETINET_IP_H)
check_include_file(netinet/ip_mroute.h HAVE_NETINET_IP_MROUTE_H)
check_include_file(netpacket/packet.h HAVE_NETPACKET_PACKET_H)
check_include_file(port.h HAVE_PORT_H)
check_include_file(sched.h HAVE_SCHED_H)
check_include_file(semaphore.h HAVE_SEMAPHORE_H)
check_include_file(stdint.h HAVE_STDINT_H)
check_include_file(stdlib.h HAVE_STDLIB_H)
check_include_file(strings.h HAVE_STRINGS_H)
check_include_file(sys/epoll.h HAVE_SYS_EPOLL_H)
check_include_file(sys/file.h HAVE_SYS_FILE_H)
check_include_file(sys/inotify.h HAVE_SYS_INOTIFY_H)
check_include_file(sys/mman.h HAVE_SYS_MMAN_H)
check_include_file(sys/mount.h HAVE_SYS_MOUNT_H)
check_include_file(sys/prctl.h HAVE_SYS_PRCTL_H)
check_include_file(sys/ptrace.h HAVE_SYS_PTRACE_H)
check_include_file(sys/select.h HAVE_SYS_SELECT_H)
check_include_file(sys/statfs.h HAVE_STATFS_H)
check_include_file(sys/syscall.h SYS_SYSCALL_H)
check_include_file(sys/sysinfo.h HAVE_SYS_SYSINFO_H)
check_include_file(sys/timex.h HAVE_SYS_TIMEX_H)
check_include_file(sys/user.h HAVE_SYS_USER_H)
check_include_file(sys/utsname.h HAVE_SYS_UTSNAME_H)
check_include_file(sys/vfs.h HAVE_SYS_VFS_H)
check_include_file(syscall.h HAVE_SYSCALL_H)
check_include_file(utime.h HAVE_UTIME_H)

# Checks for libraries
check_library_exists(z compress "" HAS_LIBZ)

# To generate libffi headers we need to know the sizes of various types.
check_type_size("double" SIZEOF_DOUBLE)
check_type_size("long double" HAVE_LONG_DOUBLE)
check_type_size("long double" SIZEOF_LONG_DOUBLE)
check_type_size("void*" SIZEOF_VOID_P)

# Check for various include files
check_include_file(alloca.h HAVE_ALLOCA_H)
check_include_file(inttypes.h HAVE_INTTYPES_H)
check_include_file(stdint.h HAVE_STDINT_H)

# Check for various functions
check_function_exists(alloca HAVE_ALLOCA)
check_function_exists(memcpy HAVE_MEMCPY)

check_c_source_compiles("int main(int argc, char **argv) { return __sync_bool_compare_and_swap(&argc, argc, -argc) ? 1 : 0; }\n" HAVE_SYNC_BOOL_COMPARE_AND_SWAP_4)
check_c_source_compiles("#include <stdint.h>\nstatic uint64_t s;\nint main(int argc, char **argv) { return __sync_bool_compare_and_swap(&s, argc, -argc) ? 0 : 1; }\n" HAVE_SYNC_BOOL_COMPARE_AND_SWAP_8)

check_c_source_compiles("int main() { __asm__(\"aesenc %xmm0, %xmm1\"); return 0; }\n" HAVE_AS_X86_AES)

# Issue an error if the C compiler doesn't support -fsplit-stack
# (in theory you can build libgo without it, so I suppose this could
# be changed to a warning).
if(GOLLVM_USE_SPLIT_STACK)
  check_c_compiler_flag("-fsplit-stack" C_SUPPORTS_SPLIT_STACK)
  if(NOT C_SUPPORTS_SPLIT_STACK)
    message(SEND_ERROR "C compiler does not support -fsplit-stack")
  endif()
  set(USING_SPLIT_STACK 1)
endif()
set(USE_LIBFFI 1)
