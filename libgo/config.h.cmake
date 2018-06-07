#ifndef CONFIG_H
#define CONFIG_H

/* Include config.h from llvm */
#include "llvm/Config/config.h"

/* Header files */
#cmakedefine HAVE_LINUX_ETHER_H
#cmakedefine HAVE_LINUX_FILTER_H
#cmakedefine HAVE_LINUX_FS_H
#cmakedefine HAVE_LINUX_IF_ADDR_H
#cmakedefine HAVE_LINUX_IF_ETHER_H
#cmakedefine HAVE_LINUX_IF_TUN_H
#cmakedefine HAVE_LINUX_NETLINK_H
#cmakedefine HAVE_LINUX_PTRACE_H
#cmakedefine HAVE_LINUX_REBOOT_H
#cmakedefine HAVE_LINUX_RTNETLINK_H
#cmakedefine HAVE_NETINET_ICMP6_H
#cmakedefine HAVE_NETINET_IF_ETHER_H
#cmakedefine HAVE_NETINET_IN_SYSTM_H
#cmakedefine HAVE_NETINET_IP_H
#cmakedefine HAVE_NETINET_IP_MROUTE_H
#cmakedefine HAVE_NETPACKET_PACKET_H
#cmakedefine HAVE_NET_IF_ARP_H
#cmakedefine HAVE_NET_IF_H
#cmakedefine HAVE_NET_ROUTE_H
#cmakedefine HAVE_PORT_H
#cmakedefine HAVE_SCHED_H
#cmakedefine HAVE_SEMAPHORE_H
#cmakedefine HAVE_STATFS_H
#cmakedefine HAVE_SYSCALL_H
#cmakedefine HAVE_SYS_EPOLL_H
#cmakedefine HAVE_SYS_FILE_H
#cmakedefine HAVE_SYS_INOTIFY_H
#cmakedefine HAVE_SYS_MMAN_H
#cmakedefine HAVE_SYS_MOUNT_H
#cmakedefine HAVE_SYS_PRCTL_H
#cmakedefine HAVE_SYS_PTRACE_H
#cmakedefine HAVE_SYS_SELECT_H
#cmakedefine HAVE_SYS_SYSCALL_H
#cmakedefine HAVE_SYS_SYSINFO_H
#cmakedefine HAVE_SYS_TIMEX_H
#cmakedefine HAVE_SYS_USER_H
#cmakedefine HAVE_SYS_UTSNAME_H
#cmakedefine HAVE_SYS_VFS_H
#cmakedefine HAVE_USTAT_H
#cmakedefine HAVE_UTIME_H

/* Define to 1 if you have the `accept4' function. */
#cmakedefine HAVE_ACCEPT4 1

/* Define to 1 if you have the `dup3' function. */
#cmakedefine HAVE_DUP3 1

/* Define to 1 if you have the `epoll_create1' function. */
#cmakedefine HAVE_EPOLL_CREATE1 1

/* Define to 1 if you have the `faccessat' function. */
#cmakedefine HAVE_FACCESSAT 1

/* Define to 1 if you have the `fallocate' function. */
#cmakedefine HAVE_FALLOCATE 1

/* Define to 1 if you have the `fchmodat' function. */
#cmakedefine HAVE_FCHMODAT 1

/* Define to 1 if you have the `fchownat' function. */
#cmakedefine HAVE_FCHOWNAT 1

/* Define to 1 if you have the `futimesat' function. */
#cmakedefine HAVE_FUTIMESAT 1

/* Define to 1 if you have the `inotify_add_watch' function. */
#cmakedefine HAVE_INOTIFY_ADD_WATCH 1

/* Define to 1 if you have the `inotify_init' function. */
#cmakedefine HAVE_INOTIFY_INIT 1

/* Define to 1 if you have the `inotify_init1' function. */
#cmakedefine HAVE_INOTIFY_INIT1 1

/* Define to 1 if you have the `inotify_rm_watch' function. */
#cmakedefine HAVE_INOTIFY_RM_WATCH 1

/* Define to 1 if you have the `listxattr' function. */
#cmakedefine HAVE_LISTXATTR 1

/* Define to 1 if you have the `mincore' function. */
#cmakedefine HAVE_MINCORE 1

/* Define to 1 if you have the `mkdirat' function. */
#cmakedefine HAVE_MKDIRAT 1

/* Define to 1 if you have the libc 'mknodat' function. */
#cmakedefine HAVE_MKNODAT 1

/* Define to 1 if you have the libc 'openat' function. */
#cmakedefine HAVE_OPENAT 1

/* Define to 1 if you have the libc 'open64' function. */
#cmakedefine HAVE_OPEN64 1

/* Define to 1 if you have the libc 'pipe2' function. */
#cmakedefine HAVE_PIPE2 1

/* Define to 1 if you have the `removexattr' function. */
#cmakedefine HAVE_REMOVEXATTR 1

/* Define to 1 if you have the `renameat' function. */
#cmakedefine HAVE_RENAMEAT 1

/* Define to 1 if you have the libc 'setenv' function. */
#cmakedefine HAVE_SETENV 1

/* Define to 1 if you have the `setxattr' function. */
#cmakedefine HAVE_SETXATTR 1

/* Define to 1 if you have the libc 'splice' function. */
#cmakedefine HAVE_SPLICE 1

/* Define to 1 if you have the libc 'strerror_r' function, and to 0 if you don't. */
#cmakedefine HAVE_STRERROR_R 1

/* Define to 1 if you have the `sync_file_range' function. */
#cmakedefine HAVE_SYNC_FILE_RANGE 1

/* Define to 1 if you have the libc 'syscall' function. */
#cmakedefine HAVE_SYSCALL 1

/* Define to 1 if you have the libc 'tee' function. */
#cmakedefine HAVE_TEE 1

/* Define to 1 if you have the `unlinkat' function. */
#cmakedefine HAVE_UNLINKAT 1

/* Define to 1 if you have the `unsetenv' function. */
#cmakedefine HAVE_UNSETENV 1

/* Define to 1 if you have the `unshare' function. */
#cmakedefine HAVE_UNSHARE 1

/* Define to 1 if you have the `utimensat' function. */
#cmakedefine HAVE_UTIMENSAT 1

/* Define to 1 if you have the `wait4' function. */
#cmakedefine HAVE_WAIT4 1

/* Define to 1 if you have '__sync_bool_compare_and_swap_4'. */
#cmakedefine HAVE_SYNC_BOOL_COMPARE_AND_SWAP_4 1

/* Define to 1 if you have '__sync_bool_compare_and_swap_8'. */
#cmakedefine HAVE_SYNC_BOOL_COMPARE_AND_SWAP_8 1

/* Define to 1 if you have '__sync_fetch_and_add_4'. */
#cmakedefine HAVE_SYNC_FETCH_AND_ADD_4 1

/* Define to 1 if you have '__sync_add_and_fetch_8'. */
#cmakedefine HAVE_SYNC_ADD_AND_FETCH_8 1

/* Define if we're to use libffi. */
#cmakedefine USE_LIBFFI 1

/* Define if the compiler supports -fsplit-stack */
#cmakedefine USING_SPLIT_STACK 1

#endif
