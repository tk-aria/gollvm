#ifndef CONFIG_H
#define CONFIG_H

/* Include config.h from llvm */
#include "llvm/Config/config.h"

/* Header files */
#cmakedefine HAVE_LINUX_ETHER_H
#cmakedefine HAVE_LINUX_FILTER_H
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

/* Define to 1 if you have the libc 'mknodat' function, and to 0 if you don't. */
#cmakedefine01 HAVE_MKNODAT

/* Define to 1 if you have the libc 'openat' function, and to 0 if you don't. */
#cmakedefine01 HAVE_OPENAT

/* Define to 1 if you have the libc 'open64' function, and to 0 if you don't. */
#cmakedefine01 HAVE_OPEN64

/* Define to 1 if you have the libc 'pipe2' function, and to 0 if you don't. */
#cmakedefine01 HAVE_PIPE2

/* Define to 1 if you have the libc 'setenv' function, and to 0 if you don't. */
#cmakedefine01 HAVE_SETENV

/* Define to 1 if you have the libc 'splice' function, and to 0 if you don't. */
#cmakedefine01 HAVE_SPLICE

/* Define to 1 if you have the libc 'strerror_r' function, and to 0 if you don't. */
#cmakedefine01 HAVE_STRERROR_R

/* Define to 1 if you have the libc 'syscall' function, and to 0 if you don't. */
#cmakedefine01 HAVE_SYSCALL

/* Define to 1 if you have the libc 'tee' function, and to 0 if you don't. */
#cmakedefine01 HAVE_TEE

/* Define to 1 if you have '__sync_bool_compare_and_swap_4', and to 0 if you don't. */
#cmakedefine01 HAVE_SYNC_BOOL_COMPARE_AND_SWAP_4

/* Define to 1 if you have '__sync_bool_compare_and_swap_8', and to 0 if you don't. */
#cmakedefine01 HAVE_SYNC_BOOL_COMPARE_AND_SWAP_8

/* Define to 1 if you have '__sync_fetch_and_add_4', and to 0 if you don't. */
#cmakedefine01 HAVE_SYNC_FETCH_AND_ADD_4

/* Define to 1 if you have '__sync_add_and_fetch_8', and to 0 if you don't. */
#cmakedefine01 HAVE_SYNC_ADD_AND_FETCH_8

/* Define if we're to use libffi. */
#cmakedefine01 USE_LIBFFI

/* Define if the compiler supports -fsplit-stack */
#cmakedefine01 USING_SPLIT_STACK

#endif
