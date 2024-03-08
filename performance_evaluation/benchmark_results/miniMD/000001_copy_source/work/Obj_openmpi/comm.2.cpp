# 1 "comm.cpp"
# 1 "<built-in>" 1
# 1 "<built-in>" 3
# 446 "<built-in>" 3
# 1 "<command line>" 1
# 1 "<built-in>" 2
# 1 "comm.cpp" 2
# 32 "comm.cpp"
# 1 "/usr/include/stdio.h" 1 3 4
# 27 "/usr/include/stdio.h" 3 4
# 1 "/usr/include/bits/libc-header-start.h" 1 3 4
# 33 "/usr/include/bits/libc-header-start.h" 3 4
# 1 "/usr/include/features.h" 1 3 4
# 416 "/usr/include/features.h" 3 4
# 1 "/usr/include/stdc-predef.h" 1 3 4
# 417 "/usr/include/features.h" 2 3 4
# 438 "/usr/include/features.h" 3 4
# 1 "/usr/include/sys/cdefs.h" 1 3 4
# 499 "/usr/include/sys/cdefs.h" 3 4
# 1 "/usr/include/bits/wordsize.h" 1 3 4
# 500 "/usr/include/sys/cdefs.h" 2 3 4
# 1 "/usr/include/bits/long-double.h" 1 3 4
# 501 "/usr/include/sys/cdefs.h" 2 3 4
# 439 "/usr/include/features.h" 2 3 4
# 462 "/usr/include/features.h" 3 4
# 1 "/usr/include/gnu/stubs.h" 1 3 4
# 10 "/usr/include/gnu/stubs.h" 3 4
# 1 "/usr/include/gnu/stubs-64.h" 1 3 4
# 11 "/usr/include/gnu/stubs.h" 2 3 4
# 463 "/usr/include/features.h" 2 3 4
# 34 "/usr/include/bits/libc-header-start.h" 2 3 4
# 28 "/usr/include/stdio.h" 2 3 4

extern "C" {



# 1 "/rwthfs/rz/cluster/work/rwth1269/software/c23/classic-flang/lib/clang/16/include/stddef.h" 1 3 4
# 46 "/rwthfs/rz/cluster/work/rwth1269/software/c23/classic-flang/lib/clang/16/include/stddef.h" 3 4
typedef long unsigned int size_t;
# 34 "/usr/include/stdio.h" 2 3 4


# 1 "/rwthfs/rz/cluster/work/rwth1269/software/c23/classic-flang/lib/clang/16/include/stdarg.h" 1 3 4
# 14 "/rwthfs/rz/cluster/work/rwth1269/software/c23/classic-flang/lib/clang/16/include/stdarg.h" 3 4
typedef __builtin_va_list __gnuc_va_list;
# 37 "/usr/include/stdio.h" 2 3 4

# 1 "/usr/include/bits/types.h" 1 3 4
# 27 "/usr/include/bits/types.h" 3 4
# 1 "/usr/include/bits/wordsize.h" 1 3 4
# 28 "/usr/include/bits/types.h" 2 3 4


typedef unsigned char __u_char;
typedef unsigned short int __u_short;
typedef unsigned int __u_int;
typedef unsigned long int __u_long;


typedef signed char __int8_t;
typedef unsigned char __uint8_t;
typedef signed short int __int16_t;
typedef unsigned short int __uint16_t;
typedef signed int __int32_t;
typedef unsigned int __uint32_t;

typedef signed long int __int64_t;
typedef unsigned long int __uint64_t;






typedef __int8_t __int_least8_t;
typedef __uint8_t __uint_least8_t;
typedef __int16_t __int_least16_t;
typedef __uint16_t __uint_least16_t;
typedef __int32_t __int_least32_t;
typedef __uint32_t __uint_least32_t;
typedef __int64_t __int_least64_t;
typedef __uint64_t __uint_least64_t;



typedef long int __quad_t;
typedef unsigned long int __u_quad_t;







typedef long int __intmax_t;
typedef unsigned long int __uintmax_t;
# 140 "/usr/include/bits/types.h" 3 4
# 1 "/usr/include/bits/typesizes.h" 1 3 4
# 141 "/usr/include/bits/types.h" 2 3 4


typedef unsigned long int __dev_t;
typedef unsigned int __uid_t;
typedef unsigned int __gid_t;
typedef unsigned long int __ino_t;
typedef unsigned long int __ino64_t;
typedef unsigned int __mode_t;
typedef unsigned long int __nlink_t;
typedef long int __off_t;
typedef long int __off64_t;
typedef int __pid_t;
typedef struct { int __val[2]; } __fsid_t;
typedef long int __clock_t;
typedef unsigned long int __rlim_t;
typedef unsigned long int __rlim64_t;
typedef unsigned int __id_t;
typedef long int __time_t;
typedef unsigned int __useconds_t;
typedef long int __suseconds_t;

typedef int __daddr_t;
typedef int __key_t;


typedef int __clockid_t;


typedef void * __timer_t;


typedef long int __blksize_t;




typedef long int __blkcnt_t;
typedef long int __blkcnt64_t;


typedef unsigned long int __fsblkcnt_t;
typedef unsigned long int __fsblkcnt64_t;


typedef unsigned long int __fsfilcnt_t;
typedef unsigned long int __fsfilcnt64_t;


typedef long int __fsword_t;

typedef long int __ssize_t;


typedef long int __syscall_slong_t;

typedef unsigned long int __syscall_ulong_t;



typedef __off64_t __loff_t;
typedef char *__caddr_t;


typedef long int __intptr_t;


typedef unsigned int __socklen_t;




typedef int __sig_atomic_t;
# 39 "/usr/include/stdio.h" 2 3 4
# 1 "/usr/include/bits/types/__fpos_t.h" 1 3 4




# 1 "/usr/include/bits/types/__mbstate_t.h" 1 3 4
# 13 "/usr/include/bits/types/__mbstate_t.h" 3 4
typedef struct
{
  int __count;
  union
  {
    unsigned int __wch;
    char __wchb[4];
  } __value;
} __mbstate_t;
# 6 "/usr/include/bits/types/__fpos_t.h" 2 3 4




typedef struct _G_fpos_t
{
  __off_t __pos;
  __mbstate_t __state;
} __fpos_t;
# 40 "/usr/include/stdio.h" 2 3 4
# 1 "/usr/include/bits/types/__fpos64_t.h" 1 3 4
# 10 "/usr/include/bits/types/__fpos64_t.h" 3 4
typedef struct _G_fpos64_t
{
  __off64_t __pos;
  __mbstate_t __state;
} __fpos64_t;
# 41 "/usr/include/stdio.h" 2 3 4
# 1 "/usr/include/bits/types/__FILE.h" 1 3 4



struct _IO_FILE;
typedef struct _IO_FILE __FILE;
# 42 "/usr/include/stdio.h" 2 3 4
# 1 "/usr/include/bits/types/FILE.h" 1 3 4



struct _IO_FILE;


typedef struct _IO_FILE FILE;
# 43 "/usr/include/stdio.h" 2 3 4
# 1 "/usr/include/bits/types/struct_FILE.h" 1 3 4
# 35 "/usr/include/bits/types/struct_FILE.h" 3 4
struct _IO_FILE;
struct _IO_marker;
struct _IO_codecvt;
struct _IO_wide_data;




typedef void _IO_lock_t;





struct _IO_FILE
{
  int _flags;


  char *_IO_read_ptr;
  char *_IO_read_end;
  char *_IO_read_base;
  char *_IO_write_base;
  char *_IO_write_ptr;
  char *_IO_write_end;
  char *_IO_buf_base;
  char *_IO_buf_end;


  char *_IO_save_base;
  char *_IO_backup_base;
  char *_IO_save_end;

  struct _IO_marker *_markers;

  struct _IO_FILE *_chain;

  int _fileno;
  int _flags2;
  __off_t _old_offset;


  unsigned short _cur_column;
  signed char _vtable_offset;
  char _shortbuf[1];

  _IO_lock_t *_lock;







  __off64_t _offset;

  struct _IO_codecvt *_codecvt;
  struct _IO_wide_data *_wide_data;
  struct _IO_FILE *_freeres_list;
  void *_freeres_buf;
  size_t __pad5;
  int _mode;

  char _unused2[15 * sizeof (int) - 4 * sizeof (void *) - sizeof (size_t)];
};
# 44 "/usr/include/stdio.h" 2 3 4


# 1 "/usr/include/bits/types/cookie_io_functions_t.h" 1 3 4
# 27 "/usr/include/bits/types/cookie_io_functions_t.h" 3 4
typedef __ssize_t cookie_read_function_t (void *__cookie, char *__buf,
                                          size_t __nbytes);







typedef __ssize_t cookie_write_function_t (void *__cookie, const char *__buf,
                                           size_t __nbytes);







typedef int cookie_seek_function_t (void *__cookie, __off64_t *__pos, int __w);


typedef int cookie_close_function_t (void *__cookie);






typedef struct _IO_cookie_io_functions_t
{
  cookie_read_function_t *read;
  cookie_write_function_t *write;
  cookie_seek_function_t *seek;
  cookie_close_function_t *close;
} cookie_io_functions_t;
# 47 "/usr/include/stdio.h" 2 3 4





typedef __gnuc_va_list va_list;
# 63 "/usr/include/stdio.h" 3 4
typedef __off_t off_t;






typedef __off64_t off64_t;






typedef __ssize_t ssize_t;






typedef __fpos_t fpos_t;




typedef __fpos64_t fpos64_t;
# 133 "/usr/include/stdio.h" 3 4
# 1 "/usr/include/bits/stdio_lim.h" 1 3 4
# 134 "/usr/include/stdio.h" 2 3 4



extern FILE *stdin;
extern FILE *stdout;
extern FILE *stderr;






extern int remove (const char *__filename) throw ();

extern int rename (const char *__old, const char *__new) throw ();



extern int renameat (int __oldfd, const char *__old, int __newfd,
       const char *__new) throw ();
# 164 "/usr/include/stdio.h" 3 4
extern int renameat2 (int __oldfd, const char *__old, int __newfd,
        const char *__new, unsigned int __flags) throw ();







extern FILE *tmpfile (void) ;
# 183 "/usr/include/stdio.h" 3 4
extern FILE *tmpfile64 (void) ;



extern char *tmpnam (char *__s) throw () ;




extern char *tmpnam_r (char *__s) throw () ;
# 204 "/usr/include/stdio.h" 3 4
extern char *tempnam (const char *__dir, const char *__pfx)
     throw () __attribute__ ((__malloc__)) ;







extern int fclose (FILE *__stream);




extern int fflush (FILE *__stream);
# 227 "/usr/include/stdio.h" 3 4
extern int fflush_unlocked (FILE *__stream);
# 237 "/usr/include/stdio.h" 3 4
extern int fcloseall (void);
# 246 "/usr/include/stdio.h" 3 4
extern FILE *fopen (const char *__restrict __filename,
      const char *__restrict __modes) ;




extern FILE *freopen (const char *__restrict __filename,
        const char *__restrict __modes,
        FILE *__restrict __stream) ;
# 270 "/usr/include/stdio.h" 3 4
extern FILE *fopen64 (const char *__restrict __filename,
        const char *__restrict __modes) ;
extern FILE *freopen64 (const char *__restrict __filename,
   const char *__restrict __modes,
   FILE *__restrict __stream) ;




extern FILE *fdopen (int __fd, const char *__modes) throw () ;





extern FILE *fopencookie (void *__restrict __magic_cookie,
     const char *__restrict __modes,
     cookie_io_functions_t __io_funcs) throw () ;




extern FILE *fmemopen (void *__s, size_t __len, const char *__modes)
  throw () ;




extern FILE *open_memstream (char **__bufloc, size_t *__sizeloc) throw () ;





extern void setbuf (FILE *__restrict __stream, char *__restrict __buf) throw ();



extern int setvbuf (FILE *__restrict __stream, char *__restrict __buf,
      int __modes, size_t __n) throw ();




extern void setbuffer (FILE *__restrict __stream, char *__restrict __buf,
         size_t __size) throw ();


extern void setlinebuf (FILE *__stream) throw ();







extern int fprintf (FILE *__restrict __stream,
      const char *__restrict __format, ...);




extern int printf (const char *__restrict __format, ...);

extern int sprintf (char *__restrict __s,
      const char *__restrict __format, ...) throw ();





extern int vfprintf (FILE *__restrict __s, const char *__restrict __format,
       __gnuc_va_list __arg);




extern int vprintf (const char *__restrict __format, __gnuc_va_list __arg);

extern int vsprintf (char *__restrict __s, const char *__restrict __format,
       __gnuc_va_list __arg) throw ();



extern int snprintf (char *__restrict __s, size_t __maxlen,
       const char *__restrict __format, ...)
     throw () __attribute__ ((__format__ (__printf__, 3, 4)));

extern int vsnprintf (char *__restrict __s, size_t __maxlen,
        const char *__restrict __format, __gnuc_va_list __arg)
     throw () __attribute__ ((__format__ (__printf__, 3, 0)));





extern int vasprintf (char **__restrict __ptr, const char *__restrict __f,
        __gnuc_va_list __arg)
     throw () __attribute__ ((__format__ (__printf__, 2, 0))) ;
extern int __asprintf (char **__restrict __ptr,
         const char *__restrict __fmt, ...)
     throw () __attribute__ ((__format__ (__printf__, 2, 3))) ;
extern int asprintf (char **__restrict __ptr,
       const char *__restrict __fmt, ...)
     throw () __attribute__ ((__format__ (__printf__, 2, 3))) ;




extern int vdprintf (int __fd, const char *__restrict __fmt,
       __gnuc_va_list __arg)
     __attribute__ ((__format__ (__printf__, 2, 0)));
extern int dprintf (int __fd, const char *__restrict __fmt, ...)
     __attribute__ ((__format__ (__printf__, 2, 3)));







extern int fscanf (FILE *__restrict __stream,
     const char *__restrict __format, ...) ;




extern int scanf (const char *__restrict __format, ...) ;

extern int sscanf (const char *__restrict __s,
     const char *__restrict __format, ...) throw ();
# 434 "/usr/include/stdio.h" 3 4
extern int vfscanf (FILE *__restrict __s, const char *__restrict __format,
      __gnuc_va_list __arg)
     __attribute__ ((__format__ (__scanf__, 2, 0))) ;





extern int vscanf (const char *__restrict __format, __gnuc_va_list __arg)
     __attribute__ ((__format__ (__scanf__, 1, 0))) ;


extern int vsscanf (const char *__restrict __s,
      const char *__restrict __format, __gnuc_va_list __arg)
     throw () __attribute__ ((__format__ (__scanf__, 2, 0)));
# 491 "/usr/include/stdio.h" 3 4
extern int fgetc (FILE *__stream);
extern int getc (FILE *__stream);





extern int getchar (void);






extern int getc_unlocked (FILE *__stream);
extern int getchar_unlocked (void);
# 516 "/usr/include/stdio.h" 3 4
extern int fgetc_unlocked (FILE *__stream);
# 527 "/usr/include/stdio.h" 3 4
extern int fputc (int __c, FILE *__stream);
extern int putc (int __c, FILE *__stream);





extern int putchar (int __c);
# 543 "/usr/include/stdio.h" 3 4
extern int fputc_unlocked (int __c, FILE *__stream);







extern int putc_unlocked (int __c, FILE *__stream);
extern int putchar_unlocked (int __c);






extern int getw (FILE *__stream);


extern int putw (int __w, FILE *__stream);







extern char *fgets (char *__restrict __s, int __n, FILE *__restrict __stream)
          ;
# 593 "/usr/include/stdio.h" 3 4
extern char *fgets_unlocked (char *__restrict __s, int __n,
        FILE *__restrict __stream) ;
# 609 "/usr/include/stdio.h" 3 4
extern __ssize_t __getdelim (char **__restrict __lineptr,
                             size_t *__restrict __n, int __delimiter,
                             FILE *__restrict __stream) ;
extern __ssize_t getdelim (char **__restrict __lineptr,
                           size_t *__restrict __n, int __delimiter,
                           FILE *__restrict __stream) ;







extern __ssize_t getline (char **__restrict __lineptr,
                          size_t *__restrict __n,
                          FILE *__restrict __stream) ;







extern int fputs (const char *__restrict __s, FILE *__restrict __stream);





extern int puts (const char *__s);






extern int ungetc (int __c, FILE *__stream);






extern size_t fread (void *__restrict __ptr, size_t __size,
       size_t __n, FILE *__restrict __stream) ;




extern size_t fwrite (const void *__restrict __ptr, size_t __size,
        size_t __n, FILE *__restrict __s);
# 668 "/usr/include/stdio.h" 3 4
extern int fputs_unlocked (const char *__restrict __s,
      FILE *__restrict __stream);
# 679 "/usr/include/stdio.h" 3 4
extern size_t fread_unlocked (void *__restrict __ptr, size_t __size,
         size_t __n, FILE *__restrict __stream) ;
extern size_t fwrite_unlocked (const void *__restrict __ptr, size_t __size,
          size_t __n, FILE *__restrict __stream);







extern int fseek (FILE *__stream, long int __off, int __whence);




extern long int ftell (FILE *__stream) ;




extern void rewind (FILE *__stream);
# 713 "/usr/include/stdio.h" 3 4
extern int fseeko (FILE *__stream, __off_t __off, int __whence);




extern __off_t ftello (FILE *__stream) ;
# 737 "/usr/include/stdio.h" 3 4
extern int fgetpos (FILE *__restrict __stream, fpos_t *__restrict __pos);




extern int fsetpos (FILE *__stream, const fpos_t *__pos);
# 756 "/usr/include/stdio.h" 3 4
extern int fseeko64 (FILE *__stream, __off64_t __off, int __whence);
extern __off64_t ftello64 (FILE *__stream) ;
extern int fgetpos64 (FILE *__restrict __stream, fpos64_t *__restrict __pos);
extern int fsetpos64 (FILE *__stream, const fpos64_t *__pos);



extern void clearerr (FILE *__stream) throw ();

extern int feof (FILE *__stream) throw () ;

extern int ferror (FILE *__stream) throw () ;



extern void clearerr_unlocked (FILE *__stream) throw ();
extern int feof_unlocked (FILE *__stream) throw () ;
extern int ferror_unlocked (FILE *__stream) throw () ;







extern void perror (const char *__s);





# 1 "/usr/include/bits/sys_errlist.h" 1 3 4
# 26 "/usr/include/bits/sys_errlist.h" 3 4
extern int sys_nerr;
extern const char *const sys_errlist[];


extern int _sys_nerr;
extern const char *const _sys_errlist[];
# 788 "/usr/include/stdio.h" 2 3 4




extern int fileno (FILE *__stream) throw () ;




extern int fileno_unlocked (FILE *__stream) throw () ;
# 806 "/usr/include/stdio.h" 3 4
extern FILE *popen (const char *__command, const char *__modes) ;





extern int pclose (FILE *__stream);





extern char *ctermid (char *__s) throw ();





extern char *cuserid (char *__s);




struct obstack;


extern int obstack_printf (struct obstack *__restrict __obstack,
      const char *__restrict __format, ...)
     throw () __attribute__ ((__format__ (__printf__, 2, 3)));
extern int obstack_vprintf (struct obstack *__restrict __obstack,
       const char *__restrict __format,
       __gnuc_va_list __args)
     throw () __attribute__ ((__format__ (__printf__, 2, 0)));







extern void flockfile (FILE *__stream) throw ();



extern int ftrylockfile (FILE *__stream) throw () ;


extern void funlockfile (FILE *__stream) throw ();
# 864 "/usr/include/stdio.h" 3 4
extern int __uflow (FILE *);
extern int __overflow (FILE *, int);




# 1 "/usr/include/bits/stdio.h" 1 3 4
# 38 "/usr/include/bits/stdio.h" 3 4
extern __inline __attribute__ ((__gnu_inline__)) int
vprintf (const char *__restrict __fmt, __gnuc_va_list __arg)
{
  return vfprintf (stdout, __fmt, __arg);
}



extern __inline __attribute__ ((__gnu_inline__)) int
getchar (void)
{
  return getc (stdin);
}




extern __inline __attribute__ ((__gnu_inline__)) int
fgetc_unlocked (FILE *__fp)
{
  return (__builtin_expect (((__fp)->_IO_read_ptr >= (__fp)->_IO_read_end), 0) ? __uflow (__fp) : *(unsigned char *) (__fp)->_IO_read_ptr++);
}





extern __inline __attribute__ ((__gnu_inline__)) int
getc_unlocked (FILE *__fp)
{
  return (__builtin_expect (((__fp)->_IO_read_ptr >= (__fp)->_IO_read_end), 0) ? __uflow (__fp) : *(unsigned char *) (__fp)->_IO_read_ptr++);
}


extern __inline __attribute__ ((__gnu_inline__)) int
getchar_unlocked (void)
{
  return (__builtin_expect (((stdin)->_IO_read_ptr >= (stdin)->_IO_read_end), 0) ? __uflow (stdin) : *(unsigned char *) (stdin)->_IO_read_ptr++);
}




extern __inline __attribute__ ((__gnu_inline__)) int
putchar (int __c)
{
  return putc (__c, stdout);
}




extern __inline __attribute__ ((__gnu_inline__)) int
fputc_unlocked (int __c, FILE *__stream)
{
  return (__builtin_expect (((__stream)->_IO_write_ptr >= (__stream)->_IO_write_end), 0) ? __overflow (__stream, (unsigned char) (__c)) : (unsigned char) (*(__stream)->_IO_write_ptr++ = (__c)));
}





extern __inline __attribute__ ((__gnu_inline__)) int
putc_unlocked (int __c, FILE *__stream)
{
  return (__builtin_expect (((__stream)->_IO_write_ptr >= (__stream)->_IO_write_end), 0) ? __overflow (__stream, (unsigned char) (__c)) : (unsigned char) (*(__stream)->_IO_write_ptr++ = (__c)));
}


extern __inline __attribute__ ((__gnu_inline__)) int
putchar_unlocked (int __c)
{
  return (__builtin_expect (((stdout)->_IO_write_ptr >= (stdout)->_IO_write_end), 0) ? __overflow (stdout, (unsigned char) (__c)) : (unsigned char) (*(stdout)->_IO_write_ptr++ = (__c)));
}





extern __inline __attribute__ ((__gnu_inline__)) __ssize_t
getline (char **__lineptr, size_t *__n, FILE *__stream)
{
  return __getdelim (__lineptr, __n, '\n', __stream);
}





extern __inline __attribute__ ((__gnu_inline__)) int
 feof_unlocked (FILE *__stream) throw ()
{
  return (((__stream)->_flags & 0x0010) != 0);
}


extern __inline __attribute__ ((__gnu_inline__)) int
 ferror_unlocked (FILE *__stream) throw ()
{
  return (((__stream)->_flags & 0x0020) != 0);
}
# 871 "/usr/include/stdio.h" 2 3 4








}
# 33 "comm.cpp" 2
# 1 "/cvmfs/software.hpc.rwth.de/Linux/RH8/x86_64/intel/sapphirerapids/software/GCCcore/12.3.0/lib/gcc/x86_64-pc-linux-gnu/12.3.0/../../../../include/c++/12.3.0/stdlib.h" 1 3
# 36 "/cvmfs/software.hpc.rwth.de/Linux/RH8/x86_64/intel/sapphirerapids/software/GCCcore/12.3.0/lib/gcc/x86_64-pc-linux-gnu/12.3.0/../../../../include/c++/12.3.0/stdlib.h" 3
# 1 "/cvmfs/software.hpc.rwth.de/Linux/RH8/x86_64/intel/sapphirerapids/software/GCCcore/12.3.0/lib/gcc/x86_64-pc-linux-gnu/12.3.0/../../../../include/c++/12.3.0/cstdlib" 1 3
# 40 "/cvmfs/software.hpc.rwth.de/Linux/RH8/x86_64/intel/sapphirerapids/software/GCCcore/12.3.0/lib/gcc/x86_64-pc-linux-gnu/12.3.0/../../../../include/c++/12.3.0/cstdlib" 3

# 1 "/cvmfs/software.hpc.rwth.de/Linux/RH8/x86_64/intel/sapphirerapids/software/GCCcore/12.3.0/lib/gcc/x86_64-pc-linux-gnu/12.3.0/../../../../include/c++/12.3.0/x86_64-pc-linux-gnu/bits/c++config.h" 1 3
# 296 "/cvmfs/software.hpc.rwth.de/Linux/RH8/x86_64/intel/sapphirerapids/software/GCCcore/12.3.0/lib/gcc/x86_64-pc-linux-gnu/12.3.0/../../../../include/c++/12.3.0/x86_64-pc-linux-gnu/bits/c++config.h" 3
namespace std
{
  typedef long unsigned int size_t;
  typedef long int ptrdiff_t;


  typedef decltype(nullptr) nullptr_t;


#pragma GCC visibility push(default)


  extern "C++" __attribute__ ((__noreturn__, __always_inline__))
  inline void __terminate() noexcept
  {
    void terminate() noexcept __attribute__ ((__noreturn__));
    terminate();
  }
#pragma GCC visibility pop
}
# 329 "/cvmfs/software.hpc.rwth.de/Linux/RH8/x86_64/intel/sapphirerapids/software/GCCcore/12.3.0/lib/gcc/x86_64-pc-linux-gnu/12.3.0/../../../../include/c++/12.3.0/x86_64-pc-linux-gnu/bits/c++config.h" 3
namespace std
{
  inline namespace __cxx11 __attribute__((__abi_tag__ ("cxx11"))) { }
}
namespace __gnu_cxx
{
  inline namespace __cxx11 __attribute__((__abi_tag__ ("cxx11"))) { }
}
# 508 "/cvmfs/software.hpc.rwth.de/Linux/RH8/x86_64/intel/sapphirerapids/software/GCCcore/12.3.0/lib/gcc/x86_64-pc-linux-gnu/12.3.0/../../../../include/c++/12.3.0/x86_64-pc-linux-gnu/bits/c++config.h" 3
namespace std
{
#pragma GCC visibility push(default)




  constexpr inline bool
  __is_constant_evaluated() noexcept
  {





    return __builtin_is_constant_evaluated();



  }
#pragma GCC visibility pop
}
# 655 "/cvmfs/software.hpc.rwth.de/Linux/RH8/x86_64/intel/sapphirerapids/software/GCCcore/12.3.0/lib/gcc/x86_64-pc-linux-gnu/12.3.0/../../../../include/c++/12.3.0/x86_64-pc-linux-gnu/bits/c++config.h" 3
# 1 "/cvmfs/software.hpc.rwth.de/Linux/RH8/x86_64/intel/sapphirerapids/software/GCCcore/12.3.0/lib/gcc/x86_64-pc-linux-gnu/12.3.0/../../../../include/c++/12.3.0/x86_64-pc-linux-gnu/bits/os_defines.h" 1 3
# 656 "/cvmfs/software.hpc.rwth.de/Linux/RH8/x86_64/intel/sapphirerapids/software/GCCcore/12.3.0/lib/gcc/x86_64-pc-linux-gnu/12.3.0/../../../../include/c++/12.3.0/x86_64-pc-linux-gnu/bits/c++config.h" 2 3


# 1 "/cvmfs/software.hpc.rwth.de/Linux/RH8/x86_64/intel/sapphirerapids/software/GCCcore/12.3.0/lib/gcc/x86_64-pc-linux-gnu/12.3.0/../../../../include/c++/12.3.0/x86_64-pc-linux-gnu/bits/cpu_defines.h" 1 3
# 659 "/cvmfs/software.hpc.rwth.de/Linux/RH8/x86_64/intel/sapphirerapids/software/GCCcore/12.3.0/lib/gcc/x86_64-pc-linux-gnu/12.3.0/../../../../include/c++/12.3.0/x86_64-pc-linux-gnu/bits/c++config.h" 2 3
# 841 "/cvmfs/software.hpc.rwth.de/Linux/RH8/x86_64/intel/sapphirerapids/software/GCCcore/12.3.0/lib/gcc/x86_64-pc-linux-gnu/12.3.0/../../../../include/c++/12.3.0/x86_64-pc-linux-gnu/bits/c++config.h" 3
# 1 "/cvmfs/software.hpc.rwth.de/Linux/RH8/x86_64/intel/sapphirerapids/software/GCCcore/12.3.0/lib/gcc/x86_64-pc-linux-gnu/12.3.0/../../../../include/c++/12.3.0/pstl/pstl_config.h" 1 3
# 842 "/cvmfs/software.hpc.rwth.de/Linux/RH8/x86_64/intel/sapphirerapids/software/GCCcore/12.3.0/lib/gcc/x86_64-pc-linux-gnu/12.3.0/../../../../include/c++/12.3.0/x86_64-pc-linux-gnu/bits/c++config.h" 2 3
# 42 "/cvmfs/software.hpc.rwth.de/Linux/RH8/x86_64/intel/sapphirerapids/software/GCCcore/12.3.0/lib/gcc/x86_64-pc-linux-gnu/12.3.0/../../../../include/c++/12.3.0/cstdlib" 2 3
# 75 "/cvmfs/software.hpc.rwth.de/Linux/RH8/x86_64/intel/sapphirerapids/software/GCCcore/12.3.0/lib/gcc/x86_64-pc-linux-gnu/12.3.0/../../../../include/c++/12.3.0/cstdlib" 3
# 1 "/usr/include/stdlib.h" 1 3 4
# 25 "/usr/include/stdlib.h" 3 4
# 1 "/usr/include/bits/libc-header-start.h" 1 3 4
# 26 "/usr/include/stdlib.h" 2 3 4





# 1 "/rwthfs/rz/cluster/work/rwth1269/software/c23/classic-flang/lib/clang/16/include/stddef.h" 1 3 4
# 32 "/usr/include/stdlib.h" 2 3 4

extern "C" {





# 1 "/usr/include/bits/waitflags.h" 1 3 4
# 40 "/usr/include/stdlib.h" 2 3 4
# 1 "/usr/include/bits/waitstatus.h" 1 3 4
# 41 "/usr/include/stdlib.h" 2 3 4
# 55 "/usr/include/stdlib.h" 3 4
# 1 "/usr/include/bits/floatn.h" 1 3 4
# 119 "/usr/include/bits/floatn.h" 3 4
# 1 "/usr/include/bits/floatn-common.h" 1 3 4
# 24 "/usr/include/bits/floatn-common.h" 3 4
# 1 "/usr/include/bits/long-double.h" 1 3 4
# 25 "/usr/include/bits/floatn-common.h" 2 3 4
# 214 "/usr/include/bits/floatn-common.h" 3 4
typedef float _Float32;
# 251 "/usr/include/bits/floatn-common.h" 3 4
typedef double _Float64;
# 268 "/usr/include/bits/floatn-common.h" 3 4
typedef double _Float32x;
# 285 "/usr/include/bits/floatn-common.h" 3 4
typedef long double _Float64x;
# 120 "/usr/include/bits/floatn.h" 2 3 4
# 56 "/usr/include/stdlib.h" 2 3 4


typedef struct
  {
    int quot;
    int rem;
  } div_t;



typedef struct
  {
    long int quot;
    long int rem;
  } ldiv_t;





__extension__ typedef struct
  {
    long long int quot;
    long long int rem;
  } lldiv_t;
# 97 "/usr/include/stdlib.h" 3 4
extern size_t __ctype_get_mb_cur_max (void) throw () ;



extern double atof (const char *__nptr)
     throw () __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1))) ;

extern int atoi (const char *__nptr)
     throw () __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1))) ;

extern long int atol (const char *__nptr)
     throw () __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1))) ;



__extension__ extern long long int atoll (const char *__nptr)
     throw () __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1))) ;



extern double strtod (const char *__restrict __nptr,
        char **__restrict __endptr)
     throw () __attribute__ ((__nonnull__ (1)));



extern float strtof (const char *__restrict __nptr,
       char **__restrict __endptr) throw () __attribute__ ((__nonnull__ (1)));

extern long double strtold (const char *__restrict __nptr,
       char **__restrict __endptr)
     throw () __attribute__ ((__nonnull__ (1)));
# 140 "/usr/include/stdlib.h" 3 4
extern _Float32 strtof32 (const char *__restrict __nptr,
     char **__restrict __endptr)
     throw () __attribute__ ((__nonnull__ (1)));



extern _Float64 strtof64 (const char *__restrict __nptr,
     char **__restrict __endptr)
     throw () __attribute__ ((__nonnull__ (1)));
# 158 "/usr/include/stdlib.h" 3 4
extern _Float32x strtof32x (const char *__restrict __nptr,
       char **__restrict __endptr)
     throw () __attribute__ ((__nonnull__ (1)));



extern _Float64x strtof64x (const char *__restrict __nptr,
       char **__restrict __endptr)
     throw () __attribute__ ((__nonnull__ (1)));
# 176 "/usr/include/stdlib.h" 3 4
extern long int strtol (const char *__restrict __nptr,
   char **__restrict __endptr, int __base)
     throw () __attribute__ ((__nonnull__ (1)));

extern unsigned long int strtoul (const char *__restrict __nptr,
      char **__restrict __endptr, int __base)
     throw () __attribute__ ((__nonnull__ (1)));



__extension__
extern long long int strtoq (const char *__restrict __nptr,
        char **__restrict __endptr, int __base)
     throw () __attribute__ ((__nonnull__ (1)));

__extension__
extern unsigned long long int strtouq (const char *__restrict __nptr,
           char **__restrict __endptr, int __base)
     throw () __attribute__ ((__nonnull__ (1)));




__extension__
extern long long int strtoll (const char *__restrict __nptr,
         char **__restrict __endptr, int __base)
     throw () __attribute__ ((__nonnull__ (1)));

__extension__
extern unsigned long long int strtoull (const char *__restrict __nptr,
     char **__restrict __endptr, int __base)
     throw () __attribute__ ((__nonnull__ (1)));




extern int strfromd (char *__dest, size_t __size, const char *__format,
       double __f)
     throw () __attribute__ ((__nonnull__ (3)));

extern int strfromf (char *__dest, size_t __size, const char *__format,
       float __f)
     throw () __attribute__ ((__nonnull__ (3)));

extern int strfroml (char *__dest, size_t __size, const char *__format,
       long double __f)
     throw () __attribute__ ((__nonnull__ (3)));
# 232 "/usr/include/stdlib.h" 3 4
extern int strfromf32 (char *__dest, size_t __size, const char * __format,
         _Float32 __f)
     throw () __attribute__ ((__nonnull__ (3)));



extern int strfromf64 (char *__dest, size_t __size, const char * __format,
         _Float64 __f)
     throw () __attribute__ ((__nonnull__ (3)));
# 250 "/usr/include/stdlib.h" 3 4
extern int strfromf32x (char *__dest, size_t __size, const char * __format,
   _Float32x __f)
     throw () __attribute__ ((__nonnull__ (3)));



extern int strfromf64x (char *__dest, size_t __size, const char * __format,
   _Float64x __f)
     throw () __attribute__ ((__nonnull__ (3)));
# 272 "/usr/include/stdlib.h" 3 4
# 1 "/usr/include/bits/types/locale_t.h" 1 3 4
# 22 "/usr/include/bits/types/locale_t.h" 3 4
# 1 "/usr/include/bits/types/__locale_t.h" 1 3 4
# 28 "/usr/include/bits/types/__locale_t.h" 3 4
struct __locale_struct
{

  struct __locale_data *__locales[13];


  const unsigned short int *__ctype_b;
  const int *__ctype_tolower;
  const int *__ctype_toupper;


  const char *__names[13];
};

typedef struct __locale_struct *__locale_t;
# 23 "/usr/include/bits/types/locale_t.h" 2 3 4

typedef __locale_t locale_t;
# 273 "/usr/include/stdlib.h" 2 3 4

extern long int strtol_l (const char *__restrict __nptr,
     char **__restrict __endptr, int __base,
     locale_t __loc) throw () __attribute__ ((__nonnull__ (1, 4)));

extern unsigned long int strtoul_l (const char *__restrict __nptr,
        char **__restrict __endptr,
        int __base, locale_t __loc)
     throw () __attribute__ ((__nonnull__ (1, 4)));

__extension__
extern long long int strtoll_l (const char *__restrict __nptr,
    char **__restrict __endptr, int __base,
    locale_t __loc)
     throw () __attribute__ ((__nonnull__ (1, 4)));

__extension__
extern unsigned long long int strtoull_l (const char *__restrict __nptr,
       char **__restrict __endptr,
       int __base, locale_t __loc)
     throw () __attribute__ ((__nonnull__ (1, 4)));

extern double strtod_l (const char *__restrict __nptr,
   char **__restrict __endptr, locale_t __loc)
     throw () __attribute__ ((__nonnull__ (1, 3)));

extern float strtof_l (const char *__restrict __nptr,
         char **__restrict __endptr, locale_t __loc)
     throw () __attribute__ ((__nonnull__ (1, 3)));

extern long double strtold_l (const char *__restrict __nptr,
         char **__restrict __endptr,
         locale_t __loc)
     throw () __attribute__ ((__nonnull__ (1, 3)));
# 316 "/usr/include/stdlib.h" 3 4
extern _Float32 strtof32_l (const char *__restrict __nptr,
       char **__restrict __endptr,
       locale_t __loc)
     throw () __attribute__ ((__nonnull__ (1, 3)));



extern _Float64 strtof64_l (const char *__restrict __nptr,
       char **__restrict __endptr,
       locale_t __loc)
     throw () __attribute__ ((__nonnull__ (1, 3)));
# 337 "/usr/include/stdlib.h" 3 4
extern _Float32x strtof32x_l (const char *__restrict __nptr,
         char **__restrict __endptr,
         locale_t __loc)
     throw () __attribute__ ((__nonnull__ (1, 3)));



extern _Float64x strtof64x_l (const char *__restrict __nptr,
         char **__restrict __endptr,
         locale_t __loc)
     throw () __attribute__ ((__nonnull__ (1, 3)));
# 360 "/usr/include/stdlib.h" 3 4
extern __inline __attribute__ ((__gnu_inline__)) int
 atoi (const char *__nptr) throw ()
{
  return (int) strtol (__nptr, (char **) __null, 10);
}
extern __inline __attribute__ ((__gnu_inline__)) long int
 atol (const char *__nptr) throw ()
{
  return strtol (__nptr, (char **) __null, 10);
}


__extension__ extern __inline __attribute__ ((__gnu_inline__)) long long int
 atoll (const char *__nptr) throw ()
{
  return strtoll (__nptr, (char **) __null, 10);
}
# 385 "/usr/include/stdlib.h" 3 4
extern char *l64a (long int __n) throw () ;


extern long int a64l (const char *__s)
     throw () __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1))) ;




# 1 "/usr/include/sys/types.h" 1 3 4
# 27 "/usr/include/sys/types.h" 3 4
extern "C" {





typedef __u_char u_char;
typedef __u_short u_short;
typedef __u_int u_int;
typedef __u_long u_long;
typedef __quad_t quad_t;
typedef __u_quad_t u_quad_t;
typedef __fsid_t fsid_t;


typedef __loff_t loff_t;




typedef __ino_t ino_t;






typedef __ino64_t ino64_t;




typedef __dev_t dev_t;




typedef __gid_t gid_t;




typedef __mode_t mode_t;




typedef __nlink_t nlink_t;




typedef __uid_t uid_t;
# 97 "/usr/include/sys/types.h" 3 4
typedef __pid_t pid_t;





typedef __id_t id_t;
# 114 "/usr/include/sys/types.h" 3 4
typedef __daddr_t daddr_t;
typedef __caddr_t caddr_t;





typedef __key_t key_t;




# 1 "/usr/include/bits/types/clock_t.h" 1 3 4






typedef __clock_t clock_t;
# 127 "/usr/include/sys/types.h" 2 3 4

# 1 "/usr/include/bits/types/clockid_t.h" 1 3 4






typedef __clockid_t clockid_t;
# 129 "/usr/include/sys/types.h" 2 3 4
# 1 "/usr/include/bits/types/time_t.h" 1 3 4






typedef __time_t time_t;
# 130 "/usr/include/sys/types.h" 2 3 4
# 1 "/usr/include/bits/types/timer_t.h" 1 3 4






typedef __timer_t timer_t;
# 131 "/usr/include/sys/types.h" 2 3 4



typedef __useconds_t useconds_t;



typedef __suseconds_t suseconds_t;





# 1 "/rwthfs/rz/cluster/work/rwth1269/software/c23/classic-flang/lib/clang/16/include/stddef.h" 1 3 4
# 145 "/usr/include/sys/types.h" 2 3 4



typedef unsigned long int ulong;
typedef unsigned short int ushort;
typedef unsigned int uint;




# 1 "/usr/include/bits/stdint-intn.h" 1 3 4
# 24 "/usr/include/bits/stdint-intn.h" 3 4
typedef __int8_t int8_t;
typedef __int16_t int16_t;
typedef __int32_t int32_t;
typedef __int64_t int64_t;
# 156 "/usr/include/sys/types.h" 2 3 4


typedef __uint8_t u_int8_t;
typedef __uint16_t u_int16_t;
typedef __uint32_t u_int32_t;
typedef __uint64_t u_int64_t;


typedef int register_t __attribute__ ((__mode__ (__word__)));
# 176 "/usr/include/sys/types.h" 3 4
# 1 "/usr/include/endian.h" 1 3 4
# 36 "/usr/include/endian.h" 3 4
# 1 "/usr/include/bits/endian.h" 1 3 4
# 37 "/usr/include/endian.h" 2 3 4
# 60 "/usr/include/endian.h" 3 4
# 1 "/usr/include/bits/byteswap.h" 1 3 4
# 33 "/usr/include/bits/byteswap.h" 3 4
static __inline __uint16_t
__bswap_16 (__uint16_t __bsx)
{



  return ((__uint16_t) ((((__bsx) >> 8) & 0xff) | (((__bsx) & 0xff) << 8)));

}






static __inline __uint32_t
__bswap_32 (__uint32_t __bsx)
{



  return ((((__bsx) & 0xff000000u) >> 24) | (((__bsx) & 0x00ff0000u) >> 8) | (((__bsx) & 0x0000ff00u) << 8) | (((__bsx) & 0x000000ffu) << 24));

}
# 69 "/usr/include/bits/byteswap.h" 3 4
__extension__ static __inline __uint64_t
__bswap_64 (__uint64_t __bsx)
{



  return ((((__bsx) & 0xff00000000000000ull) >> 56) | (((__bsx) & 0x00ff000000000000ull) >> 40) | (((__bsx) & 0x0000ff0000000000ull) >> 24) | (((__bsx) & 0x000000ff00000000ull) >> 8) | (((__bsx) & 0x00000000ff000000ull) << 8) | (((__bsx) & 0x0000000000ff0000ull) << 24) | (((__bsx) & 0x000000000000ff00ull) << 40) | (((__bsx) & 0x00000000000000ffull) << 56));

}
# 61 "/usr/include/endian.h" 2 3 4
# 1 "/usr/include/bits/uintn-identity.h" 1 3 4
# 32 "/usr/include/bits/uintn-identity.h" 3 4
static __inline __uint16_t
__uint16_identity (__uint16_t __x)
{
  return __x;
}

static __inline __uint32_t
__uint32_identity (__uint32_t __x)
{
  return __x;
}

static __inline __uint64_t
__uint64_identity (__uint64_t __x)
{
  return __x;
}
# 62 "/usr/include/endian.h" 2 3 4
# 177 "/usr/include/sys/types.h" 2 3 4


# 1 "/usr/include/sys/select.h" 1 3 4
# 30 "/usr/include/sys/select.h" 3 4
# 1 "/usr/include/bits/select.h" 1 3 4
# 22 "/usr/include/bits/select.h" 3 4
# 1 "/usr/include/bits/wordsize.h" 1 3 4
# 23 "/usr/include/bits/select.h" 2 3 4
# 31 "/usr/include/sys/select.h" 2 3 4


# 1 "/usr/include/bits/types/sigset_t.h" 1 3 4



# 1 "/usr/include/bits/types/__sigset_t.h" 1 3 4




typedef struct
{
  unsigned long int __val[(1024 / (8 * sizeof (unsigned long int)))];
} __sigset_t;
# 5 "/usr/include/bits/types/sigset_t.h" 2 3 4


typedef __sigset_t sigset_t;
# 34 "/usr/include/sys/select.h" 2 3 4



# 1 "/usr/include/bits/types/struct_timeval.h" 1 3 4







struct timeval
{
  __time_t tv_sec;
  __suseconds_t tv_usec;
};
# 38 "/usr/include/sys/select.h" 2 3 4

# 1 "/usr/include/bits/types/struct_timespec.h" 1 3 4








struct timespec
{
  __time_t tv_sec;
  __syscall_slong_t tv_nsec;
};
# 40 "/usr/include/sys/select.h" 2 3 4
# 49 "/usr/include/sys/select.h" 3 4
typedef long int __fd_mask;
# 59 "/usr/include/sys/select.h" 3 4
typedef struct
  {



    __fd_mask fds_bits[1024 / (8 * (int) sizeof (__fd_mask))];





  } fd_set;






typedef __fd_mask fd_mask;
# 91 "/usr/include/sys/select.h" 3 4
extern "C" {
# 101 "/usr/include/sys/select.h" 3 4
extern int select (int __nfds, fd_set *__restrict __readfds,
     fd_set *__restrict __writefds,
     fd_set *__restrict __exceptfds,
     struct timeval *__restrict __timeout);
# 113 "/usr/include/sys/select.h" 3 4
extern int pselect (int __nfds, fd_set *__restrict __readfds,
      fd_set *__restrict __writefds,
      fd_set *__restrict __exceptfds,
      const struct timespec *__restrict __timeout,
      const __sigset_t *__restrict __sigmask);
# 126 "/usr/include/sys/select.h" 3 4
}
# 180 "/usr/include/sys/types.h" 2 3 4





typedef __blksize_t blksize_t;






typedef __blkcnt_t blkcnt_t;



typedef __fsblkcnt_t fsblkcnt_t;



typedef __fsfilcnt_t fsfilcnt_t;
# 219 "/usr/include/sys/types.h" 3 4
typedef __blkcnt64_t blkcnt64_t;
typedef __fsblkcnt64_t fsblkcnt64_t;
typedef __fsfilcnt64_t fsfilcnt64_t;





# 1 "/usr/include/bits/pthreadtypes.h" 1 3 4
# 23 "/usr/include/bits/pthreadtypes.h" 3 4
# 1 "/usr/include/bits/thread-shared-types.h" 1 3 4
# 77 "/usr/include/bits/thread-shared-types.h" 3 4
# 1 "/usr/include/bits/pthreadtypes-arch.h" 1 3 4
# 21 "/usr/include/bits/pthreadtypes-arch.h" 3 4
# 1 "/usr/include/bits/wordsize.h" 1 3 4
# 22 "/usr/include/bits/pthreadtypes-arch.h" 2 3 4
# 65 "/usr/include/bits/pthreadtypes-arch.h" 3 4
struct __pthread_rwlock_arch_t
{
  unsigned int __readers;
  unsigned int __writers;
  unsigned int __wrphase_futex;
  unsigned int __writers_futex;
  unsigned int __pad3;
  unsigned int __pad4;

  int __cur_writer;
  int __shared;
  signed char __rwelision;




  unsigned char __pad1[7];


  unsigned long int __pad2;


  unsigned int __flags;
# 99 "/usr/include/bits/pthreadtypes-arch.h" 3 4
};
# 78 "/usr/include/bits/thread-shared-types.h" 2 3 4




typedef struct __pthread_internal_list
{
  struct __pthread_internal_list *__prev;
  struct __pthread_internal_list *__next;
} __pthread_list_t;
# 118 "/usr/include/bits/thread-shared-types.h" 3 4
struct __pthread_mutex_s
{
  int __lock ;
  unsigned int __count;
  int __owner;

  unsigned int __nusers;
# 148 "/usr/include/bits/thread-shared-types.h" 3 4
  int __kind;





  short __spins; short __elision;
  __pthread_list_t __list;
# 166 "/usr/include/bits/thread-shared-types.h" 3 4
};




struct __pthread_cond_s
{
  __extension__ union
  {
    __extension__ unsigned long long int __wseq;
    struct
    {
      unsigned int __low;
      unsigned int __high;
    } __wseq32;
  };
  __extension__ union
  {
    __extension__ unsigned long long int __g1_start;
    struct
    {
      unsigned int __low;
      unsigned int __high;
    } __g1_start32;
  };
  unsigned int __g_refs[2] ;
  unsigned int __g_size[2];
  unsigned int __g1_orig_size;
  unsigned int __wrefs;
  unsigned int __g_signals[2];
};
# 24 "/usr/include/bits/pthreadtypes.h" 2 3 4



typedef unsigned long int pthread_t;




typedef union
{
  char __size[4];
  int __align;
} pthread_mutexattr_t;




typedef union
{
  char __size[4];
  int __align;
} pthread_condattr_t;



typedef unsigned int pthread_key_t;



typedef int pthread_once_t;


union pthread_attr_t
{
  char __size[56];
  long int __align;
};

typedef union pthread_attr_t pthread_attr_t;




typedef union
{
  struct __pthread_mutex_s __data;
  char __size[40];
  long int __align;
} pthread_mutex_t;


typedef union
{
  struct __pthread_cond_s __data;
  char __size[48];
  __extension__ long long int __align;
} pthread_cond_t;





typedef union
{
  struct __pthread_rwlock_arch_t __data;
  char __size[56];
  long int __align;
} pthread_rwlock_t;

typedef union
{
  char __size[8];
  long int __align;
} pthread_rwlockattr_t;





typedef volatile int pthread_spinlock_t;




typedef union
{
  char __size[32];
  long int __align;
} pthread_barrier_t;

typedef union
{
  char __size[4];
  int __align;
} pthread_barrierattr_t;
# 228 "/usr/include/sys/types.h" 2 3 4


}
# 395 "/usr/include/stdlib.h" 2 3 4






extern long int random (void) throw ();


extern void srandom (unsigned int __seed) throw ();





extern char *initstate (unsigned int __seed, char *__statebuf,
   size_t __statelen) throw () __attribute__ ((__nonnull__ (2)));



extern char *setstate (char *__statebuf) throw () __attribute__ ((__nonnull__ (1)));







struct random_data
  {
    int32_t *fptr;
    int32_t *rptr;
    int32_t *state;
    int rand_type;
    int rand_deg;
    int rand_sep;
    int32_t *end_ptr;
  };

extern int random_r (struct random_data *__restrict __buf,
       int32_t *__restrict __result) throw () __attribute__ ((__nonnull__ (1, 2)));

extern int srandom_r (unsigned int __seed, struct random_data *__buf)
     throw () __attribute__ ((__nonnull__ (2)));

extern int initstate_r (unsigned int __seed, char *__restrict __statebuf,
   size_t __statelen,
   struct random_data *__restrict __buf)
     throw () __attribute__ ((__nonnull__ (2, 4)));

extern int setstate_r (char *__restrict __statebuf,
         struct random_data *__restrict __buf)
     throw () __attribute__ ((__nonnull__ (1, 2)));





extern int rand (void) throw ();

extern void srand (unsigned int __seed) throw ();



extern int rand_r (unsigned int *__seed) throw ();







extern double drand48 (void) throw ();
extern double erand48 (unsigned short int __xsubi[3]) throw () __attribute__ ((__nonnull__ (1)));


extern long int lrand48 (void) throw ();
extern long int nrand48 (unsigned short int __xsubi[3])
     throw () __attribute__ ((__nonnull__ (1)));


extern long int mrand48 (void) throw ();
extern long int jrand48 (unsigned short int __xsubi[3])
     throw () __attribute__ ((__nonnull__ (1)));


extern void srand48 (long int __seedval) throw ();
extern unsigned short int *seed48 (unsigned short int __seed16v[3])
     throw () __attribute__ ((__nonnull__ (1)));
extern void lcong48 (unsigned short int __param[7]) throw () __attribute__ ((__nonnull__ (1)));





struct drand48_data
  {
    unsigned short int __x[3];
    unsigned short int __old_x[3];
    unsigned short int __c;
    unsigned short int __init;
    __extension__ unsigned long long int __a;

  };


extern int drand48_r (struct drand48_data *__restrict __buffer,
        double *__restrict __result) throw () __attribute__ ((__nonnull__ (1, 2)));
extern int erand48_r (unsigned short int __xsubi[3],
        struct drand48_data *__restrict __buffer,
        double *__restrict __result) throw () __attribute__ ((__nonnull__ (1, 2)));


extern int lrand48_r (struct drand48_data *__restrict __buffer,
        long int *__restrict __result)
     throw () __attribute__ ((__nonnull__ (1, 2)));
extern int nrand48_r (unsigned short int __xsubi[3],
        struct drand48_data *__restrict __buffer,
        long int *__restrict __result)
     throw () __attribute__ ((__nonnull__ (1, 2)));


extern int mrand48_r (struct drand48_data *__restrict __buffer,
        long int *__restrict __result)
     throw () __attribute__ ((__nonnull__ (1, 2)));
extern int jrand48_r (unsigned short int __xsubi[3],
        struct drand48_data *__restrict __buffer,
        long int *__restrict __result)
     throw () __attribute__ ((__nonnull__ (1, 2)));


extern int srand48_r (long int __seedval, struct drand48_data *__buffer)
     throw () __attribute__ ((__nonnull__ (2)));

extern int seed48_r (unsigned short int __seed16v[3],
       struct drand48_data *__buffer) throw () __attribute__ ((__nonnull__ (1, 2)));

extern int lcong48_r (unsigned short int __param[7],
        struct drand48_data *__buffer)
     throw () __attribute__ ((__nonnull__ (1, 2)));




extern void *malloc (size_t __size) throw () __attribute__ ((__malloc__)) ;

extern void *calloc (size_t __nmemb, size_t __size)
     throw () __attribute__ ((__malloc__)) ;






extern void *realloc (void *__ptr, size_t __size)
     throw () __attribute__ ((__warn_unused_result__));







extern void *reallocarray (void *__ptr, size_t __nmemb, size_t __size)
     throw () __attribute__ ((__warn_unused_result__));



extern void free (void *__ptr) throw ();


# 1 "/usr/include/alloca.h" 1 3 4
# 24 "/usr/include/alloca.h" 3 4
# 1 "/rwthfs/rz/cluster/work/rwth1269/software/c23/classic-flang/lib/clang/16/include/stddef.h" 1 3 4
# 25 "/usr/include/alloca.h" 2 3 4

extern "C" {





extern void *alloca (size_t __size) throw ();





}
# 567 "/usr/include/stdlib.h" 2 3 4





extern void *valloc (size_t __size) throw () __attribute__ ((__malloc__)) ;




extern int posix_memalign (void **__memptr, size_t __alignment, size_t __size)
     throw () __attribute__ ((__nonnull__ (1))) ;




extern void *aligned_alloc (size_t __alignment, size_t __size)
     throw () __attribute__ ((__malloc__)) ;



extern void abort (void) throw () __attribute__ ((__noreturn__));



extern int atexit (void (*__func) (void)) throw () __attribute__ ((__nonnull__ (1)));




extern "C++" int at_quick_exit (void (*__func) (void))
     throw () __asm ("at_quick_exit") __attribute__ ((__nonnull__ (1)));
# 607 "/usr/include/stdlib.h" 3 4
extern int on_exit (void (*__func) (int __status, void *__arg), void *__arg)
     throw () __attribute__ ((__nonnull__ (1)));





extern void exit (int __status) throw () __attribute__ ((__noreturn__));





extern void quick_exit (int __status) throw () __attribute__ ((__noreturn__));





extern void _Exit (int __status) throw () __attribute__ ((__noreturn__));




extern char *getenv (const char *__name) throw () __attribute__ ((__nonnull__ (1))) ;




extern char *secure_getenv (const char *__name)
     throw () __attribute__ ((__nonnull__ (1))) ;






extern int putenv (char *__string) throw () __attribute__ ((__nonnull__ (1)));





extern int setenv (const char *__name, const char *__value, int __replace)
     throw () __attribute__ ((__nonnull__ (2)));


extern int unsetenv (const char *__name) throw () __attribute__ ((__nonnull__ (1)));






extern int clearenv (void) throw ();
# 672 "/usr/include/stdlib.h" 3 4
extern char *mktemp (char *__template) throw () __attribute__ ((__nonnull__ (1)));
# 685 "/usr/include/stdlib.h" 3 4
extern int mkstemp (char *__template) __attribute__ ((__nonnull__ (1))) ;
# 695 "/usr/include/stdlib.h" 3 4
extern int mkstemp64 (char *__template) __attribute__ ((__nonnull__ (1))) ;
# 707 "/usr/include/stdlib.h" 3 4
extern int mkstemps (char *__template, int __suffixlen) __attribute__ ((__nonnull__ (1))) ;
# 717 "/usr/include/stdlib.h" 3 4
extern int mkstemps64 (char *__template, int __suffixlen)
     __attribute__ ((__nonnull__ (1))) ;
# 728 "/usr/include/stdlib.h" 3 4
extern char *mkdtemp (char *__template) throw () __attribute__ ((__nonnull__ (1))) ;
# 739 "/usr/include/stdlib.h" 3 4
extern int mkostemp (char *__template, int __flags) __attribute__ ((__nonnull__ (1))) ;
# 749 "/usr/include/stdlib.h" 3 4
extern int mkostemp64 (char *__template, int __flags) __attribute__ ((__nonnull__ (1))) ;
# 759 "/usr/include/stdlib.h" 3 4
extern int mkostemps (char *__template, int __suffixlen, int __flags)
     __attribute__ ((__nonnull__ (1))) ;
# 771 "/usr/include/stdlib.h" 3 4
extern int mkostemps64 (char *__template, int __suffixlen, int __flags)
     __attribute__ ((__nonnull__ (1))) ;
# 781 "/usr/include/stdlib.h" 3 4
extern int system (const char *__command) ;





extern char *canonicalize_file_name (const char *__name)
     throw () __attribute__ ((__nonnull__ (1))) ;
# 797 "/usr/include/stdlib.h" 3 4
extern char *realpath (const char *__restrict __name,
         char *__restrict __resolved) throw () ;






typedef int (*__compar_fn_t) (const void *, const void *);


typedef __compar_fn_t comparison_fn_t;



typedef int (*__compar_d_fn_t) (const void *, const void *, void *);




extern void *bsearch (const void *__key, const void *__base,
        size_t __nmemb, size_t __size, __compar_fn_t __compar)
     __attribute__ ((__nonnull__ (1, 2, 5))) ;


# 1 "/usr/include/bits/stdlib-bsearch.h" 1 3 4
# 19 "/usr/include/bits/stdlib-bsearch.h" 3 4
extern __inline __attribute__ ((__gnu_inline__)) void *
bsearch (const void *__key, const void *__base, size_t __nmemb, size_t __size,
  __compar_fn_t __compar)
{
  size_t __l, __u, __idx;
  const void *__p;
  int __comparison;

  __l = 0;
  __u = __nmemb;
  while (__l < __u)
    {
      __idx = (__l + __u) / 2;
      __p = (void *) (((const char *) __base) + (__idx * __size));
      __comparison = (*__compar) (__key, __p);
      if (__comparison < 0)
 __u = __idx;
      else if (__comparison > 0)
 __l = __idx + 1;
      else
 return (void *) __p;
    }

  return __null;
}
# 823 "/usr/include/stdlib.h" 2 3 4




extern void qsort (void *__base, size_t __nmemb, size_t __size,
     __compar_fn_t __compar) __attribute__ ((__nonnull__ (1, 4)));

extern void qsort_r (void *__base, size_t __nmemb, size_t __size,
       __compar_d_fn_t __compar, void *__arg)
  __attribute__ ((__nonnull__ (1, 4)));




extern int abs (int __x) throw () __attribute__ ((__const__)) ;
extern long int labs (long int __x) throw () __attribute__ ((__const__)) ;


__extension__ extern long long int llabs (long long int __x)
     throw () __attribute__ ((__const__)) ;






extern div_t div (int __numer, int __denom)
     throw () __attribute__ ((__const__)) ;
extern ldiv_t ldiv (long int __numer, long int __denom)
     throw () __attribute__ ((__const__)) ;


__extension__ extern lldiv_t lldiv (long long int __numer,
        long long int __denom)
     throw () __attribute__ ((__const__)) ;
# 869 "/usr/include/stdlib.h" 3 4
extern char *ecvt (double __value, int __ndigit, int *__restrict __decpt,
     int *__restrict __sign) throw () __attribute__ ((__nonnull__ (3, 4))) ;




extern char *fcvt (double __value, int __ndigit, int *__restrict __decpt,
     int *__restrict __sign) throw () __attribute__ ((__nonnull__ (3, 4))) ;




extern char *gcvt (double __value, int __ndigit, char *__buf)
     throw () __attribute__ ((__nonnull__ (3))) ;




extern char *qecvt (long double __value, int __ndigit,
      int *__restrict __decpt, int *__restrict __sign)
     throw () __attribute__ ((__nonnull__ (3, 4))) ;
extern char *qfcvt (long double __value, int __ndigit,
      int *__restrict __decpt, int *__restrict __sign)
     throw () __attribute__ ((__nonnull__ (3, 4))) ;
extern char *qgcvt (long double __value, int __ndigit, char *__buf)
     throw () __attribute__ ((__nonnull__ (3))) ;




extern int ecvt_r (double __value, int __ndigit, int *__restrict __decpt,
     int *__restrict __sign, char *__restrict __buf,
     size_t __len) throw () __attribute__ ((__nonnull__ (3, 4, 5)));
extern int fcvt_r (double __value, int __ndigit, int *__restrict __decpt,
     int *__restrict __sign, char *__restrict __buf,
     size_t __len) throw () __attribute__ ((__nonnull__ (3, 4, 5)));

extern int qecvt_r (long double __value, int __ndigit,
      int *__restrict __decpt, int *__restrict __sign,
      char *__restrict __buf, size_t __len)
     throw () __attribute__ ((__nonnull__ (3, 4, 5)));
extern int qfcvt_r (long double __value, int __ndigit,
      int *__restrict __decpt, int *__restrict __sign,
      char *__restrict __buf, size_t __len)
     throw () __attribute__ ((__nonnull__ (3, 4, 5)));





extern int mblen (const char *__s, size_t __n) throw ();


extern int mbtowc (wchar_t *__restrict __pwc,
     const char *__restrict __s, size_t __n) throw ();


extern int wctomb (char *__s, wchar_t __wchar) throw ();



extern size_t mbstowcs (wchar_t *__restrict __pwcs,
   const char *__restrict __s, size_t __n) throw ();

extern size_t wcstombs (char *__restrict __s,
   const wchar_t *__restrict __pwcs, size_t __n)
     throw ();







extern int rpmatch (const char *__response) throw () __attribute__ ((__nonnull__ (1))) ;
# 954 "/usr/include/stdlib.h" 3 4
extern int getsubopt (char **__restrict __optionp,
        char *const *__restrict __tokens,
        char **__restrict __valuep)
     throw () __attribute__ ((__nonnull__ (1, 2, 3))) ;







extern int posix_openpt (int __oflag) ;







extern int grantpt (int __fd) throw ();



extern int unlockpt (int __fd) throw ();




extern char *ptsname (int __fd) throw () ;






extern int ptsname_r (int __fd, char *__buf, size_t __buflen)
     throw () __attribute__ ((__nonnull__ (2)));


extern int getpt (void);






extern int getloadavg (double __loadavg[], int __nelem)
     throw () __attribute__ ((__nonnull__ (1)));
# 1010 "/usr/include/stdlib.h" 3 4
# 1 "/usr/include/bits/stdlib-float.h" 1 3 4
# 24 "/usr/include/bits/stdlib-float.h" 3 4
extern __inline __attribute__ ((__gnu_inline__)) double
 atof (const char *__nptr) throw ()
{
  return strtod (__nptr, (char **) __null);
}
# 1011 "/usr/include/stdlib.h" 2 3 4
# 1020 "/usr/include/stdlib.h" 3 4
}
# 76 "/cvmfs/software.hpc.rwth.de/Linux/RH8/x86_64/intel/sapphirerapids/software/GCCcore/12.3.0/lib/gcc/x86_64-pc-linux-gnu/12.3.0/../../../../include/c++/12.3.0/cstdlib" 2 3

# 1 "/cvmfs/software.hpc.rwth.de/Linux/RH8/x86_64/intel/sapphirerapids/software/GCCcore/12.3.0/lib/gcc/x86_64-pc-linux-gnu/12.3.0/../../../../include/c++/12.3.0/bits/std_abs.h" 1 3
# 34 "/cvmfs/software.hpc.rwth.de/Linux/RH8/x86_64/intel/sapphirerapids/software/GCCcore/12.3.0/lib/gcc/x86_64-pc-linux-gnu/12.3.0/../../../../include/c++/12.3.0/bits/std_abs.h" 3
# 46 "/cvmfs/software.hpc.rwth.de/Linux/RH8/x86_64/intel/sapphirerapids/software/GCCcore/12.3.0/lib/gcc/x86_64-pc-linux-gnu/12.3.0/../../../../include/c++/12.3.0/bits/std_abs.h" 3
extern "C++"
{
namespace std __attribute__ ((__visibility__ ("default")))
{


  using ::abs;


  inline long
  abs(long __i) { return __builtin_labs(__i); }



  inline long long
  abs(long long __x) { return __builtin_llabs (__x); }
# 70 "/cvmfs/software.hpc.rwth.de/Linux/RH8/x86_64/intel/sapphirerapids/software/GCCcore/12.3.0/lib/gcc/x86_64-pc-linux-gnu/12.3.0/../../../../include/c++/12.3.0/bits/std_abs.h" 3
  inline constexpr double
  abs(double __x)
  { return __builtin_fabs(__x); }

  inline constexpr float
  abs(float __x)
  { return __builtin_fabsf(__x); }

  inline constexpr long double
  abs(long double __x)
  { return __builtin_fabsl(__x); }



  __extension__ inline constexpr __int128
  abs(__int128 __x) { return __x >= 0 ? __x : -__x; }
# 101 "/cvmfs/software.hpc.rwth.de/Linux/RH8/x86_64/intel/sapphirerapids/software/GCCcore/12.3.0/lib/gcc/x86_64-pc-linux-gnu/12.3.0/../../../../include/c++/12.3.0/bits/std_abs.h" 3
  __extension__ inline constexpr
  __float128
  abs(__float128 __x)
  { return __x < 0 ? -__x : __x; }



}
}
# 78 "/cvmfs/software.hpc.rwth.de/Linux/RH8/x86_64/intel/sapphirerapids/software/GCCcore/12.3.0/lib/gcc/x86_64-pc-linux-gnu/12.3.0/../../../../include/c++/12.3.0/cstdlib" 2 3
# 121 "/cvmfs/software.hpc.rwth.de/Linux/RH8/x86_64/intel/sapphirerapids/software/GCCcore/12.3.0/lib/gcc/x86_64-pc-linux-gnu/12.3.0/../../../../include/c++/12.3.0/cstdlib" 3
extern "C++"
{
namespace std __attribute__ ((__visibility__ ("default")))
{


  using ::div_t;
  using ::ldiv_t;

  using ::abort;

  using ::aligned_alloc;

  using ::atexit;


  using ::at_quick_exit;


  using ::atof;
  using ::atoi;
  using ::atol;
  using ::bsearch;
  using ::calloc;
  using ::div;
  using ::exit;
  using ::free;
  using ::getenv;
  using ::labs;
  using ::ldiv;
  using ::malloc;

  using ::mblen;
  using ::mbstowcs;
  using ::mbtowc;

  using ::qsort;


  using ::quick_exit;


  using ::rand;
  using ::realloc;
  using ::srand;
  using ::strtod;
  using ::strtol;
  using ::strtoul;
  using ::system;

  using ::wcstombs;
  using ::wctomb;



  inline ldiv_t
  div(long __i, long __j) { return ldiv(__i, __j); }




}
# 195 "/cvmfs/software.hpc.rwth.de/Linux/RH8/x86_64/intel/sapphirerapids/software/GCCcore/12.3.0/lib/gcc/x86_64-pc-linux-gnu/12.3.0/../../../../include/c++/12.3.0/cstdlib" 3
namespace __gnu_cxx __attribute__ ((__visibility__ ("default")))
{



  using ::lldiv_t;





  using ::_Exit;



  using ::llabs;

  inline lldiv_t
  div(long long __n, long long __d)
  { lldiv_t __q; __q.quot = __n / __d; __q.rem = __n % __d; return __q; }

  using ::lldiv;
# 227 "/cvmfs/software.hpc.rwth.de/Linux/RH8/x86_64/intel/sapphirerapids/software/GCCcore/12.3.0/lib/gcc/x86_64-pc-linux-gnu/12.3.0/../../../../include/c++/12.3.0/cstdlib" 3
  using ::atoll;
  using ::strtoll;
  using ::strtoull;

  using ::strtof;
  using ::strtold;


}

namespace std
{

  using ::__gnu_cxx::lldiv_t;

  using ::__gnu_cxx::_Exit;

  using ::__gnu_cxx::llabs;
  using ::__gnu_cxx::div;
  using ::__gnu_cxx::lldiv;

  using ::__gnu_cxx::atoll;
  using ::__gnu_cxx::strtof;
  using ::__gnu_cxx::strtoll;
  using ::__gnu_cxx::strtoull;
  using ::__gnu_cxx::strtold;
}



}
# 37 "/cvmfs/software.hpc.rwth.de/Linux/RH8/x86_64/intel/sapphirerapids/software/GCCcore/12.3.0/lib/gcc/x86_64-pc-linux-gnu/12.3.0/../../../../include/c++/12.3.0/stdlib.h" 2 3

using std::abort;
using std::atexit;
using std::exit;


  using std::at_quick_exit;


  using std::quick_exit;




using std::div_t;
using std::ldiv_t;

using std::abs;
using std::atof;
using std::atoi;
using std::atol;
using std::bsearch;
using std::calloc;
using std::div;
using std::free;
using std::getenv;
using std::labs;
using std::ldiv;
using std::malloc;

using std::mblen;
using std::mbstowcs;
using std::mbtowc;

using std::qsort;
using std::rand;
using std::realloc;
using std::srand;
using std::strtod;
using std::strtol;
using std::strtoul;
using std::system;

using std::wcstombs;
using std::wctomb;
# 34 "comm.cpp" 2
# 1 "/cvmfs/software.hpc.rwth.de/Linux/RH8/x86_64/intel/sapphirerapids/software/impi/2021.9.0-intel-compilers-2023.1.0/mpi/2021.9.0/include/mpi.h" 1
# 87 "/cvmfs/software.hpc.rwth.de/Linux/RH8/x86_64/intel/sapphirerapids/software/impi/2021.9.0-intel-compilers-2023.1.0/mpi/2021.9.0/include/mpi.h"
extern "C" {
# 117 "/cvmfs/software.hpc.rwth.de/Linux/RH8/x86_64/intel/sapphirerapids/software/impi/2021.9.0-intel-compilers-2023.1.0/mpi/2021.9.0/include/mpi.h"
# 1 "/cvmfs/software.hpc.rwth.de/Linux/RH8/x86_64/intel/sapphirerapids/software/GCCcore/12.3.0/lib/gcc/x86_64-pc-linux-gnu/12.3.0/../../../../include/c++/12.3.0/cstdint" 1 3
# 33 "/cvmfs/software.hpc.rwth.de/Linux/RH8/x86_64/intel/sapphirerapids/software/GCCcore/12.3.0/lib/gcc/x86_64-pc-linux-gnu/12.3.0/../../../../include/c++/12.3.0/cstdint" 3








# 1 "/rwthfs/rz/cluster/work/rwth1269/software/c23/classic-flang/lib/clang/16/include/stdint.h" 1 3
# 52 "/rwthfs/rz/cluster/work/rwth1269/software/c23/classic-flang/lib/clang/16/include/stdint.h" 3
# 1 "/usr/include/stdint.h" 1 3 4
# 26 "/usr/include/stdint.h" 3 4
# 1 "/usr/include/bits/libc-header-start.h" 1 3 4
# 27 "/usr/include/stdint.h" 2 3 4

# 1 "/usr/include/bits/wchar.h" 1 3 4
# 29 "/usr/include/stdint.h" 2 3 4
# 1 "/usr/include/bits/wordsize.h" 1 3 4
# 30 "/usr/include/stdint.h" 2 3 4







# 1 "/usr/include/bits/stdint-uintn.h" 1 3 4
# 24 "/usr/include/bits/stdint-uintn.h" 3 4
typedef __uint8_t uint8_t;
typedef __uint16_t uint16_t;
typedef __uint32_t uint32_t;
typedef __uint64_t uint64_t;
# 38 "/usr/include/stdint.h" 2 3 4





typedef __int_least8_t int_least8_t;
typedef __int_least16_t int_least16_t;
typedef __int_least32_t int_least32_t;
typedef __int_least64_t int_least64_t;


typedef __uint_least8_t uint_least8_t;
typedef __uint_least16_t uint_least16_t;
typedef __uint_least32_t uint_least32_t;
typedef __uint_least64_t uint_least64_t;





typedef signed char int_fast8_t;

typedef long int int_fast16_t;
typedef long int int_fast32_t;
typedef long int int_fast64_t;
# 71 "/usr/include/stdint.h" 3 4
typedef unsigned char uint_fast8_t;

typedef unsigned long int uint_fast16_t;
typedef unsigned long int uint_fast32_t;
typedef unsigned long int uint_fast64_t;
# 87 "/usr/include/stdint.h" 3 4
typedef long int intptr_t;


typedef unsigned long int uintptr_t;
# 101 "/usr/include/stdint.h" 3 4
typedef __intmax_t intmax_t;
typedef __uintmax_t uintmax_t;
# 53 "/rwthfs/rz/cluster/work/rwth1269/software/c23/classic-flang/lib/clang/16/include/stdint.h" 2 3
# 42 "/cvmfs/software.hpc.rwth.de/Linux/RH8/x86_64/intel/sapphirerapids/software/GCCcore/12.3.0/lib/gcc/x86_64-pc-linux-gnu/12.3.0/../../../../include/c++/12.3.0/cstdint" 2 3


namespace std
{

  using ::int8_t;
  using ::int16_t;
  using ::int32_t;
  using ::int64_t;

  using ::int_fast8_t;
  using ::int_fast16_t;
  using ::int_fast32_t;
  using ::int_fast64_t;

  using ::int_least8_t;
  using ::int_least16_t;
  using ::int_least32_t;
  using ::int_least64_t;

  using ::intmax_t;
  using ::intptr_t;

  using ::uint8_t;
  using ::uint16_t;
  using ::uint32_t;
  using ::uint64_t;

  using ::uint_fast8_t;
  using ::uint_fast16_t;
  using ::uint_fast32_t;
  using ::uint_fast64_t;

  using ::uint_least8_t;
  using ::uint_least16_t;
  using ::uint_least32_t;
  using ::uint_least64_t;

  using ::uintmax_t;
  using ::uintptr_t;





}
# 118 "/cvmfs/software.hpc.rwth.de/Linux/RH8/x86_64/intel/sapphirerapids/software/impi/2021.9.0-intel-compilers-2023.1.0/mpi/2021.9.0/include/mpi.h" 2
# 171 "/cvmfs/software.hpc.rwth.de/Linux/RH8/x86_64/intel/sapphirerapids/software/impi/2021.9.0-intel-compilers-2023.1.0/mpi/2021.9.0/include/mpi.h"
typedef int MPI_Datatype;
# 345 "/cvmfs/software.hpc.rwth.de/Linux/RH8/x86_64/intel/sapphirerapids/software/impi/2021.9.0-intel-compilers-2023.1.0/mpi/2021.9.0/include/mpi.h"
typedef int MPI_Comm;




typedef int MPI_Group;



typedef int MPI_Win;







typedef struct ADIOI_FileD *MPI_File;



typedef int MPI_Op;
# 428 "/cvmfs/software.hpc.rwth.de/Linux/RH8/x86_64/intel/sapphirerapids/software/impi/2021.9.0-intel-compilers-2023.1.0/mpi/2021.9.0/include/mpi.h"
typedef enum MPIR_Win_flavor {
    MPI_WIN_FLAVOR_CREATE = 1,
    MPI_WIN_FLAVOR_ALLOCATE = 2,
    MPI_WIN_FLAVOR_DYNAMIC = 3,
    MPI_WIN_FLAVOR_SHARED = 4
} MPIR_Win_flavor_t;


typedef enum MPIR_Win_model {
    MPI_WIN_SEPARATE = 1,
    MPI_WIN_UNIFIED = 2
} MPIR_Win_model_t;





typedef enum MPIR_Topo_type { MPI_GRAPH=1, MPI_CART=2, MPI_DIST_GRAPH=3 } MPIR_Topo_type;



extern int * const MPI_UNWEIGHTED ;
extern int * const MPI_WEIGHTS_EMPTY ;
# 461 "/cvmfs/software.hpc.rwth.de/Linux/RH8/x86_64/intel/sapphirerapids/software/impi/2021.9.0-intel-compilers-2023.1.0/mpi/2021.9.0/include/mpi.h"
typedef void (MPI_Handler_function) ( MPI_Comm *, int *, ... );
typedef int (MPI_Comm_copy_attr_function)(MPI_Comm, int, void *, void *,
       void *, int *);
typedef int (MPI_Comm_delete_attr_function)(MPI_Comm, int, void *, void *);
typedef int (MPI_Type_copy_attr_function)(MPI_Datatype, int, void *, void *,
       void *, int *);
typedef int (MPI_Type_delete_attr_function)(MPI_Datatype, int, void *, void *);
typedef int (MPI_Win_copy_attr_function)(MPI_Win, int, void *, void *, void *,
      int *);
typedef int (MPI_Win_delete_attr_function)(MPI_Win, int, void *, void *);

typedef void (MPI_Comm_errhandler_function)(MPI_Comm *, int *, ...);
typedef void (MPI_File_errhandler_function)(MPI_File *, int *, ...);
typedef void (MPI_Win_errhandler_function)(MPI_Win *, int *, ...);

typedef MPI_Comm_errhandler_function MPI_Comm_errhandler_fn;
typedef MPI_File_errhandler_function MPI_File_errhandler_fn;
typedef MPI_Win_errhandler_function MPI_Win_errhandler_fn;
# 489 "/cvmfs/software.hpc.rwth.de/Linux/RH8/x86_64/intel/sapphirerapids/software/impi/2021.9.0-intel-compilers-2023.1.0/mpi/2021.9.0/include/mpi.h"
typedef int MPI_Errhandler;
# 510 "/cvmfs/software.hpc.rwth.de/Linux/RH8/x86_64/intel/sapphirerapids/software/impi/2021.9.0-intel-compilers-2023.1.0/mpi/2021.9.0/include/mpi.h"
typedef int MPI_Request;


typedef int MPI_Message;


typedef void (MPI_User_function) ( void *, void *, int *, MPI_Datatype * );


typedef int (MPI_Copy_function) ( MPI_Comm, int, void *, void *, void *, int * );
typedef int (MPI_Delete_function) ( MPI_Comm, int, void *, void * );
# 591 "/cvmfs/software.hpc.rwth.de/Linux/RH8/x86_64/intel/sapphirerapids/software/impi/2021.9.0-intel-compilers-2023.1.0/mpi/2021.9.0/include/mpi.h"
enum MPIR_Combiner_enum {
    MPI_COMBINER_NAMED = 1,
    MPI_COMBINER_DUP = 2,
    MPI_COMBINER_CONTIGUOUS = 3,
    MPI_COMBINER_VECTOR = 4,
    MPI_COMBINER_HVECTOR_INTEGER = 5,
    MPI_COMBINER_HVECTOR = 6,
    MPI_COMBINER_INDEXED = 7,
    MPI_COMBINER_HINDEXED_INTEGER = 8,
    MPI_COMBINER_HINDEXED = 9,
    MPI_COMBINER_INDEXED_BLOCK = 10,
    MPI_COMBINER_STRUCT_INTEGER = 11,
    MPI_COMBINER_STRUCT = 12,
    MPI_COMBINER_SUBARRAY = 13,
    MPI_COMBINER_DARRAY = 14,
    MPI_COMBINER_F90_REAL = 15,
    MPI_COMBINER_F90_COMPLEX = 16,
    MPI_COMBINER_F90_INTEGER = 17,
    MPI_COMBINER_RESIZED = 18,
    MPI_COMBINER_HINDEXED_BLOCK = 19
};


typedef int MPI_Info;
# 644 "/cvmfs/software.hpc.rwth.de/Linux/RH8/x86_64/intel/sapphirerapids/software/impi/2021.9.0-intel-compilers-2023.1.0/mpi/2021.9.0/include/mpi.h"
typedef long MPI_Aint;
typedef int MPI_Fint;
typedef long long MPI_Count;
# 663 "/cvmfs/software.hpc.rwth.de/Linux/RH8/x86_64/intel/sapphirerapids/software/impi/2021.9.0-intel-compilers-2023.1.0/mpi/2021.9.0/include/mpi.h"
typedef long long MPI_Offset;







typedef struct MPI_Status {
    int count_lo;
    int count_hi_and_cancelled;
    int MPI_SOURCE;
    int MPI_TAG;
    int MPI_ERROR;
} MPI_Status;


struct MPIR_T_enum_s;
struct MPIR_T_cvar_handle_s;
struct MPIR_T_pvar_handle_s;
struct MPIR_T_pvar_session_s;

typedef struct MPIR_T_enum_s * MPI_T_enum;
typedef struct MPIR_T_cvar_handle_s * MPI_T_cvar_handle;
typedef struct MPIR_T_pvar_handle_s * MPI_T_pvar_handle;
typedef struct MPIR_T_pvar_session_s * MPI_T_pvar_session;


extern struct MPIR_T_pvar_handle_s * const MPI_T_PVAR_ALL_HANDLES ;
# 700 "/cvmfs/software.hpc.rwth.de/Linux/RH8/x86_64/intel/sapphirerapids/software/impi/2021.9.0-intel-compilers-2023.1.0/mpi/2021.9.0/include/mpi.h"
typedef enum MPIR_T_verbosity_t {


    MPIX_T_VERBOSITY_INVALID = 0,


    MPI_T_VERBOSITY_USER_BASIC = 221,
    MPI_T_VERBOSITY_USER_DETAIL,
    MPI_T_VERBOSITY_USER_ALL,

    MPI_T_VERBOSITY_TUNER_BASIC,
    MPI_T_VERBOSITY_TUNER_DETAIL,
    MPI_T_VERBOSITY_TUNER_ALL,

    MPI_T_VERBOSITY_MPIDEV_BASIC,
    MPI_T_VERBOSITY_MPIDEV_DETAIL,
    MPI_T_VERBOSITY_MPIDEV_ALL
} MPIR_T_verbosity_t;

typedef enum MPIR_T_bind_t {


    MPIX_T_BIND_INVALID = 0,


    MPI_T_BIND_NO_OBJECT = 9700,
    MPI_T_BIND_MPI_COMM,
    MPI_T_BIND_MPI_DATATYPE,
    MPI_T_BIND_MPI_ERRHANDLER,
    MPI_T_BIND_MPI_FILE,
    MPI_T_BIND_MPI_GROUP,
    MPI_T_BIND_MPI_OP,
    MPI_T_BIND_MPI_REQUEST,
    MPI_T_BIND_MPI_WIN,
    MPI_T_BIND_MPI_MESSAGE,
    MPI_T_BIND_MPI_INFO
} MPIR_T_bind_t;

typedef enum MPIR_T_scope_t {


    MPIX_T_SCOPE_INVALID = 0,


    MPI_T_SCOPE_CONSTANT = 60438,
    MPI_T_SCOPE_READONLY,
    MPI_T_SCOPE_LOCAL,
    MPI_T_SCOPE_GROUP,
    MPI_T_SCOPE_GROUP_EQ,
    MPI_T_SCOPE_ALL,
    MPI_T_SCOPE_ALL_EQ
} MPIR_T_scope_t;

typedef enum MPIR_T_pvar_class_t {


    MPIX_T_PVAR_CLASS_INVALID = 0,


    MPIR_T_PVAR_CLASS_FIRST = 240,
    MPI_T_PVAR_CLASS_STATE = MPIR_T_PVAR_CLASS_FIRST,
    MPI_T_PVAR_CLASS_LEVEL,
    MPI_T_PVAR_CLASS_SIZE,
    MPI_T_PVAR_CLASS_PERCENTAGE,
    MPI_T_PVAR_CLASS_HIGHWATERMARK,
    MPI_T_PVAR_CLASS_LOWWATERMARK,
    MPI_T_PVAR_CLASS_COUNTER,
    MPI_T_PVAR_CLASS_AGGREGATE,
    MPI_T_PVAR_CLASS_TIMER,
    MPI_T_PVAR_CLASS_GENERIC,
    MPIR_T_PVAR_CLASS_LAST,
    MPIR_T_PVAR_CLASS_NUMBER = MPIR_T_PVAR_CLASS_LAST - MPIR_T_PVAR_CLASS_FIRST
} MPIR_T_pvar_class_t;
# 821 "/cvmfs/software.hpc.rwth.de/Linux/RH8/x86_64/intel/sapphirerapids/software/impi/2021.9.0-intel-compilers-2023.1.0/mpi/2021.9.0/include/mpi.h"
extern MPI_Fint * MPI_F_STATUS_IGNORE ;
extern MPI_Fint * MPI_F_STATUSES_IGNORE ;
# 836 "/cvmfs/software.hpc.rwth.de/Linux/RH8/x86_64/intel/sapphirerapids/software/impi/2021.9.0-intel-compilers-2023.1.0/mpi/2021.9.0/include/mpi.h"
typedef struct {
    MPI_Fint count_lo;
    MPI_Fint count_hi_and_cancelled;
    MPI_Fint MPI_SOURCE;
    MPI_Fint MPI_TAG;
    MPI_Fint MPI_ERROR;
} MPI_F08_status;

extern MPI_F08_status MPIR_F08_MPI_STATUS_IGNORE_OBJ ;
extern MPI_F08_status MPIR_F08_MPI_STATUSES_IGNORE_OBJ[1] ;
extern int MPIR_F08_MPI_IN_PLACE ;
extern int MPIR_F08_MPI_BOTTOM ;


extern MPI_F08_status *MPI_F08_STATUS_IGNORE ;
extern MPI_F08_status *MPI_F08_STATUSES_IGNORE ;
# 860 "/cvmfs/software.hpc.rwth.de/Linux/RH8/x86_64/intel/sapphirerapids/software/impi/2021.9.0-intel-compilers-2023.1.0/mpi/2021.9.0/include/mpi.h"
typedef int (MPI_Grequest_cancel_function)(void *, int);
typedef int (MPI_Grequest_free_function)(void *);
typedef int (MPI_Grequest_query_function)(void *, MPI_Status *);
typedef int (MPIX_Grequest_poll_function)(void *, MPI_Status *);
typedef int (MPIX_Grequest_wait_function)(int, void **, double, MPI_Status *);
# 991 "/cvmfs/software.hpc.rwth.de/Linux/RH8/x86_64/intel/sapphirerapids/software/impi/2021.9.0-intel-compilers-2023.1.0/mpi/2021.9.0/include/mpi.h"
typedef int (MPI_Datarep_conversion_function)(void *, MPI_Datatype, int,
             void *, MPI_Offset, void *);
typedef int (MPI_Datarep_extent_function)(MPI_Datatype datatype, MPI_Aint *,
                      void *);
# 1011 "/cvmfs/software.hpc.rwth.de/Linux/RH8/x86_64/intel/sapphirerapids/software/impi/2021.9.0-intel-compilers-2023.1.0/mpi/2021.9.0/include/mpi.h"
int MPI_Send(const void *buf, int count, MPI_Datatype datatype, int dest, int tag,
             MPI_Comm comm) ;
int MPI_Recv(void *buf, int count, MPI_Datatype datatype, int source, int tag,
             MPI_Comm comm, MPI_Status *status) ;
int MPI_Get_count(const MPI_Status *status, MPI_Datatype datatype, int *count) ;
int MPI_Bsend(const void *buf, int count, MPI_Datatype datatype, int dest, int tag,
              MPI_Comm comm) ;
int MPI_Ssend(const void *buf, int count, MPI_Datatype datatype, int dest, int tag,
              MPI_Comm comm) ;
int MPI_Rsend(const void *buf, int count, MPI_Datatype datatype, int dest, int tag,
              MPI_Comm comm) ;
int MPI_Buffer_attach(void *buffer, int size) ;
int MPI_Buffer_detach(void *buffer_addr, int *size) ;
int MPI_Isend(const void *buf, int count, MPI_Datatype datatype, int dest, int tag,
              MPI_Comm comm, MPI_Request *request) ;
int MPI_Ibsend(const void *buf, int count, MPI_Datatype datatype, int dest, int tag,
               MPI_Comm comm, MPI_Request *request) ;
int MPI_Issend(const void *buf, int count, MPI_Datatype datatype, int dest, int tag,
               MPI_Comm comm, MPI_Request *request) ;
int MPI_Irsend(const void *buf, int count, MPI_Datatype datatype, int dest, int tag,
               MPI_Comm comm, MPI_Request *request) ;
int MPI_Irecv(void *buf, int count, MPI_Datatype datatype, int source, int tag,
              MPI_Comm comm, MPI_Request *request) ;
int MPI_Wait(MPI_Request *request, MPI_Status *status) ;
int MPI_Test(MPI_Request *request, int *flag, MPI_Status *status) ;
int MPI_Request_free(MPI_Request *request) ;
int MPI_Waitany(int count, MPI_Request array_of_requests[], int *indx, MPI_Status *status) ;
int MPI_Testany(int count, MPI_Request array_of_requests[], int *indx, int *flag,
                MPI_Status *status) ;
int MPI_Waitall(int count, MPI_Request array_of_requests[], MPI_Status array_of_statuses[]) ;
int MPI_Testall(int count, MPI_Request array_of_requests[], int *flag,
                MPI_Status array_of_statuses[]) ;
int MPI_Waitsome(int incount, MPI_Request array_of_requests[], int *outcount,
                 int array_of_indices[], MPI_Status array_of_statuses[]) ;
int MPI_Testsome(int incount, MPI_Request array_of_requests[], int *outcount,
                 int array_of_indices[], MPI_Status array_of_statuses[]) ;
int MPI_Iprobe(int source, int tag, MPI_Comm comm, int *flag, MPI_Status *status) ;
int MPI_Probe(int source, int tag, MPI_Comm comm, MPI_Status *status) ;
int MPI_Cancel(MPI_Request *request) ;
int MPI_Test_cancelled(const MPI_Status *status, int *flag) ;
int MPI_Send_init(const void *buf, int count, MPI_Datatype datatype, int dest, int tag,
                  MPI_Comm comm, MPI_Request *request) ;
int MPI_Bsend_init(const void *buf, int count, MPI_Datatype datatype, int dest, int tag,
                   MPI_Comm comm, MPI_Request *request) ;
int MPI_Ssend_init(const void *buf, int count, MPI_Datatype datatype, int dest, int tag,
                   MPI_Comm comm, MPI_Request *request) ;
int MPI_Rsend_init(const void *buf, int count, MPI_Datatype datatype, int dest, int tag,
                   MPI_Comm comm, MPI_Request *request) ;
int MPI_Recv_init(void *buf, int count, MPI_Datatype datatype, int source, int tag,
                  MPI_Comm comm, MPI_Request *request) ;
int MPI_Start(MPI_Request *request) ;
int MPI_Startall(int count, MPI_Request array_of_requests[]) ;
int MPI_Sendrecv(const void *sendbuf, int sendcount, MPI_Datatype sendtype, int dest,
                 int sendtag, void *recvbuf, int recvcount, MPI_Datatype recvtype,
                 int source, int recvtag, MPI_Comm comm, MPI_Status *status)
                                                                                                             ;
int MPI_Sendrecv_replace(void *buf, int count, MPI_Datatype datatype, int dest,
                         int sendtag, int source, int recvtag, MPI_Comm comm,
                         MPI_Status *status) ;
int MPI_Type_contiguous(int count, MPI_Datatype oldtype, MPI_Datatype *newtype) ;
int MPI_Type_vector(int count, int blocklength, int stride, MPI_Datatype oldtype,
                    MPI_Datatype *newtype) ;
int MPI_Type_hvector(int count, int blocklength, MPI_Aint stride, MPI_Datatype oldtype,
                     MPI_Datatype *newtype) ;
int MPI_Type_indexed(int count, const int *array_of_blocklengths,
                     const int *array_of_displacements, MPI_Datatype oldtype,
                     MPI_Datatype *newtype) ;
int MPI_Type_hindexed(int count, int *array_of_blocklengths,
                      MPI_Aint *array_of_displacements, MPI_Datatype oldtype,
                      MPI_Datatype *newtype) ;
int MPI_Type_struct(int count, int *array_of_blocklengths,
                    MPI_Aint *array_of_displacements,
                    MPI_Datatype *array_of_types, MPI_Datatype *newtype) ;
int MPI_Address(void *location, MPI_Aint *address) ;
int MPI_Type_extent(MPI_Datatype datatype, MPI_Aint *extent) ;
int MPI_Type_size(MPI_Datatype datatype, int *size) ;
int MPI_Type_lb(MPI_Datatype datatype, MPI_Aint *displacement) ;
int MPI_Type_ub(MPI_Datatype datatype, MPI_Aint *displacement) ;
int MPI_Type_commit(MPI_Datatype *datatype) ;
int MPI_Type_free(MPI_Datatype *datatype) ;
int MPI_Get_elements(const MPI_Status *status, MPI_Datatype datatype, int *count) ;
int MPI_Pack(const void *inbuf, int incount, MPI_Datatype datatype, void *outbuf,
             int outsize, int *position, MPI_Comm comm) ;
int MPI_Unpack(const void *inbuf, int insize, int *position, void *outbuf, int outcount,
               MPI_Datatype datatype, MPI_Comm comm) ;
int MPI_Pack_size(int incount, MPI_Datatype datatype, MPI_Comm comm, int *size) ;
int MPI_Barrier(MPI_Comm comm) ;
int MPI_Bcast(void *buffer, int count, MPI_Datatype datatype, int root, MPI_Comm comm)
                                                                    ;
int MPI_Gather(const void *sendbuf, int sendcount, MPI_Datatype sendtype, void *recvbuf,
               int recvcount, MPI_Datatype recvtype, int root, MPI_Comm comm)
                                                                                                           ;
int MPI_Gatherv(const void *sendbuf, int sendcount, MPI_Datatype sendtype, void *recvbuf,
                const int *recvcounts, const int *displs, MPI_Datatype recvtype, int root,
                MPI_Comm comm)
                                                                                                            ;
int MPI_Scatter(const void *sendbuf, int sendcount, MPI_Datatype sendtype, void *recvbuf,
                int recvcount, MPI_Datatype recvtype, int root, MPI_Comm comm)
                                                                                                            ;
int MPI_Scatterv(const void *sendbuf, const int *sendcounts, const int *displs,
                 MPI_Datatype sendtype, void *recvbuf, int recvcount, MPI_Datatype recvtype,
                 int root, MPI_Comm comm)
                                                                                                             ;
int MPI_Allgather(const void *sendbuf, int sendcount, MPI_Datatype sendtype, void *recvbuf,
                  int recvcount, MPI_Datatype recvtype, MPI_Comm comm)
                                                                                                              ;
int MPI_Allgatherv(const void *sendbuf, int sendcount, MPI_Datatype sendtype, void *recvbuf,
                   const int *recvcounts, const int *displs, MPI_Datatype recvtype, MPI_Comm comm)
                                                                                                               ;
int MPI_Alltoall(const void *sendbuf, int sendcount, MPI_Datatype sendtype, void *recvbuf,
                 int recvcount, MPI_Datatype recvtype, MPI_Comm comm)
                                                                                                             ;
int MPI_Alltoallv(const void *sendbuf, const int *sendcounts, const int *sdispls,
                  MPI_Datatype sendtype, void *recvbuf, const int *recvcounts,
                  const int *rdispls, MPI_Datatype recvtype, MPI_Comm comm)
                                                                                                              ;
int MPI_Alltoallw(const void *sendbuf, const int sendcounts[], const int sdispls[],
                  const MPI_Datatype sendtypes[], void *recvbuf, const int recvcounts[],
                  const int rdispls[], const MPI_Datatype recvtypes[], MPI_Comm comm) ;
int MPI_Exscan(const void *sendbuf, void *recvbuf, int count, MPI_Datatype datatype,
               MPI_Op op, MPI_Comm comm)
                                                                                                           ;
int MPI_Reduce(const void *sendbuf, void *recvbuf, int count, MPI_Datatype datatype,
               MPI_Op op, int root, MPI_Comm comm)
                                                                                                           ;
int MPI_Op_create(MPI_User_function *user_fn, int commute, MPI_Op *op) ;
int MPI_Op_free(MPI_Op *op) ;
int MPI_Allreduce(const void *sendbuf, void *recvbuf, int count, MPI_Datatype datatype,
                  MPI_Op op, MPI_Comm comm)
                                                                                                              ;
int MPI_Reduce_scatter(const void *sendbuf, void *recvbuf, const int recvcounts[],
                       MPI_Datatype datatype, MPI_Op op, MPI_Comm comm)
                                                                                                                   ;
int MPI_Scan(const void *sendbuf, void *recvbuf, int count, MPI_Datatype datatype, MPI_Op op,
             MPI_Comm comm)
                                                                                                         ;
int MPI_Group_size(MPI_Group group, int *size) ;
int MPI_Group_rank(MPI_Group group, int *rank) ;
int MPI_Group_translate_ranks(MPI_Group group1, int n, const int ranks1[], MPI_Group group2,
                              int ranks2[]) ;
int MPI_Group_compare(MPI_Group group1, MPI_Group group2, int *result) ;
int MPI_Comm_group(MPI_Comm comm, MPI_Group *group) ;
int MPI_Group_union(MPI_Group group1, MPI_Group group2, MPI_Group *newgroup) ;
int MPI_Group_intersection(MPI_Group group1, MPI_Group group2, MPI_Group *newgroup) ;
int MPI_Group_difference(MPI_Group group1, MPI_Group group2, MPI_Group *newgroup) ;
int MPI_Group_incl(MPI_Group group, int n, const int ranks[], MPI_Group *newgroup) ;
int MPI_Group_excl(MPI_Group group, int n, const int ranks[], MPI_Group *newgroup) ;
int MPI_Group_range_incl(MPI_Group group, int n, int ranges[][3], MPI_Group *newgroup) ;
int MPI_Group_range_excl(MPI_Group group, int n, int ranges[][3], MPI_Group *newgroup) ;
int MPI_Group_free(MPI_Group *group) ;
int MPI_Comm_size(MPI_Comm comm, int *size) ;
int MPI_Comm_rank(MPI_Comm comm, int *rank) ;
int MPI_Comm_compare(MPI_Comm comm1, MPI_Comm comm2, int *result) ;
int MPI_Comm_dup(MPI_Comm comm, MPI_Comm *newcomm) ;
int MPI_Comm_dup_with_info(MPI_Comm comm, MPI_Info info, MPI_Comm *newcomm) ;
int MPI_Comm_create(MPI_Comm comm, MPI_Group group, MPI_Comm *newcomm) ;
int MPI_Comm_split(MPI_Comm comm, int color, int key, MPI_Comm *newcomm) ;
int MPI_Comm_free(MPI_Comm *comm) ;
int MPI_Comm_test_inter(MPI_Comm comm, int *flag) ;
int MPI_Comm_remote_size(MPI_Comm comm, int *size) ;
int MPI_Comm_remote_group(MPI_Comm comm, MPI_Group *group) ;
int MPI_Intercomm_create(MPI_Comm local_comm, int local_leader, MPI_Comm peer_comm,
                         int remote_leader, int tag, MPI_Comm *newintercomm) ;
int MPI_Intercomm_merge(MPI_Comm intercomm, int high, MPI_Comm *newintracomm) ;
int MPI_Keyval_create(MPI_Copy_function *copy_fn, MPI_Delete_function *delete_fn,
                      int *keyval, void *extra_state) ;
int MPI_Keyval_free(int *keyval) ;
int MPI_Attr_put(MPI_Comm comm, int keyval, void *attribute_val) ;
int MPI_Attr_get(MPI_Comm comm, int keyval, void *attribute_val, int *flag) ;
int MPI_Attr_delete(MPI_Comm comm, int keyval) ;
int MPI_Topo_test(MPI_Comm comm, int *status) ;
int MPI_Cart_create(MPI_Comm comm_old, int ndims, const int dims[], const int periods[],
                    int reorder, MPI_Comm *comm_cart) ;
int MPI_Dims_create(int nnodes, int ndims, int dims[]) ;
int MPI_Graph_create(MPI_Comm comm_old, int nnodes, const int indx[], const int edges[],
                     int reorder, MPI_Comm *comm_graph) ;
int MPI_Graphdims_get(MPI_Comm comm, int *nnodes, int *nedges) ;
int MPI_Graph_get(MPI_Comm comm, int maxindex, int maxedges, int indx[], int edges[]) ;
int MPI_Cartdim_get(MPI_Comm comm, int *ndims) ;
int MPI_Cart_get(MPI_Comm comm, int maxdims, int dims[], int periods[], int coords[]) ;
int MPI_Cart_rank(MPI_Comm comm, const int coords[], int *rank) ;
int MPI_Cart_coords(MPI_Comm comm, int rank, int maxdims, int coords[]) ;
int MPI_Graph_neighbors_count(MPI_Comm comm, int rank, int *nneighbors) ;
int MPI_Graph_neighbors(MPI_Comm comm, int rank, int maxneighbors, int neighbors[]) ;
int MPI_Cart_shift(MPI_Comm comm, int direction, int disp, int *rank_source, int *rank_dest) ;
int MPI_Cart_sub(MPI_Comm comm, const int remain_dims[], MPI_Comm *newcomm) ;
int MPI_Cart_map(MPI_Comm comm, int ndims, const int dims[], const int periods[], int *newrank) ;
int MPI_Graph_map(MPI_Comm comm, int nnodes, const int indx[], const int edges[], int *newrank) ;
int MPI_Get_processor_name(char *name, int *resultlen) ;
int MPI_Get_version(int *version, int *subversion) ;
int MPI_Get_library_version(char *version, int *resultlen) ;
int MPI_Errhandler_create(MPI_Handler_function *function, MPI_Errhandler *errhandler) ;
int MPI_Errhandler_set(MPI_Comm comm, MPI_Errhandler errhandler) ;
int MPI_Errhandler_get(MPI_Comm comm, MPI_Errhandler *errhandler) ;
int MPI_Errhandler_free(MPI_Errhandler *errhandler) ;
int MPI_Error_string(int errorcode, char *string, int *resultlen) ;
int MPI_Error_class(int errorcode, int *errorclass) ;
double MPI_Wtime(void) ;
double MPI_Wtick(void) ;
int MPI_Init(int *argc, char ***argv) ;
int MPI_Finalize(void) ;
int MPI_Initialized(int *flag) ;
int MPI_Abort(MPI_Comm comm, int errorcode) ;



int MPI_Pcontrol(const int level, ...) ;
int MPIR_Dup_fn(MPI_Comm oldcomm, int keyval, void *extra_state, void *attribute_val_in,
               void *attribute_val_out, int *flag) ;


int MPI_Close_port(const char *port_name) ;
int MPI_Comm_accept(const char *port_name, MPI_Info info, int root, MPI_Comm comm,
                    MPI_Comm *newcomm) ;
int MPI_Comm_connect(const char *port_name, MPI_Info info, int root, MPI_Comm comm,
                     MPI_Comm *newcomm) ;
int MPI_Comm_disconnect(MPI_Comm *comm) ;
int MPI_Comm_get_parent(MPI_Comm *parent) ;
int MPI_Comm_join(int fd, MPI_Comm *intercomm) ;
int MPI_Comm_spawn(const char *command, char *argv[], int maxprocs, MPI_Info info, int root,
                   MPI_Comm comm, MPI_Comm *intercomm, int array_of_errcodes[]) ;
int MPI_Comm_spawn_multiple(int count, char *array_of_commands[], char **array_of_argv[],
                            const int array_of_maxprocs[], const MPI_Info array_of_info[],
                            int root, MPI_Comm comm, MPI_Comm *intercomm, int array_of_errcodes[]) ;
int MPI_Lookup_name(const char *service_name, MPI_Info info, char *port_name) ;
int MPI_Open_port(MPI_Info info, char *port_name) ;
int MPI_Publish_name(const char *service_name, MPI_Info info, const char *port_name) ;
int MPI_Unpublish_name(const char *service_name, MPI_Info info, const char *port_name) ;
int MPI_Comm_set_info(MPI_Comm comm, MPI_Info info) ;
int MPI_Comm_get_info(MPI_Comm comm, MPI_Info *info) ;


int MPI_Accumulate(const void *origin_addr, int origin_count, MPI_Datatype origin_datatype,
                   int target_rank, MPI_Aint target_disp, int target_count,
                   MPI_Datatype target_datatype, MPI_Op op, MPI_Win win)
                                                                         ;
int MPI_Get(void *origin_addr, int origin_count, MPI_Datatype origin_datatype,
            int target_rank, MPI_Aint target_disp, int target_count,
            MPI_Datatype target_datatype, MPI_Win win) ;
int MPI_Put(const void *origin_addr, int origin_count, MPI_Datatype origin_datatype,
            int target_rank, MPI_Aint target_disp, int target_count,
            MPI_Datatype target_datatype, MPI_Win win) ;
int MPI_Win_complete(MPI_Win win) ;
int MPI_Win_create(void *base, MPI_Aint size, int disp_unit, MPI_Info info, MPI_Comm comm,
                   MPI_Win *win) ;
int MPI_Win_fence(int assert, MPI_Win win) ;
int MPI_Win_free(MPI_Win *win) ;
int MPI_Win_get_group(MPI_Win win, MPI_Group *group) ;
int MPI_Win_lock(int lock_type, int rank, int assert, MPI_Win win) ;
int MPI_Win_post(MPI_Group group, int assert, MPI_Win win) ;
int MPI_Win_start(MPI_Group group, int assert, MPI_Win win) ;
int MPI_Win_test(MPI_Win win, int *flag) ;
int MPI_Win_unlock(int rank, MPI_Win win) ;
int MPI_Win_wait(MPI_Win win) ;


int MPI_Win_allocate(MPI_Aint size, int disp_unit, MPI_Info info, MPI_Comm comm, void *baseptr,
                     MPI_Win *win) ;
int MPI_Win_allocate_shared(MPI_Aint size, int disp_unit, MPI_Info info, MPI_Comm comm,
                            void *baseptr, MPI_Win *win) ;
int MPI_Win_shared_query(MPI_Win win, int rank, MPI_Aint *size, int *disp_unit, void *baseptr) ;
int MPI_Win_create_dynamic(MPI_Info info, MPI_Comm comm, MPI_Win *win) ;
int MPI_Win_attach(MPI_Win win, void *base, MPI_Aint size) ;
int MPI_Win_detach(MPI_Win win, const void *base) ;
int MPI_Win_get_info(MPI_Win win, MPI_Info *info_used) ;
int MPI_Win_set_info(MPI_Win win, MPI_Info info) ;
int MPI_Get_accumulate(const void *origin_addr, int origin_count,
                        MPI_Datatype origin_datatype, void *result_addr, int result_count,
                        MPI_Datatype result_datatype, int target_rank, MPI_Aint target_disp,
                        int target_count, MPI_Datatype target_datatype, MPI_Op op, MPI_Win win)

                                                                              ;
int MPI_Fetch_and_op(const void *origin_addr, void *result_addr,
                      MPI_Datatype datatype, int target_rank, MPI_Aint target_disp,
                      MPI_Op op, MPI_Win win)
                                                                            ;
int MPI_Compare_and_swap(const void *origin_addr, const void *compare_addr,
                          void *result_addr, MPI_Datatype datatype, int target_rank,
                          MPI_Aint target_disp, MPI_Win win)


                                                                                ;
int MPI_Rput(const void *origin_addr, int origin_count,
              MPI_Datatype origin_datatype, int target_rank, MPI_Aint target_disp,
              int target_count, MPI_Datatype target_datatype, MPI_Win win,
              MPI_Request *request)
                                                                    ;
int MPI_Rget(void *origin_addr, int origin_count,
              MPI_Datatype origin_datatype, int target_rank, MPI_Aint target_disp,
              int target_count, MPI_Datatype target_datatype, MPI_Win win,
              MPI_Request *request)
                                                                    ;
int MPI_Raccumulate(const void *origin_addr, int origin_count,
                     MPI_Datatype origin_datatype, int target_rank, MPI_Aint target_disp,
                     int target_count, MPI_Datatype target_datatype, MPI_Op op, MPI_Win win,
                     MPI_Request *request)
                                                                           ;
int MPI_Rget_accumulate(const void *origin_addr, int origin_count,
                         MPI_Datatype origin_datatype, void *result_addr, int result_count,
                         MPI_Datatype result_datatype, int target_rank, MPI_Aint target_disp,
                         int target_count, MPI_Datatype target_datatype, MPI_Op op, MPI_Win win,
                         MPI_Request *request)

                                                                               ;
int MPI_Win_lock_all(int assert, MPI_Win win) ;
int MPI_Win_unlock_all(MPI_Win win) ;
int MPI_Win_flush(int rank, MPI_Win win) ;
int MPI_Win_flush_all(MPI_Win win) ;
int MPI_Win_flush_local(int rank, MPI_Win win) ;
int MPI_Win_flush_local_all(MPI_Win win) ;
int MPI_Win_sync(MPI_Win win) ;


int MPI_Add_error_class(int *errorclass) ;
int MPI_Add_error_code(int errorclass, int *errorcode) ;
int MPI_Add_error_string(int errorcode, const char *string) ;
int MPI_Comm_call_errhandler(MPI_Comm comm, int errorcode) ;
int MPI_Comm_create_keyval(MPI_Comm_copy_attr_function *comm_copy_attr_fn,
                           MPI_Comm_delete_attr_function *comm_delete_attr_fn, int *comm_keyval,
                           void *extra_state) ;
int MPI_Comm_delete_attr(MPI_Comm comm, int comm_keyval) ;
int MPI_Comm_free_keyval(int *comm_keyval) ;
int MPI_Comm_get_attr(MPI_Comm comm, int comm_keyval, void *attribute_val, int *flag) ;
int MPI_Comm_get_name(MPI_Comm comm, char *comm_name, int *resultlen) ;
int MPI_Comm_set_attr(MPI_Comm comm, int comm_keyval, void *attribute_val) ;
int MPI_Comm_set_name(MPI_Comm comm, const char *comm_name) ;
int MPI_File_call_errhandler(MPI_File fh, int errorcode) ;
int MPI_Grequest_complete(MPI_Request request) ;
int MPI_Grequest_start(MPI_Grequest_query_function *query_fn, MPI_Grequest_free_function *free_fn,
                       MPI_Grequest_cancel_function *cancel_fn, void *extra_state,
                       MPI_Request *request) ;
int MPI_Init_thread(int *argc, char ***argv, int required, int *provided) ;
int MPI_Is_thread_main(int *flag) ;
int MPI_Query_thread(int *provided) ;
int MPI_Status_set_cancelled(MPI_Status *status, int flag) ;
int MPI_Status_set_elements(MPI_Status *status, MPI_Datatype datatype, int count) ;
int MPI_Type_create_keyval(MPI_Type_copy_attr_function *type_copy_attr_fn,
                           MPI_Type_delete_attr_function *type_delete_attr_fn,
                           int *type_keyval, void *extra_state) ;
int MPI_Type_delete_attr(MPI_Datatype datatype, int type_keyval) ;
int MPI_Type_dup(MPI_Datatype oldtype, MPI_Datatype *newtype) ;
int MPI_Type_free_keyval(int *type_keyval) ;
int MPI_Type_get_attr(MPI_Datatype datatype, int type_keyval, void *attribute_val, int *flag) ;
int MPI_Type_get_contents(MPI_Datatype datatype, int max_integers, int max_addresses,
                          int max_datatypes, int array_of_integers[],
                          MPI_Aint array_of_addresses[], MPI_Datatype array_of_datatypes[]) ;
int MPI_Type_get_envelope(MPI_Datatype datatype, int *num_integers, int *num_addresses,
                          int *num_datatypes, int *combiner) ;
int MPI_Type_get_name(MPI_Datatype datatype, char *type_name, int *resultlen) ;
int MPI_Type_set_attr(MPI_Datatype datatype, int type_keyval, void *attribute_val) ;
int MPI_Type_set_name(MPI_Datatype datatype, const char *type_name) ;
int MPI_Type_match_size(int typeclass, int size, MPI_Datatype *datatype) ;
int MPI_Win_call_errhandler(MPI_Win win, int errorcode) ;
int MPI_Win_create_keyval(MPI_Win_copy_attr_function *win_copy_attr_fn,
                          MPI_Win_delete_attr_function *win_delete_attr_fn, int *win_keyval,
                          void *extra_state) ;
int MPI_Win_delete_attr(MPI_Win win, int win_keyval) ;
int MPI_Win_free_keyval(int *win_keyval) ;
int MPI_Win_get_attr(MPI_Win win, int win_keyval, void *attribute_val, int *flag) ;
int MPI_Win_get_name(MPI_Win win, char *win_name, int *resultlen) ;
int MPI_Win_set_attr(MPI_Win win, int win_keyval, void *attribute_val) ;
int MPI_Win_set_name(MPI_Win win, const char *win_name) ;

int MPI_Alloc_mem(MPI_Aint size, MPI_Info info, void *baseptr) ;
int MPI_Comm_create_errhandler(MPI_Comm_errhandler_function *comm_errhandler_fn,
                               MPI_Errhandler *errhandler) ;
int MPI_Comm_get_errhandler(MPI_Comm comm, MPI_Errhandler *errhandler) ;
int MPI_Comm_set_errhandler(MPI_Comm comm, MPI_Errhandler errhandler) ;
int MPI_File_create_errhandler(MPI_File_errhandler_function *file_errhandler_fn,
                               MPI_Errhandler *errhandler) ;
int MPI_File_get_errhandler(MPI_File file, MPI_Errhandler *errhandler) ;
int MPI_File_set_errhandler(MPI_File file, MPI_Errhandler errhandler) ;
int MPI_Finalized(int *flag) ;
int MPI_Free_mem(void *base) ;
int MPI_Get_address(const void *location, MPI_Aint *address) ;
int MPI_Info_create(MPI_Info *info) ;
int MPI_Info_delete(MPI_Info info, const char *key) ;
int MPI_Info_dup(MPI_Info info, MPI_Info *newinfo) ;
int MPI_Info_free(MPI_Info *info) ;
int MPI_Info_get(MPI_Info info, const char *key, int valuelen, char *value, int *flag) ;
int MPI_Info_get_nkeys(MPI_Info info, int *nkeys) ;
int MPI_Info_get_nthkey(MPI_Info info, int n, char *key) ;
int MPI_Info_get_valuelen(MPI_Info info, const char *key, int *valuelen, int *flag) ;
int MPI_Info_set(MPI_Info info, const char *key, const char *value) ;
int MPI_Pack_external(const char datarep[], const void *inbuf, int incount,
                      MPI_Datatype datatype, void *outbuf, MPI_Aint outsize, MPI_Aint *position)
                                                                            ;
int MPI_Pack_external_size(const char datarep[], int incount, MPI_Datatype datatype,
                           MPI_Aint *size) ;
int MPI_Request_get_status(MPI_Request request, int *flag, MPI_Status *status) ;
int MPI_Status_c2f(const MPI_Status *c_status, MPI_Fint *f_status) ;
int MPI_Status_f2c(const MPI_Fint *f_status, MPI_Status *c_status) ;
int MPI_Type_create_darray(int size, int rank, int ndims, const int array_of_gsizes[],
                           const int array_of_distribs[], const int array_of_dargs[],
                           const int array_of_psizes[], int order, MPI_Datatype oldtype,
                           MPI_Datatype *newtype) ;
int MPI_Type_create_hindexed(int count, const int array_of_blocklengths[],
                             const MPI_Aint array_of_displacements[], MPI_Datatype oldtype,
                             MPI_Datatype *newtype) ;
int MPI_Type_create_hvector(int count, int blocklength, MPI_Aint stride, MPI_Datatype oldtype,
                            MPI_Datatype *newtype) ;
int MPI_Type_create_indexed_block(int count, int blocklength, const int array_of_displacements[],
                                  MPI_Datatype oldtype, MPI_Datatype *newtype) ;
int MPI_Type_create_hindexed_block(int count, int blocklength,
                                   const MPI_Aint array_of_displacements[],
                                   MPI_Datatype oldtype, MPI_Datatype *newtype) ;
int MPI_Type_create_resized(MPI_Datatype oldtype, MPI_Aint lb, MPI_Aint extent,
                            MPI_Datatype *newtype) ;
int MPI_Type_create_struct(int count, const int array_of_blocklengths[],
                           const MPI_Aint array_of_displacements[],
                           const MPI_Datatype array_of_types[], MPI_Datatype *newtype) ;
int MPI_Type_create_subarray(int ndims, const int array_of_sizes[],
                             const int array_of_subsizes[], const int array_of_starts[],
                             int order, MPI_Datatype oldtype, MPI_Datatype *newtype) ;
int MPI_Type_get_extent(MPI_Datatype datatype, MPI_Aint *lb, MPI_Aint *extent) ;
int MPI_Type_get_true_extent(MPI_Datatype datatype, MPI_Aint *true_lb, MPI_Aint *true_extent) ;
int MPI_Unpack_external(const char datarep[], const void *inbuf, MPI_Aint insize,
                        MPI_Aint *position, void *outbuf, int outcount, MPI_Datatype datatype)
                                                                              ;
int MPI_Win_create_errhandler(MPI_Win_errhandler_function *win_errhandler_fn,
                              MPI_Errhandler *errhandler) ;
int MPI_Win_get_errhandler(MPI_Win win, MPI_Errhandler *errhandler) ;
int MPI_Win_set_errhandler(MPI_Win win, MPI_Errhandler errhandler) ;




int MPI_Type_create_f90_integer(int range, MPI_Datatype *newtype) ;
int MPI_Type_create_f90_real(int precision, int range, MPI_Datatype *newtype) ;
int MPI_Type_create_f90_complex(int precision, int range, MPI_Datatype *newtype) ;

int MPI_Reduce_local(const void *inbuf, void *inoutbuf, int count, MPI_Datatype datatype,
                     MPI_Op op)
                                                                                                                 ;
int MPI_Op_commutative(MPI_Op op, int *commute) ;
int MPI_Reduce_scatter_block(const void *sendbuf, void *recvbuf, int recvcount,
                             MPI_Datatype datatype, MPI_Op op, MPI_Comm comm)

                                                                                   ;
int MPI_Dist_graph_create_adjacent(MPI_Comm comm_old, int indegree, const int sources[],
                                   const int sourceweights[], int outdegree,
                                   const int destinations[], const int destweights[],
                                   MPI_Info info, int reorder, MPI_Comm *comm_dist_graph) ;
int MPI_Dist_graph_create(MPI_Comm comm_old, int n, const int sources[], const int degrees[],
                          const int destinations[], const int weights[], MPI_Info info,
                          int reorder, MPI_Comm *comm_dist_graph) ;
int MPI_Dist_graph_neighbors_count(MPI_Comm comm, int *indegree, int *outdegree, int *weighted) ;
int MPI_Dist_graph_neighbors(MPI_Comm comm, int maxindegree, int sources[], int sourceweights[],
                             int maxoutdegree, int destinations[], int destweights[]) ;


int MPI_Improbe(int source, int tag, MPI_Comm comm, int *flag, MPI_Message *message,
                MPI_Status *status) ;
int MPI_Imrecv(void *buf, int count, MPI_Datatype datatype, MPI_Message *message,
               MPI_Request *request) ;
int MPI_Mprobe(int source, int tag, MPI_Comm comm, MPI_Message *message, MPI_Status *status) ;
int MPI_Mrecv(void *buf, int count, MPI_Datatype datatype, MPI_Message *message,
              MPI_Status *status) ;


int MPI_Comm_idup(MPI_Comm comm, MPI_Comm *newcomm, MPI_Request *request) ;
int MPI_Ibarrier(MPI_Comm comm, MPI_Request *request) ;
int MPI_Ibcast(void *buffer, int count, MPI_Datatype datatype, int root, MPI_Comm comm,
               MPI_Request *request) ;
int MPI_Igather(const void *sendbuf, int sendcount, MPI_Datatype sendtype, void *recvbuf,
                int recvcount, MPI_Datatype recvtype, int root, MPI_Comm comm,
                MPI_Request *request)
                                                                                                            ;
int MPI_Igatherv(const void *sendbuf, int sendcount, MPI_Datatype sendtype, void *recvbuf,
                 const int recvcounts[], const int displs[], MPI_Datatype recvtype, int root,
                 MPI_Comm comm, MPI_Request *request)
                                                                                                             ;
int MPI_Iscatter(const void *sendbuf, int sendcount, MPI_Datatype sendtype, void *recvbuf,
                 int recvcount, MPI_Datatype recvtype, int root, MPI_Comm comm,
                 MPI_Request *request)
                                                                                                             ;
int MPI_Iscatterv(const void *sendbuf, const int sendcounts[], const int displs[],
                  MPI_Datatype sendtype, void *recvbuf, int recvcount, MPI_Datatype recvtype,
                  int root, MPI_Comm comm, MPI_Request *request)
                                                                                                              ;
int MPI_Iallgather(const void *sendbuf, int sendcount, MPI_Datatype sendtype, void *recvbuf,
                   int recvcount, MPI_Datatype recvtype, MPI_Comm comm, MPI_Request *request)
                                                                                                               ;
int MPI_Iallgatherv(const void *sendbuf, int sendcount, MPI_Datatype sendtype, void *recvbuf,
                    const int recvcounts[], const int displs[], MPI_Datatype recvtype,
                    MPI_Comm comm, MPI_Request *request)
                                                                                                                ;
int MPI_Ialltoall(const void *sendbuf, int sendcount, MPI_Datatype sendtype, void *recvbuf,
                  int recvcount, MPI_Datatype recvtype, MPI_Comm comm, MPI_Request *request)
                                                                                                              ;
int MPI_Ialltoallv(const void *sendbuf, const int sendcounts[], const int sdispls[],
                   MPI_Datatype sendtype, void *recvbuf, const int recvcounts[],
                   const int rdispls[], MPI_Datatype recvtype, MPI_Comm comm,
                   MPI_Request *request)
                                                                                                               ;
int MPI_Ialltoallw(const void *sendbuf, const int sendcounts[], const int sdispls[],
                   const MPI_Datatype sendtypes[], void *recvbuf, const int recvcounts[],
                   const int rdispls[], const MPI_Datatype recvtypes[], MPI_Comm comm,
                   MPI_Request *request) ;
int MPI_Ireduce(const void *sendbuf, void *recvbuf, int count, MPI_Datatype datatype,
                MPI_Op op, int root, MPI_Comm comm, MPI_Request *request)
                                                                                                            ;
int MPI_Iallreduce(const void *sendbuf, void *recvbuf, int count, MPI_Datatype datatype,
                   MPI_Op op, MPI_Comm comm, MPI_Request *request)
                                                                                                               ;
int MPI_Ireduce_scatter(const void *sendbuf, void *recvbuf, const int recvcounts[],
                        MPI_Datatype datatype, MPI_Op op, MPI_Comm comm, MPI_Request *request)
                                                                                                                    ;
int MPI_Ireduce_scatter_block(const void *sendbuf, void *recvbuf, int recvcount,
                              MPI_Datatype datatype, MPI_Op op, MPI_Comm comm,
                              MPI_Request *request)

                                                                                    ;
int MPI_Iscan(const void *sendbuf, void *recvbuf, int count, MPI_Datatype datatype, MPI_Op op,
              MPI_Comm comm, MPI_Request *request)
                                                                                                          ;
int MPI_Iexscan(const void *sendbuf, void *recvbuf, int count, MPI_Datatype datatype,
                MPI_Op op, MPI_Comm comm, MPI_Request *request)
                                                                                                            ;


int MPI_Ineighbor_allgather(const void *sendbuf, int sendcount, MPI_Datatype sendtype,
                            void *recvbuf, int recvcount, MPI_Datatype recvtype,
                            MPI_Comm comm, MPI_Request *request)

                                                                                  ;
int MPI_Ineighbor_allgatherv(const void *sendbuf, int sendcount, MPI_Datatype sendtype,
                             void *recvbuf, const int recvcounts[], const int displs[],
                             MPI_Datatype recvtype, MPI_Comm comm, MPI_Request *request)

                                                                                   ;
int MPI_Ineighbor_alltoall(const void *sendbuf, int sendcount, MPI_Datatype sendtype,
                           void *recvbuf, int recvcount, MPI_Datatype recvtype, MPI_Comm comm,
                           MPI_Request *request)

                                                                                 ;
int MPI_Ineighbor_alltoallv(const void *sendbuf, const int sendcounts[], const int sdispls[],
                            MPI_Datatype sendtype, void *recvbuf, const int recvcounts[],
                            const int rdispls[], MPI_Datatype recvtype, MPI_Comm comm,
                            MPI_Request *request)

                                                                                  ;
int MPI_Ineighbor_alltoallw(const void *sendbuf, const int sendcounts[],
                            const MPI_Aint sdispls[], const MPI_Datatype sendtypes[],
                            void *recvbuf, const int recvcounts[], const MPI_Aint rdispls[],
                            const MPI_Datatype recvtypes[], MPI_Comm comm, MPI_Request *request) ;
int MPI_Neighbor_allgather(const void *sendbuf, int sendcount, MPI_Datatype sendtype,
                           void *recvbuf, int recvcount, MPI_Datatype recvtype, MPI_Comm comm)

                                                                                 ;
int MPI_Neighbor_allgatherv(const void *sendbuf, int sendcount, MPI_Datatype sendtype,
                            void *recvbuf, const int recvcounts[], const int displs[],
                            MPI_Datatype recvtype, MPI_Comm comm)

                                                                                  ;
int MPI_Neighbor_alltoall(const void *sendbuf, int sendcount, MPI_Datatype sendtype,
                          void *recvbuf, int recvcount, MPI_Datatype recvtype, MPI_Comm comm)

                                                                                ;
int MPI_Neighbor_alltoallv(const void *sendbuf, const int sendcounts[], const int sdispls[],
                           MPI_Datatype sendtype, void *recvbuf, const int recvcounts[],
                           const int rdispls[], MPI_Datatype recvtype, MPI_Comm comm)

                                                                                 ;
int MPI_Neighbor_alltoallw(const void *sendbuf, const int sendcounts[], const MPI_Aint sdispls[],
                           const MPI_Datatype sendtypes[], void *recvbuf, const int recvcounts[],
                           const MPI_Aint rdispls[], const MPI_Datatype recvtypes[], MPI_Comm comm) ;


int MPI_Comm_split_type(MPI_Comm comm, int split_type, int key, MPI_Info info, MPI_Comm *newcomm) ;


int MPI_Get_elements_x(const MPI_Status *status, MPI_Datatype datatype, MPI_Count *count) ;
int MPI_Status_set_elements_x(MPI_Status *status, MPI_Datatype datatype, MPI_Count count) ;
int MPI_Type_get_extent_x(MPI_Datatype datatype, MPI_Count *lb, MPI_Count *extent) ;
int MPI_Type_get_true_extent_x(MPI_Datatype datatype, MPI_Count *lb, MPI_Count *extent) ;
int MPI_Type_size_x(MPI_Datatype datatype, MPI_Count *size) ;


int MPI_Comm_create_group(MPI_Comm comm, MPI_Group group, int tag, MPI_Comm *newcomm) ;


MPI_Aint MPI_Aint_add(MPI_Aint base, MPI_Aint disp) ;
MPI_Aint MPI_Aint_diff(MPI_Aint addr1, MPI_Aint addr2) ;





int MPI_T_init_thread(int required, int *provided) ;
int MPI_T_finalize(void) ;
int MPI_T_enum_get_info(MPI_T_enum enumtype, int *num, char *name, int *name_len) ;
int MPI_T_enum_get_item(MPI_T_enum enumtype, int indx, int *value, char *name, int *name_len) ;
int MPI_T_cvar_get_num(int *num_cvar) ;
int MPI_T_cvar_get_info(int cvar_index, char *name, int *name_len, int *verbosity,
                        MPI_Datatype *datatype, MPI_T_enum *enumtype, char *desc, int *desc_len,
                        int *binding, int *scope) ;
int MPI_T_cvar_handle_alloc(int cvar_index, void *obj_handle, MPI_T_cvar_handle *handle,
                            int *count) ;
int MPI_T_cvar_handle_free(MPI_T_cvar_handle *handle) ;
int MPI_T_cvar_read(MPI_T_cvar_handle handle, void *buf) ;
int MPI_T_cvar_write(MPI_T_cvar_handle handle, const void *buf) ;
int MPI_T_pvar_get_num(int *num_pvar) ;
int MPI_T_pvar_get_info(int pvar_index, char *name, int *name_len, int *verbosity, int *var_class,
                        MPI_Datatype *datatype, MPI_T_enum *enumtype, char *desc, int *desc_len,
                        int *binding, int *readonly, int *continuous, int *atomic) ;
int MPI_T_pvar_session_create(MPI_T_pvar_session *session) ;
int MPI_T_pvar_session_free(MPI_T_pvar_session *session) ;
int MPI_T_pvar_handle_alloc(MPI_T_pvar_session session, int pvar_index, void *obj_handle,
                            MPI_T_pvar_handle *handle, int *count) ;
int MPI_T_pvar_handle_free(MPI_T_pvar_session session, MPI_T_pvar_handle *handle) ;
int MPI_T_pvar_start(MPI_T_pvar_session session, MPI_T_pvar_handle handle) ;
int MPI_T_pvar_stop(MPI_T_pvar_session session, MPI_T_pvar_handle handle) ;
int MPI_T_pvar_read(MPI_T_pvar_session session, MPI_T_pvar_handle handle, void *buf) ;
int MPI_T_pvar_write(MPI_T_pvar_session session, MPI_T_pvar_handle handle, const void *buf) ;
int MPI_T_pvar_reset(MPI_T_pvar_session session, MPI_T_pvar_handle handle) ;
int MPI_T_pvar_readreset(MPI_T_pvar_session session, MPI_T_pvar_handle handle, void *buf) ;
int MPI_T_category_get_num(int *num_cat) ;
int MPI_T_category_get_info(int cat_index, char *name, int *name_len, char *desc, int *desc_len,
                            int *num_cvars, int *num_pvars, int *num_categories) ;
int MPI_T_category_get_cvars(int cat_index, int len, int indices[]) ;
int MPI_T_category_get_pvars(int cat_index, int len, int indices[]) ;
int MPI_T_category_get_categories(int cat_index, int len, int indices[]) ;
int MPI_T_category_changed(int *stamp) ;
int MPI_T_cvar_get_index(const char *name, int *cvar_index) ;
int MPI_T_pvar_get_index(const char *name, int var_class, int *pvar_index) ;
int MPI_T_category_get_index(const char *name, int *cat_index) ;





int MPIX_Comm_failure_ack(MPI_Comm comm) ;
int MPIX_Comm_failure_get_acked(MPI_Comm comm, MPI_Group *failedgrp) ;
int MPIX_Comm_revoke(MPI_Comm comm) ;
int MPIX_Comm_shrink(MPI_Comm comm, MPI_Comm *newcomm) ;
int MPIX_Comm_agree(MPI_Comm comm, int *flag) ;
# 1656 "/cvmfs/software.hpc.rwth.de/Linux/RH8/x86_64/intel/sapphirerapids/software/impi/2021.9.0-intel-compilers-2023.1.0/mpi/2021.9.0/include/mpi.h"
int PMPI_Send(const void *buf, int count, MPI_Datatype datatype, int dest, int tag,
              MPI_Comm comm) ;
int PMPI_Recv(void *buf, int count, MPI_Datatype datatype, int source, int tag,
              MPI_Comm comm, MPI_Status *status) ;
int PMPI_Get_count(const MPI_Status *status, MPI_Datatype datatype, int *count) ;
int PMPI_Bsend(const void *buf, int count, MPI_Datatype datatype, int dest, int tag,
               MPI_Comm comm) ;
int PMPI_Ssend(const void *buf, int count, MPI_Datatype datatype, int dest, int tag,
               MPI_Comm comm) ;
int PMPI_Rsend(const void *buf, int count, MPI_Datatype datatype, int dest, int tag,
               MPI_Comm comm) ;
int PMPI_Buffer_attach(void *buffer, int size) ;
int PMPI_Buffer_detach(void *buffer_addr, int *size) ;
int PMPI_Isend(const void *buf, int count, MPI_Datatype datatype, int dest, int tag,
               MPI_Comm comm, MPI_Request *request) ;
int PMPI_Ibsend(const void *buf, int count, MPI_Datatype datatype, int dest, int tag,
                MPI_Comm comm, MPI_Request *request) ;
int PMPI_Issend(const void *buf, int count, MPI_Datatype datatype, int dest, int tag,
                MPI_Comm comm, MPI_Request *request) ;
int PMPI_Irsend(const void *buf, int count, MPI_Datatype datatype, int dest, int tag,
                MPI_Comm comm, MPI_Request *request) ;
int PMPI_Irecv(void *buf, int count, MPI_Datatype datatype, int source, int tag,
               MPI_Comm comm, MPI_Request *request) ;
int PMPI_Wait(MPI_Request *request, MPI_Status *status) ;
int PMPI_Test(MPI_Request *request, int *flag, MPI_Status *status) ;
int PMPI_Request_free(MPI_Request *request) ;
int PMPI_Waitany(int count, MPI_Request array_of_requests[], int *indx, MPI_Status *status) ;
int PMPI_Testany(int count, MPI_Request array_of_requests[], int *indx, int *flag,
                 MPI_Status *status) ;
int PMPI_Waitall(int count, MPI_Request array_of_requests[], MPI_Status array_of_statuses[]) ;
int PMPI_Testall(int count, MPI_Request array_of_requests[], int *flag,
                 MPI_Status array_of_statuses[]) ;
int PMPI_Waitsome(int incount, MPI_Request array_of_requests[], int *outcount,
                  int array_of_indices[], MPI_Status array_of_statuses[]) ;
int PMPI_Testsome(int incount, MPI_Request array_of_requests[], int *outcount,
                  int array_of_indices[], MPI_Status array_of_statuses[]) ;
int PMPI_Iprobe(int source, int tag, MPI_Comm comm, int *flag, MPI_Status *status) ;
int PMPI_Probe(int source, int tag, MPI_Comm comm, MPI_Status *status) ;
int PMPI_Cancel(MPI_Request *request) ;
int PMPI_Test_cancelled(const MPI_Status *status, int *flag) ;
int PMPI_Send_init(const void *buf, int count, MPI_Datatype datatype, int dest, int tag,
                   MPI_Comm comm, MPI_Request *request) ;
int PMPI_Bsend_init(const void *buf, int count, MPI_Datatype datatype, int dest, int tag,
                    MPI_Comm comm, MPI_Request *request) ;
int PMPI_Ssend_init(const void *buf, int count, MPI_Datatype datatype, int dest, int tag,
                    MPI_Comm comm, MPI_Request *request) ;
int PMPI_Rsend_init(const void *buf, int count, MPI_Datatype datatype, int dest, int tag,
                    MPI_Comm comm, MPI_Request *request) ;
int PMPI_Recv_init(void *buf, int count, MPI_Datatype datatype, int source, int tag,
                   MPI_Comm comm, MPI_Request *request) ;
int PMPI_Start(MPI_Request *request) ;
int PMPI_Startall(int count, MPI_Request array_of_requests[]) ;
int PMPI_Sendrecv(const void *sendbuf, int sendcount, MPI_Datatype sendtype, int dest,
                  int sendtag, void *recvbuf, int recvcount, MPI_Datatype recvtype,
                  int source, int recvtag, MPI_Comm comm, MPI_Status *status)

                                                                        ;
int PMPI_Sendrecv_replace(void *buf, int count, MPI_Datatype datatype, int dest,
                          int sendtag, int source, int recvtag, MPI_Comm comm,
                          MPI_Status *status) ;
int PMPI_Type_contiguous(int count, MPI_Datatype oldtype, MPI_Datatype *newtype) ;
int PMPI_Type_vector(int count, int blocklength, int stride, MPI_Datatype oldtype,
                     MPI_Datatype *newtype) ;
int PMPI_Type_hvector(int count, int blocklength, MPI_Aint stride, MPI_Datatype oldtype,
                      MPI_Datatype *newtype) ;
int PMPI_Type_indexed(int count, const int *array_of_blocklengths,
                      const int *array_of_displacements, MPI_Datatype oldtype,
                      MPI_Datatype *newtype) ;
int PMPI_Type_hindexed(int count, int *array_of_blocklengths,
                       MPI_Aint *array_of_displacements, MPI_Datatype oldtype,
                       MPI_Datatype *newtype) ;
int PMPI_Type_struct(int count, int *array_of_blocklengths,
                     MPI_Aint *array_of_displacements,
                     MPI_Datatype *array_of_types, MPI_Datatype *newtype) ;
int PMPI_Address(void *location, MPI_Aint *address) ;
int PMPI_Type_extent(MPI_Datatype datatype, MPI_Aint *extent) ;
int PMPI_Type_size(MPI_Datatype datatype, int *size) ;
int PMPI_Type_lb(MPI_Datatype datatype, MPI_Aint *displacement) ;
int PMPI_Type_ub(MPI_Datatype datatype, MPI_Aint *displacement) ;
int PMPI_Type_commit(MPI_Datatype *datatype) ;
int PMPI_Type_free(MPI_Datatype *datatype) ;
int PMPI_Get_elements(const MPI_Status *status, MPI_Datatype datatype, int *count) ;
int PMPI_Pack(const void *inbuf, int incount, MPI_Datatype datatype, void *outbuf,
              int outsize, int *position, MPI_Comm comm) ;
int PMPI_Unpack(const void *inbuf, int insize, int *position, void *outbuf, int outcount,
                MPI_Datatype datatype, MPI_Comm comm) ;
int PMPI_Pack_size(int incount, MPI_Datatype datatype, MPI_Comm comm, int *size) ;
int PMPI_Barrier(MPI_Comm comm) ;
int PMPI_Bcast(void *buffer, int count, MPI_Datatype datatype, int root, MPI_Comm comm)
                                                                     ;
int PMPI_Gather(const void *sendbuf, int sendcount, MPI_Datatype sendtype, void *recvbuf,
                int recvcount, MPI_Datatype recvtype, int root, MPI_Comm comm)
                                                                                                            ;
int PMPI_Gatherv(const void *sendbuf, int sendcount, MPI_Datatype sendtype, void *recvbuf,
                 const int *recvcounts, const int *displs, MPI_Datatype recvtype, int root,
                 MPI_Comm comm)
                                                                                                             ;
int PMPI_Scatter(const void *sendbuf, int sendcount, MPI_Datatype sendtype, void *recvbuf,
                 int recvcount, MPI_Datatype recvtype, int root, MPI_Comm comm)
                                                                                                             ;
int PMPI_Scatterv(const void *sendbuf, const int *sendcounts, const int *displs,
                  MPI_Datatype sendtype, void *recvbuf, int recvcount, MPI_Datatype recvtype,
                  int root, MPI_Comm comm)
                                                                                                              ;
int PMPI_Allgather(const void *sendbuf, int sendcount, MPI_Datatype sendtype, void *recvbuf,
                   int recvcount, MPI_Datatype recvtype, MPI_Comm comm)
                                                                                                               ;
int PMPI_Allgatherv(const void *sendbuf, int sendcount, MPI_Datatype sendtype, void *recvbuf,
                    const int *recvcounts, const int *displs, MPI_Datatype recvtype, MPI_Comm comm)
                                                                                                                ;
int PMPI_Alltoall(const void *sendbuf, int sendcount, MPI_Datatype sendtype, void *recvbuf,
                  int recvcount, MPI_Datatype recvtype, MPI_Comm comm)
                                                                                                              ;
int PMPI_Alltoallv(const void *sendbuf, const int *sendcounts, const int *sdispls,
                   MPI_Datatype sendtype, void *recvbuf, const int *recvcounts,
                   const int *rdispls, MPI_Datatype recvtype, MPI_Comm comm)
                                                                                                               ;
int PMPI_Alltoallw(const void *sendbuf, const int sendcounts[], const int sdispls[],
                   const MPI_Datatype sendtypes[], void *recvbuf, const int recvcounts[],
                   const int rdispls[], const MPI_Datatype recvtypes[], MPI_Comm comm) ;
int PMPI_Exscan(const void *sendbuf, void *recvbuf, int count, MPI_Datatype datatype,
                MPI_Op op, MPI_Comm comm)
                                                                                                            ;
int PMPI_Reduce(const void *sendbuf, void *recvbuf, int count, MPI_Datatype datatype,
                MPI_Op op, int root, MPI_Comm comm)
                                                                                                            ;
int PMPI_Op_create(MPI_User_function *user_fn, int commute, MPI_Op *op) ;
int PMPI_Op_free(MPI_Op *op) ;
int PMPI_Allreduce(const void *sendbuf, void *recvbuf, int count, MPI_Datatype datatype,
                   MPI_Op op, MPI_Comm comm)
                                                                                                               ;
int PMPI_Reduce_scatter(const void *sendbuf, void *recvbuf, const int recvcounts[],
                        MPI_Datatype datatype, MPI_Op op, MPI_Comm comm)
                                                                                                                    ;
int PMPI_Scan(const void *sendbuf, void *recvbuf, int count, MPI_Datatype datatype, MPI_Op op,
              MPI_Comm comm)
                                                                                                          ;
int PMPI_Group_size(MPI_Group group, int *size) ;
int PMPI_Group_rank(MPI_Group group, int *rank) ;
int PMPI_Group_translate_ranks(MPI_Group group1, int n, const int ranks1[], MPI_Group group2,
                               int ranks2[]) ;
int PMPI_Group_compare(MPI_Group group1, MPI_Group group2, int *result) ;
int PMPI_Comm_group(MPI_Comm comm, MPI_Group *group) ;
int PMPI_Group_union(MPI_Group group1, MPI_Group group2, MPI_Group *newgroup) ;
int PMPI_Group_intersection(MPI_Group group1, MPI_Group group2, MPI_Group *newgroup) ;
int PMPI_Group_difference(MPI_Group group1, MPI_Group group2, MPI_Group *newgroup) ;
int PMPI_Group_incl(MPI_Group group, int n, const int ranks[], MPI_Group *newgroup) ;
int PMPI_Group_excl(MPI_Group group, int n, const int ranks[], MPI_Group *newgroup) ;
int PMPI_Group_range_incl(MPI_Group group, int n, int ranges[][3], MPI_Group *newgroup) ;
int PMPI_Group_range_excl(MPI_Group group, int n, int ranges[][3], MPI_Group *newgroup) ;
int PMPI_Group_free(MPI_Group *group) ;
int PMPI_Comm_size(MPI_Comm comm, int *size) ;
int PMPI_Comm_rank(MPI_Comm comm, int *rank) ;
int PMPI_Comm_compare(MPI_Comm comm1, MPI_Comm comm2, int *result) ;
int PMPI_Comm_dup(MPI_Comm comm, MPI_Comm *newcomm) ;
int PMPI_Comm_dup_with_info(MPI_Comm comm, MPI_Info info, MPI_Comm *newcomm) ;
int PMPI_Comm_create(MPI_Comm comm, MPI_Group group, MPI_Comm *newcomm) ;
int PMPI_Comm_split(MPI_Comm comm, int color, int key, MPI_Comm *newcomm) ;
int PMPI_Comm_free(MPI_Comm *comm) ;
int PMPI_Comm_test_inter(MPI_Comm comm, int *flag) ;
int PMPI_Comm_remote_size(MPI_Comm comm, int *size) ;
int PMPI_Comm_remote_group(MPI_Comm comm, MPI_Group *group) ;
int PMPI_Intercomm_create(MPI_Comm local_comm, int local_leader, MPI_Comm peer_comm,
                          int remote_leader, int tag, MPI_Comm *newintercomm) ;
int PMPI_Intercomm_merge(MPI_Comm intercomm, int high, MPI_Comm *newintracomm) ;
int PMPI_Keyval_create(MPI_Copy_function *copy_fn, MPI_Delete_function *delete_fn,
                       int *keyval, void *extra_state) ;
int PMPI_Keyval_free(int *keyval) ;
int PMPI_Attr_put(MPI_Comm comm, int keyval, void *attribute_val) ;
int PMPI_Attr_get(MPI_Comm comm, int keyval, void *attribute_val, int *flag) ;
int PMPI_Attr_delete(MPI_Comm comm, int keyval) ;
int PMPI_Topo_test(MPI_Comm comm, int *status) ;
int PMPI_Cart_create(MPI_Comm comm_old, int ndims, const int dims[], const int periods[],
                     int reorder, MPI_Comm *comm_cart) ;
int PMPI_Dims_create(int nnodes, int ndims, int dims[]) ;
int PMPI_Graph_create(MPI_Comm comm_old, int nnodes, const int indx[], const int edges[],
                      int reorder, MPI_Comm *comm_graph) ;
int PMPI_Graphdims_get(MPI_Comm comm, int *nnodes, int *nedges) ;
int PMPI_Graph_get(MPI_Comm comm, int maxindex, int maxedges, int indx[], int edges[]) ;
int PMPI_Cartdim_get(MPI_Comm comm, int *ndims) ;
int PMPI_Cart_get(MPI_Comm comm, int maxdims, int dims[], int periods[], int coords[]) ;
int PMPI_Cart_rank(MPI_Comm comm, const int coords[], int *rank) ;
int PMPI_Cart_coords(MPI_Comm comm, int rank, int maxdims, int coords[]) ;
int PMPI_Graph_neighbors_count(MPI_Comm comm, int rank, int *nneighbors) ;
int PMPI_Graph_neighbors(MPI_Comm comm, int rank, int maxneighbors, int neighbors[]) ;
int PMPI_Cart_shift(MPI_Comm comm, int direction, int disp, int *rank_source, int *rank_dest) ;
int PMPI_Cart_sub(MPI_Comm comm, const int remain_dims[], MPI_Comm *newcomm) ;
int PMPI_Cart_map(MPI_Comm comm, int ndims, const int dims[], const int periods[], int *newrank) ;
int PMPI_Graph_map(MPI_Comm comm, int nnodes, const int indx[], const int edges[], int *newrank) ;
int PMPI_Get_processor_name(char *name, int *resultlen) ;
int PMPI_Get_version(int *version, int *subversion) ;
int PMPI_Get_library_version(char *version, int *resultlen) ;
int PMPI_Errhandler_create(MPI_Handler_function *function, MPI_Errhandler *errhandler) ;
int PMPI_Errhandler_set(MPI_Comm comm, MPI_Errhandler errhandler) ;
int PMPI_Errhandler_get(MPI_Comm comm, MPI_Errhandler *errhandler) ;
int PMPI_Errhandler_free(MPI_Errhandler *errhandler) ;
int PMPI_Error_string(int errorcode, char *string, int *resultlen) ;
int PMPI_Error_class(int errorcode, int *errorclass) ;
double PMPI_Wtime(void) ;
double PMPI_Wtick(void) ;
int PMPI_Init(int *argc, char ***argv) ;
int PMPI_Finalize(void) ;
int PMPI_Initialized(int *flag) ;
int PMPI_Abort(MPI_Comm comm, int errorcode) ;



int PMPI_Pcontrol(const int level, ...) ;


int PMPI_Close_port(const char *port_name) ;
int PMPI_Comm_accept(const char *port_name, MPI_Info info, int root, MPI_Comm comm,
                     MPI_Comm *newcomm) ;
int PMPI_Comm_connect(const char *port_name, MPI_Info info, int root, MPI_Comm comm,
                      MPI_Comm *newcomm) ;
int PMPI_Comm_disconnect(MPI_Comm *comm) ;
int PMPI_Comm_get_parent(MPI_Comm *parent) ;
int PMPI_Comm_join(int fd, MPI_Comm *intercomm) ;
int PMPI_Comm_spawn(const char *command, char *argv[], int maxprocs, MPI_Info info, int root,
                    MPI_Comm comm, MPI_Comm *intercomm, int array_of_errcodes[]) ;
int PMPI_Comm_spawn_multiple(int count, char *array_of_commands[], char **array_of_argv[],
                             const int array_of_maxprocs[], const MPI_Info array_of_info[],
                             int root, MPI_Comm comm, MPI_Comm *intercomm, int array_of_errcodes[]) ;
int PMPI_Lookup_name(const char *service_name, MPI_Info info, char *port_name) ;
int PMPI_Open_port(MPI_Info info, char *port_name) ;
int PMPI_Publish_name(const char *service_name, MPI_Info info, const char *port_name) ;
int PMPI_Unpublish_name(const char *service_name, MPI_Info info, const char *port_name) ;
int PMPI_Comm_set_info(MPI_Comm comm, MPI_Info info) ;
int PMPI_Comm_get_info(MPI_Comm comm, MPI_Info *info) ;


int PMPI_Accumulate(const void *origin_addr, int origin_count, MPI_Datatype origin_datatype,
                    int target_rank, MPI_Aint target_disp, int target_count,
                    MPI_Datatype target_datatype, MPI_Op op, MPI_Win win)
                                                                          ;
int PMPI_Get(void *origin_addr, int origin_count, MPI_Datatype origin_datatype,
             int target_rank, MPI_Aint target_disp, int target_count,
             MPI_Datatype target_datatype, MPI_Win win) ;
int PMPI_Put(const void *origin_addr, int origin_count, MPI_Datatype origin_datatype,
             int target_rank, MPI_Aint target_disp, int target_count,
             MPI_Datatype target_datatype, MPI_Win win) ;
int PMPI_Win_complete(MPI_Win win) ;
int PMPI_Win_create(void *base, MPI_Aint size, int disp_unit, MPI_Info info, MPI_Comm comm,
                    MPI_Win *win) ;
int PMPI_Win_fence(int assert, MPI_Win win) ;
int PMPI_Win_free(MPI_Win *win) ;
int PMPI_Win_get_group(MPI_Win win, MPI_Group *group) ;
int PMPI_Win_lock(int lock_type, int rank, int assert, MPI_Win win) ;
int PMPI_Win_post(MPI_Group group, int assert, MPI_Win win) ;
int PMPI_Win_start(MPI_Group group, int assert, MPI_Win win) ;
int PMPI_Win_test(MPI_Win win, int *flag) ;
int PMPI_Win_unlock(int rank, MPI_Win win) ;
int PMPI_Win_wait(MPI_Win win) ;


int PMPI_Win_allocate(MPI_Aint size, int disp_unit, MPI_Info info, MPI_Comm comm, void *baseptr,
                      MPI_Win *win) ;
int PMPI_Win_allocate_shared(MPI_Aint size, int disp_unit, MPI_Info info, MPI_Comm comm,
                             void *baseptr, MPI_Win *win) ;
int PMPI_Win_shared_query(MPI_Win win, int rank, MPI_Aint *size, int *disp_unit, void *baseptr) ;
int PMPI_Win_create_dynamic(MPI_Info info, MPI_Comm comm, MPI_Win *win) ;
int PMPI_Win_attach(MPI_Win win, void *base, MPI_Aint size) ;
int PMPI_Win_detach(MPI_Win win, const void *base) ;
int PMPI_Win_get_info(MPI_Win win, MPI_Info *info_used) ;
int PMPI_Win_set_info(MPI_Win win, MPI_Info info) ;
int PMPI_Get_accumulate(const void *origin_addr, int origin_count,
                         MPI_Datatype origin_datatype, void *result_addr, int result_count,
                         MPI_Datatype result_datatype, int target_rank, MPI_Aint target_disp,
                         int target_count, MPI_Datatype target_datatype, MPI_Op op, MPI_Win win)

                                                                               ;
int PMPI_Fetch_and_op(const void *origin_addr, void *result_addr,
                       MPI_Datatype datatype, int target_rank, MPI_Aint target_disp,
                       MPI_Op op, MPI_Win win)
                                                                             ;
int PMPI_Compare_and_swap(const void *origin_addr, const void *compare_addr,
                           void *result_addr, MPI_Datatype datatype, int target_rank,
                           MPI_Aint target_disp, MPI_Win win)


                                                                                 ;
int PMPI_Rput(const void *origin_addr, int origin_count,
               MPI_Datatype origin_datatype, int target_rank, MPI_Aint target_disp,
               int target_count, MPI_Datatype target_datatype, MPI_Win win,
               MPI_Request *request)
                                                                     ;
int PMPI_Rget(void *origin_addr, int origin_count,
               MPI_Datatype origin_datatype, int target_rank, MPI_Aint target_disp,
               int target_count, MPI_Datatype target_datatype, MPI_Win win,
               MPI_Request *request)
                                                                     ;
int PMPI_Raccumulate(const void *origin_addr, int origin_count,
                      MPI_Datatype origin_datatype, int target_rank, MPI_Aint target_disp,
                      int target_count, MPI_Datatype target_datatype, MPI_Op op, MPI_Win win,
                      MPI_Request *request)
                                                                            ;
int PMPI_Rget_accumulate(const void *origin_addr, int origin_count,
                          MPI_Datatype origin_datatype, void *result_addr, int result_count,
                          MPI_Datatype result_datatype, int target_rank, MPI_Aint target_disp,
                          int target_count, MPI_Datatype target_datatype, MPI_Op op, MPI_Win win,
                          MPI_Request *request)

                                                                                ;
int PMPI_Win_lock_all(int assert, MPI_Win win) ;
int PMPI_Win_unlock_all(MPI_Win win) ;
int PMPI_Win_flush(int rank, MPI_Win win) ;
int PMPI_Win_flush_all(MPI_Win win) ;
int PMPI_Win_flush_local(int rank, MPI_Win win) ;
int PMPI_Win_flush_local_all(MPI_Win win) ;
int PMPI_Win_sync(MPI_Win win) ;


int PMPI_Add_error_class(int *errorclass) ;
int PMPI_Add_error_code(int errorclass, int *errorcode) ;
int PMPI_Add_error_string(int errorcode, const char *string) ;
int PMPI_Comm_call_errhandler(MPI_Comm comm, int errorcode) ;
int PMPI_Comm_create_keyval(MPI_Comm_copy_attr_function *comm_copy_attr_fn,
                            MPI_Comm_delete_attr_function *comm_delete_attr_fn, int *comm_keyval,
                            void *extra_state) ;
int PMPI_Comm_delete_attr(MPI_Comm comm, int comm_keyval) ;
int PMPI_Comm_free_keyval(int *comm_keyval) ;
int PMPI_Comm_get_attr(MPI_Comm comm, int comm_keyval, void *attribute_val, int *flag) ;
int PMPI_Comm_get_name(MPI_Comm comm, char *comm_name, int *resultlen) ;
int PMPI_Comm_set_attr(MPI_Comm comm, int comm_keyval, void *attribute_val) ;
int PMPI_Comm_set_name(MPI_Comm comm, const char *comm_name) ;
int PMPI_File_call_errhandler(MPI_File fh, int errorcode) ;
int PMPI_Grequest_complete(MPI_Request request) ;
int PMPI_Grequest_start(MPI_Grequest_query_function *query_fn, MPI_Grequest_free_function *free_fn,
                        MPI_Grequest_cancel_function *cancel_fn, void *extra_state,
                        MPI_Request *request) ;
int PMPI_Init_thread(int *argc, char ***argv, int required, int *provided) ;
int PMPI_Is_thread_main(int *flag) ;
int PMPI_Query_thread(int *provided) ;
int PMPI_Status_set_cancelled(MPI_Status *status, int flag) ;
int PMPI_Status_set_elements(MPI_Status *status, MPI_Datatype datatype, int count) ;
int PMPI_Type_create_keyval(MPI_Type_copy_attr_function *type_copy_attr_fn,
                            MPI_Type_delete_attr_function *type_delete_attr_fn,
                            int *type_keyval, void *extra_state) ;
int PMPI_Type_delete_attr(MPI_Datatype datatype, int type_keyval) ;
int PMPI_Type_dup(MPI_Datatype oldtype, MPI_Datatype *newtype) ;
int PMPI_Type_free_keyval(int *type_keyval) ;
int PMPI_Type_get_attr(MPI_Datatype datatype, int type_keyval, void *attribute_val, int *flag) ;
int PMPI_Type_get_contents(MPI_Datatype datatype, int max_integers, int max_addresses,
                           int max_datatypes, int array_of_integers[],
                           MPI_Aint array_of_addresses[], MPI_Datatype array_of_datatypes[]) ;
int PMPI_Type_get_envelope(MPI_Datatype datatype, int *num_integers, int *num_addresses,
                           int *num_datatypes, int *combiner) ;
int PMPI_Type_get_name(MPI_Datatype datatype, char *type_name, int *resultlen) ;
int PMPI_Type_set_attr(MPI_Datatype datatype, int type_keyval, void *attribute_val) ;
int PMPI_Type_set_name(MPI_Datatype datatype, const char *type_name) ;
int PMPI_Type_match_size(int typeclass, int size, MPI_Datatype *datatype) ;
int PMPI_Win_call_errhandler(MPI_Win win, int errorcode) ;
int PMPI_Win_create_keyval(MPI_Win_copy_attr_function *win_copy_attr_fn,
                           MPI_Win_delete_attr_function *win_delete_attr_fn, int *win_keyval,
                           void *extra_state) ;
int PMPI_Win_delete_attr(MPI_Win win, int win_keyval) ;
int PMPI_Win_free_keyval(int *win_keyval) ;
int PMPI_Win_get_attr(MPI_Win win, int win_keyval, void *attribute_val, int *flag) ;
int PMPI_Win_get_name(MPI_Win win, char *win_name, int *resultlen) ;
int PMPI_Win_set_attr(MPI_Win win, int win_keyval, void *attribute_val) ;
int PMPI_Win_set_name(MPI_Win win, const char *win_name) ;

int PMPI_Alloc_mem(MPI_Aint size, MPI_Info info, void *baseptr) ;
int PMPI_Comm_create_errhandler(MPI_Comm_errhandler_function *comm_errhandler_fn,
                                MPI_Errhandler *errhandler) ;
int PMPI_Comm_get_errhandler(MPI_Comm comm, MPI_Errhandler *errhandler) ;
int PMPI_Comm_set_errhandler(MPI_Comm comm, MPI_Errhandler errhandler) ;
int PMPI_File_create_errhandler(MPI_File_errhandler_function *file_errhandler_fn,
                                MPI_Errhandler *errhandler) ;
int PMPI_File_get_errhandler(MPI_File file, MPI_Errhandler *errhandler) ;
int PMPI_File_set_errhandler(MPI_File file, MPI_Errhandler errhandler) ;
int PMPI_Finalized(int *flag) ;
int PMPI_Free_mem(void *base) ;
int PMPI_Get_address(const void *location, MPI_Aint *address) ;
int PMPI_Info_create(MPI_Info *info) ;
int PMPI_Info_delete(MPI_Info info, const char *key) ;
int PMPI_Info_dup(MPI_Info info, MPI_Info *newinfo) ;
int PMPI_Info_free(MPI_Info *info) ;
int PMPI_Info_get(MPI_Info info, const char *key, int valuelen, char *value, int *flag) ;
int PMPI_Info_get_nkeys(MPI_Info info, int *nkeys) ;
int PMPI_Info_get_nthkey(MPI_Info info, int n, char *key) ;
int PMPI_Info_get_valuelen(MPI_Info info, const char *key, int *valuelen, int *flag) ;
int PMPI_Info_set(MPI_Info info, const char *key, const char *value) ;
int PMPI_Pack_external(const char datarep[], const void *inbuf, int incount,
                       MPI_Datatype datatype, void *outbuf, MPI_Aint outsize, MPI_Aint *position)
                                                                             ;
int PMPI_Pack_external_size(const char datarep[], int incount, MPI_Datatype datatype,
                            MPI_Aint *size) ;
int PMPI_Request_get_status(MPI_Request request, int *flag, MPI_Status *status) ;
int PMPI_Status_c2f(const MPI_Status *c_status, MPI_Fint *f_status) ;
int PMPI_Status_f2c(const MPI_Fint *f_status, MPI_Status *c_status) ;
int PMPI_Type_create_darray(int size, int rank, int ndims, const int array_of_gsizes[],
                            const int array_of_distribs[], const int array_of_dargs[],
                            const int array_of_psizes[], int order, MPI_Datatype oldtype,
                            MPI_Datatype *newtype) ;
int PMPI_Type_create_hindexed(int count, const int array_of_blocklengths[],
                              const MPI_Aint array_of_displacements[], MPI_Datatype oldtype,
                              MPI_Datatype *newtype) ;
int PMPI_Type_create_hvector(int count, int blocklength, MPI_Aint stride, MPI_Datatype oldtype,
                             MPI_Datatype *newtype) ;
int PMPI_Type_create_indexed_block(int count, int blocklength, const int array_of_displacements[],
                                   MPI_Datatype oldtype, MPI_Datatype *newtype) ;
int PMPI_Type_create_hindexed_block(int count, int blocklength,
                                    const MPI_Aint array_of_displacements[],
                                    MPI_Datatype oldtype, MPI_Datatype *newtype) ;
int PMPI_Type_create_resized(MPI_Datatype oldtype, MPI_Aint lb, MPI_Aint extent,
                             MPI_Datatype *newtype) ;
int PMPI_Type_create_struct(int count, const int array_of_blocklengths[],
                            const MPI_Aint array_of_displacements[],
                            const MPI_Datatype array_of_types[], MPI_Datatype *newtype) ;
int PMPI_Type_create_subarray(int ndims, const int array_of_sizes[],
                              const int array_of_subsizes[], const int array_of_starts[],
                              int order, MPI_Datatype oldtype, MPI_Datatype *newtype) ;
int PMPI_Type_get_extent(MPI_Datatype datatype, MPI_Aint *lb, MPI_Aint *extent) ;
int PMPI_Type_get_true_extent(MPI_Datatype datatype, MPI_Aint *true_lb, MPI_Aint *true_extent) ;
int PMPI_Unpack_external(const char datarep[], const void *inbuf, MPI_Aint insize,
                         MPI_Aint *position, void *outbuf, int outcount, MPI_Datatype datatype)
                                                                               ;
int PMPI_Win_create_errhandler(MPI_Win_errhandler_function *win_errhandler_fn,
                               MPI_Errhandler *errhandler) ;
int PMPI_Win_get_errhandler(MPI_Win win, MPI_Errhandler *errhandler) ;
int PMPI_Win_set_errhandler(MPI_Win win, MPI_Errhandler errhandler) ;




int PMPI_Type_create_f90_integer(int r, MPI_Datatype *newtype) ;
int PMPI_Type_create_f90_real(int p, int r, MPI_Datatype *newtype) ;
int PMPI_Type_create_f90_complex(int p, int r, MPI_Datatype *newtype) ;

int PMPI_Reduce_local(const void *inbuf, void *inoutbuf, int count, MPI_Datatype datatype,
                      MPI_Op op)
                                                                                                                  ;
int PMPI_Op_commutative(MPI_Op op, int *commute) ;
int PMPI_Reduce_scatter_block(const void *sendbuf, void *recvbuf, int recvcount,
                              MPI_Datatype datatype, MPI_Op op, MPI_Comm comm)

                                                                                    ;
int PMPI_Dist_graph_create_adjacent(MPI_Comm comm_old, int indegree, const int sources[],
                                    const int sourceweights[], int outdegree,
                                    const int destinations[], const int destweights[],
                                    MPI_Info info, int reorder, MPI_Comm *comm_dist_graph) ;
int PMPI_Dist_graph_create(MPI_Comm comm_old, int n, const int sources[], const int degrees[],
                           const int destinations[], const int weights[], MPI_Info info,
                           int reorder, MPI_Comm *comm_dist_graph) ;
int PMPI_Dist_graph_neighbors_count(MPI_Comm comm, int *indegree, int *outdegree, int *weighted) ;
int PMPI_Dist_graph_neighbors(MPI_Comm comm, int maxindegree, int sources[], int sourceweights[],
                              int maxoutdegree, int destinations[], int destweights[]) ;


int PMPI_Improbe(int source, int tag, MPI_Comm comm, int *flag, MPI_Message *message,
                 MPI_Status *status) ;
int PMPI_Imrecv(void *buf, int count, MPI_Datatype datatype, MPI_Message *message,
                MPI_Request *request) ;
int PMPI_Mprobe(int source, int tag, MPI_Comm comm, MPI_Message *message, MPI_Status *status) ;
int PMPI_Mrecv(void *buf, int count, MPI_Datatype datatype, MPI_Message *message,
               MPI_Status *status) ;


int PMPI_Comm_idup(MPI_Comm comm, MPI_Comm *newcomm, MPI_Request *request) ;
int PMPI_Ibarrier(MPI_Comm comm, MPI_Request *request) ;
int PMPI_Ibcast(void *buffer, int count, MPI_Datatype datatype, int root, MPI_Comm comm,
                MPI_Request *request) ;
int PMPI_Igather(const void *sendbuf, int sendcount, MPI_Datatype sendtype, void *recvbuf,
                 int recvcount, MPI_Datatype recvtype, int root, MPI_Comm comm,
                 MPI_Request *request)
                                                                                                             ;
int PMPI_Igatherv(const void *sendbuf, int sendcount, MPI_Datatype sendtype, void *recvbuf,
                  const int recvcounts[], const int displs[], MPI_Datatype recvtype, int root,
                  MPI_Comm comm, MPI_Request *request)
                                                                                                              ;
int PMPI_Iscatter(const void *sendbuf, int sendcount, MPI_Datatype sendtype, void *recvbuf,
                  int recvcount, MPI_Datatype recvtype, int root, MPI_Comm comm,
                  MPI_Request *request)
                                                                                                              ;
int PMPI_Iscatterv(const void *sendbuf, const int sendcounts[], const int displs[],
                   MPI_Datatype sendtype, void *recvbuf, int recvcount, MPI_Datatype recvtype,
                   int root, MPI_Comm comm, MPI_Request *request)
                                                                                                               ;
int PMPI_Iallgather(const void *sendbuf, int sendcount, MPI_Datatype sendtype, void *recvbuf,
                    int recvcount, MPI_Datatype recvtype, MPI_Comm comm, MPI_Request *request)
                                                                                                                ;
int PMPI_Iallgatherv(const void *sendbuf, int sendcount, MPI_Datatype sendtype, void *recvbuf,
                     const int recvcounts[], const int displs[], MPI_Datatype recvtype,
                     MPI_Comm comm, MPI_Request *request)
                                                                                                                 ;
int PMPI_Ialltoall(const void *sendbuf, int sendcount, MPI_Datatype sendtype, void *recvbuf,
                   int recvcount, MPI_Datatype recvtype, MPI_Comm comm, MPI_Request *request)
                                                                                                               ;
int PMPI_Ialltoallv(const void *sendbuf, const int sendcounts[], const int sdispls[],
                    MPI_Datatype sendtype, void *recvbuf, const int recvcounts[],
                    const int rdispls[], MPI_Datatype recvtype, MPI_Comm comm,
                    MPI_Request *request)
                                                                                                                ;
int PMPI_Ialltoallw(const void *sendbuf, const int sendcounts[], const int sdispls[],
                    const MPI_Datatype sendtypes[], void *recvbuf, const int recvcounts[],
                    const int rdispls[], const MPI_Datatype recvtypes[], MPI_Comm comm,
                    MPI_Request *request) ;
int PMPI_Ireduce(const void *sendbuf, void *recvbuf, int count, MPI_Datatype datatype,
                 MPI_Op op, int root, MPI_Comm comm, MPI_Request *request)
                                                                                                             ;
int PMPI_Iallreduce(const void *sendbuf, void *recvbuf, int count, MPI_Datatype datatype,
                    MPI_Op op, MPI_Comm comm, MPI_Request *request)
                                                                                                                ;
int PMPI_Ireduce_scatter(const void *sendbuf, void *recvbuf, const int recvcounts[],
                         MPI_Datatype datatype, MPI_Op op, MPI_Comm comm, MPI_Request *request)

                                                                               ;
int PMPI_Ireduce_scatter_block(const void *sendbuf, void *recvbuf, int recvcount,
                               MPI_Datatype datatype, MPI_Op op, MPI_Comm comm,
                               MPI_Request *request)

                                                                                     ;
int PMPI_Iscan(const void *sendbuf, void *recvbuf, int count, MPI_Datatype datatype, MPI_Op op,
               MPI_Comm comm, MPI_Request *request)
                                                                                                           ;
int PMPI_Iexscan(const void *sendbuf, void *recvbuf, int count, MPI_Datatype datatype,
                 MPI_Op op, MPI_Comm comm, MPI_Request *request)
                                                                                                             ;


int PMPI_Ineighbor_allgather(const void *sendbuf, int sendcount, MPI_Datatype sendtype,
                             void *recvbuf, int recvcount, MPI_Datatype recvtype,
                             MPI_Comm comm, MPI_Request *request)

                                                                                   ;
int PMPI_Ineighbor_allgatherv(const void *sendbuf, int sendcount, MPI_Datatype sendtype,
                              void *recvbuf, const int recvcounts[], const int displs[],
                              MPI_Datatype recvtype, MPI_Comm comm, MPI_Request *request)

                                                                                    ;
int PMPI_Ineighbor_alltoall(const void *sendbuf, int sendcount, MPI_Datatype sendtype,
                            void *recvbuf, int recvcount, MPI_Datatype recvtype, MPI_Comm comm,
                            MPI_Request *request)

                                                                                  ;
int PMPI_Ineighbor_alltoallv(const void *sendbuf, const int sendcounts[], const int sdispls[],
                             MPI_Datatype sendtype, void *recvbuf, const int recvcounts[],
                             const int rdispls[], MPI_Datatype recvtype, MPI_Comm comm,
                             MPI_Request *request)

                                                                                   ;
int PMPI_Ineighbor_alltoallw(const void *sendbuf, const int sendcounts[],
                             const MPI_Aint sdispls[], const MPI_Datatype sendtypes[],
                             void *recvbuf, const int recvcounts[], const MPI_Aint rdispls[],
                             const MPI_Datatype recvtypes[], MPI_Comm comm, MPI_Request *request) ;
int PMPI_Neighbor_allgather(const void *sendbuf, int sendcount, MPI_Datatype sendtype,
                            void *recvbuf, int recvcount, MPI_Datatype recvtype, MPI_Comm comm)

                                                                                  ;
int PMPI_Neighbor_allgatherv(const void *sendbuf, int sendcount, MPI_Datatype sendtype,
                             void *recvbuf, const int recvcounts[], const int displs[],
                             MPI_Datatype recvtype, MPI_Comm comm)

                                                                                   ;
int PMPI_Neighbor_alltoall(const void *sendbuf, int sendcount, MPI_Datatype sendtype,
                           void *recvbuf, int recvcount, MPI_Datatype recvtype, MPI_Comm comm)

                                                                                 ;
int PMPI_Neighbor_alltoallv(const void *sendbuf, const int sendcounts[], const int sdispls[],
                            MPI_Datatype sendtype, void *recvbuf, const int recvcounts[],
                            const int rdispls[], MPI_Datatype recvtype, MPI_Comm comm)

                                                                                  ;
int PMPI_Neighbor_alltoallw(const void *sendbuf, const int sendcounts[], const MPI_Aint sdispls[],
                            const MPI_Datatype sendtypes[], void *recvbuf, const int recvcounts[],
                            const MPI_Aint rdispls[], const MPI_Datatype recvtypes[],
                            MPI_Comm comm) ;


int PMPI_Comm_split_type(MPI_Comm comm, int split_type, int key, MPI_Info info, MPI_Comm *newcomm) ;


int PMPI_Comm_create_group(MPI_Comm comm, MPI_Group group, int tag, MPI_Comm *newcomm) ;


int PMPI_Get_elements_x(const MPI_Status *status, MPI_Datatype datatype, MPI_Count *count) ;
int PMPI_Status_set_elements_x(MPI_Status *status, MPI_Datatype datatype, MPI_Count count) ;
int PMPI_Type_get_extent_x(MPI_Datatype datatype, MPI_Count *lb, MPI_Count *extent) ;
int PMPI_Type_get_true_extent_x(MPI_Datatype datatype, MPI_Count *lb, MPI_Count *extent) ;
int PMPI_Type_size_x(MPI_Datatype datatype, MPI_Count *size) ;


MPI_Aint PMPI_Aint_add(MPI_Aint base, MPI_Aint disp) ;
MPI_Aint PMPI_Aint_diff(MPI_Aint addr1, MPI_Aint addr2) ;





int PMPI_T_init_thread(int required, int *provided) ;
int PMPI_T_finalize(void) ;
int PMPI_T_enum_get_info(MPI_T_enum enumtype, int *num, char *name, int *name_len) ;
int PMPI_T_enum_get_item(MPI_T_enum enumtype, int indx, int *value, char *name, int *name_len) ;
int PMPI_T_cvar_get_num(int *num_cvar) ;
int PMPI_T_cvar_get_info(int cvar_index, char *name, int *name_len, int *verbosity,
                         MPI_Datatype *datatype, MPI_T_enum *enumtype, char *desc, int *desc_len,
                         int *binding, int *scope) ;
int PMPI_T_cvar_handle_alloc(int cvar_index, void *obj_handle, MPI_T_cvar_handle *handle,
                             int *count) ;
int PMPI_T_cvar_handle_free(MPI_T_cvar_handle *handle) ;
int PMPI_T_cvar_read(MPI_T_cvar_handle handle, void *buf) ;
int PMPI_T_cvar_write(MPI_T_cvar_handle handle, const void *buf) ;
int PMPI_T_pvar_get_num(int *num_pvar) ;
int PMPI_T_pvar_get_info(int pvar_index, char *name, int *name_len, int *verbosity, int *var_class,
                         MPI_Datatype *datatype, MPI_T_enum *enumtype, char *desc, int *desc_len,
                         int *binding, int *readonly, int *continuous, int *atomic) ;
int PMPI_T_pvar_session_create(MPI_T_pvar_session *session) ;
int PMPI_T_pvar_session_free(MPI_T_pvar_session *session) ;
int PMPI_T_pvar_handle_alloc(MPI_T_pvar_session session, int pvar_index, void *obj_handle,
                             MPI_T_pvar_handle *handle, int *count) ;
int PMPI_T_pvar_handle_free(MPI_T_pvar_session session, MPI_T_pvar_handle *handle) ;
int PMPI_T_pvar_start(MPI_T_pvar_session session, MPI_T_pvar_handle handle) ;
int PMPI_T_pvar_stop(MPI_T_pvar_session session, MPI_T_pvar_handle handle) ;
int PMPI_T_pvar_read(MPI_T_pvar_session session, MPI_T_pvar_handle handle, void *buf) ;
int PMPI_T_pvar_write(MPI_T_pvar_session session, MPI_T_pvar_handle handle, const void *buf) ;
int PMPI_T_pvar_reset(MPI_T_pvar_session session, MPI_T_pvar_handle handle) ;
int PMPI_T_pvar_readreset(MPI_T_pvar_session session, MPI_T_pvar_handle handle, void *buf) ;
int PMPI_T_category_get_num(int *num_cat) ;
int PMPI_T_category_get_info(int cat_index, char *name, int *name_len, char *desc, int *desc_len,
                             int *num_cvars, int *num_pvars, int *num_categories) ;
int PMPI_T_category_get_cvars(int cat_index, int len, int indices[]) ;
int PMPI_T_category_get_pvars(int cat_index, int len, int indices[]) ;
int PMPI_T_category_get_categories(int cat_index, int len, int indices[]) ;
int PMPI_T_category_changed(int *stamp) ;
int PMPI_T_cvar_get_index(const char *name, int *cvar_index) ;
int PMPI_T_pvar_get_index(const char *name, int var_class, int *pvar_index) ;
int PMPI_T_category_get_index(const char *name, int *cat_index) ;





int PMPIX_Comm_failure_ack(MPI_Comm comm) ;
int PMPIX_Comm_failure_get_acked(MPI_Comm comm, MPI_Group *failedgrp) ;
int PMPIX_Comm_revoke(MPI_Comm comm) ;
int PMPIX_Comm_shrink(MPI_Comm comm, MPI_Comm *newcomm) ;
int PMPIX_Comm_agree(MPI_Comm comm, int *flag) ;


int MPI_Allgather_c(const void *sendbuf, MPI_Count sendcount, MPI_Datatype sendtype, void *recvbuf,
                    MPI_Count recvcount, MPI_Datatype recvtype, MPI_Comm comm)
                                                                                                                ;
int MPI_Allreduce_c(const void *sendbuf, void *recvbuf, MPI_Count count, MPI_Datatype datatype,
                    MPI_Op op, MPI_Comm comm)
                                                                                                                ;
int MPI_Alltoall_c(const void *sendbuf, MPI_Count sendcount, MPI_Datatype sendtype, void *recvbuf,
                   MPI_Count recvcount, MPI_Datatype recvtype, MPI_Comm comm)
                                                                                                               ;
int MPI_Bcast_c(void *buffer, MPI_Count count, MPI_Datatype datatype, int root, MPI_Comm comm)
                                                          ;
int MPI_Exscan_c(const void *sendbuf, void *recvbuf, MPI_Count count, MPI_Datatype datatype,
                 MPI_Op op, MPI_Comm comm)
                                                                                                             ;
int MPI_Gather_c(const void *sendbuf, MPI_Count sendcount, MPI_Datatype sendtype, void *recvbuf,
                 MPI_Count recvcount, MPI_Datatype recvtype, int root, MPI_Comm comm)
                                                                                                             ;
int MPI_Iallgather_c(const void *sendbuf, MPI_Count sendcount, MPI_Datatype sendtype, void *recvbuf,
                     MPI_Count recvcount, MPI_Datatype recvtype, MPI_Comm comm,
                     MPI_Request *request)
                                                                                                                 ;
int MPI_Iallreduce_c(const void *sendbuf, void *recvbuf, MPI_Count count, MPI_Datatype datatype,
                     MPI_Op op, MPI_Comm comm, MPI_Request *request)
                                                                                                                 ;
int MPI_Ialltoall_c(const void *sendbuf, MPI_Count sendcount, MPI_Datatype sendtype, void *recvbuf,
                    MPI_Count recvcount, MPI_Datatype recvtype, MPI_Comm comm,
                    MPI_Request *request)
                                                                                                                ;
int MPI_Ibcast_c(void *buffer, MPI_Count count, MPI_Datatype datatype, int root, MPI_Comm comm,
                 MPI_Request *request) ;
int MPI_Iexscan_c(const void *sendbuf, void *recvbuf, MPI_Count count, MPI_Datatype datatype,
                  MPI_Op op, MPI_Comm comm, MPI_Request *request)
                                                                                                              ;
int MPI_Igather_c(const void *sendbuf, MPI_Count sendcount, MPI_Datatype sendtype, void *recvbuf,
                  MPI_Count recvcount, MPI_Datatype recvtype, int root, MPI_Comm comm,
                  MPI_Request *request)
                                                                                                              ;
int MPI_Ineighbor_allgather_c(const void *sendbuf, MPI_Count sendcount, MPI_Datatype sendtype,
                              void *recvbuf, MPI_Count recvcount, MPI_Datatype recvtype,
                              MPI_Comm comm, MPI_Request *request)
                                                                                                                          ;
int MPI_Ineighbor_alltoall_c(const void *sendbuf, MPI_Count sendcount, MPI_Datatype sendtype,
                             void *recvbuf, MPI_Count recvcount, MPI_Datatype recvtype,
                             MPI_Comm comm, MPI_Request *request)
                                                                                                                         ;
int MPI_Ireduce_c(const void *sendbuf, void *recvbuf, MPI_Count count, MPI_Datatype datatype,
                  MPI_Op op, int root, MPI_Comm comm, MPI_Request *request)
                                                                                                              ;
int MPI_Ireduce_scatter_block_c(const void *sendbuf, void *recvbuf, MPI_Count recvcount,
                                MPI_Datatype datatype, MPI_Op op, MPI_Comm comm,
                                MPI_Request *request)
                                                                                                                            ;
int MPI_Iscan_c(const void *sendbuf, void *recvbuf, MPI_Count count, MPI_Datatype datatype,
                MPI_Op op, MPI_Comm comm, MPI_Request *request)
                                                                                                            ;
int MPI_Iscatter_c(const void *sendbuf, MPI_Count sendcount, MPI_Datatype sendtype, void *recvbuf,
                   MPI_Count recvcount, MPI_Datatype recvtype, int root, MPI_Comm comm,
                   MPI_Request *request)
                                                                                                               ;
int MPI_Neighbor_allgather_c(const void *sendbuf, MPI_Count sendcount, MPI_Datatype sendtype,
                             void *recvbuf, MPI_Count recvcount, MPI_Datatype recvtype,
                             MPI_Comm comm)
                                                                                                                         ;
int MPI_Neighbor_alltoall_c(const void *sendbuf, MPI_Count sendcount, MPI_Datatype sendtype,
                            void *recvbuf, MPI_Count recvcount, MPI_Datatype recvtype,
                            MPI_Comm comm)
                                                                                                                        ;
int MPI_Reduce_c(const void *sendbuf, void *recvbuf, MPI_Count count, MPI_Datatype datatype,
                 MPI_Op op, int root, MPI_Comm comm)
                                                                                                             ;
int MPI_Reduce_scatter_block_c(const void *sendbuf, void *recvbuf, MPI_Count recvcount,
                               MPI_Datatype datatype, MPI_Op op, MPI_Comm comm)
                                                                                                                           ;
int MPI_Scan_c(const void *sendbuf, void *recvbuf, MPI_Count count, MPI_Datatype datatype,
               MPI_Op op, MPI_Comm comm)
                                                                                                           ;
int MPI_Scatter_c(const void *sendbuf, MPI_Count sendcount, MPI_Datatype sendtype, void *recvbuf,
                  MPI_Count recvcount, MPI_Datatype recvtype, int root, MPI_Comm comm)
                                                                                                              ;



int PMPI_Allgather_c(const void *sendbuf, MPI_Count sendcount, MPI_Datatype sendtype, void *recvbuf,
                    MPI_Count recvcount, MPI_Datatype recvtype, MPI_Comm comm)
                                                                                                                ;
int PMPI_Allreduce_c(const void *sendbuf, void *recvbuf, MPI_Count count, MPI_Datatype datatype,
                    MPI_Op op, MPI_Comm comm)
                                                                                                                ;
int PMPI_Alltoall_c(const void *sendbuf, MPI_Count sendcount, MPI_Datatype sendtype, void *recvbuf,
                   MPI_Count recvcount, MPI_Datatype recvtype, MPI_Comm comm)
                                                                                                               ;
int PMPI_Bcast_c(void *buffer, MPI_Count count, MPI_Datatype datatype, int root, MPI_Comm comm)
                                                          ;
int PMPI_Exscan_c(const void *sendbuf, void *recvbuf, MPI_Count count, MPI_Datatype datatype,
                 MPI_Op op, MPI_Comm comm)
                                                                                                             ;
int PMPI_Gather_c(const void *sendbuf, MPI_Count sendcount, MPI_Datatype sendtype, void *recvbuf,
                 MPI_Count recvcount, MPI_Datatype recvtype, int root, MPI_Comm comm)
                                                                                                             ;
int PMPI_Iallgather_c(const void *sendbuf, MPI_Count sendcount, MPI_Datatype sendtype, void *recvbuf,
                     MPI_Count recvcount, MPI_Datatype recvtype, MPI_Comm comm,
                     MPI_Request *request)
                                                                                                                 ;
int PMPI_Iallreduce_c(const void *sendbuf, void *recvbuf, MPI_Count count, MPI_Datatype datatype,
                     MPI_Op op, MPI_Comm comm, MPI_Request *request)
                                                                                                                 ;
int PMPI_Ialltoall_c(const void *sendbuf, MPI_Count sendcount, MPI_Datatype sendtype, void *recvbuf,
                    MPI_Count recvcount, MPI_Datatype recvtype, MPI_Comm comm,
                    MPI_Request *request)
                                                                                                                ;
int PMPI_Ibcast_c(void *buffer, MPI_Count count, MPI_Datatype datatype, int root, MPI_Comm comm,
                 MPI_Request *request) ;
int PMPI_Iexscan_c(const void *sendbuf, void *recvbuf, MPI_Count count, MPI_Datatype datatype,
                  MPI_Op op, MPI_Comm comm, MPI_Request *request)
                                                                                                              ;
int PMPI_Igather_c(const void *sendbuf, MPI_Count sendcount, MPI_Datatype sendtype, void *recvbuf,
                  MPI_Count recvcount, MPI_Datatype recvtype, int root, MPI_Comm comm,
                  MPI_Request *request)
                                                                                                              ;
int PMPI_Ineighbor_allgather_c(const void *sendbuf, MPI_Count sendcount, MPI_Datatype sendtype,
                              void *recvbuf, MPI_Count recvcount, MPI_Datatype recvtype,
                              MPI_Comm comm, MPI_Request *request)
                                                                                                                          ;
int PMPI_Ineighbor_alltoall_c(const void *sendbuf, MPI_Count sendcount, MPI_Datatype sendtype,
                             void *recvbuf, MPI_Count recvcount, MPI_Datatype recvtype,
                             MPI_Comm comm, MPI_Request *request)
                                                                                                                         ;
int PMPI_Ireduce_c(const void *sendbuf, void *recvbuf, MPI_Count count, MPI_Datatype datatype,
                  MPI_Op op, int root, MPI_Comm comm, MPI_Request *request)
                                                                                                              ;
int PMPI_Ireduce_scatter_block_c(const void *sendbuf, void *recvbuf, MPI_Count recvcount,
                                MPI_Datatype datatype, MPI_Op op, MPI_Comm comm,
                                MPI_Request *request)
                                                                                                                            ;
int PMPI_Iscan_c(const void *sendbuf, void *recvbuf, MPI_Count count, MPI_Datatype datatype,
                MPI_Op op, MPI_Comm comm, MPI_Request *request)
                                                                                                            ;
int PMPI_Iscatter_c(const void *sendbuf, MPI_Count sendcount, MPI_Datatype sendtype, void *recvbuf,
                   MPI_Count recvcount, MPI_Datatype recvtype, int root, MPI_Comm comm,
                   MPI_Request *request)
                                                                                                               ;
int PMPI_Neighbor_allgather_c(const void *sendbuf, MPI_Count sendcount, MPI_Datatype sendtype,
                             void *recvbuf, MPI_Count recvcount, MPI_Datatype recvtype,
                             MPI_Comm comm)
                                                                                                                         ;
int PMPI_Neighbor_alltoall_c(const void *sendbuf, MPI_Count sendcount, MPI_Datatype sendtype,
                            void *recvbuf, MPI_Count recvcount, MPI_Datatype recvtype,
                            MPI_Comm comm)
                                                                                                                        ;
int PMPI_Reduce_c(const void *sendbuf, void *recvbuf, MPI_Count count, MPI_Datatype datatype,
                 MPI_Op op, int root, MPI_Comm comm)
                                                                                                             ;
int PMPI_Reduce_scatter_block_c(const void *sendbuf, void *recvbuf, MPI_Count recvcount,
                               MPI_Datatype datatype, MPI_Op op, MPI_Comm comm)
                                                                                                                           ;
int PMPI_Scan_c(const void *sendbuf, void *recvbuf, MPI_Count count, MPI_Datatype datatype,
               MPI_Op op, MPI_Comm comm)
                                                                                                           ;
int PMPI_Scatter_c(const void *sendbuf, MPI_Count sendcount, MPI_Datatype sendtype, void *recvbuf,
                  MPI_Count recvcount, MPI_Datatype recvtype, int root, MPI_Comm comm)
                                                                                                              ;
# 2474 "/cvmfs/software.hpc.rwth.de/Linux/RH8/x86_64/intel/sapphirerapids/software/impi/2021.9.0-intel-compilers-2023.1.0/mpi/2021.9.0/include/mpi.h"
# 1 "/cvmfs/software.hpc.rwth.de/Linux/RH8/x86_64/intel/sapphirerapids/software/impi/2021.9.0-intel-compilers-2023.1.0/mpi/2021.9.0/include/mpio.h" 1
# 78 "/cvmfs/software.hpc.rwth.de/Linux/RH8/x86_64/intel/sapphirerapids/software/impi/2021.9.0-intel-compilers-2023.1.0/mpi/2021.9.0/include/mpio.h"
# 1 "/cvmfs/software.hpc.rwth.de/Linux/RH8/x86_64/intel/sapphirerapids/software/impi/2021.9.0-intel-compilers-2023.1.0/mpi/2021.9.0/include/mpi.h" 1
# 79 "/cvmfs/software.hpc.rwth.de/Linux/RH8/x86_64/intel/sapphirerapids/software/impi/2021.9.0-intel-compilers-2023.1.0/mpi/2021.9.0/include/mpio.h" 2








extern "C" {
# 204 "/cvmfs/software.hpc.rwth.de/Linux/RH8/x86_64/intel/sapphirerapids/software/impi/2021.9.0-intel-compilers-2023.1.0/mpi/2021.9.0/include/mpio.h"
int MPI_File_open(MPI_Comm comm, const char *filename, int amode, MPI_Info info, MPI_File *fh) ;
int MPI_File_close(MPI_File *fh) ;
int MPI_File_delete(const char *filename, MPI_Info info) ;
int MPI_File_set_size(MPI_File fh, MPI_Offset size) ;
int MPI_File_preallocate(MPI_File fh, MPI_Offset size) ;
int MPI_File_get_size(MPI_File fh, MPI_Offset *size) ;
int MPI_File_get_group(MPI_File fh, MPI_Group *group) ;
int MPI_File_get_amode(MPI_File fh, int *amode) ;
int MPI_File_set_info(MPI_File fh, MPI_Info info) ;
int MPI_File_get_info(MPI_File fh, MPI_Info *info_used) ;


int MPI_File_set_view(MPI_File fh, MPI_Offset disp, MPI_Datatype etype, MPI_Datatype filetype,
                      const char *datarep, MPI_Info info) ;
int MPI_File_get_view(MPI_File fh, MPI_Offset *disp, MPI_Datatype *etype, MPI_Datatype *filetype,
                      char *datarep) ;


int MPI_File_read_at(MPI_File fh, MPI_Offset offset, void *buf, int count, MPI_Datatype datatype,
                     MPI_Status *status) ;
int MPI_File_read_at_all(MPI_File fh, MPI_Offset offset, void * buf, int count,
                         MPI_Datatype datatype, MPI_Status *status)
                                                          ;
int MPI_File_write_at(MPI_File fh, MPI_Offset offset, const void * buf, int count,
                      MPI_Datatype datatype, MPI_Status *status)
                                                          ;
int MPI_File_write_at_all(MPI_File fh, MPI_Offset offset, const void *buf, int count,
                          MPI_Datatype datatype, MPI_Status *status)
                                                          ;




int MPI_File_iread_at(MPI_File fh, MPI_Offset offset, void *buf, int count, MPI_Datatype datatype,
                      MPI_Request *request) ;
int MPI_File_iwrite_at(MPI_File fh, MPI_Offset offset, const void *buf, int count,
                       MPI_Datatype datatype, MPI_Request *request)
                                                          ;


int MPI_File_read(MPI_File fh, void *buf, int count, MPI_Datatype datatype, MPI_Status *status)
                                                          ;
int MPI_File_read_all(MPI_File fh, void *buf, int count, MPI_Datatype datatype, MPI_Status *status)
                                                          ;
int MPI_File_write(MPI_File fh, const void *buf, int count, MPI_Datatype datatype,
                   MPI_Status *status) ;
int MPI_File_write_all(MPI_File fh, const void *buf, int count, MPI_Datatype datatype,
                       MPI_Status *status) ;





int MPI_File_iread(MPI_File fh, void *buf, int count, MPI_Datatype datatype, MPI_Request *request)
                                                          ;
int MPI_File_iwrite(MPI_File fh, const void *buf, int count, MPI_Datatype datatype,
                    MPI_Request *request) ;

int MPI_File_seek(MPI_File fh, MPI_Offset offset, int whence) ;
int MPI_File_get_position(MPI_File fh, MPI_Offset *offset) ;
int MPI_File_get_byte_offset(MPI_File fh, MPI_Offset offset, MPI_Offset *disp) ;


int MPI_File_read_shared(MPI_File fh, void *buf, int count, MPI_Datatype datatype,
                         MPI_Status *status) ;
int MPI_File_write_shared(MPI_File fh, const void *buf, int count, MPI_Datatype datatype,
                          MPI_Status *status) ;
int MPI_File_iread_shared(MPI_File fh, void *buf, int count, MPI_Datatype datatype,
                          MPI_Request *request) ;
int MPI_File_iwrite_shared(MPI_File fh, const void *buf, int count, MPI_Datatype datatype,
                           MPI_Request *request) ;
int MPI_File_read_ordered(MPI_File fh, void *buf, int count, MPI_Datatype datatype,
                          MPI_Status *status) ;
int MPI_File_write_ordered(MPI_File fh, const void *buf, int count, MPI_Datatype datatype,
                           MPI_Status *status) ;
int MPI_File_seek_shared(MPI_File fh, MPI_Offset offset, int whence) ;
int MPI_File_get_position_shared(MPI_File fh, MPI_Offset *offset) ;


int MPI_File_read_at_all_begin(MPI_File fh, MPI_Offset offset, void *buf, int count,
                               MPI_Datatype datatype) ;
int MPI_File_read_at_all_end(MPI_File fh, void *buf, MPI_Status *status) ;
int MPI_File_write_at_all_begin(MPI_File fh, MPI_Offset offset, const void *buf, int count,
                                MPI_Datatype datatype) ;
int MPI_File_write_at_all_end(MPI_File fh, const void *buf, MPI_Status *status) ;
int MPI_File_read_all_begin(MPI_File fh, void *buf, int count, MPI_Datatype datatype)
                                                          ;
int MPI_File_read_all_end(MPI_File fh, void *buf, MPI_Status *status) ;
int MPI_File_write_all_begin(MPI_File fh, const void *buf, int count, MPI_Datatype datatype)
                                                          ;
int MPI_File_write_all_end(MPI_File fh, const void *buf, MPI_Status *status) ;
int MPI_File_read_ordered_begin(MPI_File fh, void *buf, int count, MPI_Datatype datatype)
                                                          ;
int MPI_File_read_ordered_end(MPI_File fh, void *buf, MPI_Status *status) ;
int MPI_File_write_ordered_begin(MPI_File fh, const void *buf, int count, MPI_Datatype datatype)
                                                          ;
int MPI_File_write_ordered_end(MPI_File fh, const void *buf, MPI_Status *status) ;


int MPI_File_get_type_extent(MPI_File fh, MPI_Datatype datatype, MPI_Aint *extent) ;


int MPI_Register_datarep(const char *datarep, MPI_Datarep_conversion_function *read_conversion_fn,
    MPI_Datarep_conversion_function *write_conversion_fn,
    MPI_Datarep_extent_function *dtype_file_extent_fn, void *extra_state) ;


int MPI_File_set_atomicity(MPI_File fh, int flag) ;
int MPI_File_get_atomicity(MPI_File fh, int *flag) ;
int MPI_File_sync(MPI_File fh) ;
# 323 "/cvmfs/software.hpc.rwth.de/Linux/RH8/x86_64/intel/sapphirerapids/software/impi/2021.9.0-intel-compilers-2023.1.0/mpi/2021.9.0/include/mpio.h"
int MPI_File_iread_at_all(MPI_File fh, MPI_Offset offset, void *buf, int count,
                           MPI_Datatype datatype, MPI_Request *request)
                                                          ;
int MPI_File_iwrite_at_all(MPI_File fh, MPI_Offset offset, const void *buf, int count,
                            MPI_Datatype datatype, MPI_Request *request)
                                                          ;
int MPI_File_iread_all(MPI_File fh, void *buf, int count, MPI_Datatype datatype,
                        MPI_Request *request)
                                                          ;
int MPI_File_iwrite_all(MPI_File fh, const void *buf, int count, MPI_Datatype datatype,
                         MPI_Request *request)
                                                          ;
# 363 "/cvmfs/software.hpc.rwth.de/Linux/RH8/x86_64/intel/sapphirerapids/software/impi/2021.9.0-intel-compilers-2023.1.0/mpi/2021.9.0/include/mpio.h"
MPI_File MPI_File_f2c(MPI_Fint file) ;
MPI_Fint MPI_File_c2f(MPI_File file) ;
# 424 "/cvmfs/software.hpc.rwth.de/Linux/RH8/x86_64/intel/sapphirerapids/software/impi/2021.9.0-intel-compilers-2023.1.0/mpi/2021.9.0/include/mpio.h"
int PMPI_File_open(MPI_Comm, const char *, int, MPI_Info, MPI_File *) ;
int PMPI_File_close(MPI_File *) ;
int PMPI_File_delete(const char *, MPI_Info) ;
int PMPI_File_set_size(MPI_File, MPI_Offset) ;
int PMPI_File_preallocate(MPI_File, MPI_Offset) ;
int PMPI_File_get_size(MPI_File, MPI_Offset *) ;
int PMPI_File_get_group(MPI_File, MPI_Group *) ;
int PMPI_File_get_amode(MPI_File, int *) ;
int PMPI_File_set_info(MPI_File, MPI_Info) ;
int PMPI_File_get_info(MPI_File, MPI_Info *) ;


int PMPI_File_set_view(MPI_File, MPI_Offset,
    MPI_Datatype, MPI_Datatype, const char *, MPI_Info) ;
int PMPI_File_get_view(MPI_File, MPI_Offset *,
      MPI_Datatype *, MPI_Datatype *, char *) ;


int PMPI_File_read_at(MPI_File, MPI_Offset, void *,
       int, MPI_Datatype, MPI_Status *)
                                                                    ;
int PMPI_File_read_at_all(MPI_File, MPI_Offset, void *,
       int, MPI_Datatype, MPI_Status *)
                                                                    ;
int PMPI_File_write_at(MPI_File, MPI_Offset, const void *,
       int, MPI_Datatype, MPI_Status *)
                                                                    ;
int PMPI_File_write_at_all(MPI_File, MPI_Offset, const void *,
       int, MPI_Datatype, MPI_Status *)
                                                                    ;





int PMPI_File_iread_at(MPI_File, MPI_Offset, void *,
       int, MPI_Datatype, MPI_Request *)
                                                                    ;
int PMPI_File_iwrite_at(MPI_File, MPI_Offset, const void *,
       int, MPI_Datatype, MPI_Request *)
                                                                    ;


int PMPI_File_read(MPI_File, void *, int, MPI_Datatype, MPI_Status *)
                                                                         ;
int PMPI_File_read_all(MPI_File, void *, int, MPI_Datatype, MPI_Status *)
                                                                             ;
int PMPI_File_write(MPI_File, const void *, int, MPI_Datatype, MPI_Status *)
                                                                          ;
int PMPI_File_write_all(MPI_File, const void *, int, MPI_Datatype, MPI_Status *)
                                                                              ;





int PMPI_File_iread(MPI_File, void *, int, MPI_Datatype, MPI_Request *)
                                                                          ;
int PMPI_File_iwrite(MPI_File, const void *, int, MPI_Datatype, MPI_Request *)
                                                                           ;

int PMPI_File_seek(MPI_File, MPI_Offset, int) ;
int PMPI_File_get_position(MPI_File, MPI_Offset *) ;
int PMPI_File_get_byte_offset(MPI_File, MPI_Offset, MPI_Offset *) ;


int PMPI_File_read_shared(MPI_File, void *, int, MPI_Datatype, MPI_Status *)
                                                                                ;
int PMPI_File_write_shared(MPI_File, const void *, int, MPI_Datatype, MPI_Status *)
                                                                                 ;
int PMPI_File_iread_shared(MPI_File, void *, int,
      MPI_Datatype, MPI_Request *)
                                                                                 ;
int PMPI_File_iwrite_shared(MPI_File, const void *, int,
       MPI_Datatype, MPI_Request *)
                                                                                  ;
int PMPI_File_read_ordered(MPI_File, void *, int, MPI_Datatype, MPI_Status *)
                                                                                 ;
int PMPI_File_write_ordered(MPI_File, const void *, int, MPI_Datatype, MPI_Status *)
                                                                                  ;
int PMPI_File_seek_shared(MPI_File, MPI_Offset, int) ;
int PMPI_File_get_position_shared(MPI_File, MPI_Offset *) ;


int PMPI_File_read_at_all_begin(MPI_File, MPI_Offset, void *,
                               int, MPI_Datatype)
                                                                                     ;
int PMPI_File_read_at_all_end(MPI_File, void *, MPI_Status *) ;
int PMPI_File_write_at_all_begin(MPI_File, MPI_Offset, const void *,
                                 int, MPI_Datatype)
                                                                                       ;
int PMPI_File_write_at_all_end(MPI_File, const void *, MPI_Status *) ;
int PMPI_File_read_all_begin(MPI_File, void *, int, MPI_Datatype)
                                                                                   ;
int PMPI_File_read_all_end(MPI_File, void *, MPI_Status *) ;
int PMPI_File_write_all_begin(MPI_File, const void *, int, MPI_Datatype)
                                                                                    ;
int PMPI_File_write_all_end(MPI_File, const void *, MPI_Status *) ;
int PMPI_File_read_ordered_begin(MPI_File, void *, int, MPI_Datatype)
                                                                                       ;
int PMPI_File_read_ordered_end(MPI_File, void *, MPI_Status *) ;
int PMPI_File_write_ordered_begin(MPI_File, const void *, int, MPI_Datatype)
                                                                                        ;
int PMPI_File_write_ordered_end(MPI_File, const void *, MPI_Status *) ;


int PMPI_File_get_type_extent(MPI_File, MPI_Datatype, MPI_Aint *) ;


int PMPI_Register_datarep(const char *,
    MPI_Datarep_conversion_function *,
    MPI_Datarep_conversion_function *,
    MPI_Datarep_extent_function *,
    void *) ;


int PMPI_File_set_atomicity(MPI_File, int) ;
int PMPI_File_get_atomicity(MPI_File, int *) ;
int PMPI_File_sync(MPI_File) ;
# 552 "/cvmfs/software.hpc.rwth.de/Linux/RH8/x86_64/intel/sapphirerapids/software/impi/2021.9.0-intel-compilers-2023.1.0/mpi/2021.9.0/include/mpio.h"
int PMPI_File_iread_at_all(MPI_File fh, MPI_Offset offset, void *buf, int count,
                            MPI_Datatype datatype, MPI_Request *request)
                                                          ;
int PMPI_File_iwrite_at_all(MPI_File fh, MPI_Offset offset, const void *buf, int count,
                             MPI_Datatype datatype, MPI_Request *request)
                                                          ;
int PMPI_File_iread_all(MPI_File fh, void *buf, int count, MPI_Datatype datatype,
                         MPI_Request *request)
                                                          ;
int PMPI_File_iwrite_all(MPI_File fh, const void *buf, int count, MPI_Datatype datatype,
                          MPI_Request *request)
                                                          ;
# 576 "/cvmfs/software.hpc.rwth.de/Linux/RH8/x86_64/intel/sapphirerapids/software/impi/2021.9.0-intel-compilers-2023.1.0/mpi/2021.9.0/include/mpio.h"
MPI_File PMPI_File_f2c(MPI_Fint) ;
MPI_Fint PMPI_File_c2f(MPI_File) ;
# 613 "/cvmfs/software.hpc.rwth.de/Linux/RH8/x86_64/intel/sapphirerapids/software/impi/2021.9.0-intel-compilers-2023.1.0/mpi/2021.9.0/include/mpio.h"
}
# 2475 "/cvmfs/software.hpc.rwth.de/Linux/RH8/x86_64/intel/sapphirerapids/software/impi/2021.9.0-intel-compilers-2023.1.0/mpi/2021.9.0/include/mpi.h" 2


}
# 2490 "/cvmfs/software.hpc.rwth.de/Linux/RH8/x86_64/intel/sapphirerapids/software/impi/2021.9.0-intel-compilers-2023.1.0/mpi/2021.9.0/include/mpi.h"
# 1 "/cvmfs/software.hpc.rwth.de/Linux/RH8/x86_64/intel/sapphirerapids/software/impi/2021.9.0-intel-compilers-2023.1.0/mpi/2021.9.0/include/mpicxx.h" 1
# 136 "/cvmfs/software.hpc.rwth.de/Linux/RH8/x86_64/intel/sapphirerapids/software/impi/2021.9.0-intel-compilers-2023.1.0/mpi/2021.9.0/include/mpicxx.h"
namespace MPI {







extern void MPIR_Call_world_errhand( int );







typedef MPI_Offset Offset;
typedef MPI_Aint Aint;
typedef MPI_Fint Fint;


void MPIR_CXX_InitDatatypeNames( void );


class Comm;
class Nullcomm;
class Intercomm;
class Intracomm;
class Cartcomm;
class Graphcomm;
class File;


extern int Detach_buffer( void *&v1 ) ;
extern bool Is_initialized( void ) ;
extern void Get_processor_name( char * v1, int &v2 ) ;
extern void Get_error_string( int v1, char * v2, int &v3 ) ;
extern void Compute_dims( int v1, int v2, int v3[] ) ;
extern void Get_version( int &v1, int &v2 ) ;
extern void Finalize( void ) ;
extern void Pcontrol( const int v1, ... ) ;
extern void Attach_buffer( void * v1, int v2 ) ;
extern int Get_error_class( int v1 ) ;
extern Intracomm COMM_WORLD;
extern File FILE_NULL;

class Exception {

  protected:
    int the_real_exception;

  public:


    inline Exception(int obj) : the_real_exception(obj) {}
    inline Exception(void) : the_real_exception(0) {}

    virtual ~Exception() {}


    Exception(const Exception &obj) : the_real_exception(obj.the_real_exception){}

    Exception& operator=(const Exception &obj) {
      the_real_exception = obj.the_real_exception; return *this; }


    bool operator== (const Exception &obj) {
      return (the_real_exception == obj.the_real_exception); }
    bool operator!= (const Exception &obj) {
      return (the_real_exception != obj.the_real_exception); }

    inline operator int*() { return &the_real_exception; }
    inline operator int() const { return the_real_exception; }
    Exception& operator=(const int& obj) {
      the_real_exception = obj; return *this; }

  protected:
    char the_error_message[512];
  public:
    int Get_error_code(void) { return the_real_exception; }
    int Get_error_class(void) { return MPI::Get_error_class(the_real_exception); }
    const char *Get_error_string(void)
    {
 int len;
 MPI_Error_string(the_real_exception, the_error_message, &len);
 return the_error_message;
    }
};

class Datatype {
    friend class Comm;
    friend class Status;
    friend class Intracomm;
    friend class Intercomm;
    friend class Win;
    friend class File;
    friend class Op;

  protected:
    MPI_Datatype the_real_datatype;

  public:


    inline Datatype(MPI_Datatype obj) : the_real_datatype(obj) {}
    inline Datatype(void) : the_real_datatype(((MPI_Datatype)0x0c000000)) {}

    virtual ~Datatype() {}


    Datatype(const Datatype &obj) : the_real_datatype(obj.the_real_datatype){}

    Datatype& operator=(const Datatype &obj) {
      the_real_datatype = obj.the_real_datatype; return *this; }


    bool operator== (const Datatype &obj) {
      return (the_real_datatype == obj.the_real_datatype); }
    bool operator!= (const Datatype &obj) {
      return (the_real_datatype != obj.the_real_datatype); }

    inline operator MPI_Datatype*() { return &the_real_datatype; }
    inline operator MPI_Datatype() const { return the_real_datatype; }
    Datatype& operator=(const MPI_Datatype& obj) {
      the_real_datatype = obj; return *this; }
    virtual void Commit( void )
    {
        { int err = MPI_Type_commit( (MPI_Datatype *) &the_real_datatype ) ; if (err) MPIR_Call_world_errhand( err ); };
    }
    virtual void Free( void )
    {
        { int err = MPI_Type_free( (MPI_Datatype *) &the_real_datatype ) ; if (err) MPIR_Call_world_errhand( err ); };
    }
    virtual Datatype Create_indexed( int v1, const int * v2, const int * v3 ) const
    {
        Datatype v5;
        { int err = MPI_Type_indexed( v1, (const int *)v2, (const int *)v3, (MPI_Datatype) the_real_datatype, &(v5.the_real_datatype) ) ; if (err) MPIR_Call_world_errhand( err ); };
        return v5;
    }
    virtual Datatype Create_contiguous( int v1 ) const
    {
        Datatype v3;
        { int err = MPI_Type_contiguous( v1, (MPI_Datatype) the_real_datatype, &(v3.the_real_datatype) ) ; if (err) MPIR_Call_world_errhand( err ); };
        return v3;
    }
    virtual Datatype Create_vector( int v1, int v2, int v3 ) const
    {
        Datatype v5;
        { int err = MPI_Type_vector( v1, v2, v3, (MPI_Datatype) the_real_datatype, &(v5.the_real_datatype) ) ; if (err) MPIR_Call_world_errhand( err ); };
        return v5;
    }
    static Datatype Create_struct( int v1, int v2[], Aint v3[], const Datatype v4[] )
    {
        Datatype v5;
        MPI_Datatype *l4 = new MPI_Datatype[v1];
        {
            int i4;
            for (i4=0;i4<v1;i4++) {
                l4[i4] = v4[i4].the_real_datatype;
            }
        }
        { int err = MPI_Type_create_struct( v1, (const int *)v2, (const MPI_Aint *)v3, l4, &(v5.the_real_datatype) ) ; if (err) MPIR_Call_world_errhand( err ); };
                    delete[] l4;
        return v5;
    }
    virtual int Pack_size( int v1, const Comm &v3 ) const;
    virtual void Pack( const void * v1, int v2, void * v4, int v5, int &v6, const Comm &v7 ) const;
    virtual int Get_size( void ) const
    {
        int v2;
        { int err = MPI_Type_size( (MPI_Datatype) the_real_datatype, &v2 ) ; if (err) MPIR_Call_world_errhand( err ); };
        return v2;
    }
    virtual void Get_envelope( int &v2, int &v3, int &v4, int &v5 ) const
    {
        { int err = MPI_Type_get_envelope( (MPI_Datatype) the_real_datatype, &v2, &v3, &v4, &v5 ) ; if (err) MPIR_Call_world_errhand( err ); };
    }
    virtual Datatype Create_hvector( int v1, int v2, Aint v3 ) const
    {
        Datatype v5;
        { int err = MPI_Type_create_hvector( v1, v2, v3, (MPI_Datatype) the_real_datatype, &(v5.the_real_datatype) ) ; if (err) MPIR_Call_world_errhand( err ); };
        return v5;
    }
    static Datatype Match_size( int v1, int v2 )
    {
        Datatype v3;
        { int err = MPI_Type_match_size( v1, v2, &(v3.the_real_datatype) ) ; if (err) MPIR_Call_world_errhand( err ); };
        return v3;
    }
    virtual Datatype Create_resized( const Aint v2, const Aint v3 ) const
    {
        Datatype v4;
        { int err = MPI_Type_create_resized( (MPI_Datatype) the_real_datatype, (MPI_Aint)v2, (MPI_Aint)v3, &(v4.the_real_datatype) ) ; if (err) MPIR_Call_world_errhand( err ); };
        return v4;
    }
    virtual Datatype Create_indexed_block( int v1, int v2, const int v3[] ) const
    {
        Datatype v5;
        { int err = MPI_Type_create_indexed_block( v1, v2, (const int *)v3, (MPI_Datatype) the_real_datatype, &(v5.the_real_datatype) ) ; if (err) MPIR_Call_world_errhand( err ); };
        return v5;
    }
    virtual Aint Pack_external_size( const char v1[], int v2 ) const
    {
        MPI_Aint v4;
        { int err = MPI_Pack_external_size( v1, v2, (MPI_Datatype) the_real_datatype, &v4 ) ; if (err) MPIR_Call_world_errhand( err ); };
        return v4;
    }
    Datatype Dup( void ) const
    {
        Datatype v2;
        { int err = MPI_Type_dup( (MPI_Datatype) the_real_datatype, &(v2.the_real_datatype) ) ; if (err) MPIR_Call_world_errhand( err ); };
        return v2;
    }
    virtual Datatype Create_hindexed( int v1, const int v2[], const Aint v3[] ) const
    {
        Datatype v5;
        { int err = MPI_Type_create_hindexed( v1, (const int *)v2, (const MPI_Aint *)v3, (MPI_Datatype) the_real_datatype, &(v5.the_real_datatype) ) ; if (err) MPIR_Call_world_errhand( err ); };
        return v5;
    }
    virtual bool Get_attr( int v2, void * v3 ) const
    {
        int v4;
        { int err = MPI_Type_get_attr( (MPI_Datatype) the_real_datatype, v2, v3, &v4 ) ; if (err) MPIR_Call_world_errhand( err ); };
        return v4!= 0;
    }
    virtual void Get_true_extent( Aint & v2, Aint & v3 ) const
    {
        { int err = MPI_Type_get_true_extent( (MPI_Datatype) the_real_datatype, &v2, &v3 ) ; if (err) MPIR_Call_world_errhand( err ); };
    }
    virtual Datatype Create_darray( int v1, int v2, int v3, const int v4[], const int v5[], const int v6[], const int v7[], int v8 ) const
    {
        Datatype v10;
        { int err = MPI_Type_create_darray( v1, v2, v3, (const int *)v4, (const int *)v5, (const int *)v6, (const int *)v7, v8, (MPI_Datatype) the_real_datatype, &(v10.the_real_datatype) ) ; if (err) MPIR_Call_world_errhand( err ); };
        return v10;
    }
    static Datatype Create_f90_real( int v1, int v2 )
    {
        Datatype v3;
        { int err = MPI_Type_create_f90_real( v1, v2, &(v3.the_real_datatype) ) ; if (err) MPIR_Call_world_errhand( err ); };
        return v3;
    }
    virtual void Get_contents( int v2, int v3, int v4, int v5[], Aint v6[], Datatype v7[] ) const
    {
        MPI_Datatype *l7 = new MPI_Datatype[v4];
        { int err = MPI_Type_get_contents( (MPI_Datatype) the_real_datatype, v2, v3, v4, v5, v6, l7 ) ; if (err) MPIR_Call_world_errhand( err ); };
        {
            int i7;
            for (i7=0;i7<v4;i7++) {
                v7[i7].the_real_datatype = l7[i7];
            }
            delete[] l7;
        }
    }
    virtual void Set_attr( int v2, const void * v3 )
    {
        { int err = MPI_Type_set_attr( (MPI_Datatype) the_real_datatype, v2, (void *)v3 ) ; if (err) MPIR_Call_world_errhand( err ); };
    }
    virtual void Set_name( const char * v2 )
    {
        { int err = MPI_Type_set_name( (MPI_Datatype) the_real_datatype, (const char *)v2 ) ; if (err) MPIR_Call_world_errhand( err ); };
    }
    static Datatype Create_f90_complex( int v1, int v2 )
    {
        Datatype v3;
        { int err = MPI_Type_create_f90_complex( v1, v2, &(v3.the_real_datatype) ) ; if (err) MPIR_Call_world_errhand( err ); };
        return v3;
    }
    virtual Datatype Create_subarray( int v1, const int v2[], const int v3[], const int v4[], const int v5 ) const
    {
        Datatype v7;
        { int err = MPI_Type_create_subarray( v1, v2, (const int *)v3, (const int *)v4, (int)v5, (MPI_Datatype) the_real_datatype, &(v7.the_real_datatype) ) ; if (err) MPIR_Call_world_errhand( err ); };
        return v7;
    }
    virtual void Unpack_external( const char v1[], const void * v2, const Aint v3, Aint & v4, void * v5, int v6 ) const
    {
        { int err = MPI_Unpack_external( v1, (const void *)v2, (MPI_Aint)v3, &v4, v5, v6, (MPI_Datatype) the_real_datatype ) ; if (err) MPIR_Call_world_errhand( err ); };
    }
    static void Free_keyval( int &v1 )
    {
        { int err = MPI_Type_free_keyval( &v1 ) ; if (err) MPIR_Call_world_errhand( err ); };
    }
    static Datatype Create_struct( int v1, const int v2[], const Aint v3[], const Datatype v4[] )
    {
        Datatype v5;
        MPI_Datatype *l4 = new MPI_Datatype[v1];
        {
            int i4;
            for (i4=0;i4<v1;i4++) {
                l4[i4] = v4[i4].the_real_datatype;
            }
        }
        { int err = MPI_Type_create_struct( v1, (const int *)v2, (const MPI_Aint *)v3, l4, &(v5.the_real_datatype) ) ; if (err) MPIR_Call_world_errhand( err ); };
                    delete[] l4;
        return v5;
    }
    static Datatype Create_f90_integer( int v1 )
    {
        Datatype v2;
        { int err = MPI_Type_create_f90_integer( v1, &(v2.the_real_datatype) ) ; if (err) MPIR_Call_world_errhand( err ); };
        return v2;
    }
    virtual void Pack_external( const char v1[], const void * v2, const int v3, void * v5, Aint v6, Aint & v7 ) const
    {
        { int err = MPI_Pack_external( v1, (const void *)v2, (int)v3, (MPI_Datatype) the_real_datatype, v5, v6, &v7 ) ; if (err) MPIR_Call_world_errhand( err ); };
    }
    virtual void Get_extent( Aint & v2, Aint & v3 ) const
    {
        { int err = MPI_Type_get_extent( (MPI_Datatype) the_real_datatype, &v2, &v3 ) ; if (err) MPIR_Call_world_errhand( err ); };
    }
    virtual void Delete_attr( int v2 )
    {
        { int err = MPI_Type_delete_attr( (MPI_Datatype) the_real_datatype, v2 ) ; if (err) MPIR_Call_world_errhand( err ); };
    }
    virtual void Get_name( char * v2, int &v3 ) const
    {
    MPIR_CXX_InitDatatypeNames();
        { int err = MPI_Type_get_name( (MPI_Datatype) the_real_datatype, v2, &v3 ) ; if (err) MPIR_Call_world_errhand( err ); };
    }

    void Unpack( const void *, int, void *, int, int &, const Comm & ) const;
    typedef int Copy_attr_function(const Datatype& oldtype, int type_keyval, void* extra_state, void* attribute_val_in, void* attribute_val_out, bool& flag);
    typedef int Delete_attr_function(Datatype& type, int type_keyval, void* attribute_val, void* extra_state);

    static int Create_keyval( Copy_attr_function *, Delete_attr_function *,
                              void * );


    static int NULL_COPY_FN( const Datatype &oldtype __attribute__((unused)),
        int keyval __attribute__((unused)), void *ex __attribute__((unused)),
        void *attr_in __attribute__((unused)), void *attr_out __attribute__((unused)),
        bool &flag ) { flag = 1; return 0;}
    static int NULL_DELETE_FN( Datatype &type __attribute__((unused)),
        int keyval __attribute__((unused)), void * attr __attribute__((unused)),
        void *ex __attribute__((unused)) ) { return 0; }
    static int DUP_FN( const Datatype &oldtype __attribute__((unused)),
        int keyval __attribute__((unused)), void *ex __attribute__((unused)),
        void *attr_in, void *attr_out, bool &flag ) { flag = 1;
            *(void **)attr_out = attr_in; return 0;}

};

    typedef void User_function(const void *, void*, int, const Datatype&);

class Info {
    friend class File;
    friend class Win;
    friend class Comm;
    friend class Intracomm;

  protected:
    MPI_Info the_real_info;

  public:


    inline Info(MPI_Info obj) : the_real_info(obj) {}
    inline Info(void) : the_real_info(((MPI_Info)0x1c000000)) {}

    virtual ~Info() {}


    Info(const Info &obj) : the_real_info(obj.the_real_info){}

    Info& operator=(const Info &obj) {
      the_real_info = obj.the_real_info; return *this; }


    bool operator== (const Info &obj) {
      return (the_real_info == obj.the_real_info); }
    bool operator!= (const Info &obj) {
      return (the_real_info != obj.the_real_info); }

    inline operator MPI_Info*() { return &the_real_info; }
    inline operator MPI_Info() const { return the_real_info; }
    Info& operator=(const MPI_Info& obj) {
      the_real_info = obj; return *this; }
    virtual void Delete( const char * v2 )
    {
        { int err = MPI_Info_delete( (MPI_Info) the_real_info, (const char *)v2 ) ; if (err) MPIR_Call_world_errhand( err ); };
    }
    virtual void Get_nthkey( int v2, char * v3 ) const
    {
        { int err = MPI_Info_get_nthkey( (MPI_Info) the_real_info, v2, v3 ) ; if (err) MPIR_Call_world_errhand( err ); };
    }
    virtual void Free( void )
    {
        { int err = MPI_Info_free( (MPI_Info *) &the_real_info ) ; if (err) MPIR_Call_world_errhand( err ); };
    }
    static Info Create( void )
    {
        Info v1;
        { int err = MPI_Info_create( &(v1.the_real_info) ) ; if (err) MPIR_Call_world_errhand( err ); };
        return v1;
    }
    virtual void Set( const char * v2, const char * v3 )
    {
        { int err = MPI_Info_set( (MPI_Info) the_real_info, (const char *)v2, (const char *)v3 ) ; if (err) MPIR_Call_world_errhand( err ); };
    }
    Info Dup( void ) const
    {
        Info v2;
        { int err = MPI_Info_dup( (MPI_Info) the_real_info, &(v2.the_real_info) ) ; if (err) MPIR_Call_world_errhand( err ); };
        return v2;
    }
    virtual bool Get_valuelen( const char * v2, int &v3 ) const
    {
        int v4;
        { int err = MPI_Info_get_valuelen( (MPI_Info) the_real_info, (const char *)v2, &v3, &v4 ) ; if (err) MPIR_Call_world_errhand( err ); };
        return v4!= 0;
    }
    virtual bool Get( const char * v2, int v3, char * v4 ) const
    {
        int v5;
        { int err = MPI_Info_get( (MPI_Info) the_real_info, (const char *)v2, v3, v4, &v5 ) ; if (err) MPIR_Call_world_errhand( err ); };
        return v5!= 0;
    }
    virtual int Get_nkeys( void ) const
    {
        int v2;
        { int err = MPI_Info_get_nkeys( (MPI_Info) the_real_info, &v2 ) ; if (err) MPIR_Call_world_errhand( err ); };
        return v2;
    }
};

class Status {
    friend class Comm;
    friend class File;
    friend class Request;

  protected:
    MPI_Status the_real_status;

  public:


    inline Status(MPI_Status obj) : the_real_status(obj) {}
    inline Status(void) : the_real_status() {}

    virtual ~Status() {}


    Status(const Status &obj) : the_real_status(obj.the_real_status){}

    Status& operator=(const Status &obj) {
      the_real_status = obj.the_real_status; return *this; }


    inline operator MPI_Status*() { return &the_real_status; }
    inline operator MPI_Status() const { return the_real_status; }
    Status& operator=(const MPI_Status& obj) {
      the_real_status = obj; return *this; }
    virtual bool Is_cancelled( void ) const
    {
        int v2;
        { int err = MPI_Test_cancelled( (const MPI_Status *) &the_real_status, &v2 ) ; if (err) MPIR_Call_world_errhand( err ); };
        return v2!= 0;
    }
    virtual int Get_elements( const Datatype &v2 ) const
    {
        int v3;
        { int err = MPI_Get_elements( (const MPI_Status *) &the_real_status, (MPI_Datatype)(v2.the_real_datatype), &v3 ) ; if (err) MPIR_Call_world_errhand( err ); };
        return v3;
    }
    virtual int Get_count( const Datatype &v2 ) const
    {
        int v3;
        { int err = MPI_Get_count( (const MPI_Status *) &the_real_status, (MPI_Datatype)(v2.the_real_datatype), &v3 ) ; if (err) MPIR_Call_world_errhand( err ); };
        return v3;
    }
    virtual void Set_cancelled( bool v2 )
    {
        int l2;
         l2 = (v2 == true) ? 1 : 0;
        { int err = MPI_Status_set_cancelled( (MPI_Status *) &the_real_status, l2 ) ; if (err) MPIR_Call_world_errhand( err ); };
    }
    virtual void Set_elements( const Datatype &v2, int v3 )
    {
        { int err = MPI_Status_set_elements( (MPI_Status *) &the_real_status, (MPI_Datatype)(v2.the_real_datatype), v3 ) ; if (err) MPIR_Call_world_errhand( err ); };
    }

    int Get_source(void) const { return the_real_status.MPI_SOURCE; }
    int Get_tag(void) const { return the_real_status.MPI_TAG; }
    int Get_error(void) const { return the_real_status.MPI_ERROR; }
    void Set_source(int source) { the_real_status.MPI_SOURCE = source; }
    void Set_tag(int tag) { the_real_status.MPI_TAG = tag; }
    void Set_error(int error) { the_real_status.MPI_ERROR = error; }
};

class Group {
    friend class Comm;
    friend class Intracomm;
    friend class Intercomm;
    friend class Win;
    friend class File;

  protected:
    MPI_Group the_real_group;

  public:


    inline Group(MPI_Group obj) : the_real_group(obj) {}
    inline Group(void) : the_real_group(((MPI_Group)0x08000000)) {}

    virtual ~Group() {}


    Group(const Group &obj) : the_real_group(obj.the_real_group){}

    Group& operator=(const Group &obj) {
      the_real_group = obj.the_real_group; return *this; }


    bool operator== (const Group &obj) {
      return (the_real_group == obj.the_real_group); }
    bool operator!= (const Group &obj) {
      return (the_real_group != obj.the_real_group); }

    inline operator MPI_Group*() { return &the_real_group; }
    inline operator MPI_Group() const { return the_real_group; }
    Group& operator=(const MPI_Group& obj) {
      the_real_group = obj; return *this; }
    virtual Group Excl( int v2, const int v3[] ) const
    {
        Group v4;
        { int err = MPI_Group_excl( (MPI_Group) the_real_group, v2, (const int *)v3, &(v4.the_real_group) ) ; if (err) MPIR_Call_world_errhand( err ); };
        return v4;
    }
    virtual int Get_rank( void ) const
    {
        int v2;
        { int err = MPI_Group_rank( (MPI_Group) the_real_group, &v2 ) ; if (err) MPIR_Call_world_errhand( err ); };
        return v2;
    }
    virtual void Free( void )
    {
        { int err = MPI_Group_free( (MPI_Group *) &the_real_group ) ; if (err) MPIR_Call_world_errhand( err ); };
    }
    static Group Union( const Group &v1, const Group &v2 )
    {
        Group v3;
        { int err = MPI_Group_union( (MPI_Group)(v1.the_real_group), (MPI_Group)(v2.the_real_group), &(v3.the_real_group) ) ; if (err) MPIR_Call_world_errhand( err ); };
        return v3;
    }
    static Group Intersect( const Group &v1, const Group &v2 )
    {
        Group v3;
        { int err = MPI_Group_intersection( (MPI_Group)(v1.the_real_group), (MPI_Group)(v2.the_real_group), &(v3.the_real_group) ) ; if (err) MPIR_Call_world_errhand( err ); };
        return v3;
    }
    virtual Group Range_excl( int v2, const int v3[][3] ) const
    {
        Group v4;
        { int err = MPI_Group_range_excl( (MPI_Group) the_real_group, v2, (int (*)[3])v3, &(v4.the_real_group) ) ; if (err) MPIR_Call_world_errhand( err ); };
        return v4;
    }
    virtual Group Range_incl( int v2, const int v3[][3] ) const
    {
        Group v4;
        { int err = MPI_Group_range_incl( (MPI_Group) the_real_group, v2, (int (*)[3])v3, &(v4.the_real_group) ) ; if (err) MPIR_Call_world_errhand( err ); };
        return v4;
    }
    static Group Difference( const Group &v1, const Group &v2 )
    {
        Group v3;
        { int err = MPI_Group_difference( (MPI_Group)(v1.the_real_group), (MPI_Group)(v2.the_real_group), &(v3.the_real_group) ) ; if (err) MPIR_Call_world_errhand( err ); };
        return v3;
    }
    static void Translate_ranks( const Group &v1, int v2, const int v3[], const Group &v4, int v5[] )
    {
        { int err = MPI_Group_translate_ranks( (MPI_Group)(v1.the_real_group), v2, (const int *)v3, (MPI_Group)(v4.the_real_group), v5 ) ; if (err) MPIR_Call_world_errhand( err ); };
    }
    virtual Group Incl( int v2, const int v3[] ) const
    {
        Group v4;
        { int err = MPI_Group_incl( (MPI_Group) the_real_group, v2, (const int *)v3, &(v4.the_real_group) ) ; if (err) MPIR_Call_world_errhand( err ); };
        return v4;
    }
    virtual int Get_size( void ) const
    {
        int v2;
        { int err = MPI_Group_size( (MPI_Group) the_real_group, &v2 ) ; if (err) MPIR_Call_world_errhand( err ); };
        return v2;
    }
    static int Compare( const Group &v1, const Group &v2 )
    {
        int v3;
        { int err = MPI_Group_compare( (MPI_Group)(v1.the_real_group), (MPI_Group)(v2.the_real_group), &v3 ) ; if (err) MPIR_Call_world_errhand( err ); };
        return v3;
    }
};

class Op {
    friend class Intracomm;
    friend class Intercomm;
    friend class Win;
    friend class Comm;

  protected:
    MPI_Op the_real_op;

  public:


    inline Op(MPI_Op obj) : the_real_op(obj) {}
    inline Op(void) : the_real_op(((MPI_Op)0x18000000)) {}

    virtual ~Op() {}


    Op(const Op &obj) : the_real_op(obj.the_real_op){}

    Op& operator=(const Op &obj) {
      the_real_op = obj.the_real_op; return *this; }


    bool operator== (const Op &obj) {
      return (the_real_op == obj.the_real_op); }
    bool operator!= (const Op &obj) {
      return (the_real_op != obj.the_real_op); }

    inline operator MPI_Op*() { return &the_real_op; }
    inline operator MPI_Op() const { return the_real_op; }
    Op& operator=(const MPI_Op& obj) {
      the_real_op = obj; return *this; }
    virtual void Free( void )
    {
        { int err = MPI_Op_free( (MPI_Op *) &the_real_op ) ; if (err) MPIR_Call_world_errhand( err ); };
    }
    virtual bool Is_commutative( void ) const
    {
        int v2;
        { int err = MPI_Op_commutative( (MPI_Op) the_real_op, &v2 ) ; if (err) MPIR_Call_world_errhand( err ); };
        return v2!= 0;
    }
    virtual void Reduce_local( const void * v1, void * v2, int v3, const Datatype &v4 ) const
    {
        { int err = MPI_Reduce_local( (const void *)v1, v2, v3, (MPI_Datatype)(v4.the_real_datatype), (MPI_Op) the_real_op ) ; if (err) MPIR_Call_world_errhand( err ); };
    }

    void Init( User_function *, bool );
};

class Errhandler {
    friend class Comm;
    friend class File;
    friend class Win;

  protected:
    MPI_Errhandler the_real_errhandler;

  public:


    inline Errhandler(MPI_Errhandler obj) : the_real_errhandler(obj) {}
    inline Errhandler(void) : the_real_errhandler(((MPI_Errhandler)0x14000000)) {}

    virtual ~Errhandler() {}


    Errhandler(const Errhandler &obj) : the_real_errhandler(obj.the_real_errhandler){}

    Errhandler& operator=(const Errhandler &obj) {
      the_real_errhandler = obj.the_real_errhandler; return *this; }


    bool operator== (const Errhandler &obj) {
      return (the_real_errhandler == obj.the_real_errhandler); }
    bool operator!= (const Errhandler &obj) {
      return (the_real_errhandler != obj.the_real_errhandler); }

    inline operator MPI_Errhandler*() { return &the_real_errhandler; }
    inline operator MPI_Errhandler() const { return the_real_errhandler; }
    Errhandler& operator=(const MPI_Errhandler& obj) {
      the_real_errhandler = obj; return *this; }
    virtual void Free( void )
    {
        { int err = MPI_Errhandler_free( (MPI_Errhandler *) &the_real_errhandler ) ; if (err) MPIR_Call_world_errhand( err ); };
    }
};

class Request {
    friend class Comm;
    friend class File;
    friend class Grequest;

  protected:
    MPI_Request the_real_request;

  public:


    inline Request(MPI_Request obj) : the_real_request(obj) {}
    inline Request(void) : the_real_request(((MPI_Request)0x2c000000)) {}

    virtual ~Request() {}


    Request(const Request &obj) : the_real_request(obj.the_real_request){}

    Request& operator=(const Request &obj) {
      the_real_request = obj.the_real_request; return *this; }


    bool operator== (const Request &obj) {
      return (the_real_request == obj.the_real_request); }
    bool operator!= (const Request &obj) {
      return (the_real_request != obj.the_real_request); }

    inline operator MPI_Request*() { return &the_real_request; }
    inline operator MPI_Request() const { return the_real_request; }
    Request& operator=(const MPI_Request& obj) {
      the_real_request = obj; return *this; }
    static bool Testany( int v1, Request v2[], int &v3, Status & v5 )
    {
        int v4;
        MPI_Request *l2 = new MPI_Request[v1];
        {
            int i2;
            for (i2=0;i2<v1;i2++) {
                l2[i2] = v2[i2].the_real_request;
            }
        }
        { int err = MPI_Testany( v1, l2, &v3, &v4, (MPI_Status *)&(v5.the_real_status ) ) ; if (err) MPIR_Call_world_errhand( err ); };
        {
            int i2;
            for (i2=0;i2<v1;i2++) {
                v2[i2].the_real_request = l2[i2];
            }
            delete[] l2;
        }
        return v4!= 0;
    }
    static bool Testany( int v1, Request v2[], int &v3 )
    {
        int v4;
        MPI_Request *l2 = new MPI_Request[v1];
        {
            int i2;
            for (i2=0;i2<v1;i2++) {
                l2[i2] = v2[i2].the_real_request;
            }
        }
        { int err = MPI_Testany( v1, l2, &v3, &v4, (MPI_Status *)1 ) ; if (err) MPIR_Call_world_errhand( err ); };
        {
            int i2;
            for (i2=0;i2<v1;i2++) {
                v2[i2].the_real_request = l2[i2];
            }
            delete[] l2;
        }
        return v4!= 0;
    }
    static int Waitsome( int v1, Request v2[], int v4[], Status v5[] )
    {
        int v3;
        MPI_Request *l2 = new MPI_Request[v1];
        MPI_Status *l5 = new MPI_Status[v1];
        {
            int i2;
            for (i2=0;i2<v1;i2++) {
                l2[i2] = v2[i2].the_real_request;
            }
        }
        { int err = MPI_Waitsome( v1, l2, &v3, v4, l5 ) ; if (err) MPIR_Call_world_errhand( err ); };
        {
            int i2;
            for (i2=0;i2<v1;i2++) {
                v2[i2].the_real_request = l2[i2];
            }
            delete[] l2;
        }
        {
            int i5;
            for (i5=0;i5<v1;i5++) {
                v5[i5].the_real_status = l5[i5];
            }
            delete[] l5;
        }
        return v3;
    }
    static int Waitsome( int v1, Request v2[], int v4[] )
    {
        int v3;
        MPI_Request *l2 = new MPI_Request[v1];
        {
            int i2;
            for (i2=0;i2<v1;i2++) {
                l2[i2] = v2[i2].the_real_request;
            }
        }
        { int err = MPI_Waitsome( v1, l2, &v3, v4, (MPI_Status *)1 ) ; if (err) MPIR_Call_world_errhand( err ); };
        {
            int i2;
            for (i2=0;i2<v1;i2++) {
                v2[i2].the_real_request = l2[i2];
            }
            delete[] l2;
        }
        return v3;
    }
    virtual void Free( void )
    {
        { int err = MPI_Request_free( (MPI_Request *) &the_real_request ) ; if (err) MPIR_Call_world_errhand( err ); };
    }
    static bool Testall( int v1, Request v2[], Status v4[] )
    {
        int v3;
        MPI_Request *l2 = new MPI_Request[v1];
        MPI_Status *l4 = new MPI_Status[v1];
        {
            int i2;
            for (i2=0;i2<v1;i2++) {
                l2[i2] = v2[i2].the_real_request;
            }
        }
        { int err = MPI_Testall( v1, l2, &v3, l4 ) ; if (err) MPIR_Call_world_errhand( err ); };
        {
            int i2;
            for (i2=0;i2<v1;i2++) {
                v2[i2].the_real_request = l2[i2];
            }
            delete[] l2;
        }
        {
            int i4;
            for (i4=0;i4<v1;i4++) {
                v4[i4].the_real_status = l4[i4];
            }
            delete[] l4;
        }
        return v3!= 0;
    }
    static bool Testall( int v1, Request v2[] )
    {
        int v3;
        MPI_Request *l2 = new MPI_Request[v1];
        {
            int i2;
            for (i2=0;i2<v1;i2++) {
                l2[i2] = v2[i2].the_real_request;
            }
        }
        { int err = MPI_Testall( v1, l2, &v3, (MPI_Status *)1 ) ; if (err) MPIR_Call_world_errhand( err ); };
        {
            int i2;
            for (i2=0;i2<v1;i2++) {
                v2[i2].the_real_request = l2[i2];
            }
            delete[] l2;
        }
        return v3!= 0;
    }
    virtual void Wait( Status & v2 )
    {
        { int err = MPI_Wait( (MPI_Request *) &the_real_request, (MPI_Status *)&(v2.the_real_status ) ) ; if (err) MPIR_Call_world_errhand( err ); };
    }
    virtual void Wait( void )
    {
        { int err = MPI_Wait( (MPI_Request *) &the_real_request, (MPI_Status *)1 ) ; if (err) MPIR_Call_world_errhand( err ); };
    }
    static int Testsome( int v1, Request v2[], int v4[], Status v5[] )
    {
        int v3;
        MPI_Request *l2 = new MPI_Request[v1];
        MPI_Status *l5 = new MPI_Status[v1];
        {
            int i2;
            for (i2=0;i2<v1;i2++) {
                l2[i2] = v2[i2].the_real_request;
            }
        }
        { int err = MPI_Testsome( v1, l2, &v3, v4, l5 ) ; if (err) MPIR_Call_world_errhand( err ); };
        {
            int i2;
            for (i2=0;i2<v1;i2++) {
                v2[i2].the_real_request = l2[i2];
            }
            delete[] l2;
        }
        {
            int i5;
            for (i5=0;i5<v1;i5++) {
                v5[i5].the_real_status = l5[i5];
            }
            delete[] l5;
        }
        return v3;
    }
    static int Testsome( int v1, Request v2[], int v4[] )
    {
        int v3;
        MPI_Request *l2 = new MPI_Request[v1];
        {
            int i2;
            for (i2=0;i2<v1;i2++) {
                l2[i2] = v2[i2].the_real_request;
            }
        }
        { int err = MPI_Testsome( v1, l2, &v3, v4, (MPI_Status *)1 ) ; if (err) MPIR_Call_world_errhand( err ); };
        {
            int i2;
            for (i2=0;i2<v1;i2++) {
                v2[i2].the_real_request = l2[i2];
            }
            delete[] l2;
        }
        return v3;
    }
    static void Waitall( int v1, Request v2[], Status v3[] )
    {
        MPI_Request *l2 = new MPI_Request[v1];
        MPI_Status *l3 = new MPI_Status[v1];
        {
            int i2;
            for (i2=0;i2<v1;i2++) {
                l2[i2] = v2[i2].the_real_request;
            }
        }
        { int err = MPI_Waitall( v1, l2, l3 ) ; if (err) MPIR_Call_world_errhand( err ); };
        {
            int i2;
            for (i2=0;i2<v1;i2++) {
                v2[i2].the_real_request = l2[i2];
            }
            delete[] l2;
        }
        {
            int i3;
            for (i3=0;i3<v1;i3++) {
                v3[i3].the_real_status = l3[i3];
            }
            delete[] l3;
        }
    }
    static void Waitall( int v1, Request v2[] )
    {
        MPI_Request *l2 = new MPI_Request[v1];
        {
            int i2;
            for (i2=0;i2<v1;i2++) {
                l2[i2] = v2[i2].the_real_request;
            }
        }
        { int err = MPI_Waitall( v1, l2, (MPI_Status *)1 ) ; if (err) MPIR_Call_world_errhand( err ); };
        {
            int i2;
            for (i2=0;i2<v1;i2++) {
                v2[i2].the_real_request = l2[i2];
            }
            delete[] l2;
        }
    }
    static int Waitany( int v1, Request v2[], Status & v4 )
    {
        int v3;
        MPI_Request *l2 = new MPI_Request[v1];
        {
            int i2;
            for (i2=0;i2<v1;i2++) {
                l2[i2] = v2[i2].the_real_request;
            }
        }
        { int err = MPI_Waitany( v1, l2, &v3, (MPI_Status *)&(v4.the_real_status ) ) ; if (err) MPIR_Call_world_errhand( err ); };
        {
            int i2;
            for (i2=0;i2<v1;i2++) {
                v2[i2].the_real_request = l2[i2];
            }
            delete[] l2;
        }
        return v3;
    }
    static int Waitany( int v1, Request v2[] )
    {
        int v3;
        MPI_Request *l2 = new MPI_Request[v1];
        {
            int i2;
            for (i2=0;i2<v1;i2++) {
                l2[i2] = v2[i2].the_real_request;
            }
        }
        { int err = MPI_Waitany( v1, l2, &v3, (MPI_Status *)1 ) ; if (err) MPIR_Call_world_errhand( err ); };
        {
            int i2;
            for (i2=0;i2<v1;i2++) {
                v2[i2].the_real_request = l2[i2];
            }
            delete[] l2;
        }
        return v3;
    }
    virtual bool Test( Status & v3 )
    {
        int v2;
        { int err = MPI_Test( (MPI_Request *) &the_real_request, &v2, (MPI_Status *)&(v3.the_real_status ) ) ; if (err) MPIR_Call_world_errhand( err ); };
        return v2!= 0;
    }
    virtual bool Test( void )
    {
        int v2;
        { int err = MPI_Test( (MPI_Request *) &the_real_request, &v2, (MPI_Status *)1 ) ; if (err) MPIR_Call_world_errhand( err ); };
        return v2!= 0;
    }
    virtual void Cancel( void ) const
    {
        { int err = MPI_Cancel( (MPI_Request *) &the_real_request ) ; if (err) MPIR_Call_world_errhand( err ); };
    }
    virtual bool Get_status( Status & v3 ) const
    {
        int v2;
        { int err = MPI_Request_get_status( (MPI_Request) the_real_request, &v2, (MPI_Status *)&(v3.the_real_status ) ) ; if (err) MPIR_Call_world_errhand( err ); };
        return v2!= 0;
    }
    virtual bool Get_status( void ) const
    {
        int v2;
        { int err = MPI_Request_get_status( (MPI_Request) the_real_request, &v2, (MPI_Status *)1 ) ; if (err) MPIR_Call_world_errhand( err ); };
        return v2!= 0;
    }
};

class Prequest : public Request {

  public:


    inline Prequest(MPI_Request obj) : Request(obj) {}
    inline Prequest(void) : Request() {}

    virtual ~Prequest() {}


    Prequest(const Prequest &obj) : Request(obj) {}

    Prequest& operator=(const Prequest &obj) {
      the_real_request = obj.the_real_request; return *this; }


    inline operator MPI_Request*() { return &the_real_request; }
    inline operator MPI_Request() const { return the_real_request; }
    Prequest& operator=(const MPI_Request& obj) {
      the_real_request = obj; return *this; }
    virtual void Start( void )
    {
        { int err = MPI_Start( (MPI_Request *) &the_real_request ) ; if (err) MPIR_Call_world_errhand( err ); };
    }
    static void Startall( int v1, Prequest v2[] )
    {
        MPI_Request *l2 = new MPI_Request[v1];
        {
            int i2;
            for (i2=0;i2<v1;i2++) {
                l2[i2] = v2[i2].the_real_request;
            }
        }
        { int err = MPI_Startall( v1, l2 ) ; if (err) MPIR_Call_world_errhand( err ); };
        {
            int i2;
            for (i2=0;i2<v1;i2++) {
                v2[i2].the_real_request = l2[i2];
            }
            delete[] l2;
        }
    }
};

class Comm {
    friend class Cartcomm;
    friend class Intercomm;
    friend class Intracomm;
    friend class Graphcomm;
    friend class Nullcomm;
    friend class Datatype;
    friend class Win;
    friend class File;

  protected:
    MPI_Comm the_real_comm;

  public:


    inline Comm(MPI_Comm obj) : the_real_comm(obj) {}
    inline Comm(void) : the_real_comm(((MPI_Comm)0x04000000)) {}

    virtual ~Comm() {}


    Comm(const Comm &obj) : the_real_comm(obj.the_real_comm){}

    Comm& operator=(const Comm &obj) {
      the_real_comm = obj.the_real_comm; return *this; }


    bool operator== (const Comm &obj) {
      return (the_real_comm == obj.the_real_comm); }
    bool operator!= (const Comm &obj) {
      return (the_real_comm != obj.the_real_comm); }

    inline operator MPI_Comm*() { return &the_real_comm; }
    inline operator MPI_Comm() const { return the_real_comm; }
    Comm& operator=(const MPI_Comm& obj) {
      the_real_comm = obj; return *this; }
    virtual Group Get_group( void ) const
    {
        Group v2;
        { int err = MPI_Comm_group( (MPI_Comm) the_real_comm, &(v2.the_real_group) ); if (err) { (this)->Call_errhandler( err ); }};
        return v2;
    }
    virtual int Get_rank( void ) const
    {
        int v2;
        { int err = MPI_Comm_rank( (MPI_Comm) the_real_comm, &v2 ); if (err) { (this)->Call_errhandler( err ); }};
        return v2;
    }
    virtual Prequest Bsend_init( const void * v1, int v2, const Datatype &v3, int v4, int v5 ) const
    {
        Prequest v7;
        { int err = MPI_Bsend_init( (const void *)v1, v2, (MPI_Datatype)(v3.the_real_datatype), v4, v5, (MPI_Comm) the_real_comm, &(v7.the_real_request) ); if (err) { (this)->Call_errhandler( err ); }};
        return v7;
    }
    virtual Prequest Ssend_init( const void * v1, int v2, const Datatype &v3, int v4, int v5 ) const
    {
        Prequest v7;
        { int err = MPI_Ssend_init( (const void *)v1, v2, (MPI_Datatype)(v3.the_real_datatype), v4, v5, (MPI_Comm) the_real_comm, &(v7.the_real_request) ); if (err) { (this)->Call_errhandler( err ); }};
        return v7;
    }
    virtual bool Is_inter( void ) const
    {
        int v2;
        { int err = MPI_Comm_test_inter( (MPI_Comm) the_real_comm, &v2 ); if (err) { (this)->Call_errhandler( err ); }};
        return v2!= 0;
    }
    virtual Prequest Rsend_init( const void * v1, int v2, const Datatype &v3, int v4, int v5 ) const
    {
        Prequest v7;
        { int err = MPI_Rsend_init( (const void *)v1, v2, (MPI_Datatype)(v3.the_real_datatype), v4, v5, (MPI_Comm) the_real_comm, &(v7.the_real_request) ); if (err) { (this)->Call_errhandler( err ); }};
        return v7;
    }
    virtual Request Ibsend( const void * v1, int v2, const Datatype &v3, int v4, int v5 ) const
    {
        Request v7;
        { int err = MPI_Ibsend( (const void *)v1, v2, (MPI_Datatype)(v3.the_real_datatype), v4, v5, (MPI_Comm) the_real_comm, &(v7.the_real_request) ); if (err) { (this)->Call_errhandler( err ); }};
        return v7;
    }
    virtual void Abort( int v2 ) const
    {
        { int err = MPI_Abort( (MPI_Comm) the_real_comm, v2 ); if (err) { (this)->Call_errhandler( err ); }};
    }
    virtual void Free( void )
    {
        { int err = MPI_Comm_free( (MPI_Comm *) &the_real_comm ); if (err) { (this)->Call_errhandler( err ); }};
    }
    virtual Prequest Send_init( const void * v1, int v2, const Datatype &v3, int v4, int v5 ) const
    {
        Prequest v7;
        { int err = MPI_Send_init( (const void *)v1, v2, (MPI_Datatype)(v3.the_real_datatype), v4, v5, (MPI_Comm) the_real_comm, &(v7.the_real_request) ); if (err) { (this)->Call_errhandler( err ); }};
        return v7;
    }
    virtual void Recv( void * v1, int v2, const Datatype &v3, int v4, int v5, Status & v7 ) const
    {
        { int err = MPI_Recv( v1, v2, (MPI_Datatype)(v3.the_real_datatype), v4, v5, (MPI_Comm) the_real_comm, (MPI_Status *)&(v7.the_real_status ) ); if (err) { (this)->Call_errhandler( err ); }};
    }
    virtual void Recv( void * v1, int v2, const Datatype &v3, int v4, int v5 ) const
    {
        { int err = MPI_Recv( v1, v2, (MPI_Datatype)(v3.the_real_datatype), v4, v5, (MPI_Comm) the_real_comm, (MPI_Status *)1 ); if (err) { (this)->Call_errhandler( err ); }};
    }
    virtual void Sendrecv( const void * v1, int v2, const Datatype &v3, int v4, int v5, void * v6, int v7, const Datatype &v8, int v9, int v10, Status & v12 ) const
    {
        { int err = MPI_Sendrecv( (const void *)v1, v2, (MPI_Datatype)(v3.the_real_datatype), v4, v5, v6, v7, (MPI_Datatype)(v8.the_real_datatype), v9, v10, (MPI_Comm) the_real_comm, (MPI_Status *)&(v12.the_real_status ) ); if (err) { (this)->Call_errhandler( err ); }};
    }
    virtual void Sendrecv( const void * v1, int v2, const Datatype &v3, int v4, int v5, void * v6, int v7, const Datatype &v8, int v9, int v10 ) const
    {
        { int err = MPI_Sendrecv( (const void *)v1, v2, (MPI_Datatype)(v3.the_real_datatype), v4, v5, v6, v7, (MPI_Datatype)(v8.the_real_datatype), v9, v10, (MPI_Comm) the_real_comm, (MPI_Status *)1 ); if (err) { (this)->Call_errhandler( err ); }};
    }
    virtual void Sendrecv_replace( void * v1, int v2, const Datatype &v3, int v4, int v5, int v6, int v7, Status & v9 ) const
    {
        { int err = MPI_Sendrecv_replace( v1, v2, (MPI_Datatype)(v3.the_real_datatype), v4, v5, v6, v7, (MPI_Comm) the_real_comm, (MPI_Status *)&(v9.the_real_status ) ); if (err) { (this)->Call_errhandler( err ); }};
    }
    virtual void Sendrecv_replace( void * v1, int v2, const Datatype &v3, int v4, int v5, int v6, int v7 ) const
    {
        { int err = MPI_Sendrecv_replace( v1, v2, (MPI_Datatype)(v3.the_real_datatype), v4, v5, v6, v7, (MPI_Comm) the_real_comm, (MPI_Status *)1 ); if (err) { (this)->Call_errhandler( err ); }};
    }
    virtual int Get_topology( void ) const
    {
        int v2;
        { int err = MPI_Topo_test( (MPI_Comm) the_real_comm, &v2 ); if (err) { (this)->Call_errhandler( err ); }};
        return v2;
    }
    virtual Request Isend( const void * v1, int v2, const Datatype &v3, int v4, int v5 ) const
    {
        Request v7;
        { int err = MPI_Isend( (const void *)v1, v2, (MPI_Datatype)(v3.the_real_datatype), v4, v5, (MPI_Comm) the_real_comm, &(v7.the_real_request) ); if (err) { (this)->Call_errhandler( err ); }};
        return v7;
    }
    virtual void Probe( int v1, int v2, Status & v4 ) const
    {
        { int err = MPI_Probe( v1, v2, (MPI_Comm) the_real_comm, (MPI_Status *)&(v4.the_real_status ) ); if (err) { (this)->Call_errhandler( err ); }};
    }
    virtual void Probe( int v1, int v2 ) const
    {
        { int err = MPI_Probe( v1, v2, (MPI_Comm) the_real_comm, (MPI_Status *)1 ); if (err) { (this)->Call_errhandler( err ); }};
    }
    static int Compare( const Comm &v1, const Comm &v2 )
    {
        int v3;
        { int err = MPI_Comm_compare( (MPI_Comm)(v1.the_real_comm), (MPI_Comm)(v2.the_real_comm), &v3 ) ; if (err) MPIR_Call_world_errhand( err ); };
        return v3;
    }
    virtual int Get_size( void ) const
    {
        int v2;
        { int err = MPI_Comm_size( (MPI_Comm) the_real_comm, &v2 ); if (err) { (this)->Call_errhandler( err ); }};
        return v2;
    }
    virtual Request Issend( const void * v1, int v2, const Datatype &v3, int v4, int v5 ) const
    {
        Request v7;
        { int err = MPI_Issend( (const void *)v1, v2, (MPI_Datatype)(v3.the_real_datatype), v4, v5, (MPI_Comm) the_real_comm, &(v7.the_real_request) ); if (err) { (this)->Call_errhandler( err ); }};
        return v7;
    }
    virtual void Set_errhandler( const Errhandler &v2 )
    {
        { int err = MPI_Comm_set_errhandler( (MPI_Comm) the_real_comm, (MPI_Errhandler)(v2.the_real_errhandler) ); if (err) { (this)->Call_errhandler( err ); }};
    }
    virtual void Send( const void * v1, int v2, const Datatype &v3, int v4, int v5 ) const
    {
        { int err = MPI_Send( (const void *)v1, v2, (MPI_Datatype)(v3.the_real_datatype), v4, v5, (MPI_Comm) the_real_comm ); if (err) { (this)->Call_errhandler( err ); }};
    }
    virtual Request Irsend( const void * v1, int v2, const Datatype &v3, int v4, int v5 ) const
    {
        Request v7;
        { int err = MPI_Irsend( (const void *)v1, v2, (MPI_Datatype)(v3.the_real_datatype), v4, v5, (MPI_Comm) the_real_comm, &(v7.the_real_request) ); if (err) { (this)->Call_errhandler( err ); }};
        return v7;
    }
    virtual void Ssend( const void * v1, int v2, const Datatype &v3, int v4, int v5 ) const
    {
        { int err = MPI_Ssend( (const void *)v1, v2, (MPI_Datatype)(v3.the_real_datatype), v4, v5, (MPI_Comm) the_real_comm ); if (err) { (this)->Call_errhandler( err ); }};
    }
    virtual Prequest Recv_init( void * v1, int v2, const Datatype &v3, int v4, int v5 ) const
    {
        Prequest v7;
        { int err = MPI_Recv_init( v1, v2, (MPI_Datatype)(v3.the_real_datatype), v4, v5, (MPI_Comm) the_real_comm, &(v7.the_real_request) ); if (err) { (this)->Call_errhandler( err ); }};
        return v7;
    }
    virtual bool Iprobe( int v1, int v2, Status & v5 ) const
    {
        int v4;
        { int err = MPI_Iprobe( v1, v2, (MPI_Comm) the_real_comm, &v4, (MPI_Status *)&(v5.the_real_status ) ); if (err) { (this)->Call_errhandler( err ); }};
        return v4!= 0;
    }
    virtual bool Iprobe( int v1, int v2 ) const
    {
        int v4;
        { int err = MPI_Iprobe( v1, v2, (MPI_Comm) the_real_comm, &v4, (MPI_Status *)1 ); if (err) { (this)->Call_errhandler( err ); }};
        return v4!= 0;
    }
    virtual void Bsend( const void * v1, int v2, const Datatype &v3, int v4, int v5 ) const
    {
        { int err = MPI_Bsend( (const void *)v1, v2, (MPI_Datatype)(v3.the_real_datatype), v4, v5, (MPI_Comm) the_real_comm ); if (err) { (this)->Call_errhandler( err ); }};
    }
    virtual Request Irecv( void * v1, int v2, const Datatype &v3, int v4, int v5 ) const
    {
        Request v7;
        { int err = MPI_Irecv( v1, v2, (MPI_Datatype)(v3.the_real_datatype), v4, v5, (MPI_Comm) the_real_comm, &(v7.the_real_request) ); if (err) { (this)->Call_errhandler( err ); }};
        return v7;
    }
    virtual Errhandler Get_errhandler( void ) const
    {
        Errhandler v2;
        { int err = MPI_Comm_get_errhandler( (MPI_Comm) the_real_comm, &(v2.the_real_errhandler) ); if (err) { (this)->Call_errhandler( err ); }};
        return v2;
    }
    virtual void Rsend( const void * v1, int v2, const Datatype &v3, int v4, int v5 ) const
    {
        { int err = MPI_Rsend( (const void *)v1, v2, (MPI_Datatype)(v3.the_real_datatype), v4, v5, (MPI_Comm) the_real_comm ); if (err) { (this)->Call_errhandler( err ); }};
    }
    virtual void Gatherv( const void * v1, int v2, const Datatype &v3, void * v4, const int * v5, const int * v6, const Datatype &v7, int v8 ) const
    {
        { int err = MPI_Gatherv( (const void *)v1, v2, (MPI_Datatype)(v3.the_real_datatype), v4, (const int *)v5, (const int *)v6, (MPI_Datatype)(v7.the_real_datatype), v8, (MPI_Comm) the_real_comm ); if (err) { (this)->Call_errhandler( err ); }};
    }
    virtual void Disconnect( void )
    {
        { int err = MPI_Comm_disconnect( (MPI_Comm *) &the_real_comm ); if (err) { (this)->Call_errhandler( err ); }};
    }
    virtual void Allreduce( const void * v1, void * v2, int v3, const Datatype &v4, const Op &v5 ) const
    {
        { int err = MPI_Allreduce( (const void *)v1, v2, v3, (MPI_Datatype)(v4.the_real_datatype), (MPI_Op)(v5.the_real_op), (MPI_Comm) the_real_comm ); if (err) { (this)->Call_errhandler( err ); }};
    }
    virtual void Alltoallw( const void * v1, const int v2[], const int v3[], const Datatype v4[], void * v5, const int v6[], const int v7[], const Datatype v8[] ) const
    {
        MPI_Datatype *l4 = new MPI_Datatype[Get_size()];
        MPI_Datatype *l8 = new MPI_Datatype[Get_size()];
        {
            int i4;
            for (i4=0;i4<Get_size();i4++) {
                l4[i4] = v4[i4].the_real_datatype;
            }
        }
        {
            int i8;
            for (i8=0;i8<Get_size();i8++) {
                l8[i8] = v8[i8].the_real_datatype;
            }
        }
        { int err = MPI_Alltoallw( (const void *)v1, (const int *)v2, (const int *)v3, l4, v5, (const int *)v6, (const int *)v7, l8, (MPI_Comm) the_real_comm ); if (err) { (this)->Call_errhandler( err ); }};
                    delete[] l4;
                    delete[] l8;
    }
    static Intercomm Join( const int v1 ) ;
    virtual void Alltoall( const void * v1, int v2, const Datatype &v3, void * v4, int v5, const Datatype &v6 ) const
    {
        { int err = MPI_Alltoall( (const void *)v1, v2, (MPI_Datatype)(v3.the_real_datatype), v4, v5, (MPI_Datatype)(v6.the_real_datatype), (MPI_Comm) the_real_comm ); if (err) { (this)->Call_errhandler( err ); }};
    }
    virtual bool Get_attr( int v2, void * v3 ) const
    {
        int v4;
        { int err = MPI_Comm_get_attr( (MPI_Comm) the_real_comm, v2, v3, &v4 ); if (err) { (this)->Call_errhandler( err ); }};
        return v4!= 0;
    }
    virtual void Barrier( void ) const
    {
        { int err = MPI_Barrier( (MPI_Comm) the_real_comm ); if (err) { (this)->Call_errhandler( err ); }};
    }
    virtual void Bcast( void * v1, int v2, const Datatype &v3, int v4 ) const
    {
        { int err = MPI_Bcast( v1, v2, (MPI_Datatype)(v3.the_real_datatype), v4, (MPI_Comm) the_real_comm ); if (err) { (this)->Call_errhandler( err ); }};
    }
    virtual void Set_attr( int v2, const void * v3 )
    {
        { int err = MPI_Comm_set_attr( (MPI_Comm) the_real_comm, v2, (void *)v3 ); if (err) { (this)->Call_errhandler( err ); }};
    }
    virtual void Set_name( const char * v2 )
    {
        { int err = MPI_Comm_set_name( (MPI_Comm) the_real_comm, (const char *)v2 ); if (err) { (this)->Call_errhandler( err ); }};
    }
    static Intercomm Get_parent( void ) ;
    virtual void Alltoallv( const void * v1, const int * v2, const int * v3, const Datatype &v4, void * v5, const int * v6, const int * v7, const Datatype &v8 ) const
    {
        { int err = MPI_Alltoallv( (const void *)v1, (const int *)v2, (const int *)v3, (MPI_Datatype)(v4.the_real_datatype), v5, (const int *)v6, (const int *)v7, (MPI_Datatype)(v8.the_real_datatype), (MPI_Comm) the_real_comm ); if (err) { (this)->Call_errhandler( err ); }};
    }
    virtual void Reduce_scatter( const void * v1, void * v2, const int v3[], const Datatype &v4, const Op &v5 ) const
    {
        { int err = MPI_Reduce_scatter( (const void *)v1, v2, v3, (MPI_Datatype)(v4.the_real_datatype), (MPI_Op)(v5.the_real_op), (MPI_Comm) the_real_comm ); if (err) { (this)->Call_errhandler( err ); }};
    }
    virtual void Scatter( const void * v1, int v2, const Datatype &v3, void * v4, int v5, const Datatype &v6, int v7 ) const
    {
        { int err = MPI_Scatter( (const void *)v1, v2, (MPI_Datatype)(v3.the_real_datatype), v4, v5, (MPI_Datatype)(v6.the_real_datatype), v7, (MPI_Comm) the_real_comm ); if (err) { (this)->Call_errhandler( err ); }};
    }
    virtual void Gather( const void * v1, int v2, const Datatype &v3, void * v4, int v5, const Datatype &v6, int v7 ) const
    {
        { int err = MPI_Gather( (const void *)v1, v2, (MPI_Datatype)(v3.the_real_datatype), v4, v5, (MPI_Datatype)(v6.the_real_datatype), v7, (MPI_Comm) the_real_comm ); if (err) { (this)->Call_errhandler( err ); }};
    }
    static void Free_keyval( int &v1 )
    {
        { int err = MPI_Comm_free_keyval( &v1 ) ; if (err) MPIR_Call_world_errhand( err ); };
    }
    virtual void Reduce( const void * v1, void * v2, int v3, const Datatype &v4, const Op &v5, int v6 ) const
    {
        { int err = MPI_Reduce( (const void *)v1, v2, v3, (MPI_Datatype)(v4.the_real_datatype), (MPI_Op)(v5.the_real_op), v6, (MPI_Comm) the_real_comm ); if (err) { (this)->Call_errhandler( err ); }};
    }
    virtual void Allgather( const void * v1, int v2, const Datatype &v3, void * v4, int v5, const Datatype &v6 ) const
    {
        { int err = MPI_Allgather( (const void *)v1, v2, (MPI_Datatype)(v3.the_real_datatype), v4, v5, (MPI_Datatype)(v6.the_real_datatype), (MPI_Comm) the_real_comm ); if (err) { (this)->Call_errhandler( err ); }};
    }
    virtual void Delete_attr( int v2 )
    {
        { int err = MPI_Comm_delete_attr( (MPI_Comm) the_real_comm, v2 ); if (err) { (this)->Call_errhandler( err ); }};
    }
    virtual void Scatterv( const void * v1, const int * v2, const int * v3, const Datatype &v4, void * v5, int v6, const Datatype &v7, int v8 ) const
    {
        { int err = MPI_Scatterv( (const void *)v1, (const int *)v2, (const int *)v3, (MPI_Datatype)(v4.the_real_datatype), v5, v6, (MPI_Datatype)(v7.the_real_datatype), v8, (MPI_Comm) the_real_comm ); if (err) { (this)->Call_errhandler( err ); }};
    }
    virtual void Get_name( char * v2, int &v3 ) const
    {
        { int err = MPI_Comm_get_name( (MPI_Comm) the_real_comm, v2, &v3 ); if (err) { (this)->Call_errhandler( err ); }};
    }
    virtual void Allgatherv( const void * v1, int v2, const Datatype &v3, void * v4, const int * v5, const int * v6, const Datatype &v7 ) const
    {
        { int err = MPI_Allgatherv( (const void *)v1, v2, (MPI_Datatype)(v3.the_real_datatype), v4, (const int *)v5, (const int *)v6, (MPI_Datatype)(v7.the_real_datatype), (MPI_Comm) the_real_comm ); if (err) { (this)->Call_errhandler( err ); }};
    }
    virtual Comm &Clone(void) const = 0;
    typedef int Copy_attr_function(const Comm& oldcomm, int comm_keyval, void* extra_state, void* attribute_val_in, void* attribute_val_out, bool& flag);
    typedef int Delete_attr_function(Comm& comm, int comm_keyval, void* attribute_val, void* extra_state);
    typedef void Errhandler_function(Comm &, int *, ... );
    typedef Errhandler_function Errhandler_fn;

    static int Create_keyval( Copy_attr_function *, Delete_attr_function *,
                              void * );

    static int NULL_COPY_FN( const Comm &oldcomm __attribute__((unused)),
           int keyval __attribute__((unused)), void *ex __attribute__((unused)),
           void *attr_in __attribute__((unused)), void *attr_out __attribute__((unused)),
    bool &flag ) { flag = 0; return 0;}
    static int NULL_DELETE_FN( Comm &comm __attribute__((unused)),
    int keyval __attribute__((unused)), void * attr __attribute__((unused)),
    void *ex __attribute__((unused)) ) { return 0; }
    static int DUP_FN( const Comm &oldcomm __attribute__((unused)),
           int keyval __attribute__((unused)), void *ex __attribute__((unused)),
           void *attr_in, void *attr_out, bool &flag ) { flag = 1;
                    *(void **)attr_out = attr_in; return 0;}
    static Errhandler Create_errhandler( Errhandler_function * );

    virtual void Call_errhandler( int v2 ) const;
    virtual void Reduce_scatter_block( const void * v1, void * v2, int v3, const Datatype &v4, const Op &v5 ) const
    {
        { int err = MPI_Reduce_scatter_block( (const void *)v1, v2, v3, (MPI_Datatype)(v4.the_real_datatype), (MPI_Op)(v5.the_real_op), (MPI_Comm) the_real_comm ); if (err) { (this)->Call_errhandler( err ); }};
    }
};

class Nullcomm : public Comm {

  public:


    inline Nullcomm(MPI_Comm obj) : Comm(obj) {}
    inline Nullcomm(void) : Comm() {}

    virtual ~Nullcomm() {}


    Nullcomm(const Nullcomm &obj) : Comm(obj) {}

    Nullcomm& operator=(const Nullcomm &obj) {
      the_real_comm = obj.the_real_comm; return *this; }


    inline operator MPI_Comm*() { return &the_real_comm; }
    inline operator MPI_Comm() const { return the_real_comm; }
    Nullcomm& operator=(const MPI_Comm& obj) {
      the_real_comm = obj; return *this; }
# 1579 "/cvmfs/software.hpc.rwth.de/Linux/RH8/x86_64/intel/sapphirerapids/software/impi/2021.9.0-intel-compilers-2023.1.0/mpi/2021.9.0/include/mpicxx.h"
    virtual Nullcomm & Clone(void) const {
        Nullcomm *clone = new Nullcomm();
        return *clone;
    }

};

class Intercomm : public Comm {
    friend class Intracomm;

  public:


    inline Intercomm(MPI_Comm obj) : Comm(obj) {}
    inline Intercomm(void) : Comm() {}

    virtual ~Intercomm() {}


    Intercomm(const Intercomm &obj) : Comm(obj) {}

    Intercomm& operator=(const Intercomm &obj) {
      the_real_comm = obj.the_real_comm; return *this; }


    inline operator MPI_Comm*() { return &the_real_comm; }
    inline operator MPI_Comm() const { return the_real_comm; }
    Intercomm& operator=(const MPI_Comm& obj) {
      the_real_comm = obj; return *this; }
    virtual Intracomm Merge( bool v2 ) const;
    virtual Group Get_remote_group( void ) const
    {
        Group v2;
        { int err = MPI_Comm_remote_group( (MPI_Comm) the_real_comm, &(v2.the_real_group) ); if (err) { (this)->Call_errhandler( err ); }};
        return v2;
    }
    virtual int Get_remote_size( void ) const
    {
        int v2;
        { int err = MPI_Comm_remote_size( (MPI_Comm) the_real_comm, &v2 ); if (err) { (this)->Call_errhandler( err ); }};
        return v2;
    }
    Intercomm Dup( void ) const
    {
        Intercomm v2;
        { int err = MPI_Comm_dup( (MPI_Comm) the_real_comm, &(v2.the_real_comm) ); if (err) { (this)->Call_errhandler( err ); }};
        return v2;
    }
    virtual Intercomm Split( int v2, int v3 ) const
    {
        Intercomm v4;
        { int err = MPI_Comm_split( (MPI_Comm) the_real_comm, v2, v3, &(v4.the_real_comm) ); if (err) { (this)->Call_errhandler( err ); }};
        return v4;
    }
# 1645 "/cvmfs/software.hpc.rwth.de/Linux/RH8/x86_64/intel/sapphirerapids/software/impi/2021.9.0-intel-compilers-2023.1.0/mpi/2021.9.0/include/mpicxx.h"
    virtual Intercomm & Clone(void) const {
        MPI_Comm ncomm;
        MPI_Comm_dup( (MPI_Comm)the_real_comm, &ncomm);
        Intercomm *clone = new Intercomm(ncomm);
        return *clone;
    }

};

class Intracomm : public Comm {
    friend class Cartcomm;
    friend class Graphcomm;
    friend class Datatype;

  public:


    inline Intracomm(MPI_Comm obj) : Comm(obj) {}
    inline Intracomm(void) : Comm() {}

    virtual ~Intracomm() {}


    Intracomm(const Intracomm &obj) : Comm(obj) {}

    Intracomm& operator=(const Intracomm &obj) {
      the_real_comm = obj.the_real_comm; return *this; }


    inline operator MPI_Comm*() { return &the_real_comm; }
    inline operator MPI_Comm() const { return the_real_comm; }
    Intracomm& operator=(const MPI_Comm& obj) {
      the_real_comm = obj; return *this; }
    virtual Intercomm Create_intercomm( int v2, const Comm &v3, int v4, int v5 ) const
    {
        Intercomm v6;
        { int err = MPI_Intercomm_create( (MPI_Comm) the_real_comm, v2, (MPI_Comm)(v3.the_real_comm), v4, v5, &(v6.the_real_comm) ); if (err) { (this)->Call_errhandler( err ); }};
        return v6;
    }
    virtual Intracomm Split( int v2, int v3 ) const
    {
        Intracomm v4;
        { int err = MPI_Comm_split( (MPI_Comm) the_real_comm, v2, v3, &(v4.the_real_comm) ); if (err) { (this)->Call_errhandler( err ); }};
        return v4;
    }
    virtual Graphcomm Create_graph( int v2, const int v3[], const int v4[], bool v5 ) const;
    virtual Cartcomm Create_cart( int v2, const int v3[], const bool v4[], bool v5 ) const;
    virtual Intracomm Create( const Group &v2 ) const
    {
        Intracomm v3;
        { int err = MPI_Comm_create( (MPI_Comm) the_real_comm, (MPI_Group)(v2.the_real_group), &(v3.the_real_comm) ); if (err) { (this)->Call_errhandler( err ); }};
        return v3;
    }
    Intracomm Dup( void ) const
    {
        Intracomm v2;
        { int err = MPI_Comm_dup( (MPI_Comm) the_real_comm, &(v2.the_real_comm) ); if (err) { (this)->Call_errhandler( err ); }};
        return v2;
    }
    virtual void Scan( const void * v1, void * v2, int v3, const Datatype &v4, const Op &v5 ) const
    {
        { int err = MPI_Scan( (const void *)v1, v2, v3, (MPI_Datatype)(v4.the_real_datatype), (MPI_Op)(v5.the_real_op), (MPI_Comm) the_real_comm ); if (err) { (this)->Call_errhandler( err ); }};
    }
    virtual void Exscan( const void * v1, void * v2, int v3, const Datatype &v4, const Op &v5 ) const
    {
        { int err = MPI_Exscan( (const void *)v1, v2, v3, (MPI_Datatype)(v4.the_real_datatype), (MPI_Op)(v5.the_real_op), (MPI_Comm) the_real_comm ); if (err) { (this)->Call_errhandler( err ); }};
    }
    virtual Intercomm Accept( const char * v1, const Info &v2, int v3 ) const
    {
        Intercomm v5;
        { int err = MPI_Comm_accept( (const char *)v1, (MPI_Info)(v2.the_real_info), v3, (MPI_Comm) the_real_comm, &(v5.the_real_comm) ); if (err) { (this)->Call_errhandler( err ); }};
        return v5;
    }
    virtual Intercomm Connect( const char * v1, const Info &v2, int v3 ) const
    {
        Intercomm v5;
        { int err = MPI_Comm_connect( (const char *)v1, (MPI_Info)(v2.the_real_info), v3, (MPI_Comm) the_real_comm, &(v5.the_real_comm) ); if (err) { (this)->Call_errhandler( err ); }};
        return v5;
    }
# 1736 "/cvmfs/software.hpc.rwth.de/Linux/RH8/x86_64/intel/sapphirerapids/software/impi/2021.9.0-intel-compilers-2023.1.0/mpi/2021.9.0/include/mpicxx.h"
    virtual Intracomm & Clone(void) const {
        MPI_Comm ncomm;
        MPI_Comm_dup( (MPI_Comm)the_real_comm, &ncomm);
        Intracomm *clone = new Intracomm(ncomm);
        return *clone;
    }


Intercomm Spawn(const char* command, const char* argv[], int maxprocs, const MPI::Info& info, int root) const {
    Intercomm ic;
    { int err = MPI_Comm_spawn( (char *)command, (char **)argv, maxprocs, info.the_real_info, root, the_real_comm, &(ic.the_real_comm), (int *)0 ); if (err) { (this)->Call_errhandler( err ); }};



    return ic;
}
Intercomm Spawn(const char* command, const char* argv[], int maxprocs, const MPI::Info& info, int root, int array_of_errcodes[]) const {
    Intercomm ic;
    { int err = MPI_Comm_spawn( (char *)command, (char **)argv, maxprocs, info.the_real_info, root, the_real_comm, &(ic.the_real_comm), array_of_errcodes ); if (err) { (this)->Call_errhandler( err ); }};



    return ic;
}
Intercomm Spawn_multiple(int count, const char* array_of_commands[], const char** array_of_argv[], const int array_of_maxprocs[], const MPI::Info array_of_info[], int root) {
    Intercomm ic;
    MPI_Info *li = new MPI_Info [count];
    int i;
    for (i=0; i<count; i++) {
        li[i] = array_of_info[i].the_real_info;
    }
    { int err = MPI_Comm_spawn_multiple( count, (char **)array_of_commands, (char ***)array_of_argv, (int *)array_of_maxprocs, li, root, the_real_comm, &(ic.the_real_comm), (int *)0 ); if (err) { (this)->Call_errhandler( err ); }};




    delete [] li;
    return ic;
}
Intercomm Spawn_multiple(int count, const char* array_of_commands[], const char** array_of_argv[], const int array_of_maxprocs[], const MPI::Info array_of_info[], int root, int array_of_errcodes[]) {
    Intercomm ic;
    MPI_Info *li = new MPI_Info [count];
    int i;
    for (i=0; i<count; i++) {
        li[i] = array_of_info[i].the_real_info;
    }
    { int err = MPI_Comm_spawn_multiple( count, (char **)array_of_commands, (char ***)array_of_argv, (int *)array_of_maxprocs, li, root, the_real_comm, &(ic.the_real_comm), array_of_errcodes ); if (err) { (this)->Call_errhandler( err ); }};




    delete [] li;
    return ic;
}

};

class Grequest : public Request {

  public:


    inline Grequest(MPI_Request obj) : Request(obj) {}
    inline Grequest(void) : Request() {}

    virtual ~Grequest() {}


    Grequest(const Grequest &obj) : Request(obj) {}

    Grequest& operator=(const Grequest &obj) {
      the_real_request = obj.the_real_request; return *this; }


    bool operator== (const Grequest &obj) {
      return (the_real_request == obj.the_real_request); }
    bool operator!= (const Grequest &obj) {
      return (the_real_request != obj.the_real_request); }

    inline operator MPI_Request*() { return &the_real_request; }
    inline operator MPI_Request() const { return the_real_request; }
    Grequest& operator=(const MPI_Request& obj) {
      the_real_request = obj; return *this; }
    virtual void Complete( void )
    {
        { int err = MPI_Grequest_complete( (MPI_Request) the_real_request ) ; if (err) MPIR_Call_world_errhand( err ); };
    }

    typedef int Query_function( void *, Status & );
    typedef int Free_function( void * );
    typedef int Cancel_function( void *, bool );

    Grequest Start( Query_function *query_fn,
                    Free_function *free_fn,
                    Cancel_function *cancel_fn,
                    void *extra_state );
};

class Win {

  protected:
    MPI_Win the_real_win;

  public:


    inline Win(MPI_Win obj) : the_real_win(obj) {}
    inline Win(void) : the_real_win(((MPI_Win)0x20000000)) {}

    virtual ~Win() {}


    Win(const Win &obj) : the_real_win(obj.the_real_win){}

    Win& operator=(const Win &obj) {
      the_real_win = obj.the_real_win; return *this; }


    bool operator== (const Win &obj) {
      return (the_real_win == obj.the_real_win); }
    bool operator!= (const Win &obj) {
      return (the_real_win != obj.the_real_win); }

    inline operator MPI_Win*() { return &the_real_win; }
    inline operator MPI_Win() const { return the_real_win; }
    Win& operator=(const MPI_Win& obj) {
      the_real_win = obj; return *this; }
    virtual Group Get_group( void ) const
    {
        Group v2;
        { int err = MPI_Win_get_group( (MPI_Win) the_real_win, &(v2.the_real_group) ); if (err) { (this)->Call_errhandler( err ); }};
        return v2;
    }
    virtual void Fence( int v1 ) const
    {
        { int err = MPI_Win_fence( v1, (MPI_Win) the_real_win ); if (err) { (this)->Call_errhandler( err ); }};
    }
    virtual void Start( const Group &v1, int v2 ) const
    {
        { int err = MPI_Win_start( (MPI_Group)(v1.the_real_group), v2, (MPI_Win) the_real_win ); if (err) { (this)->Call_errhandler( err ); }};
    }
    virtual void Free( void )
    {
        { int err = MPI_Win_free( (MPI_Win *) &the_real_win ); if (err) { (this)->Call_errhandler( err ); }};
    }
    virtual void Put( const void * v1, int v2, const Datatype &v3, int v4, Aint v5, int v6, const Datatype &v7 ) const
    {
        { int err = MPI_Put( (const void *)v1, v2, (MPI_Datatype)(v3.the_real_datatype), v4, v5, v6, (MPI_Datatype)(v7.the_real_datatype), (MPI_Win) the_real_win ); if (err) { (this)->Call_errhandler( err ); }};
    }
    virtual void Wait( void ) const
    {
        { int err = MPI_Win_wait( (MPI_Win) the_real_win ); if (err) { (this)->Call_errhandler( err ); }};
    }
    virtual bool Test( void ) const
    {
        int v2;
        { int err = MPI_Win_test( (MPI_Win) the_real_win, &v2 ); if (err) { (this)->Call_errhandler( err ); }};
        return v2!= 0;
    }
    virtual void Get( void * v1, int v2, const Datatype &v3, int v4, Aint v5, int v6, const Datatype &v7 ) const
    {
        { int err = MPI_Get( v1, v2, (MPI_Datatype)(v3.the_real_datatype), v4, v5, v6, (MPI_Datatype)(v7.the_real_datatype), (MPI_Win) the_real_win ); if (err) { (this)->Call_errhandler( err ); }};
    }
    virtual bool Get_attr( int v2, void * v3 ) const
    {
        int v4;
        { int err = MPI_Win_get_attr( (MPI_Win) the_real_win, v2, v3, &v4 ); if (err) { (this)->Call_errhandler( err ); }};
        return v4!= 0;
    }
    virtual void Set_attr( int v2, const void * v3 )
    {
        { int err = MPI_Win_set_attr( (MPI_Win) the_real_win, v2, (void *)v3 ); if (err) { (this)->Call_errhandler( err ); }};
    }
    virtual void Complete( void ) const
    {
        { int err = MPI_Win_complete( (MPI_Win) the_real_win ); if (err) { (this)->Call_errhandler( err ); }};
    }
    virtual void Set_errhandler( const Errhandler &v2 )
    {
        { int err = MPI_Win_set_errhandler( (MPI_Win) the_real_win, (MPI_Errhandler)(v2.the_real_errhandler) ); if (err) { (this)->Call_errhandler( err ); }};
    }
    virtual void Set_name( const char * v2 )
    {
        { int err = MPI_Win_set_name( (MPI_Win) the_real_win, (const char *)v2 ); if (err) { (this)->Call_errhandler( err ); }};
    }
    virtual void Accumulate( const void * v1, int v2, const Datatype &v3, int v4, Aint v5, int v6, const Datatype &v7, const Op &v8 ) const
    {
        { int err = MPI_Accumulate( (const void *)v1, v2, (MPI_Datatype)(v3.the_real_datatype), v4, v5, v6, (MPI_Datatype)(v7.the_real_datatype), (MPI_Op)(v8.the_real_op), (MPI_Win) the_real_win ); if (err) { (this)->Call_errhandler( err ); }};
    }
    static Win Create( const void * v1, Aint v2, int v3, const Info &v4, const Intracomm &v5 )
    {
        Win v6;
        { int err = MPI_Win_create( (void *)v1, v2, v3, (MPI_Info)(v4.the_real_info), (MPI_Comm)(v5.the_real_comm), &(v6.the_real_win) ); if (err) { (v5).Call_errhandler( err ); }};
        return v6;
    }
    static void Free_keyval( int &v1 )
    {
        { int err = MPI_Win_free_keyval( &v1 ) ; if (err) MPIR_Call_world_errhand( err ); };
    }
    virtual void Post( const Group &v1, int v2 ) const
    {
        { int err = MPI_Win_post( (MPI_Group)(v1.the_real_group), v2, (MPI_Win) the_real_win ); if (err) { (this)->Call_errhandler( err ); }};
    }
    virtual void Unlock( int v1 ) const
    {
        { int err = MPI_Win_unlock( v1, (MPI_Win) the_real_win ); if (err) { (this)->Call_errhandler( err ); }};
    }
    virtual void Delete_attr( int v2 )
    {
        { int err = MPI_Win_delete_attr( (MPI_Win) the_real_win, v2 ); if (err) { (this)->Call_errhandler( err ); }};
    }
    virtual void Lock( int v1, int v2, int v3 ) const
    {
        { int err = MPI_Win_lock( v1, v2, v3, (MPI_Win) the_real_win ); if (err) { (this)->Call_errhandler( err ); }};
    }
    virtual Errhandler Get_errhandler( void ) const
    {
        Errhandler v2;
        { int err = MPI_Win_get_errhandler( (MPI_Win) the_real_win, &(v2.the_real_errhandler) ); if (err) { (this)->Call_errhandler( err ); }};
        return v2;
    }
    virtual void Get_name( char * v2, int &v3 ) const
    {
        { int err = MPI_Win_get_name( (MPI_Win) the_real_win, v2, &v3 ); if (err) { (this)->Call_errhandler( err ); }};
    }
    typedef void Errhandler_function(Win &, int *, ... );
    typedef Errhandler_function Errhandler_fn;

    static Errhandler Create_errhandler( Errhandler_function * );

    typedef int Copy_attr_function(const Win& oldwin, int win_keyval, void* extra_state, void* attribute_val_in, void* attribute_val_out, bool& flag);
    typedef int Delete_attr_function(Win& win, int win_keyval, void* attribute_val, void* extra_state);

    static int Create_keyval( Copy_attr_function *, Delete_attr_function *,
                              void * );


    static int NULL_COPY_FN( const Win &oldwin __attribute__((unused)),
        int keyval __attribute__((unused)), void *ex __attribute__((unused)),
        void *attr_in __attribute__((unused)), void *attr_out __attribute__((unused)),
        bool &flag ) { flag = 1; return 0;}
    static int NULL_DELETE_FN( Win &win __attribute__((unused)),
        int keyval __attribute__((unused)), void * attr __attribute__((unused)),
        void *ex __attribute__((unused)) ) { return 0; }
    static int DUP_FN( const Win &oldwin __attribute__((unused)),
 int keyval __attribute__((unused)), void *ex __attribute__((unused)),
        void *attr_in, void *attr_out, bool &flag ) { flag = 1;
            *(void **)attr_out = attr_in; return 0;}

    virtual void Call_errhandler( int v2 ) const;
};





class File {

  protected:
    MPI_File the_real_file;

  public:


    inline File(MPI_File obj) : the_real_file(obj) {}
    inline File(void) : the_real_file(((MPI_File)0)) {}

    virtual ~File() {}


    File(const File &obj) : the_real_file(obj.the_real_file){}

    File& operator=(const File &obj) {
      the_real_file = obj.the_real_file; return *this; }


    bool operator== (const File &obj) {
      return (the_real_file == obj.the_real_file); }
    bool operator!= (const File &obj) {
      return (the_real_file != obj.the_real_file); }

    inline operator MPI_File*() { return &the_real_file; }
    inline operator MPI_File() const { return the_real_file; }
    File& operator=(const MPI_File& obj) {
      the_real_file = obj; return *this; }

    virtual Aint Get_type_extent( const Datatype &v2 ) const
    {
        MPI_Aint v3;
        { int err = MPI_File_get_type_extent( (MPI_File) the_real_file, (MPI_Datatype)(v2.the_real_datatype), &v3 ); if (err) { (this)->Call_errhandler( err ); }};
        return v3;
    }
    virtual void Read_ordered_end( void * v2, Status & v3 )
    {
        { int err = MPI_File_read_ordered_end( (MPI_File) the_real_file, v2, (MPI_Status *)&(v3.the_real_status ) ); if (err) { (this)->Call_errhandler( err ); }};
    }
    virtual void Read_ordered_end( void * v2 )
    {
        { int err = MPI_File_read_ordered_end( (MPI_File) the_real_file, v2, (MPI_Status *)1 ); if (err) { (this)->Call_errhandler( err ); }};
    }
    virtual void Seek_shared( Offset v2, int v3 )
    {
        { int err = MPI_File_seek_shared( (MPI_File) the_real_file, v2, v3 ); if (err) { (this)->Call_errhandler( err ); }};
    }
    virtual void Read_ordered( void * v2, int v3, const Datatype &v4, Status & v5 )
    {
        { int err = MPI_File_read_ordered( (MPI_File) the_real_file, v2, v3, (MPI_Datatype)(v4.the_real_datatype), (MPI_Status *)&(v5.the_real_status ) ); if (err) { (this)->Call_errhandler( err ); }};
    }
    virtual void Read_ordered( void * v2, int v3, const Datatype &v4 )
    {
        { int err = MPI_File_read_ordered( (MPI_File) the_real_file, v2, v3, (MPI_Datatype)(v4.the_real_datatype), (MPI_Status *)1 ); if (err) { (this)->Call_errhandler( err ); }};
    }
    virtual Request Iread_shared( void * v2, int v3, const Datatype &v4 )
    {
        Request v5;
        { int err = MPI_File_iread_shared( (MPI_File) the_real_file, v2, v3, (MPI_Datatype)(v4.the_real_datatype), (MPI_Request *)&(v5.the_real_request) ); if (err) { (this)->Call_errhandler( err ); }};
        return v5;
    }
    virtual Info Get_info( void ) const
    {
        Info v2;
        { int err = MPI_File_get_info( (MPI_File) the_real_file, &(v2.the_real_info) ); if (err) { (this)->Call_errhandler( err ); }};
        return v2;
    }
    virtual void Write_ordered_begin( const void * v2, int v3, const Datatype &v4 )
    {
        { int err = MPI_File_write_ordered_begin( (MPI_File) the_real_file, (const void *)v2, v3, (MPI_Datatype)(v4.the_real_datatype) ); if (err) { (this)->Call_errhandler( err ); }};
    }
    virtual void Set_info( const Info &v2 )
    {
        { int err = MPI_File_set_info( (MPI_File) the_real_file, (MPI_Info)(v2.the_real_info) ); if (err) { (this)->Call_errhandler( err ); }};
    }
    virtual void Write_ordered( const void * v2, int v3, const Datatype &v4, Status & v5 )
    {
        { int err = MPI_File_write_ordered( (MPI_File) the_real_file, (const void *)v2, v3, (MPI_Datatype)(v4.the_real_datatype), (MPI_Status *)&(v5.the_real_status ) ); if (err) { (this)->Call_errhandler( err ); }};
    }
    virtual void Write_ordered( const void * v2, int v3, const Datatype &v4 )
    {
        { int err = MPI_File_write_ordered( (MPI_File) the_real_file, (const void *)v2, v3, (MPI_Datatype)(v4.the_real_datatype), (MPI_Status *)1 ); if (err) { (this)->Call_errhandler( err ); }};
    }
    virtual void Sync( void )
    {
        { int err = MPI_File_sync( (MPI_File) the_real_file ); if (err) { (this)->Call_errhandler( err ); }};
    }
    virtual void Read( void * v2, int v3, const Datatype &v4, Status & v5 )
    {
        { int err = MPI_File_read( (MPI_File) the_real_file, v2, v3, (MPI_Datatype)(v4.the_real_datatype), (MPI_Status *)&(v5.the_real_status ) ); if (err) { (this)->Call_errhandler( err ); }};
    }
    virtual void Read( void * v2, int v3, const Datatype &v4 )
    {
        { int err = MPI_File_read( (MPI_File) the_real_file, v2, v3, (MPI_Datatype)(v4.the_real_datatype), (MPI_Status *)1 ); if (err) { (this)->Call_errhandler( err ); }};
    }
    virtual void Write_all( const void * v2, int v3, const Datatype &v4, Status & v5 )
    {
        { int err = MPI_File_write_all( (MPI_File) the_real_file, (const void *)v2, v3, (MPI_Datatype)(v4.the_real_datatype), (MPI_Status *)&(v5.the_real_status ) ); if (err) { (this)->Call_errhandler( err ); }};
    }
    virtual void Write_all( const void * v2, int v3, const Datatype &v4 )
    {
        { int err = MPI_File_write_all( (MPI_File) the_real_file, (const void *)v2, v3, (MPI_Datatype)(v4.the_real_datatype), (MPI_Status *)1 ); if (err) { (this)->Call_errhandler( err ); }};
    }
    virtual Offset Get_size( void ) const
    {
        MPI_Offset v2;
        { int err = MPI_File_get_size( (MPI_File) the_real_file, &v2 ); if (err) { (this)->Call_errhandler( err ); }};
        return v2;
    }
    virtual void Write_all_end( const void * v2, Status & v3 )
    {
        { int err = MPI_File_write_all_end( (MPI_File) the_real_file, (const void *)v2, (MPI_Status *)&(v3.the_real_status ) ); if (err) { (this)->Call_errhandler( err ); }};
    }
    virtual void Write_all_end( const void * v2 )
    {
        { int err = MPI_File_write_all_end( (MPI_File) the_real_file, (const void *)v2, (MPI_Status *)1 ); if (err) { (this)->Call_errhandler( err ); }};
    }
    static void Delete( const char * v1, const Info &v2 )
    {
        { int err = MPI_File_delete( (const char *)v1, (MPI_Info)(v2.the_real_info) ); if (err) { (FILE_NULL).Call_errhandler( err ); }};
    }
    virtual void Read_ordered_begin( void * v2, int v3, const Datatype &v4 )
    {
        { int err = MPI_File_read_ordered_begin( (MPI_File) the_real_file, v2, v3, (MPI_Datatype)(v4.the_real_datatype) ); if (err) { (this)->Call_errhandler( err ); }};
    }
    virtual Request Iread_at( Offset v2, void * v3, int v4, const Datatype &v5 )
    {
        Request v6;
        { int err = MPI_File_iread_at( (MPI_File) the_real_file, v2, v3, v4, (MPI_Datatype)(v5.the_real_datatype), (MPI_Request *)&(v6.the_real_request) ); if (err) { (this)->Call_errhandler( err ); }};
        return v6;
    }
    virtual void Write_at_all_end( const void * v2, Status & v3 )
    {
        { int err = MPI_File_write_at_all_end( (MPI_File) the_real_file, (const void *)v2, (MPI_Status *)&(v3.the_real_status ) ); if (err) { (this)->Call_errhandler( err ); }};
    }
    virtual void Write_at_all_end( const void * v2 )
    {
        { int err = MPI_File_write_at_all_end( (MPI_File) the_real_file, (const void *)v2, (MPI_Status *)1 ); if (err) { (this)->Call_errhandler( err ); }};
    }
    virtual Offset Get_position_shared( void ) const
    {
        MPI_Offset v2;
        { int err = MPI_File_get_position_shared( (MPI_File) the_real_file, &v2 ); if (err) { (this)->Call_errhandler( err ); }};
        return v2;
    }
    virtual void Write_shared( const void * v2, int v3, const Datatype &v4, Status & v5 )
    {
        { int err = MPI_File_write_shared( (MPI_File) the_real_file, (const void *)v2, v3, (MPI_Datatype)(v4.the_real_datatype), (MPI_Status *)&(v5.the_real_status ) ); if (err) { (this)->Call_errhandler( err ); }};
    }
    virtual void Write_shared( const void * v2, int v3, const Datatype &v4 )
    {
        { int err = MPI_File_write_shared( (MPI_File) the_real_file, (const void *)v2, v3, (MPI_Datatype)(v4.the_real_datatype), (MPI_Status *)1 ); if (err) { (this)->Call_errhandler( err ); }};
    }
    virtual Request Iwrite_at( Offset v2, const void * v3, int v4, const Datatype &v5 )
    {
        Request v6;
        { int err = MPI_File_iwrite_at( (MPI_File) the_real_file, (MPI_Offset)v2, (const void *)v3, v4, (MPI_Datatype)(v5.the_real_datatype), (MPI_Request *)&(v6.the_real_request) ); if (err) { (this)->Call_errhandler( err ); }};
        return v6;
    }
    virtual void Get_view( Offset & v2, Datatype &v3, Datatype &v4, char * v5 ) const
    {
        { int err = MPI_File_get_view( (MPI_File) the_real_file, &v2, (MPI_Datatype *)&(v3.the_real_datatype), (MPI_Datatype *)&(v4.the_real_datatype), v5 ); if (err) { (this)->Call_errhandler( err ); }};
    }
    virtual void Write_all_begin( const void * v2, int v3, const Datatype &v4 )
    {
        { int err = MPI_File_write_all_begin( (MPI_File) the_real_file, (const void *)v2, v3, (MPI_Datatype)(v4.the_real_datatype) ); if (err) { (this)->Call_errhandler( err ); }};
    }
    virtual void Read_all_end( void * v2, Status & v3 )
    {
        { int err = MPI_File_read_all_end( (MPI_File) the_real_file, v2, (MPI_Status *)&(v3.the_real_status ) ); if (err) { (this)->Call_errhandler( err ); }};
    }
    virtual void Read_all_end( void * v2 )
    {
        { int err = MPI_File_read_all_end( (MPI_File) the_real_file, v2, (MPI_Status *)1 ); if (err) { (this)->Call_errhandler( err ); }};
    }
    virtual Offset Get_byte_offset( const Offset v2 ) const
    {
        MPI_Offset v3;
        { int err = MPI_File_get_byte_offset( (MPI_File) the_real_file, (MPI_Offset)v2, &v3 ); if (err) { (this)->Call_errhandler( err ); }};
        return v3;
    }
    virtual Request Iread( void * v2, int v3, const Datatype &v4 )
    {
        Request v5;
        { int err = MPI_File_iread( (MPI_File) the_real_file, v2, v3, (MPI_Datatype)(v4.the_real_datatype), (MPI_Request *)&(v5.the_real_request) ); if (err) { (this)->Call_errhandler( err ); }};
        return v5;
    }
    virtual void Read_at_all_end( void * v2, Status & v3 )
    {
        { int err = MPI_File_read_at_all_end( (MPI_File) the_real_file, v2, (MPI_Status *)&(v3.the_real_status ) ); if (err) { (this)->Call_errhandler( err ); }};
    }
    virtual void Read_at_all_end( void * v2 )
    {
        { int err = MPI_File_read_at_all_end( (MPI_File) the_real_file, v2, (MPI_Status *)1 ); if (err) { (this)->Call_errhandler( err ); }};
    }
    virtual void Write_at( Offset v2, const void * v3, int v4, const Datatype &v5, Status & v6 )
    {
        { int err = MPI_File_write_at( (MPI_File) the_real_file, v2, (const void *)v3, v4, (MPI_Datatype)(v5.the_real_datatype), (MPI_Status *)&(v6.the_real_status ) ); if (err) { (this)->Call_errhandler( err ); }};
    }
    virtual void Write_at( Offset v2, const void * v3, int v4, const Datatype &v5 )
    {
        { int err = MPI_File_write_at( (MPI_File) the_real_file, v2, (const void *)v3, v4, (MPI_Datatype)(v5.the_real_datatype), (MPI_Status *)1 ); if (err) { (this)->Call_errhandler( err ); }};
    }
    virtual void Write_at_all_begin( Offset v2, const void * v3, int v4, const Datatype &v5 )
    {
        { int err = MPI_File_write_at_all_begin( (MPI_File) the_real_file, v2, (const void *)v3, v4, (MPI_Datatype)(v5.the_real_datatype) ); if (err) { (this)->Call_errhandler( err ); }};
    }
    virtual Errhandler Get_errhandler( void ) const
    {
        Errhandler v2;
        { int err = MPI_File_get_errhandler( (MPI_File) the_real_file, &(v2.the_real_errhandler) ); if (err) { (this)->Call_errhandler( err ); }};
        return v2;
    }
    virtual int Get_amode( void ) const
    {
        int v2;
        { int err = MPI_File_get_amode( (MPI_File) the_real_file, &v2 ); if (err) { (this)->Call_errhandler( err ); }};
        return v2;
    }
    virtual void Set_atomicity( bool v2 )
    {
        int l2;
         l2 = (v2 == true) ? 1 : 0;
        { int err = MPI_File_set_atomicity( (MPI_File) the_real_file, l2 ); if (err) { (this)->Call_errhandler( err ); }};
    }
    virtual Group Get_group( void ) const
    {
        Group v2;
        { int err = MPI_File_get_group( (MPI_File) the_real_file, &(v2.the_real_group) ); if (err) { (this)->Call_errhandler( err ); }};
        return v2;
    }
    virtual Offset Get_position( void ) const
    {
        MPI_Offset v2;
        { int err = MPI_File_get_position( (MPI_File) the_real_file, &v2 ); if (err) { (this)->Call_errhandler( err ); }};
        return v2;
    }
    static File Open( const Intracomm &v1, const char * v2, int v3, const Info &v4 )
    {
        File v5;
        { int err = MPI_File_open( (MPI_Comm)(v1.the_real_comm), (const char *)v2, v3, (MPI_Info)(v4.the_real_info), &(v5.the_real_file) ); if (err) { (FILE_NULL).Call_errhandler( err ); }};
        return v5;
    }
    virtual void Seek( Offset v2, int v3 )
    {
        { int err = MPI_File_seek( (MPI_File) the_real_file, v2, v3 ); if (err) { (this)->Call_errhandler( err ); }};
    }
    virtual void Read_all_begin( void * v2, int v3, const Datatype &v4 )
    {
        { int err = MPI_File_read_all_begin( (MPI_File) the_real_file, v2, v3, (MPI_Datatype)(v4.the_real_datatype) ); if (err) { (this)->Call_errhandler( err ); }};
    }
    virtual void Read_at_all_begin( Offset v2, void * v3, int v4, const Datatype &v5 )
    {
        { int err = MPI_File_read_at_all_begin( (MPI_File) the_real_file, v2, v3, v4, (MPI_Datatype)(v5.the_real_datatype) ); if (err) { (this)->Call_errhandler( err ); }};
    }
    virtual void Read_all( void * v2, int v3, const Datatype &v4, Status & v5 )
    {
        { int err = MPI_File_read_all( (MPI_File) the_real_file, v2, v3, (MPI_Datatype)(v4.the_real_datatype), (MPI_Status *)&(v5.the_real_status ) ); if (err) { (this)->Call_errhandler( err ); }};
    }
    virtual void Read_all( void * v2, int v3, const Datatype &v4 )
    {
        { int err = MPI_File_read_all( (MPI_File) the_real_file, v2, v3, (MPI_Datatype)(v4.the_real_datatype), (MPI_Status *)1 ); if (err) { (this)->Call_errhandler( err ); }};
    }
    virtual void Preallocate( Offset v2 )
    {
        { int err = MPI_File_preallocate( (MPI_File) the_real_file, v2 ); if (err) { (this)->Call_errhandler( err ); }};
    }
    virtual void Read_at_all( Offset v2, void * v3, int v4, const Datatype &v5, Status & v6 )
    {
        { int err = MPI_File_read_at_all( (MPI_File) the_real_file, v2, v3, v4, (MPI_Datatype)(v5.the_real_datatype), (MPI_Status *)&(v6.the_real_status ) ); if (err) { (this)->Call_errhandler( err ); }};
    }
    virtual void Read_at_all( Offset v2, void * v3, int v4, const Datatype &v5 )
    {
        { int err = MPI_File_read_at_all( (MPI_File) the_real_file, v2, v3, v4, (MPI_Datatype)(v5.the_real_datatype), (MPI_Status *)1 ); if (err) { (this)->Call_errhandler( err ); }};
    }
    virtual void Read_shared( void * v2, int v3, const Datatype &v4, Status & v5 )
    {
        { int err = MPI_File_read_shared( (MPI_File) the_real_file, v2, v3, (MPI_Datatype)(v4.the_real_datatype), (MPI_Status *)&(v5.the_real_status ) ); if (err) { (this)->Call_errhandler( err ); }};
    }
    virtual void Read_shared( void * v2, int v3, const Datatype &v4 )
    {
        { int err = MPI_File_read_shared( (MPI_File) the_real_file, v2, v3, (MPI_Datatype)(v4.the_real_datatype), (MPI_Status *)1 ); if (err) { (this)->Call_errhandler( err ); }};
    }
    virtual Request Iwrite( const void * v2, int v3, const Datatype &v4 )
    {
        Request v5;
        { int err = MPI_File_iwrite( (MPI_File) the_real_file, (const void *)v2, v3, (MPI_Datatype)(v4.the_real_datatype), (MPI_Request *)&(v5.the_real_request) ); if (err) { (this)->Call_errhandler( err ); }};
        return v5;
    }
    virtual Request Iwrite_shared( const void * v2, int v3, const Datatype &v4 )
    {
        Request v5;
        { int err = MPI_File_iwrite_shared( (MPI_File) the_real_file, (const void *)v2, v3, (MPI_Datatype)(v4.the_real_datatype), (MPI_Request *)&(v5.the_real_request) ); if (err) { (this)->Call_errhandler( err ); }};
        return v5;
    }
    virtual void Set_errhandler( const Errhandler &v2 )
    {
        { int err = MPI_File_set_errhandler( (MPI_File) the_real_file, (MPI_Errhandler)(v2.the_real_errhandler) ); if (err) { (this)->Call_errhandler( err ); }};
    }
    virtual void Write_at_all( Offset v2, const void * v3, int v4, const Datatype &v5, Status & v6 )
    {
        { int err = MPI_File_write_at_all( (MPI_File) the_real_file, v2, (const void *)v3, v4, (MPI_Datatype)(v5.the_real_datatype), (MPI_Status *)&(v6.the_real_status ) ); if (err) { (this)->Call_errhandler( err ); }};
    }
    virtual void Write_at_all( Offset v2, const void * v3, int v4, const Datatype &v5 )
    {
        { int err = MPI_File_write_at_all( (MPI_File) the_real_file, v2, (const void *)v3, v4, (MPI_Datatype)(v5.the_real_datatype), (MPI_Status *)1 ); if (err) { (this)->Call_errhandler( err ); }};
    }
    virtual void Set_size( Offset v2 )
    {
        { int err = MPI_File_set_size( (MPI_File) the_real_file, v2 ); if (err) { (this)->Call_errhandler( err ); }};
    }
    virtual void Set_view( Offset v2, const Datatype &v3, const Datatype v4, const char * v5, const Info &v6 )
    {
        { int err = MPI_File_set_view( (MPI_File) the_real_file, v2, (MPI_Datatype)(v3.the_real_datatype), (MPI_Datatype)v4, (const char *)v5, (MPI_Info)(v6.the_real_info) ); if (err) { (this)->Call_errhandler( err ); }};
    }
    virtual void Read_at( Offset v2, void * v3, int v4, const Datatype &v5, Status & v6 )
    {
        { int err = MPI_File_read_at( (MPI_File) the_real_file, v2, v3, v4, (MPI_Datatype)(v5.the_real_datatype), (MPI_Status *)&(v6.the_real_status ) ); if (err) { (this)->Call_errhandler( err ); }};
    }
    virtual void Read_at( Offset v2, void * v3, int v4, const Datatype &v5 )
    {
        { int err = MPI_File_read_at( (MPI_File) the_real_file, v2, v3, v4, (MPI_Datatype)(v5.the_real_datatype), (MPI_Status *)1 ); if (err) { (this)->Call_errhandler( err ); }};
    }
    virtual void Close( void )
    {
        { int err = MPI_File_close( (MPI_File *) &the_real_file ); if (err) { (this)->Call_errhandler( err ); }};
    }
    virtual void Write_ordered_end( const void * v2, Status & v3 )
    {
        { int err = MPI_File_write_ordered_end( (MPI_File) the_real_file, (const void *)v2, (MPI_Status *)&(v3.the_real_status ) ); if (err) { (this)->Call_errhandler( err ); }};
    }
    virtual void Write_ordered_end( const void * v2 )
    {
        { int err = MPI_File_write_ordered_end( (MPI_File) the_real_file, (const void *)v2, (MPI_Status *)1 ); if (err) { (this)->Call_errhandler( err ); }};
    }
    virtual void Write( const void * v2, int v3, const Datatype &v4, Status & v5 )
    {
        { int err = MPI_File_write( (MPI_File) the_real_file, (const void *)v2, v3, (MPI_Datatype)(v4.the_real_datatype), (MPI_Status *)&(v5.the_real_status ) ); if (err) { (this)->Call_errhandler( err ); }};
    }
    virtual void Write( const void * v2, int v3, const Datatype &v4 )
    {
        { int err = MPI_File_write( (MPI_File) the_real_file, (const void *)v2, v3, (MPI_Datatype)(v4.the_real_datatype), (MPI_Status *)1 ); if (err) { (this)->Call_errhandler( err ); }};
    }
    virtual bool Get_atomicity( void ) const
    {
        int v2;
        { int err = MPI_File_get_atomicity( (MPI_File) the_real_file, &v2 ); if (err) { (this)->Call_errhandler( err ); }};
        return v2!= 0;
    }
    typedef void Errhandler_function(File &, int *, ... );
    typedef Errhandler_function Errhandler_fn;

    static Errhandler Create_errhandler( Errhandler_function * );

    virtual void Call_errhandler( int v2 ) const;

};

class Graphcomm : public Intracomm {

  public:


    inline Graphcomm(MPI_Comm obj) : Intracomm(obj) {}
    inline Graphcomm(void) : Intracomm() {}

    virtual ~Graphcomm() {}


    Graphcomm(const Graphcomm &obj) : Intracomm(obj) {}

    Graphcomm& operator=(const Graphcomm &obj) {
      the_real_comm = obj.the_real_comm; return *this; }


    inline operator MPI_Comm*() { return &the_real_comm; }
    inline operator MPI_Comm() const { return the_real_comm; }
    Graphcomm& operator=(const MPI_Comm& obj) {
      the_real_comm = obj; return *this; }
    virtual void Get_dims( int * v2, int * v3 ) const
    {
        { int err = MPI_Graphdims_get( (MPI_Comm) the_real_comm, v2, v3 ); if (err) { (this)->Call_errhandler( err ); }};
    }
    virtual void Get_topo( int v2, int v3, int v4[], int v5[] ) const
    {
        { int err = MPI_Graph_get( (MPI_Comm) the_real_comm, v2, v3, v4, v5 ); if (err) { (this)->Call_errhandler( err ); }};
    }
    virtual int Map( int v2, const int v3[], const int v4[] ) const
    {
        int v5;
        { int err = MPI_Graph_map( (MPI_Comm) the_real_comm, v2, (const int *)v3, (const int *)v4, &v5 ); if (err) { (this)->Call_errhandler( err ); }};
        return v5;
    }
    virtual void Get_neighbors( int v2, int v3, int v4[] ) const
    {
        { int err = MPI_Graph_neighbors( (MPI_Comm) the_real_comm, v2, v3, v4 ); if (err) { (this)->Call_errhandler( err ); }};
    }
    virtual int Get_neighbors_count( int v2 ) const
    {
        int v3;
        { int err = MPI_Graph_neighbors_count( (MPI_Comm) the_real_comm, v2, &v3 ); if (err) { (this)->Call_errhandler( err ); }};
        return v3;
    }
    Graphcomm Dup( void ) const
    {
        Graphcomm v2;
        { int err = MPI_Comm_dup( (MPI_Comm) the_real_comm, &(v2.the_real_comm) ); if (err) { (this)->Call_errhandler( err ); }};
        return v2;
    }
# 2414 "/cvmfs/software.hpc.rwth.de/Linux/RH8/x86_64/intel/sapphirerapids/software/impi/2021.9.0-intel-compilers-2023.1.0/mpi/2021.9.0/include/mpicxx.h"
    virtual Graphcomm & Clone(void) const {
        MPI_Comm ncomm;
        MPI_Comm_dup( (MPI_Comm)the_real_comm, &ncomm);
        Graphcomm *clone = new Graphcomm(ncomm);
        return *clone;
    }

};

class Cartcomm : public Intracomm {

  public:


    inline Cartcomm(MPI_Comm obj) : Intracomm(obj) {}
    inline Cartcomm(void) : Intracomm() {}

    virtual ~Cartcomm() {}


    Cartcomm(const Cartcomm &obj) : Intracomm(obj) {}

    Cartcomm& operator=(const Cartcomm &obj) {
      the_real_comm = obj.the_real_comm; return *this; }


    inline operator MPI_Comm*() { return &the_real_comm; }
    inline operator MPI_Comm() const { return the_real_comm; }
    Cartcomm& operator=(const MPI_Comm& obj) {
      the_real_comm = obj; return *this; }
    virtual void Get_coords( int v2, int v3, int v4[] ) const
    {
        { int err = MPI_Cart_coords( (MPI_Comm) the_real_comm, v2, v3, v4 ); if (err) { (this)->Call_errhandler( err ); }};
    }
    virtual int Get_cart_rank( const int v2[] ) const
    {
        int v3;
        { int err = MPI_Cart_rank( (MPI_Comm) the_real_comm, (const int *)v2, &v3 ); if (err) { (this)->Call_errhandler( err ); }};
        return v3;
    }
    virtual int Get_dim( void ) const
    {
        int v2;
        { int err = MPI_Cartdim_get( (MPI_Comm) the_real_comm, &v2 ); if (err) { (this)->Call_errhandler( err ); }};
        return v2;
    }
    Cartcomm Dup( void ) const
    {
        Cartcomm v2;
        { int err = MPI_Comm_dup( (MPI_Comm) the_real_comm, &(v2.the_real_comm) ); if (err) { (this)->Call_errhandler( err ); }};
        return v2;
    }
    virtual void Get_topo( int v2, int v3[], bool v4[], int v5[] ) const
    {
        int *l4 = new int[v2];
        { int err = MPI_Cart_get( (MPI_Comm) the_real_comm, v2, v3, l4, v5 ); if (err) { (this)->Call_errhandler( err ); }};
        {
            int i4;
            for (i4=0;i4<v2;i4++) {





                v4[i4] = l4[i4] != 0;
            }
            delete[] l4;
        }
    }
    virtual int Map( int v2, const int v3[], const bool v4[] ) const
    {
        int v5;
        int *l4 = new int[v2];
        {
            int i4;
            for (i4=0;i4<v2;i4++) {
                l4[i4] = v4[i4] == true ? 1 : 0;
            }
        }
        { int err = MPI_Cart_map( (MPI_Comm) the_real_comm, v2, (const int *)v3, l4, &v5 ); if (err) { (this)->Call_errhandler( err ); }};

            delete[] l4;
        return v5;
    }
    virtual Cartcomm Sub( const bool v2[] ) const
    {
        Cartcomm v3;
        int *l2 = new int[10];
        {
            int i2;
            for (i2=0;i2<10;i2++) {
                l2[i2] = v2[i2] == true ? 1 : 0;
            }
        }
        { int err = MPI_Cart_sub( (MPI_Comm) the_real_comm, l2, &(v3.the_real_comm) ); if (err) { (this)->Call_errhandler( err ); }};

            delete[] l2;
        return v3;
    }
    virtual void Shift( int v2, int v3, int &v4, int &v5 ) const
    {
        { int err = MPI_Cart_shift( (MPI_Comm) the_real_comm, v2, v3, &v4, &v5 ); if (err) { (this)->Call_errhandler( err ); }};
    }
# 2529 "/cvmfs/software.hpc.rwth.de/Linux/RH8/x86_64/intel/sapphirerapids/software/impi/2021.9.0-intel-compilers-2023.1.0/mpi/2021.9.0/include/mpicxx.h"
    virtual Cartcomm & Clone(void) const {
        MPI_Comm ncomm;
        MPI_Comm_dup( (MPI_Comm)the_real_comm, &ncomm);
        Cartcomm *clone = new Cartcomm(ncomm);
        return *clone;
    }

};
extern int Add_error_class( void ) ;
extern void* Alloc_mem( Aint v1, const Info &v2 ) ;
extern void Lookup_name( const char * v1, const Info &v2, char * v3 ) ;
extern void Publish_name( const char * v1, const Info &v2, const char * v3 ) ;
extern void Unpublish_name( const char * v1, const Info &v2, const char * v3 ) ;
extern Aint Get_address( const void * v1 ) ;
extern void Add_error_string( int v1, const char * v2 ) ;
extern int Query_thread( void ) ;
extern void Close_port( const char * v1 ) ;
extern int Add_error_code( int v1 ) ;
extern void Free_mem( void * v1 ) ;
extern void Open_port( const Info &v1, char * v2 ) ;
extern bool Is_finalized( void ) ;
extern bool Is_thread_main( void ) ;


typedef int Datarep_extent_function( const Datatype&, Aint&, void *);
typedef int Datarep_conversion_function( void *, Datatype &, int, void *,
                Offset, void * );



extern Datatype CHAR;
extern Datatype UNSIGNED_CHAR;
extern Datatype BYTE;
extern Datatype SHORT;
extern Datatype UNSIGNED_SHORT;
extern Datatype INT;
extern Datatype UNSIGNED;
extern Datatype LONG;
extern Datatype UNSIGNED_LONG;
extern Datatype FLOAT;
extern Datatype DOUBLE;
extern Datatype LONG_DOUBLE;
extern Datatype LONG_LONG_INT;
extern Datatype LONG_LONG;
extern Datatype PACKED;
extern Datatype LB;
extern Datatype UB;
extern Datatype FLOAT_INT;
extern Datatype DOUBLE_INT;
extern Datatype LONG_INT;
extern Datatype SHORT_INT;
extern Datatype LONG_DOUBLE_INT;
extern Datatype REAL4;
extern Datatype REAL8;
extern Datatype REAL16;
extern Datatype COMPLEX8;
extern Datatype COMPLEX16;
extern Datatype COMPLEX32;
extern Datatype INTEGER1;
extern Datatype INTEGER2;
extern Datatype INTEGER4;
extern Datatype INTEGER8;
extern Datatype INTEGER16;
extern Datatype WCHAR;
extern Datatype SIGNED_CHAR;
extern Datatype UNSIGNED_LONG_LONG;
extern Datatype TWOINT;
extern Datatype BOOL;

extern Datatype COMPLEX;

extern Datatype DOUBLE_COMPLEX;

extern Datatype LONG_DOUBLE_COMPLEX;

extern Datatype DATATYPE_NULL;


extern Datatype INTEGER;
extern Datatype REAL;
extern Datatype DOUBLE_PRECISION;
extern Datatype F_COMPLEX;
extern Datatype F_DOUBLE_COMPLEX;
extern Datatype LOGICAL;
extern Datatype CHARACTER;
extern Datatype TWOREAL;
extern Datatype TWODOUBLE_PRECISION;
extern Datatype TWOINTEGER;

extern const Op MAX;
extern const Op MIN;
extern const Op SUM;
extern const Op PROD;
extern const Op LAND;
extern const Op BAND;
extern const Op LOR;
extern const Op BOR;
extern const Op LXOR;
extern const Op BXOR;
extern const Op MINLOC;
extern const Op MAXLOC;
extern const Op REPLACE;
extern const Op OP_NULL;
extern Intracomm COMM_SELF;
extern const Group GROUP_EMPTY;
extern const Nullcomm COMM_NULL;
extern const Group GROUP_NULL;
extern const Request REQUEST_NULL;
extern const Errhandler ERRHANDLER_NULL;
extern const Errhandler ERRORS_RETURN;
extern const Errhandler ERRORS_ARE_FATAL;
extern const Errhandler ERRORS_THROW_EXCEPTIONS;
extern const Info INFO_NULL;
extern const Win WIN_NULL;
extern const int BSEND_OVERHEAD;
extern const int KEYVAL_INVALID;
extern const int CART;
extern const int GRAPH;
extern const int IDENT;
extern const int SIMILAR;
extern const int CONGRUENT;
extern const int UNEQUAL;
extern const int PROC_NULL;
extern const int ANY_TAG;
extern const int ANY_SOURCE;
extern const int ROOT;
extern const int TAG_UB;
extern const int IO;
extern const int HOST;
extern const int WTIME_IS_GLOBAL;
extern const int UNIVERSE_SIZE;
extern const int LASTUSEDCODE;
extern const int APPNUM;
extern const int MAX_PROCESSOR_NAME;
extern const int MAX_ERROR_STRING;
extern const int MAX_PORT_NAME;
extern const int MAX_OBJECT_NAME;
extern const int MAX_INFO_VAL;
extern const int MAX_INFO_KEY;
extern const int UNDEFINED;
extern const int LOCK_EXCLUSIVE;
extern const int LOCK_SHARED;
extern const int WIN_BASE;
extern const int WIN_DISP_UNIT;
extern const int WIN_SIZE;
extern const int SUCCESS;
extern const int ERR_BUFFER;
extern const int ERR_COUNT;
extern const int ERR_TYPE;
extern const int ERR_TAG;
extern const int ERR_COMM;
extern const int ERR_RANK;
extern const int ERR_REQUEST;
extern const int ERR_ROOT;
extern const int ERR_GROUP;
extern const int ERR_OP;
extern const int ERR_TOPOLOGY;
extern const int ERR_DIMS;
extern const int ERR_ARG;
extern const int ERR_UNKNOWN;
extern const int ERR_TRUNCATE;
extern const int ERR_OTHER;
extern const int ERR_INTERN;
extern const int ERR_PENDING;
extern const int ERR_IN_STATUS;
extern const int ERR_LASTCODE;
extern const int ERR_FILE;
extern const int ERR_ACCESS;
extern const int ERR_AMODE;
extern const int ERR_BAD_FILE;
extern const int ERR_FILE_EXISTS;
extern const int ERR_FILE_IN_USE;
extern const int ERR_NO_SPACE;
extern const int ERR_NO_SUCH_FILE;
extern const int ERR_IO;
extern const int ERR_READ_ONLY;
extern const int ERR_CONVERSION;
extern const int ERR_DUP_DATAREP;
extern const int ERR_UNSUPPORTED_DATAREP;
extern const int ERR_INFO;
extern const int ERR_INFO_KEY;
extern const int ERR_INFO_VALUE;
extern const int ERR_INFO_NOKEY;
extern const int ERR_NAME;
extern const int ERR_NO_MEM;
extern const int ERR_NOT_SAME;
extern const int ERR_PORT;
extern const int ERR_QUOTA;
extern const int ERR_SERVICE;
extern const int ERR_SPAWN;
extern const int ERR_UNSUPPORTED_OPERATION;
extern const int ERR_WIN;
extern const int ERR_BASE;
extern const int ERR_LOCKTYPE;
extern const int ERR_KEYVAL;
extern const int ERR_RMA_CONFLICT;
extern const int ERR_RMA_SYNC;
extern const int ERR_SIZE;
extern const int ERR_DISP;
extern const int ERR_ASSERT;
extern const int TYPECLASS_REAL;
extern const int TYPECLASS_INTEGER;
extern const int TYPECLASS_COMPLEX;





extern const int DISTRIBUTE_BLOCK;
extern const int DISTRIBUTE_CYCLIC;
extern const int DISTRIBUTE_DFLT_DARG;
extern const int DISTRIBUTE_NONE;
extern const int ORDER_C;
extern const int ORDER_FORTRAN;


extern const int MAX_DATAREP_STRING;
extern const MPI_Offset DISPLACEMENT_CURRENT;
extern const int MODE_APPEND;
extern const int MODE_CREATE;
extern const int MODE_DELETE_ON_CLOSE;
extern const int MODE_EXCL;
extern const int MODE_RDONLY;
extern const int MODE_RDWR;
extern const int MODE_SEQUENTIAL;
extern const int MODE_UNIQUE_OPEN;
extern const int MODE_WRONLY;

extern const int MODE_NOCHECK;
extern const int MODE_NOPRECEDE;
extern const int MODE_NOPUT;
extern const int MODE_NOSTORE;
extern const int MODE_NOSUCCEED;
extern const int COMM_TYPE_SHARED;
extern const int COMBINER_CONTIGUOUS;
extern const int COMBINER_DARRAY;
extern const int COMBINER_DUP;
extern const int COMBINER_F90_COMPLEX;
extern const int COMBINER_F90_INTEGER;
extern const int COMBINER_F90_REAL;
extern const int COMBINER_HINDEXED_INTEGER;
extern const int COMBINER_HINDEXED;
extern const int COMBINER_HVECTOR_INTEGER;
extern const int COMBINER_HVECTOR;
extern const int COMBINER_INDEXED_BLOCK;
extern const int COMBINER_INDEXED;
extern const int COMBINER_NAMED;
extern const int COMBINER_RESIZED;
extern const int COMBINER_STRUCT_INTEGER;
extern const int COMBINER_STRUCT;
extern const int COMBINER_SUBARRAY;
extern const int COMBINER_VECTOR;
extern const int COMBINER_HINDEXED_BLOCK;
extern const int THREAD_FUNNELED;
extern const int THREAD_MULTIPLE;
extern const int THREAD_SERIALIZED;
extern const int THREAD_SINGLE;
extern const char ** const ARGV_NULL;
extern const char *** const ARGVS_NULL;
extern void * const BOTTOM;
extern void * const IN_PLACE;
extern void Init(void);
extern void Init(int &, char **& );
extern int Init_thread(int);
extern int Init_thread(int &, char **&, int );
extern double Wtime(void);
extern double Wtick(void);
}
# 2491 "/cvmfs/software.hpc.rwth.de/Linux/RH8/x86_64/intel/sapphirerapids/software/impi/2021.9.0-intel-compilers-2023.1.0/mpi/2021.9.0/include/mpi.h" 2





typedef int MPIX_Grequest_class;
int MPIX_Grequest_class_create(MPI_Grequest_query_function *query_fn,
                               MPI_Grequest_free_function *free_fn,
                               MPI_Grequest_cancel_function *cancel_fn,
                               MPIX_Grequest_poll_function *poll_fn,
                               MPIX_Grequest_wait_function *wait_fn,
                               MPIX_Grequest_class *greq_class) ;
int MPIX_Grequest_class_allocate(MPIX_Grequest_class greq_class, void *extra_state,
                                 MPI_Request *request) ;
int MPIX_Grequest_start(MPI_Grequest_query_function *query_fn,
                        MPI_Grequest_free_function *free_fn,
                        MPI_Grequest_cancel_function *cancel_fn,
                        MPIX_Grequest_poll_function *poll_fn,
                        MPIX_Grequest_wait_function *wait_fn, void *extra_state,
                        MPI_Request *request) ;



int PMPIX_Grequest_class_create(MPI_Grequest_query_function *query_fn,
                                MPI_Grequest_free_function *free_fn,
                                MPI_Grequest_cancel_function *cancel_fn,
                                MPIX_Grequest_poll_function *poll_fn,
                                MPIX_Grequest_wait_function *wait_fn,
                                MPIX_Grequest_class *greq_class) ;
int PMPIX_Grequest_class_allocate(MPIX_Grequest_class greq_class, void *extra_state,
                                  MPI_Request *request) ;
int PMPIX_Grequest_start(MPI_Grequest_query_function *query_fn,
                         MPI_Grequest_free_function *free_fn,
                         MPI_Grequest_cancel_function *cancel_fn,
                         MPIX_Grequest_poll_function *poll_fn,
                         MPIX_Grequest_wait_function *wait_fn, void *extra_state,
                         MPI_Request *request) ;
# 35 "comm.cpp" 2
# 1 "./comm.h" 1
# 35 "./comm.h"
# 1 "./atom.h" 1
# 35 "./atom.h"
# 1 "./threadData.h" 1
# 35 "./threadData.h"
class ThreadData
{
  public:
    ThreadData() {
      mpi_me = 0;
      mpi_num_threads = 0;
      omp_me = 0;
      omp_num_threads = 1;
      teams = 1;
    };
    ~ThreadData() {};
    int mpi_me;
    int mpi_num_threads;
    int omp_me;
    int omp_num_threads;
    int teams;
};
# 36 "./atom.h" 2
# 1 "./types.h" 1
# 35 "./types.h"
enum ForceStyle {FORCELJ, FORCEEAM};


struct double2 {
  double x, y;
};
struct float2 {
  float x, y;
};
struct double4 {
  double x, y, z, w;
};
struct float4 {
  float x, y, z, w;
};
# 69 "./types.h"
typedef double MMD_float;
typedef double2 MMD_float2;
typedef double4 MMD_float4;

typedef int MMD_int;
typedef int MMD_bigint;
# 37 "./atom.h" 2


class Neighbor;
struct Box {
  MMD_float xprd, yprd, zprd;
  MMD_float xlo, xhi;
  MMD_float ylo, yhi;
  MMD_float zlo, zhi;
};

class Atom
{
  public:
    int natoms;
    int nlocal, nghost;
    int nmax;

    MMD_float* x;
    MMD_float* v;
    MMD_float* f;

    int ntypes;
    int* type;

    MMD_float* xold;

    ThreadData* threads;
    MMD_float virial, mass;

    int comm_size, reverse_size, border_size;

    struct Box box;

    Atom(int ntypes_);
    ~Atom();
    void addatom(MMD_float, MMD_float, MMD_float, MMD_float, MMD_float, MMD_float);
    void pbc();
    void growarray();

    void copy(int, int);

    void pack_comm(int, int*, MMD_float*, int*);
    void unpack_comm(int, int, MMD_float*);
    void pack_reverse(int, int, MMD_float*);
    void unpack_reverse(int, int*, MMD_float*);

    int pack_border(int, MMD_float*, int*);
    int unpack_border(int, MMD_float*);
    int pack_exchange(int, MMD_float*);
    int unpack_exchange(int, MMD_float*);
    int skip_exchange(MMD_float*);

    MMD_float* realloc_2d_MMD_float_array(MMD_float*, int, int, int);
    MMD_float* create_2d_MMD_float_array(int, int);
    void destroy_2d_MMD_float_array(MMD_float*);

    int* realloc_1d_int_array(int*, int, int);
    int* create_1d_int_array(int);
    void destroy_1d_int_array(int*);

    void sort(Neighbor & neighbor);

  private:
    int* binpos;
    int* bins;
    MMD_float* x_copy;
    MMD_float* v_copy;
    int* type_copy;
    int copy_size;
};
# 36 "./comm.h" 2

# 1 "./timer.h" 1
# 43 "./timer.h"
# 1 "/cvmfs/software.hpc.rwth.de/Linux/RH8/x86_64/intel/sapphirerapids/software/GCCcore/12.3.0/lib/gcc/x86_64-pc-linux-gnu/12.3.0/../../../../include/c++/12.3.0/ctime" 1 3
# 40 "/cvmfs/software.hpc.rwth.de/Linux/RH8/x86_64/intel/sapphirerapids/software/GCCcore/12.3.0/lib/gcc/x86_64-pc-linux-gnu/12.3.0/../../../../include/c++/12.3.0/ctime" 3


# 1 "/usr/include/time.h" 1 3 4
# 29 "/usr/include/time.h" 3 4
# 1 "/rwthfs/rz/cluster/work/rwth1269/software/c23/classic-flang/lib/clang/16/include/stddef.h" 1 3 4
# 30 "/usr/include/time.h" 2 3 4



# 1 "/usr/include/bits/time.h" 1 3 4
# 73 "/usr/include/bits/time.h" 3 4
# 1 "/usr/include/bits/timex.h" 1 3 4
# 26 "/usr/include/bits/timex.h" 3 4
struct timex
{
  unsigned int modes;
  __syscall_slong_t offset;
  __syscall_slong_t freq;
  __syscall_slong_t maxerror;
  __syscall_slong_t esterror;
  int status;
  __syscall_slong_t constant;
  __syscall_slong_t precision;
  __syscall_slong_t tolerance;
  struct timeval time;
  __syscall_slong_t tick;
  __syscall_slong_t ppsfreq;
  __syscall_slong_t jitter;
  int shift;
  __syscall_slong_t stabil;
  __syscall_slong_t jitcnt;
  __syscall_slong_t calcnt;
  __syscall_slong_t errcnt;
  __syscall_slong_t stbcnt;

  int tai;


  int :32; int :32; int :32; int :32;
  int :32; int :32; int :32; int :32;
  int :32; int :32; int :32;
};
# 74 "/usr/include/bits/time.h" 2 3 4

extern "C" {


extern int clock_adjtime (__clockid_t __clock_id, struct timex *__utx) throw ();

}
# 34 "/usr/include/time.h" 2 3 4





# 1 "/usr/include/bits/types/struct_tm.h" 1 3 4






struct tm
{
  int tm_sec;
  int tm_min;
  int tm_hour;
  int tm_mday;
  int tm_mon;
  int tm_year;
  int tm_wday;
  int tm_yday;
  int tm_isdst;


  long int tm_gmtoff;
  const char *tm_zone;




};
# 40 "/usr/include/time.h" 2 3 4








# 1 "/usr/include/bits/types/struct_itimerspec.h" 1 3 4







struct itimerspec
  {
    struct timespec it_interval;
    struct timespec it_value;
  };
# 49 "/usr/include/time.h" 2 3 4
struct sigevent;
# 68 "/usr/include/time.h" 3 4
extern "C" {



extern clock_t clock (void) throw ();


extern time_t time (time_t *__timer) throw ();


extern double difftime (time_t __time1, time_t __time0)
     throw () __attribute__ ((__const__));


extern time_t mktime (struct tm *__tp) throw ();





extern size_t strftime (char *__restrict __s, size_t __maxsize,
   const char *__restrict __format,
   const struct tm *__restrict __tp) throw ();




extern char *strptime (const char *__restrict __s,
         const char *__restrict __fmt, struct tm *__tp)
     throw ();






extern size_t strftime_l (char *__restrict __s, size_t __maxsize,
     const char *__restrict __format,
     const struct tm *__restrict __tp,
     locale_t __loc) throw ();



extern char *strptime_l (const char *__restrict __s,
    const char *__restrict __fmt, struct tm *__tp,
    locale_t __loc) throw ();





extern struct tm *gmtime (const time_t *__timer) throw ();



extern struct tm *localtime (const time_t *__timer) throw ();




extern struct tm *gmtime_r (const time_t *__restrict __timer,
       struct tm *__restrict __tp) throw ();



extern struct tm *localtime_r (const time_t *__restrict __timer,
          struct tm *__restrict __tp) throw ();




extern char *asctime (const struct tm *__tp) throw ();


extern char *ctime (const time_t *__timer) throw ();






extern char *asctime_r (const struct tm *__restrict __tp,
   char *__restrict __buf) throw ();


extern char *ctime_r (const time_t *__restrict __timer,
        char *__restrict __buf) throw ();




extern char *__tzname[2];
extern int __daylight;
extern long int __timezone;




extern char *tzname[2];



extern void tzset (void) throw ();



extern int daylight;
extern long int timezone;





extern int stime (const time_t *__when) throw ();
# 196 "/usr/include/time.h" 3 4
extern time_t timegm (struct tm *__tp) throw ();


extern time_t timelocal (struct tm *__tp) throw ();


extern int dysize (int __year) throw () __attribute__ ((__const__));
# 211 "/usr/include/time.h" 3 4
extern int nanosleep (const struct timespec *__requested_time,
        struct timespec *__remaining);



extern int clock_getres (clockid_t __clock_id, struct timespec *__res) throw ();


extern int clock_gettime (clockid_t __clock_id, struct timespec *__tp) throw ();


extern int clock_settime (clockid_t __clock_id, const struct timespec *__tp)
     throw ();






extern int clock_nanosleep (clockid_t __clock_id, int __flags,
       const struct timespec *__req,
       struct timespec *__rem);


extern int clock_getcpuclockid (pid_t __pid, clockid_t *__clock_id) throw ();




extern int timer_create (clockid_t __clock_id,
    struct sigevent *__restrict __evp,
    timer_t *__restrict __timerid) throw ();


extern int timer_delete (timer_t __timerid) throw ();


extern int timer_settime (timer_t __timerid, int __flags,
     const struct itimerspec *__restrict __value,
     struct itimerspec *__restrict __ovalue) throw ();


extern int timer_gettime (timer_t __timerid, struct itimerspec *__value)
     throw ();


extern int timer_getoverrun (timer_t __timerid) throw ();





extern int timespec_get (struct timespec *__ts, int __base)
     throw () __attribute__ ((__nonnull__ (1)));
# 280 "/usr/include/time.h" 3 4
extern int getdate_err;
# 289 "/usr/include/time.h" 3 4
extern struct tm *getdate (const char *__string);
# 303 "/usr/include/time.h" 3 4
extern int getdate_r (const char *__restrict __string,
        struct tm *__restrict __resbufp);


}
# 43 "/cvmfs/software.hpc.rwth.de/Linux/RH8/x86_64/intel/sapphirerapids/software/GCCcore/12.3.0/lib/gcc/x86_64-pc-linux-gnu/12.3.0/../../../../include/c++/12.3.0/ctime" 2 3
# 58 "/cvmfs/software.hpc.rwth.de/Linux/RH8/x86_64/intel/sapphirerapids/software/GCCcore/12.3.0/lib/gcc/x86_64-pc-linux-gnu/12.3.0/../../../../include/c++/12.3.0/ctime" 3
namespace std
{
  using ::clock_t;
  using ::time_t;
  using ::tm;

  using ::clock;
  using ::difftime;
  using ::mktime;
  using ::time;
  using ::asctime;
  using ::ctime;
  using ::gmtime;
  using ::localtime;
  using ::strftime;
}



namespace std
{
  using ::timespec;
  using ::timespec_get;
}
# 44 "./timer.h" 2

class Timer
{
  public:
    Timer();
    ~Timer();
    void stamp();
    void stamp(int);
    void stamp_extra_start();
    void stamp_extra_stop(int);
    void barrier_start(int);
    void barrier_stop(int);
    double* array;

  private:



    double previous_time_d, previous_time_extra_d;

};
# 38 "./comm.h" 2


class Comm
{
  public:
    Comm();
    ~Comm();
    int setup(MMD_float, Atom &);
    void communicate(Atom &);
    void reverse_communicate(Atom &);
    void exchange(Atom &);
    void exchange_all(Atom &);
    void borders(Atom &);
    void growsend(int);
    void growrecv(int);
    void growlist(int, int);

    void free_windows();


  public:
    int me;
    int nswap;
    int* pbc_any;
    int* pbc_flagx;
    int* pbc_flagy;
    int* pbc_flagz;
    int* sendnum, *recvnum;
    int* comm_send_size;
    int* comm_recv_size;
    int* reverse_send_size;
    int* reverse_recv_size;
    int* sendproc, *recvproc;
    int* sendproc_exc, *recvproc_exc;

    int* firstrecv;
    int** sendlist;
    int* maxsendlist;

    MMD_float* buf_send;
    MMD_float* buf_recv;
    MMD_float* buf;
    int maxsend;
    int buf_send_size;
    int maxrecv;

    int procneigh[3][2];
    int procgrid[3];
    int need[3];
    MMD_float* slablo, *slabhi;

    ThreadData* threads;

    int check_safeexchange;
    int do_safeexchange;
    Timer* timer;

    int copy_size;
    int* nsend_thread;
    int* nrecv_thread;
    int* nholes_thread;
    int** exc_sendlist_thread;
    int* send_flag;
    int* maxsend_thread;
    int maxthreads;
    int maxnlocal;
    int nrecv_atoms;

    MPI_Win win_buf_send;

  private:
    MPI_Win win_nsend_buf;
    int nsend_buf;


};
# 36 "comm.cpp" 2
# 1 "./openmp.h" 1
# 40 "./openmp.h"
# 1 "/rwthfs/rz/cluster/work/rwth1269/software/c23/classic-flang/lib/clang/16/include/omp.h" 1 3
# 18 "/rwthfs/rz/cluster/work/rwth1269/software/c23/classic-flang/lib/clang/16/include/omp.h" 3
# 1 "/cvmfs/software.hpc.rwth.de/Linux/RH8/x86_64/intel/sapphirerapids/software/GCCcore/12.3.0/lib/gcc/x86_64-pc-linux-gnu/12.3.0/../../../../include/c++/12.3.0/stdlib.h" 1 3
# 19 "/rwthfs/rz/cluster/work/rwth1269/software/c23/classic-flang/lib/clang/16/include/omp.h" 2 3








    extern "C" {
# 48 "/rwthfs/rz/cluster/work/rwth1269/software/c23/classic-flang/lib/clang/16/include/omp.h" 3
    typedef enum omp_sched_t {
        omp_sched_static = 1,
        omp_sched_dynamic = 2,
        omp_sched_guided = 3,
        omp_sched_auto = 4,
        omp_sched_monotonic = 0x80000000
    } omp_sched_t;


    extern void omp_set_num_threads (int);
    extern void omp_set_dynamic (int);
    extern void omp_set_nested (int);
    extern void omp_set_max_active_levels (int);
    extern void omp_set_schedule (omp_sched_t, int);


    extern int omp_get_num_threads (void);
    extern int omp_get_dynamic (void);
    extern int omp_get_nested (void);
    extern int omp_get_max_threads (void);
    extern int omp_get_thread_num (void);
    extern int omp_get_num_procs (void);
    extern int omp_in_parallel (void);
    extern int omp_in_final (void);
    extern int omp_get_active_level (void);
    extern int omp_get_level (void);
    extern int omp_get_ancestor_thread_num (int);
    extern int omp_get_team_size (int);
    extern int omp_get_thread_limit (void);
    extern int omp_get_max_active_levels (void);
    extern void omp_get_schedule (omp_sched_t *, int *);
    extern int omp_get_max_task_priority (void);


    typedef struct omp_lock_t {
        void * _lk;
    } omp_lock_t;

    extern void omp_init_lock (omp_lock_t *);
    extern void omp_set_lock (omp_lock_t *);
    extern void omp_unset_lock (omp_lock_t *);
    extern void omp_destroy_lock (omp_lock_t *);
    extern int omp_test_lock (omp_lock_t *);


    typedef struct omp_nest_lock_t {
        void * _lk;
    } omp_nest_lock_t;

    extern void omp_init_nest_lock (omp_nest_lock_t *);
    extern void omp_set_nest_lock (omp_nest_lock_t *);
    extern void omp_unset_nest_lock (omp_nest_lock_t *);
    extern void omp_destroy_nest_lock (omp_nest_lock_t *);
    extern int omp_test_nest_lock (omp_nest_lock_t *);


    typedef enum omp_sync_hint_t {
        omp_sync_hint_none = 0,
        omp_lock_hint_none = omp_sync_hint_none,
        omp_sync_hint_uncontended = 1,
        omp_lock_hint_uncontended = omp_sync_hint_uncontended,
        omp_sync_hint_contended = (1<<1),
        omp_lock_hint_contended = omp_sync_hint_contended,
        omp_sync_hint_nonspeculative = (1<<2),
        omp_lock_hint_nonspeculative = omp_sync_hint_nonspeculative,
        omp_sync_hint_speculative = (1<<3),
        omp_lock_hint_speculative = omp_sync_hint_speculative,
        kmp_lock_hint_hle = (1<<16),
        kmp_lock_hint_rtm = (1<<17),
        kmp_lock_hint_adaptive = (1<<18)
    } omp_sync_hint_t;


    typedef omp_sync_hint_t omp_lock_hint_t;


    extern void omp_init_lock_with_hint(omp_lock_t *, omp_lock_hint_t);
    extern void omp_init_nest_lock_with_hint(omp_nest_lock_t *, omp_lock_hint_t);


    extern double omp_get_wtime (void);
    extern double omp_get_wtick (void);


    extern int omp_get_default_device (void);
    extern void omp_set_default_device (int);
    extern int omp_is_initial_device (void);
    extern int omp_get_num_devices (void);
    extern int omp_get_num_teams (void);
    extern int omp_get_team_num (void);
    extern int omp_get_cancellation (void);


    extern int omp_get_initial_device (void);
    extern void* omp_target_alloc(size_t, int);
    extern void omp_target_free(void *, int);
    extern int omp_target_is_present(const void *, int);
    extern int omp_target_memcpy(void *, const void *, size_t, size_t, size_t, int, int);
    extern int omp_target_memcpy_rect(void *, const void *, size_t, int, const size_t *,
                                            const size_t *, const size_t *, const size_t *, const size_t *, int, int);
    extern int omp_target_associate_ptr(const void *, const void *, size_t, size_t, int);
    extern int omp_target_disassociate_ptr(const void *, int);


    extern int omp_get_device_num (void);
    typedef void * omp_depend_t;


    typedef intptr_t omp_intptr_t;


    typedef enum omp_interop_property {
        omp_ipr_fr_id = -1,
        omp_ipr_fr_name = -2,
        omp_ipr_vendor = -3,
        omp_ipr_vendor_name = -4,
        omp_ipr_device_num = -5,
        omp_ipr_platform = -6,
        omp_ipr_device = -7,
        omp_ipr_device_context = -8,
        omp_ipr_targetsync = -9,
        omp_ipr_first = -9
    } omp_interop_property_t;



    typedef enum omp_interop_rc {
        omp_irc_no_value = 1,
        omp_irc_success = 0,
        omp_irc_empty = -1,
        omp_irc_out_of_range = -2,
        omp_irc_type_int = -3,
        omp_irc_type_ptr = -4,
        omp_irc_type_str = -5,
        omp_irc_other = -6
    } omp_interop_rc_t;

    typedef enum omp_interop_fr {
        omp_ifr_cuda = 1,
        omp_ifr_cuda_driver = 2,
        omp_ifr_opencl = 3,
        omp_ifr_sycl = 4,
        omp_ifr_hip = 5,
        omp_ifr_level_zero = 6,
        omp_ifr_last = 7
    } omp_interop_fr_t;

    typedef void * omp_interop_t;




    extern int omp_get_num_interop_properties(const omp_interop_t);



    extern omp_intptr_t omp_get_interop_int(const omp_interop_t, omp_interop_property_t, int *);



    extern void * omp_get_interop_ptr(const omp_interop_t, omp_interop_property_t, int *);



    extern const char * omp_get_interop_str(const omp_interop_t, omp_interop_property_t, int *);



    extern const char * omp_get_interop_name(const omp_interop_t, omp_interop_property_t);



    extern const char * omp_get_interop_type_desc(const omp_interop_t, omp_interop_property_t);



    extern const char * omp_get_interop_rc_desc(const omp_interop_t, omp_interop_rc_t);






    extern int omp_target_memcpy_async(void *, const void *, size_t, size_t, size_t, int,
                                             int, int, omp_depend_t *);



    extern int omp_target_memcpy_rect_async(void *, const void *, size_t, int, const size_t *,
                                             const size_t *, const size_t *, const size_t *, const size_t *, int, int,
                                             int, omp_depend_t *);



    extern void * omp_get_mapped_ptr(const void *, int);
    extern int omp_target_is_accessible(const void *, size_t, int);


    extern int kmp_get_stacksize (void);
    extern void kmp_set_stacksize (int);
    extern size_t kmp_get_stacksize_s (void);
    extern void kmp_set_stacksize_s (size_t);
    extern int kmp_get_blocktime (void);
    extern int kmp_get_library (void);
    extern void kmp_set_blocktime (int);
    extern void kmp_set_library (int);
    extern void kmp_set_library_serial (void);
    extern void kmp_set_library_turnaround (void);
    extern void kmp_set_library_throughput (void);
    extern void kmp_set_defaults (char const *);
    extern void kmp_set_disp_num_buffers (int);


    typedef void * kmp_affinity_mask_t;

    extern int kmp_set_affinity (kmp_affinity_mask_t *);
    extern int kmp_get_affinity (kmp_affinity_mask_t *);
    extern int kmp_get_affinity_max_proc (void);
    extern void kmp_create_affinity_mask (kmp_affinity_mask_t *);
    extern void kmp_destroy_affinity_mask (kmp_affinity_mask_t *);
    extern int kmp_set_affinity_mask_proc (int, kmp_affinity_mask_t *);
    extern int kmp_unset_affinity_mask_proc (int, kmp_affinity_mask_t *);
    extern int kmp_get_affinity_mask_proc (int, kmp_affinity_mask_t *);


    typedef enum omp_proc_bind_t {
        omp_proc_bind_false = 0,
        omp_proc_bind_true = 1,
        omp_proc_bind_master = 2,
        omp_proc_bind_close = 3,
        omp_proc_bind_spread = 4
    } omp_proc_bind_t;

    extern omp_proc_bind_t omp_get_proc_bind (void);


    extern int omp_get_num_places (void);
    extern int omp_get_place_num_procs (int);
    extern void omp_get_place_proc_ids (int, int *);
    extern int omp_get_place_num (void);
    extern int omp_get_partition_num_places (void);
    extern void omp_get_partition_place_nums (int *);

    extern void * kmp_malloc (size_t);
    extern void * kmp_aligned_malloc (size_t, size_t);
    extern void * kmp_calloc (size_t, size_t);
    extern void * kmp_realloc (void *, size_t);
    extern void kmp_free (void *);

    extern void kmp_set_warnings_on(void);
    extern void kmp_set_warnings_off(void);


    typedef enum omp_control_tool_result_t {
        omp_control_tool_notool = -2,
        omp_control_tool_nocallback = -1,
        omp_control_tool_success = 0,
        omp_control_tool_ignored = 1
    } omp_control_tool_result_t;

    typedef enum omp_control_tool_t {
        omp_control_tool_start = 1,
        omp_control_tool_pause = 2,
        omp_control_tool_flush = 3,
        omp_control_tool_end = 4
    } omp_control_tool_t;

    extern int omp_control_tool(int, int, void*);


    typedef uintptr_t omp_uintptr_t;

    typedef enum {
        omp_atk_sync_hint = 1,
        omp_atk_alignment = 2,
        omp_atk_access = 3,
        omp_atk_pool_size = 4,
        omp_atk_fallback = 5,
        omp_atk_fb_data = 6,
        omp_atk_pinned = 7,
        omp_atk_partition = 8
    } omp_alloctrait_key_t;

    typedef enum {
        omp_atv_false = 0,
        omp_atv_true = 1,
        omp_atv_contended = 3,
        omp_atv_uncontended = 4,
        omp_atv_serialized = 5,
        omp_atv_sequential = omp_atv_serialized,
        omp_atv_private = 6,
        omp_atv_all = 7,
        omp_atv_thread = 8,
        omp_atv_pteam = 9,
        omp_atv_cgroup = 10,
        omp_atv_default_mem_fb = 11,
        omp_atv_null_fb = 12,
        omp_atv_abort_fb = 13,
        omp_atv_allocator_fb = 14,
        omp_atv_environment = 15,
        omp_atv_nearest = 16,
        omp_atv_blocked = 17,
        omp_atv_interleaved = 18
    } omp_alloctrait_value_t;


    typedef struct {
        omp_alloctrait_key_t key;
        omp_uintptr_t value;
    } omp_alloctrait_t;
# 386 "/rwthfs/rz/cluster/work/rwth1269/software/c23/classic-flang/lib/clang/16/include/omp.h" 3
    typedef enum omp_allocator_handle_t : omp_uintptr_t



    {
      omp_null_allocator = 0,
      omp_default_mem_alloc = 1,
      omp_large_cap_mem_alloc = 2,
      omp_const_mem_alloc = 3,
      omp_high_bw_mem_alloc = 4,
      omp_low_lat_mem_alloc = 5,
      omp_cgroup_mem_alloc = 6,
      omp_pteam_mem_alloc = 7,
      omp_thread_mem_alloc = 8,
      llvm_omp_target_host_mem_alloc = 100,
      llvm_omp_target_shared_mem_alloc = 101,
      llvm_omp_target_device_mem_alloc = 102,
      KMP_ALLOCATOR_MAX_HANDLE = (18446744073709551615UL)
    } omp_allocator_handle_t;

    typedef enum omp_memspace_handle_t : omp_uintptr_t



    {
      omp_default_mem_space = 0,
      omp_large_cap_mem_space = 1,
      omp_const_mem_space = 2,
      omp_high_bw_mem_space = 3,
      omp_low_lat_mem_space = 4,
      llvm_omp_target_host_mem_space = 100,
      llvm_omp_target_shared_mem_space = 101,
      llvm_omp_target_device_mem_space = 102,
      KMP_MEMSPACE_MAX_HANDLE = (18446744073709551615UL)
    } omp_memspace_handle_t;

    extern omp_allocator_handle_t omp_init_allocator(omp_memspace_handle_t m,
                                                       int ntraits, omp_alloctrait_t traits[]);
    extern void omp_destroy_allocator(omp_allocator_handle_t allocator);

    extern void omp_set_default_allocator(omp_allocator_handle_t a);
    extern omp_allocator_handle_t omp_get_default_allocator(void);

    extern void * omp_alloc(size_t size, omp_allocator_handle_t a = omp_null_allocator);
    extern void * omp_aligned_alloc(size_t align, size_t size,
                                                         omp_allocator_handle_t a = omp_null_allocator);
    extern void * omp_calloc(size_t nmemb, size_t size,
                                                  omp_allocator_handle_t a = omp_null_allocator);
    extern void * omp_aligned_calloc(size_t align, size_t nmemb, size_t size,
                                                          omp_allocator_handle_t a = omp_null_allocator);
    extern void * omp_realloc(void *ptr, size_t size,
                                                   omp_allocator_handle_t allocator = omp_null_allocator,
                                                   omp_allocator_handle_t free_allocator = omp_null_allocator);
    extern void omp_free(void * ptr, omp_allocator_handle_t a = omp_null_allocator);
# 453 "/rwthfs/rz/cluster/work/rwth1269/software/c23/classic-flang/lib/clang/16/include/omp.h" 3
    extern void ompc_set_affinity_format(char const *);
    extern size_t ompc_get_affinity_format(char *, size_t);
    extern void ompc_display_affinity(char const *);
    extern size_t ompc_capture_affinity(char *, size_t, char const *);






    typedef enum omp_event_handle_t { KMP_EVENT_MAX_HANDLE = (18446744073709551615UL) } omp_event_handle_t;

    extern void omp_fulfill_event ( omp_event_handle_t event );


    typedef enum omp_pause_resource_t {
      omp_pause_resume = 0,
      omp_pause_soft = 1,
      omp_pause_hard = 2
    } omp_pause_resource_t;
    extern int omp_pause_resource(omp_pause_resource_t, int);
    extern int omp_pause_resource_all(omp_pause_resource_t);

    extern int omp_get_supported_active_levels(void);


    extern void omp_set_num_teams(int num_teams);
    extern int omp_get_max_teams(void);
    extern void omp_set_teams_thread_limit(int limit);
    extern int omp_get_teams_thread_limit(void);


    extern void omp_display_env(int verbose);


#pragma omp begin declare variant match(device={kind(host)})
    static inline int omp_is_initial_device(void) { return 1; }
#pragma omp end declare variant
#pragma omp begin declare variant match(device={kind(nohost)})
    static inline int omp_is_initial_device(void) { return 0; }
#pragma omp end declare variant



    extern int omp_in_explicit_task(void);


    extern void *llvm_omp_target_dynamic_shared_alloc();







    typedef int omp_int_t;
    typedef double omp_wtime_t;


    }
# 41 "./openmp.h" 2
# 37 "comm.cpp" 2







Comm::Comm()
{
  maxsend = 1000;
  buf_send_size = maxsend + 1000;



  maxrecv = 1000;
  buf_recv = (MMD_float*) malloc(maxrecv * sizeof(MMD_float));
  check_safeexchange = 0;
  do_safeexchange = 0;
  maxthreads = 0;
  maxnlocal = 0;


  MPI_Win_create(&nsend_buf, (MPI_Aint)(1 * sizeof(int)), sizeof(int), ((MPI_Info)0x1c000000), ((MPI_Comm)0x44000000), &win_nsend_buf);

  MPI_Win_allocate((MPI_Aint)((buf_send_size) * sizeof(MMD_float)), sizeof(MMD_float), ((MPI_Info)0x1c000000), ((MPI_Comm)0x44000000), &buf_send, &win_buf_send);




  MPI_Win_lock_all(0, win_nsend_buf);
  MPI_Win_lock_all(0, win_buf_send);


}

Comm::~Comm() {
}


void Comm::free_windows(){
  if(win_nsend_buf != ((MPI_Win)0x20000000)) {



    MPI_Win_unlock_all(win_nsend_buf);

    MPI_Win_free(&win_nsend_buf);
  }
  if(win_buf_send != ((MPI_Win)0x20000000)) {



    MPI_Win_unlock_all(win_buf_send);

    MPI_Win_free(&win_buf_send);
  }
}




int Comm::setup(MMD_float cutneigh, Atom &atom)
{
  int i;
  int nprocs;
  int periods[3];
  MMD_float prd[3];
  int myloc[3];
  MPI_Comm cartesian;
  MMD_float lo, hi;
  int ineed, idim, nbox;

  prd[0] = atom.box.xprd;
  prd[1] = atom.box.yprd;
  prd[2] = atom.box.zprd;



  MPI_Comm_rank(((MPI_Comm)0x44000000), &me);
  MPI_Comm_size(((MPI_Comm)0x44000000), &nprocs);

  MMD_float area[3];

  area[0] = prd[0] * prd[1];
  area[1] = prd[0] * prd[2];
  area[2] = prd[1] * prd[2];

  MMD_float bestsurf = 2.0 * (area[0] + area[1] + area[2]);





  int ipx, ipy, ipz, nremain;
  MMD_float surf;

  ipx = 1;

  while(ipx <= nprocs) {
    if(nprocs % ipx == 0) {
      nremain = nprocs / ipx;
      ipy = 1;

      while(ipy <= nremain) {
        if(nremain % ipy == 0) {
          ipz = nremain / ipy;
          surf = area[0] / ipx / ipy + area[1] / ipx / ipz + area[2] / ipy / ipz;

          if(surf < bestsurf) {
            bestsurf = surf;
            procgrid[0] = ipx;
            procgrid[1] = ipy;
            procgrid[2] = ipz;
          }
        }

        ipy++;
      }
    }

    ipx++;
  }

  if(procgrid[0]*procgrid[1]*procgrid[2] != nprocs) {
    if(me == 0) printf("ERROR: Bad grid of processors\n");

    return 1;
  }



  int reorder = 0;
  periods[0] = periods[1] = periods[2] = 1;

  MPI_Cart_create(((MPI_Comm)0x44000000), 3, procgrid, periods, reorder, &cartesian);
  MPI_Cart_get(cartesian, 3, procgrid, periods, myloc);
  MPI_Cart_shift(cartesian, 0, 1, &procneigh[0][0], &procneigh[0][1]);
  MPI_Cart_shift(cartesian, 1, 1, &procneigh[1][0], &procneigh[1][1]);
  MPI_Cart_shift(cartesian, 2, 1, &procneigh[2][0], &procneigh[2][1]);



  atom.box.xlo = myloc[0] * prd[0] / procgrid[0];
  atom.box.xhi = (myloc[0] + 1) * prd[0] / procgrid[0];
  atom.box.ylo = myloc[1] * prd[1] / procgrid[1];
  atom.box.yhi = (myloc[1] + 1) * prd[1] / procgrid[1];
  atom.box.zlo = myloc[2] * prd[2] / procgrid[2];
  atom.box.zhi = (myloc[2] + 1) * prd[2] / procgrid[2];



  need[0] = static_cast<int>(cutneigh * procgrid[0] / prd[0] + 1);
  need[1] = static_cast<int>(cutneigh * procgrid[1] / prd[1] + 1);
  need[2] = static_cast<int>(cutneigh * procgrid[2] / prd[2] + 1);



  int maxswap = 2 * (need[0] + need[1] + need[2]);

  slablo = (MMD_float*) malloc(maxswap * sizeof(MMD_float));
  slabhi = (MMD_float*) malloc(maxswap * sizeof(MMD_float));
  pbc_any = (int*) malloc(maxswap * sizeof(int));
  pbc_flagx = (int*) malloc(maxswap * sizeof(int));
  pbc_flagy = (int*) malloc(maxswap * sizeof(int));
  pbc_flagz = (int*) malloc(maxswap * sizeof(int));
  sendproc = (int*) malloc(maxswap * sizeof(int));
  recvproc = (int*) malloc(maxswap * sizeof(int));
  sendproc_exc = (int*) malloc(maxswap * sizeof(int));
  recvproc_exc = (int*) malloc(maxswap * sizeof(int));
  sendnum = (int*) malloc(maxswap * sizeof(int));
  recvnum = (int*) malloc(maxswap * sizeof(int));
  comm_send_size = (int*) malloc(maxswap * sizeof(int));
  comm_recv_size = (int*) malloc(maxswap * sizeof(int));
  reverse_send_size = (int*) malloc(maxswap * sizeof(int));
  reverse_recv_size = (int*) malloc(maxswap * sizeof(int));
  int iswap = 0;

  for(int idim = 0; idim < 3; idim++)
    for(int i = 1; i <= need[idim]; i++, iswap += 2) {
      MPI_Cart_shift(cartesian, idim, i, &sendproc_exc[iswap], &sendproc_exc[iswap + 1]);
      MPI_Cart_shift(cartesian, idim, i, &recvproc_exc[iswap + 1], &recvproc_exc[iswap]);
    }

  MPI_Comm_free(&cartesian);

  firstrecv = (int*) malloc(maxswap * sizeof(int));
  maxsendlist = (int*) malloc(maxswap * sizeof(int));

  for(i = 0; i < maxswap; i++) maxsendlist[i] = 1000;

  sendlist = (int**) malloc(maxswap * sizeof(int*));

  for(i = 0; i < maxswap; i++)
    sendlist[i] = (int*) malloc(1000 * sizeof(int));
# 246 "comm.cpp"
  nswap = 0;

  for(idim = 0; idim < 3; idim++) {
    for(ineed = 0; ineed < 2 * need[idim]; ineed++) {
      pbc_any[nswap] = 0;
      pbc_flagx[nswap] = 0;
      pbc_flagy[nswap] = 0;
      pbc_flagz[nswap] = 0;

      if(ineed % 2 == 0) {
        sendproc[nswap] = procneigh[idim][0];
        recvproc[nswap] = procneigh[idim][1];
        nbox = myloc[idim] + ineed / 2;
        lo = nbox * prd[idim] / procgrid[idim];

        if(idim == 0) hi = atom.box.xlo + cutneigh;

        if(idim == 1) hi = atom.box.ylo + cutneigh;

        if(idim == 2) hi = atom.box.zlo + cutneigh;

        hi = ((hi) < ((nbox + 1) * prd[idim] / procgrid[idim]) ? (hi) : ((nbox + 1) * prd[idim] / procgrid[idim]));

        if(myloc[idim] == 0) {
          pbc_any[nswap] = 1;

          if(idim == 0) pbc_flagx[nswap] = 1;

          if(idim == 1) pbc_flagy[nswap] = 1;

          if(idim == 2) pbc_flagz[nswap] = 1;
        }
      } else {
        sendproc[nswap] = procneigh[idim][1];
        recvproc[nswap] = procneigh[idim][0];
        nbox = myloc[idim] - ineed / 2;
        hi = (nbox + 1) * prd[idim] / procgrid[idim];

        if(idim == 0) lo = atom.box.xhi - cutneigh;

        if(idim == 1) lo = atom.box.yhi - cutneigh;

        if(idim == 2) lo = atom.box.zhi - cutneigh;

        lo = ((lo) > (nbox * prd[idim] / procgrid[idim]) ? (lo) : (nbox * prd[idim] / procgrid[idim]));

        if(myloc[idim] == procgrid[idim] - 1) {
          pbc_any[nswap] = 1;

          if(idim == 0) pbc_flagx[nswap] = -1;

          if(idim == 1) pbc_flagy[nswap] = -1;

          if(idim == 2) pbc_flagz[nswap] = -1;
        }
      }

      slablo[nswap] = lo;
      slabhi[nswap] = hi;
      nswap++;
    }
  }

  return 0;
}



void Comm::communicate(Atom &atom)
{

  int iswap;
  int pbc_flags[4];
  MMD_float* buf;

  for(iswap = 0; iswap < nswap; iswap++) {



    pbc_flags[0] = pbc_any[iswap];
    pbc_flags[1] = pbc_flagx[iswap];
    pbc_flags[2] = pbc_flagy[iswap];
    pbc_flags[3] = pbc_flagz[iswap];


    atom.pack_comm(sendnum[iswap], sendlist[iswap], buf_send, pbc_flags);


#pragma omp single
        MPI_Barrier(((MPI_Comm)0x44000000));







    if(sendproc[iswap] != me) {
#pragma omp master
      {
        MPI_Datatype type = (sizeof(MMD_float) == 4) ? ((MPI_Datatype)0x4c00040a) : ((MPI_Datatype)0x4c00080b);


        MPI_Get(buf_recv, comm_recv_size[iswap], type, recvproc[iswap], 0, comm_recv_size[iswap], type, win_buf_send);



        MPI_Win_flush(recvproc[iswap], win_buf_send);
        MPI_Barrier(((MPI_Comm)0x44000000));






      }
      buf = buf_recv;
    } else buf = buf_send;

#pragma omp barrier


    atom.unpack_comm(recvnum[iswap], firstrecv[iswap], buf);

  }
}



void Comm::reverse_communicate(Atom &atom)
{
  int iswap;
  MMD_float* buf;

  for(iswap = nswap - 1; iswap >= 0; iswap--) {




    atom.pack_reverse(recvnum[iswap], firstrecv[iswap], buf_send);


#pragma omp master
        MPI_Barrier(((MPI_Comm)0x44000000));
#pragma omp barrier






    if(sendproc[iswap] != me) {

#pragma omp master
      {
        MPI_Datatype type = (sizeof(MMD_float) == 4) ? ((MPI_Datatype)0x4c00040a) : ((MPI_Datatype)0x4c00080b);


        MPI_Get(buf_recv, reverse_recv_size[iswap], type, sendproc[iswap], 0, reverse_recv_size[iswap], type, win_buf_send);



        MPI_Win_flush(sendproc[iswap], win_buf_send);
        MPI_Barrier(((MPI_Comm)0x44000000));






      }
      buf = buf_recv;
    } else buf = buf_send;



#pragma omp barrier
    atom.unpack_reverse(sendnum[iswap], sendlist[iswap], buf);

  }
}
# 435 "comm.cpp"
void Comm::exchange(Atom &atom)
{
  if(do_safeexchange)
    return exchange_all(atom);

  int i, m, n, idim, nsend, nrecv, nrecv1, nrecv2, nlocal;

  MMD_float lo, hi, value;
  MMD_float* x;



  atom.pbc();


  int tid = omp_get_thread_num();

  for(idim = 0; idim < 3; idim++) {



    if(procgrid[idim] == 1) continue;




    i = nsend = 0;

    if(idim == 0) {
      lo = atom.box.xlo;
      hi = atom.box.xhi;
    } else if(idim == 1) {
      lo = atom.box.ylo;
      hi = atom.box.yhi;
    } else {
      lo = atom.box.zlo;
      hi = atom.box.zhi;
    }

    x = atom.x;

    nlocal = atom.nlocal;

#pragma omp master
    {
      if(nlocal > maxnlocal) {
        send_flag = new int[nlocal];
        maxnlocal = nlocal;
      }

      if(maxthreads < threads->omp_num_threads) {
        maxthreads = threads->omp_num_threads;
        nsend_thread = new int [maxthreads];
        nrecv_thread = new int [maxthreads];
        nholes_thread = new int [maxthreads];
        maxsend_thread = new int [maxthreads];
        exc_sendlist_thread = new int*[maxthreads];

        for(int i = 0; i < maxthreads; i++) {
          maxsend_thread[i] = maxsend;
          exc_sendlist_thread[i] = (int*) malloc(maxsend * sizeof(int));
        }
      }
    }

#pragma omp barrier

    nsend = 0;
#pragma omp for

    for(int i = 0; i < threads->omp_num_threads; i++) {
      nsend_thread[i] = 0;
      nholes_thread[i] = 0;
    }

#pragma omp for
    for(int i = 0; i < nlocal; i++) {
      if(x[i * 3 + idim] < lo || x[i * 3 + idim] >= hi) {
        if(nsend >= maxsend_thread[tid]) {
          maxsend_thread[tid] = nsend + 100;
          exc_sendlist_thread[tid] = (int*) realloc(exc_sendlist_thread[tid], (nsend + 100) * sizeof(int));
        }

        exc_sendlist_thread[tid][nsend++] = i;
        send_flag[i] = 0;
      } else
        send_flag[i] = 1;
    }

    nsend_thread[tid] = nsend;

#pragma omp barrier

#pragma omp master
    {
      int total_nsend = 0;

      for(int i = 0; i < threads->omp_num_threads; i++) {
        total_nsend += nsend_thread[i];
        nsend_thread[i] = total_nsend;
      }

      growsend(total_nsend * 7);
    }

#pragma omp barrier

    int total_nsend = nsend_thread[threads->omp_num_threads - 1];
    int nholes = 0;

    for(int i = 0; i < nsend; i++)
      if(exc_sendlist_thread[tid][i] < nlocal - total_nsend)
        nholes++;

    nholes_thread[tid] = nholes;
#pragma omp barrier

#pragma omp master
    {
      int total_nholes = 0;

      for(int i = 0; i < threads->omp_num_threads; i++) {
        total_nholes += nholes_thread[i];
        nholes_thread[i] = total_nholes;
      }
    }
#pragma omp barrier

    int j = nlocal;
    int holes = 0;

    while(holes < nholes_thread[tid]) {
      j--;

      if(send_flag[j]) holes++;
    }


    for(int k = 0; k < nsend; k++) {
      atom.pack_exchange(exc_sendlist_thread[tid][k], &buf_send[(k + nsend_thread[tid] - nsend) * 7]);

      if(exc_sendlist_thread[tid][k] < nlocal - total_nsend) {
        while(!send_flag[j]) j++;

        atom.copy(j++, exc_sendlist_thread[tid][k]);
      }
    }

    nsend *= 7;
#pragma omp barrier
#pragma omp master
    {
      atom.nlocal = nlocal - total_nsend;
      nsend = total_nsend * 7;

      nsend_buf = nsend;
      MPI_Barrier(((MPI_Comm)0x44000000));






      MPI_Get(&nrecv1, 1, ((MPI_Datatype)0x4c000405), procneigh[idim][1], 0, 1, ((MPI_Datatype)0x4c000405), win_nsend_buf);



      MPI_Win_flush(procneigh[idim][1], win_nsend_buf);






      nrecv = nrecv1;

      if(procgrid[idim] > 2) {

      MPI_Get(&nrecv2, 1, ((MPI_Datatype)0x4c000405), procneigh[idim][0], 0, 1, ((MPI_Datatype)0x4c000405), win_nsend_buf);



      MPI_Win_flush(procneigh[idim][0], win_nsend_buf);






        nrecv += nrecv2;
      }

      if(nrecv > maxrecv) growrecv(nrecv);

      MPI_Datatype type = (sizeof(MMD_float) == 4) ? ((MPI_Datatype)0x4c00040a) : ((MPI_Datatype)0x4c00080b);


      MPI_Get(buf_recv, nrecv1, type, procneigh[idim][1], 0, nrecv1, type, win_buf_send);



      MPI_Win_flush(procneigh[idim][1], win_buf_send);
      MPI_Barrier(((MPI_Comm)0x44000000));







      if(procgrid[idim] > 2) {

        MPI_Get(buf_recv+nrecv1, nrecv2, type, procneigh[idim][0], 0, nrecv2, type, win_buf_send);



        MPI_Win_flush(procneigh[idim][0], win_buf_send);






      }
      nrecv_atoms = nrecv / 7;

      MPI_Barrier(((MPI_Comm)0x44000000));


      for(int i = 0; i < threads->omp_num_threads; i++)
        nrecv_thread[i] = 0;

    }



#pragma omp barrier

    nrecv = 0;

#pragma omp for
    for(int i = 0; i < nrecv_atoms; i++) {
      value = buf_recv[i * 7 + idim];

      if(value >= lo && value < hi)
        nrecv++;
    }

    nrecv_thread[tid] = nrecv;
    nlocal = atom.nlocal;
#pragma omp barrier

#pragma omp master
    {
      int total_nrecv = 0;

      for(int i = 0; i < threads->omp_num_threads; i++) {
        total_nrecv += nrecv_thread[i];
        nrecv_thread[i] = total_nrecv;
      }

      atom.nlocal += total_nrecv;
    }
#pragma omp barrier

    int copyinpos = nlocal + nrecv_thread[tid] - nrecv;

#pragma omp for
    for(int i = 0; i < nrecv_atoms; i++) {
      value = buf_recv[i * 7 + idim];

      if(value >= lo && value < hi)
        atom.unpack_exchange(copyinpos++, &buf_recv[i * 7]);
    }



  }
}

void Comm::exchange_all(Atom &atom)
{
  int i, m, n, idim, nsend, nrecv, nrecv1, nrecv2, nlocal;

  MMD_float lo, hi, value;
  MMD_float* x;



  atom.pbc();


  int iswap = 0;

  for(idim = 0; idim < 3; idim++) {



    if(procgrid[idim] == 1) {
      iswap += 2 * need[idim];
      continue;
    }




    i = nsend = 0;

    if(idim == 0) {
      lo = atom.box.xlo;
      hi = atom.box.xhi;
    } else if(idim == 1) {
      lo = atom.box.ylo;
      hi = atom.box.yhi;
    } else {
      lo = atom.box.zlo;
      hi = atom.box.zhi;
    }

    x = atom.x;

    nlocal = atom.nlocal;

    while(i < nlocal) {
      if(x[i * 3 + idim] < lo || x[i * 3 + idim] >= hi) {
        growsend(nsend);

        nsend += atom.pack_exchange(i, &buf_send[nsend]);
        atom.copy(nlocal - 1, i);
        nlocal--;
      } else i++;
    }

    atom.nlocal = nlocal;

    nsend_buf = nsend;
    MPI_Barrier(((MPI_Comm)0x44000000));




    for(int ineed = 0; ineed < 2 * need[idim]; ineed += 1) {
      if(ineed < procgrid[idim] - 1) {


        MPI_Get(&nrecv, 1, ((MPI_Datatype)0x4c000405), recvproc_exc[iswap], 0, 1, ((MPI_Datatype)0x4c000405), win_nsend_buf);



        MPI_Win_flush(recvproc_exc[iswap], win_nsend_buf);







        if(nrecv > maxrecv) growrecv(nrecv);

        MPI_Datatype type = (sizeof(MMD_float) == 4) ? ((MPI_Datatype)0x4c00040a) : ((MPI_Datatype)0x4c00080b);


        MPI_Get(buf_recv, nrecv, type, recvproc_exc[iswap], 0, nrecv, type, win_buf_send);



        MPI_Win_flush(recvproc_exc[iswap], win_buf_send);
        MPI_Barrier(((MPI_Comm)0x44000000));
# 813 "comm.cpp"
        n = atom.nlocal;
        m = 0;

        while(m < nrecv) {
          value = buf_recv[m + idim];

          if(value >= lo && value < hi)
            m += atom.unpack_exchange(n++, &buf_recv[m]);
          else m += atom.skip_exchange(&buf_recv[m]);
        }

        atom.nlocal = n;
      }

      iswap += 1;

    }
  }
}
# 842 "comm.cpp"
void Comm::borders(Atom &atom)
{
  int i, m, n, iswap, idim, nsend, ineed, nrecv, nall, nfirst, nlast;

  MMD_float lo, hi;
  int pbc_flags[4];
  MMD_float* x;



  atom.nghost = 0;



  iswap = 0;

  int tid = omp_get_thread_num();

#pragma omp master
    {
      if(atom.nlocal > maxnlocal) {
        send_flag = new int[atom.nlocal];
        maxnlocal = atom.nlocal;
      }

      if(maxthreads < threads->omp_num_threads) {
        maxthreads = threads->omp_num_threads;
        nsend_thread = new int [maxthreads];
        nrecv_thread = new int [maxthreads];
        nholes_thread = new int [maxthreads];
        maxsend_thread = new int [maxthreads];
        exc_sendlist_thread = new int*[maxthreads];

        for(int i = 0; i < maxthreads; i++) {
          maxsend_thread[i] = maxsend;
          exc_sendlist_thread[i] = (int*) malloc(maxsend * sizeof(int));
        }
      }
    }

  for(idim = 0; idim < 3; idim++) {
    nlast = 0;

    for(ineed = 0; ineed < 2 * need[idim]; ineed++) {







      lo = slablo[iswap];
      hi = slabhi[iswap];
      pbc_flags[0] = pbc_any[iswap];
      pbc_flags[1] = pbc_flagx[iswap];
      pbc_flags[2] = pbc_flagy[iswap];
      pbc_flags[3] = pbc_flagz[iswap];

      x = atom.x;

      if(ineed % 2 == 0) {
        nfirst = nlast;
        nlast = atom.nlocal + atom.nghost;
      }

#pragma omp for

      for(int i = 0; i < threads->omp_num_threads; i++) {
        nsend_thread[i] = 0;
      }


      nsend = 0;
      m = 0;

#pragma omp for
      for(int i = nfirst; i < nlast; i++) {
        if(x[i * 3 + idim] >= lo && x[i * 3 + idim] <= hi) {
          if(nsend >= maxsend_thread[tid]) {
            maxsend_thread[tid] = nsend + 100;
            exc_sendlist_thread[tid] = (int*) realloc(exc_sendlist_thread[tid], (nsend + 100) * sizeof(int));
          }

          exc_sendlist_thread[tid][nsend++] = i;
        }
      }

      nsend_thread[tid] = nsend;

#pragma omp barrier

#pragma omp master
      {
        int total_nsend = 0;

        for(int i = 0; i < threads->omp_num_threads; i++) {
          total_nsend += nsend_thread[i];
          nsend_thread[i] = total_nsend;
        }

        if(total_nsend > maxsendlist[iswap]) growlist(iswap, total_nsend);

        growsend(total_nsend * 4);
      }
#pragma omp barrier

      for(int k = 0; k < nsend; k++) {
        atom.pack_border(exc_sendlist_thread[tid][k], &buf_send[(k + nsend_thread[tid] - nsend) * 4], pbc_flags);
        sendlist[iswap][k + nsend_thread[tid] - nsend] = exc_sendlist_thread[tid][k];
      }

#pragma omp barrier






#pragma omp master
      {
        nsend = nsend_thread[threads->omp_num_threads - 1];
        nsend_buf = nsend;

        if(sendproc[iswap] != me) {


          MPI_Barrier(((MPI_Comm)0x44000000));
          MPI_Get(&nrecv, 1, ((MPI_Datatype)0x4c000405), recvproc[iswap], 0, 1, ((MPI_Datatype)0x4c000405), win_nsend_buf);



          MPI_Win_flush(recvproc[iswap],win_nsend_buf);







          if(nrecv * atom.border_size > maxrecv) growrecv(nrecv * atom.border_size);

          MPI_Datatype type = (sizeof(MMD_float) == 4) ? ((MPI_Datatype)0x4c00040a) : ((MPI_Datatype)0x4c00080b);

          MPI_Get(buf_recv, nrecv * atom.border_size, type, recvproc[iswap], 0, nrecv * atom.border_size, type, win_buf_send);



          MPI_Win_flush(recvproc[iswap], win_buf_send);
          MPI_Barrier(((MPI_Comm)0x44000000));







          buf = buf_recv;
        } else {
          nrecv = nsend;
          buf = buf_send;
        }

        nrecv_atoms = nrecv;
      }


#pragma omp barrier
      n = atom.nlocal + atom.nghost;
      nrecv = nrecv_atoms;

#pragma omp for
      for(int i = 0; i < nrecv; i++)
        atom.unpack_border(n + i, &buf[i * 4]);





#pragma omp master
      {
        sendnum[iswap] = nsend;
        recvnum[iswap] = nrecv;
        comm_send_size[iswap] = nsend * atom.comm_size;
        comm_recv_size[iswap] = nrecv * atom.comm_size;
        reverse_send_size[iswap] = nrecv * atom.reverse_size;
        reverse_recv_size[iswap] = nsend * atom.reverse_size;
        firstrecv[iswap] = atom.nlocal + atom.nghost;
        atom.nghost += nrecv;
      }
#pragma omp barrier
      iswap++;
    }
  }



  int max1, max2;
  max1 = max2 = 0;

  for(iswap = 0; iswap < nswap; iswap++) {
    max1 = ((max1) > (reverse_send_size[iswap]) ? (max1) : (reverse_send_size[iswap]));
    max2 = ((max2) > (reverse_recv_size[iswap]) ? (max2) : (reverse_recv_size[iswap]));
  }
#pragma omp master
  growsend(max1);
#pragma omp barrier

  if(max2 > maxrecv) growrecv(max2);
}



void Comm::growsend(int n)
{

  bool bgrow = false;

  if(n > maxsend) {
    maxsend = static_cast<int>(1.5 * n);
    buf_send_size = maxsend + 100;




    bgrow = true;

  }

  MPI_Allreduce((void *) -1, &bgrow, 1, ((MPI_Datatype)0x4c000133), (MPI_Op)(0x58000007), ((MPI_Comm)0x44000000));
  if(bgrow == false) return;



  MPI_Win_unlock_all(win_buf_send);

  MPI_Win_free(&win_buf_send);
  MPI_Win_allocate((MPI_Aint)((buf_send_size) * sizeof(MMD_float)), sizeof(MMD_float), ((MPI_Info)0x1c000000), ((MPI_Comm)0x44000000), &buf_send, &win_buf_send);




  MPI_Win_lock_all(0, win_buf_send);


}



void Comm::growrecv(int n)
{
  maxrecv = static_cast<int>(1.5 * n);
  free(buf_recv);
  buf_recv = (MMD_float*) malloc(maxrecv * sizeof(MMD_float));
}



void Comm::growlist(int iswap, int n)
{
  maxsendlist[iswap] = static_cast<int>(1.5 * n);
  sendlist[iswap] =
    (int*) realloc(sendlist[iswap], maxsendlist[iswap] * sizeof(int));
}
