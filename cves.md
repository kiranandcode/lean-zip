
- CVE-2026-27171 	1 Zlib 	1 Zlib 	2026-03-25 	2.9 Low
    zlib before 1.3.2 allows CPU consumption via crc32_combine64 and crc32_combine_gen64 because x2nmodp  can do right shifts within a loop that has no termination condition.
- CVE-2026-22184 	1 Zlib 	1 Zlib 	2026-03-18 	7.8 High
    zlib versions up to and including 1.3.1.2 include a global buffer overflow in the untgz utility located under contrib/untgz. The vulnerability is limited to the standalone demonstration utility and does not affect the core zlib compression library. The flaw occurs when a user executes the untgz command with an excessively long archive name supplied via the command line, leading to an out-of-bounds write in a fixed-size global buffer.
- CVE-2016-9842 	8 Apple, Canonical, Debian and 5 more 	22 Iphone Os, Mac Os X, Tvos and 19 more 	2025-12-04 	8.8 High
    The inflateMark function in inflate.c in zlib 1.2.8 might allow context-dependent attackers to have unspecified impact via vectors involving left shifts of negative integers.
- CVE-2018-25032 	13 Apple, Azul, Debian and 10 more 	47 Mac Os X, Macos, Zulu and 44 more 	2025-08-21 	7.5 High
    zlib before 1.2.12 allows memory corruption when deflating (i.e., when compressing) if the input has many distant matches.
- CVE-2025-0725 	3 Haxx, Netapp, Zlib 	12 Curl, Libcurl, Hci Baseboard Management Controller and 9 more 	2025-06-27 	7.3 High
    When libcurl is asked to perform automatic gzip decompression of content-encoded HTTP responses with the `CURLOPT_ACCEPT_ENCODING` option, **using zlib 1.2.0.3 or older**, an attacker-controlled integer overflow would make libcurl perform a buffer overflow.
- CVE-2022-37434 	7 Apple, Debian, Fedoraproject and 4 more 	24 Ipados, Iphone Os, Macos and 21 more 	2025-05-30 	9.8 Critical
    zlib through 1.2.12 has a heap-based buffer over-read or buffer overflow in inflate in inflate.c via a large gzip header extra field. NOTE: only applications that call inflateGetHeader are affected. Some common applications bundle the affected zlib source code but may be unable to call inflateGetHeader (e.g., see the nodejs/node reference).
- CVE-2016-9840 	9 Apple, Boost, Canonical and 6 more 	27 Iphone Os, Mac Os X, Tvos and 24 more 	2025-04-20 	8.8 High
    inftrees.c in zlib 1.2.8 might allow context-dependent attackers to have unspecified impact by leveraging improper pointer arithmetic.
- CVE-2016-9843 	10 Apple, Canonical, Debian and 7 more 	27 Iphone Os, Mac Os X, Tvos and 24 more 	2025-04-20 	9.8 Critical
    The crc32_big function in crc32.c in zlib 1.2.8 might allow context-dependent attackers to have unspecified impact via vectors involving big-endian CRC calculation.
- CVE-2016-9841 	9 Apple, Canonical, Debian and 6 more 	42 Iphone Os, Mac Os X, Tvos and 39 more 	2025-04-20 	9.8 Critical
    inffast.c in zlib 1.2.8 might allow context-dependent attackers to have unspecified impact by leveraging improper pointer arithmetic.
- CVE-2015-1191 	1 Zlib 	1 Pigz 	2025-04-12 	N/A
    Multiple directory traversal vulnerabilities in pigz 2.3.1 allow remote attackers to write to arbitrary files via a (1) full pathname or (2) .. (dot dot) in an archive.
- CVE-2013-0296 	1 Zlib 	1 Pigz 	2025-04-12 	N/A
    Race condition in pigz before 2.2.5 uses permissions derived from the umask when compressing a file before setting that file's permissions to match those of the original file, which might allow local users to bypass intended access permissions while compression is occurring.
- CVE-2005-1849 	2 Redhat, Zlib 	3 Enterprise Linux, Network Satellite, Zlib 	2025-04-03 	N/A
    inftrees.h in zlib 1.2.2 allows remote attackers to cause a denial of service (application crash) via an invalid file that causes a large dynamic tree to be produced.
- CVE-2004-0797 	1 Zlib 	1 Zlib 	2025-04-03 	N/A
    The error handling in the (1) inflate and (2) inflateBack functions in ZLib compression library 1.2.x allows local users to cause a denial of service (application crash).
- CVE-2003-0107 	2 Redhat, Zlib 	3 Enterprise Linux, Linux, Zlib 	2025-04-03 	N/A
    Buffer overflow in the gzprintf function in zlib 1.1.4, when zlib is compiled without vsnprintf or when long inputs are truncated using vsnprintf, allows attackers to cause a denial of service or possibly execute arbitrary code.
- CVE-2005-2096 	2 Redhat, Zlib 	3 Enterprise Linux, Network Satellite, Zlib 	2025-04-03 	N/A
    zlib 1.2 and later versions allows remote attackers to cause a denial of service (crash) via a crafted compressed stream with an incomplete code description of a length greater than 1, which leads to a buffer overflow, as demonstrated using a crafted PNG file.
- CVE-2002-0059 	2 Redhat, Zlib 	3 Linux, Powertools, Zlib 	2025-04-03 	9.8 Critical
    The decompression algorithm in zlib 1.1.3 and earlier, as used in many different utilities and packages, causes inflateEnd to release certain memory more than once (a "double free"), which may allow local and remote attackers to execute arbitrary code via a block of malformed compression data.
- CVE-2023-45853 	3 Redhat, Smihica, Zlib 	3 Jboss Core Services, Pyminizip, Zlib 	2024-12-20 	9.8 Critical
    MiniZip in zlib through 1.3 has an integer overflow and resultant heap-based buffer overflow in zipOpenNewFileInZip4_64 via a long filename, comment, or extra field. NOTE: MiniZip is not a supported part of the zlib product. NOTE: pyminizip through 0.2.6 is also vulnerable because it bundles an affected zlib version, and exposes the applicable MiniZip code through its compress API.
