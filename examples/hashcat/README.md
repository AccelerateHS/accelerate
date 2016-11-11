accelerate-hashcat
==================

The `accelerate-hashcat` program attempts to recover the plain text of an MD5
hash by comparing the unknown to the hash of every entry in a given dictionary,
which contains one word per line.

Some \*nix systems ship with an MD5 implementation which can be used to generate
hashes, try one of:

    $ md5 -s password
    MD5 ("password") = 5f4dcc3b5aa765d61d8327deb882cf99

    $ echo -n password | md5sum
    5f4dcc3b5aa765d61d8327deb882cf99  -

In the second example the `-n` argument to `echo` is required to omit the
trailing newline, which will change the computed hash value.

Standard dictionaries can also be found on most systems, and can be fed directly
into the program.

    $ accelerate-hashcat -s 5f4dcc3b5aa765d61d8327deb882cf99 -d /usr/share/dict/english

The program will also accept multiple unknowns to recover, either via multiple
`-s` arguments or read from file, one per line.

Of course, it is more fun if we don't know what what results to expect
beforehand, in which case a dictionary of standard words won't get us too far.
Luckily, the Internet is a
[playground](https://wiki.skullsecurity.org/index.php?title=Passwords)...

    $ accelerate-hashcat -d rockyou.txt md5.txt

