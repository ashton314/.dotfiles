#!/usr/bin/env perl
use strict;
use warnings;

my $home = $ENV{HOME};
opendir my $dh, "$home/.dotfiles/"
  or die "Could not find .dotfiles/ in home directory";

while (readdir $dh) {
    next unless $_ =~ /^\./ && -e -f $_;
    print "Creating symlink for $_\n";
    symlink "$home/.dotfiles/$_", "$home/$_";
}
close $dh;
print "Done\n";
