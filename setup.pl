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

# Try moving the snippets into place
mkdir "$home/.emacs.d"
  unless -d "$home/.emacs.d";

symlink "$home/.dotfiles/snippets", "$home/.emacs.d/snippets";

print "Done\n";
