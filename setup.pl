#!/usr/bin/env perl
use strict;
use warnings;

use File::Copy;

my $home = $ENV{HOME};
opendir my $dh, "$home/.dotfiles/"
  or die "Could not find .dotfiles/ in home directory";

while (readdir $dh) {
    next unless $_ =~ /^\./ && -e -f $_;
    print "Creating symlink for $_\n";
    symlink "$home/.dotfiles/$_", "$home/$_";
}
close $dh;

if (-f "$home/.zshenv") {
    print "~/.zshenv already exists; overwrite? (y/N) ";
    chomp(my $resp = <STDIN>);
    unless ($resp eq 'y' or $resp eq 'yes') {
	copy("$home/.zshenv", "$home/.zshenv_bak");
	print "Old .zshenv copied to ~/.zshenv_bak\n";
    }
}
print "Recommend setting in ~/.zshenv:\nPATH=\"" . `echo \$PATH` . "\"\n";
copy("$home/.dotfiles/zshenv_template", "$home/.zshenv");

# Try moving the snippets into place
mkdir "$home/.emacs.d"
  unless -d "$home/.emacs.d";

symlink "$home/.dotfiles/snippets", "$home/.emacs.d/snippets";

# Try moving my emacs lockfile into place
mkdir "$home/.emacs.d/straight"
    unless -d "$home/.emacs.d/straight";

symlink "$home/.dotfiles/versions", "$home/.emacs.d/straight/versions";

print "Done\n\n";

print "Be sure to modify ~/.zshenv to suit your needs. It has been
copied, not symlinked into your home directory.\n\n"
