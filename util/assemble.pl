#!/usr/bin/perl

use strict;
use warnings;

sub slurp { 
    my $file      = shift;
    my $seperator = shift || "\n";
    open F, "<", $file || die "Cannot open $file because $!";
    my $contents = join $seperator => map { chomp; $_ } <F>;
    close F || die "Cannot close $file because $!";
    return $contents;
}

my $out_file_name = 'forest.ml';

my $source = "(* Generated by util/assemble.pl on " . scalar(localtime) . " *)\n\n";
$source .= slurp "lib/forest.ml";
$source .= "\n\n";
$source .= slurp "lib/forest_types.ml";
$source .= "\n\n";
foreach (qw[
        treeReader
        treeWriter
        treeIndexer
        ]) {
    $source .= "module " . ucfirst($_) . " =\nstruct\n\n    ";
    $source .= slurp("lib/" . $_ . ".ml", "\n    ");
    $source .= "\n\nend\n\n";
}

open OUT, ">", $out_file_name || die "Cannot open $out_file_name because $!";
print OUT $source;
close OUT;

1;