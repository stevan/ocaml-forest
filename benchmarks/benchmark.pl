#!/usr/bin/perl

use strict;
use warnings;

use Time::HiRes qw/gettimeofday tv_interval/;
use Tree::Simple;
use Tree::Parser;

my $start = [gettimeofday];
my $tp = Tree::Parser->new("t/data/test.tree");
$tp->useSpaceIndentedFilters(4);
my $tree = $tp->parse();
print "seconds to parse tree: ", tv_interval($start), "\n";
print "size of tree: ", $tree->size(), "\n";