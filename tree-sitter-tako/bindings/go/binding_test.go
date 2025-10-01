package tree_sitter_tako_test

import (
	"testing"

	tree_sitter "github.com/tree-sitter/go-tree-sitter"
	tree_sitter_tako "github.com/tree-sitter/tree-sitter-tako/bindings/go"
)

func TestCanLoadGrammar(t *testing.T) {
	language := tree_sitter.NewLanguage(tree_sitter_tako.Language())
	if language == nil {
		t.Errorf("Error loading Tako grammar")
	}
}
