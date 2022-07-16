vim.opt.packpath = vim.opt.packpath + "<packpath>"

vim.opt.number = true

vim.opt.expandtab = true
vim.opt.tabstop = 4
vim.opt.softtabstop = -1
vim.opt.shiftwidth = 0

vim.opt.clipboard = "unnamedplus"

vim.opt.splitright = true

vim.keymap.set("n", "<C-n>", ":bnext<CR>")
vim.keymap.set("n", "<C-p>", ":bprevious<CR>")

vim.keymap.set("n", "<C-l>", "<C-w>l")
vim.keymap.set("n", "<C-h>", "<C-w>h")
vim.keymap.set("n", "<C-j>", "<C-w>j")
vim.keymap.set("n", "<C-k>", "<C-w>k")

vim.g.nix_recommended_style = false

local ok, treesitter = pcall(require, "nvim-treesitter.configs")
if not ok then
    print "Require error nvim-treesitter"
    return
end

local ok, autopairs = pcall(require, "nvim-autopairs")
if not ok then
    print "Require error nvim-autopairs"
    return
end

local ok, indentblankline = pcall(require, "indent_blankline")
if not ok then
    print "Require error indent_blankline"
    return
end

local ok, tree = pcall(require, "nvim-tree")
if not ok then
    print "Require error nvim-tree"
    return
end

local ok, gitsigns = pcall(require, "gitsigns")
if not ok then
    print "Require error gitsigns"
    return
end

local ok, bufferline = pcall(require, "bufferline")
if not ok then
    print "Require error bufferline"
    return
end

treesitter.setup {
    ensure_installed = "all",
    sync_install = false,
    auto_install = true,

    highlight = {
        enable = true,
        additional_vim_regex_highlighting = false
    },

    indent = {
        enable = true
    },

    rainbow = {
        enable = true
    }
}

autopairs.setup {
    check_ts = true
}

indentblankline.setup {}

tree.setup {
    hijack_cursor = true,

    view = {
        mappings = {
            list = {
                { key = { "<CR>", "l" }, action = "edit" }
            }
        }
    },

    renderer = {
        indent_markers = {
            enable = true
        }
    }
}

gitsigns.setup {}

bufferline.setup {
    options = {
        show_buffer_close_icons = false
    }
}
