vim.opt.packpath = vim.opt.packpath + "<packpath>"

vim.opt.number = true

vim.opt.expandtab = true
vim.opt.tabstop = 4
vim.opt.softtabstop = -1
vim.opt.shiftwidth = 0

vim.opt.clipboard = "unnamedplus"

vim.opt.splitright = true

vim.keymap.set("n", "<C-l>", ":bnext<CR>")
vim.keymap.set("n", "<C-h>", ":bprevious<CR>")

vim.keymap.set("n", "<A-l>", "<C-w>l")
vim.keymap.set("n", "<A-h>", "<C-w>h")
vim.keymap.set("n", "<A-j>", "<C-w>j")
vim.keymap.set("n", "<A-k>", "<C-w>k")

vim.keymap.set("v", "<lt>", "<lt>gv")
vim.keymap.set("v", ">", ">gv")

vim.keymap.set("v", "<A-j>", ":m '>+1<CR>gv=gv")
vim.keymap.set("v", "<A-k>", ":m '<-2<CR>gv=gv")

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

local ok, Comment = pcall(require, "Comment")
if not ok then
    print "Require error Comment"
    return
end

local ok, neorg = pcall(require, "neorg")
if not ok then
    print "Require error neorg"
    return
end

local ok, cmp = pcall(require, "cmp")
if not ok then
    print "Require error cmp"
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
    disable_filetype = { "norg" },
    check_ts = true
}

indentblankline.setup {}

cmp.setup {
    mapping = cmp.mapping.preset.insert {
        ["<C-Space>"] = cmp.mapping.complete(),
        ["<CR>"] = cmp.mapping.confirm { select = true },
        ["<C-j>"] = cmp.mapping.select_next_item(),
        ["<C-k>"] = cmp.mapping.select_prev_item(),
        ["<C-f>"] = cmp.mapping.scroll_docs(4),
        ["<C-b>"] = cmp.mapping.scroll_docs(4),
        ["<C-e>"] = cmp.mapping.abort()
    },
    sources = cmp.config.sources {
        { name = "buffer" },
        { name = "neorg" }
    }
}

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

Comment.setup {}

neorg.setup {
    load = {
        ["core.defaults"] = {},
        ["core.norg.concealer"] = {},
        ["core.norg.completion"] = {
            config = {
                engine = "nvim-cmp"
            }
        }
    }
}
