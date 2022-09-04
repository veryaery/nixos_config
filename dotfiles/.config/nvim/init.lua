vim.opt.packpath = vim.opt.packpath + "{{ packpath }}"

local typescript = "{{ pkgs.typescript }}"

vim.opt.number = true

vim.opt.expandtab = true
vim.opt.tabstop = 4
vim.opt.softtabstop = -1
vim.opt.shiftwidth = 0

vim.opt.clipboard = "unnamedplus"

vim.opt.splitright = true

vim.keymap.set("n", "<A-l>", "<C-w>l")
vim.keymap.set("n", "<A-h>", "<C-w>h")
vim.keymap.set("n", "<A-j>", "<C-w>j")
vim.keymap.set("n", "<A-k>", "<C-w>k")

vim.keymap.set("v", "<lt>", "<lt>gv")
vim.keymap.set("v", ">", ">gv")

vim.keymap.set("v", "<A-j>", ":m '>+1<CR>gv=gv")
vim.keymap.set("v", "<A-k>", ":m '<-2<CR>gv=gv")

-- LSP mappings
-- Recommended by https://github.com/neovim/nvim-lspconfig
vim.keymap.set("n", "<space>e", vim.diagnostic.open_float)
vim.keymap.set("n", "[d", vim.diagnostic.goto_prev)
vim.keymap.set("n", "]d", vim.diagnostic.goto_next)
vim.keymap.set("n", "<space>q", vim.diagnostic.setloclist)

vim.g.nix_recommended_style = false

local theme_name = os.getenv("NIXOSCFG_THEME_NAME")

local ok, err = pcall(require, "themes." .. theme_name)
if not ok then
    print("Require error theme " .. theme_name .. ": " .. err)
end

local ok, lspconfig = pcall(require, "lspconfig")
if not ok then
    print "Require error lspconfig"
    return
end

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

local ok, Comment = pcall(require, "Comment")
if not ok then
    print "Require error Comment"
    return
end

local ok, cmp = pcall(require, "cmp")
if not ok then
    print "Require error cmp"
    return
end

local ok, luasnip = pcall(require, "luasnip")
if not ok then
    print "Require error luasnip"
    return
end

local ok, cmp_nvim_lsp = pcall(require, "cmp_nvim_lsp")
if not ok then
    print "Require error cmp_nvim_lsp"
    return
end

local ok, telescope = pcall(require, "telescope")
if not ok then
    print "Require error telescope"
    return
end

local ok, telescope_builtin = pcall(require, "telescope.builtin")
if not ok then
    print "Require error telescope.builtin"
    return
end

local ok, telescope_actions = pcall(require, "telescope.actions")
if not ok then
    print "Require error telescope.actions"
    return
end

function on_attach(client, bufn)
    local opts = {
        buffer = bufn,
        noremap = true
    }

    -- LSP mappings
    -- Recommended by https://github.com/neovim/nvim-lspconfig
    vim.keymap.set("n", "gD", vim.lsp.buf.declaration, opts)
    vim.keymap.set("n", "gd", vim.lsp.buf.definition, opts)
    vim.keymap.set("n", "K", vim.lsp.buf.hover, opts)
    vim.keymap.set("n", "gi", vim.lsp.buf.implementation, opts)
    vim.keymap.set("n", "<C-k>", vim.lsp.buf.signature_help, opts)
    vim.keymap.set("n", "<space>wa", vim.lsp.buf.add_workspace_folder, opts)
    vim.keymap.set("n", "<space>wr", vim.lsp.buf.remove_workspace_folder, opts)
    vim.keymap.set("n", "<space>wl", function()
        print(vim.inspect(vim.lsp.buf.list_workspace_folders()))
    end, opts)
    vim.keymap.set("n", "<space>D", vim.lsp.buf.type_definition, opts)
    vim.keymap.set("n", "<space>rn", vim.lsp.buf.rename, opts)
    vim.keymap.set("n", "<space>ca", vim.lsp.buf.code_action, opts)
    vim.keymap.set("n", "gr", vim.lsp.buf.references, opts)
    vim.keymap.set("n", "<space>f", vim.lsp.buf.formatting, opts)
end

local capabilities = vim.lsp.protocol.make_client_capabilities()
capabilities = cmp_nvim_lsp.update_capabilities(capabilities)

lspconfig["tsserver"].setup {
    cmd = { "typescript-language-server", "--stdio", "--tsserver-path=" .. typescript .. "/lib/node_modules/typescript/lib" },
    capabilities = capabilities,
    on_attach = on_attach
}

lspconfig["rust_analyzer"].setup {
    on_attach = on_attach,
    settings = {
        ["rust-analyzer"] = {}
    }
}

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
    snippet = {
        expand = function(args)
            luasnip.lsp_expand(args.body)
        end
    },
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
        { name = "nvim_lsp" }
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

Comment.setup {}

telescope.setup {
    defaults = {
        mappings = {
            i = {
                ["<C-j>"] = telescope_actions.move_selection_next,
                ["<C-k>"] = telescope_actions.move_selection_previous
            }
        }
    }
}

vim.keymap.set("n", "<A-f>", function()
    telescope_builtin.buffers {}
end)
