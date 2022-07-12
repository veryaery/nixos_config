vim.cmd [[ set packpath+=<packpath> ]]

vim.opt.number = true

vim.opt.expandtab = true
vim.opt.tabstop = 4
vim.opt.softtabstop = -1
vim.opt.shiftwidth = 0

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

treesitter.setup {
    ensure_installed = "all"
    auto_install = true

    highlight = {
        enable = true
        additional_vim_regex_highlighting = false
    }

    rainbow = {
        enable = true
    }
}

autopairs.setup {}
