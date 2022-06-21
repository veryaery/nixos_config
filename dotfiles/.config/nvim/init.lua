local opts = {
    wrap = false,
    number = true,
    expandtab = true
}

for key, value in pairs(opts) do
    vim.opt[key] = value;
end