return {
    "catppuccin/nvim",
    name = "catppuccin",
    priority = 1000,
    config = function()
        require("catppuccin").setup({
            transparent_background = true,
            float = {
                transparent = false,
                solid = false,
            },
        })
        vim.cmd.colorscheme("catppuccin")
    end,
}
