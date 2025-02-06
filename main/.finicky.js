// Use https://finicky-kickstart.now.sh to generate basic configuration
// Learn more about configuration options: https://github.com/johnste/finicky/wiki/Configuration

module.exports = {
    defaultBrowser: "Google Chrome",
    options: {
        hideIcon: false,
        checkForUpdate: true,
    },
    handlers: [
        {
            match: ({ url }) => url.protocol === "slack",
            browser: "Slack",
        },
        {
            match: "https://discord.com/*",
            url: { protocol: "discord" },
            browser: "Discord",
        },
        {
            // Open Apple Music links directly in Music.app
            match: ["music.apple.com*", "geo.music.apple.com*"],
            url: {
                protocol: "itmss",
            },
            browser: "Music",
        },
        {
            match: /meet\.google\.com/i,
            browser: {
                name: "Google Chrome",
                profile: "Profile 2",
            },
        },
        {
            match: /tobiko/i,
            browser: {
                name: "Google Chrome",
                profile: "Profile 2",
            },
        },
        {
            match: /paypal\.com/,
            browser: {
                name: "Google Chrome",
                profile: "Default",
            },
        },
        {
            match: /reddit\.com/,
            browser: {
                name: "Google Chrome",
                profile: "Default",
            },
        },
        {
            match: /18xx\.games/,
            browser: {
                name: "Google Chrome",
                profile: "Default",
            },
        },
        {
            match: /twitch\.tv/,
            browser: {
                name: "Google Chrome",
                profile: "Default",
            },
        },
        {
            match: /boardgamearena\.com/,
            browser: {
                name: "Google Chrome",
                profile: "Default",
            },
        },
        {
            match: /rally-the-troops\.com/,
            browser: {
                name: "Google Chrome",
                profile: "Default",
            },
        },
        {
            match: /notion\.so/,
            browser: {
                name: "Google Chrome",
                profile: "Default",
            },
        },
        {
            match: /steampowered/,
            browser: {
                name: "Google Chrome",
                profile: "Default",
            },
        },
        {
            match: /kelsin/,
            browser: {
                name: "Google Chrome",
                profile: "Default",
            },
        },
        {
            match: /kels\.in/,
            browser: {
                name: "Google Chrome",
                profile: "Default",
            },
        },
        {
            match: /hrf\.im/,
            browser: {
                name: "Google Chrome",
                profile: "Default",
            },
        },
        {
            match: /youtube\.com/,
            browser: {
                name: "Google Chrome",
                profile: "Default",
            },
        },
        {
            match: /youtu\.be/,
            browser: {
                name: "Google Chrome",
                profile: "Default",
            },
        },
    ],
};
