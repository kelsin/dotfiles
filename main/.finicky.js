// Use https://finicky-kickstart.now.sh to generate basic configuration
// Learn more about configuration options: https://github.com/johnste/finicky/wiki/Configuration

export default {
    defaultBrowser: "Google Chrome",
    options: {
        checkForUpdates: true,
    },
    handlers: [
        {
            match: (url) => url.protocol === "slack:",
            browser: "Slack",
        },
        {
            match: [
                "https://discord.com/*",
                (url) => url.protocol === "discord:",
            ],
            browser: "Discord",
        },
        {
            // Open Apple Music links directly in Music.app
            match: [
                "music.apple.com*",
                "geo.music.apple.com*",
                (url) => url.protocol === "itmss:",
            ],
            browser: "Music",
        },
        {
            match: /meet\.google\.com/i,
            browser: {
                name: "Google Chrome",
                profile: "Work",
            },
        },
        {
            match: /tobiko/i,
            browser: {
                name: "Google Chrome",
                profile: "Work",
            },
        },
        {
            match: /incident\.io/i,
            browser: {
                name: "Google Chrome",
                profile: "Work",
            },
        },
        {
            match: /paypal\.com/,
            browser: {
                name: "Google Chrome",
                profile: "Kelsin",
            },
        },
        {
            match: /ebay\.com/,
            browser: {
                name: "Google Chrome",
                profile: "Kelsin",
            },
        },
        {
            match: /itch\.io/,
            browser: {
                name: "Google Chrome",
                profile: "Kelsin",
            },
        },
        {
            match: /reddit\.com/,
            browser: {
                name: "Google Chrome",
                profile: "Kelsin",
            },
        },
        {
            match: /18xx\.games/,
            browser: {
                name: "Google Chrome",
                profile: "Kelsin",
            },
        },
        {
            match: /twitch\.tv/,
            browser: {
                name: "Google Chrome",
                profile: "Kelsin",
            },
        },
        {
            match: /boardgamearena\.com/,
            browser: {
                name: "Google Chrome",
                profile: "Kelsin",
            },
        },
        {
            match: /rally-the-troops\.com/,
            browser: {
                name: "Google Chrome",
                profile: "Kelsin",
            },
        },
        {
            match: /notion\.so/,
            browser: {
                name: "Google Chrome",
                profile: "Kelsin",
            },
        },
        {
            match: /steampowered/,
            browser: {
                name: "Google Chrome",
                profile: "Kelsin",
            },
        },
        {
            match: /kelsin/,
            browser: {
                name: "Google Chrome",
                profile: "Kelsin",
            },
        },
        {
            match: /kels\.in/,
            browser: {
                name: "Google Chrome",
                profile: "Kelsin",
            },
        },
        {
            match: /hrf\.im/,
            browser: {
                name: "Google Chrome",
                profile: "Kelsin",
            },
        },
        {
            match: /youtube\.com/,
            browser: {
                name: "Google Chrome",
                profile: "Kelsin",
            },
        },
        {
            match: /youtu\.be/,
            browser: {
                name: "Google Chrome",
                profile: "Kelsin",
            },
        },
    ],
};
