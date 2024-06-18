module.exports = {
  configureWebpack: (config, env) => {
    // Manipulate the config object and return it.
    config.module.rules[2].use[0].options.plugins = [ "@babel/plugin-transform-class-properties" ];

    return config;
  }
}