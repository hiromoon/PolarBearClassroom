module.exports = {
  roots: [
    '<rootDir>',
  ],
  globals: {
    'ts-jest': {
      tsConfig: 'tsconfig.json',
      diagnostics: false,
    },
  },
  transform: {
    '^.+\\.ts?$': 'ts-jest',
  },
  testRegex: '(__tests__/.*|(\\.|/)(test|spec))\\.ts?$',
  moduleFileExtensions: [
    'ts',
    'tsx',
    'js',
    'jsx',
    'json',
    'node',
  ],
  preset: 'ts-jest',
  testEnvironment: 'node',
};
