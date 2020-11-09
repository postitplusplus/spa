module.exports = {
  theme: {
    fontFamily: {
      manrope : [ 'manrope', "sans-serif" ]
    },
    extend: {
      minWidth: {
        '64': '16rem',
      },
      minHeight: {
        '64': '16rem',
      },
    },
  },
  variants: {
    borderRadius: ['responsive', 'last'],
    borderWidth: ['responsive', 'last', 'hover', 'focus'],
    textDecoration: ['hover', 'responsive', 'group-hover'],
  },
  plugins: [],
}
