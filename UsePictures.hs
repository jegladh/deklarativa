import Pictures

blackHorse :: Picture
blackHorse = invertColour horse


rotateHorse :: Picture
rotateHorse = rotate horse

black :: Picture
black = superimpose blackHorse horse

checkers :: Picture
checkers = 