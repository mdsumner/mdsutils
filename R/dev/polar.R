xyz <- function () 
{
    DEG2RAD = pi/180
    azrad = seq(-pi, pi, length = 40)
    diprad = seq(pi, 0, length = 20)
    azdip <- expand.grid(azrad, diprad)
    azrad <- azdip[,1]
    diprad <- azdip[,2]
    
    z = cos(diprad)
    temp = sin(diprad)
    x = cos(azrad) * temp
    y = sin(azrad) * temp
    len = sqrt(x * x + y * y + z * z)
    z = z/len
    x = x/len
    y = y/len
    return(list(x = x, y = y, z = z))
}
 plot3d(xyz())
 

