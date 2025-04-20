# Program:     tower3.R
# Programmer:  Sean R. McCorkle
# Language:    R
#
#  requires R library "rgl" from CRAN for 3-d renderig
#

library( rgl )

                          ###############
                          # Subroutines #
                          ###############

#
# dump out x,y,z coordinates of a triangle's points (i.e. the tops of struts)
#
print_triangle <- function( title, x, y, z )
   {
    cat( title, "\n" )
    print( x )
    print( y )
    print( z )
   }

#
# returns a vector of (x,y,z), angle theta in radians
#

rotate <- function( theta, x, y, z )
   {
    xp <-  x * cos( theta ) + y * sin( theta )
    yp <- -x * sin( theta ) + y * cos( theta )
    zp <-  z
    c( xp, yp, zp )
   }

#
# Tower constant parameters
# 
cyl_rad    <- (3/8) / 2                     # inches, radius of rods (3/8" dowels)
beta       <- (1/2) * (pi / 3 + pi / 4)     # angle from horizontal of rods
alpha      <- pi / 10                       # angle (ccw) from base triangle line 
#rot_offset <- pi / 4                        # how much each prism is rotated wrt previous
rot_offset <- pi / 3                        # how much each prism is rotated wrt previous

# base level parameters

L0 <- 48                  # inches, length of base rods
b0 <- 32                  # inches, length of base triagle (cable)


# 
# globals
#

full_height <- 0   

#########################################################################
# create_prism( level, z_base, b, L, rot_angle=0 )
#
# this creates coordinates and connections for one layer of three strut prism 
# (https://tensegritywiki.com/index.php?title=Prism)
#
# z_base: level of base of this prism
# b0 is x coordinate of starting corner of triangular base
# L is length of compression rod
# rot_angle is rotation from previous layer

create_prism <- function( level, z_base, b, L, rot_angle=0 )
   {
    # Bottom triangle 
    
    xb <- c( 0, 0, 0 )   # xb, yb, zb - arrays of length 3, coordinates of base corners
    yb <- c( 0, 0, 0 )   #              one index for each cornder of prism, 
    zb <- c( 0, 0, 0 )   #              these are bottom positions of the cylinders
    
    xt <- c( 0, 0, 0 )   # xt, yt, zt - arrays of length 3, coordinates of top corners
    yt <- c( 0, 0, 0 )   #              of cylinders, one for each cylinder
    zt <- c( 0, 0, 0 )

    # 
    # inital values for coordinates for first rod corner (base)
    xb[1] <- b / sqrt( 3 )
    yb[1] <- 0
    zb[1] <- z_base

    cat( "position 1\n")
    print_triangle( "base triangle", xb, yb, zb )
    
    xt[1] <- L * cos( beta ) * cos( 5 * pi/6 + alpha ) + xb[1]
    yt[1] <- L * cos( beta ) * sin( 5 * pi/6 + alpha )
    zt[1] <- L * sin( beta ) + zb[1]

    print_triangle( "upper triangle", xt, yt, zt )

    # rotate by angle offset for upper level

    if ( level > 1 )
       {
        cat( "level ", level, " initial rotations by ", rot_angle, "\n" )
        rxyz <- rotate( rot_angle, xb[1], yb[1], zb[1] )
        xb[1] <- rxyz[1]
        yb[1] <- rxyz[2]
        zb[1] <- rxyz[3]

        rxyz <- rotate( rot_angle, xt[1], yt[1], zt[1] )
        xt[1] <- rxyz[1]
        yt[1] <- rxyz[2]
        zt[1] <- rxyz[3]
       }
    print_triangle( "base triangle now", xb, yb, zb )
    print_triangle( "upper triangle now", xt, yt, zt )

    # That completes the coordinates of the first corner of the prism.
    # Now calculate the remained by rotating the previous by 2 * pi / 3

    for ( j in 1:2 )
       {
        cat( "rotation ", j, "\n" )
    
        rxyz <- rotate( 2 * pi / 3, xb[j], yb[j], zb[j] )
        print( rxyz )
        xb[j+1] <- rxyz[1]
        yb[j+1] <- rxyz[2]
        zb[j+1] <- rxyz[3]
        print_triangle( "rot1 base triangle", xb, yb, zb )
        
        rxyz <- rotate( 2 * pi / 3, xt[j], yt[j], zt[j] )
        xt[j+1] <- rxyz[1]
        yt[j+1] <- rxyz[2]
        zt[j+1] <- rxyz[3]
        print_triangle( "rot1 upper triangle", xt, yt, zt )
       }

    list( xb = xb, yb = yb, zb = zb, 
          xt = xt, yt = yt, zt = zt
        )
   }

distance <- function( x1, x2, y1, y2, z1, z2 )
   return( sqrt( (x1-x2)^2 + (y1-y2)^2 + (z1-z2)^2 ) )

report <- function( ... )
    cat( "report: ", ..., "\n" )

#
# this plots/renders the dowels (cylinder structs) for one prism level
#
plot3d_struts <- function( pr )
   {
    for ( i in 1:3 )
       {
        dowel_top_centers <- cbind( c( pr$xb[i], pr$xt[i] ), c( pr$yb[i], pr$yt[i] ), 
                                c( pr$zb[i], pr$zt[i] ) )
        print( dowel_top_centers )
        dowel <- cylinder3d( dowel_top_centers, radius=cyl_rad )
        shade3d( dowel, col = "darkgreen" )
       }
   }

#
# this reports the lengths of the struts
#

report_struts <- function( msg, pr )
   {
    d <- distance( pr$xb[1], pr$xt[1], 
                   pr$yb[1], pr$yt[1], 
                   pr$zb[1], pr$zt[1] )
    report( msg," dowels: 3  ", " length = ", d )
   }

#
# this reports the number and length of a line segment (assumed to repeat)
#

report_lines <- function( msg, n, xs, ys, zs )
   {
    d <- distance( xs[1], xs[2],  ys[1], ys[2],  zs[1], zs[2] )
    report( msg, n, " count ", d, "\n" )
   }



plot3d_and_report_prism <- function( level, prev_pr, pr )
   {
    # dowels (compression rods)
    
    plot3d_struts( pr )
    report_struts( paste( "level", level ), pr )

    # base

    if ( level == 1 )
       {
        lines3d( c(pr$xb, pr$xb[1]), c(pr$yb, pr$yb[1]), c(pr$zb, pr$zb[1]), col="cornflowerblue" )
        report_lines( "base (level 1) lines ", 3, pr$xb[1:2], pr$yb[1:2], pr$zb[1:2] )
       }
         
    #lines3d( c(pr$xt, pr$xt[1]), c(pr$yt, pr$yt[1]), c(pr$zt, pr$zt[1]), col="darkorchid1")

    # version tension lines between top and bottom
    # note to self; put this in loop with modulo

    for ( j in 1:3 )
       {
        k <- (j + 1) %% 3 + 1
        lines3d( c( pr$xt[j], pr$xb[k] ), c( pr$yt[j], pr$yb[k] ),c( pr$zt[j], pr$zb[k] ),
                 col="orange")
        # pr 3 -> 2
       }
    report_lines( paste( "level", toString( level ),"vertical" ), 3, 
                  c( pr$xt[1], pr$xb[3] ), c( pr$yt[1], pr$yb[3] ), c( pr$zt[1], pr$zb[3] ) )

    # 
    if ( level > 1 )
        for ( j in 1:3 )
           {
            #k <- j
            #k <- (j + 1) %% 3 + 1
            k <- j %% 3 + 1
            lines3d( c( pr$xb[j], prev_pr$xb[k] ), c( pr$yb[j], prev_pr$yb[k] ), c( pr$zb[j], prev_pr$zb[k] ),
                     col="deeppink4" )
            # pr 3 -> 2
           }
    report_lines( paste( "level", toString( level ),"upper base vertical to lower" ), 3,
                  c( pr$xb[1], prev_pr$xb[2] ), c( pr$yb[1], prev_pr$yb[2] ), c( pr$zb[1], prev_pr$zb[2] ) )

    # Coordinate box

    bbox3d( col=c( "#FEFEFE", "black"), alpha=0.3 )
    #rgl.bbox(color=c("lightgrey","black"), 
    #      shininess=5, alpha=0.8 ) 
    #rgl.bbox(color=c("#333377","black"), emission="#333377",
    #     specular="#3333FF", shininess=5, alpha=0.8 ) 

    if ( level > 1 )
       {
        # string supporting lines between lower (previous) prism top and current 
        # prism bottom

        lines3d( c( prev_pr$xt[2], pr$xb[1],
                    prev_pr$xt[3], pr$xb[2],
                    prev_pr$xt[1], pr$xb[3],
                    prev_pr$xt[2] ),
                 c( prev_pr$yt[2], pr$yb[1],
                    prev_pr$yt[3], pr$yb[2],
                    prev_pr$yt[1], pr$yb[3],
                    prev_pr$yt[2] ),
                 c( prev_pr$zt[2], pr$zb[1],
                    prev_pr$zt[3], pr$zb[2],
                    prev_pr$zt[1], pr$zb[3],
                    prev_pr$zt[2] ),
             col="red" )

        report_lines( paste( "level", toString( level ),"base lines a" ), 3,
                      c( prev_pr$xt[2], pr$xb[1] ), c( prev_pr$yt[2], pr$yb[1] ), c( prev_pr$zt[2], pr$zb[1] ) )
        report_lines( paste( "level", toString( level ),"base lines b" ), 3,
                      c( pr$xb[1], prev_pr$xt[3] ), c( pr$yb[1], prev_pr$yt[3] ), c( pr$zb[1], prev_pr$zt[3] ) )


       }
   }



                          ################
                          # Main program #
                          ################


sink( "junk1.out" )
curr_z <- 0


#
# We start with the bottom prism, level 1
#

lower_prism <- create_prism( level = 1, z_base = curr_z, b0, L=L0 )
full_height <- max( lower_prism$zt )
cat( "full_height ", full_height, "\n" )

#  (no rotation at this level)

cat( "level 1 lower_prism is\n" )
print( lower_prism )

open3d()

plot3d_and_report_prism( 1, lower_prism, lower_prism )

#
# Now we iterate over all upper prisms, stacking them
#
b      <- b0
prev_L <- L0

for ( level in 2:3 )
   {
    # subsequent z 
    L                  <- prev_L - 4
    f                  <- L / prev_L
    b                  <- f * b
    prev_L             <- L

    # place this prism bottom 30% lower than previous prism top

    lower_prism_height <- lower_prism$zt[1] - lower_prism$zb[1]
    curr_z             <- lower_prism$zt[1] - 0.3 * lower_prism_height

    rot                <- (level - 1 ) * rot_offset

    cat( "level ", level, "lower_prism_height", lower_prism_height, " curr_z ", curr_z, 
         " b is ", b, "\n" ) 
    cat( "lower_prism is\n" )
    print( lower_prism )
    
    upper_prism <- create_prism( level = 2, z_base = curr_z, b, L, rot_angle = rot )
    full_height <- max( upper_prism$zt )
    cat( "full_height ", full_height, "\n" )
    
    cat( "level ", level, " prism is\n" )
    print( upper_prism )
    
    plot3d_and_report_prism( level, lower_prism, upper_prism )
    lower_prism <- upper_prism
   }

cat( "Program ends.   Max height ", full_height, "\n" )

sink()

#
# End program
#
##########################################################################

