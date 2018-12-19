##
## Derive distance matrix for a given set of receivers within the boundaries
## af the provided water bodies - support functions
##
## Van Hoey S.
## Lifewatch INBO
##

library("sp")
library("rgdal")
library("rgeos")
library("raster")
library("gdistance")

library("assertthat")

## --------------------------------------------
## Functions as support for the mask adaptation
## to ensure the incorporation of the receivers
## --------------------------------------------

#' cell number to row/col index
#'
#' Transform cell number to row/col indices
#' (support function for adapt_binarymask)
#'
#' @param ids cell number in matrix
#' @param nrows number of rows of the matrix
#'
#' @return (id of row, id of column)
#'
#' @examples
get_rowcol <- function(ids, nrows){
    rem <- ids %% nrows
    if (rem == 0) {
        row <- nrows
        col <- ids %/% nrows
    } else {
        row <- ids %% nrows
        col <- (ids %/% nrows) + 1
    }
    rowcol <- c(row, col)
    return(rowcol)
}

#' Extend small patches
#'
#' Change a Moore environment to 1 values around the provided cell ids
#' (support function for adapt_binarymask)
#'
#' @param inputmat RasterLayer to adjust the cells from
#' @param ids the identifiers defining the cells for which the environment is
#' added
#'
#' @return RasterLayer
#'
#' @examples
extend_patches <- function(inputmat, ids){
    # inputmat -> matrix
    nrows <- nrow(inputmat)
    crdnts <- sapply(ids, get_rowcol, nrows)
    for (i in 1:ncol(crdnts) ) {
        inputmat[(crdnts[1, i] - 1):(crdnts[1, i] + 1), (crdnts[2, i] - 1):(crdnts[2, i] + 1)] <- 1
    }
    return(inputmat)
}

#' Get patch info
#'
#' Extract the information about the patches and their respective sizes
#' (support function for adapt_binarymask)
#'
#' @param inputlayer RasterLayer for which to extract the patch information
#'
#' @return vector with zone id and the sum of cells for each zone
#' @export
#'
#' @examples
#' get_patches_info(study.area.binary)
get_patches_info <- function(inputlayer){
    patch_count <- clump(inputlayer)
    # derive surface (cell count) for each patch
    patchCells <- zonal(inputlayer, patch_count, "sum")
    # sort to make last row main one
    patchCells <- patchCells[sort.list(patchCells[, 2]), ]
    return(patchCells)
}

#' Extend binary mask
#'
#' Extend the binary mask to incorporate the receivers itself into the mask
#'
#' @param binary.mask RasterLayer (0/1 values)
#' @param receivers SpatialPointsDataFrame
#'
#' @return RasterLayer
#' @export
#'
#' @examples
adapt_binarymask <- function(binary.mask, receivers){

    # add locations itself to raster as well:
    locs2ras <- rasterize(receivers, binary.mask, 1.)
    locs2ras[is.na(locs2ras)] <- 0
    study.area.binary <- max(binary.mask, locs2ras)

    patch_count <- clump(study.area.binary)
    patchCells <- zonal(study.area.binary, patch_count, "sum")
    patchCells <- patchCells[sort.list(patchCells[, 2]), ]
    n.patches <- nrow(patchCells)

    # check current number of patches
    print(n.patches)

    while (!is.null(n.patches)) {
        # first row indices of the single patches extended
        ids <- which(as.matrix(patch_count) == patchCells[1, 1])
        temp <- as.matrix(study.area.binary)
        temp <- extend_patches(temp, ids)
        study.area.binary <- raster(temp, template = study.area.binary)

        # patches definition etc
        patch_count <- clump(study.area.binary)
        # derive surface (cell count) for each patch
        patchCells <- zonal(study.area.binary, patch_count, "sum")
        # sort to make last row main one
        patchCells <- patchCells[sort.list(patchCells[, 2]), ]
        # check current number of patches
        n.patches <- nrow(patchCells)
        print(n.patches)
    }
    return(study.area.binary)
}

#' Check patch characteristics
#'
#' Control the charactersitics of the binary mask:
#' 1. patch of connected cells
#' 2. all receivers are within the patch
#'
#' @param binary.mask RasterLayer with the patch of waterbodies
#' @param receivers SpatialPointsDataFrame with receiver location info
#'
#' @return TRUE is both tests are valid
#' @export
#'
#' @examples
control_mask <- function(binary.mask, receivers){
    match_ids <- raster::extract(binary.mask, receivers)
    match_ids[is.na(match_ids)] <- 0
    matched.receivers <- receivers[as.logical(match_ids), ]
    # CHECK:
    assert_that(length(receivers) == length(matched.receivers))

    # Check if area is one big environment without islands
    temp <- clump(binary.mask)
    # a single clump is what we want: min and max should be both == 1
    # CHECK:
    assert_that(cellStats(temp, stat = 'min', na.rm = TRUE) == 1)
    assert_that(cellStats(temp, stat = 'max', na.rm = TRUE) == 1)
}

#' Get distance matrix
#'
#' Calculate the cost distance matrix of the receivers
#'
#' @param binary.mask RasterLayer with the patch of waterbodies
#' @param receivers SpatialPointsDataFrame with receiver location info
#'
#' @return data.frame
#' @export
#'
#' @examples
get_distance_matrix <- function(binary.mask, receivers){
    tr <- transition(binary.mask, max, directions = 8)
    tr_geocorrected <- geoCorrection(tr, type = "c")

    cst.dst <- costDistance(tr_geocorrected, receivers)
    cst.dst.arr <- as.matrix(cst.dst)
    receiver_names <- as.data.frame(receivers)$station_name
    rownames(cst.dst.arr) <- receiver_names
    colnames(cst.dst.arr) <- receiver_names
    return(cst.dst.arr)
}




