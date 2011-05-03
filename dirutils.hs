module Main where

import System.IO
import System.Environment (getArgs)
import System.Directory (getDirectoryContents, doesDirectoryExist, canonicalizePath)
import System.FilePath ((</>))
import System.Posix.Types
import System.Posix.Files


data FileSystemNode = DirNode {path :: FilePath, nodes :: [FileSystemNode] } | FileNode {path :: FilePath, size :: FileOffset} deriving (Show)


createFileNode :: FilePath -> IO FileSystemNode
createFileNode path = do
    filesize <- getFileSize path
    let node = FileNode {
        path = path,
        size = filesize
    }
    return node

createDirNode :: FilePath -> IO FileSystemNode
createDirNode path = do
    filelist <- getDirectoryContents path
    let filelist2 = filter removeRelativeDirs filelist
    let filelist3 = map (makePath path) filelist2
    subnodes <- mapM createFileSystemNode filelist3
    let node = DirNode {
        path = path,
        nodes = subnodes
    }
    return node

createFileSystemNode :: FilePath -> IO FileSystemNode
createFileSystemNode path = do
    isDir <- doesDirectoryExist path
    node <- if isDir
        then createDirNode path
        else createFileNode path
    return node

getFileSize :: String -> IO FileOffset
getFileSize path = do
    stat <- getFileStatus path
    return (fileSize stat)

removeRelativeDirs :: String -> Bool
removeRelativeDirs x 
    | x == "." = False
    | x == ".." = False
    | otherwise = True 

makePath :: String -> FilePath -> FilePath
makePath x y = x </> y

-- calculating size of directory or file
getSize :: FileSystemNode -> FileOffset
getSize (FileNode {size = x}) = x
getSize (DirNode {nodes = x}) = sum $ map getSize x


-- human readable file size
humanSize :: (Fractional a, Ord a) => a -> [[Char]] -> [Char]
humanSize x bases
    | x >= 1024 = humanSize (x / 1024) (tail bases)
    | otherwise = (show x) ++ " " ++ (head bases)


main = do
    args <- getArgs
    path <- if length args > 0
                then canonicalizePath $ last args
                else canonicalizePath "."
    node <- createFileSystemNode $ path
    let size = fromIntegral $ getSize node
    --let dirsize = humanSize size ["Bytes", "Kb", "Mb", "Gb", "Tb"] 
    print size