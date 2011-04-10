module Main where

import System.IO
import System.Environment (getArgs)
import System.Directory (getDirectoryContents, doesDirectoryExist, canonicalizePath)
import System.FilePath ((</>))


data FileSystemNode = DirNode {path :: FilePath, nodes :: [FileSystemNode] } | FileNode {path :: FilePath, size :: Integer} deriving (Show)


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

getFileSize :: FilePath -> IO Integer
getFileSize x = do
    handle <- openFile x ReadMode
    size <- hFileSize handle
    hClose handle
    return size

removeRelativeDirs :: String -> Bool
removeRelativeDirs x 
    | x == "." = False
    | x == ".." = False
    | otherwise = True 

makePath :: String -> FilePath -> FilePath
makePath x y = x </> y

-- calculating size of directory or file
getSize :: FileSystemNode -> Integer
getSize (FileNode {size = x}) = x
getSize (DirNode {nodes = x}) = sum $ map getSize x

humanSize :: (Fractional a, Ord a) => a -> [[Char]] -> [Char]
humanSize x bases
    | x >= 1024 = humanSize (x / 1024) (tail bases)
    | otherwise = (show x) ++ " " ++ (head bases)

nodeLength :: FileSystemNode -> Integer
--nodeLength (DirNode {nodes = x}) = length nodes + 
nodeLength x = 1

main = do
    args <- getArgs
    path <- if length args > 0
                then canonicalizePath $ last args
                else canonicalizePath "."
    --let dir = "/home/mark/Development/studium/haskell-filesizes/"
    node <- createFileSystemNode $ path
    let size = fromInteger $ getSize node
    --let dirsize = humanSize $ (getSize node) 2 ["", "kb", "mb", "gb", "tb"]
    let dirsize = humanSize size ["Bytes", "Kb", "Mb", "Gb", "Tb"]
    let l = nodeLength node
    print dirsize
    print l