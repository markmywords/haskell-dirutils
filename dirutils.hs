module Main where

import System.IO
import System.Environment (getArgs)
import System.Directory (getDirectoryContents, doesDirectoryExist)
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
    content <- getDirectoryContents path
    let content2 = filter removeRelativeDirs content
    let content3 = map (makePath path) content2
    content4 <- mapM createFileSystemNode content3
    let node = DirNode {
        path = path,
        nodes = content4
    }
    return node

createFileSystemNode :: FilePath -> IO FileSystemNode
createFileSystemNode path = do
    isDir <- doesDirectoryExist path
    node <- if isDir
        then createDirNode path
        else createFileNode path
    return node

getFileSize :: FilePath -> IO (Integer)
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

-- print human readable filesizes. testing out guards.
--humanSize :: Integer -> String
--humanSize x
--    | x >= 1024 = (x / 1024) ++ " KB"
--    | otherwise = x  ++ " Bytes"


main = do
    args <- getArgs
    --let dir = "/home/mark/Development/studium/haskell-filesizes/"
    node <- createFileSystemNode $ last args
    let dirsize = getSize node
    print dirsize