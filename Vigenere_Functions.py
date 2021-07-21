#!/usr/bin/env python
# coding: utf-8

# In[ ]:


alphabet = ["a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","q","r","s","t","u","v","w","x","y","z"]

class Text:
    
    def __init__(self, string, key):
        """
        Class that creates an instance of a piece of text, and the cipherkey.
        
        Parameters:
        string (string): A piece of ciphertext, which could have punctuation or capital letters.
        key (string): A short word or random series of letters, all letters should be lower case.
        """
        self.string = string
        self.key = key
    
    def formatted(self, **kwargs):
        """
        Function that removes punctuation, capital letters and spaces from the string.
        
        Parameters:
        """
        edit = ""
        for i in self.string:
            if i in alphabet:
                edit = edit+i
        return edit
    
    def encrypt(self, ciphertext):
        """
        Function that returns the encrypted text for a given key.
        This function will break if the chosen alphabet has more or less than 26 letters.
        
        Parameters:
        ciphertext (string): The formatted string of text with no punctuation to be encrypted.

        """
        s1 = (len(ciphertext)//len(self.key))*self.key + self.key[:(len(ciphertext)-(len(ciphertext)//len(self.key)*len(self.key)))]
        
        #s1 is a string of equal length to the ciphertext which is just a repition of the key
        
        encrypted = ""
        for i in range(0,len(ciphertext)):
            j = ciphertext[i]
            k = s1[i]
            number = alphabet.index(j) + alphabet.index(k)
            encrypted = encrypted + alphabet[number % 26]
        
        #depending on the aligning characters in the ciphertext and s1, the letter in the encrypted text will be changed.
        #read up on the Vigenere Cipher if you are unsure what occurs here.
        
        return encrypted
    
    def decrypt(self, encrypted, keyword):
        """
        Function that returns the decrypted text using the best guess of the keyword.
        
        Parameters:
        encrypted (string): The scrambled/encrypted text encoded using the Vigenere Cipher
        keyword (string): The best guess for the keyword used to encode the plaintext.
        
        Returns: 
        decrypted (string): The recovered plaintext
        """
        
        decrypted = ""
        key = keyword
        key_matching_string = (len(encrypted)//len(key))*key + key[:(len(encrypted)-(len(encrypted)//len(key)*len(key)))]

        for i in range(0,len(encrypted)):
            j = encrypted[i]
            k=key_matching_string[i]
            number = alphabet.index(j) - alphabet.index(k)
            decrypted = decrypted + alphabet[number % 26]

        return decrypted

