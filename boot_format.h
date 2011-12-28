/*
 * Copyright (C) 2011 Freescale Semiconductor, Inc.
 *
 * Author: Chen Gong
 * 	   Mingkai Hu <Mingiak.hu@freescale.com>
 *
 * This file is used for putting config words and U-Boot image to SDCard
 * when boot from SDCard, or to SPI image when boot from SPI flash. This
 * code puts the config words into the 1st partition, so please ensure
 * the size is enough on the 1st partition.
 *
 * NOTE: DON'T support FAT12 (it looks obsolete)
 *       DON'T support 64bit machine (it looks not necessary)
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston,
 * MA 02111-1307 USA
 */

#define CONFIG_REV_SPACE 0

/**
 * MMC/SD card bootup application code utility
 *
 * This application implements writing the bootup code to MMC/SD cards.
 *
 * It contains
 *  Offset     Data Bits [0:31]
 * 0x00-0x3F   Reserved
 * 0x40-0x43   BOOT signature. This location should contain the value 0x424f_4f54,
 * 0x44-0x47   Reserved
 * 0x48-0x4B   User's code length. Number of bytes in the user's code to be copied.
 *             It must be a multiple of the SD/MMC cards block size, It <= 2GBytes.
 * 0x4C-0x4F   Reserved
 * 0x50-0x53   Source Address. Contains the starting address of the user's code
 *             as an offset from the SD/MMC card starting address. In Standard
 *             Capacity SD/MMC Cards, it specifies the memory address in byte
 *             address format. This must be a multiple of the SD/MMC cards block
 *             size. In High Capacity SD/MMC Cards (>2GByte), the Address specifies
 *             the memory address in block address format. Block length is fixed
 *             to 512 bytes as per the SD High Capacity specification.
 * 0x54-0x57   Reserved
 * 0x58-0x5B   Target Address. Contains the target address in the system's local
 *             memory address space in which the user's code will be copied to.
 * 0x5C-0x5F   Reserved
 * 0x60-0x63   Execution Starting Address. Contains the jump address in the
 *             system's local memory address space into the user's code first
 *             instruction to be executed.
 * 0x64-0x67   Reserved
 * 0x68-0x6B   N. Number of Configuration Address/Data pairs. Must be 1<=N<=1024
 * 0x6C-0x7F   Reserved
 * 0x80-0x83   Configuration Address 1
 * 0x84-0x87   Configuration Data 1
 * 0x88-0x8B   Configuration Address 2
 * 0x8C-0x8F   Configuration Data 2
 *  ...
 * 0x80 + 8*(N-1) Configuration Address N
 * 0x80 + 8*(N-1)+4  Configuration Data N
 * ...
 * User's Code
 *
 * NOTE: N <= 40 (Offset can be from first page up to 24th page in a 512 block)
 */

#ifdef DEBUG
	#define debug(fmt, arg...) printf(fmt, ##arg)
#else
	#define debug(fmt, arg...)
#endif

#define BOOT_IMAGE_LEN_OFF	0x48
#define BOOT_IMAGE_ADDR_OFF	0x50
#define BOOT_MAX_CONFIG_WORDS   64

#define BOOT_WORK_MODE_SD	0x1
#define BOOT_WORK_MODE_SPI	0x2
#define BOOT_REV_SPACE		0	/* Reserve spave for Env variabls */

#define MBR_DPT_OFF	0x1be
#define SECTOR_SIZE	512
#define SEC_TO_BYTE(x)	((x) * SECTOR_SIZE)
#define FSTYPE_FAT16	0
#define FSTYPE_FAT32	1
#define FAT32_BOUNDARYUNIT   8192
#define EXFAT_BOUNDARYUNIT   32768

#define LITTLE_ENDIAN_MODE	0
#define BIG_ENDIAN_MODE		1

#include <stdint.h>

#define uchar	uint8_t
#define ushort	uint16_t
#define uint	uint32_t

#define MSG_OPEN_FILE_FAIL "Fail to open file \"%s\". Pls check whether it " \
			   "exists and verify your permission.\n"

#define MSG_READ_FILE_FAIL "Fail to read file \"%s\". Pls check whether it " \
			   "exists and verify your read permission.\n"

#define MSG_WRITE_FILE_FAIL "Fail to write file \"%s\". Pls check whether it "\
			    "exists and verify your write permission.\n"

#define MSG_SIGNATURE_FAIL "Fail to find MBR/DBR. Pls check whether the card "\
			    "is formatted correctly\n"

#define MSG_PARTLENGTH_FAIL "Partition is too small to save the user code. "\
			    "Pls re-partition it again\n"

#define DELIMITER	':'
#define RET_FLG1	'\r'
#define RET_FLG2	'\n'

#define SWAP16(x) ((((x) & 0x00ff) << 8) | (((x) & 0xff00) >> 8))

#define SWAP32(x) ((((x) & 0x000000ff) << 24) | \
		   (((x) & 0x0000ff00) << 8)  | \
		   (((x) & 0x00ff0000) >> 8)  | \
		   (((x) & 0xff000000) >> 24))

struct config_word {
	uint off; /* if off = 0 means config word ends */
	uint val;
};

#pragma pack(1)
struct mbr_disk_part_tbl {
	uchar	boot_ind;	/* Current state of partition (00h=Inactive, 80h=Active) */
	uchar	start_head;	/* CHS address of first block in partition - Head */
	ushort	start_cylsec;	/* CHS address of first block in partition - Cylinder/Sector
			      		15 14 13 12 11 10 9 8 | 7 6      | 5 4 3 2 1 0
			      		Cylinder Bits 7 to 0  | Cylinder | Sector Bits 5 to 0
			      		                      | Bits9+8  |                    */
	uchar	part_type;	/* Type of Partition
					Value	Description
					00h	Unknown or Nothing
				 	01h	12-bit FAT
					04h	16-bit FAT (Partition Smaller than 32MB)
					05h	Extended MS-DOS Partition
					06h	16-bit FAT (Partition Larger than 32MB)
					0Bh	32-bit FAT (Partition Up to 2048GB)
					0Ch	Same as 0BH, but uses LBA1 13h Extensions
					0Eh	Same as 06H, but uses LBA1 13h Extensions
					0Fh	Same as 05H, but uses LBA1 13h Extensions */
	uchar	end_head;	/* CHS address of last block in partition - Head*/
	ushort	end_cylsec;	/* CHS address of first block in partition - Cylinder/Sector */
	uint	rel_sectors;	/* LBA of first sector in the partition */
	uint	total_sectors;	/* Number of sectors in the partition */
};

/*
 * BIOS Parameter Block
 *
 * BPB is a data structure in the Volume Boot Record describing the physical
 * layout of a data storage volume. A basic BPB can appear and be used on any
 * partition, however, certain filesystems also make use of it in describing
 * basic filesystem structures. Filesystems making use of a BIOS parameter
 * block include FAT16, FAT32, HPFS, and NTFS.
 */
struct bpb {
	ushort	sector_size;		/* 0x0B: Byte Per Sector - must be one of 512, 1024, 2048, 4096 */
	uchar	sectors_p_cluster;	/* 0x0D: Sectors Per Cluster - must be one of 1, 2, 4, 8, 16, 32, 64, 128 */
	ushort	reserved_sectors;	/* 0x0E: Number of reserved sectors, FAT16 = 1. FAT32 = 32 */
	uchar	fat_copys;		/* 0x10: Number of FAT copies - 1 or 2 */
	ushort	root_entries;		/* 0x11: Number of root directory entries.
						0 for FAT32(that means FAT32 doesn't use this field).
						512 is recommended for FAT16i */
	ushort	small_sectors;		/* 0x13: Number of small sectors
						FAT32: must be 0
						FAT16: in case the partition is not FAT32 and smaller than 32 MB */
	uchar	media_type;		/* 0x15: f0: 1.4 MB floppy, f8: hard disk */
	ushort	sectors_p_fat;		/* 0x16: Number of sectors per FAT. 0 for FAT32 */
	ushort	sector_p_track;		/* 0x18: Number of sectors per track */
	ushort	heads;			/* 0x1A: Number of heads */
	uint	hidden_sectors;		/* 0x1C: Number of hidden sectors */
	uint	large_sectors;		/* 0x20: in case the partition is FAT32 or larger than 32 MB */
};

/* Extended BPB */
struct ext_bpb {
	uchar	phy_disk_num;		/* Physical drive number */
	uchar	res;
	uchar	ext_boot_signature;	/* Extended boot signature. It must be 0x28 or 0x29 */
	uint	vol;			/* Serial number of partition */
	uchar	vol_label[11];
	char	fs_type[8];		/* Filesystem type (E.g. "FAT32 ", "FAT16 ", or all zero.) */
};

struct ext_bpb_fat32 {
	uint	sectors_p_fat;		/* 0x24: Sectors per FAT. Only used for FAT32 */
	ushort	ext_flag;		/* Bits 0-3: number of active FAT (if bit 7 is 1)
						Bits 4-6: reserveds
						Bit 7: one: single active FAT; zero: all FATs are updated at runtimes
						Bits 8-15: reserved */
	ushort	fs_version;		/* Filesystem version */
	uint	root_cluster;		/* First cluster of root directory (usually 2) */
	ushort	fs_info;		/* Filesystem information sector number in reserved area (usually 1) */
	ushort	backup_boot_sector;	/* Backup boot sector location: 0 or 0xffff if none (usually 6) */
	char	res[12];
};

struct boot_sector {
	uchar	jmp_code[3];
	char	oem[8];
	struct	bpb bpb;
	union {
		struct bpb16 {
			struct ext_bpb ext_bpb;
		}fat16;
		struct bpb32 {
			struct ext_bpb_fat32 ext;
			struct ext_bpb ext_bpb;
		}fat32;
	};
};
#pragma pack()
