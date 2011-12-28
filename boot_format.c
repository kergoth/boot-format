/*
 * Copyright (C) 2011 Freescale Semiconductor, Inc.
 *
 * Author: Chen Gong
 * 	   Mingkai Hu <Mingiak.hu@freescale.com>
 *     Jimmy Zhao <jimmy.zhao@freescale.com>
 *  Rev: 1.0    Initial release
 *       1.1:   Bug fix for generating SPI image
 *              All the source code offset is byte mode instead of block mode
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

#define _LARGEFILE64_SOURCE
#define _GNU_SOURCE
#define DEBUG 1

#include <sys/types.h>
#include <sys/stat.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <fcntl.h>
#include <unistd.h>
#include <getopt.h>
#include <errno.h>
#include "boot_format.h"

/*
 * Print data buffer in hex and ascii form to the terminal.
 *
 * parameters:
 *    data: pointer to data buffer
 *    len: data length
 *    width: data value width.  May be 1, 2, or 4.
 */
static void print_buf(void *data, uint len, uint width)
{
#ifdef DEBUG
	uint i;
	uint *uip = (uint *)data;
	ushort *usp = (ushort *)data;
	uchar *ucp = (uchar *)data;

	debug("\n");
	for (i = 0; i < len/width; i++) {
		if ((i % (16/width)) == 0)
			debug("0x%04x:", i);

		if (width == 4)
			debug(" %08x", uip[i]);
		else if (width == 2)
			debug(" %04x", usp[i]);
		else
			debug(" %02x", ucp[i]);

		if (((i+1) % (16/width)) == 0)
			debug("\n");
	}
	debug("\n");
#endif
}

static void print_mbr(struct mbr_disk_part_tbl *mbr_dpt)
{
#ifdef DEBUG
	debug("\n================== MBR ==================\n");
	debug("boot_ind		= 0x%x\n", mbr_dpt->boot_ind);
	debug("start_head	= 0x%x\n", mbr_dpt->start_head);
	debug("start_cylesec	= 0x%x\n", mbr_dpt->start_cylsec);
	debug("part_type	= 0x%x\n", mbr_dpt->part_type);
	debug("end_head		= 0x%x\n", mbr_dpt->end_head);
	debug("end_cylsec	= 0x%x\n", mbr_dpt->end_cylsec);
	debug("rel_sectors	= 0x%x\n", mbr_dpt->rel_sectors);
	debug("total_sectors	= 0x%x\n", mbr_dpt->total_sectors);
	debug("=========================================\n");
#endif
}

static void print_dbr(struct boot_sector *dbr)
{
#ifdef DEBUG
	debug("\n================== DBR ==================\n");
	debug("jmp_code[0]	= 0x%x\n", dbr->jmp_code[0]);

	debug("\n");
	debug("sector_size	= 0x%x\n", dbr->bpb.sector_size);
	debug("root_entries	= 0x%x\n", dbr->bpb.root_entries);
	debug("small_sector	= 0x%x\n", dbr->bpb.small_sectors);
	debug("sectors_p_fat	= 0x%x\n", dbr->bpb.sectors_p_fat);
	debug("=========================================\n");
#endif
}

static uint get_endian_mode(void)
{
	ushort us = 0x1122;
	uchar uc = *(uchar *)&us;
	if (uc == 0x11)
		return BIG_ENDIAN_MODE;
	else
		return LITTLE_ENDIAN_MODE;
}

static void swap_mbr(struct mbr_disk_part_tbl *mbr_dpt)
{
	mbr_dpt->start_cylsec = SWAP16(mbr_dpt->start_cylsec);
	mbr_dpt->end_cylsec = SWAP16(mbr_dpt->end_cylsec);
	mbr_dpt->rel_sectors = SWAP32(mbr_dpt->rel_sectors);
	mbr_dpt->total_sectors = SWAP32(mbr_dpt->total_sectors);
}

static void swap_dbr(struct boot_sector *dbr)
{
	dbr->bpb.sector_size = SWAP16(dbr->bpb.sector_size);
	dbr->bpb.reserved_sectors = SWAP16(dbr->bpb.reserved_sectors);
	dbr->bpb.root_entries = SWAP16(dbr->bpb.root_entries);
	dbr->bpb.small_sectors = SWAP16(dbr->bpb.small_sectors);
	dbr->bpb.sectors_p_fat = SWAP16(dbr->bpb.sectors_p_fat);
	dbr->bpb.sector_p_track = SWAP16(dbr->bpb.sector_p_track);
	dbr->bpb.heads = SWAP16(dbr->bpb.heads);
	dbr->bpb.hidden_sectors = SWAP32(dbr->bpb.hidden_sectors);
	dbr->bpb.large_sectors = SWAP32(dbr->bpb.large_sectors);

	dbr->fat16.ext_bpb.vol = SWAP32(dbr->fat16.ext_bpb.vol);

	dbr->fat32.ext.ext_flag = SWAP16(dbr->fat32.ext.ext_flag);
	dbr->fat32.ext.fs_version = SWAP16(dbr->fat32.ext.fs_version);
	dbr->fat32.ext.root_cluster = SWAP32(dbr->fat32.ext.root_cluster);
	dbr->fat32.ext.fs_info = SWAP16(dbr->fat32.ext.fs_info);
	dbr->fat32.ext.backup_boot_sector =
		SWAP16(dbr->fat32.ext.backup_boot_sector);
}

static inline ushort cylsec_to_sec(ushort cylsec)
{
	return cylsec & 0x3F;
}

static inline ushort cylsec_to_cyl(ushort cylsec)
{
	return ((cylsec & 0xC0) << 2) | ((cylsec & 0xFF00) >> 8);
}

static inline ushort back_to_cylsec(ushort sec, ushort cyl)
{
    return ((sec & 0x3F) || ((cyl & 0x300)>> 2) || ((cyl & 0xFF)<<8));
}

/* NOTE: some formatted card hasn't MBR, only DBR */
static void parse_mbr(char *sd_data, struct mbr_disk_part_tbl **mbr)
{
	ushort cylsec, start_cyl;
	uchar start_sec;
	struct mbr_disk_part_tbl *mbr_dpt;

    mbr_dpt = (struct mbr_disk_part_tbl *)(sd_data + MBR_DPT_OFF); //Partition Table
	if (get_endian_mode() == BIG_ENDIAN_MODE)
		swap_mbr(mbr_dpt);
	print_mbr(mbr_dpt);

	/* Check if it is a MBR */
	cylsec = mbr_dpt->start_cylsec;
	start_sec = cylsec_to_sec(cylsec);
	start_cyl = cylsec_to_cyl(cylsec);

	if (mbr_dpt->part_type == 0 ||
	    (mbr_dpt->boot_ind != 0 && mbr_dpt->boot_ind != 0x80) ||
/*-----------------8/12/2010 10:28AM----------------
 * The start head does not necessary to be the 1st
 * --------------------------------------------------*/
//       mbr_dpt->start_head != 1 ||
//       start_sec != 1 ||
//       start_cyl != 0 ||
	    mbr_dpt->end_head < mbr_dpt->start_head ||
	    mbr_dpt->total_sectors < mbr_dpt->rel_sectors) {
		*mbr = NULL;
		debug("It is not a valid MBR.\n");
	} else {
		*mbr = mbr_dpt;
		debug("It is a valid MBR\n");
	}
}

static int parse_dbr(struct boot_sector *dbr)
{
	print_dbr(dbr);

	if (dbr->jmp_code[0] != 0xEB || dbr->bpb.sector_size != SECTOR_SIZE)
		return 1;
	else
		return 0;
}

static void write_config_file(char *out, int num, struct config_word *pword[])
{
	struct stat sb;
	int h_config, i, n;
	char buf[20];

	stat(out, &sb);
	if ((errno == ENOENT) || S_ISREG(sb.st_mode)) {
		h_config = open(out, O_WRONLY|O_CREAT|O_TRUNC, 0666);
		if (h_config == 0) {
			printf(MSG_OPEN_FILE_FAIL, out);
			return;
		}
		for (i = 0; i < num; i++) {
			n = sprintf(buf, "%02x:%08x\n",
					pword[i]->off, pword[i]->val);
			write(h_config, buf, n);
		}
		close(h_config);
	}
}

/* FIXME: the 1st line of the config file can't be a blank line */
static int parse_config_file(char *config_data, int config_len,
			struct config_word *word[])
{
	int i = 0, newline = 1;
	char *tmp = config_data;

	while (tmp < (config_data + config_len)) {
		if (newline == 1) {
			word[i++] = (struct config_word *)tmp;
			newline = 0;
		}

		if ((*tmp == RET_FLG1) || (*tmp == RET_FLG2)) {
			do {
				*tmp++ = '\0';
			} while ((*tmp == RET_FLG1) || (*tmp == RET_FLG2));

			newline = 1;
		} else
			tmp++;

		/* if overflow, just ignore left data */
		if (i >= BOOT_MAX_CONFIG_WORDS)
			break;
	}

	config_len = i;
	for (i = 0; i < config_len; i++) {
		config_data = (char *)word[i];
		tmp = config_data;
		word[i] = malloc(sizeof(struct config_word));

		while (*tmp++ != DELIMITER) ;
			*(tmp - 1) = '\0';

		word[i]->off = strtoul(config_data, (char **)NULL, 16);
		word[i]->val = strtoul(tmp, (char **)NULL, 16);
	}

	return config_len;
}

/*
 * NOTE: partition can't cross cylinder
 * sectors per cylinder = heads * (sectors per track)
 * though heads can be 256, but because of limits of address registers in ASM,
 * heads offten is 255.
 * e.g. if heads = 255, sectors per track = 63
 * sectors per cylinder = 16065(255 * 63)
 *
 * to 1st partition, which sectors amount equals total sectors + hidden sectors
 *
 * if left space in the 1st partition is enough to save user code, that's
 * perfect, or related items in MBR/DBR tables must be fixed to exclude
 * the space that is used by user code
 * return: 0 -- normal capacity
 *         1 -- high capacity
 *         negtive -- error
 */
static uint adjust_partition_table(struct mbr_disk_part_tbl *mbr, struct boot_sector *pdbr,
		int sectors, int *high_cap)
{
    uint cyl_secs, left_sec, total_sec, hidden_sec, extra, bu;
    ushort start_sec, start_cyl;
    int off, shrink_secs, i;

	cyl_secs = pdbr->bpb.sector_p_track * pdbr->bpb.heads;
	total_sec = pdbr->bpb.small_sectors;
	if (total_sec == 0) /* partitions > 32M */
		total_sec = pdbr->bpb.large_sectors;
	hidden_sec = pdbr->bpb.hidden_sectors;

	/* check whether 1st partition is enough big, maybe not needed */
	if (total_sec < sectors)
		goto bad;

	/*
	 * whether the card is a high capacity card. The judge standard
	 * is the capacity whether or not is more than 2G
     * JZ: For card less than 2GB, the data will pending on the end of the partition
     *     For card > 2GB, need to put at the ahead of the Boot sector
     *     Therefore, the start head, cylinder/sec need to be changed
     *     However, the ending head & cylinder/sec is the same
	 */
    if (total_sec / ( 2 * 1024) > 2048) {  // (2 GB < Partition <= 32 GB)
		*high_cap = 1;
        off = 9; //Set to 9 See Figure 4-2 of File System Spec.
        bu = FAT32_BOUNDARYUNIT;
/*        if (total_sec / ( 2 * 1024) > 32768) {    // (32 GB < Partition < 2 TB)
           *high_cap = 2;
           off = 24; //Set to 24 See Figure 5-2 of File System Spec.
           bu = EXFAT_BOUNDARYUNIT;
        }
 */
        sectors += off;
        if (hidden_sec < sectors) {
            for ( i = 1; i< 32; i++) {
                if (sectors < i*bu)   //Need to keep BU
                   break;
            }
            if (i == 32) //boot image is too large. The limit size is 128 MB ( 1 byte)
                 goto bad;
            hidden_sec = i*bu;
            extra = hidden_sec - mbr->rel_sectors;
            debug("new/old hidden_sec %d/%d, difference %d, rel_sectors %d",
                   hidden_sec, pdbr->bpb.hidden_sectors, extra, mbr->rel_sectors );
            start_sec = (hidden_sec % pdbr->bpb.sector_p_track ) + 1;
            start_cyl = hidden_sec / cyl_secs;
            mbr->start_head = (uchar) (hidden_sec % cyl_secs)/ pdbr->bpb.sector_p_track;
            mbr->start_cylsec = back_to_cylsec (start_sec, start_cyl);
            mbr->rel_sectors = hidden_sec;
            mbr->total_sectors = total_sec - extra;
            debug("start_secotr %d, start_cyl %d, cylsec %x, start_head %d",
                   start_sec, start_cyl, mbr->start_cylsec, mbr->start_head);

            pdbr->bpb.hidden_sectors = hidden_sec;

        }

    }
    else {              //partition <= 2 GB
        *high_cap = 0;

        /* check left sectors whether to save user code */
        left_sec = (total_sec + hidden_sec) % cyl_secs;
        off = total_sec + hidden_sec - left_sec;
        shrink_secs = sectors - left_sec;

        /*
         * if shrink_secs <= 0, which means left space is enough large to
         * save the user code + reserved space
         */
        if (shrink_secs <= 0)
            goto end;

        /* don't worry about the problem that crosses the cylinder */
        total_sec -= shrink_secs;
        off -= shrink_secs;

        if (pdbr->bpb.small_sectors == 0)
            pdbr->bpb.large_sectors = total_sec;
        else
            pdbr->bpb.small_sectors = total_sec;
    }

    if (mbr != NULL)
        mbr->total_sectors = total_sec;

end:
	return off;
bad:
	return (uint)-1;
}

int main(int argc, char *argv[])
{
	int h_dev, h_spi_dev = 0, h_sd_dev = 0, h_config = 0, h_usercode = 0;
	char *cfg_data_buf = NULL, *usercode_buf = NULL;
	char *mbr_buf = NULL, *boot_sect_buf = NULL;
	char p_configname[256], p_usercodename[256], p_devname[256];
	struct mbr_disk_part_tbl *mbr_dpt = NULL;
	struct boot_sector *boot_sector = NULL;
	struct config_word *pconfig_word[BOOT_MAX_CONFIG_WORDS];
	int exitcode = 0, h_cap, code_addr = 0, work_mode = 0;
	uint i, n, config_num = 0, len, sec_user, rev_space = BOOT_REV_SPACE;
	uchar *ptr = NULL;
	struct stat sb;
	int opt = 0;
	uint endian_mode;
	uint rel_sectors = 0;
	struct option longopts[] = {
		{"sd", required_argument, NULL, 'd'},
		{"spi", required_argument, NULL, 'p'},
		{"o", required_argument, NULL, 'o'},
		{"r", required_argument, NULL, 'r'},
		{0, 0, 0, 0},
	};

	memset(pconfig_word, 0, sizeof(struct config_word *) *
			BOOT_MAX_CONFIG_WORDS);
	if (argc < 5) {
		printf("Usage: %s config_file image -sd dev [-o out_config]"
			" | -spi spiimage", argv[0]);
#if CONFIG_REV_SPACE
		printf(" [-r size]");
#endif
		printf("\n");
		printf("\n\tconfig_file : includes boot signature and config words");
		printf("\n\timage       : the U-Boot image for booting from eSDHC/eSPI");
		printf("\n\tdev         : SDCard's device node(e.g. /dev/sdb, /dev/mmcblk0)");
		printf("\n\tspiimage    : boot image for SPI mode");
		printf("\n\tout_config  : modified config file for SD mode");
#if CONFIG_REV_SPACE
		printf("\n\tsize        : reserved space (default 0K).");
#endif
		printf("\n");
		exitcode = -EINVAL;
		goto end;
	}

	/* Endian mode */
	endian_mode = get_endian_mode();
	debug("It is a %s endian machine.\n",
			endian_mode == BIG_ENDIAN_MODE ? "big" : "little");

	memset(p_devname, 0, sizeof(p_devname));
	strncpy(p_configname, argv[1], 255);
	strncpy(p_usercodename, argv[2], 255);
	while((n = getopt_long_only(argc, argv, "", longopts, NULL)) != -1) {
		/*
		 * if "sd" and "spi" flags are used together,
		 * work_mode must be wrong !
         * JZ: added print information for more than 1 option
		 */
        if (work_mode != 0)
        {
            printf("\n Only one option is allowed. work_mode: %d \n", work_mode);
            goto end;
        }

		switch (n) {
		case 'd':
            work_mode |= BOOT_WORK_MODE_SD;
			strncpy(p_devname, optarg, 255);
			break;
		case 'p':
            work_mode |= BOOT_WORK_MODE_SPI;
			strncpy(p_devname, optarg, 255);
			break;
		}
	}

	memset(&sb, 0, sizeof(struct stat));
	stat(p_devname, &sb);
	if (((work_mode == BOOT_WORK_MODE_SD) && !S_ISBLK(sb.st_mode)) ||
	    ((work_mode == BOOT_WORK_MODE_SPI) &&
			((errno != ENOENT) && !S_ISREG(sb.st_mode)))) {
		exitcode = -EINVAL;
		goto end;
	}

	if (work_mode == BOOT_WORK_MODE_SD) {
		/* Open SD device */
		h_sd_dev = open(p_devname, O_RDWR, 0666);
		if (h_sd_dev == 0) {
			printf(MSG_OPEN_FILE_FAIL, p_devname);
			exitcode = -EIO;
			goto end;
		}

		/* Read first sector from sd */
		mbr_buf = (char*)malloc(SEC_TO_BYTE(1));
		if (mbr_buf == NULL) {
			exitcode = -ENOMEM;
			goto end;
		}

		if (read(h_sd_dev, mbr_buf, SEC_TO_BYTE(1))
				!= SEC_TO_BYTE(1)) {
			printf(MSG_READ_FILE_FAIL, p_devname);
			exitcode = -EIO;
			goto end;
		}
		debug("Read MBR from SDCard:\n");
		print_buf(mbr_buf, SEC_TO_BYTE(1), 1);

		parse_mbr(mbr_buf, &mbr_dpt);
		rel_sectors = mbr_dpt->rel_sectors;

		/* Some formatted card hasn't MBR, only DBR */
		if (mbr_dpt == NULL) {
			boot_sect_buf = mbr_buf;
			mbr_buf = NULL;
			boot_sector = (struct boot_sector *)boot_sect_buf;
		} else {
			lseek(h_sd_dev, mbr_dpt->rel_sectors * SECTOR_SIZE,
					SEEK_SET);
			boot_sect_buf = (char*)malloc(SEC_TO_BYTE(1));
			if (boot_sect_buf == NULL) {
				exitcode = -ENOMEM;
				goto end;
			}
			if (read(h_sd_dev, boot_sect_buf, SEC_TO_BYTE(1)) !=
					SEC_TO_BYTE(1)) {
				printf(MSG_READ_FILE_FAIL, p_devname);
				exitcode = -EIO;
				goto end;
			}
			debug("Read DBR from SDCard:\n");
			print_buf(boot_sect_buf, SEC_TO_BYTE(1), 1);

			boot_sector = (struct boot_sector *)boot_sect_buf;
			if (endian_mode == BIG_ENDIAN_MODE)
				swap_dbr(boot_sector);
		}

		/* Parse partition. NOTE: mbr_dpt maybe always be NULL */
		if (parse_dbr(boot_sector) != 0) {
			printf(MSG_SIGNATURE_FAIL);
			exitcode = -EINVAL;
			goto end;
		}
	} else {
		h_spi_dev = open(p_devname, O_WRONLY|O_CREAT|O_TRUNC, 0666);
		if (h_spi_dev == 0) {
			printf(MSG_OPEN_FILE_FAIL, p_devname);
			exitcode = -EIO;
			goto end;
		}
	}

	/* Open config file */
	h_config = open(p_configname, O_RDONLY);
	if (h_config == 0) {
		printf(MSG_OPEN_FILE_FAIL, p_configname);
		exitcode = -EIO;
		goto end;
	}

	len = lseek(h_config, 0, SEEK_END);
	lseek(h_config, 0, SEEK_SET);
	cfg_data_buf = malloc(len);
	if (cfg_data_buf == NULL) {
		exitcode = -ENOMEM;
		goto end;
	}

	if (read(h_config, cfg_data_buf, len) != len) {
		printf(MSG_READ_FILE_FAIL, p_configname);
		exitcode = -EIO;
		goto end;
	}

	/* Parse config file */
	config_num = parse_config_file(cfg_data_buf, len, pconfig_word);

#if CONFIG_REV_SPACE
	optind = 0;
	while((opt = getopt_long_only(argc, argv, "", longopts, NULL)) != -1) {
		if (opt == 'r')
			rev_space = strtoul(optarg, (char**)NULL, 10) * 1024;
	}
#endif

	/* Open user code file */
	h_usercode = open(p_usercodename, O_RDONLY);
	if (h_usercode == 0) {
		printf(MSG_OPEN_FILE_FAIL, p_usercodename);
		exitcode = -EIO;
		goto end;
	}

	n = lseek(h_usercode, 0, SEEK_END);
	lseek(h_usercode, 0, SEEK_SET);
	len = (n + SECTOR_SIZE - 1) & ~(SECTOR_SIZE - 1);
	usercode_buf = malloc(len);
	if (usercode_buf == NULL) {
		exitcode = -ENOMEM;
		goto end;
	}
	if (read(h_usercode, usercode_buf, n) != n) {
		printf(MSG_READ_FILE_FAIL, p_usercodename);
		exitcode = -EIO;
		goto end;
	}

	/* User code + Reserved space */
	len += rev_space;
	sec_user = len / SECTOR_SIZE;
	for (i = 0; i < config_num; i++) {
		if (pconfig_word[i]->off == BOOT_IMAGE_LEN_OFF) {
			/* The length must align sector size */
			pconfig_word[i]->val = len;
			break;
		}
	}

	if (work_mode == BOOT_WORK_MODE_SD) {
		/* FIXME: don't support disk clean up */
		n = adjust_partition_table(mbr_dpt, boot_sector,
				sec_user, &h_cap);
        rel_sectors = mbr_dpt->rel_sectors;
    } else {
		h_cap = 0;
		n = 2; /* Start address of user code in SPI mode,  */
	}

	if (n == (uint)-1) {
		printf(MSG_PARTLENGTH_FAIL);
		exitcode = -EINVAL;
		goto end;
	}

    /* Change config word[0x50] to final user code position
     * JZ: Source address should be always byte mode*/
	for (i = 0; i < config_num; i++) {
		if (pconfig_word[i]->off == BOOT_IMAGE_ADDR_OFF) {
			code_addr = i;
            pconfig_word[i]->val = n * SECTOR_SIZE;
			break;
		}
	}

	/* Only in SD mode config file is supported to output */
	optind = 0;
	if (work_mode == BOOT_WORK_MODE_SD) {
		while((opt = getopt_long_only(argc, argv, "", longopts, NULL))
			!= -1) {
			if (opt == 'o')
				write_config_file(optarg, config_num,
						pconfig_word);
		}
	}

	/*
	 * Write back all data to sd card/spi image.
	 * NOTE: sequence is important, user code must be first because of
         * config word byte swap.
         */
	if (work_mode == BOOT_WORK_MODE_SD)
		h_dev = h_sd_dev;
	else
		h_dev = h_spi_dev;

	/* Write user code to sd card */
	debug("\nWriting image to %s...",
		(work_mode == BOOT_WORK_MODE_SD) ? "SDCard" : "SPI image");
	if (h_cap)
		/*
		 * For high-capacity card, the byte offset maybe beyond
		 * 0xffffffff, so it has to move step by step
         * JZ: The source address is byte mode now.
		 */
        lseek64(h_dev, (off64_t)pconfig_word[code_addr]->val,
				SEEK_SET);
	else
		lseek(h_dev, pconfig_word[code_addr]->val, SEEK_SET);

	len = SEC_TO_BYTE(sec_user) - rev_space;
	ptr = (uchar *)usercode_buf;
	while (len > 0) {
		n = write(h_dev, ptr, len);
		if (n < 0) {
			printf(MSG_WRITE_FILE_FAIL, p_devname);
			exitcode = -EIO;
			goto end;
		}
		ptr += n;
		len -= n;
	}
	debug("OK.\n");

	if ((work_mode == BOOT_WORK_MODE_SPI)&& (rev_space != 0)) {
		/* Extend file size to accomodate reserved space */
		if (rev_space) {
			lseek(h_dev, rev_space - 1, SEEK_CUR);
			write(h_dev, &n, 1);
		}
	}

	lseek(h_dev, 0, SEEK_SET);
	if (work_mode == BOOT_WORK_MODE_SD) {
		/* Write config words to sd card in proper byte endian */
		debug("\nWriting config words to MBR's buffer first...");
		ptr = (uchar *)mbr_buf;
		if (ptr == NULL)
			ptr = (uchar *)boot_sect_buf;

		for (i = 0; i < config_num; i++) {
			n = pconfig_word[i]->val;
			/* Config words are provided with big endian mode */
			if (endian_mode == LITTLE_ENDIAN_MODE)
				n = SWAP32(n);
			*(int *)(ptr + pconfig_word[i]->off) = n;
		}
		debug("OK.\n");

		/* Write MBR */
		debug("\nWriting MBR to SDCard...");
		len = SEC_TO_BYTE(1);
		if (mbr_buf) {
			ptr = (uchar *)(mbr_buf + MBR_DPT_OFF);
			if (endian_mode == BIG_ENDIAN_MODE)
				swap_mbr((struct mbr_disk_part_tbl *)ptr);
			print_mbr((struct mbr_disk_part_tbl *)ptr);

			ptr = (uchar *)mbr_buf;
			print_buf(ptr, SEC_TO_BYTE(1), 1);
			while (len > 0) {
				n = write(h_dev, ptr, len);
				if (n < 0) {
					printf(MSG_WRITE_FILE_FAIL, p_devname);
					exitcode = -EIO;
				}
				ptr += n;
				len -= n;
			}
			lseek(h_dev, rel_sectors * SECTOR_SIZE,	SEEK_SET);
		}
		debug("OK.\n");

		/* Write DBR */
		debug("\nWriting DBR to SDCard...");
		len = SEC_TO_BYTE(1);
		if (boot_sect_buf) {
			if (endian_mode == BIG_ENDIAN_MODE)
				swap_dbr((struct boot_sector *)boot_sect_buf);
			print_dbr((struct boot_sector *)boot_sect_buf);

			ptr = (uchar *)boot_sect_buf;
			print_buf(ptr, SEC_TO_BYTE(1), 1);
			while (len > 0) {
				n = write(h_dev, ptr, len);
				if (n < 0) {
					printf(MSG_WRITE_FILE_FAIL, p_devname);
					exitcode = -EIO;
				}
				ptr += n;
				len -= n;
			}
		}
		debug("OK.\n");
	} else {
		uchar sector[SECTOR_SIZE];

		memset(sector, 0, SECTOR_SIZE);
		for (i = 0; i < config_num; i++) {
			n = pconfig_word[i]->val;
			/* Config words are provided with big endian mode */
			if (endian_mode == LITTLE_ENDIAN_MODE)
				n = SWAP32(n);
			*(int *)(sector + pconfig_word[i]->off) = n;
		}

		debug("\nWriting config words to SPI image...");
		len = SEC_TO_BYTE(1);
		while (len > 0) {
			ptr = (uchar *)sector;
			n = write(h_dev, ptr, len);
			if (n < 0) {
				printf(MSG_WRITE_FILE_FAIL, p_devname);
				exitcode = -EIO;
			}
			ptr += n;
			len -= n;
		}
		debug("OK.\n");
	}
	sync();

end:
	for (i = 0; i < config_num; i++)
		free(pconfig_word[i]);
	free(boot_sect_buf);
	free(mbr_buf);
	free(cfg_data_buf);
	free(usercode_buf);
	if (h_usercode != 0)
		close(h_usercode);
	if (h_config != 0)
		close(h_config);
	if (h_sd_dev != 0)
		close(h_sd_dev);
	if (h_spi_dev != 0)
		close(h_spi_dev);

	if (exitcode == 0)
		printf("Congratulations! It is done successfully.\n");

	return exitcode;
}
