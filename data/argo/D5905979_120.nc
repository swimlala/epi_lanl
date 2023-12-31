CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2020-06-19T17:09:22Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         ZPRIMARY | https://orcid.org/0000-0001-7324-3159 | Matthew Alkire, University of Washington        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    7   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7(   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7,   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    70   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7@   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7P   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7`   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7h   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        8   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    8   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    8    DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     8$   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8D   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8H   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8L   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8l   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8�   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8�   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           8�   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            8�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    K�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  U�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ]�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    g�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  i�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  q�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    y�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �T   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �d   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �h   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �x   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �|   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20200619170922  20220204114423  5905979 UW, Argo                                                        STEPHEN RISER                                                   PRES            TEMP            PSAL               xA   AO  7662                            2C  D   APEX                            8312                            080318                          846 @���B1   @����5@6n��P�c��R1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    xA   B   B   @�ff@�  A   A!��A@  A^ffA~ffA�33A�  A�  A�  A�  A�  A�  B ffB  B  B  B   B(  B0  B8ffB?��BG��BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�33B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C�C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+y�D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6fD6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DKfDK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� DbfDb� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�fDy�\D�{D�VfD��HD���D�#3D�QHD��D��=D�"�D�O\D���D��D�RD�T)D�v�D���D�=D�M�D�D�ɚ111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @���@�33@�33A34A=��A\  A|  A�  A���A���A���A���A���A���A���BffBffBffBffB'ffB/ffB7��B?  BG  BOffBWffB_ffBgffBoffBwffBffB��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B�� Bó3Bǳ3B˳3Bϳ3Bӳ3B׳3B۳3B߳3B��fB��fB��fB�3B�3B��3B��3B��3CٚCٚCٚCٚC	ٚCٚCٚC�4CٚCٚCٚCٚCٚCٚCٚCٚC!ٚC#ٚC%ٚC'ٚC)ٚC+ٚC-ٚC/ٚC1ٚC3ٚC5ٚC7ٚC9ٚC;ٚC=ٚC?ٚCAٚCCٚCEٚCGٚCIٚCKٚCMٚCOٚCQٚCSٚCUٚCWٚCYٚC[ٚC]ٚC_ٚCaٚCcٚCeٚCgٚCiٚCkٚCmٚCoٚCqٚCsٚCuٚCwٚCyٚC{ٚC}ٚCٚC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�� C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�� C���D vfD �fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fD	vfD	�fD
vfD
�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fD vfD �fD!vfD!�fD"vfD"�fD#vfD#�fD$vfD$�fD%vfD%�fD&vfD&�fD'vfD'�fD(vfD(�fD)vfD)�fD*vfD*�fD+p D+�fD,vfD,�fD-vfD-�fD.vfD.�fD/vfD/�fD0vfD0�fD1vfD1�fD2vfD2�fD3vfD3�fD4vfD4�fD5vfD5��D6vfD6�fD7vfD7�fD8vfD8�fD9vfD9�fD:vfD:�fD;vfD;�fD<vfD<�fD=vfD=�fD>vfD>�fD?vfD?�fD@vfD@�fDAvfDA�fDBvfDB�fDCvfDC�fDDvfDD�fDEvfDE�fDFvfDF�fDGvfDG�fDHvfDH�fDIvfDI�fDJvfDJ��DKvfDK�fDLvfDL�fDMvfDM�fDNvfDN�fDOvfDO�fDPvfDP�fDQvfDQ�fDRvfDR�fDSvfDS�fDTvfDT�fDUvfDU�fDVvfDV�fDWvfDW�fDXvfDX�fDYvfDY�fDZvfDZ�fD[vfD[�fD\vfD\�fD]vfD]�fD^vfD^�fD_vfD_�fD`vfD`�fDavfDa��DbvfDb�fDcvfDc�fDdvfDd�fDevfDe�fDfvfDf�fDgvfDg�fDhvfDh�fDivfDi�fDjvfDj�fDkvfDk�fDlvfDl�fDmvfDm�fDnvfDn�fDovfDo�fDpvfDp�fDqvfDq�fDrvfDr�fDsvfDs�fDtvfDt��Dy��D��D�Q�D��{D��D�fD�L{D��RD��pD��D�J�D�� D���D��D�O\D�q�D��
D�pD�H�D��D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��Aѥ�Aѧ�Aѥ�Aѥ�Aѣ�Aѡ�Aѩ�Aѥ�Aѥ�Aѧ�AѴ9AѰ!Aѩ�Aѩ�AѬAѩ�Aѩ�AѬAѮAѰ!AѰ!AѰ!AѰ!AѺ^AѶFAѴ9AѲ-AѼjAѾwA���A���A�A�A�ĜA�ȴA�ƨA�ƨA�ƨA�ƨA�ĜA�ĜA�ĜA�AѼjAѓuAЋDA�ĜA�^5A�ZAüjA��^A���A�-A���A���A�ffA�+A��-A���A�;dA���A�\)A���A��HA��7A�G�A���A�ĜA��wA�A���A�r�A���A��A�$�A��A��A���A�jA�7LA�&�A���A�p�A��-A�?}A���A��
A��A�r�A�dZA�S�A��hA�K�A�&�A�r�A�A���A�A��yA��A���A�ZA�/A��7A��A���A�{A��wA�
=A���A�ƨA��yA��A�O�A}+Az1Ax  At�`Ar  AnȴAjM�Ahr�Af�+Ad��Ab��Aa��A^�RAZVAXz�AWAV��AU+ASVAQ��APĜAM�wAL1AJ  AH�jAF�`AEhsADJAB�\AAA@5?A=��A<�\A;�hA;oA9��A9�A8ȴA7|�A5%A3��A3|�A3VA1��A0�A/x�A.�`A. �A-C�A+��A*M�A)O�A(z�A'�-A&�/A&5?A%�wA%�A%��A%|�A%A$Q�A#�hA"��A!%A�A?}A9XA^5A%A��A��A�jA��A�hA\)Az�A�
AG�A�RA�TA�A�A�A��AO�A�AffA1'A�-A�A
�A	A�+A$�AA�RAM�A��A�TA+A��A�DA��AXA �yA E�@���@��T@��w@�hs@���@��@���@��9@�ƨ@���@�A�@�"�@�J@�r�@�t�@�@�V@�%@�ƨ@�M�@�1@���@�K�@܋D@�`B@�o@��/@��;@�;d@Ѳ-@мj@��m@��H@̬@�b@��#@ȣ�@�Z@�hs@�O�@��
@�v�@�o@��@�9X@���@�M�@���@��
@��@��@���@��#@�?}@��h@���@��T@�ȴ@�Ĝ@�n�@�7L@��F@�"�@�v�@��@���@��9@���@���@���@�V@��@��j@�Q�@�I�@�  @�t�@�C�@���@�{@���@�?}@� �@�  @��@���@���@���@�$�@���@��-@�Ĝ@�l�@���@���@��@���@�&�@��@��D@�A�@��F@�C�@���@�=q@���@�Ĝ@�j@��w@���@�t�@�;d@��@���@�@���@�G�@���@���@��@�  @��
@���@�@��@�r�@��w@��P@��@���@��-@�G�@��@��^@��^@�7L@���@���@��/@��j@�r�@��@��;@���@�K�@�C�@�C�@�o@��@��R@���@�v�@�-@��@�@�@���@�G�@�?}@�&�@���@���@���@���@��j@���@��@�I�@�b@��;@���@��w@���@�@���@���@��R@��\@�M�@�5?@�$�@�{@��#@��#@��^@��h@�X@��@�V@���@���@���@���@��`@�Ĝ@��@�r�@�9X@��
@�|�@�S�@�33@�+@�
=@���@���@�n�@�-@��@���@��-@���@��7@��@�hs@�G�@���@�A�@�1@���@���@���@��;@�ƨ@���@�C�@��H@���@��+@�~�@�v�@�V@�5?@�5?@�-@�$�@�$�@�J@��@��T@���@���@�hs@�G�@��@�%@���@���@���@���@��D@�z�@�bN@� �@��w@���@�+@��@�o@��y@��R@���@�~�@�ff@�E�@�{@��^@��h@1�@s�]@lی@e�.@^��@YO�@Q��@K�$@G\)@A}�@:6�@5��@.3�@(��@#o�@u@ѷ@�M@L�@f�@	�)111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  Aѥ�Aѧ�Aѥ�Aѥ�Aѣ�Aѡ�Aѩ�Aѥ�Aѥ�Aѧ�AѴ9AѰ!Aѩ�Aѩ�AѬAѩ�Aѩ�AѬAѮAѰ!AѰ!AѰ!AѰ!AѺ^AѶFAѴ9AѲ-AѼjAѾwA���A���A�A�A�ĜA�ȴA�ƨA�ƨA�ƨA�ƨA�ĜA�ĜA�ĜA�AѼjAѓuAЋDA�ĜA�^5A�ZAüjA��^A���A�-A���A���A�ffA�+A��-A���A�;dA���A�\)A���A��HA��7A�G�A���A�ĜA��wA�A���A�r�A���A��A�$�A��A��A���A�jA�7LA�&�A���A�p�A��-A�?}A���A��
A��A�r�A�dZA�S�A��hA�K�A�&�A�r�A�A���A�A��yA��A���A�ZA�/A��7A��A���A�{A��wA�
=A���A�ƨA��yA��A�O�A}+Az1Ax  At�`Ar  AnȴAjM�Ahr�Af�+Ad��Ab��Aa��A^�RAZVAXz�AWAV��AU+ASVAQ��APĜAM�wAL1AJ  AH�jAF�`AEhsADJAB�\AAA@5?A=��A<�\A;�hA;oA9��A9�A8ȴA7|�A5%A3��A3|�A3VA1��A0�A/x�A.�`A. �A-C�A+��A*M�A)O�A(z�A'�-A&�/A&5?A%�wA%�A%��A%|�A%A$Q�A#�hA"��A!%A�A?}A9XA^5A%A��A��A�jA��A�hA\)Az�A�
AG�A�RA�TA�A�A�A��AO�A�AffA1'A�-A�A
�A	A�+A$�AA�RAM�A��A�TA+A��A�DA��AXA �yA E�@���@��T@��w@�hs@���@��@���@��9@�ƨ@���@�A�@�"�@�J@�r�@�t�@�@�V@�%@�ƨ@�M�@�1@���@�K�@܋D@�`B@�o@��/@��;@�;d@Ѳ-@мj@��m@��H@̬@�b@��#@ȣ�@�Z@�hs@�O�@��
@�v�@�o@��@�9X@���@�M�@���@��
@��@��@���@��#@�?}@��h@���@��T@�ȴ@�Ĝ@�n�@�7L@��F@�"�@�v�@��@���@��9@���@���@���@�V@��@��j@�Q�@�I�@�  @�t�@�C�@���@�{@���@�?}@� �@�  @��@���@���@���@�$�@���@��-@�Ĝ@�l�@���@���@��@���@�&�@��@��D@�A�@��F@�C�@���@�=q@���@�Ĝ@�j@��w@���@�t�@�;d@��@���@�@���@�G�@���@���@��@�  @��
@���@�@��@�r�@��w@��P@��@���@��-@�G�@��@��^@��^@�7L@���@���@��/@��j@�r�@��@��;@���@�K�@�C�@�C�@�o@��@��R@���@�v�@�-@��@�@�@���@�G�@�?}@�&�@���@���@���@���@��j@���@��@�I�@�b@��;@���@��w@���@�@���@���@��R@��\@�M�@�5?@�$�@�{@��#@��#@��^@��h@�X@��@�V@���@���@���@���@��`@�Ĝ@��@�r�@�9X@��
@�|�@�S�@�33@�+@�
=@���@���@�n�@�-@��@���@��-@���@��7@��@�hs@�G�@���@�A�@�1@���@���@���@��;@�ƨ@���@�C�@��H@���@��+@�~�@�v�@�V@�5?@�5?@�-@�$�@�$�@�J@��@��T@���@���@�hs@�G�@��@�%@���@���@���@���@��D@�z�@�bN@� �@��w@���@�+@��@�o@��y@��R@���@�~�@�ff@�E�@�{@��^G�O�@1�@s�]@lی@e�.@^��@YO�@Q��@K�$@G\)@A}�@:6�@5��@.3�@(��@#o�@u@ѷ@�M@L�@f�@	�)111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
��B
��B
B
B
B
B
��B
B
B
��B
��B
��B
B
��B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
��B
��B
��B
��B
��B
B
B
B
B
ÖB
ĜB
�)B+B �B�B
��B
��B%BuB�B(�B49B9XBB�BF�BO�Bv�B�oBB��B�B�sBB$�B,B8RBF�BP�BB�B0!B1'B:^B6FB(�B�B�BB	7B1'B?}B-B'�B�B{BB��B�B�fB�HB�/B��BȴB�XB��B�PB�+By�Bn�BjBaHBH�B/B�B
��B
��B
�B
�B
s�B
dZB
M�B
0!B
�B
B	�yB	��B	�dB	�{B	�1B	z�B	o�B	dZB	[#B	J�B	33B	&�B	$�B	&�B	!�B	�B	�B	�B	
=B	B��B��B�B�ZB�#B��BǮB�qB�B��B��B��B��B��B��B��B�oB�bB�VB�PB�VB�JB�DB�=B�1B�%B�B�B�B�B�B�B�B�B�B�B�B�B�B� B|�B}�B}�B� B� B�B�B|�B{�B�B�B�B�B�%B�%B�B�B�B�B�B�B�B|�By�Bx�Bw�Bx�Bx�Bw�Br�Bq�Bq�Bt�Bu�Bu�Bz�Bx�Bw�Bv�Bw�Bv�Bt�Bt�Bs�Bs�Br�Bv�Bs�Br�Bq�Bs�Br�Br�Bs�Bs�Bs�Bp�Bu�Bu�Bw�Bw�Bv�Bs�Bp�BhsB`BBaHBYBR�BQ�BR�BR�BQ�BP�BO�BN�BO�BO�BN�BO�BM�BS�B^5BjBiyBk�Br�Bq�Bl�BiyBl�Bn�Bk�Bn�Bw�B~�B{�B{�B�B�B|�B�B� Bz�By�B}�B�B�B�B�B�B�7B�\B�oB�{B��B��B��B��B��B��B��B�B�B�B�!B�'B�?B�RB�RB�XB�jBBǮBǮB��B��B��B��B�B�B�/B�BB�`B�sB�B�B�B�B�B�B��B��B��B��B��B��B��B��B	B	B	B	JB	oB	uB	�B	�B	"�B	%�B	&�B	$�B	$�B	$�B	%�B	)�B	+B	.B	5?B	5?B	5?B	6FB	8RB	=qB	?}B	?}B	A�B	B�B	D�B	H�B	N�B	Q�B	VB	ZB	[#B	]/B	`BB	cTB	ffB	jB	l�B	o�B	r�B	t�B	v�B	v�B	z�B	z�B	{�B	}�B	� B	�B	�B	�+B	�7B	�=B	�DB	�PB	�bB	�oB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�'B	�-B	�-B	�-B	�9B	�?B	�LB	�^B	�dB	�jB	�qB	�wB	�}B	�}B	��B	��B	ÖB	ŢB	ƨB	ǮB	ǮB	ȴB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�#B	�)B	�)B	�)B	�/B	�/B	�5B	�;B	�BB	�TB	�TB	�TB	�ZB	�`B	�`B	�fB	�fB	�mB	�mB	�yB	�yB	�5B
  B
�B
&B
#TB
.�B
2�B
8�B
=�B
B�B
G�B
JXB
P�B
W?B
[qB
`BB
e`B
h�B
m]B
r�B
u�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�rBEB
B�B
�B
�%B
�nB�B�B<B(~B-�B6�B:�BD#Bk	B��B��B�B�=BܫB�TBB ;B,�B:�BEB6�B$TB%[B.�B*zB+B�B
�B�XB�pB%]B3�B!DB'B�B�B�ZB�B��BڥBՇB�oB�>B��B��B�;B��B{tBn%Bb�B^�BU�B=B#mB	�B
�B
�;B
�mB
ujB
hB
X�B
B<B
$�B

�B	�}B	��B	�UB	��B	��B	|�B	o`B	dB	X�B	O�B	?FB	'�B	rB	fB	rB	UB	+B	1B	B��B��B�vB�XB�.B��B϶B�sB�CB�B��B��B�dB�dB�FB�4B�!B�B�
B��B��B��B��B��B�B~�B|�Bz�Bw�Bw�Bu�Bu�Bv�Bu�Bv�Bu�Bu�Bu�Bu�Bv�Bv�Bt�Bq�Br�Br�Bt�Bt�Bx�Bw�Bq�Bp�Bu�Bx�Bw�Bw�Bz�Bz�By�By�Bx�Bu�Bu�Bu�Bu�Bq�BnBmyBlsBmyBmyBltBgUBfPBfPBibBjiBjiBo�Bm{BluBkoBluBkoBicBicBh]Bh]BgWBkpBh^BgXBfRBh^BgXBgXBh^Bh^Bh^BeMBjlBjlBlxBlxBkrBh_BeNB]BT�BU�BM�BG�BF�BG�BG�BF�BE�BD�BC�BD�BD�BC�BD�BB�BH�BR�B_,B^&B`2Bg\BfWBa8B^'Ba8BcEB`3BcEBl|Bs�Bp�Bp�Bw�Bv�Bq�By�Bt�Bo�Bn�Br�Bu�Bu�Bu�Bv�By�B}�B�B�B�'B�EB�]B�iB�pB�|B��B��B��B��B��B��B��B��B��B��B�B�B�8B�VB�VB�oBōBȟBɥB̷B��B��B��B�B�B�%B�%B�7B�7B�IB�[B�aB�tB�nB�tB�tB�zB�B�B��B��B��B	 �B	B	B	*B	NB	rB	�B	�B	~B	~B	B	�B	�B	�B	"�B	)�B	)�B	)�B	*�B	,�B	2B	4B	4B	6(B	7.B	9;B	=SB	CwB	F�B	J�B	N�B	O�B	Q�B	T�B	W�B	[B	_B	a'B	d:B	gKB	iWB	kdB	kdB	o|B	o|B	p�B	r�B	t�B	v�B	x�B	{�B	}�B	~�B	�B	��B	��B	�B	�B	�B	�%B	�+B	�+B	�2B	�>B	�PB	�PB	�]B	�]B	�cB	�cB	�cB	�hB	�hB	�hB	�hB	�nB	�zB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B	�B	�B	�B	�B	�B	�B	�+B	�7B	�=B	�CB	�CB	�IB	�OB	�OB	�UB	�[B	�gB	�mB	�sB	�sB	�yB	�yB	ƀB	ǆB	ǆB	ǆB	ȌB	ɒB	ʘB	ʘB	ʘB	̤B	̤B	ͪB	ΰB	ΰB	ΰB	϶B	мB	мB	мB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�G�O�B	��B	��B
 "B
�B
�B
#&B
'>B
-/B
2B
78B
<�B
>�B
EXB
K�B
O�B
T�B
Y�B
]B
a�B
g"B
j111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.15 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9997(+/-0.0001), vertically averaged dS =-0.011(+/-0.004) in PSS-78.                                                                                                                                                                                       Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                      202202041144232022020411442320220204114423  AO  ARCAADJP                                                                    20200619170922    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20200619170922  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20200619170922  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20220204114423  IP                  G�O�G�O�G�O�                