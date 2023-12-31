CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2020-06-19T17:09:06Z creation      
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20200619170906  20220204114416  5905979 UW, Argo                                                        STEPHEN RISER                                                   PRES            TEMP            PSAL               6A   AO  7662                            2C  D   APEX                            8312                            080318                          846 @ء��ٿ�1   @ء�fftp@5��S����c�O�;dZ1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    6A   B   B   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BHffBP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CA�fCD  CF�CH  CJ  CL  CN  CP  CR�CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Co�fCr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt� Dy��D��\D�/\D�xRD��\D�{D�a�D���D��\D��D�b=D���D��
D��D�@�DژRD��D�-�D�_�D�qD��H111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�  @�33@�33A��A=��A]��A}��A���A���A���A���A���A���A���A���BffBffBffBffB'ffB/ffB7ffB?ffBG��BOffBWffB_ffBgffBoffBwffBffB��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3Bó3Bǳ3B˳3Bϳ3BӀ B׳3B۳3B߳3B�3B�3B�3B�3B�3B��3B��3B��3CٚCٚCٚCٚC	ٚCٚCٚCٚCٚCٚCٚCٚCٚCٚCٚCٚC!ٚC#ٚC%ٚC'ٚC)ٚC+ٚC-ٚC/ٚC1ٚC3ٚC5ٚC7ٚC9ٚC;ٚC=ٚC?ٚCA� CCٚCE�4CGٚCIٚCKٚCMٚCOٚCQ�4CSٚCUٚCWٚCYٚC[ٚC]ٚC_ٚCaٚCcٚCeٚCgٚCiٚCkٚCmٚCo� CqٚCsٚCuٚCwٚCyٚC{ٚC}ٚCٚC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�� C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�� C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D vfD �fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fD	vfD	�fD
vfD
�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fD vfD �fD!vfD!�fD"vfD"�fD#vfD#�fD$vfD$�fD%vfD%�fD&vfD&�fD'vfD'�fD(vfD(�fD)vfD)�fD*vfD*�fD+vfD+�fD,vfD,�fD-vfD-�fD.vfD.�fD/vfD/�fD0vfD0�fD1vfD1�fD2vfD2�fD3vfD3�fD4vfD4�fD5vfD5�fD6vfD6�fD7vfD7�fD8vfD8�fD9vfD9�fD:vfD:�fD;vfD;�fD<vfD<�fD=vfD=�fD>vfD>�fD?vfD?�fD@vfD@�fDAvfDA�fDBvfDB�fDCvfDC�fDDvfDD�fDEvfDE�fDFvfDF�fDGvfDG�fDHvfDH�fDIvfDI�fDJvfDJ�fDKvfDK�fDLvfDL�fDMvfDM�fDNvfDN�fDOvfDO�fDPvfDP�fDQvfDQ�fDRvfDR�fDSvfDS�fDTvfDT�fDUvfDU�fDVvfDV�fDWvfDW�fDXvfDX�fDYvfDY�fDZvfDZ�fD[vfD[�fD\vfD\�fD]vfD]�fD^vfD^�fD_vfD_�fD`vfD`�fDavfDa�fDbvfDb�fDcvfDc�fDdvfDd�fDevfDe�fDfvfDf�fDgvfDg�fDhvfDh�fDivfDi�fDjvfDj�fDkvfDk�fDlvfDl�fDmvfDm�fDnvfDn�fDovfDo�fDpvfDp�fDqvfDq�fDrvfDr�fDsvfDs�fDtvfDt�fDy��D��D�*�D�s�D�ʏD��D�\�D��D��D�)D�]pD���D��=D� D�;�Dړ�D��RD�(�D�Z�D�D��{111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�S�A�I�A���A��#Aò-Aß�AÏ\AÅA�x�A�p�A�ffA�O�A�C�A�+A�VA��A���A��#A¼jAº^Aº^A²-A¥�AuA�O�A�G�A�|�A�A7A�C�A��/A���A��DA�S�A���A�S�A�%A��A�x�A�dZA��A���A�$�A��A��A�ƨA��HA��A�VA�oA�|�A�bNA�XA�\)A�XA�G�A�5?A��wA�t�A�I�A��A�ƨA���A���A��hA�\)A�VA��+A��A��;A��;A��!A�n�A���A�v�A�1A��A���A��9A��RA�z�A�^5A�G�A���A�A�A���A��hA��A��A��A���A�ȴA�ZA��yA�I�A�A�z�A���A�;dA�t�A��+A�=qA��A�A��A���A���A��wA���A�|�A~�A|  Ay�^Ay&�Ax��Aw��Av1At�RAst�ApVAn~�Al�!AjȴAi��Ait�Ah~�AfE�Ae33AeG�AeXAa��A_�wA]�TA\��A[��AZ��AZ~�AZ1'AYS�AY+AW�AUoATr�AS�AR�AR�AP��AM\)AK%AI�AHȴAHE�AG�
AGƨAFQ�AD��ADI�AC��ACp�AB��AB�AA�A?��A>ĜA<�RA<9XA;�hA;�A9p�A5��A4��A4JA3��A2�HA1��A0JA-�TA*��A'��A&jA%�wA%�7A$ȴA#�mA#dZA �HA �A��AƨAĜAz�A�TAȴAt�A�RAbA�A��AA�A�wA��AXA%AȴA�!A��A�DA\)A
�A	?}A��A�A��A�A�AdZA�`@���@�V@�1@���@�\)@�S�@�33@�
=@�@��@�ȴ@�=q@��u@���@��@�{@�`B@�I�@���@�@�X@�&�@��`@�ff@���@�/@��@�  @�O�@� �@�S�@���@�r�@��@�;d@��@�Q�@�1'@���@�t�@�C�@�@�$�@ٲ-@���@�r�@�
=@���@�hs@�@�  @��@�bN@�9X@�dZ@�~�@Ѳ-@�&�@�A�@�o@Ώ\@�X@�33@�v�@�^5@�5?@�G�@Ǯ@��@Ƨ�@�n�@�=q@���@���@Å@��y@+@�J@��@��
@�ȴ@�?}@��j@�A�@��;@�+@�{@��@�7L@�bN@�A�@�1'@� �@� �@��@���@���@�=q@�5?@�@���@��@�7L@���@�l�@�S�@�+@��!@�E�@��@���@�@�x�@��9@��D@�j@�9X@� �@� �@�(�@�+@�O�@��m@��^@�j@�Q�@�9X@��
@��@��y@�$�@�@�7L@���@��@�(�@��;@�t�@�;d@��@�ȴ@���@�=q@�-@���@��h@��h@��@�G�@���@�bN@��m@���@��@�ƨ@��;@��@���@���@���@���@���@���@���@���@��@�(�@�9X@�A�@�A�@�9X@�1'@�(�@���@�|�@�~�@��-@�`B@�?}@��@��`@���@���@�Z@��m@��F@��@��@��@���@���@��P@��@��@�|�@�t�@�S�@�C�@�C�@�33@�+@�"�@�
=@��y@���@�n�@�E�@�{@��T@���@�hs@�7L@��@��u@�  @��@�l�@�33@���@�v�@�M�@�J@���@�X@�&�@��@�%@���@��u@�9X@��@��;@���@�t�@�@��y@���@��R@���@���@���@���@��+@��+@�M�@�-@��@��@�hs@��@��@���@�9X@��@�
=@���@�n�@�E�@�{@��@���@�@��^@���@���@��@�x�@�p�@�p�@�`B@�V@���@{Z�@v^5@t  @poi@l�5@W��@L[�@C6z@@r�@<-�@5�@.E�@'��@!�H@8�@��@!@��@		l@a@ ��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A�S�A�I�A���A��#Aò-Aß�AÏ\AÅA�x�A�p�A�ffA�O�A�C�A�+A�VA��A���A��#A¼jAº^Aº^A²-A¥�AuA�O�A�G�A�|�A�A7A�C�A��/A���A��DA�S�A���A�S�A�%A��A�x�A�dZA��A���A�$�A��A��A�ƨA��HA��A�VA�oA�|�A�bNA�XA�\)A�XA�G�A�5?A��wA�t�A�I�A��A�ƨA���A���A��hA�\)A�VA��+A��A��;A��;A��!A�n�A���A�v�A�1A��A���A��9A��RA�z�A�^5A�G�A���A�A�A���A��hA��A��A��A���A�ȴA�ZA��yA�I�A�A�z�A���A�;dA�t�A��+A�=qA��A�A��A���A���A��wA���A�|�A~�A|  Ay�^Ay&�Ax��Aw��Av1At�RAst�ApVAn~�Al�!AjȴAi��Ait�Ah~�AfE�Ae33AeG�AeXAa��A_�wA]�TA\��A[��AZ��AZ~�AZ1'AYS�AY+AW�AUoATr�AS�AR�AR�AP��AM\)AK%AI�AHȴAHE�AG�
AGƨAFQ�AD��ADI�AC��ACp�AB��AB�AA�A?��A>ĜA<�RA<9XA;�hA;�A9p�A5��A4��A4JA3��A2�HA1��A0JA-�TA*��A'��A&jA%�wA%�7A$ȴA#�mA#dZA �HA �A��AƨAĜAz�A�TAȴAt�A�RAbA�A��AA�A�wA��AXA%AȴA�!A��A�DA\)A
�A	?}A��A�A��A�A�AdZA�`@���@�V@�1@���@�\)@�S�@�33@�
=@�@��@�ȴ@�=q@��u@���@��@�{@�`B@�I�@���@�@�X@�&�@��`@�ff@���@�/@��@�  @�O�@� �@�S�@���@�r�@��@�;d@��@�Q�@�1'@���@�t�@�C�@�@�$�@ٲ-@���@�r�@�
=@���@�hs@�@�  @��@�bN@�9X@�dZ@�~�@Ѳ-@�&�@�A�@�o@Ώ\@�X@�33@�v�@�^5@�5?@�G�@Ǯ@��@Ƨ�@�n�@�=q@���@���@Å@��y@+@�J@��@��
@�ȴ@�?}@��j@�A�@��;@�+@�{@��@�7L@�bN@�A�@�1'@� �@� �@��@���@���@�=q@�5?@�@���@��@�7L@���@�l�@�S�@�+@��!@�E�@��@���@�@�x�@��9@��D@�j@�9X@� �@� �@�(�@�+@�O�@��m@��^@�j@�Q�@�9X@��
@��@��y@�$�@�@�7L@���@��@�(�@��;@�t�@�;d@��@�ȴ@���@�=q@�-@���@��h@��h@��@�G�@���@�bN@��m@���@��@�ƨ@��;@��@���@���@���@���@���@���@���@���@��@�(�@�9X@�A�@�A�@�9X@�1'@�(�@���@�|�@�~�@��-@�`B@�?}@��@��`@���@���@�Z@��m@��F@��@��@��@���@���@��P@��@��@�|�@�t�@�S�@�C�@�C�@�33@�+@�"�@�
=@��y@���@�n�@�E�@�{@��T@���@�hs@�7L@��@��u@�  @��@�l�@�33@���@�v�@�M�@�J@���@�X@�&�@��@�%@���@��u@�9X@��@��;@���@�t�@�@��y@���@��R@���@���@���@���@��+@��+@�M�@�-@��@��@�hs@��@��@���@�9X@��@�
=@���@�n�@�E�@�{@��@���@�@��^@���@���@��@�x�@�p�@�p�@�`B@�VG�O�@{Z�@v^5@t  @poi@l�5@W��@L[�@C6z@@r�@<-�@5�@.E�@'��@!�H@8�@��@!@��@		l@a@ ��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB7LB7LBI�Be`BgmBk�Bl�Bl�Bl�Bl�Bl�Bl�Bm�Bn�Bn�Bo�Bo�Bq�Bq�Bq�Bq�Bq�Bs�Bw�B�VB�!B��B�TB�B  B+BiyB�1B�PB�VB�VB�JB�7B�%B�B{�BffBZBR�B6FB33B33B9XB:^BM�BJ�BO�BO�BO�BO�BO�BN�BH�BF�BC�BG�BG�BG�BG�BH�BF�BE�BE�B<jB1'B@�B6FB.B+B�yB�NB�-B��B��B�oB�bB�VB�JB�JB�B`BBM�BD�B?}B=qB1'B�B�B\BB
��B
��B
�5B
��B
ɺB
��B
��B
��B
��B
�{B
�VB
~�B
cTB
M�B
<jB
(�B
�B
DB
+B
+B
+B	��B	��B	�B	�5B	��B	ÖB	�3B	�B	��B	��B	�{B	�hB	��B	��B	��B	�VB	�B	}�B	t�B	n�B	l�B	l�B	e`B	cTB	\)B	S�B	O�B	E�B	6FB	1'B	!�B	1B�B�TB�;B�NB�sB�B�B�ZB�HB�HB�;B�)B�B��BƨB��B�qB�RB�FB�3B�3B��B��B��B��B��B��B��B�hB�PB�7B�B�B� B|�Bt�Bq�Bq�Bu�Bo�B`BBZBS�BN�BM�BM�BM�BN�BL�BC�B;dB8RB8RB8RB7LB8RB7LB8RB8RB8RB5?B33B2-B1'B0!B0!B0!B/B1'B5?B2-B33B33B33B33B49B49B49B49B49B6FB:^B;dB:^B<jB<jB>wB@�BC�BJ�Bk�B}�Bx�Bo�BjBiyBhsBe`BaHB_;B[#BT�BR�BQ�BR�BQ�BR�BR�BR�BQ�BQ�BP�BW
BXB\)B^5BbNBk�B~�B�B�%B�VB��B��B��B��B��B��B�B�B�B�!B�'B�!B�!B�'B�3B�9B�9B�?B�FB�LB�RB�^B�^B�jB�wB��BƨBɺB��B��B��B��B��B�
B�B�B�B�#B�/B�;B�TB�`B�mB�B�B�B�B�B��B��B��B��B��B��B��B	B	B	B	B	B		7B		7B	DB	JB	PB	PB	\B	�B	�B	!�B	(�B	)�B	)�B	)�B	+B	,B	/B	33B	6FB	9XB	:^B	<jB	>wB	@�B	B�B	D�B	D�B	F�B	G�B	J�B	J�B	L�B	N�B	N�B	O�B	S�B	VB	XB	YB	]/B	gmB	iyB	l�B	n�B	o�B	o�B	p�B	q�B	r�B	s�B	s�B	t�B	u�B	v�B	w�B	w�B	w�B	x�B	y�B	z�B	}�B	�B	�7B	�PB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�'B	�-B	�9B	�?B	�LB	�XB	�qB	B	ŢB	ƨB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�#B	�#B	�#B	�)B	�)B	�)B	�)B	�/B	�/B	�/B	�;B	�;B	�;B	�BB	�TB	�ZB	�`B	�fB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B
YB
JB
B
FB
+B
7�B
?B
B[B
F�B
NB
S�B
Z�B
`BB
dtB
iB
q�B
v�B
zB
}B
��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B//B//BA�B]AB_NBceBdkBdkBdkBdkBdkBdkBeqBfxBfxBg~Bg~Bi�Bi�Bi�Bi�Bi�Bk�Bo�B�4B��BƳB�-B�^B��B"�BaJB� B�B�%B�&B�B�B}�By�Bs�B^9BQ�BJ�B.B+
B+
B1/B25BE�BB�BG�BG�BG�BG�BG�BF�B@�B>B;nB?�B?�B?�B?�B@�B>�B=zB=zB4BB) B8\B.B%�B�B�XB�.B�B��B�gB�UB�HB�<B�0B�0Bx�BX,BE�B<�B7iB5]B)B�BpBLB
�
B
��B
��B
�(B
��B
��B
��B
��B
��B
��B
�tB
�OB
v�B
[PB
E�B
4iB
 �B
�B
GB	�.B	�.B	�.B	��B	��B	�B	�<B	��B	��B	�=B	�B	��B	��B	��B	�tB	��B	��B	��B	�cB	{ B	vB	l�B	f�B	d�B	d�B	]pB	[eB	T:B	L
B	G�B	=�B	.ZB	);B	�B	 HB�B�nB�UB�hB��B�B�B�uB�cB�cB�VB�DB�2B�B��B��B��B�pB�dB�RB�RB�B��B��B��B��B��B��B��B�sB�ZB}CBz0Bx$BuBl�Bi�Bi�Bm�Bg�BXjBREBL!BGBE�BE�BE�BGBD�B;�B3�B0~B0~B0~B/xB0~B/xB0~B0~B0~B-kB+`B*ZB)TB(NB(NB(NB'HB)TB-mB*[B+aB+aB+aB+aB,gB,gB,gB,gB,gB.tB2�B3�B2�B4�B4�B6�B8�B;�BB�Bc�BvBp�Bg�Bb�Ba�B`�B]�BYuBWhBSPBM,BK BJBK BJBK BK BK BJBJBIBO8BP>BTWBVcBZ|Bc�Bw&B{>B~QB��B��B��B��B�B�B� B�2B�,B�EB�KB�QB�KB�KB�QB�]B�cB�cB�iB�pB�vB�|B��B��B��B��B��B��B��B��B��B��B��B��B�3B�9B�FB�FB�LB�XB�dB�}B݉BߕB�B�B�B��B��B��B��B�	B�B�B�B�"B�.B�9B�@B�@B�FB	^B	^B	kB	qB	wB	wB	�B	�B	�B	�B	!B	""B	""B	""B	#(B	$.B	'AB	+XB	.kB	1}B	2�B	4�B	6�B	8�B	:�B	<�B	<�B	>�B	?�B	B�B	B�B	D�B	F�B	F�B	HB	LB	N(B	P4B	Q;B	URB	_�B	a�B	d�B	f�B	g�B	g�B	h�B	i�B	j�B	k�B	k�B	l�B	m�B	n�B	o�B	o�B	o�B	p�B	q�B	sB	vB	|:B	�XB	�qB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�!B	�-B	�3B	�@B	�FB	�LB	�WB	�]B	�jB	�vB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�	B	�B	�!B	�'B	�-B	�4B	�4B	�4B	�@B	�@B	�@B	�FB	�FB	�FB	�FB	�KB	�KB	�KB	�WB	�WB	�WB	�^B	�pB	�vB	�|B	ނB	߉B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��G�O�B	�	B	�tB
dB
	4B
`B
#5B
/�B
7-B
:tB
>�B
FB
LB
R�B
XZB
\�B
a(B
i�B
n�B
r'B
w�B
z�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.15 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9998(+/-0.0001), vertically averaged dS =-0.008(+/-0.003) in PSS-78.                                                                                                                                                                                       Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                      202202041144162022020411441620220204114416  AO  ARCAADJP                                                                    20200619170906    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20200619170906  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20200619170906  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20220204114416  IP                  G�O�G�O�G�O�                