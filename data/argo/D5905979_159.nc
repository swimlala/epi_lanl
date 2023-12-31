CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2020-06-29T14:01:52Z creation      
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20200629140152  20220204114427  5905979 UW, Argo                                                        STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  7662                            2C  D   APEX                            8312                            080318                          846 @�$ܲ��1   @�$�K�!\@6+��Q��b�I�^51   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   B   B   @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B/��B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DPy�DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Djy�Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Doy�Do��Dpy�Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt� Dy�RD��D�VfD��)D��D�#�D�UD���D��=D�%D�I�D���D��)D�%qD�Y�Dڏ�D��\D��D�X�D�D��f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�33@�33@�33A��A=��A]��A}��A���A���A���A���A���A���A���A���BffBffBffBffB'ffB/  B7ffB?ffBGffBOffBWffB_ffBgffBoffBwffBffB��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B�� B��3B��3B��3B��3B��3Bó3Bǳ3B˳3Bϳ3Bӳ3B׳3B۳3B߳3B�3B�3B�3B�3B�3B��3B��3B��3CٚCٚCٚCٚC	ٚCٚCٚCٚCٚCٚCٚCٚCٚCٚCٚCٚC!ٚC#ٚC%ٚC'ٚC)ٚC+ٚC-ٚC/ٚC1ٚC3ٚC5ٚC7ٚC9ٚC;ٚC=ٚC?ٚCAٚCCٚCEٚCGٚCIٚCKٚCMٚCOٚCQٚCSٚCUٚCWٚCYٚC[ٚC]ٚC_ٚCaٚCcٚCeٚCgٚCiٚCkٚCmٚCoٚCqٚCsٚCuٚCwٚCyٚC{ٚC}ٚCٚC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�� C�� C���C���C���C���C���C���C���C���C���C���C���C���C���C�� C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D vfD �fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fD	vfD	�fD
vfD
�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fD vfD �fD!vfD!�fD"vfD"�fD#vfD#�fD$vfD$�fD%vfD%�fD&vfD&�fD'vfD'�fD(vfD(�fD)vfD)�fD*vfD*�fD+vfD+�fD,vfD,�fD-vfD-�fD.vfD.�fD/vfD/�fD0vfD0�fD1vfD1�fD2vfD2�fD3vfD3�fD4vfD4�fD5vfD5�fD6vfD6�fD7vfD7�fD8vfD8�fD9vfD9�fD:vfD:�fD;vfD;�fD<vfD<�fD=vfD=�fD>vfD>�fD?vfD?�fD@vfD@�fDAvfDA�fDBvfDB�fDCvfDC�fDDvfDD�fDEvfDE�fDFvfDF�fDGvfDG�fDHvfDH�fDIvfDI�fDJvfDJ�fDKvfDK�fDLvfDL�fDMvfDM�fDNvfDN�fDOvfDO�fDPp DP�fDQvfDQ�fDRvfDR�fDSvfDS�fDTvfDT�fDUvfDU�fDVvfDV�fDWvfDW�fDXvfDX�fDYvfDY�fDZvfDZ�fD[vfD[�fD\vfD\�fD]vfD]�fD^vfD^�fD_vfD_�fD`vfD`�fDavfDa�fDbvfDb�fDcvfDc�fDdvfDd�fDevfDe�fDfvfDf�fDgvfDg�fDhvfDh�fDivfDi�fDjp Dj�fDkvfDk�fDlvfDl�fDmvfDm�fDnvfDn�fDop Do� Dpp Dp�fDqvfDq�fDrvfDr�fDsvfDs�fDtvfDt�fDy��D��D�Q�D��\D���D��D�PRD���D��pD� RD�D�D�� D��\D� �D�T�Dڊ�D��D� D�T)D�RD�љ111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��AȑhAț�Aȗ�Aț�AȍPA�n�A�9XAơ�Aƛ�AƋDA�n�A�VA�=qA�&�A�A���AŶFA�jA���Ać+A�M�A�dZA�%A�(�A�M�AA�A�\)A���A�\)A�  A�x�A��A���A��A��A��-A�l�A���A���A�O�A�p�A���A��DA���A�A��A��uA�dZA�"�A���A���A�^5A�I�A�
=A��^A�K�A��;A�t�A��\A�bNA�/A��A��A��/A��A�1A��#A�ZA���A��9A�E�A��A��
A�jA���A��A��RA��
A�-A�\)A�"�A���A�oA�;dA�XA��A�{A�C�A���A��!A�|�A�ĜA��`A�z�A��A�/A��A�?}A�oA��TA���A��\A�E�A��PA�9XA���A�/A���A���A��!A�{A�&�A�A���A�oA�M�A�C�A�
=A�A��A���A�mA}hsA{��Ay
=Au��As
=ArE�Aq�
Amp�Ai;dAf��Ab��Aa;dA_�A\{AW��ATĜAP�`AN��AM�^AMVAJ��AJ$�AI��AHȴAGS�AF^5AF-AD�yAC��AC;dAB  AA%A@jA>r�A<�uA<n�A;K�A9oA7C�A5��A3\)A21A1p�A133A0�\A0=qA/�;A.��A.�A,��A,�\A+�TA)S�A(M�A&I�A$��A$JA#�PA#�A"E�A 9XAt�AdZA��A��A�+AXA�A1A�hAbA��A9XA��A?}An�AC�A�mAA�+A�wA+A~�AJAA
�A	�A	S�A�A1'A�TA-A�7A��A�A=qAZAE�AbNA ��A r�@��@�~�@���@�V@���@�-@���@�hs@�\)@�ȴ@�7L@�-@���@�p�@�+@�9X@�t�@�R@�@�+@�@�D@�h@ߥ�@�"�@�
=@��;@�J@ڇ+@���@٩�@���@��H@�bN@���@���@�z�@���@�hs@˶F@�n�@�G�@���@���@�v�@�5?@��@�Ĝ@å�@�@�x�@��;@��!@�5?@���@�O�@���@�A�@�|�@��@��@�J@���@��@���@�  @��@�o@�@�@���@��@�7L@�Ĝ@�C�@�n�@��@���@���@�ƨ@�"�@���@�Q�@�
=@��@���@�M�@�n�@��@�n�@���@��@���@��j@���@��@� �@��@�A�@� �@���@�?}@��/@��9@�Ĝ@���@�C�@�"�@�33@�5?@���@�?}@�z�@�Q�@�A�@�A�@�1'@� �@�I�@� �@�  @� �@��w@�9X@���@��D@�r�@�b@���@�33@��R@�ff@�^5@��-@���@��@�M�@��@�hs@�x�@�p�@�`B@��T@��\@�~�@�x�@��@�Ĝ@��u@� �@���@��@��!@�ff@�5?@�J@��-@�?}@�%@��9@�r�@�j@���@��@��u@��@�Z@��m@��y@��+@���@���@��u@���@��@��@��u@�I�@�I�@��@���@���@�dZ@�@��!@�V@��#@�O�@�V@���@�Ĝ@��9@���@�r�@�  @�S�@�C�@�;d@�;d@�+@�"�@��@�"�@�"�@�
=@���@��\@�ff@�J@���@�@��7@�/@���@��j@�I�@��@��
@��@���@�\)@�;d@��@�o@�o@��@��R@�M�@��@�{@��@��^@��@�`B@�X@�?}@�V@�%@��@��9@��D@�1'@�b@��
@��w@��P@��y@�v�@��@��@��@�V@�E�@���@���@��h@��7@��7@��@�`B@�O�@�V@�r�@� �@��;@��@|4n@sa@kO@c i@\@VE�@NQ@FZ�@A�@9��@3E9@.{@)�d@&��@#e�@��@�@w2@g8@�_111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  AȑhAț�Aȗ�Aț�AȍPA�n�A�9XAơ�Aƛ�AƋDA�n�A�VA�=qA�&�A�A���AŶFA�jA���Ać+A�M�A�dZA�%A�(�A�M�AA�A�\)A���A�\)A�  A�x�A��A���A��A��A��-A�l�A���A���A�O�A�p�A���A��DA���A�A��A��uA�dZA�"�A���A���A�^5A�I�A�
=A��^A�K�A��;A�t�A��\A�bNA�/A��A��A��/A��A�1A��#A�ZA���A��9A�E�A��A��
A�jA���A��A��RA��
A�-A�\)A�"�A���A�oA�;dA�XA��A�{A�C�A���A��!A�|�A�ĜA��`A�z�A��A�/A��A�?}A�oA��TA���A��\A�E�A��PA�9XA���A�/A���A���A��!A�{A�&�A�A���A�oA�M�A�C�A�
=A�A��A���A�mA}hsA{��Ay
=Au��As
=ArE�Aq�
Amp�Ai;dAf��Ab��Aa;dA_�A\{AW��ATĜAP�`AN��AM�^AMVAJ��AJ$�AI��AHȴAGS�AF^5AF-AD�yAC��AC;dAB  AA%A@jA>r�A<�uA<n�A;K�A9oA7C�A5��A3\)A21A1p�A133A0�\A0=qA/�;A.��A.�A,��A,�\A+�TA)S�A(M�A&I�A$��A$JA#�PA#�A"E�A 9XAt�AdZA��A��A�+AXA�A1A�hAbA��A9XA��A?}An�AC�A�mAA�+A�wA+A~�AJAA
�A	�A	S�A�A1'A�TA-A�7A��A�A=qAZAE�AbNA ��A r�@��@�~�@���@�V@���@�-@���@�hs@�\)@�ȴ@�7L@�-@���@�p�@�+@�9X@�t�@�R@�@�+@�@�D@�h@ߥ�@�"�@�
=@��;@�J@ڇ+@���@٩�@���@��H@�bN@���@���@�z�@���@�hs@˶F@�n�@�G�@���@���@�v�@�5?@��@�Ĝ@å�@�@�x�@��;@��!@�5?@���@�O�@���@�A�@�|�@��@��@�J@���@��@���@�  @��@�o@�@�@���@��@�7L@�Ĝ@�C�@�n�@��@���@���@�ƨ@�"�@���@�Q�@�
=@��@���@�M�@�n�@��@�n�@���@��@���@��j@���@��@� �@��@�A�@� �@���@�?}@��/@��9@�Ĝ@���@�C�@�"�@�33@�5?@���@�?}@�z�@�Q�@�A�@�A�@�1'@� �@�I�@� �@�  @� �@��w@�9X@���@��D@�r�@�b@���@�33@��R@�ff@�^5@��-@���@��@�M�@��@�hs@�x�@�p�@�`B@��T@��\@�~�@�x�@��@�Ĝ@��u@� �@���@��@��!@�ff@�5?@�J@��-@�?}@�%@��9@�r�@�j@���@��@��u@��@�Z@��m@��y@��+@���@���@��u@���@��@��@��u@�I�@�I�@��@���@���@�dZ@�@��!@�V@��#@�O�@�V@���@�Ĝ@��9@���@�r�@�  @�S�@�C�@�;d@�;d@�+@�"�@��@�"�@�"�@�
=@���@��\@�ff@�J@���@�@��7@�/@���@��j@�I�@��@��
@��@���@�\)@�;d@��@�o@�o@��@��R@�M�@��@�{@��@��^@��@�`B@�X@�?}@�V@�%@��@��9@��D@�1'@�b@��
@��w@��P@��y@�v�@��@��@��@�V@�E�@���@���@��h@��7@��7@��@�`B@�O�@�V@�r�@� �G�O�@��@|4n@sa@kO@c i@\@VE�@NQ@FZ�@A�@9��@3E9@.{@)�d@&��@#e�@��@�@w2@g8@�_111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
�B
�B
�B
�B
�B
�B
�B
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
�B
��B
=BS�Bo�B�{B�#B��BJB�B��Bv�BD�B8RBI�BbNBr�By�B�B�\B��B�-B��B  B1BDBoBuB{B{B�B�B�B)�B0!B1'B49B6FB9XB<jBA�BK�BM�BP�BP�BP�BS�BZBaHBcTBiyBp�Bs�Bz�B�B�B�B�B�%B{�Bp�BjBbNBcTBo�Bn�Be`BA�B�BVBB��B�B�BB�B��BȴBĜB�XB��B��B�JB�+B�B~�Bn�B_;BW
BF�B9XB33B/B+B#�B�B\BB
��B
�fB
��B
�'B
��B
z�B
gmB
L�B
5?B
%�B
VB	��B	�`B	�/B	�
B	��B	��B	�JB	o�B	dZB	P�B	?}B	�B	%B�B�;B�#B�BɺBĜB��B��B�jB�XB�RB�dB�?B�-B�-B�B�B��B��B��B��B��B�DB�+B�B}�B~�B� B�B�=B��B��B��B��B��B��B��B�DB�%B|�Bw�Bu�Bs�Bo�Bl�BjBiyBiyBl�Bk�Bl�Bl�Bl�Bl�Bo�Bp�Bt�B{�Bq�Bm�BffBbNBbNBdZBcTBaHBaHBaHBbNBaHB_;B`BB_;B_;BcTBffBe`BjBl�BgmBhsBy�Bo�B_;BbNB`BBaHB_;B^5B_;Be`Bp�Bq�Bn�Bm�Bk�BdZBcTBcTBcTB_;B^5B_;B^5B]/B]/BaHBbNB\)B]/BbNB^5BXB^5BgmBdZBhsBp�Bt�Bt�Bq�Bt�Bt�Bt�Bt�Bv�Bw�By�Bx�By�B�B�7B�7B�1B�+B�1B�PB�PB�VB�hB�hB�uB��B��B��B��B��B��B��B��B��B��B�B�B�B�B�'B�-B�?B�RB�dB�dB�jB�^B�XB�qBĜB��B��BŢBĜBŢBǮB��B��B�B�
B�B�)B�HB�TB�ZB�mB�B�B�B��B��B��B��B��B��B��B��B	B	B	%B	DB	PB	\B	bB	hB	oB	�B	�B	�B	�B	 �B	(�B	5?B	7LB	7LB	6FB	6FB	6FB	9XB	>wB	@�B	@�B	B�B	G�B	M�B	M�B	N�B	S�B	T�B	VB	\)B	`BB	dZB	dZB	iyB	m�B	m�B	o�B	o�B	q�B	s�B	t�B	u�B	v�B	w�B	x�B	x�B	z�B	z�B	{�B	~�B	�B	�%B	�%B	�%B	�+B	�1B	�7B	�JB	�VB	�\B	�bB	�hB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�'B	�3B	�FB	�LB	�LB	�LB	�RB	�^B	�jB	�wB	��B	��B	ÖB	ĜB	ƨB	ǮB	ɺB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�)B	�)B	�;B	�BB	�BB	�BB	�HB	�NB	�TB	�TB	�ZB	�ZB	�ZB	�`B	�fB	�mB	�sB	�sB	�sB	�mB	�mB	�`B	�fB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	�(B
�B
�B
�B
&�B
/�B
6�B
?�B
G�B
M�B
S&B
X�B
_;B
b�B
ezB
h$B
k�B
qB
wB
{0B
~�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B
�PB
�PB
�PB
�PB
�IB
�CB
�gB
�sB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�{B
�\B
�{B
��BC�B_HB� BʿB�B��B�JB�~BftB4MB(B9kBQ�Bb\Bi�Bq�BB�ZB��B�dB�B��B��B	BBBB!B!BKB�B�B �B#�B%�B(�B, B1B;\B=hB@yB@yB@yBC�BI�BP�BR�BYB`5BcGBjqBq�Bs�Bs�Bt�Bu�BkxB`7BZBQ�BR�B_2B^,BT�B1#BFB��B��B�B�PB��BǺB�~B�`B�IB�B��B�@B{�Bv�Bs�Bn�B^QBN�BF�B6gB)B"�B�B�B�BMB
�#B
��B
�B
�3B
��B
��B
��B
j�B
WLB
<�B
%%B
�B	�BB	��B	�RB	�"B	��B	��B	��B	|IB	_�B	T_B	@�B	/�B	�B�8BޯB�TB�=B�1B��B��B��B��B��B�wB�rB��B�_B�NB�NB�=B�*B�B��B��B��B��B{lBwTBr6BnBo%Bp+Br7BzgB��B��B��B��B��B��B��B{oBvQBmBg�Be�Bc�B_�B\�BZ�BY�BY�B\�B[�B\�B\�B\�B\�B_�B`�Bd�BlBa�B]�BV�BR�BR�BT�BS�BQBQBQBR�BQBOsBPzBOsBOsBS�BV�BU�BZ�B\�BW�BX�BjB_�BOuBR�BP|BQ�BOuBNpBOvBU�B`�Ba�B^�B]�B[�BT�BS�BS�BS�BOwBNqBOwBNrBMlBMlBQ�BR�BLgBMmBR�BNsBHOBNtBW�BT�BX�B`�Bd�Bd�Ba�Bd�Bd�Bd�Bd�BgBhBjBiBjBrIBysBysBxmBwhBxnB}�B}�B~�B��B��B��B��B��B��B��B��B��B��B�B�B�*B�BB�BB�BB�TB�`B�fB�xB��B��B��B��B��B��B��B��B�B�(B��B��B��B��B�
B�.B�:B�@B�MB�_B�}BӉBԏBסB��B��B��B��B��B�B�B� B�B�B�&B�>B�KB�WB�uB��B��B	 �B	�B	�B	�B	
�B	�B	�B	�B	$B	%kB	'xB	'xB	&rB	&rB	&rB	)�B	.�B	0�B	0�B	2�B	7�B	=�B	=�B	?B	D!B	E'B	F,B	LQB	PiB	T�B	T�B	Y�B	]�B	]�B	_�B	_�B	a�B	c�B	d�B	e�B	f�B	g�B	h�B	h�B	kB	kB	lB	oB	uBB	vHB	vHB	vHB	wNB	xTB	yZB	|mB	~yB	B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�	B	�B	�"B	�:B	�@B	�FB	�RB	�eB	�kB	�kB	�kB	�pB	�|B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�+B	�2B	�8B	�8B	�8B	�DB	�DB	�UB	�\B	�\B	�\B	�bB	�hB	�nB	�nB	�tB	�tB	�tB	�zB	րB	ׇB	،B	؍B	؍B	ׇB	ׇB	�zB	րB	ٓB	ژB	۞B	ޱB	ݪB	ޱB	߷B	߷B	߷B	�B	�B	�B	�B	��B	��B	��G�O�B	�@B	��B
�B
�B
�B
�B
&�B
/�B
7�B
=�B
C8B
H�B
OLB
R�B
U�B
X5B
[�B
aB
g'B
k@B
n�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.15 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9996(+/-0.0001), vertically averaged dS =-0.016(+/-0.004) in PSS-78.                                                                                                                                                                                       Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                      202202041144272022020411442720220204114427  AO  ARCAADJP                                                                    20200629140152    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20200629140152  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20200629140152  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20220204114427  IP                  G�O�G�O�G�O�                