CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       ~2017-09-30T15:35:27Z creation;2017-09-30T15:35:30Z conversion to V3.1;2019-12-18T07:27:49Z update;2022-11-21T05:31:55Z update;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         BPRIMARY|https://orcid.org/0000-0001-9150-6442|Kanako Sato, JAMSTEC        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    7H   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7X   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7\   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    7`   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7p   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  8   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        8H   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    8L   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    8P   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     8T   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8t   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8x   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8|   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8�   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8�   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8�   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    9   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    9   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    9   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        :   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  :    PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ]l   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  aX   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  q   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �@   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �,   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �x   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �(   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �h   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �h   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �h   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �h   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �8   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �H   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �L   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �\   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �`   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �d   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �hArgo profile    3.1 1.2 19500101000000  20170930153527  20221123111507  4902148 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               rA   JA  I1_0397_114                     2C  Dd �NAVIS_A                         0397                            ARGO 011514                     863 @�*��X�1   @�* ��-�@;�S��Mj�d �c�	1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @333@y��@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B ffB'��B0  B7��B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�ɚD��311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@4z�@z�H@���A Q�A Q�A@Q�A`Q�A�(�A�(�A�(�A�(�A�(�A�(�A�(�A�(�B {B{B{B{B z�B'�B0{B7�B@{BH{BP{BX{B`{Bh{Bp{Bx{B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=C CCCCC
CCCCCCCCCCC C"C$C&C(C*C,C.C0C2C4C6C8C:C<C>C@CBCDCFCHCJCLCNCPCRCTCVCXCZC\C^C`CbCdCfChCjClCnCpCrCtCvCxCzC|C~C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C���C��C�\C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C���C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D HD �HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HD	HD	�HD
HD
�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HD��D�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HD HD �HD!HD!�HD"HD"�HD#HD#�HD$HD$�HD%HD%�HD&HD&�HD'HD'�HD(HD(�HD)HD)�HD*HD*�HD+HD+�HD,HD,�HD-HD-�HD.HD.�HD/HD/�HD0HD0�HD1HD1�HD2HD2�HD3HD3�HD4HD4�HD5HD5�HD6HD6�HD7HD7�HD8HD8�HD9HD9�HD:HD:�HD;HD;�HD<HD<�HD=HD=�HD>HD>�HD?HD?�HD@HD@�HDAHDA�HDBHDB�HDCHDC�HDDHDD�HDEHDE�HDFHDF�HDGHDG�HDHHDH�HDIHDI�HDJHDJ�HDKHDK�HDLHDL�HDMHDM�HDNHDN�HDOHDO�HDPHDP�HDQHDQ�HDRHDR�HDSHDS�HDTHDT�HDUHDU�HDVHDV�HDWHDW�HDXHDX�HDYHDY�HDZHDZ�HD[HD[�HD\HD\�HD]HD]�HD^HD^�HD_HD_�HD`HD`�HDaHDa�HDbHDb�HDcHDc�HDdHDd�HDeHDe�HDfHDf�HDgHDg�HDhHDh�HDiHDi�HDjHDj�HDkHDk�HDlHDl�HDmHDm�HDnHDn�HDoHDo�HDpHDp�HDqHDq�HDrHDr�HDsHDs�HDtHDt�HDuHDu�HDvHDv�HDwHDw�HDxHDx�HDyHDy�HDzHDz�HD{HD{�HD|HD|�HD}HD}�HD~HD~�HDHD�HD� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D�D���D� �D�@�DÀ�D���D� �D�@�DĀ�D���D� �D�@�Dŀ�D���D� �D�@�Dƀ�D���D� �D�@�Dǀ�D���D� �D�@�DȀ�D���D� �D�@�Dɀ�D���D� �D�@�Dʀ�D���D� �D�@�Dˀ�D���D� �D�@�D̀�D���D� �D�@�D̀�D���D� �D�@�D΀�D���D� �D�@�Dπ�D���D� �D�@�DЀ�D���D� �D�@�Dр�D���D� �D�@�DҀ�D���D� �D�@�DӀ�D���D� �D�@�DԀ�D���D� �D�@�DՀ�D���D� �D�@�Dր�D���D� �D�@�D׀�D���D� �D�@�D؀�D���D� �D�@�Dـ�D���D� �D�@�Dڀ�D���D� �D�@�Dۀ�D���D� �D�@�D܀�D���D� �D�@�D݀�D���D� �D�@�Dހ�D���D� �D�@�D߀�D���D� �D�@�D���D���D� �D�@�DဤD���D� �D�@�D․D���D� �D�@�D〤D���D� �D�@�D䀤D���D� �D�@�D値D���D� �D�@�D怤D���D� �D�@�D瀤D���D� �D�@�D耤D���D� �D�@�D逤D���D� �D�@�DꀤD���D� �D�@�D뀤D���D� �D�@�D쀤D���D� �D�@�D퀤D���D� �D�@�DD���D� �D�@�DD���D� �D�@�D���D���D� �D�@�D�D���D� �D�@�D�D���D� �D�@�D�D���D� �D�@�D�D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D��=D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�z�AۃA�|�AہA�|�A�~�A�|�A�r�A�1'AٶFA؏\A�%A�ZA֍PAՓuA�ZA�M�A�9XA�;dA�ffA�x�A���A�z�A��9A�/A�+A��A�/A�n�A��A��;A�/A���A�r�A��A�ȴA��A�ƨA���A���A���A���A�A�hsA��/A�ffA�A�A���A��A�E�A�  A�33A�\)A�`BA��yA�ĜA��A�bA���A��DA�~�A�S�A��A�\)A�%A�O�A��
A��A���A�ffA�5?A��mA���A��PA�t�A�ƨA�1A��TA���A�ȴA���A��A�jA��A��uA�VA�(�A��
A�|�A&�A~�A~bNA|ȴAy��Aw��AwoAvȴAvz�Av1Au��Au�At��AsXAq;dAnE�Am��AkdZAiAh �Ag��Af��Ae"�Ab��A_/A]hsA[�7AZ��AZ~�AZJAX��AW��AVĜAU��AU`BAU"�AU%AT�AT�AS��AQ��AN�\AL�yAL�AK�mAK"�AJĜAI�;AIoAH�!AH  AGC�AF�+AF �AE33AC��AA&�A@ZA?��A>�`A>1'A<=qA;%A9��A8�A8(�A7A6��A5�A4��A4Q�A4(�A4�A4  A3�wA3+A2�9A29XA1�hA1dZA0I�A/hsA.�yA.ȴA.�jA.�A.�uA.z�A.jA.^5A-�A,�+A*5?A(  A&�HA&��A&M�A%l�A$��A$�A#��A"�jA"z�A"jA"E�A!ƨA!&�A ��A   A`BAffA`BAoA�A�TA��A�A�wA��A7LA�;AhsA7LAr�A�PA7LA"�AA��A1'A��Al�A%A�!A�Ar�A�A"�A��AVA�A�-A&�A
v�A
9XA	�A(�AjAVA �A�A�A�TA�#A��A�hA��A ��@���@�{@��m@���@�l�@���@��#@� �@�+@��y@�R@�@�ff@�J@�-@�Z@�o@�h@��@�@�1'@�@�%@�|�@�V@ܼj@�@�?}@��/@��;@�@�7L@�Ĝ@�33@с@�A�@�9X@�1'@�b@��m@��@�$�@�hs@ǥ�@��@�5?@���@�x�@�?}@��/@��@�33@��7@�9X@�;d@�ff@��@���@��@�ƨ@��@��@�ȴ@�X@���@�Z@��m@��@�ff@�5?@���@���@��P@��!@��^@���@���@�+@�Z@�dZ@�K�@�o@�ȴ@���@�~�@�v�@�ff@�5?@���@�O�@��@��\@��j@�t�@��y@��+@�-@�@��@��@�bN@�\)@��7@��@�I�@��y@�/@��9@��w@���@��/@�bN@���@�ƨ@���@���@���@���@���@���@��P@�dZ@�;d@��@���@��R@��R@���@�~�@�7L@�r�@�1'@��;@���@�C�@�ȴ@�v�@�ff@�5?@���@���@�@���@�O�@�j@�9X@�b@��@���@��P@��@��@��@�~�@�{@��T@��^@��h@�O�@��/@�bN@� �@�b@���@��@��;@�|�@��@��@��R@�n�@�J@��^@�%@�(�@���@�t�@�o@���@��!@���@���@��\@��+@��+@�v�@�n�@�n�@�n�@�^5@�E�@�$�@�@��@���@�O�@�Ĝ@�  @�w@�P@K�@
=@~E�@}�T@}��@}��@}�@}�@|�@|�@|1@{S�@yhs@x�9@x��@x�u@x�u@x�u@x�@xbN@x �@w�w@w�P@w\)@w;d@w�@w�@w
=@v��@vȴ@vv�@v5?@v@u��@u�@u�@s��@sdZ@s@r�\@rM�@q��@q��@q��@q�^@q��@q&�@p�`@p�u@p1'@o\)@o
=@n5?@l1@k��@k33@j��@j��@j~�@j-@i��@i�#@i��@i�7@ix�@ihs@iX@i7L@i&�@i%@h�`@h�9@hQ�@f�y@f5?@f$�@f@e�T@e�-@e�@eO�@eV@d�@d��@d�@d�@d�@d��@dz�@dZ@d9X@d�@c�m@cdZ@b��@b=q@a��@aX@`r�@_��@_
=@^ȴ@^$�@]?}@\�@\I�@\1@[�@[C�@[@Z�H@Z��@Z��@Z��@Z^5@Y�#@Y��@Y�7@Y�@X�u@XA�@W\)@Vff@V5?@U�T@U��@U��@U@Up�@U`B@U?}@U/@T��@T�/@T��@Tz�@Tz�@Tj@Tj@TI�@T1@S��@Q�#@Q�7@QX@Q7L@Q&�@P��@P �@O�P@N�y@N�y@N�y@N��@O�@O+@O|�@O|�@N��@N5?@M��@M?}@L��@L�@Lz�@LZ@K�m@Kƨ@K��@Ko@J^5@IX@HbN@H  @G��@G|�@GK�@G+@F��@F�@F��@FE�@F{@F@E��@E@E�-@E��@E�@D�/@DI�@D(�@D(�@D9X@D9X@D(�@D(�@D(�@D(�@D9X@DI�@DI�@DI�@DI�@D9X@D�@D1@C�
@C�
@C�
@Cƨ@C��@C"�@B��@A�7@@�@?�;@?l�@?+@>��@=�T@=��@=`B@=V@<�j@<z�@;��@;��@;dZ@;S�@;S�@;"�@;@:��@:M�@:J@9G�@9&�@8�`@8�9@8�@8Q�@8Q�@8Q�@8 �@8  @7�@7�w@7K�@7
=@6�R@6E�@5�T@5��@5�h@5`B@5?}@5V@4��@4��@4�@4��@4��@4��@4�D@4�D@4�D@4�D@4��@4j@49X@4(�@3��@3�m@3�
@3�F@3��@3�@3dZ@3S�@333@3o@2�!@2n�@1�@1�@0��@0Q�@0b@/�;@/�;@/|�@/\)@/
=@.�@.v�@.{@-�T@-�-@-�h@-p�@-O�@-?}@,�@,�@,Z@,9X@,1@+�@+"�@*�H@*��@*��@*^5@)�^@)��@)X@)7L@)%@(Ĝ@(bN@'\)@';d@'+@&��@%�@%@%��@%��@%��@%��@%p�@%?}@%V@$�/@$��@$��@$��@$j@$9X@$�@$1@#��@#�@!�#@ ��@ Ĝ@ r�@ A�@  �@|�@�@ȴ@�R@�R@��@�+@v�@{@�@�@z�@9X@�F@�@"�@��@��@^5@-@��@��@x�@hs@X@X@G�@�@%@��@��@Ĝ@��@1'@|�@�@
=@
=@��@ff@�h@`B@V@�/@�/@z�@(�@1@1@�@��@1@33@33@33@33@33@"�@"�@�@�!@��@�\@~�@^5@J@�@�#@��@�7@�7@x�@hs@7L@�`@�9@�9@�@Q�@A�@1'@ �@  @��@�@��@�P@|�@l�@l�@l�@l�@K�@�@
=@�@v�@$�@@@�@�@�@��@@�h@/@�/@��@�D@Z@I�@9X@�@�@�@�m@��@�@t�@
��@
M�@
-@
�@
J@
J@
J@
�@
J@
J@
�@
J@
J@
J@
J@
J@	��@	�@	X@��@  @��@�@�P@
=@��@��@�y@�+@v�@v�@v�@v�@E�@E�@E�@{@{@{@�T@�T@��@��@��@��@@��@��@@��@�T@�T@�T@�T@p�@/@ƨ@��@�@t�@t�@dZ@S�@33@33@o@�@@@@o@o@o@o@"�@o@@@@o@o@�@��@��@��@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�z�AۃA�|�AہA�|�A�~�A�|�A�r�A�1'AٶFA؏\A�%A�ZA֍PAՓuA�ZA�M�A�9XA�;dA�ffA�x�A���A�z�A��9A�/A�+A��A�/A�n�A��A��;A�/A���A�r�A��A�ȴA��A�ƨA���A���A���A���A�A�hsA��/A�ffA�A�A���A��A�E�A�  A�33A�\)A�`BA��yA�ĜA��A�bA���A��DA�~�A�S�A��A�\)A�%A�O�A��
A��A���A�ffA�5?A��mA���A��PA�t�A�ƨA�1A��TA���A�ȴA���A��A�jA��A��uA�VA�(�A��
A�|�A&�A~�A~bNA|ȴAy��Aw��AwoAvȴAvz�Av1Au��Au�At��AsXAq;dAnE�Am��AkdZAiAh �Ag��Af��Ae"�Ab��A_/A]hsA[�7AZ��AZ~�AZJAX��AW��AVĜAU��AU`BAU"�AU%AT�AT�AS��AQ��AN�\AL�yAL�AK�mAK"�AJĜAI�;AIoAH�!AH  AGC�AF�+AF �AE33AC��AA&�A@ZA?��A>�`A>1'A<=qA;%A9��A8�A8(�A7A6��A5�A4��A4Q�A4(�A4�A4  A3�wA3+A2�9A29XA1�hA1dZA0I�A/hsA.�yA.ȴA.�jA.�A.�uA.z�A.jA.^5A-�A,�+A*5?A(  A&�HA&��A&M�A%l�A$��A$�A#��A"�jA"z�A"jA"E�A!ƨA!&�A ��A   A`BAffA`BAoA�A�TA��A�A�wA��A7LA�;AhsA7LAr�A�PA7LA"�AA��A1'A��Al�A%A�!A�Ar�A�A"�A��AVA�A�-A&�A
v�A
9XA	�A(�AjAVA �A�A�A�TA�#A��A�hA��A ��@���@�{@��m@���@�l�@���@��#@� �@�+@��y@�R@�@�ff@�J@�-@�Z@�o@�h@��@�@�1'@�@�%@�|�@�V@ܼj@�@�?}@��/@��;@�@�7L@�Ĝ@�33@с@�A�@�9X@�1'@�b@��m@��@�$�@�hs@ǥ�@��@�5?@���@�x�@�?}@��/@��@�33@��7@�9X@�;d@�ff@��@���@��@�ƨ@��@��@�ȴ@�X@���@�Z@��m@��@�ff@�5?@���@���@��P@��!@��^@���@���@�+@�Z@�dZ@�K�@�o@�ȴ@���@�~�@�v�@�ff@�5?@���@�O�@��@��\@��j@�t�@��y@��+@�-@�@��@��@�bN@�\)@��7@��@�I�@��y@�/@��9@��w@���@��/@�bN@���@�ƨ@���@���@���@���@���@���@��P@�dZ@�;d@��@���@��R@��R@���@�~�@�7L@�r�@�1'@��;@���@�C�@�ȴ@�v�@�ff@�5?@���@���@�@���@�O�@�j@�9X@�b@��@���@��P@��@��@��@�~�@�{@��T@��^@��h@�O�@��/@�bN@� �@�b@���@��@��;@�|�@��@��@��R@�n�@�J@��^@�%@�(�@���@�t�@�o@���@��!@���@���@��\@��+@��+@�v�@�n�@�n�@�n�@�^5@�E�@�$�@�@��@���@�O�@�Ĝ@�  @�w@�P@K�@
=@~E�@}�T@}��@}��@}�@}�@|�@|�@|1@{S�@yhs@x�9@x��@x�u@x�u@x�u@x�@xbN@x �@w�w@w�P@w\)@w;d@w�@w�@w
=@v��@vȴ@vv�@v5?@v@u��@u�@u�@s��@sdZ@s@r�\@rM�@q��@q��@q��@q�^@q��@q&�@p�`@p�u@p1'@o\)@o
=@n5?@l1@k��@k33@j��@j��@j~�@j-@i��@i�#@i��@i�7@ix�@ihs@iX@i7L@i&�@i%@h�`@h�9@hQ�@f�y@f5?@f$�@f@e�T@e�-@e�@eO�@eV@d�@d��@d�@d�@d�@d��@dz�@dZ@d9X@d�@c�m@cdZ@b��@b=q@a��@aX@`r�@_��@_
=@^ȴ@^$�@]?}@\�@\I�@\1@[�@[C�@[@Z�H@Z��@Z��@Z��@Z^5@Y�#@Y��@Y�7@Y�@X�u@XA�@W\)@Vff@V5?@U�T@U��@U��@U@Up�@U`B@U?}@U/@T��@T�/@T��@Tz�@Tz�@Tj@Tj@TI�@T1@S��@Q�#@Q�7@QX@Q7L@Q&�@P��@P �@O�P@N�y@N�y@N�y@N��@O�@O+@O|�@O|�@N��@N5?@M��@M?}@L��@L�@Lz�@LZ@K�m@Kƨ@K��@Ko@J^5@IX@HbN@H  @G��@G|�@GK�@G+@F��@F�@F��@FE�@F{@F@E��@E@E�-@E��@E�@D�/@DI�@D(�@D(�@D9X@D9X@D(�@D(�@D(�@D(�@D9X@DI�@DI�@DI�@DI�@D9X@D�@D1@C�
@C�
@C�
@Cƨ@C��@C"�@B��@A�7@@�@?�;@?l�@?+@>��@=�T@=��@=`B@=V@<�j@<z�@;��@;��@;dZ@;S�@;S�@;"�@;@:��@:M�@:J@9G�@9&�@8�`@8�9@8�@8Q�@8Q�@8Q�@8 �@8  @7�@7�w@7K�@7
=@6�R@6E�@5�T@5��@5�h@5`B@5?}@5V@4��@4��@4�@4��@4��@4��@4�D@4�D@4�D@4�D@4��@4j@49X@4(�@3��@3�m@3�
@3�F@3��@3�@3dZ@3S�@333@3o@2�!@2n�@1�@1�@0��@0Q�@0b@/�;@/�;@/|�@/\)@/
=@.�@.v�@.{@-�T@-�-@-�h@-p�@-O�@-?}@,�@,�@,Z@,9X@,1@+�@+"�@*�H@*��@*��@*^5@)�^@)��@)X@)7L@)%@(Ĝ@(bN@'\)@';d@'+@&��@%�@%@%��@%��@%��@%��@%p�@%?}@%V@$�/@$��@$��@$��@$j@$9X@$�@$1@#��@#�@!�#@ ��@ Ĝ@ r�@ A�@  �@|�@�@ȴ@�R@�R@��@�+@v�@{@�@�@z�@9X@�F@�@"�@��@��@^5@-@��@��@x�@hs@X@X@G�@�@%@��@��@Ĝ@��@1'@|�@�@
=@
=@��@ff@�h@`B@V@�/@�/@z�@(�@1@1@�@��@1@33@33@33@33@33@"�@"�@�@�!@��@�\@~�@^5@J@�@�#@��@�7@�7@x�@hs@7L@�`@�9@�9@�@Q�@A�@1'@ �@  @��@�@��@�P@|�@l�@l�@l�@l�@K�@�@
=@�@v�@$�@@@�@�@�@��@@�h@/@�/@��@�D@Z@I�@9X@�@�@�@�m@��@�@t�@
��@
M�@
-@
�@
J@
J@
J@
�@
J@
J@
�@
J@
J@
J@
J@
J@	��@	�@	X@��@  @��@�@�P@
=@��@��@�y@�+@v�@v�@v�@v�@E�@E�@E�@{@{@{@�T@�T@��@��@��@��@@��@��@@��@�T@�T@�T@�T@p�@/@ƨ@��@�@t�@t�@dZ@S�@33@33@o@�@@@@o@o@o@o@"�@o@@@@o@o@�@��@��@��@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B~�B~�B~�B~�B~�B~�B~�B~�B�B�%B�B|�Bv�Bo�BgmB_;BW
BN�BE�B'�B�B��B�^B��B��B��B�hB�PB�7B�B�B~�By�Bv�Bq�Bm�BiyBdZB_;BYBP�BD�B0!B�B
=B%BBBB��B��B�BǮB�B��B��B��B��B�JB�%B�B�B� Bw�BcTBI�B@�B;dB0!B-B+B'�B"�B�BbBB
��B
�B
�B
�B
�B
�yB
�HB
�B
ĜB
�9B
�'B
�B
��B
��B
��B
�{B
�hB
�B
o�B
bNB
^5B
\)B
YB
VB
R�B
O�B
J�B
?}B
0!B
�B
�B

=B	��B	��B	�B	�yB	�B	ŢB	�B	��B	��B	��B	�uB	�\B	�+B	� B	w�B	p�B	n�B	n�B	m�B	l�B	k�B	e`B	ZB	K�B	B�B	?}B	<jB	8RB	5?B	0!B	,B	(�B	$�B	 �B	�B	�B	oB	
=B��B��B��B�B�B�mB�HB�)B�B�B��B��B��BǮBƨBƨBŢBĜBÖB��B��B�wB�jB�^B�LB�?B�9B�9B�9B�9B�9B�3B�3B�'B�B��B��B��B��B��B��B��B��B��B�{B�uB�oB�oB�hB�bB�VB�PB�DB�7B�+B�B�B�B� B}�B|�Bz�Bx�Bv�Bt�Br�Bq�Bp�Bo�Bo�Bn�Bn�Bn�Bm�Bl�Bk�BiyBffBbNB^5B]/B[#B[#BZBYBXBVBT�BS�BQ�BN�BL�BJ�BJ�BJ�BI�BI�BI�BH�BG�BD�BC�BB�B@�B?}B>wB>wB=qB<jB<jB<jB<jB<jB<jB<jB<jB;dB:^B9XB7LB6FB7LB7LB8RB7LB7LB6FB7LB;dB;dB;dB;dB=qB=qB<jB;dB:^B8RB8RB7LB7LB6FB5?B2-B5?B5?B5?B5?B5?B5?B5?B5?B5?B5?B6FB7LB8RB9XB;dB;dB;dB=qB=qB=qB>wBA�BA�BA�BA�BC�BD�BD�BE�BH�BJ�BL�BN�BP�BO�BR�BZB]/B]/B]/B^5B_;B_;B_;B_;B_;B`BB`BBbNBbNBffBiyBiyBhsBgmBffBffBffBe`BiyBs�Bu�Bx�B}�B�B�+B�=B�oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�'B�-B�3B�FB�RB�RB�^B�dB�jB�jB�jB�wBŢBƨBȴBɺBɺBɺB��B��B��B��B�B�
B�B�B�#B�5B�BB�NB�NB�TB�TB�TB�`B�mB�mB�sB�yB�B�B��B��B��B	B	B	B	%B	%B	+B	+B	1B	1B	1B	1B	1B	1B		7B	
=B	DB	JB	PB	\B	hB	�B	�B	�B	�B	�B	 �B	$�B	&�B	&�B	&�B	'�B	)�B	+B	,B	.B	1'B	:^B	=qB	=qB	=qB	=qB	=qB	=qB	>wB	?}B	A�B	B�B	C�B	C�B	D�B	D�B	D�B	E�B	E�B	G�B	H�B	I�B	J�B	K�B	M�B	R�B	T�B	W
B	YB	ZB	[#B	[#B	\)B	\)B	]/B	^5B	`BB	bNB	bNB	ffB	iyB	n�B	~�B	�B	�B	�%B	�+B	�1B	�7B	�=B	�DB	�DB	�DB	�JB	�JB	�JB	�PB	�PB	�VB	�VB	�\B	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�'B	�?B	�RB	�dB	�jB	�wB	B	ĜB	ŢB	ƨB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�#B	�#B	�#B	�#B	�)B	�/B	�/B	�/B	�5B	�5B	�;B	�;B	�;B	�;B	�;B	�;B	�BB	�BB	�`B	�`B	�fB	�fB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
%B
%B
%B
+B
+B
+B
+B
+B
+B
+B
	7B

=B

=B

=B

=B

=B

=B

=B

=B

=B

=B

=B

=B

=B

=B
DB
DB
DB
JB
JB
JB
JB
JB
PB
PB
\B
hB
oB
uB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
 �B
 �B
 �B
 �B
 �B
!�B
!�B
"�B
"�B
#�B
#�B
$�B
$�B
$�B
%�B
%�B
%�B
&�B
&�B
&�B
&�B
&�B
&�B
'�B
'�B
'�B
&�B
'�B
'�B
'�B
(�B
(�B
(�B
(�B
)�B
)�B
)�B
)�B
)�B
+B
+B
,B
-B
.B
/B
/B
0!B
0!B
0!B
1'B
1'B
1'B
1'B
2-B
33B
33B
33B
49B
49B
49B
49B
49B
5?B
5?B
5?B
5?B
6FB
7LB
7LB
7LB
7LB
8RB
9XB
9XB
9XB
9XB
9XB
:^B
:^B
;dB
<jB
<jB
<jB
=qB
>wB
>wB
>wB
>wB
>wB
>wB
>wB
?}B
?}B
?}B
?}B
?}B
?}B
@�B
@�B
@�B
@�B
@�B
B�B
D�B
D�B
E�B
E�B
E�B
F�B
H�B
H�B
H�B
H�B
H�B
I�B
H�B
I�B
J�B
K�B
K�B
K�B
L�B
L�B
M�B
M�B
M�B
N�B
N�B
N�B
O�B
O�B
O�B
O�B
O�B
O�B
P�B
P�B
P�B
P�B
P�B
P�B
Q�B
R�B
S�B
S�B
S�B
S�B
T�B
W
B
W
B
W
B
XB
W
B
XB
YB
YB
YB
YB
YB
YB
[#B
ZB
[#B
[#B
ZB
[#B
[#B
[#B
[#B
[#B
\)B
\)B
\)B
\)B
]/B
]/B
]/B
]/B
]/B
]/B
]/B
^5B
^5B
_;B
_;B
_;B
_;B
_;B
`BB
`BB
`BB
`BB
`BB
`BB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
bNB
bNB
bNB
cTB
cTB
cTB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
e`B
e`B
e`B
ffB
ffB
ffB
ffB
ffB
ffB
ffB
gmB
gmB
gmB
hsB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
jB
k�B
l�B
l�B
m�B
m�B
n�B
n�B
n�B
n�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
o�B
p�B
p�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B~�B~�B~�BBB.B}B�OB��B�1B�aB~�Bx�Bq�BjBa�BZBSBNVB88B��B�9B�{B�QB�B��B�uB�(B�)B�tB��B�UB{JBxRBt9Bo�BlBf�Ba|B[�BTFBIRB4�B�B^B�B�B{B�B iB�.B�B�B�;B��B��B�pB��B�6B�tB�mB��B��B{�Bg�BK^BA�B="B0�B-�B+�B(�B#�B \B�B�B
�0B
�B
�B
��B
�]B
�6B
�:B
�qB
�+B
�B
��B
� B
�B
��B
�eB
�gB
��B
�EB
q�B
c B
^�B
\�B
Y�B
V�B
S�B
P�B
L�B
BAB
3B
!B
QB
�B	�B	��B	��B	�B	�IB	ɆB	�OB	��B	��B	�B	�aB	��B	��B	�UB	x�B	q'B	n�B	n�B	m�B	m]B	m]B	h$B	]dB	M�B	CGB	@iB	=<B	9	B	6`B	1B	,�B	)�B	%�B	!�B	�B	B	�B	�B	 B��B��B�B��B�B��B�IB��B��B�,B�hB��B�B��B��B��B�B�gB�AB�;B�HB�B��B�RB��B�nB�TB�nB�nB�nB�hB��B�GB�AB��B�2B�B�)B�WB��B�_B�$B�mB��B��B��B��B�:B�B�B�VB�JB��B�fB��B��B�MB�;B~�B}�B{dBy�BxRButBsMBr�Bq�BpBo�Bo Bo5BoOBn/Bm)BlWBj�Bh�Be,B_VB^B[�B[�BZ�BY�BX�BV�BU�BU�BS�BQBN�BK�BKBJ�BI�BI�BJ	BI�BIlBGBEBCaBBBAB?�B?.B>BB=qB=B<�B<�B<�B<�B<�B<�B<jB;B:�B9�B8�B8�B8�B9>B8lB8RB7�B8�B<B;�B<6B<�B>B>B=qB<�B:�B8lB8lB7�B7�B7B6�B4�B6`B5�B5�B5�B5�B5�B5�B5�B6+B6zB7LB8B9	B:*B;�B;�B<B=�B=�B>(B?cBA�BA�BBBBABC�BD�BE9BFtBIlBKxBMjBOvBQ�BQ4BT�BZ�B]dB]dB]~B^jB_VB_VB_pB_�B_�B`�BaHBc�Bc�Bg8Bi�Bi�Bh�Bg�Bf�Bf�Bf�BfLBj�BtBv�By�BB��B�B�^B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�4B��B�B�/B�iB�vB�|B��B�zB�lB��B��B��B��B��B��B�BŢB��B��B��B�	B�#B�B�B�.B�&B�9B�$B�+B�eBیBބB�vB�NB�NB�nB�B�B�B�B�B��B��B��B�!B�ZB�6B�BB	;B	GB	B	%B	?B	+B	+B	KB	1B	1B	KB	KB	1B		RB	
XB	^B	~B	�B	�B	�B	
B	�B	�B	�B	�B	!B	$�B	'B	&�B	'B	($B	*B	+B	,WB	.}B	1�B	:�B	=�B	=�B	=�B	=�B	=�B	=qB	>�B	?�B	A�B	B�B	C�B	C�B	D�B	D�B	D�B	E�B	E�B	G�B	H�B	I�B	J�B	K�B	N<B	S&B	U2B	W?B	Y1B	Z7B	[#B	[=B	\CB	\CB	]dB	^jB	`\B	b�B	b�B	f�B	i�B	oOB	B	�;B	�3B	�?B	�+B	�KB	�RB	�XB	�^B	�^B	�^B	�dB	�dB	�JB	�jB	�jB	�pB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�
B	�
B	�B	�*B	�B	�QB	�)B	�5B	�UB	��B	��B	��B	��B	��B	��B	��B	ĶB	ŢB	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	�B	�B	�&B	�FB	�SB	�1B	�7B	�#B	�#B	�=B	�=B	�CB	�IB	�IB	�/B	�OB	�OB	�;B	�!B	�;B	�VB	�VB	�pB	��B	��B	�zB	�zB	�B	�B	�B	�B	�B	��B	�B	�B	�B	�B	�B	�B	��B	�B	�	B	�B	�B	��B	��B	��B	��B	�B	�B	�B	�6B	�<B	�cB
;B
-B
3B
3B
9B
9B
9B
9B
%B
%B
?B
+B
+B
B
EB
EB
+B
_B
	lB

#B

=B

=B

=B

=B

=B

#B

=B

=B

#B

=B

=B

#B

XB
^B
^B
^B
JB
JB
JB
dB
~B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
 �B
 �B
 �B
 �B
 �B
!�B
!�B
"�B
#B
#�B
#�B
$�B
$�B
$�B
%�B
%�B
%�B
&�B
&�B
&�B
&�B
&�B
&�B
'�B
'�B
'�B
&�B
(
B
(
B
'�B
(�B
)B
)B
)B
)�B
*B
*B
*B
*B
+B
+B
,=B
-]B
./B
/5B
/5B
0;B
0;B
0;B
1'B
1AB
1AB
1[B
2GB
3MB
3MB
3MB
4TB
4TB
4TB
4TB
4TB
5?B
5ZB
5ZB
5tB
6FB
7fB
7fB
7LB
7�B
8�B
9rB
9rB
9rB
9rB
9rB
:�B
:�B
;B
<�B
<�B
<�B
=�B
>wB
>wB
>wB
>wB
>wB
>�B
>�B
?�B
?}B
?cB
?�B
?�B
?�B
@�B
@�B
@�B
@�B
AB
B�B
D�B
D�B
E�B
E�B
E�B
F�B
H�B
H�B
H�B
H�B
H�B
I�B
H�B
J	B
J�B
K�B
K�B
K�B
MB
L�B
M�B
M�B
M�B
N�B
N�B
N�B
O�B
O�B
O�B
O�B
O�B
O�B
P�B
P�B
P�B
Q B
Q B
QB
R B
SB
S�B
S�B
S�B
T,B
UMB
W$B
W$B
W$B
XB
W?B
X+B
YB
YB
YB
Y1B
YB
Y1B
[#B
ZB
[#B
[#B
Z7B
[#B
[=B
[=B
[	B
[=B
\CB
\CB
\CB
\CB
]/B
]IB
]IB
]B
]/B
]IB
]IB
^OB
^5B
_;B
_VB
_VB
_;B
_VB
`BB
`\B
`\B
`\B
`BB
`BB
aHB
aHB
aHB
aHB
aHB
abB
abB
bNB
bhB
bhB
cnB
cnB
cTB
dZB
dZB
dZB
dtB
dtB
dtB
dtB
dtB
ezB
e`B
e`B
ffB
ffB
ffB
ffB
ffB
ffB
ffB
g�B
g�B
g�B
hsB
iyB
i_B
iyB
iyB
i_B
iyB
i_B
iyB
iyB
iyB
iyB
iyB
iyB
i_B
iyB
i�B
i�B
j�B
k�B
l�B
l�B
m�B
m�B
n�B
n}B
n�B
n�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
o�B
p�B
qB
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<�@�<2��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=-0.02(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201710110034402017101100344020171011003440202211182132022022111821320220221118213202201804031937312018040319373120180403193731  JA  ARFMdecpA19c                                                                20171001003507  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20170930153527  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20170930153529  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20170930153529  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20170930153530  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20170930153530  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20170930153530  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20170930153530  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20170930153530  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20170930153530                      G�O�G�O�G�O�                JA  ARUP                                                                        20170930155537                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20170930153229  CV  JULD            G�O�G�O�F�P�                JM  ARCAJMQC2.0                                                                 20171010153440  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20171010153440  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180403103731  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200114171535                      G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20221118123202  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20221123111507                      G�O�G�O�G�O�                