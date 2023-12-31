CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2016-12-19T21:35:12Z creation;2016-12-19T21:35:15Z conversion to V3.1;2019-12-19T08:19:20Z update;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7T   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     88   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8X   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8\   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8d   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8h   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8p   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8x   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  IH   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M4   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  px   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  td   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �@   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �,   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ΄   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �0   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �@   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �D   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �H   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �L   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �P   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20161219213512  20200116211516  5905048                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               FA   JA  I2_0577_070                     2C  D   NAVIS_A                         0577                            ARGO 102115                     863 @���]�R�1   @����� @2���e���d��~���1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @9��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B?��BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C5�fC8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D�fDfD� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dhy�Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� Dz��D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ D�|�D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @:�H@���@���A Q�A Q�A@Q�A`Q�A�(�A�(�A�(�A�(�A�(�A�(�A�(�A�(�B {B{B{B{B {B({B0{B8{B?�BH{BP{BX{B`{Bh{Bp{Bx{B�
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
CCCCCCCCCCC C"C$C&C(C*C,C.C0C2C4C5�C8C:C<C>C@CBCDCFCHCJCLCNCPCRCTCVCXCZC\C^C`CbCdCfChCjClCnCpCrCtCvCxCzC|C~C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C���C���C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C���C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D HD �HDHD�HDHD�HDHD�HDHD�HDHD��D�D�HDHD�HDHD�HD	HD	�HD
HD
�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HD�D�HDHD�HDHD�HDHD�HDHD�HDHDz�DHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HD HD �HD!HD!�HD"HD"�HD#HD#�HD$HD$�HD%HD%�HD&HD&�HD'HD'�HD(HD(�HD)HD)�HD*HD*�HD+HD+�HD,HD,�HD-HD-�HD.HD.�HD/HD/�HD0HD0�HD1HD1�HD2HD2�HD3HD3�HD4HD4�HD5HD5�HD6HD6�HD7HD7�HD8HD8�HD9HD9�HD:HD:�HD;HD;�HD<HD<�HD=HD=�HD>HD>�HD?HD?�HD@HD@�HDAHDA�HDBHDB�HDCHDC�HDDHDD�HDEHDE�HDFHDF�HDGHDG�HDHHDH�HDIHDI�HDJHDJ�HDKHDK�HDLHDL�HDMHDM�HDNHDN�HDOHDO�HDPHDP�HDQHDQ�HDRHDR�HDSHDS�HDTHDT�HDUHDU�HDVHDV�HDWHDW�HDXHDX�HDYHDY�HDZHDZ�HD[HD[�HD\HD\�HD]HD]�HD^HD^�HD_HD_�HD`HD`�HDaHDa�HDbHDb�HDcHDc�HDdHDd�HDeHDe�HDfHDf�HDgHDg�HDhHDhz�DiHDi�HDjHDj�HDkHDk�HDlHDl�HDmHDm�HDnHDn�HDoHDo�HDpHDp�HDqHDq�HDrHDr�HDsHDs�HDtHDt�HDuHDu�HDvHDv�HDwHDw�HDxHDx�HDyHDy�HDzHDz�HDz��D{�HD|HD|�HD}HD}�HD~HD~�HDHD�HD� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D�}qD���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D�D���D� �D�@�DÀ�D���D� �D�@�DĀ�D���D� �D�@�Dŀ�D���D� �D�@�Dƀ�D���D� �D�@�Dǀ�D���D� �D�@�DȀ�D���D� �D�@�Dɀ�D���D� �D�@�Dʀ�D���D� �D�@�Dˀ�D���D� �D�@�D̀�D���D� �D�@�D̀�D���D� �D�@�D΀�D���D� �D�@�Dπ�D���D� �D�@�DЀ�D���D� �D�@�Dр�D���D� �D�@�DҀ�D���D� �D�@�DӀ�D���D� �D�@�D�}qD���D� �D�@�DՀ�D���D� �D�@�Dր�D���D� �D�@�D׀�D���D� �D�@�D؀�D���D� �D�@�Dـ�D���D� �D�@�Dڀ�D���D� �D�@�Dۀ�D���D� �D�@�D܀�D���D� �D�@�D݀�D���D� �D�@�Dހ�D���D� �D�@�D߀�D���D� �D�@�D���D���D� �D�@�DဤD���D� �D�@�D․D���D� �D�@�D〤D���D� �D�@�D䀤D���D� �D�@�D値D���D� �D�@�D怤D���D� �D�@�D瀤D���D� �D�@�D耤D���D� �D�@�D逤D���D� �D�@�DꀤD���D� �D�@�D뀤D���D� �D�@�D쀤D���D� �D�@�D퀤D���D� �D�@�DD���D� �D�@�DD�qD� �D�@�D���D���D� �D�@�D�D���D� �D�@�D�D���D� �D�@�D�D���D� �D�@�D�D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A� �A��A��A��A�VA�bA�bA�{A�{A�bA�bA�JA�bA�VA�VA�bA�oA�{A��A��A��A��A��A��A� �A��A� �A�"�A�$�A�"�A�"�A�Q�AӶFA�ƨAӰ!Aӏ\A�VA�JAҶFAҝ�Aҕ�Aҏ\A�/A���A���Aѡ�Aя\AсA�A�AЛ�A�{AϋDA���A�-A�;dA���Aʕ�A�&�A��;Aĩ�A��A�(�A���A��hA�33A�1A�&�A���A���A���A�9XA���A�p�A��A�+A��A��A��^A�O�A�jA���A��
A���A���A�oA�|�A��A�&�A��9A�33A��wA���A�$�A�ƨA�l�A�bNA��DA�1A��/A�O�A���A�G�A���A�ƨA�=qA�(�A���A���A��A��+A�$�A�C�A��A�33At�A~�!A}XA{?}Ay�hAw;dAu��At�Ar��Ap^5AoC�AmS�Aj��Ai��AioAhv�Af��Aet�Ac�Ab�Aa;dA`ȴA_��A^��A\�yA\$�AZn�AX��AWO�AV5?AU+AT�!AS�AQ�AOAMoAL�AK��AKXAJI�AI��AIG�AH�!AH1AGXAF�9AEƨACdZAA�wA@�RA?S�A=hsA<1A;O�A:�A:�A9�FA8�DA7|�A6^5A5O�A4ffA3G�A1��A1��A0��A.  A-XA,A�A+A* �A)�^A)XA)33A(�\A&Q�A$�`A$A#��A#��A"��A"I�A!�A ��A%AdZAȴAM�AA`BA��A��A7LAdZAG�AjA��AjA�AƨA��A�+AA
�yA	�7Av�A��A�wA�A��AbNA1A|�A�A�DA�-A33@��@��@���@�@�&�@�"�@�A�@��@�t�@�ȴ@�5?@�-@���@�j@���@�Q�@�ƨ@���@��@�/@�dZ@�+@�
=@�h@��@�j@� �@�ƨ@�C�@ް!@۝�@��y@�M�@ٙ�@�O�@��@ؼj@ׅ@պ^@�/@��/@�b@��@�v�@�V@�A�@Ͼw@Ώ\@�{@�J@�Ĝ@�9X@ˮ@�dZ@��H@ɲ-@ȃ@� �@ǍP@�o@�ȴ@���@�&�@��`@���@ě�@�z�@�Q�@�A�@ă@�r�@�;d@�G�@�5?@���@�j@���@���@���@��@��@���@��u@��@��H@�~�@�-@��@���@��-@�X@��D@�|�@��@��@��@�9X@��@�  @��w@��@�^5@���@�&�@��/@�9X@�dZ@���@�~�@�^5@�=q@��#@���@�X@���@��`@��/@���@���@��@���@�|�@�S�@�+@�
=@�o@�K�@�|�@��P@�t�@�"�@�$�@���@�hs@�`B@�/@�%@�Ĝ@��@��u@��@�j@���@�S�@���@�$�@�@�@��^@�G�@��D@��@�K�@�33@�
=@���@��\@�^5@�^5@�^5@�V@�M�@��@�{@��@�p�@�V@��T@��+@�v�@�`B@� �@�b@���@��;@��;@��@�t�@�dZ@�K�@��H@���@��@��H@��R@��\@�E�@�5?@�$�@�$�@���@���@�`B@���@�Ĝ@�9X@���@�K�@�
=@��@�~�@�ff@�-@�@��@���@��h@�7L@�%@�%@�A�@�C�@�ȴ@�~�@�ff@�^5@�-@��T@���@�hs@�G�@��@��D@��@��@�j@�  @��F@��P@��P@�S�@��y@���@��+@�M�@�$�@���@���@��7@�&�@���@�j@��@�"�@���@���@��\@�v�@�5?@��@��@��j@���@�1'@���@�l�@�"�@��y@��+@�ff@�M�@�$�@���@���@���@�9X@��
@���@��@�l�@�
=@���@���@�v�@�E�@��#@���@��@�hs@�?}@��j@��@�z�@�j@�A�@�1'@���@��
@��@�l�@�C�@�33@�o@��@��R@���@�5?@��T@��^@�7L@���@��9@��D@�bN@�I�@�@l�@�@~�+@}�@}@}O�@}V@|j@|1@{�m@{�F@{t�@{dZ@{C�@z��@zJ@y�@y��@y��@y�7@yx�@yG�@y&�@y%@x�u@xbN@x1'@x  @w�@vE�@v5?@v{@u�T@up�@u/@tz�@tj@t9X@sƨ@sS�@r��@r�@q�^@qX@q7L@p��@p�9@pbN@o��@o
=@nff@m�@l�j@lZ@k�m@k��@k33@j��@j~�@j^5@jM�@j�@iX@h�@g�w@g�P@gK�@f�@f�+@e��@e�@d9X@c33@b��@bn�@bJ@a�@a�@a��@aG�@`Ĝ@`1'@_�;@_�P@_\)@_�@_+@_�@^��@^��@^�y@^�y@^�@^ȴ@^v�@^E�@^@]��@]`B@]O�@\��@\�@\9X@[�@["�@[@Z�!@Z^5@Y��@X�`@X�9@X�@Xb@W�P@Wl�@W\)@W;d@V��@V�+@V{@U��@T�@TZ@T9X@T1@S�F@S�@St�@S@R��@R-@R-@R�@Q�7@PĜ@PbN@O�;@O�w@O\)@N�+@N$�@M�@M�@M�-@M�h@MO�@L��@L��@Lz�@Lz�@Lj@L(�@K��@K��@K@J��@J~�@I�@I7L@H��@H��@Hr�@G�P@F��@F�@F��@F�+@Fff@F5?@F@E��@E�@EV@Dz�@D(�@C�
@C��@CdZ@Co@B�H@B��@B~�@B=q@BJ@A��@A��@Ahs@A7L@@��@@�u@@A�@@ �@?�;@?�@?l�@?K�@?�@>��@>�@>�R@>��@>ff@>E�@>@=/@<�j@<�@;dZ@;"�@:�\@:-@9��@8Ĝ@8�u@81'@7�P@6ȴ@5��@5O�@4�D@3��@3��@3�@2�@2n�@2=q@1��@1x�@1G�@1�@0��@0�9@0�@0bN@01'@/�@/��@/K�@.�@.ff@.$�@.@-�@-@-�h@-�@,�j@,z�@,(�@+�
@+��@+t�@+S�@+C�@+33@*�@*��@*=q@)�#@)x�@)&�@)%@(��@(�9@(r�@(Q�@'�;@'|�@'K�@';d@'�@'
=@&�@&��@&��@&E�@&{@&@&@%�@%@%�@%O�@%/@%�@$��@$�/@$�@$��@$z�@$Z@$(�@#�
@#S�@#"�@"�@"�\@"~�@"M�@!��@!��@!7L@!�@!�@!�@!�@!%@!%@!%@!�@!%@ �9@ �@ 1'@  �@ b@�@�@�@��@�P@�@��@�y@�@��@v�@ff@E�@��@��@�h@�h@�@`B@`B@O�@/@�@�@�/@��@�j@��@z�@�@1@��@�m@ƨ@�F@��@33@@��@�\@^5@-@�7@�@%@�`@r�@�;@�P@|�@
=@�@��@E�@5?@$�@{@��@O�@�@V@V@�@�/@�/@�@�j@��@�D@Z@(�@��@ƨ@��@�@t�@S�@C�@"�@@�H@�!@�!@��@~�@^5@=q@�@�7@G�@7L@&�@&�@7L@�@Ĝ@�u@�@��@|�@\)@��@ȴ@��@E�@5?@{@�-@O�@�@�j@�j@��@��@z�@(�@�
@�F@�@S�@
��@
M�@
�@
J@
J@	��@	�@	�@	�@	�@	�#@	�#@	�#@	�#@	�#@	��@	��@	�7@	hs@	&�@	%@��@��@�u@Q�@Q�@A�@A�@�@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A� �A��A��A��A�VA�bA�bA�{A�{A�bA�bA�JA�bA�VA�VA�bA�oA�{A��A��A��A��A��A��A� �A��A� �A�"�A�$�A�"�A�"�A�Q�AӶFA�ƨAӰ!Aӏ\A�VA�JAҶFAҝ�Aҕ�Aҏ\A�/A���A���Aѡ�Aя\AсA�A�AЛ�A�{AϋDA���A�-A�;dA���Aʕ�A�&�A��;Aĩ�A��A�(�A���A��hA�33A�1A�&�A���A���A���A�9XA���A�p�A��A�+A��A��A��^A�O�A�jA���A��
A���A���A�oA�|�A��A�&�A��9A�33A��wA���A�$�A�ƨA�l�A�bNA��DA�1A��/A�O�A���A�G�A���A�ƨA�=qA�(�A���A���A��A��+A�$�A�C�A��A�33At�A~�!A}XA{?}Ay�hAw;dAu��At�Ar��Ap^5AoC�AmS�Aj��Ai��AioAhv�Af��Aet�Ac�Ab�Aa;dA`ȴA_��A^��A\�yA\$�AZn�AX��AWO�AV5?AU+AT�!AS�AQ�AOAMoAL�AK��AKXAJI�AI��AIG�AH�!AH1AGXAF�9AEƨACdZAA�wA@�RA?S�A=hsA<1A;O�A:�A:�A9�FA8�DA7|�A6^5A5O�A4ffA3G�A1��A1��A0��A.  A-XA,A�A+A* �A)�^A)XA)33A(�\A&Q�A$�`A$A#��A#��A"��A"I�A!�A ��A%AdZAȴAM�AA`BA��A��A7LAdZAG�AjA��AjA�AƨA��A�+AA
�yA	�7Av�A��A�wA�A��AbNA1A|�A�A�DA�-A33@��@��@���@�@�&�@�"�@�A�@��@�t�@�ȴ@�5?@�-@���@�j@���@�Q�@�ƨ@���@��@�/@�dZ@�+@�
=@�h@��@�j@� �@�ƨ@�C�@ް!@۝�@��y@�M�@ٙ�@�O�@��@ؼj@ׅ@պ^@�/@��/@�b@��@�v�@�V@�A�@Ͼw@Ώ\@�{@�J@�Ĝ@�9X@ˮ@�dZ@��H@ɲ-@ȃ@� �@ǍP@�o@�ȴ@���@�&�@��`@���@ě�@�z�@�Q�@�A�@ă@�r�@�;d@�G�@�5?@���@�j@���@���@���@��@��@���@��u@��@��H@�~�@�-@��@���@��-@�X@��D@�|�@��@��@��@�9X@��@�  @��w@��@�^5@���@�&�@��/@�9X@�dZ@���@�~�@�^5@�=q@��#@���@�X@���@��`@��/@���@���@��@���@�|�@�S�@�+@�
=@�o@�K�@�|�@��P@�t�@�"�@�$�@���@�hs@�`B@�/@�%@�Ĝ@��@��u@��@�j@���@�S�@���@�$�@�@�@��^@�G�@��D@��@�K�@�33@�
=@���@��\@�^5@�^5@�^5@�V@�M�@��@�{@��@�p�@�V@��T@��+@�v�@�`B@� �@�b@���@��;@��;@��@�t�@�dZ@�K�@��H@���@��@��H@��R@��\@�E�@�5?@�$�@�$�@���@���@�`B@���@�Ĝ@�9X@���@�K�@�
=@��@�~�@�ff@�-@�@��@���@��h@�7L@�%@�%@�A�@�C�@�ȴ@�~�@�ff@�^5@�-@��T@���@�hs@�G�@��@��D@��@��@�j@�  @��F@��P@��P@�S�@��y@���@��+@�M�@�$�@���@���@��7@�&�@���@�j@��@�"�@���@���@��\@�v�@�5?@��@��@��j@���@�1'@���@�l�@�"�@��y@��+@�ff@�M�@�$�@���@���@���@�9X@��
@���@��@�l�@�
=@���@���@�v�@�E�@��#@���@��@�hs@�?}@��j@��@�z�@�j@�A�@�1'@���@��
@��@�l�@�C�@�33@�o@��@��R@���@�5?@��T@��^@�7L@���@��9@��D@�bN@�I�@�@l�@�@~�+@}�@}@}O�@}V@|j@|1@{�m@{�F@{t�@{dZ@{C�@z��@zJ@y�@y��@y��@y�7@yx�@yG�@y&�@y%@x�u@xbN@x1'@x  @w�@vE�@v5?@v{@u�T@up�@u/@tz�@tj@t9X@sƨ@sS�@r��@r�@q�^@qX@q7L@p��@p�9@pbN@o��@o
=@nff@m�@l�j@lZ@k�m@k��@k33@j��@j~�@j^5@jM�@j�@iX@h�@g�w@g�P@gK�@f�@f�+@e��@e�@d9X@c33@b��@bn�@bJ@a�@a�@a��@aG�@`Ĝ@`1'@_�;@_�P@_\)@_�@_+@_�@^��@^��@^�y@^�y@^�@^ȴ@^v�@^E�@^@]��@]`B@]O�@\��@\�@\9X@[�@["�@[@Z�!@Z^5@Y��@X�`@X�9@X�@Xb@W�P@Wl�@W\)@W;d@V��@V�+@V{@U��@T�@TZ@T9X@T1@S�F@S�@St�@S@R��@R-@R-@R�@Q�7@PĜ@PbN@O�;@O�w@O\)@N�+@N$�@M�@M�@M�-@M�h@MO�@L��@L��@Lz�@Lz�@Lj@L(�@K��@K��@K@J��@J~�@I�@I7L@H��@H��@Hr�@G�P@F��@F�@F��@F�+@Fff@F5?@F@E��@E�@EV@Dz�@D(�@C�
@C��@CdZ@Co@B�H@B��@B~�@B=q@BJ@A��@A��@Ahs@A7L@@��@@�u@@A�@@ �@?�;@?�@?l�@?K�@?�@>��@>�@>�R@>��@>ff@>E�@>@=/@<�j@<�@;dZ@;"�@:�\@:-@9��@8Ĝ@8�u@81'@7�P@6ȴ@5��@5O�@4�D@3��@3��@3�@2�@2n�@2=q@1��@1x�@1G�@1�@0��@0�9@0�@0bN@01'@/�@/��@/K�@.�@.ff@.$�@.@-�@-@-�h@-�@,�j@,z�@,(�@+�
@+��@+t�@+S�@+C�@+33@*�@*��@*=q@)�#@)x�@)&�@)%@(��@(�9@(r�@(Q�@'�;@'|�@'K�@';d@'�@'
=@&�@&��@&��@&E�@&{@&@&@%�@%@%�@%O�@%/@%�@$��@$�/@$�@$��@$z�@$Z@$(�@#�
@#S�@#"�@"�@"�\@"~�@"M�@!��@!��@!7L@!�@!�@!�@!�@!%@!%@!%@!�@!%@ �9@ �@ 1'@  �@ b@�@�@�@��@�P@�@��@�y@�@��@v�@ff@E�@��@��@�h@�h@�@`B@`B@O�@/@�@�@�/@��@�j@��@z�@�@1@��@�m@ƨ@�F@��@33@@��@�\@^5@-@�7@�@%@�`@r�@�;@�P@|�@
=@�@��@E�@5?@$�@{@��@O�@�@V@V@�@�/@�/@�@�j@��@�D@Z@(�@��@ƨ@��@�@t�@S�@C�@"�@@�H@�!@�!@��@~�@^5@=q@�@�7@G�@7L@&�@&�@7L@�@Ĝ@�u@�@��@|�@\)@��@ȴ@��@E�@5?@{@�-@O�@�@�j@�j@��@��@z�@(�@�
@�F@�@S�@
��@
M�@
�@
J@
J@	��@	�@	�@	�@	�@	�#@	�#@	�#@	�#@	�#@	��@	��@	�7@	hs@	&�@	%@��@��@�u@Q�@Q�@A�@A�@�@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B
ȴB
ɺB
ɺB
ɺB
ȴB
ȴB
ȴB
ȴB
ȴB
ȴB
ȴB
ȴB
ȴB
ȴB
ǮB
ȴB
ȴB
ȴB
ȴB
ȴB
ȴB
ȴB
ȴB
ȴB
ȴB
ǮB
ȴB
ȴB
ȴB
ȴB
ȴB
�B�=B�sB�B��BBoB33BH�B\)BbNBaHBaHBdZBe`BhsBjBo�Bm�BjBl�Bp�Bv�Bz�Bz�B�B�B�+B��B��B��B��B�BÖB��B�
B�/B��B�!B�!B�B�B��B�\B|�B�1B�oB�uB�=Br�Bm�BiyB\)BVB:^B+B��B�`B�B��B�wB�wB�jB�RB�!B��B�Bp�Bm�BjBe`B^5BVBN�BL�BG�B;dB.B+B
�;B
ɺB
�9B
��B
�B
{�B
s�B
aHB
S�B
B�B
2-B
+B
�B
1B
B	�B	�NB	�B	��B	��B	ƨB	�XB	�-B	��B	��B	��B	�{B	�bB	�DB	�\B	�DB	�+B	}�B	w�B	p�B	l�B	hsB	]/B	R�B	C�B	;dB	:^B	7LB	33B	.B	,B	)�B	%�B	!�B	�B	�B	hB	
=B	B��B��B��B��B�B�B�B�B�B�sB�`B�NB�NB�#B�#B�;B�B��B��B��B��B��B��B��B��B��B��BŢBĜBɺBƨB��B��B��BƨB�wB�dB�XB�RB�?B�9B�!B�B��B��B��B��B��B�uB�\B�=B�7B�DB�bB�\B�7B�1B�+B�=B�VB�PB�PB�VB�VB�PB�1B�+B�1B� B�B�1B�7B�7B�%B�1B�DB�oB�{B�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�-B�3B�LB�^B�^B�^B�dB�^B�dB�dB�jB�jB�jB�qB��BĜBŢBǮBǮBɺB��B��B��B�
B�B�B�/B�BB�NB�TB�`B�sB�B��B��B	B	
=B		7B	hB	bB	PB	
=B		7B	
=B	JB	bB	oB	uB	�B	�B	�B	�B	�B	�B	�B	"�B	#�B	#�B	)�B	5?B	:^B	>wB	@�B	A�B	E�B	H�B	I�B	L�B	N�B	O�B	R�B	VB	XB	[#B	^5B	aHB	gmB	iyB	m�B	o�B	p�B	o�B	o�B	p�B	s�B	v�B	w�B	x�B	y�B	|�B	}�B	� B	�B	�B	�B	�B	�B	�B	�B	�B	�1B	�7B	�=B	�JB	�VB	�bB	�oB	�uB	��B	��B	��B	��B	��B	��B	�B	�!B	�!B	�-B	�3B	�FB	�XB	�^B	�wB	��B	��B	��B	B	ĜB	ŢB	ŢB	ŢB	ǮB	��B	��B	�B	��B	��B	��B	��B	�B	�B	�#B	�#B	�B	�B	�B	�)B	�5B	�;B	�BB	�NB	�TB	�ZB	�ZB	�ZB	�`B	�fB	�mB	�sB	�mB	�mB	�sB	�sB	�yB	�yB	�yB	�yB	�sB	�sB	�sB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
%B
%B
+B
+B
+B
+B
	7B

=B

=B

=B
DB

=B

=B

=B

=B
DB
DB
DB
DB
JB
JB
JB
DB
DB
DB
JB
JB
PB
JB
JB
PB
PB
VB
VB
VB
VB
VB
bB
hB
hB
oB
oB
oB
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
!�B
"�B
"�B
#�B
#�B
#�B
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
'�B
'�B
'�B
(�B
(�B
(�B
)�B
)�B
)�B
)�B
)�B
)�B
+B
+B
+B
,B
,B
,B
-B
-B
-B
-B
-B
-B
-B
-B
-B
-B
-B
-B
-B
-B
-B
-B
.B
.B
/B
/B
/B
/B
0!B
0!B
0!B
0!B
1'B
1'B
1'B
1'B
2-B
2-B
33B
33B
33B
33B
49B
49B
5?B
49B
5?B
5?B
5?B
49B
49B
49B
49B
49B
49B
49B
49B
5?B
6FB
6FB
6FB
7LB
7LB
8RB
8RB
9XB
9XB
9XB
:^B
:^B
;dB
;dB
;dB
;dB
<jB
<jB
=qB
=qB
=qB
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
@�B
@�B
A�B
A�B
A�B
A�B
B�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
E�B
E�B
E�B
E�B
E�B
E�B
F�B
F�B
F�B
F�B
F�B
G�B
G�B
G�B
G�B
G�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
K�B
K�B
L�B
M�B
M�B
M�B
M�B
N�B
N�B
O�B
O�B
O�B
O�B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
S�B
S�B
S�B
S�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
VB
W
B
W
B
W
B
W
B
W
B
XB
XB
XB
YB
YB
YB
YB
ZB
ZB
ZB
ZB
ZB
[#B
[#B
\)B
\)B
]/B
]/B
]/B
]/B
]/B
^5B
]/B
^5B
_;B
_;B
_;B
_;B
_;B
_;B
`BB
`BB
`BB
aHB
aHB
aHB
aHB
aHB
bNB
cTB
cTB
dZB
dZB
dZB
dZB
dZB
e`B
e`B
e`B
e`B
ffB
ffB
ffB
ffB
ffB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
hsB
gmB
gmB
gmB
gmB
hsB
hsB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
jB
jB
jB
jB
jB
jB
jB
jB
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
n�B
n�B
n�B
n�B
n�B
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
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
r�B
r�B
r�B
s�B
s�B
s�B
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
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
v�B
w�B
w�B
w�B
w�B
w�B
x�B
x�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
z�B
z�B
z�B
{�B
{�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
}�B
}�B
}�B
}�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
� B
� 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B
��B
ɺB
ɺB
��B
ȴB
ȴB
ȴB
ȴB
ȴB
ȴB
ȴB
ȴB
ȴB
ȴB
ǮB
ȴB
ȴB
ȴB
ȴB
ȴB
ȴB
ȴB
ȴB
ȴB
ȴB
ǮB
ȴB
ȴB
ȴB
ȚB
�KB
��B��B��B�B�XB�B&B3MBH�B\CBb�Ba�Ba�Bd�Be�Bh�BkQBp�Bn�BlBn�Bs�Bx�B|6B~B�gB��B��B�VB��B��B�kB�3BżBB�_B�vB��B�9B��B��B��B�B��B~�B��B��B��B��Bt�BpoBl"B_�BY�B=qB	�B��B�RB�B� B� B�}B��B��B��B�vB�7Br-BoBk�BgB`BW$BO�BN�BJ#B>]B3hBB
��B
�6B
�B
��B
��B
}�B
v+B
c�B
V�B
DgB
3�B
-�B
B

	B
{B	�?B	��B	��B	�B	��B	ȚB	�dB	�B	��B	��B	�B	��B	��B	��B	�hB	�6B	�B	cB	y	B	q�B	nB	kB	`B	U�B	D�B	;�B	;JB	8�B	4B	.�B	,�B	*�B	&�B	"�B	pB	jB	�B	�B	B	;B�B��B��B�hB�B�'B�B�B��B�B��B��B��B��B�4B�7BՁBևB��B�vB�pB�pB�BB�vB̈́B��B�?B�B��B��B�B��B��BȀB�cB�B��B�XB�`B�ZB��B��B�0B��B��B��B�B�B��B�0B��B��B�B��B��B��B��B�dB�(B��B�"B�(B�(B��B�RB�B��B� B��B�B��B��B�tB��B��B��B��B�2B�SB�$B�mB�#B�OB�)B��B��B�B�5B��B�'B��B�@B� B�nB��B��B�\B�;B�'B�>B�iB��B�9B�RB��B��B�B�B��B�PB��B��B�"B��B��B�[B�B�B��B�KBʦBϑB�@B�gB�YB�_BٴBݘB�vB�hB�B�B�B�B��B�cB	B	B		B	B	NB	pB	
�B		�B	
�B	0B	bB	�B	,B	#B	�B	�B	�B	�B	�B	 'B	#nB	$�B	$ZB	*�B	5�B	:�B	>�B	@�B	A�B	F?B	IB	J=B	M6B	OB	PHB	S�B	V�B	X+B	[=B	^jB	a�B	g�B	i�B	m�B	o�B	p�B	o�B	o�B	qB	s�B	v�B	w�B	x�B	y�B	|�B	}�B	�B	�'B	�MB	��B	��B	�aB	�AB	�-B	�SB	�KB	�lB	�rB	�JB	�pB	��B	��B	��B	�B	��B	��B	��B	�:B	�eB	��B	��B	�UB	�GB	�hB	�zB	�rB	��B	�wB	�iB	��B	��B	��B	ĜB	��B	��B	żB	�EB	ΊB	�MB	֡B	յB	�B	�,B	��B	�+B	�KB	�#B	�=B	�QB	�QB	�7B	�)B	�5B	�VB	�vB	�B	�nB	�tB	�tB	�tB	�B	�B	�B	�B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	�B	�5B	�5B	��B	��B	�B	�B	�B	��B	��B	��B	��B	�B	��B	��B	��B	��B	�B	�B	��B	�B	�B	�6B	�"B	�(B	�(B	�B	�.B
 4B
 OB
UB
[B
aB
{B
�B
YB
?B
EB
_B
zB
�B
	�B

rB

rB

�B
�B

rB

rB

rB

�B
DB
^B
^B
�B
�B
�B
�B
�B
xB
^B
dB
�B
jB
~B
�B
�B
�B
�B
pB
VB
�B
�B
}B
�B
�B
�B
oB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!B
!�B
!�B
!�B
!�B
!�B
"�B
#B
#�B
#�B
$B
%B
%,B
&B
&B
%�B
'B
'B
'B
'B
'B
(>B
(>B
($B
)DB
)*B
)*B
*B
*0B
*B
*B
*B
*B
+6B
+QB
+QB
,=B
,"B
,"B
-CB
-CB
-]B
-CB
-]B
-]B
-CB
-)B
-)B
-)B
-B
-)B
-CB
-)B
-)B
-B
./B
./B
/5B
/B
/ B
/5B
0!B
0!B
0!B
0;B
1AB
1'B
1AB
1AB
2GB
2GB
33B
3MB
3MB
3hB
4nB
4nB
5ZB
4TB
5ZB
5tB
5�B
4TB
4TB
4nB
4TB
49B
4TB
4TB
4TB
5tB
6zB
6zB
6�B
7fB
7fB
8lB
8lB
9rB
9rB
9rB
:xB
:�B
;JB
;B
;�B
;�B
<�B
<�B
=�B
=�B
=�B
>wB
?}B
?cB
?�B
?�B
?�B
?�B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
A�B
A�B
A�B
A�B
B�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
E�B
E�B
E�B
E�B
E�B
E�B
F�B
F�B
F�B
F�B
F�B
G�B
G�B
G�B
G�B
G�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
KB
K�B
LB
MB
M�B
NB
NB
NB
O(B
N�B
PB
P.B
P.B
P.B
QB
Q4B
R B
RB
Q�B
R B
S&B
SB
S&B
S�B
TB
TB
TB
T�B
UB
UB
UB
UB
UB
U2B
V9B
W$B
W$B
W$B
W$B
W$B
X+B
XEB
XEB
Y1B
Y1B
YB
Y1B
Z7B
Z7B
ZB
ZB
Z7B
[#B
[=B
\CB
\CB
]IB
]IB
]IB
]IB
]IB
^OB
]dB
^OB
_VB
_;B
_VB
_VB
_VB
_VB
`\B
`BB
`\B
aHB
aHB
abB
aHB
aHB
bhB
cTB
cnB
dtB
dZB
dtB
dZB
dtB
ezB
ezB
ezB
e�B
f�B
f�B
f�B
f�B
ffB
g�B
g�B
g�B
gmB
gRB
gmB
gmB
gmB
hsB
gmB
gmB
g�B
gmB
h�B
h�B
iyB
iyB
iyB
i_B
iyB
i�B
iyB
i�B
j�B
jB
j�B
j�B
j�B
j�B
j�B
j�B
k�B
k�B
kkB
k�B
k�B
k�B
k�B
k�B
l�B
l�B
l�B
l�B
lqB
l�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
n�B
n�B
n�B
n�B
n�B
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
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
r�B
r�B
r�B
s�B
s�B
s�B
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
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
v�B
w�B
w�B
w�B
w�B
xB
x�B
x�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
z�B
z�B
z�B
|B
|B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
}B
~B
}�B
~B
}�B
~�B
~�B
B
~�B
B
~�B
~�B
~�B
B
� B
�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=-0.02(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201612240034192016122400341920161224003419201806221306392018062213063920180622130639201804050706592018040507065920180405070659  JA  ARFMdecpA19c                                                                20161220063505  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20161219213512  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20161219213513  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20161219213513  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20161219213514  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20161219213514  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20161219213514  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20161219213514  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20161219213514  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20161219213515                      G�O�G�O�G�O�                JA  ARUP                                                                        20161219224308                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20161220153600  CV  JULD            G�O�G�O�F��                JM  ARCAJMQC2.0                                                                 20161223153419  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20161223153419  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404220659  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622040639  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200116211516                      G�O�G�O�G�O�                