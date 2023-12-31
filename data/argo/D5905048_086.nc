CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2017-02-06T00:35:34Z creation;2017-02-06T00:35:37Z conversion to V3.1;2019-12-19T08:15:40Z update;     
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
_FillValue                 �  ID   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M0   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  pl   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tX   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �(   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �d   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ޜ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �    HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �$   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �(   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �,   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �l   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �|   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20170206003534  20200116211516  5905048                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               VA   JA  I2_0577_086                     2C  D   NAVIS_A                         0577                            ARGO 102115                     863 @���A 1   @���O���@4�8�YK�d�&�x��1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @9��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(ffB0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D
��D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  Dy�D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9y�D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D]��D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�3D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�P D�` 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @:�H@���@���A Q�A Q�A@Q�A`Q�A�(�A�(�A�(�A�(�A�(�A�(�A�(�A�(�B {B{B{B{B {B(z�B0{B8{B@{BH{BP{BX{B`{Bh{Bp{Bx{B�
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
CCCCCCCCCCC C"C$C&C(C*C,C.C0C2C4C6C8C:C<C>C@CBCDCFCHCJCLCNCPCRCTCVCXCZC\C^C`CbCdCfChCjClCnCpCrCtCvCxCzC|C~C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�\C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C���C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D HD �HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HD	HD	�HD
HD
�HD
��D�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD��DHD�HDHDz�DHD�HD HD �HD!HD!�HD"HD"�HD#HD#�HD$HD$�HD%HD%�HD&HD&�HD'HD'�HD(HD(�HD)HD)�HD*HD*�HD+HD+�HD,HD,�HD-HD-�HD.HD.�HD/HD/�HD0HD0�HD1HD1�HD2HD2�HD3HD3�HD4HD4�HD5HD5�HD6HD6�HD7HD7�HD8HD8�HD9HD9z�D:HD:�HD;HD;�HD<HD<�HD=HD=�HD>HD>�HD?HD?�HD@HD@�HDAHDA�HDBHDB�HDCHDC�HDDHDD�HDEHDE�HDFHDF�HDGHDG�HDHHDH�HDIHDI�HDJHDJ�HDKHDK�HDLHDL�HDMHDM�HDNHDN�HDOHDO�HDPHDP�HDQHDQ�HDRHDR�HDSHDS�HDTHDT�HDUHDU�HDVHDV�HDWHDW�HDXHDX�HDYHDY�HDZHDZ�HD[HD[�HD\HD\�HD]HD]�HD]��D^�HD_HD_�HD`HD`�HDaHDa�HDbHDb�HDcHDc�HDdHDd�HDeHDe�HDfHDf�HDgHDg�HDhHDh�HDiHDi�HDjHDj�HDkHDk�HDlHDl�HDmHDm�HDnHDn�HDoHDo�HDpHDp�HDqHDq�HDrHDr�HDsHDs�HDtHDt�HDuHDu�HDvHDv�HDwHDw�HDxHDx�HDyHDy�HDzHDz�HD{HD{�HD|HD|�HD}HD}�HD~HD~�HDHD�HD� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D�}qD���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D��D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D��D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D�D���D� �D�@�DÀ�D���D� �D�@�DĀ�D���D� �D�@�Dŀ�D���D� �D�@�Dƀ�D���D� �D�@�Dǀ�D���D� �D�@�DȀ�D���D� �D�@�Dɀ�D���D� �D�@�Dʀ�D���D� �D�@�Dˀ�D���D� �D�@�D̀�D���D� �D�@�D̀�D���D� �D�@�D΀�D���D� �D�@�Dπ�D���D� �D�@�DЀ�D���D� �D�@�Dр�D���D� �D�@�DҀ�D���D� �D�@�DӀ�D���D� �D�@�DԀ�D���D� �D�@�DՀ�D���D� �D�@�Dր�D���D� �D�@�D׀�D���D� �D�@�D؀�D���D� �D�@�Dـ�D���D� �D�@�Dڀ�D���D� �D�@�Dۀ�D���D� �D�@�D܀�D���D� �D�@�D݀�D���D� �D�@�Dހ�D���D� �D�@�D߀�D���D� �D�@�D���D���D� �D�@�DဤD���D� �D�@�D․D���D� �D�@�D〤D���D� �D�@�D䀤D���D� �D�@�D値D���D� �D�@�D怤D���D� �D�@�D瀤D���D� �D�@�D耤D���D� �D�@�D逤D���D� �D�@�DꀤD���D� �D�@�D��D���D� �D�@�D쀤D���D� �D�@�D퀤D���D� �D�@�DD���D� �D�@�DD���D� �D�@�D���D���D� �D�@�D�D���D� �D�@�D�D���D� �D�@�D�D���D� �D�@�D�D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�P�D�`�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�VA�oA�{A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A�"�A�$�A�$�A�&�A�(�A�+A�-A�1'A�A�A�7LA��#A�ȴAɸRAə�AɑhAɃA�l�A�hsA�t�A�;dA�5?A�=qA�1'A� �A�(�A�;dA�K�A�O�A�Q�A�(�A���A�r�A�M�A�1A��;A�"�A�n�A���A��A���A�?}A�ƨA�A��+A��A��A���A��mA�=qA�%A��HA�7LA�ȴA�?}A�1'A���A��A�
=A��+A�33A�ffA�{A�%A��A��A���A��RA��A���A���A�C�A�A�A�A�VA��yA��`A���A�ƨA���A�n�A�XA���A��A���A�M�A�33A��A�;dA��A�^5A�  A���A��\A�O�A�p�A��A�9XA��uA��A�(�A�;A��A;dA~�\A|z�A{��Azv�Aw�At�Ap��An�uAm��Am"�Al�RAk�
Akl�Aj�jAghsAc�;AcAa��Aa33A`�9A`5?A_�A_��A_+A^ffA[�^AX=qAVJAS��AS/AP�AN�\AMK�AL  AJ��AHffAE��AB5?A?A>��A=�A;�A;7LA:�\A9t�A6�uA5"�A4=qA3��A3K�A1l�A0�A/33A-�
A*�yA)�PA(�\A'�wA&��A&9XA%�^A%&�A#�A#A"�A!l�A!\)A ��A �AXA��AVA�#AXA�`Ar�A^5AO�A�;A��A5?AAx�A/A��AbAx�AZA|�A%AZAAM�A�^A��A7LA
��A
�A
I�A
A	�A��AƨAhsA��A��A�AXAffA\)A�RAA�AhsA33A/AS�A�A �`A ȴA �uA 5?A {@�+@�7L@�Z@�{@���@��@�=q@�t�@�!@�ff@�{@�h@�C�@�@��/@�t�@�V@�h@���@��@�I�@�5?@�z�@ߍP@�o@ޗ�@��@ݑh@܋D@�"�@�{@�hs@��`@�1'@ץ�@�C�@�M�@ԃ@҇+@�?}@Ѓ@�r�@�r�@�j@�9X@�|�@�n�@�hs@�/@��@̓u@�ƨ@�K�@��@�;d@�l�@�\)@˅@�l�@��@�\)@ʸR@�J@�G�@�/@�Z@�ff@���@��`@�I�@��T@���@���@���@���@�n�@�5?@�J@�@�G�@��`@�r�@�1@�ȴ@��h@�`B@��@�r�@�\)@���@�7L@�v�@��D@�Z@�(�@�S�@���@�O�@���@�1'@��;@�|�@�
=@���@��\@�=q@���@�7L@���@���@�Ĝ@���@�j@�b@��R@�-@�E�@���@��h@�x�@�/@���@���@���@��/@��@�;d@��@���@��@���@��7@�O�@�&�@�V@�V@��@���@�Ĝ@��u@��@��;@�  @�1'@�A�@�Q�@�bN@�r�@�bN@�9X@�(�@��@�  @�;d@��@���@�p�@��9@��@���@���@�n�@���@��@��/@��9@��9@��9@��@�r�@��@��w@�;d@��H@���@��+@�V@��@�@��@��@��T@���@��-@��-@���@���@���@��7@��7@�?}@�V@��/@�r�@�9X@� �@�b@�1@�  @�  @��m@��w@��F@���@��P@�dZ@�
=@���@�E�@��@��@�@�O�@���@��@��@��/@�Ĝ@���@�r�@�Q�@�1'@��;@�ƨ@���@��@�K�@�o@��!@�n�@�ff@�-@��@���@���@��-@�`B@��@���@��`@�Ĝ@��u@�Q�@�1'@���@��;@��F@�\)@�
=@���@�^5@�=q@�{@���@�p�@�z�@� �@��@�1@��m@�ƨ@���@�\)@��y@��!@���@�n�@���@���@��@���@���@��u@�9X@��@��@�ƨ@���@�C�@�ff@�{@��T@��#@���@��h@�X@�&�@���@��@���@�j@�A�@�  @�w@K�@~�y@~$�@|��@|�D@{��@{o@{@z��@z^5@y�@y��@y��@y�7@y�7@xĜ@w�@w+@v�y@v�R@v��@vff@vE�@v@u�@tZ@st�@r��@rn�@rM�@r-@q��@q�@q�#@qx�@p�`@p��@pQ�@p �@pb@o;d@o
=@n��@nv�@mV@lI�@l1@k�
@k��@kt�@j�@j~�@j~�@j^5@j-@i�#@i��@i&�@h�`@h�`@h�`@h��@h��@hĜ@h�9@h�@hr�@hr�@hbN@h1'@hb@h  @g��@g\)@g�@fff@f{@ep�@d�D@dI�@c��@cS�@c@b�!@b~�@bn�@b^5@bM�@b=q@b-@a��@a�7@a�@`��@`A�@_\)@^ȴ@^v�@^E�@]��@]p�@\��@\�/@\�/@\��@\z�@[��@[ƨ@[�@[@Zn�@Y�^@YG�@Y%@X�9@X�@XbN@XQ�@W�;@W�@V��@Vv�@V$�@U�@U�T@U��@U��@T�@S�@S"�@R�!@R=q@Q��@Q�7@Q7L@P��@P�u@O|�@N�y@Nȴ@N�R@N�R@N�R@N��@M�@L�@Ko@J��@J�!@J�\@J~�@Jn�@JM�@J=q@J-@I��@I%@G�P@Fv�@F5?@F@E��@EO�@D�@D9X@B�\@A�@Ax�@@�`@@�`@@��@@��@@��@@�`@@��@@�9@@��@@�@@bN@?��@>�y@>��@>�+@>v�@>v�@>v�@>ff@>v�@>ff@>v�@>V@>V@>V@>V@>E�@=@=�@=O�@=/@=�@<��@<�D@;�m@;��@;dZ@:�@:��@:��@:��@:�!@:�\@:^5@:=q@:�@:J@:J@:J@9��@:J@:J@9�#@9��@9G�@8�`@8A�@7|�@7+@6ȴ@6E�@6@5��@5�h@5`B@5�@4�j@4�D@4j@4Z@41@3�F@3�@3dZ@3S�@2��@2�@1�@1��@1��@1X@1&�@0��@0��@0��@0�9@0A�@/l�@/
=@.��@.�@.�+@.{@-�@-�@-�@,(�@+ƨ@+S�@*��@*��@*~�@*^5@*�@)��@)��@)��@)x�@)�7@)�7@)hs@)�@(�`@(Ĝ@(��@(�u@(�@( �@'�w@'|�@'+@'
=@&�R@&v�@&E�@&{@%�T@%�-@$�@$�@$�@$��@$z�@$I�@$�@#��@#��@#dZ@#C�@#"�@"��@"��@"��@"�\@"M�@"^5@"M�@"M�@"=q@"�@!�@!��@!�^@!��@!�7@!�7@!x�@!hs@!X@!&�@ ��@ �u@ bN@   @|�@K�@+@�@v�@@�@p�@O�@?}@�@�@��@�j@��@j@I�@9X@(�@��@��@t�@�@t�@dZ@33@o@@�@�!@-@�@��@��@X@�@��@�`@�u@b@|�@�@ȴ@v�@5?@@�@�@�T@@�-@�@�@�/@��@�D@j@(�@ƨ@��@S�@o@��@��@��@^5@=q@�@��@�#@��@��@��@x�@�`@Ĝ@��@�@bN@ �@�@|�@l�@\)@K�@+@��@�@�R@�R@�R@��@��@��@v�@V@E�@{@�@�T@��@@�-@��@�@�j@I�@1@�@C�@C�@33@@
��@
��@
��@
�!@
�!@
��@
n�@
^5@
=q@	�@	��@	��@	��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�VA�oA�{A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A�"�A�$�A�$�A�&�A�(�A�+A�-A�1'A�A�A�7LA��#A�ȴAɸRAə�AɑhAɃA�l�A�hsA�t�A�;dA�5?A�=qA�1'A� �A�(�A�;dA�K�A�O�A�Q�A�(�A���A�r�A�M�A�1A��;A�"�A�n�A���A��A���A�?}A�ƨA�A��+A��A��A���A��mA�=qA�%A��HA�7LA�ȴA�?}A�1'A���A��A�
=A��+A�33A�ffA�{A�%A��A��A���A��RA��A���A���A�C�A�A�A�A�VA��yA��`A���A�ƨA���A�n�A�XA���A��A���A�M�A�33A��A�;dA��A�^5A�  A���A��\A�O�A�p�A��A�9XA��uA��A�(�A�;A��A;dA~�\A|z�A{��Azv�Aw�At�Ap��An�uAm��Am"�Al�RAk�
Akl�Aj�jAghsAc�;AcAa��Aa33A`�9A`5?A_�A_��A_+A^ffA[�^AX=qAVJAS��AS/AP�AN�\AMK�AL  AJ��AHffAE��AB5?A?A>��A=�A;�A;7LA:�\A9t�A6�uA5"�A4=qA3��A3K�A1l�A0�A/33A-�
A*�yA)�PA(�\A'�wA&��A&9XA%�^A%&�A#�A#A"�A!l�A!\)A ��A �AXA��AVA�#AXA�`Ar�A^5AO�A�;A��A5?AAx�A/A��AbAx�AZA|�A%AZAAM�A�^A��A7LA
��A
�A
I�A
A	�A��AƨAhsA��A��A�AXAffA\)A�RAA�AhsA33A/AS�A�A �`A ȴA �uA 5?A {@�+@�7L@�Z@�{@���@��@�=q@�t�@�!@�ff@�{@�h@�C�@�@��/@�t�@�V@�h@���@��@�I�@�5?@�z�@ߍP@�o@ޗ�@��@ݑh@܋D@�"�@�{@�hs@��`@�1'@ץ�@�C�@�M�@ԃ@҇+@�?}@Ѓ@�r�@�r�@�j@�9X@�|�@�n�@�hs@�/@��@̓u@�ƨ@�K�@��@�;d@�l�@�\)@˅@�l�@��@�\)@ʸR@�J@�G�@�/@�Z@�ff@���@��`@�I�@��T@���@���@���@���@�n�@�5?@�J@�@�G�@��`@�r�@�1@�ȴ@��h@�`B@��@�r�@�\)@���@�7L@�v�@��D@�Z@�(�@�S�@���@�O�@���@�1'@��;@�|�@�
=@���@��\@�=q@���@�7L@���@���@�Ĝ@���@�j@�b@��R@�-@�E�@���@��h@�x�@�/@���@���@���@��/@��@�;d@��@���@��@���@��7@�O�@�&�@�V@�V@��@���@�Ĝ@��u@��@��;@�  @�1'@�A�@�Q�@�bN@�r�@�bN@�9X@�(�@��@�  @�;d@��@���@�p�@��9@��@���@���@�n�@���@��@��/@��9@��9@��9@��@�r�@��@��w@�;d@��H@���@��+@�V@��@�@��@��@��T@���@��-@��-@���@���@���@��7@��7@�?}@�V@��/@�r�@�9X@� �@�b@�1@�  @�  @��m@��w@��F@���@��P@�dZ@�
=@���@�E�@��@��@�@�O�@���@��@��@��/@�Ĝ@���@�r�@�Q�@�1'@��;@�ƨ@���@��@�K�@�o@��!@�n�@�ff@�-@��@���@���@��-@�`B@��@���@��`@�Ĝ@��u@�Q�@�1'@���@��;@��F@�\)@�
=@���@�^5@�=q@�{@���@�p�@�z�@� �@��@�1@��m@�ƨ@���@�\)@��y@��!@���@�n�@���@���@��@���@���@��u@�9X@��@��@�ƨ@���@�C�@�ff@�{@��T@��#@���@��h@�X@�&�@���@��@���@�j@�A�@�  @�w@K�@~�y@~$�@|��@|�D@{��@{o@{@z��@z^5@y�@y��@y��@y�7@y�7@xĜ@w�@w+@v�y@v�R@v��@vff@vE�@v@u�@tZ@st�@r��@rn�@rM�@r-@q��@q�@q�#@qx�@p�`@p��@pQ�@p �@pb@o;d@o
=@n��@nv�@mV@lI�@l1@k�
@k��@kt�@j�@j~�@j~�@j^5@j-@i�#@i��@i&�@h�`@h�`@h�`@h��@h��@hĜ@h�9@h�@hr�@hr�@hbN@h1'@hb@h  @g��@g\)@g�@fff@f{@ep�@d�D@dI�@c��@cS�@c@b�!@b~�@bn�@b^5@bM�@b=q@b-@a��@a�7@a�@`��@`A�@_\)@^ȴ@^v�@^E�@]��@]p�@\��@\�/@\�/@\��@\z�@[��@[ƨ@[�@[@Zn�@Y�^@YG�@Y%@X�9@X�@XbN@XQ�@W�;@W�@V��@Vv�@V$�@U�@U�T@U��@U��@T�@S�@S"�@R�!@R=q@Q��@Q�7@Q7L@P��@P�u@O|�@N�y@Nȴ@N�R@N�R@N�R@N��@M�@L�@Ko@J��@J�!@J�\@J~�@Jn�@JM�@J=q@J-@I��@I%@G�P@Fv�@F5?@F@E��@EO�@D�@D9X@B�\@A�@Ax�@@�`@@�`@@��@@��@@��@@�`@@��@@�9@@��@@�@@bN@?��@>�y@>��@>�+@>v�@>v�@>v�@>ff@>v�@>ff@>v�@>V@>V@>V@>V@>E�@=@=�@=O�@=/@=�@<��@<�D@;�m@;��@;dZ@:�@:��@:��@:��@:�!@:�\@:^5@:=q@:�@:J@:J@:J@9��@:J@:J@9�#@9��@9G�@8�`@8A�@7|�@7+@6ȴ@6E�@6@5��@5�h@5`B@5�@4�j@4�D@4j@4Z@41@3�F@3�@3dZ@3S�@2��@2�@1�@1��@1��@1X@1&�@0��@0��@0��@0�9@0A�@/l�@/
=@.��@.�@.�+@.{@-�@-�@-�@,(�@+ƨ@+S�@*��@*��@*~�@*^5@*�@)��@)��@)��@)x�@)�7@)�7@)hs@)�@(�`@(Ĝ@(��@(�u@(�@( �@'�w@'|�@'+@'
=@&�R@&v�@&E�@&{@%�T@%�-@$�@$�@$�@$��@$z�@$I�@$�@#��@#��@#dZ@#C�@#"�@"��@"��@"��@"�\@"M�@"^5@"M�@"M�@"=q@"�@!�@!��@!�^@!��@!�7@!�7@!x�@!hs@!X@!&�@ ��@ �u@ bN@   @|�@K�@+@�@v�@@�@p�@O�@?}@�@�@��@�j@��@j@I�@9X@(�@��@��@t�@�@t�@dZ@33@o@@�@�!@-@�@��@��@X@�@��@�`@�u@b@|�@�@ȴ@v�@5?@@�@�@�T@@�-@�@�@�/@��@�D@j@(�@ƨ@��@S�@o@��@��@��@^5@=q@�@��@�#@��@��@��@x�@�`@Ĝ@��@�@bN@ �@�@|�@l�@\)@K�@+@��@�@�R@�R@�R@��@��@��@v�@V@E�@{@�@�T@��@@�-@��@�@�j@I�@1@�@C�@C�@33@@
��@
��@
��@
�!@
�!@
��@
n�@
^5@
=q@	�@	��@	��@	��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  Bu�Bt�Bu�Bu�Bu�Bu�Bu�Bt�Bu�Bu�Bt�Bt�Bt�Bt�Bt�Bt�Bu�Bu�Bu�Bu�Bv�Bv�Bx�Bv�Bv�Bz�B{�B{�B}�B�B�B�B�1B��B��B�-B�3B�-B�!B�!B�B�B�B�qBƨB�#B�`B�yB�B��B
=BhBhBhB+BI�BaHBffBu�B�B�%B�JB��B��B��B��B��B��B��B��B��B��B��B�B�B�B��B��B�B�B�B��B��B��B��B�bB�=B� Bs�Bp�Bl�B`BBC�B+B'�B �B�BoB
=B��B�mB��BȴB�dB�B��B��B�bB�7B�Br�BdZBT�BC�B+B"�B�BVB
�B
�B
ƨB
�XB
�B
��B
��B
��B
��B
�uB
�\B
�B
{�B
r�B
_;B
J�B
2-B
�B
�B
{B
hB
DB
+B
B	�B	�B	��B	ɺB	ĜB	��B	�qB	�dB	�XB	�?B	�!B	��B	�+B	x�B	ffB	aHB	W
B	G�B	A�B	:^B	33B	)�B	�B	hB	B��B��B�B�B�sB�NB�B��B��BȴBǮBB�qB�^B�?B�B��B��B��B��B��B��B��B��B��B�{B�oB�hB�hB�hB�bB�7B�+B�JB�bB�hB�uB�uB��B��B�oB�uB�uB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�B�B�B�B�9B�'B�B�'B�-B�XB�XB�dBÖB��B��B��B�B�/B�5B�TB�`B�ZB�`B�TB�NB�HB�HB�TB�TB�TB�TB�NB�BB�;B�/B�)B�#B�B�B��B�B�B�B�B�#B�)B�)B�;B�ZB�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B	B	B	%B	DB	JB	JB	PB	hB	�B	�B	�B	"�B	!�B	'�B	)�B	'�B	&�B	&�B	%�B	 �B	�B	 �B	 �B	�B	�B	{B	�B	�B	�B	�B	�B	�B	�B	!�B	&�B	,B	0!B	0!B	33B	7LB	>wB	A�B	@�B	@�B	<jB	9XB	:^B	:^B	:^B	:^B	=qB	?}B	A�B	B�B	E�B	H�B	L�B	N�B	Q�B	YB	\)B	]/B	^5B	^5B	`BB	aHB	cTB	m�B	p�B	q�B	u�B	w�B	y�B	{�B	{�B	{�B	{�B	{�B	|�B	|�B	}�B	� B	�B	�B	�+B	�1B	�=B	�=B	�=B	�JB	�PB	�\B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�'B	�!B	�B	�B	�B	�B	�B	�B	�B	�!B	�B	�!B	�'B	�'B	�-B	�9B	�9B	�?B	�FB	�RB	�^B	�jB	��B	B	ĜB	ƨB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�/B	�/B	�5B	�;B	�;B	�;B	�;B	�;B	�;B	�;B	�BB	�NB	�NB	�ZB	�fB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
B
B
%B
%B
B
B
B
B
B
B
+B
1B
1B
1B
+B
+B
1B
	7B
	7B

=B
DB
JB
JB
JB
PB
PB
PB
VB
VB
VB
\B
bB
hB
oB
oB
uB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
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
$�B
$�B
&�B
&�B
'�B
'�B
'�B
'�B
(�B
(�B
(�B
(�B
)�B
)�B
+B
+B
+B
+B
+B
+B
+B
,B
,B
,B
,B
,B
,B
,B
,B
,B
,B
-B
-B
-B
-B
.B
/B
/B
/B
/B
/B
0!B
0!B
0!B
0!B
0!B
0!B
0!B
0!B
0!B
0!B
0!B
1'B
1'B
2-B
2-B
2-B
2-B
2-B
33B
33B
33B
33B
33B
33B
33B
33B
49B
49B
5?B
5?B
6FB
6FB
6FB
6FB
6FB
7LB
7LB
8RB
8RB
8RB
8RB
8RB
8RB
8RB
9XB
;dB
;dB
;dB
<jB
<jB
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
>wB
?}B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
A�B
B�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
G�B
G�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
I�B
I�B
J�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
L�B
L�B
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
P�B
P�B
P�B
Q�B
Q�B
R�B
R�B
R�B
S�B
S�B
S�B
S�B
S�B
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
VB
VB
VB
VB
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
^5B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
_;B
_;B
`BB
`BB
aHB
aHB
aHB
aHB
aHB
aHB
bNB
cTB
cTB
cTB
cTB
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
e`B
e`B
e`B
e`B
e`B
e`B
e`B
e`B
e`B
e`B
e`B
e`B
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
hsB
hsB
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
m�B
m�B
m�B
m�B
n�B
n�B
n�B
n�B
o�B
o�B
o�B
p�B
p�B
p�B
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
t�B
t�B
t�B
t�B
t�B
u�B
u�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
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
w�B
w�B
w�B
w�B
w�B
w�B
x�B
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
y�B
y�B
y�B
y�B
y�B
z�B
z�B
z�B
z�B
z�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  Bu�Bt�Bu�Bu�Bu�Bu�Bu�Bt�Bu�Bu�Bt�Bt�Bt�Bt�Bt�Bt�Bu�Bu�Bu�Bu�Bv�Bv�Bx�Bv�Bv�Bz�B{�B{�B}�B�B�B�B�KB��B��B�GB�MB�aB�;B�;B�/B�B�B��B��B�	B�zB�B�B��B
#BhB�B B+�BJrBa�Bg�By	B��B�fB�.B��B�B��B�!B��B��B��B�>B�DB��B�B��B�UB�wB�0B�B�)B��B�CB��B��B��B�=B�@B�~B��BtnBrBo�BeBE�B,B(�B!�B�BB�B�B�0B�YB�^B�B�OB�B�1B��B�B�7Bt�BgmBXBF�B,"B$tBB[B
�B
��B
�#B
��B
��B
�sB
�=B
�B
�?B
��B
��B
�GB
~BB
vB
cTB
N�B
4�B
 �B
EB
2B
TB
JB
	B
B	�B	�B	�B	ʦB	�9B	� B	��B	�B	�^B	�B	��B	��B	�	B	{JB	h
B	d&B	Y�B	IlB	CaB	<�B	6zB	-�B	�B	,B	�B��B�8B��B��B�B�zB��B� B�~B�#B�	B�MB�B��B�lB��B�8B�B��B��B��B��B�/B��B��B�MB��B�oB�oB��B�oB��B��B��B�B� B�FB��B�=B�	B�&B�B��B��B�9B��B��B�/B��B��B��B�bB��B�jB�B�VB�\B�LB�kB��B��B�"B�]B��B��B��B��B�UB�tB�|B�B��B�3B��B�rB�B��B�"B�@B�aBؓBݘB�!B�B�LB��B�LB�ZB�nB��B��B�B�B�B��B�TB�B�B��B��B��BۦB��B�mB�$BخB�BچBیB��B��B�BB�B�B��B�B�B�B�B�B�B��B�8B��B��B��B�B�fB��B��B	GB	SB	�B	�B	�B	dB	PB	hB	�B	�B	�B	#B	!�B	(XB	*B	(sB	'8B	'�B	'B	!�B	B	!�B	"NB	�B	B	�B	�B	�B	�B	�B	�B		B	 B	"4B	'mB	,�B	0�B	0oB	3�B	7�B	?HB	B�B	AoB	B'B	=�B	9�B	:�B	:�B	:�B	;0B	=�B	?�B	A�B	B�B	E�B	H�B	MB	O(B	RoB	YeB	\]B	]IB	^OB	^jB	`�B	a�B	d@B	m�B	p�B	q�B	vB	w�B	zB	|B	|B	{�B	|B	|�B	}qB	}<B	~BB	�OB	�GB	�SB	�_B	�KB	�=B	�=B	�XB	�JB	�jB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�/B	�!B	�AB	�oB	��B	��B	�WB	�qB	��B	�kB	�QB	��B	�iB	��B	��B	�[B	�GB	�9B	�TB	�ZB	�`B	��B	��B	��B	��B	��B	ĶB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�KB	�IB	�IB	ބB	�pB	�VB	�VB	�VB	�VB	�VB	�VB	�BB	�hB	�NB	�ZB	�B	��B	��B	��B	�B	��B	�B	��B	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�0B	�B	�B	�B	�"B	�B	��B	�"B	�(B	�(B	�B	�B	�B
 4B
 4B
 B
;B
 B
;B
[B
[B
AB
[B
-B
GB
aB
�B
�B
?B
%B
B
9B
?B
YB
SB
mB
9B
9B
SB
�B
zB
�B
KB
fB
_B
_B
KB
	RB
	RB

rB
�B
�B
~B
dB
jB
jB
�B
�B
�B
pB
vB
�B
�B
oB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!B
 B
 �B
 �B
 �B
!�B
!�B
!�B
!�B
!�B
!�B
"�B
"�B
#�B
#�B
$B
$�B
$�B
%,B
%`B
'B
&�B
(
B
(
B
(
B
(
B
)B
(�B
)B
)B
*B
*B
+B
+B
+B
+B
+B
+B
+B
,B
,"B
,B
+�B
,"B
,"B
,B
,"B
,"B
,=B
-)B
-CB
-)B
-]B
.IB
/5B
/OB
/5B
/B
/5B
0!B
0B
0!B
0!B
0;B
0;B
0!B
0;B
0UB
0UB
0UB
1[B
1AB
2GB
2GB
2aB
2aB
2aB
3MB
3B
3MB
3MB
3hB
3MB
3MB
3hB
4nB
4nB
5tB
5ZB
6FB
6`B
6`B
6`B
6zB
7�B
7�B
8lB
8lB
8lB
8RB
8lB
8�B
8�B
9�B
;�B
;�B
;B
<�B
<jB
<�B
<�B
<�B
=�B
>�B
>wB
>wB
>wB
>�B
>�B
>�B
?B
?�B
@�B
@�B
@�B
@iB
@�B
@�B
@�B
@�B
@�B
@�B
BB
B�B
C�B
C�B
C�B
C�B
D�B
D�B
EB
G�B
G�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
I�B
J	B
J�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
L�B
L�B
L�B
M�B
M�B
NB
NB
N�B
N�B
O�B
O�B
O�B
O�B
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
Q B
Q B
QB
QB
R B
R:B
SB
S&B
S&B
TB
S�B
S�B
TB
TB
TB
TB
TB
TB
UB
T�B
UB
UB
UB
UMB
VB
VB
VB
VB
VB
W$B
W$B
W$B
V�B
W$B
W$B
W?B
X+B
X+B
X+B
Y1B
YKB
Y1B
Y1B
ZQB
[qB
[WB
[=B
\]B
\CB
\CB
\CB
]dB
]IB
]IB
^5B
^5B
^B
^B
^OB
^OB
^OB
^OB
^OB
^OB
^OB
^jB
^OB
_VB
_VB
`\B
`\B
abB
abB
aHB
aHB
aHB
abB
bhB
c:B
cTB
cnB
cnB
cnB
cnB
cTB
dtB
dZB
dtB
dtB
dZB
dZB
dtB
ezB
eFB
e`B
e`B
e`B
e`B
ezB
ezB
e`B
ezB
e`B
e`B
eFB
e`B
ezB
ezB
f�B
f�B
f�B
f�B
f�B
g�B
g�B
gmB
g�B
h�B
h�B
hsB
h�B
iyB
i�B
i�B
i�B
iyB
i�B
iyB
iyB
iyB
iyB
i�B
i�B
i�B
iyB
iyB
jB
j�B
j�B
jB
j�B
j�B
j�B
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
m�B
m�B
m�B
m�B
n�B
n�B
n�B
n�B
o�B
o�B
o�B
p�B
p�B
p�B
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
t�B
t�B
t�B
t�B
t�B
u�B
u�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
w�B
w�B
w�B
xB
w�B
w�B
w�B
w�B
w�B
xB
xB
y	B
x�B
x�B
y�B
y�B
y�B
zB
y�B
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
z�B
z�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=-0.02(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201702100033462017021000334620170210003346201806221308532018062213085320180622130853201804050709392018040507093920180405070939  JA  ARFMdecpA19c                                                                20170206093506  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20170206003534  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20170206003535  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20170206003535  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20170206003536  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20170206003536  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20170206003536  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20170206003536  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20170206003536  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20170206003537                      G�O�G�O�G�O�                JA  ARUP                                                                        20170206010144                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20170206153747  CV  JULD            G�O�G�O�F�w�                JM  ARCAJMQC2.0                                                                 20170209153346  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20170209153346  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404220939  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622040853  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200116211516                      G�O�G�O�G�O�                