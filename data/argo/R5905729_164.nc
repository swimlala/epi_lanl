CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2022-10-23T09:01:24Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7(   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7h   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8,   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           80   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    88   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            8<   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8D   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8L   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8T   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8X   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8`   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9`   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9d   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9h   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9l   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  `�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  p@   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �|   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �h   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �L   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �8   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �<   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �@   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �D   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �H   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �L   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20221023090124  20221023090124  5905729 Argo PMEL                                                       GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               �A   AO  7160                            2B  A   NAVIS_A                         0838                            170425                          863 @��R��?�1   @��SF)��@+�9Xb�dKƧ�1   GPS     Primary sampling: averaged                                                                                                                                                                                                                                         �A   A   A   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A���A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C�C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�3D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D���D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��
@���A Q�A Q�A@Q�A`Q�A�(�A�(�A�(�A�(�A�(�A���A�(�A�(�B {B{B{B{B {B({B0{B8{B@{BH{BP{BX{B`{Bh{Bp{Bx{B�
=B�
=B���B��
B�
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
CC�CCCCCCCCC C"C$C&C(C*C,C.C0C2C4C6C8C:C<C>C@CBCDCFCHCJCLCNCPCRCTCVCXCZC\C^C`CbCdCfChCjClCnCpCrCtCvCxCzC|C~C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C���C���C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D HD �HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HD	HD	�HD
HD
�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HD HD �HD!HD!�HD"HD"�HD#HD#�HD$HD$�HD%HD%�HD&HD&�HD'HD'�HD(HD(�HD)HD)�HD*HD*�HD+HD+�HD,HD,�HD-HD-�HD.HD.�HD/HD/�HD0HD0�HD1HD1�HD2HD2�HD3HD3�HD4HD4�HD5HD5�HD6HD6�HD7HD7�HD8HD8�HD9HD9�HD:HD:�HD;HD;�HD<HD<�HD=HD=�HD>HD>�HD?HD?�HD@HD@�HDAHDA�HDBHDB�HDCHDC�HDDHDD�HDEHDE�HDFHDF�HDGHDG�HDHHDH�HDIHDI�HDJHDJ�HDKHDK�HDLHDL�HDMHDM�HDNHDN�HDOHDO�HDPHDP�HDQHDQ�HDRHDR�HDSHDS�HDTHDT�HDUHDU�HDVHDV�HDWHDW�HDXHDX�HDYHDY�HDZHDZ�HD[HD[�HD\HD\�HD]HD]�HD^HD^�HD_HD_�HD`HD`�HDaHDa�HDbHDb�HDcHDc�HDdHDd�HDeHDe�HDfHDf�HDgHDg�HDhHDh�HDiHDi�HDjHDj�HDkHDk�HDlHDl�HDmHDm�HDnHDn�HDoHDo�HDpHDp�HDqHDq�HDrHDr�HDsHDs�HDtHDt�HDuHDu�HDvHDv�HDwHDw�HDxHDx�HDyHDy�HDzHDz�HD{HD{�HD|HD|�HD}HD}�HD~HD~�HDHD�HD� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D��qD� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D�D���D� �D�@�DÀ�D���D� �D�@�DĀ�D���D� �D�@�Dŀ�D���D� �D�@�Dƀ�D���D� �D�@�Dǀ�D���D� �D�@�DȀ�D���D� �D�@�Dɀ�D���D� �D�@�Dʀ�D���D� �D�@�Dˀ�D���D� �D�@�D̀�D���D� �D�@�D̀�D���D� �D�@�D΀�D���D� �D�@�Dπ�D���D� �D�@�DЀ�D���D� �D�@�Dр�D���D� �D�@�DҀ�D���D� �D�@�DӀ�D���D� �D�@�DԀ�D���D� �D�@�DՀ�D���D��D�@�Dր�D���D� �D�@�D׀�D���D� �D�@�D؀�D���D� �D�@�Dـ�D���D� �D�@�Dڀ�D���D� �D�@�Dۀ�D���D� �D�@�D܀�D���D� �D�@�D݀�D���D� �D�@�Dހ�D���D� �D�@�D߀�D���D� �D�@�D���D���D� �D�@�DဤD���D� �D�@�D․D���D� �D�@�D〤D���D� �D�@�D䀤D���D� �D�@�D値D���D� �D�@�D怤D���D� �D�@�D瀤D���D� �D�@�D耤D���D� �D�@�D逤D���D� �D�@�DꀤD���D� �D�@�D뀤D���D� �D�@�D쀤D���D� �D�@�D퀤D���D� �D�@�DD���D� �D�@�DD���D� �D�@�D���D���D� �D�@�D�D���D� �D�@�D�D���D� �D�@�D�D���D� �D�@�D�D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D��>D��>111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�~�A��A�|�A�x�A�n�A�r�A�~�A�|�A�z�A�x�A�x�A�r�A�bNA�I�A�=qA�;dA�=qA�=qA��A�ĜA�/A��
A�t�Aݧ�A�ffA�I�A�VA�K�A�dZA��;A�l�A�+A�&�AҺ^A�ȴAЏ\A��;AΩ�A�t�A�bNAİ!A�33A� �A���A�I�A�S�A�
=A�1'A�-A���A�x�A��hA�?}A��!A��jA�\)A��A�bA�JA��A�S�A��FA�A��A��\A���A�E�A��uA�5?A�bA��;A��hA�dZA���A�;dA��#A�%A�dZA�jA��A�FA~�!AzZAvjAn�\Ak�Ai+AhbNAg��Ac��A[��AX��AV~�ATI�ARE�AQ�AO��AL�\AH^5AE`BADjACACG�AB��A@$�A>ffA=ƨA<5?A:�/A9�A9K�A6��A4JA2�uA0��A.�`A.ZA-7LA,��A,�jA,��A-A-l�A-S�A-&�A,�RA+��A*bNA(�A'�hA&��A&E�A%�FA%G�A$�/A$bA#�#A#ƨA$  A"��A"A!��A!�hA ��A ��A �A�mA��A��A7LA��A�RA��AffA1'A�
Ap�A;dAȴAQ�A�A��A��A/A�A~�A�AK�Ar�A  A��A?}A�-A��A�A  A��A(�A��At�AhsAXA/A��A��A�!AQ�A��A�`A�!A�A�A��AS�A&�A�!AffA�A
�DA	�A	/Ar�A��AC�A�A��A�;At�A"�A��AI�A5?A1'A  A��A�A�A-A�7AoA �uA J@���@���@���@�v�@�$�@�?}@���@���@���@�o@�M�@��/@�R@�@�%@�1@�
=@�O�@�dZ@�?}@��@��@�M�@�^5@�R@���@��@��@�|�@�@��@�1'@�|�@�\@�@�@�Q�@�  @�
=@�Ĝ@�1'@�C�@�=q@�@�@���@�`B@�V@�Q�@�b@۝�@�"�@�@��H@ڟ�@�v�@ڇ+@��@���@���@�A�@��m@׶F@�|�@�\)@�K�@�"�@֧�@�{@�hs@ԋD@�I�@��@��;@ӝ�@Ӆ@���@���@�G�@��/@Гu@�A�@��;@�K�@��y@�~�@�X@̣�@�1'@���@ʗ�@�M�@�$�@���@�`B@�&�@���@�j@�  @���@ǝ�@�dZ@�+@Ɨ�@�J@��@ũ�@�&�@���@�j@�  @î@�M�@���@�`B@�%@�bN@� �@��;@��@�o@���@�E�@�&�@�%@��/@��9@��@�dZ@��H@�~�@�E�@��T@���@�`B@�V@��u@�9X@���@�dZ@��y@��+@�&�@���@���@�j@���@�o@���@��@��y@���@�{@�`B@�G�@�7L@�/@��@���@��@�Ĝ@�  @��@�
=@���@�=q@�?}@�V@��@���@��9@��D@��m@��F@��w@�ƨ@���@��@�\)@���@���@�o@��y@���@�V@�E�@�-@���@��7@�O�@�?}@�V@��@�A�@� �@�  @��;@���@��F@�K�@��R@�v�@��\@�=q@���@�X@��/@�j@�A�@�(�@��@���@�t�@�33@��H@��\@�=q@�$�@���@��@�7L@���@���@�Ĝ@� �@�o@���@�~�@�~�@�~�@�n�@�ff@�E�@���@���@�p�@�7L@��/@��9@���@��@��\@�{@��^@�G�@��@���@�r�@�Q�@��;@�;d@��y@��!@�~�@��@��@�7L@���@��j@��9@���@��@��u@��@��;@�dZ@�C�@��H@�~�@�ff@�M�@�$�@��@��#@���@�hs@�V@���@�I�@�b@���@��w@�+@��H@�~�@�V@�V@�V@�V@�^5@�V@�=q@��-@�hs@�G�@�&�@��@�V@���@��/@��@�Z@�(�@�1@��@�t�@���@�E�@��@�J@�J@��@��#@���@��@�?}@���@�r�@�Q�@�A�@�(�@�1@��
@��F@��@���@�\)@���@��H@�ȴ@���@�-@���@���@�O�@���@��9@��u@�1'@���@���@�l�@�\)@�K�@�"�@��!@�v�@�V@���@���@��h@�p�@�?}@���@��/@���@�I�@��@�@~�+@}�-@|�@|(�@{dZ@{C�@z�@z��@y��@xĜ@xr�@xbN@xA�@x �@w��@v�y@v�R@vv�@vff@vE�@v5?@u�@u`B@t�/@tz�@tI�@sƨ@sS�@r�H@r�\@rn�@rn�@r�@qG�@q%@pĜ@p�@p1'@o��@nV@l��@l�D@lZ@k�m@k�F@k��@k@jM�@i�^@i7L@hQ�@g�@g�;@g�w@g|�@g;d@g
=@fȴ@f�+@f5?@f@e��@e`B@eV@eV@d��@d�/@dz�@c�F@c"�@b��@b�\@b-@a�#@a��@a��@a7L@`�u@` �@_�P@_;d@_+@^��@^��@^V@^@]��@]�@\1@[�F@[��@[dZ@[C�@[33@["�@Z��@Z�@Y�@Y��@Y&�@Y%@XĜ@X��@XbN@XQ�@XQ�@X �@W|�@W�@V�@Vv�@VE�@U��@U`B@T��@Tz�@Tj@T�@Sƨ@S��@St�@SS�@S@R�!@R~�@RM�@Q�@Qhs@P��@P �@O��@O��@O\)@OK�@O+@Nȴ@N��@N�+@Nv�@M��@M?}@L��@LZ@L9X@K�F@J��@J~�@J^5@JM�@J�@I�#@I&�@HbN@G�;@G�@F5?@E�@E�T@E��@E@Ep�@D��@D9X@CdZ@B�@B~�@B-@A�7@A�@@r�@@b@?�;@?|�@?
=@>�R@>�+@>ff@>$�@=�T@=@=�h@<��@<�@;��@;dZ@;S�@:��@9��@9G�@8��@8�@8Q�@8A�@81'@81'@81'@81'@8 �@8b@7�@7\)@6�@6ȴ@6�R@6��@6��@6ff@6$�@6@5��@5�h@5`B@5?}@5O�@5?}@5V@4j@3��@3��@3dZ@3S�@3"�@3o@2��@2�!@2��@2�\@2�\@2n�@2M�@2J@1��@1�@0��@0��@0�9@0��@0�u@0�@0�@0bN@0bN@0A�@0A�@01'@0  @/�P@/;d@/�@.v�@-@-�@,�@,�j@,�D@,j@,(�@,1@+�
@+ƨ@+��@+@*M�@*-@*�@)�@)��@)X@)G�@)7L@)&�@)&�@)�@)�@)�@)�@)�@)�@)�@(��@(r�@(b@'l�@';d@'
=@&�@&v�@&E�@&$�@%��@%�@$�@$�@$I�@#��@#dZ@#33@"�H@"�\@"~�@"M�@"=q@"-@"J@!x�@!&�@!&�@!7L@!&�@!�@!%@ �`@ ��@ ��@ r�@ A�@ A�@ A�@ 1'@ b@   @�@�;@|�@
=@
=@��@�y@ȴ@�+@E�@{@@�T@�@O�@V@�@z�@�@�m@ƨ@t�@33@"�@�@�!@~�@^5@^5@M�@-@�#@�7@G�@7L@7L@%@��@�@Q�@b@�@��@|�@l�@�@��@�y@��@v�@V@5?@{@�T@�-@�h@`B@O�@?}@�@V@��@�/@�D@9X@1@��@�m@�
@��@t�@S�@C�@"�@@�H@��@�!@n�@^5@^5@=q@��@��@��@��@X@�@�@�`@�9@bN@1'@ �@�@l�@;d@+@
=@
=111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�~�A��A�|�A�x�A�n�A�r�A�~�A�|�A�z�A�x�A�x�A�r�A�bNA�I�A�=qA�;dA�=qA�=qA��A�ĜA�/A��
A�t�Aݧ�A�ffA�I�A�VA�K�A�dZA��;A�l�A�+A�&�AҺ^A�ȴAЏ\A��;AΩ�A�t�A�bNAİ!A�33A� �A���A�I�A�S�A�
=A�1'A�-A���A�x�A��hA�?}A��!A��jA�\)A��A�bA�JA��A�S�A��FA�A��A��\A���A�E�A��uA�5?A�bA��;A��hA�dZA���A�;dA��#A�%A�dZA�jA��A�FA~�!AzZAvjAn�\Ak�Ai+AhbNAg��Ac��A[��AX��AV~�ATI�ARE�AQ�AO��AL�\AH^5AE`BADjACACG�AB��A@$�A>ffA=ƨA<5?A:�/A9�A9K�A6��A4JA2�uA0��A.�`A.ZA-7LA,��A,�jA,��A-A-l�A-S�A-&�A,�RA+��A*bNA(�A'�hA&��A&E�A%�FA%G�A$�/A$bA#�#A#ƨA$  A"��A"A!��A!�hA ��A ��A �A�mA��A��A7LA��A�RA��AffA1'A�
Ap�A;dAȴAQ�A�A��A��A/A�A~�A�AK�Ar�A  A��A?}A�-A��A�A  A��A(�A��At�AhsAXA/A��A��A�!AQ�A��A�`A�!A�A�A��AS�A&�A�!AffA�A
�DA	�A	/Ar�A��AC�A�A��A�;At�A"�A��AI�A5?A1'A  A��A�A�A-A�7AoA �uA J@���@���@���@�v�@�$�@�?}@���@���@���@�o@�M�@��/@�R@�@�%@�1@�
=@�O�@�dZ@�?}@��@��@�M�@�^5@�R@���@��@��@�|�@�@��@�1'@�|�@�\@�@�@�Q�@�  @�
=@�Ĝ@�1'@�C�@�=q@�@�@���@�`B@�V@�Q�@�b@۝�@�"�@�@��H@ڟ�@�v�@ڇ+@��@���@���@�A�@��m@׶F@�|�@�\)@�K�@�"�@֧�@�{@�hs@ԋD@�I�@��@��;@ӝ�@Ӆ@���@���@�G�@��/@Гu@�A�@��;@�K�@��y@�~�@�X@̣�@�1'@���@ʗ�@�M�@�$�@���@�`B@�&�@���@�j@�  @���@ǝ�@�dZ@�+@Ɨ�@�J@��@ũ�@�&�@���@�j@�  @î@�M�@���@�`B@�%@�bN@� �@��;@��@�o@���@�E�@�&�@�%@��/@��9@��@�dZ@��H@�~�@�E�@��T@���@�`B@�V@��u@�9X@���@�dZ@��y@��+@�&�@���@���@�j@���@�o@���@��@��y@���@�{@�`B@�G�@�7L@�/@��@���@��@�Ĝ@�  @��@�
=@���@�=q@�?}@�V@��@���@��9@��D@��m@��F@��w@�ƨ@���@��@�\)@���@���@�o@��y@���@�V@�E�@�-@���@��7@�O�@�?}@�V@��@�A�@� �@�  @��;@���@��F@�K�@��R@�v�@��\@�=q@���@�X@��/@�j@�A�@�(�@��@���@�t�@�33@��H@��\@�=q@�$�@���@��@�7L@���@���@�Ĝ@� �@�o@���@�~�@�~�@�~�@�n�@�ff@�E�@���@���@�p�@�7L@��/@��9@���@��@��\@�{@��^@�G�@��@���@�r�@�Q�@��;@�;d@��y@��!@�~�@��@��@�7L@���@��j@��9@���@��@��u@��@��;@�dZ@�C�@��H@�~�@�ff@�M�@�$�@��@��#@���@�hs@�V@���@�I�@�b@���@��w@�+@��H@�~�@�V@�V@�V@�V@�^5@�V@�=q@��-@�hs@�G�@�&�@��@�V@���@��/@��@�Z@�(�@�1@��@�t�@���@�E�@��@�J@�J@��@��#@���@��@�?}@���@�r�@�Q�@�A�@�(�@�1@��
@��F@��@���@�\)@���@��H@�ȴ@���@�-@���@���@�O�@���@��9@��u@�1'@���@���@�l�@�\)@�K�@�"�@��!@�v�@�V@���@���@��h@�p�@�?}@���@��/@���@�I�@��@�@~�+@}�-@|�@|(�@{dZ@{C�@z�@z��@y��@xĜ@xr�@xbN@xA�@x �@w��@v�y@v�R@vv�@vff@vE�@v5?@u�@u`B@t�/@tz�@tI�@sƨ@sS�@r�H@r�\@rn�@rn�@r�@qG�@q%@pĜ@p�@p1'@o��@nV@l��@l�D@lZ@k�m@k�F@k��@k@jM�@i�^@i7L@hQ�@g�@g�;@g�w@g|�@g;d@g
=@fȴ@f�+@f5?@f@e��@e`B@eV@eV@d��@d�/@dz�@c�F@c"�@b��@b�\@b-@a�#@a��@a��@a7L@`�u@` �@_�P@_;d@_+@^��@^��@^V@^@]��@]�@\1@[�F@[��@[dZ@[C�@[33@["�@Z��@Z�@Y�@Y��@Y&�@Y%@XĜ@X��@XbN@XQ�@XQ�@X �@W|�@W�@V�@Vv�@VE�@U��@U`B@T��@Tz�@Tj@T�@Sƨ@S��@St�@SS�@S@R�!@R~�@RM�@Q�@Qhs@P��@P �@O��@O��@O\)@OK�@O+@Nȴ@N��@N�+@Nv�@M��@M?}@L��@LZ@L9X@K�F@J��@J~�@J^5@JM�@J�@I�#@I&�@HbN@G�;@G�@F5?@E�@E�T@E��@E@Ep�@D��@D9X@CdZ@B�@B~�@B-@A�7@A�@@r�@@b@?�;@?|�@?
=@>�R@>�+@>ff@>$�@=�T@=@=�h@<��@<�@;��@;dZ@;S�@:��@9��@9G�@8��@8�@8Q�@8A�@81'@81'@81'@81'@8 �@8b@7�@7\)@6�@6ȴ@6�R@6��@6��@6ff@6$�@6@5��@5�h@5`B@5?}@5O�@5?}@5V@4j@3��@3��@3dZ@3S�@3"�@3o@2��@2�!@2��@2�\@2�\@2n�@2M�@2J@1��@1�@0��@0��@0�9@0��@0�u@0�@0�@0bN@0bN@0A�@0A�@01'@0  @/�P@/;d@/�@.v�@-@-�@,�@,�j@,�D@,j@,(�@,1@+�
@+ƨ@+��@+@*M�@*-@*�@)�@)��@)X@)G�@)7L@)&�@)&�@)�@)�@)�@)�@)�@)�@)�@(��@(r�@(b@'l�@';d@'
=@&�@&v�@&E�@&$�@%��@%�@$�@$�@$I�@#��@#dZ@#33@"�H@"�\@"~�@"M�@"=q@"-@"J@!x�@!&�@!&�@!7L@!&�@!�@!%@ �`@ ��@ ��@ r�@ A�@ A�@ A�@ 1'@ b@   @�@�;@|�@
=@
=@��@�y@ȴ@�+@E�@{@@�T@�@O�@V@�@z�@�@�m@ƨ@t�@33@"�@�@�!@~�@^5@^5@M�@-@�#@�7@G�@7L@7L@%@��@�@Q�@b@�@��@|�@l�@�@��@�y@��@v�@V@5?@{@�T@�-@�h@`B@O�@?}@�@V@��@�/@�D@9X@1@��@�m@�
@��@t�@S�@C�@"�@@�H@��@�!@n�@^5@^5@=q@��@��@��@��@X@�@�@�`@�9@bN@1'@ �@�@l�@;d@+@
=@
=111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
{B
{B
{B
{B
�B
�B
{B
{B
{B
{B
{B
{B
{B
�B
�B
�B
�B
�B
0!B
k�B
��B
�sB
��B%BPBhB+B
��B
��B
�B
�fB
�B	7B�BR�BN�BM�BL�BgmB��BȴB�B�B
=B\B+B�B�BuB\BB�`B�BB�B�TB��B��B�FB�LB�dB�XB�'B��B�1B�7Bz�BjB^5BK�B,B
=B
�B
��B
ĜB
��B
e`B
5?B
�B
\B
B	�B	��B	�B	�JB	�B	�B	{�B	l�B	M�B	�B	+B	'�B	�B	uB	bB	  B�B�B�5B�B�B�B�NB��B�B�ZB�BB�`B�B�B�ZB�TB�yB�yB�HB�B�B	VB	&�B	6FB	D�B	jB	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�FB	ƨB	��B	��B	��B	�B	�`B	�B	��B	��B
  B	��B	��B
B
%B
+B
	7B
1B
+B
+B
1B
DB
1B
1B
DB

=B

=B
1B
	7B
1B
B
B
B
%B
1B
	7B
�B
�B
�B
oB
uB
�B
�B
$�B
&�B
%�B
$�B
#�B
$�B
$�B
 �B
 �B
�B
+B
(�B
,B
,B
'�B
(�B
%�B
#�B
�B
�B
�B
�B
�B
�B
�B
!�B
"�B
�B
 �B
"�B
#�B
"�B
'�B
'�B
%�B
#�B
�B
 �B
"�B
�B
�B
�B
�B
�B
#�B
$�B
"�B
!�B
�B
�B
uB
VB
%B
+B
B	��B
  B	��B	��B	�B	�sB	�ZB	�/B	�/B	�)B	�/B	�NB	�sB	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
+B
1B
	7B
DB
JB
VB
VB
VB
VB
JB

=B

=B
DB
PB
uB
uB
oB
hB
hB
VB
JB
bB
bB
hB
hB
bB
bB
\B
VB
DB
VB
bB
bB
JB
oB
uB
oB
bB
oB
hB
hB
oB
uB
uB
oB
oB
bB
hB
{B
uB
hB
oB
oB
hB
hB
PB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
{B
oB
\B
{B
�B
{B
uB
�B
�B
�B
�B
�B
�B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
!�B
!�B
!�B
 �B
 �B
#�B
$�B
#�B
"�B
"�B
$�B
$�B
#�B
"�B
#�B
$�B
#�B
 �B
"�B
#�B
#�B
#�B
#�B
"�B
 �B
 �B
#�B
&�B
$�B
#�B
#�B
"�B
#�B
&�B
'�B
'�B
(�B
+B
)�B
(�B
)�B
)�B
,B
+B
,B
,B
-B
-B
,B
(�B
&�B
.B
0!B
1'B
1'B
0!B
0!B
/B
-B
/B
/B
/B
.B
/B
.B
+B
%�B
-B
/B
/B
0!B
0!B
2-B
2-B
0!B
/B
2-B
2-B
2-B
1'B
1'B
49B
33B
6FB
6FB
6FB
6FB
5?B
49B
2-B
2-B
5?B
5?B
5?B
8RB
8RB
7LB
7LB
8RB
7LB
7LB
6FB
6FB
7LB
8RB
9XB
8RB
6FB
8RB
:^B
;dB
=qB
=qB
<jB
<jB
;dB
:^B
8RB
;dB
<jB
<jB
=qB
=qB
=qB
<jB
;dB
;dB
<jB
<jB
;dB
9XB
8RB
<jB
?}B
@�B
@�B
?}B
?}B
>wB
>wB
=qB
=qB
@�B
A�B
A�B
A�B
A�B
A�B
A�B
B�B
A�B
@�B
?}B
B�B
B�B
A�B
?}B
@�B
C�B
C�B
B�B
E�B
E�B
C�B
C�B
E�B
F�B
G�B
F�B
E�B
D�B
F�B
F�B
F�B
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
F�B
H�B
I�B
K�B
J�B
M�B
L�B
K�B
J�B
I�B
L�B
M�B
M�B
L�B
K�B
J�B
L�B
M�B
M�B
M�B
M�B
L�B
K�B
L�B
L�B
M�B
L�B
M�B
M�B
N�B
O�B
O�B
N�B
L�B
N�B
O�B
O�B
N�B
M�B
J�B
L�B
P�B
R�B
Q�B
R�B
R�B
P�B
P�B
Q�B
R�B
Q�B
T�B
VB
VB
VB
VB
T�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
W
B
VB
T�B
S�B
R�B
T�B
VB
W
B
VB
W
B
XB
W
B
VB
VB
W
B
W
B
YB
ZB
YB
YB
YB
YB
YB
W
B
W
B
ZB
\)B
[#B
\)B
\)B
[#B
ZB
ZB
\)B
\)B
[#B
]/B
]/B
]/B
]/B
^5B
]/B
\)B
[#B
\)B
]/B
]/B
^5B
]/B
^5B
^5B
]/B
`BB
_;B
_;B
`BB
`BB
`BB
_;B
_;B
`BB
`BB
_;B
_;B
_;B
_;B
aHB
bNB
bNB
bNB
bNB
aHB
cTB
bNB
bNB
`BB
aHB
aHB
cTB
cTB
bNB
aHB
dZB
e`B
e`B
dZB
dZB
bNB
bNB
cTB
dZB
e`B
hsB
iyB
iyB
iyB
hsB
gmB
gmB
hsB
iyB
iyB
iyB
iyB
jB
jB
k�B
l�B
k�B
l�B
m�B
m�B
m�B
m�B
m�B
m�B
l�B
jB
k�B
l�B
m�B
m�B
l�B
k�B
m�B
n�B
p�B
p�B
q�B
q�B
q�B
q�B
q�B
q�B
p�B
p�B
p�B
p�B
r�B
r�B
r�B
r�B
q�B
q�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
r�B
q�B
r�B
t�B
t�B
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
u�B
u�B
u�B
w�B
w�B
w�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
w�B
v�B
v�B
v�B
v�B
u�B
u�B
v�B
x�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
x�B
w�B
x�B
z�B
{�B
{�B
z�B
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
{�B
{�B
y�B
z�B
{�B
|�B
}�B
}�B
}�B
}�B
}�B
|�B
{�B
}�B
}�B
|�B
}�B
|�B
~�B
~�B
~�B
�B
� B
�B
�B
� B
~�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�B
�%B
�+B
�+B
�+B
�+B
�1B
�1B
�1B
�+B
�+B
�+B
�1B
�7B
�7B
�1B
�1B
�7B
�1B
�1B
�7B
�7B
�7B
�7B
�7B
�=B
�=B
�7B
�=B
�DB
�DB
�DB
�DB
�DB
�JB
�JB
�JB
�JB
�JB
�JB
�PB
�JB
�JB
�JB
�PB
�PB
�VB
�VB
�PB
�PB
�VB
�VB
�VB
�VB
�VB
�\B
�VB
�\B
�\B
�\B
�\B
�\B
�\B
�bB
�bB
�\B
�bB
�hB
�bB
�bB
�bB
�bB
�hB
�bB
�bB
�hB
�oB
�oB
�oB
�u111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
{B
{B
{B
{B
�B
�B
{B
{B
{B
{B
{B
{B
{B
�B
�B
�B
�B
�B
0!B
k�B
��B
�sB
��B%BPBhB+B
��B
��B
�B
�fB
�B	7B�BR�BN�BM�BL�BgmB��BȴB�B�B
=B\B+B�B�BuB\BB�`B�BB�B�TB��B��B�FB�LB�dB�XB�'B��B�1B�7Bz�BjB^5BK�B,B
=B
�B
��B
ĜB
��B
e`B
5?B
�B
\B
B	�B	��B	�B	�JB	�B	�B	{�B	l�B	M�B	�B	+B	'�B	�B	uB	bB	  B�B�B�5B�B�B�B�NB��B�B�ZB�BB�`B�B�B�ZB�TB�yB�yB�HB�B�B	VB	&�B	6FB	D�B	jB	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�FB	ƨB	��B	��B	��B	�B	�`B	�B	��B	��B
  B	��B	��B
B
%B
+B
	7B
1B
+B
+B
1B
DB
1B
1B
DB

=B

=B
1B
	7B
1B
B
B
B
%B
1B
	7B
�B
�B
�B
oB
uB
�B
�B
$�B
&�B
%�B
$�B
#�B
$�B
$�B
 �B
 �B
�B
+B
(�B
,B
,B
'�B
(�B
%�B
#�B
�B
�B
�B
�B
�B
�B
�B
!�B
"�B
�B
 �B
"�B
#�B
"�B
'�B
'�B
%�B
#�B
�B
 �B
"�B
�B
�B
�B
�B
�B
#�B
$�B
"�B
!�B
�B
�B
uB
VB
%B
+B
B	��B
  B	��B	��B	�B	�sB	�ZB	�/B	�/B	�)B	�/B	�NB	�sB	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
+B
1B
	7B
DB
JB
VB
VB
VB
VB
JB

=B

=B
DB
PB
uB
uB
oB
hB
hB
VB
JB
bB
bB
hB
hB
bB
bB
\B
VB
DB
VB
bB
bB
JB
oB
uB
oB
bB
oB
hB
hB
oB
uB
uB
oB
oB
bB
hB
{B
uB
hB
oB
oB
hB
hB
PB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
{B
oB
\B
{B
�B
{B
uB
�B
�B
�B
�B
�B
�B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
!�B
!�B
!�B
 �B
 �B
#�B
$�B
#�B
"�B
"�B
$�B
$�B
#�B
"�B
#�B
$�B
#�B
 �B
"�B
#�B
#�B
#�B
#�B
"�B
 �B
 �B
#�B
&�B
$�B
#�B
#�B
"�B
#�B
&�B
'�B
'�B
(�B
+B
)�B
(�B
)�B
)�B
,B
+B
,B
,B
-B
-B
,B
(�B
&�B
.B
0!B
1'B
1'B
0!B
0!B
/B
-B
/B
/B
/B
.B
/B
.B
+B
%�B
-B
/B
/B
0!B
0!B
2-B
2-B
0!B
/B
2-B
2-B
2-B
1'B
1'B
49B
33B
6FB
6FB
6FB
6FB
5?B
49B
2-B
2-B
5?B
5?B
5?B
8RB
8RB
7LB
7LB
8RB
7LB
7LB
6FB
6FB
7LB
8RB
9XB
8RB
6FB
8RB
:^B
;dB
=qB
=qB
<jB
<jB
;dB
:^B
8RB
;dB
<jB
<jB
=qB
=qB
=qB
<jB
;dB
;dB
<jB
<jB
;dB
9XB
8RB
<jB
?}B
@�B
@�B
?}B
?}B
>wB
>wB
=qB
=qB
@�B
A�B
A�B
A�B
A�B
A�B
A�B
B�B
A�B
@�B
?}B
B�B
B�B
A�B
?}B
@�B
C�B
C�B
B�B
E�B
E�B
C�B
C�B
E�B
F�B
G�B
F�B
E�B
D�B
F�B
F�B
F�B
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
F�B
H�B
I�B
K�B
J�B
M�B
L�B
K�B
J�B
I�B
L�B
M�B
M�B
L�B
K�B
J�B
L�B
M�B
M�B
M�B
M�B
L�B
K�B
L�B
L�B
M�B
L�B
M�B
M�B
N�B
O�B
O�B
N�B
L�B
N�B
O�B
O�B
N�B
M�B
J�B
L�B
P�B
R�B
Q�B
R�B
R�B
P�B
P�B
Q�B
R�B
Q�B
T�B
VB
VB
VB
VB
T�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
W
B
VB
T�B
S�B
R�B
T�B
VB
W
B
VB
W
B
XB
W
B
VB
VB
W
B
W
B
YB
ZB
YB
YB
YB
YB
YB
W
B
W
B
ZB
\)B
[#B
\)B
\)B
[#B
ZB
ZB
\)B
\)B
[#B
]/B
]/B
]/B
]/B
^5B
]/B
\)B
[#B
\)B
]/B
]/B
^5B
]/B
^5B
^5B
]/B
`BB
_;B
_;B
`BB
`BB
`BB
_;B
_;B
`BB
`BB
_;B
_;B
_;B
_;B
aHB
bNB
bNB
bNB
bNB
aHB
cTB
bNB
bNB
`BB
aHB
aHB
cTB
cTB
bNB
aHB
dZB
e`B
e`B
dZB
dZB
bNB
bNB
cTB
dZB
e`B
hsB
iyB
iyB
iyB
hsB
gmB
gmB
hsB
iyB
iyB
iyB
iyB
jB
jB
k�B
l�B
k�B
l�B
m�B
m�B
m�B
m�B
m�B
m�B
l�B
jB
k�B
l�B
m�B
m�B
l�B
k�B
m�B
n�B
p�B
p�B
q�B
q�B
q�B
q�B
q�B
q�B
p�B
p�B
p�B
p�B
r�B
r�B
r�B
r�B
q�B
q�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
r�B
q�B
r�B
t�B
t�B
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
u�B
u�B
u�B
w�B
w�B
w�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
w�B
v�B
v�B
v�B
v�B
u�B
u�B
v�B
x�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
x�B
w�B
x�B
z�B
{�B
{�B
z�B
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
{�B
{�B
y�B
z�B
{�B
|�B
}�B
}�B
}�B
}�B
}�B
|�B
{�B
}�B
}�B
|�B
}�B
|�B
~�B
~�B
~�B
�B
� B
�B
�B
� B
~�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�B
�%B
�+B
�+B
�+B
�+B
�1B
�1B
�1B
�+B
�+B
�+B
�1B
�7B
�7B
�1B
�1B
�7B
�1B
�1B
�7B
�7B
�7B
�7B
�7B
�=B
�=B
�7B
�=B
�DB
�DB
�DB
�DB
�DB
�JB
�JB
�JB
�JB
�JB
�JB
�PB
�JB
�JB
�JB
�PB
�PB
�VB
�VB
�PB
�PB
�VB
�VB
�VB
�VB
�VB
�\B
�VB
�\B
�\B
�\B
�\B
�\B
�\B
�bB
�bB
�\B
�bB
�hB
�bB
�bB
�bB
�bB
�hB
�bB
�bB
�hB
�oB
�oB
�oB
�u111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.02 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20221023090124                              AO  ARCAADJP                                                                    20221023090124    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20221023090124  QCP$                G�O�G�O�G�O�1F83E           AO  ARGQQCPL                                                                    20221023090124  QCF$                G�O�G�O�G�O�4000            