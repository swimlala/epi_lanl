CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T17:23:34Z creation;2022-06-04T17:23:34Z conversion to V3.1      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  78   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7x   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8<   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8@   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8H   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8L   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8T   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8\   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8d   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8h   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8p   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8t   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8x   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8|   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9|   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `h   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �l   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �P   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �T   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �8   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �X   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �D   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �H   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �L   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �P   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �T   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20220604172334  20220610121506  5905853                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               A   JA                                  2B  A   APEX                            8421                            2.11.2                          846 @سX/��1   @سX�I��@,.z�G��dS�;dZ1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @333@�  @�  @���A   A@  A`  A�  A���A���A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bi��BnffBw��B�  B�  B�ffB���B���B�  B�  B�  B�  B�  B�  B�  B�  B���B���B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�ffB���B�  B�  B���B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C33C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB33CD  CF  CH  CI�fCL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl�Cn�Cp�Cr  Cs�fCv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� DefDe� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�3D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӃ3D��3D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�3D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@4z�@���@���@�p�A Q�A@Q�A`Q�A�(�A���A���A�(�A�(�A�(�A�(�A�(�B {B{B{B{B {B({B0{B8{B@{BH{BP{BX{B`{Bi�Bnz�Bw�B�
=B�
=B�p�B���B��
B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B��
B��
B�
=B�
=B�
=B�
=B�
=B��
B�
=B�
=B�
=B�
=B�p�B��
B�
=B�
=B��
B�
=B�
=C CCCCC
CCCCCCC8RCCCC C"C$C&C(C*C,C.C0C2C4C6C8C:C<C>C@CB8RCDCFCHCI�CLCNCPCRCTCVCXCZC\C^C`CbCdCfChCjCl�Cn�Cp�CrCs�CvCxCzC|C~C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C���C��C�\C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�\C�\C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�\C��C��C��C��C��C��C��D HD �HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HD	HD	�HD
HD
�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HD HD �HD!HD!�HD"HD"�HD#HD#�HD$HD$�HD%HD%�HD&HD&�HD'HD'�HD(HD(�HD)HD)�HD*HD*�HD+HD+�HD,HD,�HD-HD-�HD.HD.�HD/HD/�HD0HD0�HD1HD1�HD2HD2�HD3HD3�HD4HD4�HD5HD5�HD6HD6�HD7HD7�HD8HD8�HD9HD9�HD:HD:�HD;HD;�HD<HD<�HD=HD=�HD>HD>�HD?HD?�HD@HD@�HDAHDA�HDBHDB�HDCHDC�HDDHDD�HDEHDE�HDFHDF�HDGHDG�HDHHDH�HDIHDI�HDJHDJ�HDKHDK�HDLHDL�HDMHDM�HDNHDN�HDOHDO�HDPHDP�HDQHDQ�HDRHDR�HDSHDS�HDTHDT�HDUHDU�HDVHDV�HDWHDW�HDXHDX�HDYHDY�HDZHDZ�HD[HD[�HD\HD\�HD]HD]�HD^HD^�HD_HD_�HD`HD`�HDaHDa�HDbHDb�HDcHDc�HDdHDd�HDe�De�HDfHDf�HDgHDg�HDhHDh�HDiHDi�HDjHDj�HDkHDk�HDlHDl�HDmHDm�HDnHDn�HDoHDo�HDpHDp�HDqHDq�HDrHDr�HDsHDs�HDtHDt�HDuHDu�HDvHDv�HDwHDw�HDxHDx�HDyHDy�HDzHDz�HD{HD{�HD|HD|�HD}HD}�HD~HD~�HDHD�HD� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�C�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D��qD� �D�@�D���D���D� �D�@�D���D���D� �D�@�D�D���D��D�@�DÀ�D���D� �D�@�DĀ�D���D� �D�@�Dŀ�D���D� �D�@�Dƀ�D���D� �D�@�Dǀ�D���D� �D�@�DȀ�D���D� �D�@�Dɀ�D���D� �D�@�Dʀ�D���D� �D�@�Dˀ�D���D� �D�@�D̀�D���D� �D�@�D̀�D���D� �D�@�D΀�D���D� �D�@�Dπ�D���D� �D�@�DЀ�D���D� �D�@�Dр�D���D� �D�@�DҀ�D���D� �D�@�DӃ�D���D� �D�@�DԀ�D���D� �D�@�DՀ�D���D� �D�@�Dր�D���D� �D�@�D׀�D���D� �D�@�D؀�D���D� �D�@�Dـ�D���D� �D�@�Dڀ�D���D� �D�@�Dۀ�D���D� �D�@�D܀�D���D� �D�@�D݀�D���D� �D�@�Dހ�D���D� �D�@�D߀�D���D� �D�@�D���D���D� �D�@�DဤD���D� �D�@�D․D���D� �D�@�D〤D���D� �D�@�D䀤D���D� �D�@�D値D���D� �D�@�D怤D���D��D�@�D瀤D���D� �D�@�D耤D���D� �D�@�D逤D���D� �D�@�DꀤD���D� �D�@�D뀤D���D� �D�@�D쀤D���D� �D�@�D퀤D���D� �D�@�DD���D� �D�@�DD���D� �D�@�D���D���D� �D�@�D�D���D� �D�@�D�D���D� �D�@�D�D���D� �D�@�D�D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�<6A�F?A�N�A�P}A�PA�P�A�M6A�I�A�L0A�B�A��A�A� iA��A���AΟ�A�P}A�J�A�E�A�A A�:�A�-�A�xA��A��A� iA���A��|A�ƨA�� A�@�A���A�#:A���A��.A���A��0Aʖ�A��A�\)A��BA�v+A��A�J�A�-CA�S[A��^A�A�i�A�1A�(�A�e�A�b�A�U�A��NA�0�A���A�rA�A���A�]dA���A�2�A�~A�Y�A�-CA�ffA��
A�  A�a�A��A��WA�YA�0!A��A��A�pA�6A��A��:A��A�]�A�+A��-A��5A�XEA��A�B[A~��A{��AyϫAtT�ApߤAm�Aj�3Ai�4AgQAd�YAa�A^��A\^5AV�	AP�AK�AG��AD�A@S�A=tTA;VA:��A:u�A:VA9��A8�A5v`A3xlA2sA1��A18A.3�A,��A,QA+��A*��A*SA)��A(�dA()_A&ƨA$��A%�A$Z�A#xA"�}A"8�A!�A�PA�HA9�A��A�|A�aA�[A|�AA�$A?�AiDA_�Aw�AA�{A�AiDA+A�A�nA��A+AQ�Ay>A]�A�"A�KAv�A;dA7�A�5A��A��A�xA~�Ai�A!-A��A]dAVmAL�A#�AMA�|A��A;�A	�KA�FA�A��A��A��A��A:�Ak�A�YAn�A��A��AoiA/�Ab�AP�A1�A�mA�A&A��A:�A�2A��AOA��A�6A�wA�	A?}A �PA �A �hA ��A B�@��"@��@�D�@��=@�C�@�@��!@�6@�\�@��@��@��@��*@�S�@�5�@���@��)@�]d@� �@���@�/@�u�@��#@��	@�N<@���@��@���@�U2@�(�@�H@�_�@�Ov@�?�@�v`@�i�@���@���@��F@�$@�zx@�f�@�X�@�K�@�8�@���@�S@�8�@�M�@��+@�F@��B@噚@俱@�A�@�hs@���@��@�M@�
�@�R�@�<�@۟V@��@ۜ�@�Vm@���@ڰ�@ڠ�@ڈ�@�n�@�@�@�|@��?@ة�@�H�@�c�@�?@�{@�%�@��@�t�@���@�S�@�4n@�x�@ґ @�/�@���@�!-@��@Ы6@�M@���@Ϸ@��@�oi@���@���@ͻ0@͕�@ͅ@�qv@��@� �@˸�@�~�@�C�@ʰ!@�C�@ɱ[@�e�@�c�@�J�@� i@�_�@��@Ǭq@�,�@���@��@�z�@�y�@��@�1�@Ó@���@u@�@��k@��@�n�@�ݘ@���@���@���@���@�y�@�Dg@���@��I@���@�A�@���@�	l@���@�V�@��@��'@�;d@��Y@�Z�@�9X@�e@���@��P@���@��h@�m�@��g@�e�@��@�	@��@�f�@���@��@�,�@�V@��?@���@��t@�^�@���@�H�@��@��S@�K�@��H@���@�n�@�4n@���@��3@���@��=@��$@��"@�_p@�/@��4@���@�&�@��]@�u%@�e@���@��@�L�@�5�@��,@�J@���@��@��@��@��j@��@@�Y�@�J�@�4@��L@���@�f�@�U�@�6z@�(@��|@��6@���@��D@�W�@�0U@�@�˒@�$t@�4n@�ƨ@���@��@��@�|�@�1�@�G@���@�L�@���@�PH@�8�@�b@�@��.@���@�ݘ@���@�}�@�@@��\@��@�ݘ@���@���@�<6@���@���@��A@�C-@��@���@���@�N<@��@��$@�E�@�c�@�Q�@��@�t�@��E@��1@�}V@�V�@��@��@��@�ԕ@���@�o�@�F�@��@���@�|�@�<�@��+@���@��@��3@�ƨ@���@�v`@�W?@�<6@��@�ȴ@��_@�/�@���@���@�U�@�ی@���@���@��'@�rG@�U�@�33@���@��M@�҉@��D@�~@���@�&�@��@��'@�V�@��+@�dZ@�+�@�	l@��@�n�@�-@�u@���@�j�@��@���@�>B@�-�@��@���@�qv@�*0@�֡@�>B@��=@�"�@��|@���@�h�@�:*@��@��;@���@�a@�Q�@�V@���@�Z@�J�@�0U@��T@��0@�S&@��@�Ɇ@��O@�y>@��@+@~��@~� @~:*@~�@}��@}�7@}Dg@|�@|��@|7�@{� @{�f@{
=@z��@zxl@y��@yw2@y�@xK^@v��@u��@u��@u+�@t�`@tc�@s�a@sb�@s@r��@r;�@q�@q-w@py>@o;d@n��@n��@m�Z@mj@m*0@lی@l�@k�@k��@k��@kMj@j�b@i�3@i|@iT�@h��@h�@g��@g@f��@f_@e��@e�n@erG@e%F@d��@dy>@c�@c�@c��@c�@b��@b�<@b�L@b�x@bd�@a��@a��@aQ�@a�@`�[@`��@`m�@`PH@`1@_t�@_@^��@^�]@^�@]�o@]�@]@\�K@\6@[��@[�@[>�@Z��@Z��@Z}V@Z�@Y�X@Y%@W�g@W_p@V�6@V��@VGE@V �@U@T�[@T6@S��@S��@SdZ@S4�@S�:@S�k@Sx@S"�@SS@R�m@R�A@Rn�@ROv@R1�@R{@Q�@Q��@Q�@PPH@O��@Os@O�@N�H@N�b@N��@N�@M�S@L�@L�j@Lq@K��@J͟@J��@J
�@I��@H��@H,=@G�:@GU�@F��@FL0@F$�@E��@Ef�@E=�@E%@D�e@D�_@D�@DU2@D7@C��@C�k@Cg�@CJ#@B��@B��@B:*@A��@A��@A@@@m�@@K^@@6@@�@?�W@?� @?��@?+@>�@>0U@>_@=��@=j@=�@<�@<A�@;�@;W?@;&@:Q@:)�@:)�@9�T@9��@9@8�K@8`�@7�@7��@7��@7_p@7J#@6�]@6�@5��@5s�@4�@4�_@4M@3�@3��@3�0@3O@2�@2E�@1��@1��@1`B@1G�@1e,@1�"@1�7@1��@1}�@1k�@1T�@1Dg@1:�@1+�@0�f@0�@0�@0��@0��@0]d@0I�@0x@/ƨ@/��@.�@.{�@.;�@-ϫ@-��@,�P@,��@,c�@,A�@+�@+t�@+�@*��@*.�@)�@)�-@)��@)�~@)-w@(�@(,=@(�@'��@'��@'|�@')_@&��@&�h@&��@&V@&#:@%�>@%�@%�~@%Y�@%V@$�p@$r�@$]d@$M@$%�@$@$~@#��@#o�@#$t@"��@"��@"B[@!��@!c@!o @!Dg@ �`@ D�@ M@��@\)@��@�+@:*@��@/@V@ \@#�@+�@0�@�@Ĝ@��@�@��@�{@��@�f@ i@��@�.@�@j@B�@�?@D�@!@G@@@�@˒@�w@��@�@��@�	@y�@\)@E9@8@&@$t@�@�@�@��@��@c @J�@E�@($@��@c�@	l@�5@�O@_@Ft@6@7@˒@��@x@+@�@ߤ@�@��@��@��@W�@R�@YK@�+@v�@B[@
�@�T@�9@@N<@�@��@l"@`�@bN@��@�@�@��@33@�@��@i�@
�@�H@��@�X@��@�"@��@�M@m]@�@�O@|�@I�@�@��@�}@�a@��@�$@RT@;d@$t@�@
�M@
�,@
�+111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�<6A�F?A�N�A�P}A�PA�P�A�M6A�I�A�L0A�B�A��A�A� iA��A���AΟ�A�P}A�J�A�E�A�A A�:�A�-�A�xA��A��A� iA���A��|A�ƨA�� A�@�A���A�#:A���A��.A���A��0Aʖ�A��A�\)A��BA�v+A��A�J�A�-CA�S[A��^A�A�i�A�1A�(�A�e�A�b�A�U�A��NA�0�A���A�rA�A���A�]dA���A�2�A�~A�Y�A�-CA�ffA��
A�  A�a�A��A��WA�YA�0!A��A��A�pA�6A��A��:A��A�]�A�+A��-A��5A�XEA��A�B[A~��A{��AyϫAtT�ApߤAm�Aj�3Ai�4AgQAd�YAa�A^��A\^5AV�	AP�AK�AG��AD�A@S�A=tTA;VA:��A:u�A:VA9��A8�A5v`A3xlA2sA1��A18A.3�A,��A,QA+��A*��A*SA)��A(�dA()_A&ƨA$��A%�A$Z�A#xA"�}A"8�A!�A�PA�HA9�A��A�|A�aA�[A|�AA�$A?�AiDA_�Aw�AA�{A�AiDA+A�A�nA��A+AQ�Ay>A]�A�"A�KAv�A;dA7�A�5A��A��A�xA~�Ai�A!-A��A]dAVmAL�A#�AMA�|A��A;�A	�KA�FA�A��A��A��A��A:�Ak�A�YAn�A��A��AoiA/�Ab�AP�A1�A�mA�A&A��A:�A�2A��AOA��A�6A�wA�	A?}A �PA �A �hA ��A B�@��"@��@�D�@��=@�C�@�@��!@�6@�\�@��@��@��@��*@�S�@�5�@���@��)@�]d@� �@���@�/@�u�@��#@��	@�N<@���@��@���@�U2@�(�@�H@�_�@�Ov@�?�@�v`@�i�@���@���@��F@�$@�zx@�f�@�X�@�K�@�8�@���@�S@�8�@�M�@��+@�F@��B@噚@俱@�A�@�hs@���@��@�M@�
�@�R�@�<�@۟V@��@ۜ�@�Vm@���@ڰ�@ڠ�@ڈ�@�n�@�@�@�|@��?@ة�@�H�@�c�@�?@�{@�%�@��@�t�@���@�S�@�4n@�x�@ґ @�/�@���@�!-@��@Ы6@�M@���@Ϸ@��@�oi@���@���@ͻ0@͕�@ͅ@�qv@��@� �@˸�@�~�@�C�@ʰ!@�C�@ɱ[@�e�@�c�@�J�@� i@�_�@��@Ǭq@�,�@���@��@�z�@�y�@��@�1�@Ó@���@u@�@��k@��@�n�@�ݘ@���@���@���@���@�y�@�Dg@���@��I@���@�A�@���@�	l@���@�V�@��@��'@�;d@��Y@�Z�@�9X@�e@���@��P@���@��h@�m�@��g@�e�@��@�	@��@�f�@���@��@�,�@�V@��?@���@��t@�^�@���@�H�@��@��S@�K�@��H@���@�n�@�4n@���@��3@���@��=@��$@��"@�_p@�/@��4@���@�&�@��]@�u%@�e@���@��@�L�@�5�@��,@�J@���@��@��@��@��j@��@@�Y�@�J�@�4@��L@���@�f�@�U�@�6z@�(@��|@��6@���@��D@�W�@�0U@�@�˒@�$t@�4n@�ƨ@���@��@��@�|�@�1�@�G@���@�L�@���@�PH@�8�@�b@�@��.@���@�ݘ@���@�}�@�@@��\@��@�ݘ@���@���@�<6@���@���@��A@�C-@��@���@���@�N<@��@��$@�E�@�c�@�Q�@��@�t�@��E@��1@�}V@�V�@��@��@��@�ԕ@���@�o�@�F�@��@���@�|�@�<�@��+@���@��@��3@�ƨ@���@�v`@�W?@�<6@��@�ȴ@��_@�/�@���@���@�U�@�ی@���@���@��'@�rG@�U�@�33@���@��M@�҉@��D@�~@���@�&�@��@��'@�V�@��+@�dZ@�+�@�	l@��@�n�@�-@�u@���@�j�@��@���@�>B@�-�@��@���@�qv@�*0@�֡@�>B@��=@�"�@��|@���@�h�@�:*@��@��;@���@�a@�Q�@�V@���@�Z@�J�@�0U@��T@��0@�S&@��@�Ɇ@��O@�y>@��@+@~��@~� @~:*@~�@}��@}�7@}Dg@|�@|��@|7�@{� @{�f@{
=@z��@zxl@y��@yw2@y�@xK^@v��@u��@u��@u+�@t�`@tc�@s�a@sb�@s@r��@r;�@q�@q-w@py>@o;d@n��@n��@m�Z@mj@m*0@lی@l�@k�@k��@k��@kMj@j�b@i�3@i|@iT�@h��@h�@g��@g@f��@f_@e��@e�n@erG@e%F@d��@dy>@c�@c�@c��@c�@b��@b�<@b�L@b�x@bd�@a��@a��@aQ�@a�@`�[@`��@`m�@`PH@`1@_t�@_@^��@^�]@^�@]�o@]�@]@\�K@\6@[��@[�@[>�@Z��@Z��@Z}V@Z�@Y�X@Y%@W�g@W_p@V�6@V��@VGE@V �@U@T�[@T6@S��@S��@SdZ@S4�@S�:@S�k@Sx@S"�@SS@R�m@R�A@Rn�@ROv@R1�@R{@Q�@Q��@Q�@PPH@O��@Os@O�@N�H@N�b@N��@N�@M�S@L�@L�j@Lq@K��@J͟@J��@J
�@I��@H��@H,=@G�:@GU�@F��@FL0@F$�@E��@Ef�@E=�@E%@D�e@D�_@D�@DU2@D7@C��@C�k@Cg�@CJ#@B��@B��@B:*@A��@A��@A@@@m�@@K^@@6@@�@?�W@?� @?��@?+@>�@>0U@>_@=��@=j@=�@<�@<A�@;�@;W?@;&@:Q@:)�@:)�@9�T@9��@9@8�K@8`�@7�@7��@7��@7_p@7J#@6�]@6�@5��@5s�@4�@4�_@4M@3�@3��@3�0@3O@2�@2E�@1��@1��@1`B@1G�@1e,@1�"@1�7@1��@1}�@1k�@1T�@1Dg@1:�@1+�@0�f@0�@0�@0��@0��@0]d@0I�@0x@/ƨ@/��@.�@.{�@.;�@-ϫ@-��@,�P@,��@,c�@,A�@+�@+t�@+�@*��@*.�@)�@)�-@)��@)�~@)-w@(�@(,=@(�@'��@'��@'|�@')_@&��@&�h@&��@&V@&#:@%�>@%�@%�~@%Y�@%V@$�p@$r�@$]d@$M@$%�@$@$~@#��@#o�@#$t@"��@"��@"B[@!��@!c@!o @!Dg@ �`@ D�@ M@��@\)@��@�+@:*@��@/@V@ \@#�@+�@0�@�@Ĝ@��@�@��@�{@��@�f@ i@��@�.@�@j@B�@�?@D�@!@G@@@�@˒@�w@��@�@��@�	@y�@\)@E9@8@&@$t@�@�@�@��@��@c @J�@E�@($@��@c�@	l@�5@�O@_@Ft@6@7@˒@��@x@+@�@ߤ@�@��@��@��@W�@R�@YK@�+@v�@B[@
�@�T@�9@@N<@�@��@l"@`�@bN@��@�@�@��@33@�@��@i�@
�@�H@��@�X@��@�"@��@�M@m]@�@�O@|�@I�@�@��@�}@�a@��@�$@RT@;d@$t@�@
�M@
�,@
�+111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B%�B&B&LB&fB&fB&LB%�B%�B%�B$�B!-B!�BVB�BqB�B�B�B�B�B�B�B:B�BTB&BB�B#�B$�B]�B	5?B	��B	�+B
 B
J�B
�B
�"B
�{B
ȚB
� B
��B
��Bw�B� B~BB�fB��B��B�=BӏB��BUB
�B�B\B�B�B�B!|BjB�B!�B/B�B�B�5B�uB��B�B��B�&B��Bw�Bm�BYBEB4�B$�B�BMB
ܬB
�B
vzB
W�B
/OB
�B	ݘB	�<B	��B	��B	��B	��B	wfB	v�B	s�B	j�B	]�B	O\B	G+B	@OB	6+B	 �B		�B	oB	xB	�B	B	�B	EB	�B	1B	B	$@B	6�B	E�B	K)B	NB	K�B	BuB	L�B	^�B	��B	�RB	��B	�#B	��B	��B	��B	��B	�hB	�XB	�tB	�RB	�xB	��B	��B	��B	��B	�=B	��B	��B	��B	�bB	�jB	��B	��B	�;B	��B	��B	��B	��B	�B	�ZB	��B	�kB	��B	�vB	�dB	��B	�B	�uB	�YB	�]B	�B	�B	�!B	��B	ۦB	�QB	�B	�eB	چB	�7B	��B	�KB	�B	�B	�tB	��B	�%B	�B	�B	�B	�B	�B	�B	�qB	�WB	�B	�B	�iB	�oB	�B	�LB	�B	��B	�}B
�B

�B
�B
�B

rB
�B
0B
�B
�B
�B
�B
6B
�B
"B
�B
�B
vB
�B
}B
�B
NB
�B
B
�B
�B
aB
{B
�B
oB
TB
TB
 B
oB
�B
�B
}B
 B
�B
�B
hB
B
�B
HB
�B
vB
�B
�B
jB
B

rB
	lB
�B
YB
�B
�B
xB
0B
B
�B
�B
�B
�B
�B
�B
�B
B

	B
�B
SB
�B
�B
{B
'B	�.B	��B	�PB	�jB	�dB	�dB	��B	�.B	�<B	��B	��B
�B
�B
uB
 �B
 OB
  B	��B	�.B	�wB	��B	�DB	�*B	�lB	��B	��B	��B	��B	��B	��B	�zB	�ZB	��B	��B	��B	��B	��B	�LB	��B	�JB	�(B
 �B
 �B
 �B
UB
-B
�B
�B
�B
{B
�B
�B
3B
B
3B
gB
%B
�B
EB
�B
�B
�B
tB
�B
YB
�B
tB
%B
�B
mB
B
�B
B
�B
KB
�B
	�B
	�B
	�B
	7B
	7B
	B
	B
	B
	B
	B
	B
	RB
	7B
�B
�B
�B
�B
fB
�B
�B
�B
+B
�B
	�B
	�B
	�B
	�B
	�B
	RB
	�B

=B
	�B
	7B
	7B
�B
B
KB
�B
+B
�B
�B
KB
B
�B
KB
	�B
	lB
	�B

=B

�B

�B

�B

rB

�B

�B
^B
�B
�B
�B
�B
�B
^B
DB
�B
~B
6B
jB
PB
6B
PB
�B
PB
�B
�B
pB
�B
pB
�B
pB
\B
�B
�B
�B
.B
�B
NB
NB
hB
hB
hB
�B
�B
�B
�B
B
�B
:B
B
aB
�B
�B
�B
FB
�B
�B
aB
�B
aB
aB
�B
[B
�B
�B
�B
�B
�B
B
�B
2B
�B
�B
�B
�B

B
sB
�B
B
EB
�B
KB
QB
7B
	B
�B
�B
WB
)B
�B
B
IB
�B
dB
IB
/B
dB
/B
/B
B
�B
dB
�B
�B
B
�B
�B
�B
pB
�B
�B
�B
�B
 �B
!-B
!HB
!|B
!�B
"�B
#nB
$B
$B
$tB
$�B
%FB
%�B
&�B
&�B
&�B
&�B
'B
&�B
'B
'8B
'�B
(
B
(sB
(XB
(>B
(�B
)yB
*B
*0B
*eB
*B
+B
*�B
+B
+QB
+6B
+�B
,qB
,�B
,�B
,�B
,�B
-wB
-�B
.cB
/OB
0;B
0�B
1B
1vB
1�B
2B
2GB
2aB
2�B
2�B
2�B
3B
3�B
4B
4B
4B
4�B
4�B
5tB
5tB
5�B
5�B
5�B
6`B
5�B
5�B
6+B
6�B
72B
7LB
7�B
7�B
7�B
7�B
7�B
7�B
7�B
88B
8B
88B
8RB
8lB
8RB
8RB
6�B
4�B
4TB
49B
4TB
5ZB
6+B
6FB
6FB
6+B
5�B
5B
4nB
4�B
4TB
4TB
4B
5ZB
5�B
5�B
6�B
8lB
8�B
8�B
8�B
8�B
8RB
8RB
8�B
9>B
9rB
9�B
9�B
:DB
:xB
:�B
:�B
;B
;0B
;dB
;JB
;�B
<�B
="B
=�B
>(B
>wB
>�B
>�B
>�B
?HB
?cB
?}B
?�B
?�B
?�B
?�B
?�B
@B
@4B
@4B
?�B
?�B
?�B
?�B
@�B
AUB
A�B
A�B
BuB
B�B
B�B
B�B
C-B
C{B
C{B
C�B
C�B
DgB
E9B
E9B
E�B
E�B
F?B
F?B
F?B
F�B
GB
GEB
GEB
G�B
IB
KB
K^B
K�B
LB
L0B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
MB
M6B
M6B
M�B
M�B
NB
N"B
N�B
OB
N�B
N�B
N�B
N�B
O(B
OBB
O\B
OvB
OBB
O�B
OvB
O�B
P.B
P}B
O�B
P}B
PbB
PbB
P�B
Q4B
QNB
QNB
Q�B
RB
Q�B
Q�B
Q�B
Q�B
RB
Q�B
Q�B
RB
RoB
R�B
S@B
S[B
SuB
S�B
S�B
S�B
S�B
T�B
UMB
U�B
U�B
VmB
V�B
V�B
W
B
W�B
X�B
X�B
X�B
YeB
Y�B
Y�B
Z�B
[�B
[qB
[=B
[�B
[qB
[qB
[WB
[�B
[qB
[�B
\)B
\�B
\�B
]IB
]~B
]~B
]~B
]IB
]/B
]�B
]�B
^B
^OB
^5B
^5B
^OB
`B
a�B
a�B
a�B
bNB
b4B
b4B
bB
bNB
bhB
b4B
a�B
a�B
bB
c:B
c�B
c�B
c�B
cTB
c�B
dB
c�B
c�B
c�B
c�B
c�B
c�B
dB
dZB
dB
d�B
eB
d�B
e,B
d�B
d�B
d�B
d�B
d�B
d�B
d�B
d�B
e`B
e�B
e�B
e�B
f�B
h>B
hXB
h�B
h�B
hsB
h>B
h$B
hXB
h
B
g�B
hsB
h�B
h�B
h�B
iyB
i�B
j0B
i�B
i�B
jB
j0B
jB
jB
j�B
k�B
lWB
l�B
l�B
m]B
n/B
n�B
n�B
oiB
o B
n}B
n/B
oOB
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
q'B
r�B
rGB
r�B
rGB
r-B
r�B
r�B
r�B
rGB
rGB
r�B
s3B
sMB
s�B
shB
tnB
tnB
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
uB
u�B
vB
v�B
vzB
vzB
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
wB
wLB
wLB
w�B
x�B
x8B
x8B
xRB
xRB
xB
x8B
xB
xB
x8B
xB
x�B
yXB
y�B
z�B
{JB
{�B
{�B
{�B
{�B
{�B
|B
|B
|6B
|6B
|6B
|6B
|�B
|�B
|6B
|PB
|6B
{�B
{�B
{�B
{B
{�B
|B
|�B
|�B
}<B
}B
}<B
}qB
}<B
|�B
}<B
}B
}"B
}"B
}qB
}<B
}VB
}�B
}�B
}�B
}�B
~B
~B
~]B
~�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B%�B&B&LB&fB&fB&LB%�B%�B%�B$�B!-B!�BVB�BqB�B�B�B�B�B�B�B:B�BTB&BB�B#�B$�B]�B	5?B	��B	�+B
 B
J�B
�B
�"B
�{B
ȚB
� B
��B
��Bw�B� B~BB�fB��B��B�=BӏB��BUB
�B�B\B�B�B�B!|BjB�B!�B/B�B�B�5B�uB��B�B��B�&B��Bw�Bm�BYBEB4�B$�B�BMB
ܬB
�B
vzB
W�B
/OB
�B	ݘB	�<B	��B	��B	��B	��B	wfB	v�B	s�B	j�B	]�B	O\B	G+B	@OB	6+B	 �B		�B	oB	xB	�B	B	�B	EB	�B	1B	B	$@B	6�B	E�B	K)B	NB	K�B	BuB	L�B	^�B	��B	�RB	��B	�#B	��B	��B	��B	��B	�hB	�XB	�tB	�RB	�xB	��B	��B	��B	��B	�=B	��B	��B	��B	�bB	�jB	��B	��B	�;B	��B	��B	��B	��B	�B	�ZB	��B	�kB	��B	�vB	�dB	��B	�B	�uB	�YB	�]B	�B	�B	�!B	��B	ۦB	�QB	�B	�eB	چB	�7B	��B	�KB	�B	�B	�tB	��B	�%B	�B	�B	�B	�B	�B	�B	�qB	�WB	�B	�B	�iB	�oB	�B	�LB	�B	��B	�}B
�B

�B
�B
�B

rB
�B
0B
�B
�B
�B
�B
6B
�B
"B
�B
�B
vB
�B
}B
�B
NB
�B
B
�B
�B
aB
{B
�B
oB
TB
TB
 B
oB
�B
�B
}B
 B
�B
�B
hB
B
�B
HB
�B
vB
�B
�B
jB
B

rB
	lB
�B
YB
�B
�B
xB
0B
B
�B
�B
�B
�B
�B
�B
�B
B

	B
�B
SB
�B
�B
{B
'B	�.B	��B	�PB	�jB	�dB	�dB	��B	�.B	�<B	��B	��B
�B
�B
uB
 �B
 OB
  B	��B	�.B	�wB	��B	�DB	�*B	�lB	��B	��B	��B	��B	��B	��B	�zB	�ZB	��B	��B	��B	��B	��B	�LB	��B	�JB	�(B
 �B
 �B
 �B
UB
-B
�B
�B
�B
{B
�B
�B
3B
B
3B
gB
%B
�B
EB
�B
�B
�B
tB
�B
YB
�B
tB
%B
�B
mB
B
�B
B
�B
KB
�B
	�B
	�B
	�B
	7B
	7B
	B
	B
	B
	B
	B
	B
	RB
	7B
�B
�B
�B
�B
fB
�B
�B
�B
+B
�B
	�B
	�B
	�B
	�B
	�B
	RB
	�B

=B
	�B
	7B
	7B
�B
B
KB
�B
+B
�B
�B
KB
B
�B
KB
	�B
	lB
	�B

=B

�B

�B

�B

rB

�B

�B
^B
�B
�B
�B
�B
�B
^B
DB
�B
~B
6B
jB
PB
6B
PB
�B
PB
�B
�B
pB
�B
pB
�B
pB
\B
�B
�B
�B
.B
�B
NB
NB
hB
hB
hB
�B
�B
�B
�B
B
�B
:B
B
aB
�B
�B
�B
FB
�B
�B
aB
�B
aB
aB
�B
[B
�B
�B
�B
�B
�B
B
�B
2B
�B
�B
�B
�B

B
sB
�B
B
EB
�B
KB
QB
7B
	B
�B
�B
WB
)B
�B
B
IB
�B
dB
IB
/B
dB
/B
/B
B
�B
dB
�B
�B
B
�B
�B
�B
pB
�B
�B
�B
�B
 �B
!-B
!HB
!|B
!�B
"�B
#nB
$B
$B
$tB
$�B
%FB
%�B
&�B
&�B
&�B
&�B
'B
&�B
'B
'8B
'�B
(
B
(sB
(XB
(>B
(�B
)yB
*B
*0B
*eB
*B
+B
*�B
+B
+QB
+6B
+�B
,qB
,�B
,�B
,�B
,�B
-wB
-�B
.cB
/OB
0;B
0�B
1B
1vB
1�B
2B
2GB
2aB
2�B
2�B
2�B
3B
3�B
4B
4B
4B
4�B
4�B
5tB
5tB
5�B
5�B
5�B
6`B
5�B
5�B
6+B
6�B
72B
7LB
7�B
7�B
7�B
7�B
7�B
7�B
7�B
88B
8B
88B
8RB
8lB
8RB
8RB
6�B
4�B
4TB
49B
4TB
5ZB
6+B
6FB
6FB
6+B
5�B
5B
4nB
4�B
4TB
4TB
4B
5ZB
5�B
5�B
6�B
8lB
8�B
8�B
8�B
8�B
8RB
8RB
8�B
9>B
9rB
9�B
9�B
:DB
:xB
:�B
:�B
;B
;0B
;dB
;JB
;�B
<�B
="B
=�B
>(B
>wB
>�B
>�B
>�B
?HB
?cB
?}B
?�B
?�B
?�B
?�B
?�B
@B
@4B
@4B
?�B
?�B
?�B
?�B
@�B
AUB
A�B
A�B
BuB
B�B
B�B
B�B
C-B
C{B
C{B
C�B
C�B
DgB
E9B
E9B
E�B
E�B
F?B
F?B
F?B
F�B
GB
GEB
GEB
G�B
IB
KB
K^B
K�B
LB
L0B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
MB
M6B
M6B
M�B
M�B
NB
N"B
N�B
OB
N�B
N�B
N�B
N�B
O(B
OBB
O\B
OvB
OBB
O�B
OvB
O�B
P.B
P}B
O�B
P}B
PbB
PbB
P�B
Q4B
QNB
QNB
Q�B
RB
Q�B
Q�B
Q�B
Q�B
RB
Q�B
Q�B
RB
RoB
R�B
S@B
S[B
SuB
S�B
S�B
S�B
S�B
T�B
UMB
U�B
U�B
VmB
V�B
V�B
W
B
W�B
X�B
X�B
X�B
YeB
Y�B
Y�B
Z�B
[�B
[qB
[=B
[�B
[qB
[qB
[WB
[�B
[qB
[�B
\)B
\�B
\�B
]IB
]~B
]~B
]~B
]IB
]/B
]�B
]�B
^B
^OB
^5B
^5B
^OB
`B
a�B
a�B
a�B
bNB
b4B
b4B
bB
bNB
bhB
b4B
a�B
a�B
bB
c:B
c�B
c�B
c�B
cTB
c�B
dB
c�B
c�B
c�B
c�B
c�B
c�B
dB
dZB
dB
d�B
eB
d�B
e,B
d�B
d�B
d�B
d�B
d�B
d�B
d�B
d�B
e`B
e�B
e�B
e�B
f�B
h>B
hXB
h�B
h�B
hsB
h>B
h$B
hXB
h
B
g�B
hsB
h�B
h�B
h�B
iyB
i�B
j0B
i�B
i�B
jB
j0B
jB
jB
j�B
k�B
lWB
l�B
l�B
m]B
n/B
n�B
n�B
oiB
o B
n}B
n/B
oOB
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
q'B
r�B
rGB
r�B
rGB
r-B
r�B
r�B
r�B
rGB
rGB
r�B
s3B
sMB
s�B
shB
tnB
tnB
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
uB
u�B
vB
v�B
vzB
vzB
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
wB
wLB
wLB
w�B
x�B
x8B
x8B
xRB
xRB
xB
x8B
xB
xB
x8B
xB
x�B
yXB
y�B
z�B
{JB
{�B
{�B
{�B
{�B
{�B
|B
|B
|6B
|6B
|6B
|6B
|�B
|�B
|6B
|PB
|6B
{�B
{�B
{�B
{B
{�B
|B
|�B
|�B
}<B
}B
}<B
}qB
}<B
|�B
}<B
}B
}"B
}"B
}qB
}<B
}VB
}�B
}�B
}�B
}�B
~B
~B
~]B
~�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604104844  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604172334  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604172334  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604172334                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605022341  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605022341  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610121506                      G�O�G�O�G�O�                