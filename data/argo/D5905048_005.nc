CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2016-06-24T09:48:36Z creation;2016-06-24T09:48:38Z conversion to V3.1;2019-12-19T08:34:56Z update;     
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
_FillValue                 �  IL   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M8   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tp   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �X   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �D   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  Τ   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �T   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �d   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �h   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �l   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �p   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �t   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20160624094836  20200116201515  5905048                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               A   JA  I2_0577_005                     2C  D   NAVIS_A                         0577                            ARGO 102115                     863 @ײ&�q� 1   @ײ'I���@4��`A��d�+I�1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @9��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"�C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Do��Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D��f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@:�H@���@���A Q�A Q�A@Q�A`Q�A�(�A�(�A�(�A�(�A�(�A�(�A�(�A�(�B {B{B{B{B {B({B0{B8{B@{BH{BP{BX{B`{Bh{Bp{Bx{B�
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
=B�=qB�
=B�
=B�
=B�
=B�
=C CCCCC
CCCCCCCCCCC C"�C$C&C(C*C,C.C0C2C4C6C8C:C<C>C@CBCDCFCHCJCLCNCPCRCTCVCXCZC\C^C`CbCdCfChCjClCnCpCrCtCvCxCzC|C~C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�\C�\C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C���C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D HD �HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HD	HD	�HD
HD
�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD��DHD�HDHD�HD HD �HD!HD!�HD"HD"�HD#HD#�HD$HD$�HD%HD%�HD&HD&�HD'HD'�HD(HD(�HD)HD)�HD*HD*�HD+HD+�HD,HD,�HD-HD-�HD.HD.�HD/HD/�HD0HD0�HD1HD1�HD2HD2�HD3HD3�HD4HD4�HD5HD5�HD6HD6�HD7HD7�HD8HD8�HD9HD9�HD:HD:�HD;HD;�HD<HD<�HD=HD=�HD>HD>�HD?HD?�HD@HD@�HDAHDA�HDBHDB�HDCHDC�HDDHDD�HDEHDE�HDFHDF�HDGHDG�HDHHDH�HDIHDI�HDJHDJ�HDKHDK�HDLHDL�HDMHDM�HDNHDN�HDOHDO�HDPHDP�HDQHDQ�HDRHDR�HDSHDS�HDTHDT�HDUHDU�HDVHDV�HDWHDW�HDXHDX�HDYHDY�HDZHDZ�HD[HD[�HD\HD\�HD]HD]�HD^HD^�HD_HD_�HD`HD`�HDaHDa�HDbHDb�HDcHDc�HDdHDd�HDeHDe�HDfHDf�HDgHDg�HDhHDh�HDiHDi�HDjHDj�HDkHDk�HDlHDl�HDmHDm�HDnHDn�HDoHDo�HDo��Dp�HDqHDq�HDrHDr�HDsHDs�HDtHDt�HDuHDu�HDvHDv�HDwHDw�HDxHDx�HDyHDy�HDzHDz�HD{HD{�HD|HD|�HD}HD}�HD~HD~�HDHD�HD� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D�D���D� �D�@�DÀ�D���D� �D�@�DĀ�D���D� �D�@�Dŀ�D���D� �D�@�Dƀ�D���D� �D�@�Dǀ�D���D� �D�@�DȀ�D���D� �D�@�Dɀ�D���D� �D�@�Dʀ�D���D� �D�@�Dˀ�D���D� �D�@�D̀�D���D� �D�@�D̀�D���D� �D�@�D΀�D���D� �D�@�Dπ�D���D� �D�@�DЀ�D���D� �D�@�Dр�D���D� �D�@�DҀ�D���D� �D�@�DӀ�D���D� �D�@�DԀ�D���D� �D�@�DՀ�D���D� �D�@�Dր�D���D� �D�@�D׀�D���D� �D�@�D؀�D���D� �D�@�Dـ�D���D� �D�@�Dڀ�D���D� �D�@�Dۀ�D���D� �D�@�D܀�D���D� �D�@�D݀�D���D� �D�@�Dހ�D���D� �D�@�D߀�D���D� �D�@�D���D���D� �D�@�DဤD���D� �D�@�D․D���D� �D�@�D〤D���D� �D�@�D䀤D���D� �D�@�D値D���D� �D�@�D怤D���D� �D�@�D瀤D���D� �D�@�D耤D���D� �D�@�D逤D���D� �D�@�DꀤD���D� �D�@�D뀤D���D� �D�@�D쀤D���D� �D�@�D퀤D���D� �D�@�DD���D� �D�@�DD���D� �D�@�D���D���D� �D�@�D�D���D� �D�@�D�D���D� �D�@�D�D���D� �D�@�D�D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D��
11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�bA�JA�oA�{A��A��A��A��A��A��A��A��A��A��A��A��A��A�JA��yA��A�x�A�A�bNAӥ�AӋDA�p�A�9XAҮA�A�/A��AЅA�M�A�&�A���Aͥ�A� �Ȧ+A�\)A�JA�x�A�VA��#Aɩ�A�5?A��A���A�\)A��A�VA�VA��A�;dA�A�A���A��\A���A��RA��7A��wA��wA�"�A�ȴA��DA�{A�G�A�VA�jA���A��yA�A�?}A��A�  A�VA���A���A��;A�x�A�ffA�-A���A�{A���A��RA���A�XA��A�=qA��/A��A�bNA�~�A��9A��9A�ȴA��yA���A�{A�ĜA���A�z�A�p�A�^5A�5?A���A�&�A��TA�oA��yA��jA�v�A�?}A��;A�G�A{��A{�^Az�RAz �Az^5Axv�AtZAr-Aq\)Ao�wAo+An�Aj�Ah=qAf��Aa��A_A]VAZ�!AY�AX(�AU�
ATAS&�AQ�#AP�AN^5AM��AL�AJ��AIAGXAE\)AD��ADffAC��AB-A?x�A=��A;��A:��A8�A6��A4��A3��A2��A1
=A/��A-/A+��A+K�A*v�A(�A'ƨA'dZA&1'A$��A$5?A#x�A"ĜA!�A�mA�^A�A(�A{AE�A9XA��A��Ar�AdZA�RA{A/AG�AȴA�+A�Al�AZA��A�hA�#A
A�A�A��A{A�yA�A��A�A�7A��A��AG�AdZA
=A �/@��@��@��\@��#@�r�@�~�@�X@�1@��@�^5@���@�F@���@�dZ@��^@��@�9X@�K�@�V@�7@��@���@�C�@�{@�G�@���@�%@��
@�hs@�@�M�@�9@�v�@�r�@��@�dZ@�"�@�r�@�V@���@�r�@�b@�1@ԋD@Ӿw@�@���@У�@��@�hs@�hs@�Z@ϥ�@��@�ȴ@���@�p�@��@̴9@�bN@�`B@ʰ!@�ƨ@�-@Ɵ�@�=q@Ų-@Å@�=q@���@�b@�o@��R@���@�$�@�"�@���@��@���@�p�@�E�@§�@��@�&�@��F@��@��@��@���@�&�@���@��@��#@��@�J@�33@�j@�Z@��w@�|�@���@���@���@� �@�9X@�b@���@�ƨ@�
=@�x�@���@�Ĝ@��/@��@���@��9@���@���@�l�@�"�@���@��@��!@�5?@��@��T@�@�p�@�O�@�V@��9@�j@�I�@�ƨ@�=q@���@��u@��9@�Ĝ@��/@�bN@��@�M�@�J@���@�-@��^@��7@�p�@���@�G�@�%@���@� �@�|�@�33@���@��`@��F@�j@�/@�9X@�b@���@���@���@�+@��T@�G�@�Z@�dZ@�\)@��w@�9X@�Z@��m@�\)@���@��9@�(�@�K�@��@�@��@��H@��R@���@�@�+@�
=@�~�@�~�@�ff@�$�@���@�Q�@��\@�V@���@���@���@�n�@�K�@�K�@�v�@��@���@��T@�&�@��@��/@��@�r�@�9X@�  @���@���@�"�@���@��@���@�?}@��`@�bN@��m@��P@�l�@�o@��@���@�v�@�M�@���@�x�@�%@�Ĝ@��@��
@�o@��T@�&�@���@�r�@�(�@�  @�ƨ@��@���@���@���@�|�@���@�n�@��R@�ȴ@��H@��@�@��@�E�@�J@��^@���@�x�@���@��u@��D@�Z@�I�@�9X@��@���@�  @���@��P@�+@�o@��@��R@�~�@�=q@�J@���@��@�?}@�V@��j@�Z@���@��w@�\)@�33@�33@�+@�"�@��@���@��+@�~�@�v�@�ff@�M�@�@��@��@���@��@��9@�r�@��@��m@�l�@�"�@�@��!@�=q@�$�@�$�@�$�@�{@��-@�7L@�&�@�%@��9@�z�@�9X@��@�@;d@~ff@}�T@}O�@}?}@}V@|�/@|z�@{�m@{�F@{�@z��@zM�@y�^@y7L@y%@xr�@w�P@v��@u��@u�@uO�@uO�@uO�@u`B@u`B@u?}@u�@uV@t�@t�j@tZ@s�m@s��@sS�@r�H@q�@p�`@pQ�@o�@o\)@o
=@nȴ@n��@nv�@n$�@n@m�@m�-@mV@l�j@lz�@l�@k�@j�H@j�\@i��@hĜ@g��@g\)@gK�@g
=@fV@e�h@e�@e�@e`B@eV@d��@dj@d�@c��@cC�@bn�@a��@ahs@`��@`��@`�u@`bN@`1'@_�P@_�@^�R@^ff@^5?@]�T@]�h@]O�@\�D@\I�@\9X@\1@[�
@[�F@[S�@[C�@[33@["�@["�@[@Z�H@Y��@X�`@X��@X�9@XbN@Xr�@XbN@X �@Wl�@V�y@U��@UO�@U�@T�@Tj@T(�@T�@T�@T1@S�m@St�@S@R^5@Q�@Q�^@Q��@Q�7@Qx�@QG�@Q%@P��@P��@P��@PĜ@P�u@PA�@O��@N�R@N�+@NV@NE�@N5?@MO�@L�j@L�@L�@K�F@K"�@K@J�@J�H@J��@J��@J~�@I�@H��@H�u@H  @G�;@G��@G��@G\)@F�y@F��@FE�@E@EV@C�
@C33@B��@B��@B~�@B^5@BM�@B=q@A��@AX@@��@@r�@@A�@?��@?�w@?�w@?�@?�@?�P@?�@>5?@=p�@=V@<�/@<�@;�
@;ƨ@;��@;33@:�@:��@9�@9X@8��@8�`@8��@8Ĝ@8�9@8r�@8Q�@7�;@7��@7�P@6��@6�@6��@5�T@5`B@4�@4�j@4z�@4Z@3�m@3��@3dZ@3"�@3o@2�@2��@2M�@1�@1��@1hs@17L@1&�@0��@0Ĝ@0�u@0Q�@0b@/�;@/�w@/�P@.�y@.5?@-��@-`B@,��@,�@,�/@,��@,z�@+�
@+t�@*�H@*n�@*M�@*=q@*=q@)��@)��@)�7@)�7@)x�@)X@)&�@(�`@(�9@(�@(Q�@(b@'\)@&�y@&ȴ@&�+@&ff@&5?@&@%�T@%�T@%@%�@$��@$�j@$��@$�D@$j@$Z@$9X@$1@#ƨ@#��@#dZ@#o@"�\@"~�@"n�@"-@!��@!�#@!x�@!�@ ��@   @l�@\)@K�@;d@+@�@E�@$�@{@{@�T@��@�-@��@��@�@?}@�D@I�@(�@1@ƨ@�@dZ@"�@��@^5@-@J@��@X@G�@&�@�@��@��@�u@Q�@A�@�@��@�w@�w@�w@�w@�w@�w@�w@�@��@�P@K�@��@v�@E�@E�@{@@��@p�@��@Z@�@1@�m@�F@��@dZ@S�@C�@o@�!@^5@�@��@��@�^@��@hs@7L@7L@�`@�u@r�@A�@1'@ �@�;@�@l�@K�@K�@��@��@ff@V@E�@5?@{@�@��@@�-@�-@��@p�@`B@O�@?}@/@V@�/@z�@Z@�@�
@�F@�@S�@"�@
�@
��@
~�@
�@	��@	��@	��@	hs@	X@	7L@	&�@	%@Ĝ@�u@r�@Q�@A�@ �@  @��@�@��@�P@|�@K�@�@�y@��@ff@ff@{@�-@@�-@�@O�@O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�bA�JA�oA�{A��A��A��A��A��A��A��A��A��A��A��A��A��A�JA��yA��A�x�A�A�bNAӥ�AӋDA�p�A�9XAҮA�A�/A��AЅA�M�A�&�A���Aͥ�A� �Ȧ+A�\)A�JA�x�A�VA��#Aɩ�A�5?A��A���A�\)A��A�VA�VA��A�;dA�A�A���A��\A���A��RA��7A��wA��wA�"�A�ȴA��DA�{A�G�A�VA�jA���A��yA�A�?}A��A�  A�VA���A���A��;A�x�A�ffA�-A���A�{A���A��RA���A�XA��A�=qA��/A��A�bNA�~�A��9A��9A�ȴA��yA���A�{A�ĜA���A�z�A�p�A�^5A�5?A���A�&�A��TA�oA��yA��jA�v�A�?}A��;A�G�A{��A{�^Az�RAz �Az^5Axv�AtZAr-Aq\)Ao�wAo+An�Aj�Ah=qAf��Aa��A_A]VAZ�!AY�AX(�AU�
ATAS&�AQ�#AP�AN^5AM��AL�AJ��AIAGXAE\)AD��ADffAC��AB-A?x�A=��A;��A:��A8�A6��A4��A3��A2��A1
=A/��A-/A+��A+K�A*v�A(�A'ƨA'dZA&1'A$��A$5?A#x�A"ĜA!�A�mA�^A�A(�A{AE�A9XA��A��Ar�AdZA�RA{A/AG�AȴA�+A�Al�AZA��A�hA�#A
A�A�A��A{A�yA�A��A�A�7A��A��AG�AdZA
=A �/@��@��@��\@��#@�r�@�~�@�X@�1@��@�^5@���@�F@���@�dZ@��^@��@�9X@�K�@�V@�7@��@���@�C�@�{@�G�@���@�%@��
@�hs@�@�M�@�9@�v�@�r�@��@�dZ@�"�@�r�@�V@���@�r�@�b@�1@ԋD@Ӿw@�@���@У�@��@�hs@�hs@�Z@ϥ�@��@�ȴ@���@�p�@��@̴9@�bN@�`B@ʰ!@�ƨ@�-@Ɵ�@�=q@Ų-@Å@�=q@���@�b@�o@��R@���@�$�@�"�@���@��@���@�p�@�E�@§�@��@�&�@��F@��@��@��@���@�&�@���@��@��#@��@�J@�33@�j@�Z@��w@�|�@���@���@���@� �@�9X@�b@���@�ƨ@�
=@�x�@���@�Ĝ@��/@��@���@��9@���@���@�l�@�"�@���@��@��!@�5?@��@��T@�@�p�@�O�@�V@��9@�j@�I�@�ƨ@�=q@���@��u@��9@�Ĝ@��/@�bN@��@�M�@�J@���@�-@��^@��7@�p�@���@�G�@�%@���@� �@�|�@�33@���@��`@��F@�j@�/@�9X@�b@���@���@���@�+@��T@�G�@�Z@�dZ@�\)@��w@�9X@�Z@��m@�\)@���@��9@�(�@�K�@��@�@��@��H@��R@���@�@�+@�
=@�~�@�~�@�ff@�$�@���@�Q�@��\@�V@���@���@���@�n�@�K�@�K�@�v�@��@���@��T@�&�@��@��/@��@�r�@�9X@�  @���@���@�"�@���@��@���@�?}@��`@�bN@��m@��P@�l�@�o@��@���@�v�@�M�@���@�x�@�%@�Ĝ@��@��
@�o@��T@�&�@���@�r�@�(�@�  @�ƨ@��@���@���@���@�|�@���@�n�@��R@�ȴ@��H@��@�@��@�E�@�J@��^@���@�x�@���@��u@��D@�Z@�I�@�9X@��@���@�  @���@��P@�+@�o@��@��R@�~�@�=q@�J@���@��@�?}@�V@��j@�Z@���@��w@�\)@�33@�33@�+@�"�@��@���@��+@�~�@�v�@�ff@�M�@�@��@��@���@��@��9@�r�@��@��m@�l�@�"�@�@��!@�=q@�$�@�$�@�$�@�{@��-@�7L@�&�@�%@��9@�z�@�9X@��@�@;d@~ff@}�T@}O�@}?}@}V@|�/@|z�@{�m@{�F@{�@z��@zM�@y�^@y7L@y%@xr�@w�P@v��@u��@u�@uO�@uO�@uO�@u`B@u`B@u?}@u�@uV@t�@t�j@tZ@s�m@s��@sS�@r�H@q�@p�`@pQ�@o�@o\)@o
=@nȴ@n��@nv�@n$�@n@m�@m�-@mV@l�j@lz�@l�@k�@j�H@j�\@i��@hĜ@g��@g\)@gK�@g
=@fV@e�h@e�@e�@e`B@eV@d��@dj@d�@c��@cC�@bn�@a��@ahs@`��@`��@`�u@`bN@`1'@_�P@_�@^�R@^ff@^5?@]�T@]�h@]O�@\�D@\I�@\9X@\1@[�
@[�F@[S�@[C�@[33@["�@["�@[@Z�H@Y��@X�`@X��@X�9@XbN@Xr�@XbN@X �@Wl�@V�y@U��@UO�@U�@T�@Tj@T(�@T�@T�@T1@S�m@St�@S@R^5@Q�@Q�^@Q��@Q�7@Qx�@QG�@Q%@P��@P��@P��@PĜ@P�u@PA�@O��@N�R@N�+@NV@NE�@N5?@MO�@L�j@L�@L�@K�F@K"�@K@J�@J�H@J��@J��@J~�@I�@H��@H�u@H  @G�;@G��@G��@G\)@F�y@F��@FE�@E@EV@C�
@C33@B��@B��@B~�@B^5@BM�@B=q@A��@AX@@��@@r�@@A�@?��@?�w@?�w@?�@?�@?�P@?�@>5?@=p�@=V@<�/@<�@;�
@;ƨ@;��@;33@:�@:��@9�@9X@8��@8�`@8��@8Ĝ@8�9@8r�@8Q�@7�;@7��@7�P@6��@6�@6��@5�T@5`B@4�@4�j@4z�@4Z@3�m@3��@3dZ@3"�@3o@2�@2��@2M�@1�@1��@1hs@17L@1&�@0��@0Ĝ@0�u@0Q�@0b@/�;@/�w@/�P@.�y@.5?@-��@-`B@,��@,�@,�/@,��@,z�@+�
@+t�@*�H@*n�@*M�@*=q@*=q@)��@)��@)�7@)�7@)x�@)X@)&�@(�`@(�9@(�@(Q�@(b@'\)@&�y@&ȴ@&�+@&ff@&5?@&@%�T@%�T@%@%�@$��@$�j@$��@$�D@$j@$Z@$9X@$1@#ƨ@#��@#dZ@#o@"�\@"~�@"n�@"-@!��@!�#@!x�@!�@ ��@   @l�@\)@K�@;d@+@�@E�@$�@{@{@�T@��@�-@��@��@�@?}@�D@I�@(�@1@ƨ@�@dZ@"�@��@^5@-@J@��@X@G�@&�@�@��@��@�u@Q�@A�@�@��@�w@�w@�w@�w@�w@�w@�w@�@��@�P@K�@��@v�@E�@E�@{@@��@p�@��@Z@�@1@�m@�F@��@dZ@S�@C�@o@�!@^5@�@��@��@�^@��@hs@7L@7L@�`@�u@r�@A�@1'@ �@�;@�@l�@K�@K�@��@��@ff@V@E�@5?@{@�@��@@�-@�-@��@p�@`B@O�@?}@/@V@�/@z�@Z@�@�
@�F@�@S�@"�@
�@
��@
~�@
�@	��@	��@	��@	hs@	X@	7L@	&�@	%@Ĝ@�u@r�@Q�@A�@ �@  @��@�@��@�P@|�@K�@�@�y@��@ff@ff@{@�-@@�-@�@O�@O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B!�B!�B!�B!�B"�B"�B!�B"�B"�B!�B"�B"�B"�B"�B"�B"�B"�B"�B#�B0!B@�BA�BC�BD�BD�BD�BE�BH�BL�BS�BVB[#Bm�B~�B�B�B�1B�\B��B��B��BƨB��B��B�/B�BB�B�B-B49B7LB@�BC�BT�BdZBo�BgmBbNBhsBr�Bz�B�B�B�B�B�B�B�B�B{�Bp�Bn�Bm�BbNBXBJ�B@�B,B�BB�B�NB��BĜB�RB��B�+Br�BhsBiyBe`B\)BQ�BE�B9XB)�B�B+B
�B
�#B
�
B
��B
��B
��B
��B
��B
B
�wB
�9B
��B
�uB
�+B
r�B
_;B
R�B
7LB
G�B
E�B
P�B
ffB
]/B
=qB
)�B
#�B
�B
uB
\B	��B	�5B	��B	�B	��B	�PB	}�B	t�B	n�B	bNB	T�B	O�B	G�B	A�B	8RB	8RB	8RB	-B	$�B	oB	PB	PB	JB	
=B	B�B�B�HB�/B��B��B��BǮBȴBƨB��B�qB�FB�9B�-B�'B�B�B�B�B�B�B�B�3B�3B�3B�RB�LB�^B�qBÖBǮBƨBƨBŢBŢBŢB��B�
B�#B�BB�fB�yB�yB�fB�;B��BƨB�FB�'B�B�B�'B�B�RB�RB�9B�3B�9B�dB�jB�qB�dB�RB�LB�?B�'B�B��B��B�B�B��B�B�B�B�RB�3B�B�B�B�B�B��B��B��B��B�uB�VB�JB�B��B��B�dB�B��B��B�B�B��B��B��B��B��B��B�B�!B�B�B�?B�dB�qBÖBBŢBǮBȴBȴB��B�B�BB�sB�B�B�HB�;B�ZB�ZB�TB�HB�)B�#B�#B�)B�/B�HB�`B�B�B��B��B	  B	
=B	{B	�B	�B	�B	�B	"�B	(�B	(�B	'�B	+B	,B	6FB	F�B	D�B	P�B	^5B	cTB	aHB	bNB	hsB	l�B	n�B	q�B	r�B	r�B	r�B	u�B	x�B	x�B	z�B	}�B	~�B	� B	�B	�%B	�%B	�1B	�JB	�JB	�DB	�DB	�JB	�PB	�JB	�PB	�\B	�bB	�\B	�bB	�bB	�hB	�hB	�hB	�VB	�DB	�=B	�DB	�PB	�\B	�uB	��B	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�B	�B	��B	��B	�B	�FB	�FB	�XB	��B	ÖB	ÖB	ÖB	��B	�}B	��B	�}B	��B	ĜB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�#B	�#B	�/B	�;B	�BB	�5B	�B	��B	��B	ǮB	��B	��B	�BB	�sB	�yB	�sB	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B	��B
  B
B
B
B
%B
+B
+B
	7B
	7B

=B
	7B

=B
	7B

=B

=B
DB
DB
DB
DB
DB
DB
DB
DB

=B

=B

=B

=B

=B

=B
	7B
	7B
	7B

=B
	7B
	7B
	7B
	7B
1B
	7B
	7B
	7B
	7B
1B
1B
+B
1B
1B
1B
	7B
DB
DB
DB
DB
DB
DB
DB
DB
DB
DB
PB
VB
\B
\B
bB
bB
bB
bB
bB
oB
uB
uB
uB
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
!�B
!�B
!�B
!�B
!�B
!�B
!�B
!�B
!�B
!�B
"�B
"�B
"�B
"�B
#�B
$�B
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
'�B
(�B
'�B
'�B
(�B
(�B
)�B
)�B
+B
+B
,B
,B
,B
-B
.B
.B
.B
.B
.B
.B
.B
.B
.B
.B
.B
.B
.B
/B
/B
/B
/B
/B
/B
0!B
0!B
0!B
1'B
1'B
1'B
1'B
1'B
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
33B
5?B
6FB
6FB
6FB
7LB
7LB
8RB
8RB
9XB
8RB
8RB
9XB
9XB
:^B
9XB
9XB
9XB
9XB
9XB
9XB
9XB
9XB
:^B
:^B
:^B
:^B
:^B
:^B
;dB
;dB
;dB
;dB
;dB
;dB
;dB
;dB
<jB
=qB
=qB
=qB
>wB
>wB
?}B
?}B
?}B
?}B
?}B
?}B
?}B
?}B
?}B
?}B
?}B
@�B
@�B
A�B
A�B
A�B
A�B
A�B
A�B
B�B
B�B
B�B
B�B
C�B
C�B
D�B
E�B
E�B
E�B
E�B
F�B
F�B
E�B
F�B
F�B
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
J�B
J�B
K�B
K�B
K�B
K�B
K�B
K�B
L�B
L�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
N�B
N�B
N�B
N�B
O�B
O�B
O�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
R�B
R�B
R�B
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
T�B
VB
VB
W
B
W
B
W
B
XB
XB
XB
XB
XB
YB
YB
ZB
ZB
ZB
ZB
ZB
[#B
[#B
[#B
[#B
[#B
[#B
[#B
[#B
[#B
\)B
\)B
\)B
]/B
]/B
]/B
]/B
]/B
]/B
^5B
^5B
^5B
^5B
^5B
_;B
_;B
_;B
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
bNB
cTB
dZB
e`B
e`B
ffB
ffB
ffB
ffB
gmB
hsB
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
k�B
k�B
k�B
k�B
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
p�B
p�B
p�B
o�B
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
x�B
x�B
x�B
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
z�B
z�B
z�B
z�B
{�B
{�B
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
� B
� B
� B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B!�B!�B!�B!�B"�B"�B!�B"�B"�B!�B"�B"�B"�B"�B"�B"�B# B#�B%zB1vBAoBB�BD�BD�BEBESBF�BJ#BNVBT�BWYB]�Bo�B�B��B�9B�RB��B�B�OB�XB�B�xB�oB߾B�aB�BqB�B.}B5?B8�BA�BFBW�BgmBsBi�Be`BlWBvFB|6B��B�SB��B��B�B�3B��B��B~(Br-Bp�Bp�Bd�BZ�BL�BCaB/�B�B3B�B�B�B��B�"B�_B��Bt�Bi�BkQBg8B^OBS�BHB;�B,�B$BB
��B
ۦB
�YB
�2B
�FB
ӏB
� B
�6B
��B
��B
�B
��B
��B
�XB
vFB
cTB
W
B
8B
H�B
FtB
Q�B
iB
abB
?�B
+kB
%�B
�B
B
�B	�*B	�bB	�B	��B	��B	�B	� B	vzB	qAB	dZB	VmB	Q�B	IlB	C�B	9�B	:B	:�B	.�B	'�B	�B	"B	B	�B	�B	?B�%B��B�TB�'B��B�PB�JB�7B��B��B�MB��B��B��B�9B�GB�B��B��B�B�/B�WB��B��B��B�9B�>B��B�xB�(BŢBɆBǮB��BƨB��B�+B��B��B��B�-B�mB�B�B�DB�4B�KBɆB��B�GB��B��B��B�B�XB��B�ZB��B�TB��B��B�wB�B��B�B�`B�aB��B��B��B��B��B��B��B��B��B�xB�nB��B��B��B��B��B��B��B��B��B�{B�"B��B��B� B��B��B�UB�yB�B��B��B�QB��B�-B� B��B��B��B��B��B��B�%B�JB��B�MB�B�B��B�BɆB�)B�9B�vB�sB�vB�=B�B�;B�B��B�B�B��B�)B��B�xB�IB�bB��B�)B�-B��B��B��B	
=B	�B	9B	9B	�B	�B	#B	)�B	)�B	($B	+B	+QB	5�B	F�B	DB	PbB	^OB	c�B	a|B	bNB	hsB	lWB	n}B	q�B	r�B	r�B	sB	vzB	y�B	y>B	z�B	}�B	~�B	�4B	�[B	��B	�?B	�fB	�~B	�dB	�xB	�^B	��B	��B	�dB	�jB	��B	��B	��B	��B	��B	��B	�B	�oB	�B	�^B	�=B	�DB	�jB	��B	�aB	��B	��B	��B	�MB	��B	��B	��B	�B	��B	�*B	�]B	�}B	��B	�oB	��B	�/B	�yB	��B	��B	��B	�`B	�$B	� B	��B	�B	�gB	�B	�B	�B	�}B	�OB	�gB	��B	�BB	�}B	��B	ЗB	�VB	�hB	�B	��B	�B	�B	�B	��B	��B	�B	�KB	�qB	�=B	�IB	ߊB	��B	�;B	�QB	��B	�dB	�_B	�	B	�,B	��B	�B	��B	�B	�B	�B	��B	�B	�B	��B	�B	�B	��B	��B	��B	�B	�%B	�+B	�2B	�B	�B	�2B	�2B	�B	��B	�B	��B	�B	�B	�B	�<B	�VB	�BB	�.B	�BB	�wB	��B	��B	�^B	�B	�B	��B	��B	�B	�B	��B	�B	��B	�.B
oB	�B	��B
�B
B
B
%B
_B
�B
	RB
	lB

XB
	RB

�B
	RB

=B

XB
^B
^B
DB
^B
DB
^B
�B
xB

XB

XB

rB

XB

rB

XB
	�B
	lB
	lB

XB
	�B
	�B
	�B
	lB
fB
	7B
	7B
	RB
	RB
fB
KB
EB
1B
KB
1B
	lB
�B
xB
xB
^B
^B
xB
xB
�B
xB
�B
�B
�B
�B
�B
}B
bB
}B
}B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
!B
 B
 �B
!�B
!�B
!�B
!�B
!�B
!�B
!�B
!�B
!�B
!�B
!�B
!�B
#B
"�B
#B
#B
$&B
%,B
&B
&B
'B
'B
'B
&�B
'B
'B
(
B
(
B
'�B
($B
)B
(
B
($B
)*B
)*B
*0B
*eB
+6B
+QB
,"B
,"B
,=B
-CB
.IB
.B
-�B
./B
./B
./B
.IB
./B
./B
.cB
.cB
.IB
.IB
/OB
/B
/5B
/5B
/5B
/OB
0UB
0UB
0;B
1AB
1AB
1AB
1[B
1[B
2GB
2GB
2-B
2-B
3MB
3MB
33B
33B
33B
33B
3MB
3hB
3�B
5tB
6FB
6FB
6`B
7LB
7fB
8�B
8�B
9�B
8�B
8lB
9rB
9rB
:xB
9rB
9XB
9XB
9rB
9rB
9rB
9�B
9�B
:xB
:xB
:xB
:DB
:xB
:xB
;B
;dB
;B
;JB
;B
;dB
;B
;�B
<�B
=�B
=�B
=�B
>�B
>�B
?�B
?}B
?�B
?�B
?�B
?�B
?}B
?cB
?�B
?�B
?�B
@�B
@�B
A�B
A�B
A�B
A�B
A�B
A�B
B�B
B�B
B�B
B�B
C�B
C�B
D�B
E�B
E�B
E�B
E�B
F�B
F�B
E�B
F�B
F�B
G�B
G�B
G�B
H�B
H�B
H�B
H�B
H�B
H�B
IB
J	B
I�B
J�B
J�B
K�B
K�B
K�B
K�B
K�B
K�B
MB
MB
M�B
M�B
M�B
M�B
M�B
M�B
M�B
N�B
N�B
N�B
OB
O�B
PB
P.B
QB
QB
Q B
RB
RB
R B
RB
SB
SB
S&B
SB
SB
SB
TB
TB
TB
TB
TB
TB
UB
UB
UB
UB
T�B
T�B
UB
VSB
VSB
W$B
W?B
W$B
XB
X+B
X+B
XEB
XEB
YKB
YKB
Z7B
Z7B
ZB
Z7B
Z7B
[#B
[	B
[#B
[#B
[=B
[#B
[#B
[=B
[=B
\CB
\CB
\]B
]/B
]/B
]IB
]/B
]IB
]IB
^B
^5B
^5B
^OB
^OB
_;B
_VB
_;B
_VB
_;B
_VB
_VB
_VB
_;B
`\B
`\B
`vB
aHB
abB
abB
abB
abB
b�B
b�B
c�B
d�B
e�B
e`B
ffB
ffB
f�B
f�B
g�B
h�B
iyB
i_B
i�B
iyB
i�B
jB
jeB
j�B
jB
j�B
k�B
k�B
k�B
k�B
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
p�B
p�B
p�B
o�B
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
x�B
x�B
x�B
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
z�B
z�B
z�B
z�B
{�B
|B
|B
|B
}B
}B
}B
}B
}B
}B
|�B
}B
}B
}�B
~B
}�B
~B
B
B
B
B
~�B
B
B
B
B
� B
� B
�B
�B
�4B
� B
�B
� B
�B
� B
�B
�B
�B
�B
� B
�B
�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=-0.02(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201606120037152016061200371520160612003715201806221257392018062212573920180622125739201804050655562018040506555620180405065556  JA  ARFMdecpA19c                                                                20160624183541  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20160624094836  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20160624094837  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20160624094837  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20160624094838  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20160624094838  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20160624094838  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20160624094838  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20160624094838  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20160624094838                      G�O�G�O�G�O�                JA  ARUP                                                                        20160624102538                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20160608153402  CV  JULD            G�O�G�O�F��5                JM  ARCAJMQC2.0                                                                 20160611153715  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20160611153715  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404215556  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622035739  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200116201515                      G�O�G�O�G�O�                