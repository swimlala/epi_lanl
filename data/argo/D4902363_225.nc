CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-04-02T03:37:33Z creation;2018-04-02T03:37:38Z conversion to V3.1;2019-12-19T07:45:48Z update;     
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20180402033733  20200115121517  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0576_225                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @�W��& 1   @�W��b��@:
=p���dlV�u1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C#�fC&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<�fD=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�3D�C3D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�3D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D���D�� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��
@���A Q�A Q�A@Q�A`Q�A�(�A�(�A�(�A�(�A�(�A�(�A�(�A�(�B {B{B{B{B {B({B0{B8{B@{BH{BP{BX{B`{Bh{Bp{Bx{B�
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
CCCCCCCCCCC C"C#�C&C(C*C,C.C0C2C4C6C8C:C<C>C@CBCDCFCHCJCLCNCPCRCTCVCXCZC\C^C`CbCdCfChCjClCnCpCrCtCvCxCzC|C~C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�\C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D HD �HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HD	HD	�HD
HD
�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HD HD �HD!HD!�HD"HD"�HD#HD#�HD$HD$�HD%HD%�HD&HD&�HD'HD'�HD(HD(�HD)HD)�HD*HD*�HD+HD+�HD,HD,�HD-HD-�HD.HD.�HD/HD/�HD0HD0�HD1HD1�HD2HD2�HD3HD3�HD4HD4�HD5HD5�HD6HD6�HD7HD7�HD8HD8�HD9HD9�HD:HD:�HD;HD;�HD<HD<��D=HD=�HD>HD>�HD?HD?�HD@HD@�HDAHDA�HDBHDB�HDCHDC�HDDHDD�HDEHDE�HDFHDF�HDGHDG�HDHHDH�HDIHDI�HDJHDJ�HDKHDK�HDLHDL�HDMHDM�HDNHDN�HDOHDO�HDPHDP�HDQHDQ�HDRHDR�HDSHDS�HDTHDT�HDUHDU�HDVHDV�HDWHDW�HDXHDX�HDYHDY�HDZHDZ�HD[HD[�HD\HD\�HD]HD]�HD^HD^�HD_HD_�HD`HD`�HDaHDa�HDbHDb�HDcHDc�HDdHDd�HDeHDe�HDfHDf�HDgHDg�HDhHDh�HDiHDi�HDjHDj�HDkHDk�HDlHDl�HDmHDm�HDnHDn�HDoHDo�HDpHDp�HDqHDq�HDrHDr�HDsHDs�HDtHDt�HDuHDu�HDvHDv�HDwHDw�HDxHDx�HDyHDy�HDzHDz�HD{HD{�HD|HD|�HD}HD}�HD~HD~�HDHD�HD� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D�}qD���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�=qD���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D�D���D� �D�@�DÀ�D���D� �D�@�DĀ�D���D� �D�@�Dŀ�D���D� �D�@�Dƀ�D���D� �D�@�Dǀ�D���D� �D�@�DȀ�D���D� �D�@�Dɀ�D���D� �D�@�Dʀ�D���D� �D�@�Dˀ�D���D� �D�@�D̀�D���D� �D�@�D̀�D���D� �D�@�D΀�D���D� �D�@�Dπ�D���D� �D�@�DЀ�D���D� �D�@�Dр�D���D� �D�@�DҀ�D���D� �D�@�DӀ�D���D� �D�@�DԀ�D���D� �D�@�DՀ�D���D� �D�@�Dր�D���D� �D�@�D׀�D���D��D�C�D؀�D���D� �D�@�Dـ�D���D� �D�@�Dڀ�D���D� �D�@�Dۀ�D���D� �D�@�D܀�D���D� �D�@�D݀�D���D� �D�@�Dހ�D���D� �D�@�D߀�D���D� �D�@�D���D���D� �D�@�DဤD���D� �D�@�D․D���D� �D�@�D��D���D� �D�@�D䀤D���D� �D�@�D値D���D� �D�@�D怤D���D� �D�@�D瀤D���D� �D�@�D耤D���D� �D�@�D逤D���D� �D�@�DꀤD���D� �D�@�D뀤D���D� �D�@�D쀤D���D� �D�@�D퀤D���D� �D�@�DD���D� �D�@�DD���D� �D�@�D���D���D� �D�@�D�D���D� �D�@�D�D���D� �D�@�D�D���D� �D�@�D�D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D��qD���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�ȴA���A��wA��^A���A�x�A�ZA�
=A��RA��A��A�JA�?}A���A���A��mA��^A�E�A���A���A�G�A���A�l�A�/A�%A��A��TA���A�-A��A�M�A��RA�|�A�\)A���A���A�|�A�;dA�1'A��A�A���A�dZA�x�A�(�A��A��mA��A���A�t�A�E�A��A��A�~�A�ffA�p�A��7A��
A���A�-A�VA���A��A���A�
=A�jA�t�A��DA�JA��FA�A�A��
A��A�K�A�1A�A���A��9A�XA���A~5?A{��AzȴAx��AxZAw"�Av{Au�Au;dAtE�AsdZArv�AqO�Ap�Anv�Al�Al(�Aj�jAh�Ag��AgG�Af�AfM�AehsAd�jAd(�Ac\)A`�A\�\A[�
A[�AY��AXv�AWO�AV�9AVVAV(�AV{AV1AU�
AU�AT�yATQ�AR�RAP�/APn�AP�AO��AOK�AM�7AJ�yAIƨAH��AG��AF��AEdZADQ�AC�AC��ACS�ABE�AA�AA`BAAXAAS�AAK�AA?}AA�AAA@�`A@ffA?�A>�A=p�A;��A:�\A:  A7l�A5x�A4r�A3�A3��A3�A3A2~�A2A1S�A0^5A/��A/x�A/33A.�`A.��A-dZA,��A,(�A+��A+�
A+�A*��A)��A)p�A(bNA'�mA'�7A%K�A#�mA#��A#��A"�yA"v�A"A�A!�A!�A �A��AoA�yA��AbA�HAffAbA�#A�A��A~�A��Av�AE�A(�AJA�-A
=AȴAv�A�mA\)A+A�A��Az�A$�AG�AbA�A�A/AbNAXA
�\A	�FA��A(�Ar�A(�A�^AffA�A�\A9XA��A �A J@�ƨ@�S�@���@�@�G�@���@�1'@�;d@�ff@�Z@�=q@��@��@��@��9@���@�V@�-@��@��
@�@�ƨ@�;d@���@�hs@��@�J@��@���@�|�@�"�@�\@��#@�O�@���@���@�1'@�~�@ݲ-@ܣ�@�  @ە�@�~�@�S�@�5?@�?}@�r�@�dZ@ѩ�@Л�@��@Ώ\@�@���@�V@�&�@�r�@�@ũ�@���@å�@\@��@�`B@�G�@�Ĝ@�z�@��;@��@��@�V@�%@��P@��@��-@�/@� �@�C�@���@�{@���@���@��@��F@�\)@��@�V@�@���@���@�V@�(�@���@�dZ@�
=@��+@��@���@� �@��@�^5@�J@�X@��@�z�@���@��\@���@��h@�/@�z�@���@��
@��F@��@���@��@�t�@�+@���@��+@�5?@�@���@�7L@�Ĝ@�ƨ@�~�@���@�%@���@�\)@���@�E�@�$�@�@���@��@��
@���@�+@��@�n�@��^@�G�@��/@��D@��@��m@���@��@���@�C�@�"�@��!@�=q@���@�p�@��@��@�Ĝ@�A�@��w@���@��@�t�@�S�@��H@��+@�~�@�n�@��#@�X@�`B@�?}@���@��9@�Z@��@��m@��F@�K�@�;d@��@��@���@��@���@��^@�x�@�?}@�?}@���@��@��u@�Z@�9X@���@�|�@�t�@�l�@�\)@�S�@�;d@�@���@�V@���@��T@���@�/@�V@���@��u@�z�@�9X@�w@;d@~$�@}��@}p�@}p�@}`B@}/@}V@|�@|�@|�D@|(�@{C�@zn�@yhs@x�u@x1'@xb@w�@w�;@w
=@tz�@s��@sdZ@sC�@s33@s"�@s@r�@r�H@r��@r��@rn�@q��@qx�@pA�@o�@o�P@o|�@ol�@o�@n�+@nff@nV@m��@m�-@m�-@m/@m�@mV@l��@l�@l�j@lI�@k��@j�H@j=q@i��@i�#@i��@i�7@ihs@i��@i��@i��@i��@i��@i�#@i&�@h��@h�@hQ�@h �@g�;@g��@g��@g�w@g�P@g
=@fff@f{@e@e�@d��@d�D@cƨ@c33@b��@b��@b�\@b~�@b^5@b-@a�@a�#@a��@ahs@a&�@a%@`��@`  @_\)@_
=@^��@^ff@]�T@]O�@]?}@]�@\��@\��@\Z@\9X@[��@[S�@Z�@Z�@Y�@X�u@XbN@X1'@W��@W+@VV@V$�@U/@T�@T�/@T��@T�@T��@TI�@T(�@T�@S��@S�m@S�
@Sƨ@SdZ@R��@R�\@RM�@RJ@Q�@Q�^@Qhs@QX@QX@QG�@Q�@P�`@P��@P�u@PQ�@P1'@P  @O�w@O\)@Nv�@NV@N{@M��@M�@L�/@L�@L�@L�D@Lj@LZ@L1@K��@KdZ@Ko@J��@J�\@Jn�@JM�@J�@I��@Ihs@H�`@Hr�@HQ�@H �@G��@G�@F��@F{@E�@E�@E�T@E��@E�-@Ep�@D�@DZ@D1@D1@C��@C�m@CdZ@C@B��@B=q@BJ@A�@A�^@Ahs@@��@@�`@@�`@@Ĝ@@r�@@1'@?��@?l�@?�@>�y@>�+@>ff@=�@=O�@=�@=V@<�@<��@<9X@<�@;ƨ@;��@;t�@;o@:��@:M�@9x�@97L@9%@81'@7�@7K�@7;d@7�@6�R@6V@65?@6$�@5�T@5�h@5/@4�@4z�@49X@3ƨ@3C�@2�H@2n�@2-@1��@1��@1�@1�^@1hs@1X@1�@1%@0�`@0Ĝ@0�9@0Q�@0b@/��@/+@.ȴ@.V@.5?@.5?@.$�@.@-�@-��@-@-�h@-�@-`B@-�@,�j@,�j@,Z@+�m@+ƨ@+S�@*�@*M�@*�@)�#@)&�@(��@(Ĝ@(��@(�u@(bN@(A�@(1'@(b@'�@'�;@'�;@'�;@'�w@'��@'l�@&ȴ@&5?@&@%�@%�T@%��@%��@%@%�-@%`B@$�@$��@$j@$Z@#�m@#��@#��@#��@#��@#�@#dZ@#o@"�@"�!@"�\@"n�@"n�@"n�@"^5@!�@!�7@!�@ ��@ Ĝ@ �u@ �u@ �u@ �@ r�@ r�@ 1'@ b@�;@|�@
=@ȴ@��@ff@5?@{@�T@@��@`B@�@��@�j@�@z�@�@t�@"�@�@��@~�@M�@x�@7L@�9@r�@r�@bN@b@�@�@��@�P@|�@l�@l�@\)@ȴ@ff@E�@{@@@��@`B@/@�@V@��@�j@�D@z�@I�@�@�m@ƨ@��@t�@S�@33@�@��@��@��@n�@M�@�@�^@��@��@��@��@hs@7L@��@��@r�@Q�@A�@1'@�;@�P@l�@+@�@��@�y@ȴ@�R@��@V@�@�T@�T@��@��@�-@p�@O�@�@��@�j@��@z�@j@Z@9X@(�@�m@��@33@@
��@
^5@	�@	��@	�^@	��@	��@	hs@	G�@	G�@	7L@�`@�9@�@1'@ �@  @�;@�;@��@��@l�@K�@;d@�@
=@��@�y@�@�@�@�R@��@ff@E�@$�@$�@@�T@@p�@O�@?}@?}@/@/@�@��@��@�j@�@z�@9X@�m@��@dZ@C�@o@@�@�H@��@��@^5@J@�@��@x�@X@X@G�@%@ Ĝ@ Ĝ111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�ȴA���A��wA��^A���A�x�A�ZA�
=A��RA��A��A�JA�?}A���A���A��mA��^A�E�A���A���A�G�A���A�l�A�/A�%A��A��TA���A�-A��A�M�A��RA�|�A�\)A���A���A�|�A�;dA�1'A��A�A���A�dZA�x�A�(�A��A��mA��A���A�t�A�E�A��A��A�~�A�ffA�p�A��7A��
A���A�-A�VA���A��A���A�
=A�jA�t�A��DA�JA��FA�A�A��
A��A�K�A�1A�A���A��9A�XA���A~5?A{��AzȴAx��AxZAw"�Av{Au�Au;dAtE�AsdZArv�AqO�Ap�Anv�Al�Al(�Aj�jAh�Ag��AgG�Af�AfM�AehsAd�jAd(�Ac\)A`�A\�\A[�
A[�AY��AXv�AWO�AV�9AVVAV(�AV{AV1AU�
AU�AT�yATQ�AR�RAP�/APn�AP�AO��AOK�AM�7AJ�yAIƨAH��AG��AF��AEdZADQ�AC�AC��ACS�ABE�AA�AA`BAAXAAS�AAK�AA?}AA�AAA@�`A@ffA?�A>�A=p�A;��A:�\A:  A7l�A5x�A4r�A3�A3��A3�A3A2~�A2A1S�A0^5A/��A/x�A/33A.�`A.��A-dZA,��A,(�A+��A+�
A+�A*��A)��A)p�A(bNA'�mA'�7A%K�A#�mA#��A#��A"�yA"v�A"A�A!�A!�A �A��AoA�yA��AbA�HAffAbA�#A�A��A~�A��Av�AE�A(�AJA�-A
=AȴAv�A�mA\)A+A�A��Az�A$�AG�AbA�A�A/AbNAXA
�\A	�FA��A(�Ar�A(�A�^AffA�A�\A9XA��A �A J@�ƨ@�S�@���@�@�G�@���@�1'@�;d@�ff@�Z@�=q@��@��@��@��9@���@�V@�-@��@��
@�@�ƨ@�;d@���@�hs@��@�J@��@���@�|�@�"�@�\@��#@�O�@���@���@�1'@�~�@ݲ-@ܣ�@�  @ە�@�~�@�S�@�5?@�?}@�r�@�dZ@ѩ�@Л�@��@Ώ\@�@���@�V@�&�@�r�@�@ũ�@���@å�@\@��@�`B@�G�@�Ĝ@�z�@��;@��@��@�V@�%@��P@��@��-@�/@� �@�C�@���@�{@���@���@��@��F@�\)@��@�V@�@���@���@�V@�(�@���@�dZ@�
=@��+@��@���@� �@��@�^5@�J@�X@��@�z�@���@��\@���@��h@�/@�z�@���@��
@��F@��@���@��@�t�@�+@���@��+@�5?@�@���@�7L@�Ĝ@�ƨ@�~�@���@�%@���@�\)@���@�E�@�$�@�@���@��@��
@���@�+@��@�n�@��^@�G�@��/@��D@��@��m@���@��@���@�C�@�"�@��!@�=q@���@�p�@��@��@�Ĝ@�A�@��w@���@��@�t�@�S�@��H@��+@�~�@�n�@��#@�X@�`B@�?}@���@��9@�Z@��@��m@��F@�K�@�;d@��@��@���@��@���@��^@�x�@�?}@�?}@���@��@��u@�Z@�9X@���@�|�@�t�@�l�@�\)@�S�@�;d@�@���@�V@���@��T@���@�/@�V@���@��u@�z�@�9X@�w@;d@~$�@}��@}p�@}p�@}`B@}/@}V@|�@|�@|�D@|(�@{C�@zn�@yhs@x�u@x1'@xb@w�@w�;@w
=@tz�@s��@sdZ@sC�@s33@s"�@s@r�@r�H@r��@r��@rn�@q��@qx�@pA�@o�@o�P@o|�@ol�@o�@n�+@nff@nV@m��@m�-@m�-@m/@m�@mV@l��@l�@l�j@lI�@k��@j�H@j=q@i��@i�#@i��@i�7@ihs@i��@i��@i��@i��@i��@i�#@i&�@h��@h�@hQ�@h �@g�;@g��@g��@g�w@g�P@g
=@fff@f{@e@e�@d��@d�D@cƨ@c33@b��@b��@b�\@b~�@b^5@b-@a�@a�#@a��@ahs@a&�@a%@`��@`  @_\)@_
=@^��@^ff@]�T@]O�@]?}@]�@\��@\��@\Z@\9X@[��@[S�@Z�@Z�@Y�@X�u@XbN@X1'@W��@W+@VV@V$�@U/@T�@T�/@T��@T�@T��@TI�@T(�@T�@S��@S�m@S�
@Sƨ@SdZ@R��@R�\@RM�@RJ@Q�@Q�^@Qhs@QX@QX@QG�@Q�@P�`@P��@P�u@PQ�@P1'@P  @O�w@O\)@Nv�@NV@N{@M��@M�@L�/@L�@L�@L�D@Lj@LZ@L1@K��@KdZ@Ko@J��@J�\@Jn�@JM�@J�@I��@Ihs@H�`@Hr�@HQ�@H �@G��@G�@F��@F{@E�@E�@E�T@E��@E�-@Ep�@D�@DZ@D1@D1@C��@C�m@CdZ@C@B��@B=q@BJ@A�@A�^@Ahs@@��@@�`@@�`@@Ĝ@@r�@@1'@?��@?l�@?�@>�y@>�+@>ff@=�@=O�@=�@=V@<�@<��@<9X@<�@;ƨ@;��@;t�@;o@:��@:M�@9x�@97L@9%@81'@7�@7K�@7;d@7�@6�R@6V@65?@6$�@5�T@5�h@5/@4�@4z�@49X@3ƨ@3C�@2�H@2n�@2-@1��@1��@1�@1�^@1hs@1X@1�@1%@0�`@0Ĝ@0�9@0Q�@0b@/��@/+@.ȴ@.V@.5?@.5?@.$�@.@-�@-��@-@-�h@-�@-`B@-�@,�j@,�j@,Z@+�m@+ƨ@+S�@*�@*M�@*�@)�#@)&�@(��@(Ĝ@(��@(�u@(bN@(A�@(1'@(b@'�@'�;@'�;@'�;@'�w@'��@'l�@&ȴ@&5?@&@%�@%�T@%��@%��@%@%�-@%`B@$�@$��@$j@$Z@#�m@#��@#��@#��@#��@#�@#dZ@#o@"�@"�!@"�\@"n�@"n�@"n�@"^5@!�@!�7@!�@ ��@ Ĝ@ �u@ �u@ �u@ �@ r�@ r�@ 1'@ b@�;@|�@
=@ȴ@��@ff@5?@{@�T@@��@`B@�@��@�j@�@z�@�@t�@"�@�@��@~�@M�@x�@7L@�9@r�@r�@bN@b@�@�@��@�P@|�@l�@l�@\)@ȴ@ff@E�@{@@@��@`B@/@�@V@��@�j@�D@z�@I�@�@�m@ƨ@��@t�@S�@33@�@��@��@��@n�@M�@�@�^@��@��@��@��@hs@7L@��@��@r�@Q�@A�@1'@�;@�P@l�@+@�@��@�y@ȴ@�R@��@V@�@�T@�T@��@��@�-@p�@O�@�@��@�j@��@z�@j@Z@9X@(�@�m@��@33@@
��@
^5@	�@	��@	�^@	��@	��@	hs@	G�@	G�@	7L@�`@�9@�@1'@ �@  @�;@�;@��@��@l�@K�@;d@�@
=@��@�y@�@�@�@�R@��@ff@E�@$�@$�@@�T@@p�@O�@?}@?}@/@/@�@��@��@�j@�@z�@9X@�m@��@dZ@C�@o@@�@�H@��@��@^5@J@�@��@x�@X@X@G�@%@ Ĝ@ Ĝ111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�bB�hB�bB�\B�\B�bB�PB�PB�7B��B��B�hB�PB��B�FB�-B��B�B�'B�B��B��B��B��B��B��B��B�{B�hB�JB�7B�7B�PB�1B�=B�1B�%B�DB�7B�B}�Br�BaHBK�BaHBhsB^5BL�BM�BJ�BD�B33B�B��B��B�B�LB�FB��B�{B�JB�BdZB:^BBJBbB1B
=B
��B
�B
ȴB
ĜB
�-B
��B
�!B
��B
�hB
q�B
;dB
7LB
K�B
:^B
C�B
<jB
0!B
8RB
49B
%�B
!�B
{B
VB
	7B	��B	�ZB	�B	�TB	��B	�B	�HB	�;B	�B	��B	ȴB	B	�'B	�bB	hsB	��B	�oB	�7B	�B	�B	�%B	�7B	�=B	�=B	�1B	�B	|�B	s�B	jB	\)B	H�B	]/B	[#B	P�B	H�B	.B	{B	%�B	$�B	�B	�B	oB	oB	�B	�B	uB	DB	DB	�B	�B	�B	�B	�B	uB	hB	JB	B�B��B�sB�B�
B�#B�RB�dBÖB��B��B��BƨBB��B�^B�?B�LB�wB�qB�LB�?B��B��B�B�3B�!B�B��B��B��B�{B��B�uBx�B}�B�{B��B�JB�=B�\B�=B�%B~�Bw�By�B�B� Br�Bl�Bp�Bs�Bo�B_;BW
BYBR�BdZBk�Bk�BhsBcTB]/BbNB_;B[#BZBaHB^5B[#BYBQ�BE�B9XB:^B>wB:^B;dB5?B7LB6FB49B0!B�B;dB49B!�B!�B33B49B0!B'�B2-B:^B9XB7LB49B8RB6FB49B/B,B �B"�B0!B49B49B5?B-B$�B0!B)�B�B�B�B&�B%�B�B\B�B�B�B"�B#�B �B�B �B#�B"�B�BoB�B�B�B�BVBBhB�B{BuBJB{BuB�B�BbB
=BhB�BJB�B�B�B�B#�B#�B(�B%�B%�B�B�BuBB�B �B+B'�B1'B/B33B8RB9XB;dB9XB<jBA�BA�BA�BB�BE�BF�BF�BB�BB�BG�BM�BK�BI�BH�BD�BM�BL�BR�BW
BS�BVBW
BS�BVB`BBdZBgmBffBl�Br�Bs�Bu�Bt�Bv�Bw�Bu�Bu�Bw�Bx�Bx�B|�B|�B{�Bz�By�B�%B�PB�VB�oB��B��B��B��B��B��B�B�!B�B�!B�'B�'B�FB�^B�qB�}BĜBƨBǮBǮBƨBȴBƨBȴB��B��B��B�B�B�B�/B�`B�fB�mB�fB�`B�B�B�B�B�B��B��B��B��B��B��B	B	B	B		7B	JB	PB	VB	JB	{B	�B	�B	�B	�B	�B	�B	#�B	%�B	&�B	%�B	)�B	0!B	1'B	2-B	33B	2-B	2-B	33B	6FB	9XB	>wB	=qB	>wB	C�B	C�B	D�B	G�B	H�B	K�B	M�B	P�B	VB	YB	[#B	\)B	\)B	\)B	\)B	\)B	]/B	\)B	\)B	`BB	e`B	jB	p�B	r�B	r�B	p�B	l�B	jB	|�B	�B	�B	�%B	�%B	�+B	�1B	�7B	�7B	�7B	�1B	�7B	�1B	�1B	�VB	�uB	�{B	�{B	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�!B	�!B	�9B	�RB	�dB	�jB	�qB	�qB	�wB	�}B	�jB	�wB	�wB	�}B	��B	B	ŢB	ƨB	ƨB	ƨB	ƨB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�#B	�B	�B	�#B	�5B	�5B	�;B	�;B	�BB	�ZB	�ZB	�TB	�TB	�ZB	�ZB	�TB	�ZB	�ZB	�TB	�`B	�yB	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B	��B
B
B
B
B
B
B
B
B
%B
%B
+B
1B
1B
1B
1B
+B
	7B
	7B

=B
JB
DB

=B
DB
JB
PB
hB
hB
hB
hB
bB
\B
\B
bB
uB
{B
{B
uB
hB
oB
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
�B
!�B
!�B
 �B
 �B
 �B
�B
#�B
#�B
!�B
$�B
&�B
(�B
'�B
'�B
'�B
)�B
)�B
(�B
(�B
(�B
(�B
,B
,B
+B
,B
-B
.B
/B
0!B
1'B
0!B
0!B
0!B
1'B
1'B
2-B
2-B
2-B
2-B
1'B
1'B
1'B
2-B
33B
33B
6FB
7LB
6FB
6FB
7LB
6FB
6FB
6FB
7LB
7LB
6FB
6FB
7LB
6FB
6FB
8RB
7LB
7LB
6FB
9XB
9XB
8RB
;dB
=qB
=qB
=qB
=qB
=qB
>wB
>wB
>wB
?}B
?}B
>wB
>wB
=qB
=qB
;dB
=qB
@�B
A�B
A�B
A�B
A�B
A�B
@�B
?}B
?}B
@�B
A�B
B�B
@�B
B�B
D�B
D�B
D�B
C�B
C�B
B�B
C�B
C�B
D�B
E�B
E�B
E�B
D�B
C�B
C�B
D�B
E�B
G�B
G�B
H�B
H�B
G�B
G�B
G�B
F�B
F�B
F�B
E�B
F�B
H�B
H�B
I�B
I�B
J�B
J�B
J�B
K�B
J�B
J�B
L�B
M�B
L�B
K�B
J�B
J�B
L�B
M�B
N�B
N�B
M�B
K�B
N�B
N�B
O�B
Q�B
P�B
P�B
Q�B
Q�B
R�B
R�B
R�B
R�B
R�B
Q�B
O�B
P�B
R�B
S�B
S�B
R�B
S�B
S�B
T�B
T�B
VB
T�B
T�B
T�B
VB
VB
T�B
VB
W
B
VB
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
XB
XB
XB
W
B
ZB
ZB
ZB
ZB
YB
YB
YB
YB
ZB
[#B
[#B
[#B
ZB
ZB
\)B
\)B
]/B
]/B
]/B
]/B
]/B
]/B
\)B
\)B
_;B
_;B
_;B
_;B
^5B
]/B
^5B
^5B
_;B
^5B
_;B
`BB
`BB
`BB
`BB
`BB
_;B
_;B
_;B
`BB
aHB
`BB
aHB
cTB
dZB
dZB
dZB
cTB
cTB
dZB
dZB
cTB
cTB
dZB
dZB
ffB
e`B
ffB
gmB
ffB
ffB
ffB
gmB
gmB
gmB
hsB
hsB
hsB
hsB
hsB
hsB
hsB
gmB
hsB
hsB
iyB
iyB
hsB
hsB
iyB
hsB
jB
jB
jB
jB
jB
jB
iyB
iyB
jB
jB
iyB
iyB
iyB
iyB
k�B
l�B
k�B
l�B
m�B
m�B
l�B
l�B
l�B
l�B
n�B
n�B
m�B
n�B
p�B
o�B
n�B
o�B
q�B
q�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�}B�hB�}B��B��B��B�"B�<B��B�YB�1B��B�bB�BB�`B��B�B�B��B��B��B��B�fB�>B�*B�8B�jB��B��B�6B�XB��B��B�B��B��B��B�^B�lB��B~�BtBc�BNpBa�Bh�B_;BN<BN�BKDBESB4�B7B��B�eB��B��B��B�0B�SB�jB��BgB>wB	�B�B:B	�B)B
��B
�1B
��B
�tB
��B
��B
�oB
�fB
��B
tnB
A;B
:*B
L�B
<jB
DgB
=�B
1�B
8�B
4�B
'8B
"�B
�B
�B

�B	�LB	�B	�B	�FB	�NB	�#B	�B	��B	��B	��B	ɠB	ÖB	��B	��B	mwB	�#B	��B	��B	��B	�AB	��B	��B	�rB	�XB	�fB	�{B	}�B	t�B	k�B	^B	J�B	]�B	[�B	Q�B	I�B	0�B	�B	'8B	&B	B	B	,B	�B	B		B	FB	�B	0B	�B	�B	�B	�B	�B	�B	�B	�B	�B�3B��B�0B�WBؓB�xB��B��B��B�^B�<B�6B�_B�aB�UB�dB�zB�8B��B��B��B��B�sB��B��B�hB�oB��B��B��B�VB��B�9B�aB{�B�B��B��B�PB��B��B��B��B� By>Bz�B�aB�OBs�Bm�Bq[Bt9Bp;B`�BX�BZ�BT�Bd�Bk�Bk�Bh�Bc�B^Bb�B_�B[�BZ�Ba�B^�B[�BYBR�BGB;B;�B?�B;�B<�B6�B8lB7�B5tB1vB!�B;�B5B#�B#nB3�B4�B1B)DB2�B:�B9�B7�B4�B8�B6�B4�B/�B,�B"NB$&B0�B4�B4�B5tB-�B%�B0UB*�BB�B�B'RB&LB�B�B=BdB�B# B$&B!HB \B!B$B#:B;B�B7BKBBB\BB�BEBBFB�BMBaB#B7BhB�B:BQB�B�BjBxB~B$@B$ZB)B&LB&B \B�B�BgB vB!�B+�B(�B1�B/�B3�B8�B9�B;�B:B<�BA�BA�BA�BB�BE�BF�BF�BC-BCGBHBNBLBJ=BIRBE�BN<BM�BS[BWYBT�BVmBWsBT�BV�B`�Bd�Bg�Bf�Bl�Br�Bs�Bu�Bt�Bv�Bw�BvBvBxBy$By$B}"B}VB|jB{�Bz�B��B��B�(B��B��B�	B��B�B��B�`B�OB�UB��B�UB��B��B��B��B��B��BĶB��B��B��B��B��B�B�B�<B�:B�FB�EB�KBؓBݘB�zB�B�B�B��B��B�B��B�B��B��B�	B�B�B�*B�(B	AB	GB	aB		RB	~B	�B	�B	�B	�B	�B	�B	�B	�B	�B	B	#�B	%�B	'B	&LB	*0B	0;B	1AB	2GB	3MB	2aB	2aB	3�B	6�B	9�B	>wB	=�B	>�B	C�B	C�B	D�B	G�B	IB	LB	NB	QNB	V9B	Y1B	[#B	\CB	\CB	\CB	\CB	\CB	]/B	\]B	\�B	`�B	e�B	j�B	p�B	r�B	r�B	p�B	mB	k�B	}"B	�-B	�9B	�?B	�%B	�EB	�KB	�RB	�RB	�RB	�1B	�lB	�fB	��B	�pB	��B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�8B	�DB	�/B	�;B	�;B	�TB	�RB	�JB	�jB	�qB	�qB	�wB	�}B	��B	��B	��B	��B	��B	ªB	ŢB	ƨB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�"B	� B	�B	�+B	�+B	�B	�1B	�+B	�1B	�7B	�7B	�7B	�B	�=B	�QB	�eB	�WB	�OB	�jB	�;B	�pB	�vB	�ZB	�tB	�nB	�nB	�tB	�tB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	�B	��B	�B	�B	��B	�"B	�"B	�6B
 B
 B
 B	�HB
-B
9B
B
9B
9B
B
MB
MB
?B
?B
EB
1B
KB
KB
1B
_B
	RB
	lB

rB
JB
^B

rB
xB
~B
�B
hB
hB
�B
�B
}B
vB
�B
}B
�B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
�B
!�B
!�B
 �B
 �B
 �B
�B
#�B
#�B
"B
%B
'B
(�B
(
B
($B
'�B
*B
*B
)B
)B
)*B
)*B
,"B
,"B
+6B
,=B
-CB
.IB
/5B
0;B
1'B
0;B
0;B
0;B
1AB
1AB
2-B
2-B
2GB
2GB
1AB
1AB
1[B
2aB
3MB
3hB
6FB
7LB
6`B
6`B
7LB
6`B
6`B
6FB
7LB
7fB
6`B
6`B
7LB
6zB
6zB
8lB
7�B
7fB
6zB
9rB
9rB
8�B
;B
=qB
=�B
=�B
=qB
=�B
>wB
>�B
>�B
?}B
?}B
>wB
>�B
=qB
=�B
;�B
=�B
@�B
A�B
A�B
A�B
AoB
A�B
@�B
?�B
?�B
@�B
A�B
B�B
@�B
B�B
D�B
D�B
D�B
C�B
C�B
B�B
C�B
C�B
D�B
E�B
E�B
E�B
D�B
C�B
C�B
D�B
E�B
G�B
G�B
H�B
H�B
G�B
G�B
G�B
F�B
F�B
F�B
E�B
F�B
H�B
H�B
I�B
I�B
J�B
J�B
J�B
K�B
J�B
J�B
L�B
M�B
L�B
K�B
J�B
J�B
L�B
M�B
N�B
N�B
M�B
LB
N�B
OB
O�B
Q�B
Q B
Q B
RB
Q�B
R�B
R�B
R�B
R�B
R�B
RB
PB
Q B
SB
TB
TB
SB
TB
S�B
UB
T�B
VB
UB
T�B
UB
VB
VB
UB
VB
W$B
VB
W$B
W$B
W$B
W?B
X+B
XB
X+B
X+B
X+B
X+B
W$B
ZB
ZB
ZB
ZB
Y1B
Y1B
Y1B
Y1B
Z7B
[=B
[#B
[=B
Z7B
Z7B
\CB
\CB
]/B
]IB
]B
]IB
]IB
]IB
\CB
\CB
_;B
_;B
_;B
_;B
^OB
]IB
^OB
^OB
_VB
^5B
_VB
`BB
`'B
`BB
`\B
`\B
_VB
_;B
_VB
`\B
abB
`\B
aHB
cTB
dZB
dZB
dZB
c�B
cnB
dZB
dtB
cnB
cnB
dtB
dtB
ffB
ezB
f�B
gRB
ffB
f�B
f�B
gmB
gmB
g�B
hsB
hsB
h�B
hsB
hsB
hsB
hsB
g�B
h�B
h�B
iyB
iyB
h�B
h�B
i�B
h�B
jB
jB
jB
jB
jeB
jeB
i�B
i�B
jeB
jB
i�B
i�B
i�B
i�B
k�B
l�B
k�B
l�B
m�B
m�B
l�B
l�B
l�B
l�B
n�B
n�B
m�B
n�B
p�B
o�B
n�B
o�B
q�B
q�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=-0.02(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201804060033542018040600335420180406003354201806221239542018062212395420180622123954201804271405112018042714051120180427140511  JA  ARFMdecpA19c                                                                20180402123521  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180402033733  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180402033735  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180402033736  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180402033736  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180402033736  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180402033736  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180402033736  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20180402033738  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180402033738                      G�O�G�O�G�O�                JA  ARUP                                                                        20180402035756                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180402154057  CV  JULD            G�O�G�O�F¿�                JM  ARCAJMQC2.0                                                                 20180405153354  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180405153354  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180427050511  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622033954  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115121517                      G�O�G�O�G�O�                