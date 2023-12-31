CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-09-02T00:35:14Z creation;2018-09-02T00:35:19Z conversion to V3.1;2019-12-19T07:33:44Z update;     
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
_FillValue                 �  I    PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `l   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �X   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �<   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �(   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ܘ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �(   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �(   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �(   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �(   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �    HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �$   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �(Argo profile    3.1 1.2 19500101000000  20180902003514  20200115131518  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL              A   JA  I2_0576_276                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @�~6>�u 1   @�~7�[ @9��-��d^E����1   GPS     A   B   B   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�  @�  A��A   A@  A`  A�  A�33A�33A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  BxffB�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl�Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~�C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DR��DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� Dd��De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�<�D�|�D�� D�  D�<�D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�3D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @���@���A�A Q�A@Q�A`Q�A�(�A�\)A�\)A�(�A�(�A�(�A�(�A�(�B {B{B{B{B {B({B0{B8{B@{BH{BP{BX{B`{Bh{Bp{Bxz�B�
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
CCCCCCCCCCC C"C$C&C(C*C,C.C0C2C4C6C8C:C<C>C@CBCDCFCHCJCLCNCPCRCTCVCXCZC\C^C`CbCdCfChCjCl�CnCpCrCtCvCxCzC|C~�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�\C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D HD �HDHD�HDHD�HDHD�HDHD�HD��D�HDHD�HDHD�HDHD�HD	HD	�HD
HD
�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HD HD �HD!HD!�HD"HD"�HD#HD#�HD$HD$�HD%HD%�HD&HD&�HD'HD'�HD(HD(�HD)HD)�HD*HD*�HD+HD+�HD,HD,�HD-HD-�HD.HD.�HD/HD/�HD0HD0�HD1HD1�HD2HD2�HD3HD3�HD4HD4�HD5HD5�HD6HD6�HD7HD7�HD8HD8�HD9HD9�HD:HD:�HD;HD;�HD<HD<�HD=HD=�HD>HD>�HD?HD?�HD@HD@�HDAHDA�HDBHDB�HDCHDC�HDDHDD�HDEHDE�HDFHDF�HDGHDG�HDHHDH�HDIHDI�HDJHDJ�HDKHDK�HDLHDL�HDMHDM�HDNHDN�HDOHDO�HDPHDP�HDQHDQ�HDRHDR�HDR��DS�HDTHDT�HDUHDU�HDVHDV�HDWHDW�HDXHDX�HDYHDY�HDZHDZ�HD[HD[�HD\HD\�HD]HD]�HD^HD^�HD_HD_�HD`HD`�HDaHDa�HDbHDb�HDcHDc�HDdHDd�HDd��De�HDfHDf�HDgHDg�HDhHDh�HDiHDi�HDjHDj�HDkHDk�HDlHDl�HDmHDm�HDnHDn�HDoHDo�HDpHDp�HDqHDq�HDrHDr�HDsHDs�HDtHDt�HDuHDu�HDvHDv�HDwHDw�HDxHDx�HDyHDy�HDzHDz�HD{HD{�HD|HD|�HD}HD}�HD~HD~�HDHD�HD� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�C�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D�D���D� �D�@�DÀ�D���D� �D�@�DĀ�D���D� �D�@�Dŀ�D���D� �D�@�Dƀ�D���D� �D�@�Dǀ�D���D� �D�@�DȀ�D���D� �D�@�Dɀ�D���D� �D�@�Dʀ�D���D� �D�@�Dˀ�D���D� �D�@�D̀�D���D� �D�@�D̀�D���D� �D�@�D΀�D���D� �D�@�Dπ�D���D� �D�@�DЀ�D���D� �D�@�Dр�D���D� �D�@�DҀ�D���D� �D�@�DӀ�D���D� �D�@�DԀ�D���D� �D�@�DՀ�D���D� �D�@�Dր�D���D� �D�=qD�}qD���D� �D�=qD؀�D���D� �D�@�Dـ�D���D� �D�@�Dڀ�D���D� �D�@�Dۀ�D���D� �D�@�D܀�D���D� �D�@�D݀�D���D� �D�@�Dހ�D���D� �D�@�D߀�D���D��D�@�D���D���D� �D�@�DဤD���D� �D�@�D․D���D� �D�@�D〤D���D� �D�@�D䀤D���D� �D�@�D値D���D� �D�@�D怤D���D� �D�@�D瀤D���D� �D�@�D耤D���D� �D�@�D逤D���D� �D�@�DꀤD���D� �D�@�D뀤D���D� �D�@�D쀤D���D� �D�@�D퀤D���D� �D�@�DD���D� �D�@�DD���D� �D�@�D���D���D� �D�@�D�D���D� �D�@�D�D���D� �D�@�D�D���D� �D�@�D�D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D��
111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   AӺ^Aӟ�Aӗ�AӓuAӍPAӋDAӇ+AӇ+AӇ+AӇ+AӅAӃAӃAӁAӁAӃAӅAӃAӁAӃAӃAӃAӁA�r�A���A�
=Ȧ+A��Aʣ�A���A��/A���A��RA���A�A�|�A���A�"�A�%A�p�A��A�?}A�/A��mA�+A��jA�;dA���A���A��hA�A�A�bNA��;A���A���A�Q�A�bNA�(�A��A�jA��HA�hsA���A���A�\)A�9XA�7LA�ĜA�;dA���A��RA��A�l�A�VA�1'A�"�A��!A�+A��/A�C�A���A�9XA���A��
A�`BA��A��DA��A�A�;dA�9XA���A��A�%A�G�A�p�A��jA���A��PA��^A��HA��;A�A��-A�M�A~ȴA|�uA{
=Az�+Ay�Ay��Ay33Ax��Aw�Av�AvAt�yAs�Ar�yAr~�Aq��Ao�hAm�wAlE�Ak�7AkC�Aj��Ah=qAf9XAe/Adn�Ac�
AcG�Ab��Ab�Aa�PA]�hAZ��AY�AW�^AU��AUdZAU33AU"�AU
=ATA�ASARbNARJAQ33AOoAN�DAN�9ANjAN1AM�AM�ALI�AJ�AJE�AI�AI&�AH��AH�RAH��AH�AHĜAH��AH��AH�AG��AG?}AFz�AE��AE\)AC�AB  AAoA@-A?|�A=�A<bA;dZA:bA8��A7K�A6�9A6~�A5��A5VA3��A3;dA2bNA2�DA2M�A2{A1�A1�#A1hsA0{A-�A-oA+&�A*ffA)�;A)��A)\)A)
=A(�9A'��A&^5A%S�A#33A"{A ��A�yA��At�AS�A7LA��A�#A��Al�AbA"�A�/A��A�A1'A  AQ�A%A�A��A�DA5?A�A/A
ȴA
M�A	7LA��A�DA�wA��A�#AG�Av�A��A ��A 9X@��@���@���@�O�@�9X@��\@���@�1@�~�@�hs@�@�@��@�l�@�M�@�D@�F@�ȴ@��@���@� �@�"�@��@�n�@���@�5?@�(�@�ƨ@�|�@�v�@�`B@��@�X@ԛ�@��m@���@љ�@� �@Ϯ@�K�@��y@�^5@���@�X@�
=@��@���@�x�@��`@ǅ@�"�@�o@�@�n�@��/@�K�@��@�X@���@�(�@�t�@��+@�G�@���@�
=@���@�{@���@���@��#@���@�p�@��D@�C�@���@��F@�"�@�
=@��!@�`B@��j@��@���@���@���@���@���@��+@�ff@�5?@��@��u@��@��R@���@���@��F@�;d@��!@���@�~�@�J@���@�p�@�?}@�Q�@��P@�5?@�%@�z�@�A�@�ƨ@�"�@�~�@�E�@�-@���@��-@�`B@�%@��9@��@�K�@��y@�ff@�{@���@���@��@���@��@�bN@�(�@��;@�\)@���@�n�@�E�@�{@��^@���@�I�@��m@�=q@��w@�@�ȴ@�5?@���@��j@�  @��@��@�K�@��@���@��+@��+@�`B@���@��@�r�@�1'@�1@�ƨ@�o@���@��+@�ff@�$�@���@��@��j@���@��@���@��/@�Ĝ@�bN@���@�;d@�@���@���@���@���@�~�@�ff@���@�%@���@�bN@� �@�@K�@~�y@~�R@}��@|�/@|z�@|9X@|(�@|1@{��@{�@{"�@z�@z�@z�@z�H@z��@z^5@y��@yX@x��@x�9@x�`@x��@x�9@x��@xr�@x1'@x1'@w�;@v��@v5?@u��@uO�@u?}@u?}@u/@t��@t��@t�/@t�j@t�D@tI�@s�
@s�
@tj@tj@s��@sƨ@s�F@s�@r�@r�H@r�H@r�!@q��@p��@pQ�@p �@p1'@pr�@pĜ@p�`@p�`@p�9@p�@pb@o|�@o+@o
=@n�y@nV@m�T@m`B@l��@lZ@l1@kƨ@kC�@j��@i�#@ihs@h�`@h�@h �@g�@gl�@f�@f�R@f�+@e�-@eO�@eV@d�@d�j@d��@d�D@c��@c��@c"�@b��@b~�@bM�@b�@a�#@a��@ax�@a�@`Ĝ@`bN@_�@_��@_;d@^��@^$�@]�-@]`B@]/@]V@\��@\��@\z�@\9X@\�@[�
@[S�@Z�@Z��@Y�@YG�@X��@X�u@XbN@W�@W�@W�P@Wl�@V�R@Vv�@VE�@V5?@V$�@U��@U�-@U��@UO�@T�@T��@T�@S��@S"�@R�@R��@Rn�@Rn�@R^5@R^5@RM�@RM�@R=q@R�@RJ@Q��@Q�@P��@P  @O�@Ol�@Nȴ@N�R@N�+@M�@Mp�@MO�@L��@L�/@L�@LI�@K�m@K��@K"�@J�H@J�\@J�@I��@IG�@H�`@H��@H�9@Hr�@H�@HQ�@G�@G�w@G��@Gl�@G+@F�y@Fv�@F$�@E��@EO�@EV@D��@D�@D�/@D�j@DZ@D(�@Ct�@B^5@B�@A�@A��@A&�@@��@@��@@�@?�;@?�P@?;d@>�@=�T@=p�@=p�@=/@=V@<�@<�D@<(�@;��@;t�@:�H@:=q@9�#@9x�@9&�@8��@8��@8��@8r�@8  @7�w@7��@7l�@7�@6��@6ff@6{@5��@5�@5`B@5O�@5V@4�j@4�D@4�@3�m@3��@3�@3dZ@3C�@3"�@3@2��@2n�@2J@1��@1��@1&�@0�@0bN@0bN@0bN@0bN@01'@0 �@/��@/�P@/K�@/�@.��@.�R@.��@.��@.�+@.v�@.V@.{@-��@-O�@-�@-V@-V@,�@,�j@,�D@,(�@+�
@+�F@+��@+��@+t�@+dZ@+o@*��@*�\@*M�@*�@)�#@)��@)G�@)G�@)G�@)&�@(�9@(Q�@(1'@( �@'�@'��@'�P@'l�@'+@&��@&�R@&��@&�+@&$�@%�@$��@$��@$�@$��@$j@$9X@#ƨ@#C�@#"�@"�@"�!@"-@!��@!�@!�#@!�^@!�7@!x�@!G�@!�@ ��@ �`@ Ĝ@ ��@ �u@ r�@ Q�@ b@ b@��@�@�P@|�@\)@+@�@ff@5?@@�@�@�@��@��@Z@�@��@t�@S�@33@�@��@n�@�@�^@��@��@X@�@��@Ĝ@�u@A�@b@|�@+@�@
=@��@�@��@ff@$�@@��@��@p�@��@�D@I�@��@ƨ@��@��@dZ@33@�H@�!@��@�\@n�@M�@=q@=q@-@J@�#@��@��@hs@X@7L@%@��@r�@A�@  @�;@�@\)@+@�@��@�@�+@V@V@E�@E�@5?@$�@@@�@�T@O�@��@z�@j@�@��@�
@�
@ƨ@ƨ@�F@��@�@S�@33@"�@
�H@
�!@
n�@
J@	��@	��@	��@	�7@	x�@	X@	%@��@Ĝ@�9@�@Q�@ �@  @�;@��@l�@+@��@�y@�@ȴ@�R@��@V@E�@{@�@�@��@��@p�@?}@/@�@V@�/@�j@�D@Z@Z@9X@��@�m@��@t�@S�@33@o@@@�@�H@�!@M�@-@-@�@J@J@��@�#@��@hs@7L@7L@&�@�@%@ ��@ �9@ �9111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   AӺ^Aӟ�Aӗ�AӓuAӍPAӋDAӇ+AӇ+AӇ+AӇ+AӅAӃAӃAӁAӁAӃAӅAӃAӁAӃAӃAӃAӁA�r�A���A�
=Ȧ+A��Aʣ�A���A��/A���A��RA���A�A�|�A���A�"�A�%A�p�A��A�?}A�/A��mA�+A��jA�;dA���A���A��hA�A�A�bNA��;A���A���A�Q�A�bNA�(�A��A�jA��HA�hsA���A���A�\)A�9XA�7LA�ĜA�;dA���A��RA��A�l�A�VA�1'A�"�A��!A�+A��/A�C�A���A�9XA���A��
A�`BA��A��DA��A�A�;dA�9XA���A��A�%A�G�A�p�A��jA���A��PA��^A��HA��;A�A��-A�M�A~ȴA|�uA{
=Az�+Ay�Ay��Ay33Ax��Aw�Av�AvAt�yAs�Ar�yAr~�Aq��Ao�hAm�wAlE�Ak�7AkC�Aj��Ah=qAf9XAe/Adn�Ac�
AcG�Ab��Ab�G�O�A]�hAZ��AY�AW�^AU��AUdZAU33AU"�AU
=ATA�ASARbNARJAQ33AOoAN�DAN�9ANjAN1AM�AM�ALI�AJ�AJE�AI�AI&�AH��AH�RAH��AH�AHĜAH��AH��AH�AG��AG?}AFz�AE��AE\)AC�AB  AAoA@-A?|�A=�A<bA;dZA:bA8��A7K�A6�9A6~�A5��A5VA3��A3;dA2bNA2�DA2M�A2{A1�A1�#A1hsA0{A-�A-oA+&�A*ffA)�;A)��A)\)A)
=A(�9A'��A&^5A%S�A#33A"{A ��A�yA��At�AS�A7LA��A�#A��Al�AbA"�A�/A��A�A1'A  AQ�A%A�A��A�DA5?A�A/A
ȴA
M�A	7LA��A�DA�wA��A�#AG�Av�A��A ��A 9X@��@���@���@�O�@�9X@��\@���@�1@�~�@�hs@�@�@��@�l�@�M�@�D@�F@�ȴ@��@���@� �@�"�@��@�n�@���@�5?@�(�@�ƨ@�|�@�v�@�`B@��@�X@ԛ�@��m@���@љ�@� �@Ϯ@�K�@��y@�^5@���@�X@�
=@��@���@�x�@��`@ǅ@�"�@�o@�@�n�@��/@�K�@��@�X@���@�(�@�t�@��+@�G�@���@�
=@���@�{@���@���@��#@���@�p�@��D@�C�@���@��F@�"�@�
=@��!@�`B@��j@��@���@���@���@���@���@��+@�ff@�5?@��@��u@��@��R@���@���@��F@�;d@��!@���@�~�@�J@���@�p�@�?}@�Q�@��P@�5?@�%@�z�@�A�@�ƨ@�"�@�~�@�E�@�-@���@��-@�`B@�%@��9@��@�K�@��y@�ff@�{@���@���@��@���@��@�bN@�(�@��;@�\)@���@�n�@�E�@�{@��^@���@�I�@��m@�=q@��w@�@�ȴ@�5?@���@��j@�  @��@��@�K�@��@���@��+@��+@�`B@���@��@�r�@�1'@�1@�ƨ@�o@���@��+@�ff@�$�@���@��@��j@���@��@���@��/@�Ĝ@�bN@���@�;d@�@���@���@���@���@�~�@�ff@���@�%@���@�bN@� �@�@K�@~�y@~�R@}��@|�/@|z�@|9X@|(�@|1@{��@{�@{"�@z�@z�@z�@z�H@z��@z^5@y��@yX@x��@x�9@x�`@x��@x�9@x��@xr�@x1'@x1'@w�;@v��@v5?@u��@uO�@u?}@u?}@u/@t��@t��@t�/@t�j@t�D@tI�@s�
@s�
@tj@tj@s��@sƨ@s�F@s�@r�@r�H@r�H@r�!@q��@p��@pQ�@p �@p1'@pr�@pĜ@p�`@p�`@p�9@p�@pb@o|�@o+@o
=@n�y@nV@m�T@m`B@l��@lZ@l1@kƨ@kC�@j��@i�#@ihs@h�`@h�@h �@g�@gl�@f�@f�R@f�+@e�-@eO�@eV@d�@d�j@d��@d�D@c��@c��@c"�@b��@b~�@bM�@b�@a�#@a��@ax�@a�@`Ĝ@`bN@_�@_��@_;d@^��@^$�@]�-@]`B@]/@]V@\��@\��@\z�@\9X@\�@[�
@[S�@Z�@Z��@Y�@YG�@X��@X�u@XbN@W�@W�@W�P@Wl�@V�R@Vv�@VE�@V5?@V$�@U��@U�-@U��@UO�@T�@T��@T�@S��@S"�@R�@R��@Rn�@Rn�@R^5@R^5@RM�@RM�@R=q@R�@RJ@Q��@Q�@P��@P  @O�@Ol�@Nȴ@N�R@N�+@M�@Mp�@MO�@L��@L�/@L�@LI�@K�m@K��@K"�@J�H@J�\@J�@I��@IG�@H�`@H��@H�9@Hr�@H�@HQ�@G�@G�w@G��@Gl�@G+@F�y@Fv�@F$�@E��@EO�@EV@D��@D�@D�/@D�j@DZ@D(�@Ct�@B^5@B�@A�@A��@A&�@@��@@��@@�@?�;@?�P@?;d@>�@=�T@=p�@=p�@=/@=V@<�@<�D@<(�@;��@;t�@:�H@:=q@9�#@9x�@9&�@8��@8��@8��@8r�@8  @7�w@7��@7l�@7�@6��@6ff@6{@5��@5�@5`B@5O�@5V@4�j@4�D@4�@3�m@3��@3�@3dZ@3C�@3"�@3@2��@2n�@2J@1��@1��@1&�@0�@0bN@0bN@0bN@0bN@01'@0 �@/��@/�P@/K�@/�@.��@.�R@.��@.��@.�+@.v�@.V@.{@-��@-O�@-�@-V@-V@,�@,�j@,�D@,(�@+�
@+�F@+��@+��@+t�@+dZ@+o@*��@*�\@*M�@*�@)�#@)��@)G�@)G�@)G�@)&�@(�9@(Q�@(1'@( �@'�@'��@'�P@'l�@'+@&��@&�R@&��@&�+@&$�@%�@$��@$��@$�@$��@$j@$9X@#ƨ@#C�@#"�@"�@"�!@"-@!��@!�@!�#@!�^@!�7@!x�@!G�@!�@ ��@ �`@ Ĝ@ ��@ �u@ r�@ Q�@ b@ b@��@�@�P@|�@\)@+@�@ff@5?@@�@�@�@��@��@Z@�@��@t�@S�@33@�@��@n�@�@�^@��@��@X@�@��@Ĝ@�u@A�@b@|�@+@�@
=@��@�@��@ff@$�@@��@��@p�@��@�D@I�@��@ƨ@��@��@dZ@33@�H@�!@��@�\@n�@M�@=q@=q@-@J@�#@��@��@hs@X@7L@%@��@r�@A�@  @�;@�@\)@+@�@��@�@�+@V@V@E�@E�@5?@$�@@@�@�T@O�@��@z�@j@�@��@�
@�
@ƨ@ƨ@�F@��@�@S�@33@"�@
�H@
�!@
n�@
J@	��@	��@	��@	�7@	x�@	X@	%@��@Ĝ@�9@�@Q�@ �@  @�;@��@l�@+@��@�y@�@ȴ@�R@��@V@E�@{@�@�@��@��@p�@?}@/@�@V@�/@�j@�D@Z@Z@9X@��@�m@��@t�@S�@33@o@@@�@�H@�!@M�@-@-@�@J@J@��@�#@��@hs@7L@7L@&�@�@%@ ��@ �9@ �9111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�9B�LB�RB�RB�XB�XB�XB�XB�XB�XB�XB�XB�^B�^B�^B�^B�XB�^B�^B�^B�XB�XB�?B��B��BoBo�B��B�B/B0!BYBR�B2-B6FBhsBx�B`BBYB�B�B� B��B�PB}�B�B�DB�{B��B�oB�B�JBs�Bn�BaHB^5BO�BL�BW
BR�BH�B6FBPB�B��B��B��B�B�fB�B��B��B�!B��B�\B��B�BgmBA�BN�BR�BbNB\)BJ�B)�B,B$�BPB
=B
��B
��B
�B
�B
��B
�B
��B
��B
�JB
�DB
�B
y�B
�PB
�1B
x�B
_;B
P�B
O�B
aHB
\)B
[#B
VB
M�B
F�B
7LB
9XB
+B
,B
"�B
$�B
�B	��B	�B	��B	��B
  B	�B	��B	��B	ÖB	��B	�dB	�RB	�!B	��B	��B	`BB	^5B	XB	ZB	M�B	T�B	[#B	ZB	S�B	G�B	9XB	<jB	=qB	2-B	 �B	5?B	O�B	I�B	D�B	@�B	5?B	,B	�B	"�B	 �B	(�B	-B	5?B	<jB	=qB	A�B	@�B	<jB	5?B	2-B	49B	+B	%�B	�B	DB�B��B��B�B��B��B�)B�BɺBŢBƨB��BĜBÖB�?B�qB�jB��B��B��B��BĜB�?B��B�B��B�B��B��B��B��B��B��B�+Bt�Bw�B^5BjBffBXB_;Bw�Bt�Bo�BcTBN�B5?B1'B<jBJ�BR�BL�B8RB.B�B(�B49BI�BL�BG�BD�BA�B=qB?}B9XB+B�B'�B&�B)�B$�B-B%�B+B�B#�B �B�B�B�B"�B�B�B!�B�B�BVBoB�B{B{BuB�B�B�B�B�B�B�B�BDBBDB�B�B{BoB%B�B�B�B�B�B�B%�B'�B&�B$�B"�B �B�B!�B+B)�B&�B"�B-B1'B/B(�B�B!�B!�B,B,B)�B(�B%�B"�B#�B-B2-B2-B7LB9XB:^B6FB0!B'�B#�B�B5?B>wBB�B>wB6FB?}B?}BA�BQ�BO�BP�BO�BM�BK�BI�B?}BJ�BG�BB�BA�BR�B\)B\)B\)BbNBbNB_;BbNBgmBgmBbNBbNB`BBhsBq�Bv�Bs�Br�Bx�B~�B�B� B� B~�B� B�B|�B�1B�JB�JB�bB�uB��B�oB��B��B��B��B��B��B��B��B��B��B��B��B�B��B��B��B��B��B��B��B��B��B�B�/B�#B�/B�TB�mB�mB�BB�mB�B�B�B�B�B�B	  B	B	%B	B	%B		7B	DB	�B	�B	�B	�B	�B	�B	�B	 �B	&�B	'�B	-B	.B	-B	.B	.B	)�B	.B	7LB	;dB	<jB	=qB	=qB	>wB	?}B	>wB	D�B	K�B	N�B	P�B	P�B	Q�B	P�B	VB	ZB	\)B	\)B	\)B	\)B	\)B	^5B	^5B	`BB	gmB	m�B	l�B	m�B	n�B	n�B	n�B	o�B	m�B	jB	p�B	s�B	u�B	|�B	}�B	~�B	~�B	� B	� B	� B	� B	� B	�B	�B	�DB	�DB	�DB	�JB	�PB	�PB	�JB	�\B	�hB	�\B	�VB	�PB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�'B	�3B	�3B	�'B	�-B	�FB	�?B	�FB	�LB	�RB	�LB	�^B	�wB	�wB	�qB	B	ŢB	ƨB	ǮB	ǮB	ǮB	ƨB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�B	�#B	�/B	�5B	�BB	�;B	�HB	�HB	�BB	�HB	�HB	�BB	�HB	�NB	�HB	�TB	�fB	�yB	�B	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B	��B	��B	��B
B
B
B
B
B
B
B
B
B
B
B
+B
+B
	7B
DB
DB
DB
PB
DB
DB
PB
PB
PB
PB
PB
PB
VB
VB
\B
hB
uB
oB
oB
hB
bB
hB
\B
\B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
"�B
"�B
#�B
$�B
$�B
&�B
%�B
$�B
%�B
'�B
&�B
%�B
&�B
&�B
'�B
(�B
(�B
+B
+B
)�B
)�B
+B
)�B
,B
,B
-B
-B
.B
-B
.B
-B
,B
-B
.B
/B
.B
.B
2-B
49B
33B
33B
2-B
33B
2-B
2-B
33B
33B
49B
33B
5?B
5?B
5?B
5?B
49B
33B
33B
49B
5?B
7LB
7LB
6FB
6FB
6FB
6FB
6FB
8RB
8RB
8RB
8RB
8RB
7LB
7LB
8RB
8RB
9XB
9XB
:^B
:^B
<jB
<jB
;dB
9XB
:^B
=qB
=qB
<jB
=qB
=qB
=qB
=qB
=qB
=qB
>wB
=qB
<jB
;dB
>wB
A�B
A�B
A�B
@�B
@�B
?}B
@�B
C�B
B�B
B�B
B�B
D�B
F�B
F�B
E�B
E�B
F�B
E�B
F�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
H�B
G�B
H�B
I�B
I�B
H�B
H�B
G�B
G�B
J�B
J�B
I�B
J�B
L�B
L�B
L�B
L�B
M�B
L�B
O�B
O�B
O�B
N�B
O�B
N�B
N�B
Q�B
R�B
Q�B
Q�B
Q�B
R�B
Q�B
Q�B
Q�B
R�B
Q�B
S�B
VB
VB
VB
VB
T�B
VB
VB
VB
W
B
VB
W
B
T�B
VB
XB
YB
YB
ZB
ZB
ZB
ZB
ZB
[#B
\)B
\)B
\)B
\)B
]/B
]/B
\)B
\)B
\)B
]/B
]/B
\)B
^5B
]/B
\)B
\)B
]/B
^5B
^5B
^5B
_;B
^5B
_;B
`BB
`BB
`BB
_;B
aHB
bNB
bNB
bNB
bNB
bNB
aHB
bNB
aHB
`BB
^5B
_;B
bNB
cTB
cTB
dZB
e`B
e`B
e`B
e`B
e`B
e`B
dZB
dZB
e`B
e`B
dZB
dZB
dZB
dZB
ffB
ffB
ffB
gmB
gmB
gmB
ffB
gmB
hsB
hsB
gmB
gmB
hsB
hsB
hsB
hsB
hsB
iyB
iyB
jB
k�B
k�B
jB
jB
jB
l�B
k�B
l�B
l�B
l�B
l�B
l�B
l�B
n�B
n�B
m�B
m�B
m�B
m�B
n�B
o�B
n�B
m�B
n�B
n�B
o�B
o�B
o�B
p�B
p�B
p�B
p�B
p�B
o�B
o�B
q�B
r�B
r�B
r�B
r�B
r�B
q�B
q�B
q�B
r�B
s�B
s�B
s�B
s�B
s�B
t�B
t�B
t�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B��B�LB�RB�RB�XB�XB�XB�XB�XB�XB�XB�XB�^B�^B�^B�^B�XB�^B�^B�^B�rB��B�B��B�B�BraB��B��B9rB72B[WBU�B6�B:�BjKBz�Bd�B\]B�mB��B�[B�IB�BB��B�'B�B��B�B�uB�?B��Bv�BqBd&B`vBR�BO(BXEBTFBJ#B8�B:B�vB�DB��B��B�B�
B�)BϫB�JB��B�ZB��B��B��BjBE�BQhBT{Bb�B\�BL0B,�B-wB&fB�B0B
�B
�B
��B
�xB
�BB
�B
��B
��B
�vB
�PB
�B
|B
��B
��B
z*B
a�B
SuB
Q�B
a�B
\�B
[�B
V�B
N�B
G�B
8�B
:^B
,qB
-CB
$B
%zB
�B
 �B	��B	�XB	��B
 �B	��B	�B	��B	��B	�uB	�6B	�	B	��B	��B	�EG�O�B	a-B	ZkB	[�B	O�B	U�B	[WB	ZQB	TaB	H�B	:�B	=<B	>BB	3�B	#TB	5�B	O�B	J=B	E9B	AB	6+B	-CB	jB	#�B	!�B	)yB	-wB	5ZB	<PB	=�B	A�B	@�B	<�B	6+B	2�B	4�B	,B	&�B	�B	6B�+B�B��B��B�mB��B�/B��B˒B�+BǔB�HB��BāB��B�(B�qB��B�6B�B�B�B�FB��B�B��B�?B�~B�vB�FB� B�IB�EB��Bv�By�BaBlBh$BZ�B`�Bw�BuBpBd@BP�B8RB4B>(BK�BS@BM�B:*B0;B�B*�B5�BI�BMBH1BE9BB[B>BB@4B:DB,�B�B)DB(>B+6B&LB.B'B+�B BB$�B!�B+B�B�B#�B�B�B"NB�BsB�BuB5B�BMB�B1B]B=BKB1B+BBB�B�BdBB!BMB[B�BSBIBVB�BkB�B&2B(XB'RB%`B#:B!bB
B"hB+6B*eB'�B#�B-]B1'B/iB)�B�B"�B"�B,qB,�B*�B)�B&�B#�B$�B-wB2|B2�B7�B9rB:xB6zB0�B(�B$�B �B5�B>�BB�B>�B7fB@ B@OBB'BQ�BO�BP�BO�BNBK�BJ#B@iBKBHfBC�BB�BSuB\]B\�B\�BbhBb�B_�Bb�Bg�Bg�Bc BcBaHBiDBrBv�BtBs3By$B.B�B�4B�OBHB�OB�UB}�B��B��B��B��B��B��B��B��B��B��B��B��B�)B�B�B�*B�DB�RB��B�CB��B� B�nB��B��B�JB�VBΊB�uB�1B�IBیBݘB�B�B�B�B��B�B�B��B��B��B�TB	 OB	GB	%B	mB	tB		�B	�B	sB	�B	�B	�B	�B	B	B	!B	'B	(>B	-B	./B	-)B	.B	.IB	*�B	.�B	7�B	;B	<�B	=�B	=�B	>�B	?�B	>�B	D�B	K�B	N�B	Q B	P�B	RB	Q4B	VB	Z7B	\)B	\CB	\)B	\CB	\]B	^jB	^jB	`vB	g�B	m�B	l�B	m�B	n�B	n�B	n�B	o�B	m�B	j�B	p�B	s�B	u�B	|�B	}�B	B	B	� B	�B	�B	�B	�B	�;B	�B	�)B	�^B	�^B	�dB	�jB	�jB	�~B	�\B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�$B	�*B	�6B	�CB	�AB	�MB	�hB	�vB	�aB	�zB	�ZB	�zB	��B	�lB	��B	��B	��B	��B	��B	ªB	żB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�&B	�B	�,B	�B	�B	�?B	�EB	�WB	�IB	�OB	�\B	�VB	�bB	�bB	�\B	�bB	�bB	�vB	�|B	�B	�|B	�B	�B	�B	�B	�B	�B	�B	��B	��B	�B	��B	��B	�B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�	B	��B	�B	�B	�6B
  B	�B	�(B	�B
'B
B
-B
-B
AB
GB
MB
MB
9B
SB
SB
+B
_B
	RB
DB
^B
^B
PB
^B
^B
jB
jB
jB
jB
jB
jB
pB
pB
�B
�B
uB
oB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
"�B
#B
#�B
$�B
$�B
&�B
%�B
%B
%�B
(
B
'B
&B
'B
'B
(
B
)B
)B
+B
+B
*B
*B
+B
*0B
,"B
,"B
-)B
-)B
./B
-B
./B
-)B
,=B
-)B
./B
/5B
.IB
.IB
2B
49B
33B
33B
2GB
33B
2GB
2GB
3MB
33B
4TB
33B
5?B
5?B
5%B
5?B
4TB
3MB
3hB
4TB
5?B
7LB
72B
6`B
6FB
6FB
6`B
6FB
88B
8RB
8RB
8RB
8lB
7LB
7fB
8lB
8lB
9rB
9rB
:xB
:^B
<jB
<jB
;B
9�B
:�B
=qB
=�B
<�B
=�B
=�B
=�B
=qB
=�B
=�B
>]B
=�B
<�B
;�B
>�B
A�B
A�B
A�B
@�B
@�B
?�B
@�B
C�B
B�B
B�B
B�B
D�B
F�B
F�B
E�B
E�B
F�B
E�B
F�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
H�B
G�B
H�B
I�B
I�B
H�B
H�B
G�B
G�B
J�B
J�B
I�B
J�B
L�B
L�B
L�B
L�B
M�B
L�B
O�B
O�B
O�B
N�B
O�B
OB
N�B
Q�B
R�B
RB
Q�B
RB
SB
RB
RB
RB
SB
R B
TB
VB
VB
VB
VB
UB
VB
VB
VB
W
B
VB
W$B
U2B
V9B
X+B
Y1B
Y1B
ZB
Z7B
Z7B
Z7B
Z7B
[#B
\)B
\)B
\CB
\CB
]/B
]/B
\CB
\CB
\CB
]/B
]IB
\CB
^5B
]IB
\CB
\CB
]IB
^OB
^OB
^OB
_VB
^OB
_VB
`BB
`\B
`\B
_VB
abB
bNB
bNB
bNB
bNB
bNB
abB
bNB
abB
`\B
^OB
_pB
bhB
cnB
cTB
dtB
e`B
e`B
e`B
e`B
eFB
e`B
dtB
dtB
ezB
e`B
dtB
dtB
dtB
dtB
ffB
f�B
f�B
gmB
g�B
g�B
f�B
gmB
hsB
hsB
g�B
g�B
h�B
h�B
h�B
h�B
h�B
i�B
iyB
jB
k�B
kkB
j�B
j�B
j�B
lqB
k�B
l�B
lqB
l�B
l�B
l�B
l�B
n}B
n�B
m�B
m�B
m�B
m�B
n�B
o�B
n�B
m�B
n�B
n�B
o�B
o�B
o�B
p�B
p�B
p�B
p�B
p�B
o�B
o�B
q�B
r�B
r�B
r�B
r�B
r�B
q�B
q�B
q�B
r�B
s�B
s�B
s�B
s�B
s�B
t�B
t�B
t�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<%zx<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=-0.02(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201809060034402018090600344020180906003440201809060200162018090602001620180906020016201809070023572018090700235720180907002357  JA  ARFMdecpA19c                                                                20180902093513  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180902003514  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180902003517  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180902003518  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180902003518  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180902003518  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180902003518  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcp2.8e                                                                20180902003518  QCF$                G�O�G�O�G�O�            4000JA  ARGQaqcp2.8e                                                                20180902003518  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180902003518  QCF$                G�O�G�O�G�O�            4000JA  ARGQrqcpt16c                                                                20180902003519  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180902003519                      G�O�G�O�G�O�                JA  ARUP                                                                        20180902005617                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180902153456  CV  JULD            G�O�G�O�F��                JM  ARSQJMQC2.0                                                                 20180903000000  CF  PSAL_ADJUSTED_QCC�  C�  G�O�                JM  ARSQJMQC2.0                                                                 20180903000000  CF  TEMP_ADJUSTED_QCC�  C�  G�O�                JM  ARCAJMQC2.0                                                                 20180905153440  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180905153440  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180905170016  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180906152357  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115131518                      G�O�G�O�G�O�                