CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-09-20T00:35:16Z creation;2018-09-20T00:35:21Z conversion to V3.1;2019-12-19T07:32:17Z update;     
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
resolution        =���   axis      Z        d  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I    PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     d  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \@   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     d  `   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     d  s\   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     d  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     d  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �@   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     d  �   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     d  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     d  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �H   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �X   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �\   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �`   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �d   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �h   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20180920003516  20200115131517  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL              A   JA  I2_0576_282                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @؂��C� 1   @؂�����@9��IQ���dI*0U2a1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @333@�  @�  A   A   A@  A`  A�  A���A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BHffBP  BX  B`  Bg��Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~�C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�<�D�|�D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D��3D�3D�)�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @4z�@���@���A Q�A Q�A@Q�A`Q�A�(�A���A�(�A�(�A�(�A�(�A�(�A�(�B {B{B{B{B {B({B0{B8{B@{BHz�BP{BX{B`{Bg�Bp{Bx{B�
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
=B�=qB�
=B�
=B�
=B�
=C CCCCC
CCCCCCCCCCC C"C$C&C(C*C,C.C0C2C4C6C8C:C<C>C@CBCDCFCHCJCLCNCPCRCTCVCXCZC\C^C`CbCdCfChCjClCnCpCrCtCvCxCzC|C~�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D HD �HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HD	HD	�HD
HD
�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HD HD �HD!HD!�HD"HD"�HD#HD#�HD$HD$�HD%HD%�HD&HD&�HD'HD'�HD(HD(�HD)HD)�HD*HD*�HD+HD+�HD,HD,�HD-HD-�HD.HD.�HD/HD/�HD0HD0�HD1HD1�HD2HD2�HD3HD3�HD4HD4�HD5HD5�HD6HD6�HD7HD7�HD8HD8�HD9HD9�HD:HD:�HD;HD;�HD<HD<�HD=HD=�HD>HD>�HD?HD?�HD@HD@�HDAHDA�HDBHDB�HDCHDC�HDDHDD�HDEHDE�HDFHDF�HDGHDG�HDHHDH�HDIHDI�HDJHDJ�HDKHDK�HDLHDL�HDMHDM�HDNHDN�HDOHDO�HDPHDP�HDQHDQ�HDRHDR�HDSHDS�HDTHDT�HDUHDU�HDVHDV�HDWHDW�HDXHDX�HDYHDY�HDZHDZ�HD[HD[�HD\HD\�HD]HD]�HD^HD^�HD_HD_�HD`HD`�HDaHDa�HDbHDb�HDcHDc�HDdHDd�HDeHDe�HDfHDf�HDgHDg�HDhHDh�HDiHDi�HDjHDj�HDkHDk�HDlHDl�HDmHDm�HDnHDn�HDoHDo�HDpHDp�HDqHDq�HDrHDr�HDsHDs�HDtHDt�HDuHDu�HDvHDv�HDwHDw�HDxHDx�HDyHDy�HDzHDz�HD{HD{�HD|HD|�HD}HD}�HD~HD~�HDHD�HD� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D��D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D�D���D� �D�@�DÀ�D���D� �D�@�DĀ�D���D� �D�@�Dŀ�D���D� �D�@�Dƀ�D���D� �D�@�Dǀ�D���D� �D�@�DȀ�D���D� �D�@�Dɀ�D���D� �D�@�Dʀ�D���D� �D�@�Dˀ�D���D� �D�@�D̀�D���D� �D�@�D̀�D���D� �D�@�D΀�D���D� �D�@�Dπ�D���D� �D�@�DЀ�D���D� �D�@�Dр�D���D� �D�@�DҀ�D���D� �D�@�DӀ�D���D� �D�@�DԀ�D���D� �D�@�DՀ�D���D� �D�@�Dր�D���D� �D�@�D׀�D���D� �D�@�D؀�D���D� �D�@�Dـ�D���D� �D�@�Dڀ�D���D� �D�@�Dۀ�D���D� �D�=qD�}qD���D� �D�@�D݀�D���D� �D�@�Dހ�D���D� �D�@�D߀�D���D� �D�@�D���D���D� �D�@�DဤD���D� �D�@�D․D���D� �D�@�D〤D���D� �D�@�D䀤D���D� �D�@�D値D���D� �D�@�D怤D���D� �D�@�D瀤D���D� �D�@�D耤D���D� �D�@�D逤D���D� �D�@�DꀤD���D� �D�@�D뀤D���D� �D�@�D쀤D���D� �D�@�D퀤D���D� �D�@�DD���D� �D�@�DD���D� �D�@�D���D���D� �D�@�D�D���D� �D�@�D�D���D� �D�@�D�D���D� �D�@�D�D���D� �D�@�D���D���D��D�*=1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�|�A�bNA�XA�K�A�A�A�;dA�;dA�9XA�7LA�7LA�1'A�/A�+A�(�A�&�A�(�A�+A�&�A�&�A�+A� �A��A� �A� �A� �A��A��A��A�I�A��/A�&�Aʇ+A�p�A�JA��A��A�A�z�A��A��#A�r�A���A���A���A�1A�ĜA�\)A��A��A�;dA��A��9A�G�A�VA�l�A��A�?}A�A��hA�+A�VA���A��A��/A�  A�+A�C�A�VA�A�7LA�hsA���A�M�A�=qA��+A���A���A��A�Q�A��A��HA�ZA�=qA�=qA���A��DA�A��!A�C�A��A��jA�ƨA�oA�dZA��uA�C�A��`A�`BA�  A�;dA�%A�(�A�A��A���A��A���A�I�A&�A~$�A}S�A{G�Az^5AxffAw"�Av�jAvffAul�At�+Ar�uAq%Ap��Ap�9ApjAooAn �Am�Aln�Ak�
Aj�Ah~�Af�HAeS�Adr�Ac�Ac��AbȴAa�^A_S�A]�TA\��A[dZAZ�\AZ$�AY�-AX�/AXM�AW"�AVE�AUƨAUXAS�
ASG�AR�AQ�AP�!AO�wAN5?AJ��AIp�AH��AH�9AG�AEoAD{AC��AB�HAA/A@��A@5?A?A>M�A=A=�A;�A9ƨA8�\A7+A5ƨA5`BA4�RA4r�A3�A2r�A1�#A1G�A0�\A.ffA-"�A,�A,�A+�-A+hsA+�A*��A*bNA)�mA)��A)��A)O�A)&�A)�A(��A'�;A'&�A&1A$�A#?}A"VA!"�A =qA�\A��AQ�AA�A�AXAZAS�A�\A=qA��A�A�A��A=qA��A��A�A�\AQ�A��AVAM�AȴAE�A\)A7LA33A+A
��A
�yA
�/A
�A
JA^5A�jA�;A�yA�TAO�A&�AĜA{A �/A 1@�$�@�|�@���@�Q�@�=q@�G�@���@�J@� �@�\)@���@�Z@띲@�@�n�@�=q@�{@�-@�p�@�r�@�F@�9@�G�@�Q�@�=q@݁@�G�@�z�@���@��#@��/@��@�|�@��@�z�@�7L@ЋD@υ@��y@θR@�~�@Ͳ-@̛�@�S�@�
=@�{@�?}@ȋD@���@�t�@�C�@��@��T@ÍP@��H@+@��#@��D@�C�@���@��/@�I�@���@�K�@���@�@�/@�  @���@���@�M�@���@�Q�@���@�ƨ@�|�@�;d@�^5@��h@�A�@���@���@���@��@�"�@���@�$�@���@�x�@�7L@���@���@�z�@��@�5?@�33@���@���@�p�@���@��m@�@��\@�/@�A�@���@��@��@��R@��+@��@���@��-@��7@�&�@���@�r�@�A�@� �@�  @��w@�K�@�
=@�~�@�O�@��@�Q�@�(�@�ƨ@��P@�S�@�C�@�
=@���@�@���@�G�@�bN@�1'@��@��@��\@�~�@�V@��@��#@�7L@�j@� �@���@�\)@�
=@�~�@�@��7@�/@���@���@��`@��D@�(�@��
@�|�@�C�@���@���@�@��@��T@��#@�hs@��@�Ĝ@��D@�I�@�I�@� �@��;@�|�@�S�@�o@��y@���@���@�=q@��@��@���@��h@�7L@���@���@�1'@�w@;d@~�@~��@~V@~{@}�-@}?}@|�/@|(�@{ƨ@{��@{C�@z�!@z~�@z-@y�^@y��@y��@y�7@yG�@xr�@w�P@w;d@w+@v�+@v5?@v$�@vE�@vȴ@v{@t��@t1@r�@q�#@r�\@t9X@tZ@t�D@tI�@t1@s�
@s��@sS�@r�\@rM�@q��@q��@q�@p��@pĜ@p�9@pĜ@o�;@o��@pb@o�w@n@lz�@l(�@kC�@kS�@kdZ@kt�@k33@k@k"�@j�H@i7L@h�u@h�u@hĜ@h1'@g�P@g
=@fȴ@f��@fff@f{@e�@e`B@eV@d��@dj@c�
@c�F@c��@cC�@co@b�@b��@b�\@b^5@b=q@bJ@a��@a�^@aG�@`��@`�u@`Q�@`  @_��@_�@_
=@^�y@^�y@^�R@^��@^v�@^{@]�T@]V@\�@\1@[�@[dZ@[S�@[C�@[33@Z�!@Z^5@Z-@Y�7@YX@Y7L@Xb@WK�@V��@V��@Vff@U�T@U�-@Up�@U?}@U/@U�@T�@TI�@S�F@SdZ@SC�@S33@So@R�\@Q��@Q�7@QX@Q&�@Q%@P�`@PĜ@P�u@PA�@P �@OK�@O
=@N�y@N�R@Nff@M�@M�T@M/@L��@L��@Lz�@LZ@L(�@K�
@K�F@K��@KdZ@J��@Jn�@J=q@I�#@I��@Ix�@IX@I�@H�9@H1'@G�;@G��@G\)@F��@F5?@F5?@F$�@E�-@E�@E�@E��@E�@Ep�@D��@DI�@D9X@D9X@D9X@D�@C��@C�
@C"�@B��@B~�@B-@A�^@Ahs@AG�@A%@@��@@�u@@�u@@bN@@b@@ �@@Q�@@Q�@@1'@@ �@@b@?�@>�y@>��@>ȴ@>ȴ@?\)@?+@>��@>��@=�-@=�@<�/@<�D@<9X@<1@;ƨ@<�@;��@;S�@:�H@:�!@:�\@:n�@:�!@:n�@:~�@:^5@:=q@:M�@:M�@:-@:�@9�#@9��@9�@8��@8bN@8  @7��@7|�@7l�@7�@7�w@7��@7|�@7K�@6ȴ@5@5?}@5/@4�/@4�j@4�@4�@4j@4�@3�F@3t�@3C�@2^5@2M�@2-@2J@1��@1�#@1��@1X@17L@0�9@0Q�@01'@0  @/��@/�@/�@/��@/��@/|�@/\)@/;d@/+@/
=@.�R@.�+@.5?@-��@-�-@-��@-�@-`B@-?}@,��@,z�@,I�@,(�@,1@+�m@+��@+33@+"�@*�@*��@*n�@*-@)�#@)��@)X@)&�@)%@(�9@(r�@(1'@'�;@'�@'K�@&ȴ@&E�@%�T@%?}@$�@$��@$j@$�@#�m@#��@#�@#"�@"�!@"�\@"-@"J@!�@!��@!hs@!&�@ ��@ �u@ 1'@�w@��@�P@;d@�y@ȴ@V@V@E�@@�T@��@�@�/@z�@1@�m@�F@C�@33@@��@��@n�@M�@J@��@��@��@X@�`@Ĝ@Ĝ@��@�u@bN@A�@1'@ �@b@�P@l�@;d@�@��@�+@E�@$�@�@�T@�-@�h@O�@/@��@�j@9X@��@��@�m@��@t�@33@o@��@�!@��@��@�\@�\@n�@^5@-@�@��@x�@�@��@��@�`@��@Ĝ@Ĝ@�9@��@bN@ �@�;@��@��@l�@��@�@�R@��@�+@E�@{@�T@��@@�-@p�@?}@/@�@V@��@�@��@��@�j@�j@��@�j@�@��@z�@9X@�@�@�@�@�@�@�@�@��@��@�@C�@
�H@
�\@
M�@
�@	��@	X@	G�@	�@��@��@��@��@Ĝ@�9@�@bN@Q�@Q�@A�@A�@1'@ �@  @  @�@�w@��@K�@+@�@�y@�@ȴ@ȴ@ȴ@ȴ@�R@��@�+@ff@V@$�@$�@{@$�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�|�A�bNA�XA�K�A�A�A�;dA�;dA�9XA�7LA�7LA�1'A�/A�+A�(�A�&�A�(�A�+A�&�A�&�A�+A� �A��A� �A� �A� �A��A��A��A�I�A��/A�&�Aʇ+A�p�A�JA��A��A�A�z�A��A��#A�r�A���A���A���A�1A�ĜA�\)A��A��A�;dA��A��9A�G�A�VA�l�A��A�?}A�A��hA�+A�VA���A��A��/A�  A�+A�C�A�VA�A�7LA�hsA���A�M�A�=qA��+A���A���A��A�Q�A��A��HA�ZA�=qA�=qA���A��DA�A��!A�C�A��A��jA�ƨA�oA�dZA��uA�C�A��`A�`BA�  A�;dA�%A�(�A�A��A���A��A���A�I�A&�A~$�A}S�A{G�Az^5AxffAw"�Av�jAvffAul�At�+Ar�uAq%Ap��Ap�9ApjAooAn �Am�Aln�Ak�
Aj�Ah~�Af�HAeS�Adr�Ac�Ac��AbȴAa�^A_S�A]�TA\��A[dZAZ�\AZ$�AY�-AX�/AXM�AW"�AVE�AUƨAUXAS�
ASG�AR�AQ�AP�!AO�wAN5?AJ��AIp�AH��AH�9AG�AEoAD{AC��AB�HAA/A@��A@5?A?A>M�A=A=�A;�A9ƨA8�\A7+A5ƨA5`BA4�RA4r�A3�A2r�A1�#A1G�A0�\A.ffA-"�A,�A,�A+�-A+hsA+�A*��A*bNA)�mA)��A)��A)O�A)&�A)�A(��A'�;A'&�A&1A$�A#?}A"VA!"�A =qA�\A��AQ�AA�A�AXAZAS�A�\A=qA��A�A�A��A=qA��A��A�A�\AQ�A��AVAM�AȴAE�A\)A7LA33A+A
��A
�yA
�/A
�A
JA^5A�jA�;A�yA�TAO�A&�AĜA{A �/A 1@�$�@�|�@���@�Q�@�=q@�G�@���@�J@� �@�\)@���@�Z@띲@�@�n�@�=q@�{@�-@�p�@�r�@�F@�9@�G�@�Q�@�=q@݁@�G�@�z�@���@��#@��/@��@�|�@��@�z�@�7L@ЋD@υ@��y@θR@�~�@Ͳ-@̛�@�S�@�
=@�{@�?}@ȋD@���@�t�@�C�@��@��T@ÍP@��H@+@��#@��D@�C�@���@��/@�I�@���@�K�@���@�@�/@�  @���@���@�M�@���@�Q�@���@�ƨ@�|�@�;d@�^5@��h@�A�@���@���@���@��@�"�@���@�$�@���@�x�@�7L@���@���@�z�@��@�5?@�33@���@���@�p�@���@��m@�@��\@�/@�A�@���@��@��@��R@��+@��@���@��-@��7@�&�@���@�r�@�A�@� �@�  @��w@�K�@�
=@�~�@�O�@��@�Q�@�(�@�ƨ@��P@�S�@�C�@�
=@���@�@���@�G�@�bN@�1'@��@��@��\@�~�@�V@��@��#@�7L@�j@� �@���@�\)@�
=@�~�@�@��7@�/@���@���@��`@��D@�(�@��
@�|�@�C�@���@���@�@��@��T@��#@�hs@��@�Ĝ@��D@�I�@�I�@� �@��;@�|�@�S�@�o@��y@���@���@�=q@��@��@���@��h@�7L@���@���@�1'@�w@;d@~�@~��@~V@~{@}�-@}?}@|�/@|(�@{ƨ@{��@{C�@z�!@z~�@z-@y�^@y��@y��@y�7@yG�@xr�@w�P@w;d@w+@v�+@v5?@v$�@vE�@vȴ@v{@t��@t1@r�@q�#@r�\@t9X@tZ@t�D@tI�@t1@s�
@s��@sS�@r�\@rM�@q��@q��@q�@p��@pĜ@p�9@pĜ@o�;@o��@pb@o�w@n@lz�@l(�@kC�@kS�@kdZ@kt�@k33@k@k"�@j�H@i7L@h�u@h�u@hĜ@h1'@g�P@g
=@fȴ@f��@fff@f{@e�@e`B@eV@d��@dj@c�
@c�F@c��@cC�@co@b�@b��@b�\@b^5@b=q@bJ@a��@a�^@aG�@`��@`�u@`Q�@`  @_��@_�@_
=@^�y@^�y@^�R@^��@^v�@^{@]�T@]V@\�@\1@[�@[dZ@[S�@[C�@[33@Z�!@Z^5@Z-@Y�7@YX@Y7L@Xb@WK�@V��@V��@Vff@U�T@U�-@Up�@U?}@U/@U�@T�@TI�@S�F@SdZ@SC�@S33@So@R�\@Q��@Q�7@QX@Q&�@Q%@P�`@PĜ@P�u@PA�@P �@OK�@O
=@N�y@N�R@Nff@M�@M�T@M/@L��@L��@Lz�@LZ@L(�@K�
@K�F@K��@KdZ@J��@Jn�@J=q@I�#@I��@Ix�@IX@I�@H�9@H1'@G�;@G��@G\)@F��@F5?@F5?@F$�@E�-@E�@E�@E��@E�@Ep�@D��@DI�@D9X@D9X@D9X@D�@C��@C�
@C"�@B��@B~�@B-@A�^@Ahs@AG�@A%@@��@@�u@@�u@@bN@@b@@ �@@Q�@@Q�@@1'@@ �@@b@?�@>�y@>��@>ȴ@>ȴ@?\)@?+@>��@>��@=�-@=�@<�/@<�D@<9X@<1@;ƨ@<�@;��@;S�@:�H@:�!@:�\@:n�@:�!@:n�@:~�@:^5@:=q@:M�@:M�@:-@:�@9�#@9��@9�@8��@8bN@8  @7��@7|�@7l�@7�@7�w@7��@7|�@7K�@6ȴ@5@5?}@5/@4�/@4�j@4�@4�@4j@4�@3�F@3t�@3C�@2^5@2M�@2-@2J@1��@1�#@1��@1X@17L@0�9@0Q�@01'@0  @/��@/�@/�@/��@/��@/|�@/\)@/;d@/+@/
=@.�R@.�+@.5?@-��@-�-@-��@-�@-`B@-?}@,��@,z�@,I�@,(�@,1@+�m@+��@+33@+"�@*�@*��@*n�@*-@)�#@)��@)X@)&�@)%@(�9@(r�@(1'@'�;@'�@'K�@&ȴ@&E�@%�T@%?}@$�@$��@$j@$�@#�m@#��@#�@#"�@"�!@"�\@"-@"J@!�@!��@!hs@!&�@ ��@ �u@ 1'@�w@��@�P@;d@�y@ȴ@V@V@E�@@�T@��@�@�/@z�@1@�m@�F@C�@33@@��@��@n�@M�@J@��@��@��@X@�`@Ĝ@Ĝ@��@�u@bN@A�@1'@ �@b@�P@l�@;d@�@��@�+@E�@$�@�@�T@�-@�h@O�@/@��@�j@9X@��@��@�m@��@t�@33@o@��@�!@��@��@�\@�\@n�@^5@-@�@��@x�@�@��@��@�`@��@Ĝ@Ĝ@�9@��@bN@ �@�;@��@��@l�@��@�@�R@��@�+@E�@{@�T@��@@�-@p�@?}@/@�@V@��@�@��@��@�j@�j@��@�j@�@��@z�@9X@�@�@�@�@�@�@�@�@��@��@�@C�@
�H@
�\@
M�@
�@	��@	X@	G�@	�@��@��@��@��@Ĝ@�9@�@bN@Q�@Q�@A�@A�@1'@ �@  @  @�@�w@��@K�@+@�@�y@�@ȴ@ȴ@ȴ@ȴ@�R@��@�+@ff@V@$�@$�@{@$�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�BB�TB�TB�ZB�`B�`B�`B�`B�`B�`B�`B�`B�fB�fB�fB�fB�fB�fB�fB�`B�`B�fB�fB�fB�`B�ZB�BB��B��B�dB�9B�LB�B��B��B�FB�B��B��B�B��B�B_;B}�B�%B�JB��B�1Bx�BM�BffBk�BYBN�B;dBC�BF�BK�BA�B0!B-B�B�BoB+B��B�mB�;B�B�B��B��B��B�qB�B��B��B��B��B�BcTBL�BS�BYBR�BH�BE�B;dB)�B�BJB1B
��B
��B
��B
�B
�/B
ÖB
�XB
B
�B
��B
��B
�hB
�PB
�\B
�B
t�B
n�B
k�B
S�B
VB
I�B
C�B
K�B
H�B
=qB
2-B
%�B
�B
+B
+B
$�B
�B
oB
VB

=B
+B	��B	�mB	�5B	��B	��B	��B	ǮB	�jB	�B	��B	�hB	�=B	� B	�B	� B	|�B	r�B	n�B	e`B	cTB	dZB	_;B	N�B	N�B	F�B	E�B	?}B	49B	-B	VB	JB	�B	uB	1B�B��B��B��B�yB��B�B�`B�sB�B�mB�)B�#B�B��B��B�)B�B�
B��BŢB��BƨB�}B�B�!B�XB�LB�?B�LB�?B�FB�'B�B�-B�'B�B�B�B��B��B��B�JB�B�B� Bz�Bv�Bl�BjBw�Bx�Bt�Bk�BgmBbNBdZBgmBbNB\)BR�BW
B\)B\)B]/BYBW
BXBQ�BM�BG�B8RB2-BA�BN�BQ�BP�BN�BN�BM�BI�B@�B2-B.B7LB49B5?B9XB<jB8RB1'B(�B,B&�B"�B%�B%�B!�B(�B(�B�B�B&�B!�B �B'�B(�B)�B,B+B'�B&�B�B�B\B
=B�B�B"�B&�B"�B�B �B!�B"�B"�B�B�BoB&�B'�B)�B-B,B&�B%�B$�B,B'�B(�B)�B+B/B-B)�B$�B�B+B,B+B&�B,B+B33B5?B6FB7LB7LB6FB49B33B8RB>wB>wB=qB9XBA�BD�BC�BA�B>wB=qB=qBC�BE�B@�BL�BT�BXBYB\)B]/B^5B^5B_;B^5BZBR�BQ�Bn�Bo�Bs�Bt�Bs�Bv�Bz�Bx�B�B�1B�DB�bB�bB�hB�oB��B�{B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�!B�!B�9B�LB�XB�XB�RB�dB��B��B��BɺBȴB��B��B��B��B��B��B��B�B�;B�;B�NB�NB�TB�ZB�B�B�B�B�B�B��B��B��B��B��B��B	B		7B	
=B	
=B		7B	VB	{B	{B	�B	�B	�B	�B	�B	�B	�B	�B	 �B	"�B	"�B	'�B	(�B	+B	,B	-B	2-B	8RB	:^B	?}B	B�B	E�B	H�B	I�B	J�B	J�B	L�B	O�B	Q�B	VB	YB	[#B	_;B	bNB	cTB	dZB	hsB	hsB	hsB	gmB	ffB	jB	p�B	r�B	r�B	v�B	y�B	z�B	� B	�B	�B	�B	�B	�+B	�\B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	�B	�B	�B	��B	�B	�B	�'B	�3B	�3B	�LB	�^B	�}B	�wB	�dB	�qB	ĜB	ŢB	ĜB	ÖB	ĜB	ŢB	ŢB	ŢB	ĜB	ĜB	ĜB	ƨB	ǮB	ƨB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�#B	�5B	�;B	�5B	�;B	�BB	�BB	�HB	�BB	�TB	�ZB	�`B	�sB	�yB	�yB	�yB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
%B
%B
%B
+B
+B
1B
+B
+B

=B
DB
DB
PB
VB
bB
bB
\B
bB
bB
hB
oB
hB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
 �B
�B
 �B
"�B
#�B
#�B
#�B
"�B
"�B
 �B
 �B
"�B
"�B
"�B
#�B
#�B
&�B
%�B
&�B
&�B
'�B
'�B
'�B
(�B
)�B
+B
,B
,B
-B
-B
-B
.B
.B
-B
.B
0!B
/B
/B
/B
0!B
2-B
33B
49B
33B
33B
2-B
1'B
0!B
2-B
5?B
5?B
6FB
7LB
7LB
6FB
6FB
6FB
7LB
7LB
6FB
9XB
:^B
:^B
;dB
;dB
;dB
:^B
;dB
;dB
;dB
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
A�B
A�B
A�B
A�B
@�B
@�B
@�B
B�B
C�B
C�B
C�B
B�B
B�B
D�B
D�B
D�B
C�B
D�B
D�B
E�B
F�B
F�B
G�B
F�B
G�B
G�B
G�B
H�B
G�B
G�B
H�B
I�B
I�B
J�B
K�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
N�B
M�B
N�B
N�B
N�B
N�B
O�B
N�B
N�B
N�B
M�B
O�B
O�B
N�B
N�B
O�B
N�B
Q�B
Q�B
P�B
P�B
O�B
N�B
O�B
N�B
N�B
P�B
P�B
O�B
P�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
R�B
R�B
Q�B
Q�B
Q�B
T�B
T�B
T�B
T�B
S�B
T�B
T�B
T�B
T�B
S�B
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
XB
XB
XB
XB
XB
XB
YB
YB
XB
ZB
[#B
[#B
ZB
ZB
[#B
[#B
[#B
]/B
]/B
]/B
]/B
]/B
]/B
]/B
]/B
]/B
]/B
]/B
]/B
_;B
`BB
`BB
`BB
`BB
`BB
`BB
`BB
_;B
_;B
`BB
aHB
`BB
`BB
`BB
bNB
bNB
bNB
bNB
bNB
bNB
cTB
cTB
cTB
cTB
cTB
cTB
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
ffB
ffB
gmB
ffB
ffB
ffB
ffB
e`B
e`B
e`B
e`B
e`B
e`B
ffB
gmB
ffB
hsB
iyB
iyB
iyB
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
k�B
k�B
k�B
k�B
k�B
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
o�B
o�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�vB�TB�nB�tB�`B�`B�`B�`B�`B�`B�`B�`B�fB�fB�fB�fB�fB�fB�fB�zB�`B�fB�fB�fB�zB�B�bBοB��B�dB�DB��B�|B�B�[B��B�B��B��B�;B��B�_Be�B�oB�B�BB��B��B|BR�BhXBl�B[qBQB>(BESBG�BLdBB�B2-B.�B �BB{B	7B�(B�0B�B�B��B�}BЗB�bB�.B��B��B�jB�;B�9B�YBf�BOvBU�BY�BS�BI�BF�B<�B+�B�BpB	�B
��B
��B
��B
�B
��B
ƎB
�B
�{B
�5B
�EB
��B
�[B
��B
�B
�{B
v`B
o�B
l�B
VmB
WYB
K�B
EB
LJB
IRB
>�B
3�B
(
B
dB
+6B
+QB
%�B
7B
�B
�B
)B
1B
 iB	�KB	�'B	յB	��B	�jB	�fB	��B	��B	�kB	�&B	��B	��B	��B	��B	}�B	s�B	o�B	f�B	dZB	eB	`B	P�B	O�B	HB	F�B	@�B	5�B	/iB	�B	�B	+B	FB	
	B�nB�B��B�B�6B�+B�B��B�yB�qB�B�jB�IB��BּBңBܬB��BרB��B�zB�xBǔB��B��B��B�B�B��B��B��B��B��B��B�aB�[B��B�]B�QB��B��B��B�B�B��B�oB|�BxRBn�Bl�BxBy$BuZBl�Bh�Bc�Be`Bh
Bc:B]dBT�BXB\�B\�B]�BY�BW�BX�BR�BN�BIB:�B5BBuBN�BRBQBO(BOBN"BJXBA�B4�B0;B8�B5�B6zB:B<�B9$B2aB*�B-CB(XB$�B'B'B#:B)�B)�BpB�B'�B"�B!�B(sB)yB*eB,=B+B(XB'8B �B�B�BdB�B�B#TB'8B#�B�B!�B"hB#nB#�B�B�BaB'mB(�B*eB-CB,WB'�B&�B%�B,=B(�B)�B*�B+�B/iB-]B*B%�BIB+kB,qB+�B(
B,�B,"B3�B5�B6�B7�B7�B6�B4�B4B9	B>�B>�B=�B:^BA�BD�BC�BA�B?.B>(B>]BD3BFYBA�BM�BUgBXyBYB\xB]~B^�B^�B_�B^�BZ�BT{BS�Bo Bp;BtBuZBtnBw�B{dBy�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�!B��B�\B��B�`B�IB�;B�oB�nB��B��B��B��B��B��B�B� B��B�B�0B�<B�B�,B�FB�FB�uB֡B�pBߤB�B�B��B��B��B��B��B��B��B��B�B�B�*B�B�6B�(B	oB		RB	
XB	
rB		�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	 �B	#B	# B	(
B	)B	+6B	,=B	-]B	2|B	8�B	:�B	?�B	B�B	E�B	H�B	I�B	J�B	J�B	L�B	PB	R:B	V9B	Y1B	[=B	_pB	bhB	c�B	d�B	hsB	h�B	h�B	g�B	f�B	j�B	p�B	r�B	r�B	v�B	y�B	z�B	� B	�aB	��B	�gB	��B	�zB	�(B	�B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	��B	�B	�B	�
B	�B	�B	�KB	�"B	�B	�cB	��B	��B	�"B	�WB	�'B	�3B	�MB	�fB	�xB	�cB	��B	��B	��B	ĜB	ŢB	��B	��B	��B	żB	żB	żB	ĶB	ĜB	ĶB	��B	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B	� B	�B	�B	�&B	�,B	��B	�+B	�B	�B	�7B	�WB	�OB	�;B	�OB	�VB	�\B	�vB	�bB	��B	�B	�B	�B	�XB	�yB	�yB	�B	�B	�B	�B	�B	��B	��B	��B	��B	�B	��B	�B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	�B	�B	��B	�B	�B	��B	�B	�B	�B	�"B	��B	�HB
 B
'B
'B
 B
;B
-B
[B
-B
9B
?B
?B
?B
EB
EB
KB
EB
_B

=B
^B
^B
jB
pB
}B
}B
�B
�B
}B
�B
�B
�B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
 �B
�B
 �B
"�B
#�B
#�B
#�B
"�B
#B
!B
 �B
"�B
"�B
"�B
#�B
#�B
&�B
&B
'B
'B
(
B
(
B
'�B
(�B
*B
+B
,"B
+�B
,�B
-B
-)B
./B
./B
-CB
.IB
0;B
/OB
/5B
/5B
0;B
2B
33B
49B
3MB
33B
2-B
1[B
0�B
2aB
5ZB
5ZB
6`B
7LB
7LB
6`B
6`B
6zB
7fB
7fB
6�B
9>B
:xB
:xB
;B
;B
;dB
:xB
;B
;�B
;�B
=qB
=�B
>�B
>�B
?}B
?}B
?}B
?�B
?�B
?}B
?�B
?�B
?�B
?�B
?�B
@�B
A�B
A�B
A�B
A�B
@�B
@�B
@�B
B�B
C�B
C�B
C�B
B�B
B�B
D�B
D�B
D�B
C�B
D�B
D�B
E�B
F�B
F�B
G�B
F�B
G�B
G�B
G�B
H�B
G�B
G�B
H�B
I�B
I�B
J�B
K�B
L�B
L�B
L�B
L�B
L�B
MB
MB
N�B
NB
N�B
N�B
N�B
N�B
O�B
N�B
O(B
N�B
NB
O�B
O�B
N�B
N�B
O�B
N�B
Q�B
Q�B
Q B
Q B
O�B
OB
O�B
N�B
N�B
Q B
Q B
O�B
Q B
Q B
P�B
Q B
RB
RB
Q�B
R�B
R�B
Q�B
RB
R B
T�B
T�B
UB
T�B
S�B
T�B
T�B
UB
UB
T,B
UB
UB
UB
VB
W
B
W$B
W$B
X+B
X+B
X+B
X+B
XB
X+B
Y1B
Y1B
XEB
Z7B
[#B
[=B
Z7B
Z7B
[=B
[#B
[=B
]/B
]/B
]/B
]/B
]/B
]IB
]/B
]IB
]IB
]/B
]IB
]IB
_;B
`BB
`BB
`BB
`BB
`BB
`BB
`\B
_VB
_VB
`\B
aHB
`BB
`BB
`\B
bhB
bhB
bNB
bhB
b�B
bhB
cnB
cTB
c:B
cnB
cnB
c�B
dZB
e`B
e`B
e`B
eFB
e`B
e`B
e`B
e`B
e`B
e`B
eFB
e`B
ezB
ezB
ezB
ffB
ffB
gRB
fLB
fLB
ffB
ffB
ezB
ezB
ezB
ezB
ezB
ezB
ffB
g�B
f�B
hsB
iyB
i�B
i�B
jB
jB
jB
jB
j�B
j�B
k�B
kkB
k�B
k�B
k�B
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
m�B
m�B
m�B
m�B
mwB
mwB
m�B
mwB
m�B
n�B
n�B
n�B
n�B
o�B
o�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<2��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=-0.02(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201809240034342018092400343420180924003434201809240200162018092402001620180924020016201809250028262018092500282620180925002826  JA  ARFMdecpA19c                                                                20180920093513  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180920003516  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180920003519  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180920003520  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180920003521  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180920003521  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180920003521  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180920003521  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20180920003521  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180920003521                      G�O�G�O�G�O�                JA  ARUP                                                                        20180920005630                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180920153338  CV  JULD            G�O�G�O�F��                JM  ARCAJMQC2.0                                                                 20180923153434  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180923153434  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180923170016  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180924152826  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115131517                      G�O�G�O�G�O�                