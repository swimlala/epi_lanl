CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2017-12-09T00:35:23Z creation;2017-12-09T00:35:26Z conversion to V3.1;2019-12-19T07:54:44Z update;     
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20171209003523  20200115121517  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0576_187                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @�;u�1N 1   @�;v����@;������dZ쿱[W1   GPS     A   A   B   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�  @�  @���A   A@  A`  A�  A�  A�  A�33A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:�fD;fD;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Dr��Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D��3D�3D�FfD�ff1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @���@���@�p�A Q�A@Q�A`Q�A�(�A�(�A�(�A�\)A�(�A�(�A�(�A�(�B {B{B{B{B {B({B0{B8{B@{BH{BP{BX{B`{Bh{Bp{Bx{B�
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
CCCCCCCCCCC C"C$C&C(C*C,C.C0C2C4C6C8C:C<C>C@CBCDCFCHCJCLCNCPCRCTCVCXCZC\C^C`CbCdCfChCjClCnCpCrCtCvCxCzC|C~C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�\C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D HD �HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HD	HD	�HD
HD
�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD��DHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HD HD �HD!HD!�HD"HD"�HD#HD#�HD$HD$�HD%HD%�HD&HD&�HD'HD'�HD(HD(�HD)HD)�HD*HD*�HD+HD+�HD,HD,�HD-HD-�HD.HD.�HD/HD/�HD0HD0�HD1HD1�HD2HD2�HD3HD3�HD4HD4�HD5HD5�HD6HD6�HD7HD7�HD8HD8�HD9HD9�HD:HD:��D;�D;�HD<HD<�HD=HD=�HD>HD>�HD?HD?�HD@HD@�HDAHDA�HDBHDB�HDCHDC�HDDHDD�HDEHDE�HDFHDF�HDGHDG�HDHHDH�HDIHDI�HDJHDJ�HDKHDK�HDLHDL�HDMHDM�HDNHDN�HDOHDO�HDPHDP�HDQHDQ�HDRHDR�HDSHDS�HDTHDT�HDUHDU�HDVHDV�HDWHDW�HDXHDX�HDYHDY�HDZHDZ�HD[HD[�HD\HD\�HD]HD]�HD^HD^�HD_HD_�HD`HD`�HDaHDa�HDbHDb�HDcHDc�HDdHDd�HDeHDe�HDfHDf�HDgHDg�HDhHDh�HDiHDi�HDjHDj�HDkHDk�HDlHDl�HDmHDm�HDnHDn�HDoHDo�HDpHDp�HDqHDq�HDrHDr�HDr��Ds�HDtHDt�HDuHDu�HDvHDv�HDwHDw�HDxHDx�HDyHDy�HDzHDz�HD{HD{�HD|HD|�HD}HD}�HD~HD~�HDHD�HD� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D�}qD���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D��qD� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D��qD�@�D�D���D� �D�@�DÀ�D���D� �D�@�DĀ�D���D� �D�@�Dŀ�D���D� �D�@�Dƀ�D���D� �D�@�Dǀ�D���D� �D�@�DȀ�D���D� �D�@�Dɀ�D���D� �D�@�Dʀ�D���D� �D�@�Dˀ�D���D� �D�@�D̀�D���D� �D�@�D̀�D���D� �D�@�D΀�D���D� �D�@�Dπ�D���D� �D�@�DЀ�D���D� �D�@�Dр�D���D� �D�@�DҀ�D���D� �D�@�DӀ�D���D� �D�@�DԀ�D���D� �D�@�DՀ�D���D� �D�@�Dր�D���D� �D�@�D׀�D���D� �D�@�D؀�D���D� �D�@�Dـ�D���D� �D�@�Dڀ�D���D� �D�@�Dۀ�D���D� �D�@�D܀�D���D� �D�@�D݀�D���D� �D�@�Dހ�D���D� �D�@�D߀�D���D� �D�@�D���D���D� �D�@�DဤD���D� �D�@�D․D���D� �D�@�D〤D���D� �D�@�D䀤D���D� �D�@�D値D���D� �D�@�D怤D���D� �D�@�D瀤D���D� �D�@�D耤D���D� �D�@�D逤D���D� �D�@�DꀤD���D� �D�@�D뀤D���D� �D�@�D쀤D���D� �D�@�D퀤D���D� �D�@�DD���D� �D�@�DD���D� �D�@�D���D���D� �D�@�D�D���D� �D�@�D�D���D� �D�@�D�D���D� �D�@�D�D���D� �D�@�D���D���D��D�G
D�g
1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��A��A��mA��`A��A��A��A��`A���A���A���A���A���A���A��A���A���A��A�ffA�=qA��yA�v�A�v�A�{A��A���A��wA��FA��-A���A���A��hA��+A�dZA���A�~�A�;dA�+A�"�A��A�%A��A�A�x�A���A� �A�XA�O�A���A��A���A���A�-A�G�A��7A�$�A�n�A�G�A�?}A��A�t�A��A���A���A�
=A���A�A��\A���A�t�A��A�
=A���A�l�A���A�z�A�1'A��A���A���A�VA�A�/A~  A|�A|z�A{�FAy�
Ay"�Axn�AwVAu��As�#As\)As7LAr�yAqt�Ao�An�yAm��Al�`AljAk|�Aj�Ai�Ai�
Ai��Ah�!Ag�FAf�!Ae�Ad�HAaƨA`z�A_dZA^��A\�jA[\)AZjAY|�AYC�AY�AX��AXI�AW��AV�DAU�hAT��ATI�ASAR�jAPAOAN�AN�jAM�PAL�!AK�;AK�wAK��AKt�AKK�AJ��AJ  AI"�AH^5AG7LAE�7AC�AC
=AA�AA�AAx�AAG�AA%A@�DA@�A?��A>��A>�A<�jA<=qA;�A9�A9oA8M�A6��A4�A3�
A3"�A2M�A1��A1`BA133A0��A0jA/A.��A-��A,��A+�FA*bNA)��A(ĜA(z�A(5?A't�A&�A&��A&1'A%"�A$E�A#�A#�A"�RA"bNA"�A!��A ��A �Az�A�
Al�A�jA��A�9A;dAr�A��AI�A�AXA+A  A�A�uAx�A�AQ�AS�AQ�A�`A�!A�A	�A	7LA�A`BA�HA�9A1'A��A�A��AE�A-A��AS�A�RA(�A+A �u@�7L@�dZ@�-@�X@��`@��@�1'@���@��F@��P@�l�@��@��+@�%@�@�J@�Ĝ@@�(�@�$�@�7L@��`@�@�9X@�^5@�  @��;@�t�@��@�@�|�@�X@���@�ƨ@�V@�C�@�^5@պ^@ՙ�@�hs@���@Դ9@�Z@���@�;d@с@�  @���@�{@� �@�
=@ʟ�@��T@�hs@�j@�o@�n�@���@öF@°!@���@�hs@��@�ƨ@�;d@���@���@�l�@���@�{@��u@�^5@��@�K�@���@��@���@�?}@�%@���@�bN@�t�@��H@���@��y@��H@���@�{@�?}@��@�1'@��w@���@�$�@��`@�1@��R@�=q@�5?@�@���@�A�@��w@�
=@�=q@��`@���@�b@�
=@�ȴ@�V@���@�7L@�I�@��
@���@��@�n�@�5?@��T@���@��@�O�@��`@��u@�9X@�(�@�b@���@���@�{@���@��7@�/@��/@���@�Ĝ@�Ĝ@��@�I�@���@��R@���@���@��^@��h@��/@� �@�1'@�(�@���@��w@�S�@�;d@�33@�33@�C�@�"�@��y@��!@�V@�J@��#@���@��@� �@�1@��
@��P@�C�@���@�n�@�@�O�@�7L@�?}@�/@���@�Z@�b@���@�dZ@�\)@�;d@�o@���@�ȴ@�^5@�M�@�E�@�$�@���@�G�@�/@��@�V@�%@�%@��j@�;@�@��@�@~ȴ@~��@~ff@~E�@~{@}�@}�@}��@}@}�@}p�@}O�@}/@|�@{�F@z=q@y��@y�#@x��@w�;@w��@wK�@v��@u��@t��@t��@t��@t��@t��@t��@s�m@st�@sdZ@sdZ@st�@s�m@t9X@t9X@t(�@t1@s�
@s��@sdZ@so@r��@r�@p�9@p��@q&�@p��@p�u@q7L@p�`@o�;@n�y@m�@l(�@k"�@j��@j-@jJ@i��@i�^@i�7@i��@i�^@i��@iG�@h�`@hĜ@h��@g�@gK�@g�@f�@f�R@f��@fff@e��@e`B@e/@e�@eV@d�j@d�D@dz�@dj@d(�@c��@c33@b�@b��@b^5@bJ@a��@a�@a�@a�^@ax�@ax�@ax�@a�7@a%@a�@`��@`bN@`1'@` �@`b@_�@_�w@_\)@^��@^5?@]@]p�@]?}@]V@\�/@\9X@[��@[�m@[�F@[@[t�@["�@Z~�@Zn�@Z^5@Z-@Z�@ZJ@Y��@Y%@Y%@X�`@X�9@X�9@X��@Xb@V�R@V5?@U`B@UV@T�@T�@Tz�@TI�@S�m@S�F@S�@SdZ@R��@R�!@R=q@Q�#@Qx�@QG�@Q%@P��@P�9@Pr�@PbN@PbN@P�@P��@P�9@P�@PbN@O�@Ol�@OK�@O�@N�y@N�R@N��@NE�@N@M�@L��@L��@LI�@K�F@K��@K��@Kt�@KS�@Jn�@I��@I7L@H��@HĜ@H�u@HQ�@HbN@HQ�@G�;@G��@G�w@F�+@E`B@D�j@D��@D�D@Dj@C��@C"�@B�H@B��@B~�@B^5@B-@A�#@A��@A�^@A�^@A��@A��@Ahs@AG�@A7L@A%@@�`@@�9@?�@?�@?��@?|�@?+@?
=@?
=@?
=@>��@>ȴ@>ȴ@>ȴ@>ȴ@>��@>ff@>E�@>{@=�@=�T@=��@=�@=`B@=�@<z�@<I�@<�@;��@;�
@;�
@;�
@;�F@;��@;dZ@:��@9�^@9X@9&�@9�@8��@8�u@7�w@7�P@7�P@7K�@6�y@6�+@6@5��@5�h@5/@4��@4�/@4��@4Z@3�m@3ƨ@3�F@3��@3t�@333@2�@2�H@2�!@2^5@2M�@2-@2J@17L@0�u@0 �@0  @/�;@/K�@.�y@.��@.��@.�+@.5?@-�T@-��@-@-�-@-�@-?}@-�@,�@,�/@,��@,z�@,�@,1@+�
@+t�@+"�@*��@*~�@*-@)��@)��@)%@(r�@(A�@( �@(  @'�;@'�;@'�;@'�;@'��@'��@'�w@'�w@'|�@';d@&��@&E�@%�@%��@%�h@%`B@%V@$��@$��@$��@$�@$1@#�
@#�F@#��@#t�@#C�@#o@"�!@"n�@"M�@"-@!��@!7L@!%@ �`@ ��@ Ĝ@ bN@   @;d@��@��@��@��@�+@ff@��@�h@p�@`B@?}@�@�j@Z@��@��@��@��@��@��@��@��@dZ@33@�@��@��@�!@�\@J@��@x�@hs@X@7L@7L@�@�`@Q�@�;@�P@K�@
=@��@��@��@�@��@V@E�@E�@$�@@�@�T@@�h@/@z�@�m@ƨ@�@dZ@S�@C�@"�@��@�@��@G�@7L@�@%@Ĝ@�u@bN@ �@�;@��@�;@��@��@�w@�@l�@
=@�y@�@��@��@v�@v�@v�@5?@@�@��@`B@?}@�@V@��@�@�/@�@z�@j@j@I�@�@��@�m@�F@t�@dZ@C�@
�@
�!@
n�@
-@
J@	��@	�@	��@	��@	&�@��@�`@Ĝ@Ĝ@Ĝ@Ĝ@Ĝ@�9@�u@Q�@�;@|�@
=@��@�y@�y@�y@�y@�y@�@�R@v�@ff@5?@5?@$�@$�@5?@5?@$�@@�T@�-@p�@`B@O�@?}@/@�@��@��@��@�@��@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��A��A��mA��`A��A��A��A��`A���A���A���A���A���A���A��A���A���A��A�ffA�=qA��yA�v�A�v�A�{A��A���A��wA��FA��-A���A���A��hA��+A�dZA���A�~�A�;dA�+A�"�A��A�%A��A�A�x�A���A� �A�XA�O�A���A��A���A���A�-A�G�A��7A�$�A�n�A�G�A�?}A��A�t�A��A���A���A�
=A���A�A��\A���A�t�A��A�
=A���A�l�A���A�z�A�1'A��A���A���A�VA�A�/A~  A|�A|z�A{�FAy�
Ay"�Axn�AwVAu��As�#As\)As7LAr�yAqt�Ao�An�yAm��Al�`AljAk|�Aj�Ai�Ai�
Ai��Ah�!Ag�FAf�!Ae�Ad�HAaƨA`z�A_dZA^��A\�jA[\)AZjAY|�AYC�AY�AX��AXI�AW��AV�DAU�hAT��ATI�ASAR�jAPAOAN�AN�jAM�PAL�!AK�;AK�wAK��AKt�AKK�AJ��AJ  AI"�AH^5AG7LAE�7AC�AC
=AA�AA�AAx�AAG�AA%A@�DA@�A?��A>��A>�A<�jA<=qA;�A9�A9oA8M�A6��A4�A3�
A3"�A2M�A1��A1`BA133A0��A0jA/A.��A-��A,��A+�FA*bNA)��A(ĜA(z�A(5?A't�A&�A&��A&1'A%"�A$E�A#�A#�A"�RA"bNA"�A!��A ��A �Az�A�
Al�A�jA��A�9A;dAr�A��AI�A�AXA+A  A�A�uAx�A�AQ�AS�AQ�A�`A�!A�A	�A	7LA�A`BA�HA�9A1'A��A�A��AE�A-A��AS�A�RA(�A+A �u@�7L@�dZ@�-@�X@��`@��@�1'@���@��F@��P@�l�@��@��+@�%@�@�J@�Ĝ@@�(�@�$�@�7L@��`@�@�9X@�^5@�  @��;@�t�@��@�@�|�@�X@���@�ƨ@�V@�C�@�^5@պ^@ՙ�@�hs@���@Դ9@�Z@���@�;d@с@�  @���@�{@� �@�
=@ʟ�@��T@�hs@�j@�o@�n�@���@öF@°!@���@�hs@��@�ƨ@�;d@���@���@�l�@���@�{@��u@�^5@��@�K�@���@��@���@�?}@�%@���@�bN@�t�@��H@���@��y@��H@���@�{@�?}@��@�1'@��w@���@�$�@��`@�1@��R@�=q@�5?@�@���@�A�@��w@�
=@�=q@��`@���@�b@�
=@�ȴ@�V@���@�7L@�I�@��
@���@��@�n�@�5?@��T@���@��@�O�@��`@��u@�9X@�(�@�b@���@���@�{@���@��7@�/@��/@���@�Ĝ@�Ĝ@��@�I�@���@��R@���@���@��^@��h@��/@� �@�1'@�(�@���@��w@�S�@�;d@�33@�33@�C�@�"�@��y@��!@�V@�J@��#@���@��@� �@�1@��
@��P@�C�@���@�n�@�@�O�@�7L@�?}@�/@���@�Z@�b@���@�dZ@�\)@�;d@�o@���@�ȴ@�^5@�M�@�E�@�$�@���@�G�@�/@��@�V@�%@�%@��j@�;@�@��@�@~ȴ@~��@~ff@~E�@~{@}�@}�@}��@}@}�@}p�@}O�@}/@|�@{�F@z=q@y��@y�#@x��@w�;@w��@wK�@v��@u��@t��@t��@t��@t��@t��@t��@s�m@st�@sdZ@sdZ@st�@s�m@t9X@t9X@t(�@t1@s�
@s��@sdZ@so@r��@r�@p�9@p��@q&�@p��@p�u@q7L@p�`@o�;@n�y@m�@l(�@k"�@j��@j-@jJ@i��@i�^@i�7@i��@i�^@i��@iG�@h�`@hĜ@h��@g�@gK�@g�@f�@f�R@f��@fff@e��@e`B@e/@e�@eV@d�j@d�D@dz�@dj@d(�@c��@c33@b�@b��@b^5@bJ@a��@a�@a�@a�^@ax�@ax�@ax�@a�7@a%@a�@`��@`bN@`1'@` �@`b@_�@_�w@_\)@^��@^5?@]@]p�@]?}@]V@\�/@\9X@[��@[�m@[�F@[@[t�@["�@Z~�@Zn�@Z^5@Z-@Z�@ZJ@Y��@Y%@Y%@X�`@X�9@X�9@X��@Xb@V�R@V5?@U`B@UV@T�@T�@Tz�@TI�@S�m@S�F@S�@SdZ@R��@R�!@R=q@Q�#@Qx�@QG�@Q%@P��@P�9@Pr�@PbN@PbN@P�@P��@P�9@P�@PbN@O�@Ol�@OK�@O�@N�y@N�R@N��@NE�@N@M�@L��@L��@LI�@K�F@K��@K��@Kt�@KS�@Jn�@I��@I7L@H��@HĜ@H�u@HQ�@HbN@HQ�@G�;@G��@G�w@F�+@E`B@D�j@D��@D�D@Dj@C��@C"�@B�H@B��@B~�@B^5@B-@A�#@A��@A�^@A�^@A��@A��@Ahs@AG�@A7L@A%@@�`@@�9@?�@?�@?��@?|�@?+@?
=@?
=@?
=@>��@>ȴ@>ȴ@>ȴ@>ȴ@>��@>ff@>E�@>{@=�@=�T@=��@=�@=`B@=�@<z�@<I�@<�@;��@;�
@;�
@;�
@;�F@;��@;dZ@:��@9�^@9X@9&�@9�@8��@8�u@7�w@7�P@7�P@7K�@6�y@6�+@6@5��@5�h@5/@4��@4�/@4��@4Z@3�m@3ƨ@3�F@3��@3t�@333@2�@2�H@2�!@2^5@2M�@2-@2J@17L@0�u@0 �@0  @/�;@/K�@.�y@.��@.��@.�+@.5?@-�T@-��@-@-�-@-�@-?}@-�@,�@,�/@,��@,z�@,�@,1@+�
@+t�@+"�@*��@*~�@*-@)��@)��@)%@(r�@(A�@( �@(  @'�;@'�;@'�;@'�;@'��@'��@'�w@'�w@'|�@';d@&��@&E�@%�@%��@%�h@%`B@%V@$��@$��@$��@$�@$1@#�
@#�F@#��@#t�@#C�@#o@"�!@"n�@"M�@"-@!��@!7L@!%@ �`@ ��@ Ĝ@ bN@   @;d@��@��@��@��@�+@ff@��@�h@p�@`B@?}@�@�j@Z@��@��@��@��@��@��@��@��@dZ@33@�@��@��@�!@�\@J@��@x�@hs@X@7L@7L@�@�`@Q�@�;@�P@K�@
=@��@��@��@�@��@V@E�@E�@$�@@�@�T@@�h@/@z�@�m@ƨ@�@dZ@S�@C�@"�@��@�@��@G�@7L@�@%@Ĝ@�u@bN@ �@�;@��@�;@��@��@�w@�@l�@
=@�y@�@��@��@v�@v�@v�@5?@@�@��@`B@?}@�@V@��@�@�/@�@z�@j@j@I�@�@��@�m@�F@t�@dZ@C�@
�@
�!@
n�@
-@
J@	��@	�@	��@	��@	&�@��@�`@Ĝ@Ĝ@Ĝ@Ĝ@Ĝ@�9@�u@Q�@�;@|�@
=@��@�y@�y@�y@�y@�y@�@�R@v�@ff@5?@5?@$�@$�@5?@5?@$�@@�T@�-@p�@`B@O�@?}@/@�@��@��@��@�@��@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B%�B(�B(�B&�B&�B'�B'�B'�B&�B&�B#�B�BuB\BDB
=B1B%BB��B�B��B�BG�B��B�3B��B��B�DB�PBz�B_;Bo�Bt�B}�Bn�BW
BO�BK�BD�B8RB0!B/B&�B�B{BB
��B
�B
�mB
��B
B
B
��B
�LB
�B
�B
��B
��B
�VB
�B
u�B
z�B
s�B
cTB
`BB
[#B
P�B
I�B
?}B
C�B
A�B
<jB
0!B
$�B
!�B
�B
�B
�B
bB
1B
+B
1B
B	��B	�B	�B	�fB	�)B	ǮB	B	�qB	�XB	�B	��B	��B	��B	��B	��B	��B	��B	��B	�\B	�7B	�+B	�B	� B	v�B	iyB	e`B	k�B	hsB	aHB	\)B	XB	ZB	YB	XB	VB	Q�B	J�B	D�B	@�B	8RB	0!B	(�B	)�B	"�B	%�B	#�B	!�B	!�B	�B	�B	�B	VB	JB	B	B	B��B��B�B�B�5B�;B�;B�#B�B�
B�
B��B��B��BǮB�}B�^B�LB�-B�3B�-B�9B�3B�B�B�B��B��B��B��B��B��B��B��B��B�{B�oB�7B�+B�1B�B�1B�%B|�B� B{�B}�Bz�By�Bx�Br�BiyBjBk�Bn�Bn�BiyBhsBbNBe`BbNBZBYBYBW
BXBYBW
BT�BS�BS�BS�BS�BR�BO�BM�BJ�BG�BG�B@�BB�BB�BC�BD�BC�BC�BC�BC�BB�BA�BA�B?}B=qB=qB:^B:^B8RB:^B;dB>wB?}B>wB<jB8RB5?B;dB9XB5?B6FB8RB5?B:^B:^B49B7LB:^B;dB=qB<jB;dB:^B:^B9XB8RB49B49B7LB9XB7LB:^B?}B>wB>wB=qB;dB>wB<jB8RB>wBA�B@�B=qB7LBD�BC�B?}BB�BF�BE�BC�BD�BE�BP�BR�BT�BXBYBZBZBZBYB\)BaHBffBk�Bn�Bo�Bp�Bs�Bt�Bu�Bt�Bw�Bu�Bz�B{�B�B�B�B�B�B�%B�%B�7B�DB�uB�uB�{B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�3B�XB��BÖBBŢBŢBɺB��B��B�B�B�B�#B�)B�#B�)B�5B�HB�sB�B�B�B�B��B��B��B��B��B��B��B��B	  B	B	B	B	B	B	B	B	B	B	
=B		7B		7B	
=B		7B	1B		7B	DB	hB	oB	oB	oB	uB	�B	�B	�B	�B	�B	�B	�B	�B	�B	"�B	#�B	#�B	#�B	%�B	-B	/B	0!B	1'B	1'B	1'B	33B	8RB	9XB	<jB	>wB	@�B	@�B	A�B	B�B	C�B	D�B	D�B	D�B	D�B	E�B	E�B	E�B	F�B	E�B	K�B	R�B	R�B	Q�B	S�B	XB	XB	ZB	[#B	]/B	aHB	bNB	bNB	bNB	aHB	bNB	ffB	iyB	jB	jB	l�B	p�B	t�B	x�B	x�B	y�B	{�B	}�B	}�B	~�B	� B	� B	�B	�B	�B	�%B	�1B	�DB	�\B	�hB	�\B	�PB	�JB	�VB	�bB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�'B	�-B	�-B	�-B	�3B	�?B	�?B	�?B	�?B	�LB	�RB	�RB	�RB	�XB	�jB	�wB	�}B	��B	B	ÖB	ÖB	ĜB	ŢB	ƨB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�#B	�)B	�)B	�5B	�;B	�BB	�HB	�NB	�NB	�TB	�TB	�`B	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
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
%B
+B
1B
	7B
	7B
	7B
	7B

=B
JB
JB
PB
PB
PB
PB
\B
\B
\B
\B
\B
bB
bB
bB
bB
hB
hB
bB
oB
uB
uB
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
"�B
"�B
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
%�B
%�B
'�B
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
(�B
(�B
)�B
,B
-B
-B
,B
.B
/B
0!B
/B
/B
0!B
1'B
1'B
1'B
1'B
1'B
2-B
2-B
33B
33B
2-B
33B
49B
49B
49B
5?B
5?B
6FB
7LB
7LB
7LB
8RB
8RB
9XB
9XB
9XB
9XB
:^B
:^B
:^B
:^B
:^B
9XB
:^B
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
=qB
=qB
=qB
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
@�B
A�B
B�B
B�B
B�B
B�B
B�B
B�B
C�B
D�B
E�B
E�B
F�B
E�B
E�B
E�B
F�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
H�B
I�B
I�B
J�B
J�B
J�B
I�B
I�B
J�B
J�B
J�B
K�B
K�B
K�B
J�B
K�B
L�B
L�B
M�B
M�B
N�B
N�B
O�B
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
P�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
R�B
S�B
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
YB
YB
YB
ZB
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
[#B
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
`BB
aHB
`BB
`BB
aHB
aHB
aHB
bNB
bNB
bNB
bNB
bNB
bNB
dZB
dZB
dZB
e`B
e`B
e`B
e`B
dZB
dZB
dZB
dZB
dZB
e`B
gmB
gmB
hsB
hsB
hsB
hsB
hsB
hsB
hsB
hsB
hsB
iyB
iyB
iyB
jB
jB
jB
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
k�B
k�B
k�B
k�B
l�B
l�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�BB 'B;B�B�BxB&�B)DB)*B'B'B(
B(
B(
B'B'B$ZB �B{B�BxB
XBfBYBoB��B�]B��B��BNB�B��B��B�bB�pB�BB�Bd�BraBv`B~�Bq�B\BR�BM�BFYB:^B2-B0;B(sB!-B�B�B
��B
��B
��B
�
B
żB
�{B
�uB
��B
��B
��B
��B
�#B
�B
��B
wfB
{JB
t�B
e`B
aHB
\CB
R�B
KxB
AoB
DB
A�B
=<B
1�B
&�B
# B
B
eB
SB
�B
	RB
�B
fB
�B	�"B	�B	��B	�B	�B	��B	�3B	��B	��B	�IB	�sB	��B	��B	� B	� B	�'B	�xB	��B	��B	�rB	�B	��B	�B	x�B	l=B	f�B	k�B	iB	b�B	]IB	X�B	ZQB	YeB	X_B	VmB	R�B	LB	E�B	A�B	:B	2GB	*�B	+6B	$B	&2B	$&B	"4B	"4B	dB	KB	?B	�B	6B	�B	B	�B�^B��B��B�WB��B��B�BB�)B��B�sB�sBԕBЗB��B��B� B��B��B��B�9B�3B��B��B�B��B��B��B�LB��B��B��B�bB�BB�5B��B��B��B�B��B��B��B�fB��B~�B�UB}�B~�B{�BzxBy�Bt�BlBlqBl�BoiBo�BkBi�Bc�Be�Bc�B\�BZ7BZ7BXyBX�BY�BW�BU�BT�BT�BTaBTFBSuBP�BN�BK�BIBH�BB�BC�BCaBDBEBC�BC�BC�BC�BB�BA�BA�B@4B>�B>wB;�B;B9�B<B<�B>�B?�B>�B="B9�B6�B;B9�B6FB7LB9XB6�B:�B;B6B8lB:�B;�B=�B<�B;�B:�B:�B9�B9$B5tB5?B8B:*B8�B;B?�B?B>�B>BB<PB?B="B9�B?BB'BA;B>�B9�BEBD3B@�BC{BGEBFtBD�BF?BG+BQhBSuBU�BX_BYeBZkBZkBZ�BY�B\�BaHBf�Bk�Bn�Bp!Bq'Bt9Bu%BvFBu�BxlBv�B{�B|�B�UB�MB�uB��B��B��B��B��B�B��B��B�2B��B�B�B�;B�NB�LB�8B�fB�RB�0B�QB�=B�)B�CB�wB��B��B��B��B��B�%B�%B�	B�B�:B�9B�1B�B�=B�]BیB��B��B��B�B�B��B�!B�3B��B��B�B�B�0B��B�B��B	 B	;B	 B	AB	aB	aB	MB	mB	�B	�B	
XB		lB		lB	
�B		�B	�B		�B	�B	�B	oB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	B	"�B	#�B	$B	$&B	&LB	-)B	/5B	0;B	1AB	1AB	1[B	3�B	8RB	9rB	<�B	>�B	@�B	@�B	A�B	B�B	C�B	D�B	D�B	D�B	D�B	E�B	E�B	E�B	F�B	F%B	LJB	SB	S&B	RTB	TFB	X+B	X+B	ZkB	[qB	]~B	abB	bNB	bhB	bhB	a|B	b�B	f�B	iyB	jB	jB	lWB	p�B	t�B	x�B	x�B	y�B	{�B	}�B	~(B	.B	�OB	�iB	�B	�B	�SB	�%B	�B	�xB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�$B	�*B	�)B	�5B	�B	�AB	�GB	�aB	�aB	�hB	�?B	�ZB	�ZB	�ZB	�LB	�lB	�lB	�lB	��B	��B	��B	��B	��B	ªB	ÖB	ðB	ĶB	żB	��B	ȴB	ɺB	��B	�B	��B	� B	� B	�B	��B	�B	�B	�B	�EB	�eB	�WB	�]B	�]B	�OB	�;B	�\B	�bB	�hB	�hB	�nB	�B	�`B	�B	�B	�B	�B	��B	��B	��B	��B	��B	�B	��B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	��B	��B	��B	��B	��B	�B	�B	�"B	�(B	��B	�B	��B
 B
 B
 B
 4B
 OB
'B
'B
AB
GB
3B
B
9B
9B
gB
gB
9B
%B
?B
?B
EB
+B
EB
EB
+B
_B
�B
zB
fB
	RB
	RB
	RB
	�B

rB
dB
dB
jB
jB
jB
jB
\B
\B
\B
vB
vB
}B
}B
}B
}B
�B
�B
�B
oB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
�B
�B
�B
�B
 B
"�B
"�B
"�B
#B
$B
$B
$�B
$�B
$�B
%�B
%�B
%�B
&B
%�B
(
B
'�B
'�B
(
B
(
B
)B
)B
(�B
)B
)�B
*B
(�B
)DB
*0B
,"B
-)B
-)B
,=B
./B
/B
0!B
/5B
/5B
0;B
1B
1'B
1AB
1AB
1AB
2GB
2GB
33B
33B
2GB
33B
4TB
4TB
4nB
5ZB
5ZB
6`B
7LB
7fB
7�B
8�B
8�B
9rB
9rB
9XB
9rB
:^B
:^B
:^B
:^B
:^B
9rB
:DB
9XB
9rB
9�B
:xB
:xB
;B
<jB
<�B
<�B
=qB
=�B
=�B
=�B
?}B
?�B
?�B
?�B
?}B
?�B
?�B
?�B
@�B
@�B
A�B
@�B
A�B
B�B
B�B
B�B
B�B
B�B
B�B
C�B
D�B
E�B
E�B
F�B
E�B
E�B
E�B
F�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
H�B
I�B
I�B
J�B
J�B
J�B
I�B
I�B
J�B
J�B
J�B
K�B
K�B
K�B
J�B
K�B
L�B
L�B
M�B
M�B
N�B
N�B
O�B
OB
PB
O�B
O�B
O�B
O�B
O�B
O�B
O�B
PB
Q B
P�B
P�B
Q B
Q B
P�B
Q B
Q B
Q B
QB
QB
QB
SB
TB
T�B
T�B
UB
T�B
U2B
U2B
V9B
W$B
W
B
X+B
X+B
X+B
XB
XB
Y1B
Y1B
ZB
ZB
ZB
ZB
ZB
Y1B
Y1B
Y1B
Z7B
[#B
[=B
[#B
[=B
[#B
[#B
[=B
[=B
\)B
\CB
[=B
\CB
]IB
]B
]/B
]/B
]IB
]IB
^OB
^5B
^5B
^OB
^5B
_VB
_VB
_VB
`\B
aHB
`BB
`\B
abB
abB
aHB
bNB
bNB
bhB
bNB
bhB
b�B
d�B
d@B
dtB
e`B
e`B
e`B
e`B
dtB
dtB
dtB
d�B
dtB
ezB
gmB
gmB
hsB
hsB
hsB
hsB
hsB
hsB
h�B
hXB
h�B
iyB
iyB
iyB
jB
jB
jeB
i�B
i�B
i�B
i�B
iyB
i_B
iyB
j�B
j�B
j�B
k�B
k�B
k�B
k�B
l�B
l�1111111111111111111111111111111111111111111311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<?�[<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=-0.02(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201712130031382017121300313820171213003138201806221234372018062212343720180622123437201804050430472018040504304720180405043047  JA  ARFMdecpA19c                                                                20171209093512  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20171209003523  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20171209003525  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20171209003525  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20171209003526  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20171209003526  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20171209003526  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20171209003526  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20171209003526  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20171209003526                      G�O�G�O�G�O�                JA  ARUP                                                                        20171209005522                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20171209153515  CV  JULD            G�O�G�O�F�۰                JM  ARSQJMQC2.0                                                                 20171212000000  CF  PSAL_ADJUSTED_QCB�  B�  G�O�                JM  ARCAJMQC2.0                                                                 20171212153138  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20171212153138  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404193047  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622033437  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115121517                      G�O�G�O�G�O�                