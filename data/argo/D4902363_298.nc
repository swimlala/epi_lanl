CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-11-07T00:35:16Z creation;2018-11-07T00:35:20Z conversion to V3.1;2019-12-19T07:28:39Z update;     
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
_FillValue                    �(Argo profile    3.1 1.2 19500101000000  20181107003516  20200115131516  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL              *A   JA  I2_0576_298                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @؎�+�d�1   @؎� @8���q��d3/�V��1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�33@�33A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B��B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DPy�DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׃3D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�<�D� D�� D�  D�@ D�3D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D���D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�y�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��
@��
A Q�A Q�A@Q�A`Q�A�(�A�(�A�(�A�(�A�(�A�(�A�(�A�(�B {B{B{B{B {B({B0{B8{B@{BH{BP{BX{B`{Bh{Bp{Bx{B�B�
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
=B��
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
=C CCCCC
CCCCCCCCCCC C"C$C&C(C*C,C.C0C2C4C6C8C:C<C>C@CBCDCFCHCJCLCNCPCRCTCVCXCZC\C^C`CbCdCfChCjClCnCpCrCtCvCxCzC|C~C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C���C��C�\C�\C�\C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�\C��C��C��C��C��C��C��C�\C�\C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D HD �HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HD	HD	�HD
HD
�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HD HD �HD!HD!�HD"HD"�HD#HD#�HD$HD$�HD%HD%�HD&HD&�HD'HD'�HD(HD(�HD)HD)�HD*HD*�HD+HD+�HD,HD,�HD-HD-�HD.HD.�HD/HD/�HD0HD0�HD1HD1�HD2HD2�HD3HD3�HD4HD4�HD5HD5�HD6HD6�HD7HD7�HD8HD8�HD9HD9�HD:HD:�HD;HD;�HD<HD<�HD=HD=�HD>HD>�HD?HD?�HD@HD@�HDAHDA�HDBHDB�HDCHDC�HDDHDD�HDEHDE�HDFHDF�HDGHDG�HDHHDH�HDIHDI�HDJHDJ�HDKHDK�HDLHDL�HDMHDM�HDNHDN�HDOHDO�HDPHDPz�DQHDQ�HDRHDR�HDSHDS�HDTHDT�HDUHDU�HDVHDV�HDWHDW�HDXHDX�HDYHDY�HDZHDZ�HD[HD[�HD\HD\�HD]HD]�HD^HD^�HD_HD_�HD`HD`�HDaHDa�HDbHDb�HDcHDc�HDdHDd�HDeHDe�HDfHDf�HDgHDg�HDhHDh�HDiHDi�HDjHDj�HDkHDk�HDlHDl�HDmHDm�HDnHDn�HDoHDo�HDpHDp�HDqHDq�HDrHDr�HDsHDs�HDtHDt�HDuHDu�HDvHDv�HDwHDw�HDxHDx�HDyHDy�HDzHDz�HD{HD{�HD|HD|�HD}HD}�HD~HD~�HDHD�HD� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D��qD�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D�D���D� �D�@�DÀ�D���D� �D�@�DĀ�D���D� �D�@�Dŀ�D���D� �D�@�Dƀ�D���D� �D�@�Dǀ�D���D� �D�@�DȀ�D���D� �D�@�Dɀ�D���D� �D�@�Dʀ�D���D� �D�@�Dˀ�D���D� �D�@�D̀�D���D� �D�@�D̀�D���D� �D�@�D΀�D���D� �D�@�Dπ�D���D� �D�@�DЀ�D���D� �D�@�Dр�D���D� �D�@�DҀ�D���D� �D�@�DӀ�D���D� �D�@�DԀ�D���D� �D�@�DՀ�D���D� �D�@�Dր�D���D� �D�@�D׃�D���D� �D�@�D؀�D���D� �D�@�Dـ�D���D� �D�@�Dڀ�D���D� �D�@�Dۀ�D���D� �D�@�D܀�D���D� �D�@�D݀�D���D� �D�@�Dހ�D���D� �D�@�D߀�D���D� �D�@�D���D���D� �D�@�DဤD���D� �D�@�D․D���D� �D�@�D〤D���D� �D�@�D䀤D���D� �D�@�D値D���D� �D�@�D怤D���D� �D�=qD瀤D���D� �D�@�D��D���D� �D�@�D逤D���D� �D�@�DꀤD���D� �D�@�D뀤D���D� �D�@�D쀤D���D� �D�@�D퀤D���D��qD�@�DD���D� �D�@�DD���D� �D�@�D���D���D� �D�@�D�D���D� �D�@�D�D���D� �D�@�D�D���D� �D�@�D�D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�C�D�z=111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�E�A�+A��A�1A�%A�VA�bA�VA�VA�JA�
=A�
=A�
=A�
=A�JA�VA�oA��A� �A� �A�$�A�(�A�-A�9XA�O�A�G�A��A΍PA���A�bA��A��A��;A�I�A�1A�E�A��!A���A��;A��hA�`BA���A�p�A�1'A�=qA���A��wA�z�A�`BA���A��A�r�A�S�A�z�A��A�&�A�A���A��\A�`BA�C�A�%A��/A��hA��9A�dZA��A��A���A�ȴA���A�ffA�XA���A��A���A�XA�bA��;A�hsA�hsA���A��HA�r�A�^5A��uA��A��wA�hsA���A��uA�"�A���A�I�A�~�A�hsA�+A�z�A��FA�VAx�A~bA}x�A}�A|5?Az��Ay�mAw;dAu;dAs?}Ar{ApbNAn��Am�TAln�Aj�yAj-Ai%Ah��AfffAd�RAd�AchsAc%Ab^5AbAax�A`�jA_t�A^�uA\~�AZ�!AZJAX��AWp�AW+AVĜAVM�AU+AS7LARM�AOx�AN��AN9XAM�#AMO�AKl�AI+AH�uAHI�AG�AG\)AF�AE��AD�AA�A@VA>�RA<ZA;A:-A9�wA9A8�A8��A8z�A8bA7��A7VA6=qA5C�A4(�A2��A1��A0�\A0  A/`BA.��A-7LA,Q�A,5?A,9XA,1'A,�A,$�A*~�A(ȴA'��A'�^A'|�A'7LA&�HA&��A& �A%��A$�A!��A
=A�A?}A��AoA�mA�wA��A��A�AffA-A�A�A��AdZA��AJAp�At�A?}A�A��A��A��AbA��Al�A?}A
=A1'A
��A	��A	�A  A�A=qA�
A��A(�A��A�A��A|�A �j@���@�(�@� �@�{@���@��/@�z�@�A�@�  @�ƨ@�\)@���@�@�^5@��@�u@�5?@���@�u@��@���@�^5@��@睲@���@��@���@�j@�!@�=q@�hs@���@��@�`B@܃@۝�@��H@ڗ�@��@�1'@�|�@��T@ԛ�@ӶF@җ�@�t�@�x�@�%@̛�@�r�@��
@�~�@�hs@ư!@ēu@��y@��@�b@�ff@��u@��P@�K�@���@���@�1'@��T@�I�@���@���@�dZ@�C�@��@��@�v�@��#@��^@���@�`B@�Ĝ@���@�@��#@���@��h@�x�@�?}@���@��w@���@�n�@���@�p�@�7L@���@��@�I�@��@��;@���@�S�@�~�@�J@��@���@��^@�p�@�&�@��@���@��@��\@�~�@�v�@�V@��@�x�@��@��j@�b@���@�dZ@���@���@�x�@�z�@��@��@�v�@��-@��@�j@���@�C�@���@�ff@�M�@�$�@��7@��`@��@�9X@�+@�5?@���@��h@��@�O�@�j@��m@���@�\)@�;d@�"�@�ȴ@�v�@��@��h@���@��9@��D@�z�@�I�@�1@��
@��@���@���@�p�@�O�@���@�b@��@��@���@�(�@��F@��\@��-@�%@�Z@�Z@�A�@�b@�(�@��j@���@�(�@��w@��@��@��+@�@���@��#@��#@��#@��T@��-@�p�@�`B@��@�z�@���@��9@��@�1@~��@}�@|�/@|j@|I�@|(�@{�m@{dZ@z��@z��@y��@y��@y�^@y��@x��@xbN@w��@w;d@v�R@vv�@vff@vff@vff@v�+@v��@v��@v��@vE�@u�@u��@u@u��@u�-@uV@t�j@t�@t�@t��@t��@t��@t�D@t9X@s�@s@r��@r�!@rM�@r-@q�#@q�@q%@p�`@p1'@o+@nv�@m�T@m@m��@m�@m`B@m/@l�@kƨ@kdZ@ko@j�H@j�!@j^5@j�@i&�@h�@hA�@g�w@g|�@g\)@g;d@fE�@eO�@d��@c��@cdZ@cC�@c33@c33@c33@c33@c33@c"�@c"�@c@b^5@aX@` �@_��@_+@^�+@^@]�@\��@\(�@[dZ@[33@["�@["�@[@Z�@Z�H@Z��@Z��@Zn�@Y%@XQ�@Xb@Xb@X  @W��@W�P@V��@V$�@U�-@U�@U�@Up�@Up�@T��@S��@S�@SS�@So@R�@R��@R-@Q�@Q%@P��@P��@P��@P�`@PĜ@P�9@P��@Pr�@P �@O�@O��@O��@O+@N��@N�+@N$�@M�-@M`B@L�/@L�D@LZ@L1@Kƨ@K��@K��@K��@KS�@Ko@J-@IX@H �@F��@E�@E��@E@E��@E?}@E�@DZ@D1@B��@B^5@BM�@A��@Ax�@AG�@A�@@��@@��@@r�@@r�@@Q�@?�w@>��@=�@=�-@=V@<Z@<1@;dZ@:��@:�\@:~�@:M�@9�7@8r�@8bN@8bN@8bN@8bN@8bN@8bN@8bN@8bN@8Q�@8bN@8bN@8bN@8r�@8bN@8bN@8Q�@8  @7�@7�w@7K�@6��@6ȴ@6��@6E�@5?}@5�@5?}@4�@4�/@4��@5/@5V@49X@3o@2�H@2�@3@3@2�@2�H@2��@2n�@1�@1��@1�^@1��@1�@1�@1�@1�#@1�@1�@1�@1��@1%@0bN@0Q�@0Q�@/�@/�;@/�@/�@/�P@/|�@/\)@/\)@/K�@/+@/�@.��@.��@.�y@.�y@.�y@.�y@.�y@.�y@.ȴ@.��@.ff@.5?@.$�@.{@-��@-V@,9X@+S�@*�H@*^5@*J@)��@)&�@(��@(�u@(��@(��@(�`@(�@(bN@(r�@(Q�@( �@(b@'�w@&��@&V@&@%�T@%�h@%/@$�j@$z�@$I�@$9X@$(�@#�m@#��@#�@#33@"�@"��@"�\@"M�@!�#@!��@!�7@!�7@!x�@!hs@!X@!G�@!7L@!�@!%@ �9@ �u@ bN@ A�@ 1'@ b@�@�w@|�@@�/@�D@��@�D@�D@z�@j@Z@Z@I�@9X@(�@�@�@�@1@��@�m@�m@�
@�F@t�@33@"�@�@��@�!@�!@��@�\@~�@^5@M�@-@�@�^@hs@hs@G�@G�@7L@�@��@�9@Q�@Q�@Q�@�@|�@
=@�+@{@�T@��@@�h@p�@�@��@�j@j@(�@�m@ƨ@��@33@33@@��@��@��@��@��@��@��@��@~�@n�@n�@n�@M�@�@�^@�^@��@�7@x�@�@��@��@�@�@�@�@Q�@A�@b@�@�@�;@��@��@��@;d@
=@��@��@�y@��@�y@E�@��@�h@�@`B@?}@�@��@�@�j@1@t�@"�@@
�H@
^5@
=q@
�@	��@	�#@	��@	x�@	hs@	hs@	X@	G�@	G�@	G�@	7L@	&�@Ĝ@�@1'@�;@|�@+@�@
=@�@ȴ@��@v�@ff@ff@V@5?@$�@{@{@@@@�@�T@@��@p�@`B@?}@�@�j@�@j@1@1@1@�m@��@�@t�@33@o@o@@@@�@�@��@~�@^5@^5@^5@^5@M�@M�@=q@-@�@hs@x�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�E�A�+A��A�1A�%A�VA�bA�VA�VA�JA�
=A�
=A�
=A�
=A�JA�VA�oA��A� �A� �A�$�A�(�A�-A�9XA�O�A�G�A��A΍PA���A�bA��A��A��;A�I�A�1A�E�A��!A���A��;A��hA�`BA���A�p�A�1'A�=qA���A��wA�z�A�`BA���A��A�r�A�S�A�z�A��A�&�A�A���A��\A�`BA�C�A�%A��/A��hA��9A�dZA��A��A���A�ȴA���A�ffA�XA���A��A���A�XA�bA��;A�hsA�hsA���A��HA�r�A�^5A��uA��A��wA�hsA���A��uA�"�A���A�I�A�~�A�hsA�+A�z�A��FA�VAx�A~bA}x�A}�A|5?Az��Ay�mAw;dAu;dAs?}Ar{ApbNAn��Am�TAln�Aj�yAj-Ai%Ah��AfffAd�RAd�AchsAc%Ab^5AbAax�A`�jA_t�A^�uA\~�AZ�!AZJAX��AWp�AW+AVĜAVM�AU+AS7LARM�AOx�AN��AN9XAM�#AMO�AKl�AI+AH�uAHI�AG�AG\)AF�AE��AD�AA�A@VA>�RA<ZA;A:-A9�wA9A8�A8��A8z�A8bA7��A7VA6=qA5C�A4(�A2��A1��A0�\A0  A/`BA.��A-7LA,Q�A,5?A,9XA,1'A,�A,$�A*~�A(ȴA'��A'�^A'|�A'7LA&�HA&��A& �A%��A$�A!��A
=A�A?}A��AoA�mA�wA��A��A�AffA-A�A�A��AdZA��AJAp�At�A?}A�A��A��A��AbA��Al�A?}A
=A1'A
��A	��A	�A  A�A=qA�
A��A(�A��A�A��A|�A �j@���@�(�@� �@�{@���@��/@�z�@�A�@�  @�ƨ@�\)@���@�@�^5@��@�u@�5?@���@�u@��@���@�^5@��@睲@���@��@���@�j@�!@�=q@�hs@���@��@�`B@܃@۝�@��H@ڗ�@��@�1'@�|�@��T@ԛ�@ӶF@җ�@�t�@�x�@�%@̛�@�r�@��
@�~�@�hs@ư!@ēu@��y@��@�b@�ff@��u@��P@�K�@���@���@�1'@��T@�I�@���@���@�dZ@�C�@��@��@�v�@��#@��^@���@�`B@�Ĝ@���@�@��#@���@��h@�x�@�?}@���@��w@���@�n�@���@�p�@�7L@���@��@�I�@��@��;@���@�S�@�~�@�J@��@���@��^@�p�@�&�@��@���@��@��\@�~�@�v�@�V@��@�x�@��@��j@�b@���@�dZ@���@���@�x�@�z�@��@��@�v�@��-@��@�j@���@�C�@���@�ff@�M�@�$�@��7@��`@��@�9X@�+@�5?@���@��h@��@�O�@�j@��m@���@�\)@�;d@�"�@�ȴ@�v�@��@��h@���@��9@��D@�z�@�I�@�1@��
@��@���@���@�p�@�O�@���@�b@��@��@���@�(�@��F@��\@��-@�%@�Z@�Z@�A�@�b@�(�@��j@���@�(�@��w@��@��@��+@�@���@��#@��#@��#@��T@��-@�p�@�`B@��@�z�@���@��9@��@�1@~��@}�@|�/@|j@|I�@|(�@{�m@{dZ@z��@z��@y��@y��@y�^@y��@x��@xbN@w��@w;d@v�R@vv�@vff@vff@vff@v�+@v��@v��@v��@vE�@u�@u��@u@u��@u�-@uV@t�j@t�@t�@t��@t��@t��@t�D@t9X@s�@s@r��@r�!@rM�@r-@q�#@q�@q%@p�`@p1'@o+@nv�@m�T@m@m��@m�@m`B@m/@l�@kƨ@kdZ@ko@j�H@j�!@j^5@j�@i&�@h�@hA�@g�w@g|�@g\)@g;d@fE�@eO�@d��@c��@cdZ@cC�@c33@c33@c33@c33@c33@c"�@c"�@c@b^5@aX@` �@_��@_+@^�+@^@]�@\��@\(�@[dZ@[33@["�@["�@[@Z�@Z�H@Z��@Z��@Zn�@Y%@XQ�@Xb@Xb@X  @W��@W�P@V��@V$�@U�-@U�@U�@Up�@Up�@T��@S��@S�@SS�@So@R�@R��@R-@Q�@Q%@P��@P��@P��@P�`@PĜ@P�9@P��@Pr�@P �@O�@O��@O��@O+@N��@N�+@N$�@M�-@M`B@L�/@L�D@LZ@L1@Kƨ@K��@K��@K��@KS�@Ko@J-@IX@H �@F��@E�@E��@E@E��@E?}@E�@DZ@D1@B��@B^5@BM�@A��@Ax�@AG�@A�@@��@@��@@r�@@r�@@Q�@?�w@>��@=�@=�-@=V@<Z@<1@;dZ@:��@:�\@:~�@:M�@9�7@8r�@8bN@8bN@8bN@8bN@8bN@8bN@8bN@8bN@8Q�@8bN@8bN@8bN@8r�@8bN@8bN@8Q�@8  @7�@7�w@7K�@6��@6ȴ@6��@6E�@5?}@5�@5?}@4�@4�/@4��@5/@5V@49X@3o@2�H@2�@3@3@2�@2�H@2��@2n�@1�@1��@1�^@1��@1�@1�@1�@1�#@1�@1�@1�@1��@1%@0bN@0Q�@0Q�@/�@/�;@/�@/�@/�P@/|�@/\)@/\)@/K�@/+@/�@.��@.��@.�y@.�y@.�y@.�y@.�y@.�y@.ȴ@.��@.ff@.5?@.$�@.{@-��@-V@,9X@+S�@*�H@*^5@*J@)��@)&�@(��@(�u@(��@(��@(�`@(�@(bN@(r�@(Q�@( �@(b@'�w@&��@&V@&@%�T@%�h@%/@$�j@$z�@$I�@$9X@$(�@#�m@#��@#�@#33@"�@"��@"�\@"M�@!�#@!��@!�7@!�7@!x�@!hs@!X@!G�@!7L@!�@!%@ �9@ �u@ bN@ A�@ 1'@ b@�@�w@|�@@�/@�D@��@�D@�D@z�@j@Z@Z@I�@9X@(�@�@�@�@1@��@�m@�m@�
@�F@t�@33@"�@�@��@�!@�!@��@�\@~�@^5@M�@-@�@�^@hs@hs@G�@G�@7L@�@��@�9@Q�@Q�@Q�@�@|�@
=@�+@{@�T@��@@�h@p�@�@��@�j@j@(�@�m@ƨ@��@33@33@@��@��@��@��@��@��@��@��@~�@n�@n�@n�@M�@�@�^@�^@��@�7@x�@�@��@��@�@�@�@�@Q�@A�@b@�@�@�;@��@��@��@;d@
=@��@��@�y@��@�y@E�@��@�h@�@`B@?}@�@��@�@�j@1@t�@"�@@
�H@
^5@
=q@
�@	��@	�#@	��@	x�@	hs@	hs@	X@	G�@	G�@	G�@	7L@	&�@Ĝ@�@1'@�;@|�@+@�@
=@�@ȴ@��@v�@ff@ff@V@5?@$�@{@{@@@@�@�T@@��@p�@`B@?}@�@�j@�@j@1@1@1@�m@��@�@t�@33@o@o@@@@�@�@��@~�@^5@^5@^5@^5@M�@M�@=q@-@�@hs@x�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B��BĜBǮBȴBɺB��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�)B�NB�yB��BffB�B�hB��B�\BdZBW
BjB/B$�BC�B:^B2-B]/B_;Bt�Bq�BiyBgmBcTBH�BM�BL�B,BA�BD�B7LB.B"�BDB�/B��B�B�B�B�B�BbBBB��B�`B�B��BɺB�dB�9B�B�{B��B��B��B�hB~�BQ�B33B%�BoB+B%B
��B
�B
�B
�B
�TB
�#B
��B
ȴB
�LB
��B
�oB
�PB
�+B
�B
y�B
p�B
o�B
m�B
bNB
S�B
J�B
49B
$�B
 �B
�B
�B
+B

=B
B	�B	��B	�B	�B	�B	ǮB	��B	��B	��B	��B	ȴB	ĜB	�wB	�-B	��B	��B	�oB	��B	�hB	�7B	�oB	�\B	�7B	|�B	jB	l�B	S�B	YB	ZB	VB	M�B	9XB	#�B	33B	0!B	.B	&�B	�B	�B	%B��B�B�B�B�#B�TB�NB�BB�TB�TB�;B�#B�B��B��BǮBŢB�}B�qB�LB�jB�RB�FB��B��B�-B�-B�B��B��B�VB�B�\B��B��B��B�oB�\B�7B}�Bm�B[#BZBe`Bl�BgmBXBT�BF�BI�BS�BbNBaHB_;B^5B]/B]/B[#BT�BP�BR�BYBVBS�BS�BN�BE�B<jBK�BN�BN�BJ�BA�B8RB8RB?}B5?B6FB7LB&�B2-B;dB=qB@�B;dB6FB-B$�B�B�B�B-B+B-B/B/B.B,B+B-B+B'�B�B�B�B(�B#�B!�B'�B%�B�B(�B1'B-B"�B#�B0!B-B'�B'�B&�B+B)�B+B-B'�B"�B%�B"�B �B$�B!�B�B!�B1'B1'B33B/B+B-B$�B%�B/B1'B/B.B6FB:^BB�B?}B=qB8RB8RBB�BL�BO�BP�BQ�BR�BR�BQ�BQ�BW
BYBW
BVBR�BP�B^5B_;B^5B^5B\)BYBW
B[#B`BBaHBbNBdZBdZBe`BhsBhsBhsBiyBffBe`BjBn�Bn�Bn�Bm�Bk�BjBjBo�Bu�Bx�Bx�Bw�Bv�Bs�Bv�Bx�Bx�B}�B}�B}�B}�By�B� B�B�DB�DB�DB�\B�{B��B��B��B��B��B��B��B��B�!B�!B�B�9B�dBBÖBB��BȴB��B��B��B��B��B��B��B�B�B�5B�BB�HB�BB�HB�NB�NB�/B�TB�B�B�B��B	B		7B	oB	�B	�B	�B	�B	(�B	+B	49B	49B	5?B	:^B	A�B	@�B	=qB	<jB	?}B	F�B	M�B	O�B	S�B	W
B	XB	YB	ZB	ZB	]/B	aHB	`BB	gmB	l�B	l�B	k�B	iyB	k�B	n�B	v�B	|�B	�B	�B	�B	�%B	�=B	�\B	�VB	�{B	��B	��B	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	��B	�B	�B	�!B	�B	�!B	�B	�B	�B	�B	�'B	�9B	�?B	�?B	�LB	�LB	�LB	�^B	�^B	�RB	�XB	�jB	�wB	��B	B	B	B	B	B	��B	ƨB	ǮB	ȴB	ȴB	ǮB	ǮB	ŢB	ƨB	ɺB	ɺB	��B	��B	��B	ɺB	��B	��B	��B	��B	�B	�B	�B	�B	�B	�#B	�B	�B	�B	�B	�B	�#B	�;B	�HB	�HB	�TB	�ZB	�ZB	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�sB	�B	�B	��B	��B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
B
B
%B
%B
+B
+B
	7B
JB
JB
PB
VB
VB
\B
bB
bB
hB
oB
oB
oB
hB
bB
VB
PB
VB
VB
oB
�B
�B
�B
�B
�B
{B
�B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
#�B
#�B
#�B
!�B
!�B
%�B
&�B
&�B
&�B
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
'�B
'�B
'�B
'�B
)�B
)�B
+B
-B
.B
,B
+B
,B
.B
.B
.B
/B
/B
/B
-B
,B
/B
1'B
1'B
2-B
2-B
2-B
2-B
1'B
2-B
6FB
7LB
8RB
8RB
8RB
8RB
9XB
9XB
9XB
9XB
8RB
7LB
6FB
8RB
9XB
7LB
8RB
8RB
9XB
9XB
9XB
9XB
9XB
9XB
9XB
:^B
;dB
<jB
=qB
=qB
=qB
=qB
=qB
=qB
=qB
>wB
>wB
>wB
>wB
?}B
=qB
=qB
;dB
:^B
;dB
<jB
=qB
<jB
<jB
<jB
?}B
@�B
A�B
A�B
A�B
B�B
C�B
C�B
B�B
C�B
B�B
A�B
B�B
D�B
E�B
D�B
C�B
D�B
F�B
G�B
H�B
G�B
H�B
I�B
H�B
J�B
K�B
K�B
K�B
L�B
L�B
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
N�B
N�B
O�B
P�B
P�B
O�B
O�B
N�B
K�B
G�B
L�B
S�B
W
B
VB
W
B
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
W
B
W
B
W
B
VB
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
XB
YB
XB
XB
YB
XB
ZB
ZB
[#B
[#B
ZB
[#B
ZB
ZB
\)B
\)B
[#B
ZB
[#B
\)B
\)B
^5B
_;B
_;B
^5B
_;B
^5B
_;B
^5B
_;B
_;B
`BB
aHB
`BB
`BB
aHB
aHB
aHB
cTB
cTB
cTB
cTB
cTB
cTB
bNB
bNB
bNB
cTB
bNB
aHB
aHB
bNB
cTB
cTB
cTB
cTB
bNB
bNB
dZB
dZB
e`B
e`B
e`B
dZB
e`B
dZB
e`B
e`B
e`B
e`B
e`B
e`B
dZB
ffB
ffB
ffB
gmB
ffB
e`B
dZB
e`B
gmB
hsB
hsB
hsB
hsB
hsB
hsB
ffB
e`B
ffB
iyB
k�B
k�B
jB
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
n�B
n�B
n�B
l�B
m�B
m�B
n�B
n�B
o�B
p�B
q�B
p�B
q�B
q�B
q�B
r�B
r�B
q�B
q�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
q�B
r�B
r�B
r�B
s�B
s�B
s�B
r�B
s�B
s�B
s�B
t�B
t�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
v�B
w�B
w�B
w�B
w�B
w�B
v�B
v�B
v�B
u�B
x�B
y�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B��BĶB��BȴBɺB��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�)B�4B�yB��BfLB�YB�@B�B�[BlWB^OBp�B6�B(�BD�B=B5�B_�Ba�Bv�BtTBl"Bi�Be`BL�BP�BO�B1�BD3BGB9�B0;B%FB\B�B 4B�BB�B$BBNB�BB�PB��B��B�hB�~B�(B��B��B�YB��B��B�KB�TB�BV�B7�B)�B�B	�B�B
��B
��B
�B
�B
�ZB
�CB
�:B
��B
�rB
��B
�B
�B
��B
�-B
{dB
rB
pUB
n/B
c�B
U�B
L~B
7fB
'mB
#B
VB
�B
	RB
^B
�B	�hB	��B	�B	�WB	خB	ɠB	өB	бB	�pB	˒B	�RB	�mB	��B	��B	��B	�/B	��B	��B	�B	��B	��B	��B	�=B	~�B	mB	m�B	V�B	Y�B	Z�B	V�B	N�B	;�B	&LB	3�B	0�B	.�B	'�B	�B	$B	KB�lB��B��B��B��B�ZB�B��B�B�B߾B��B��B��B�B�B�+B� B��B�	B�<B�>B�LB��B�B�GB�GB�OB�KB�TB��B�?B�bB��B��B�B�B��B�	B.Bo�B^�B]/Bf�Bm]Bh�BZ7BV�BI�BL0BUBbhBa�B_�B^�B]�B]~B[�BU�BR BS�BYBVSBT{BTFBOvBF�B>]BL0BOBBOBBK^BB�B:B9�B@OB7LB7�B8�B)�B3�B<B=�B@�B;�B6�B.}B&�B�B�B�B-]B+�B-]B/OB/iB.cB,qB+kB-]B+kB(�B �B	B �B)DB$�B"�B(sB&�B 'B)yB1'B-�B$ZB%,B0�B-�B)B(�B'�B+�B*�B+�B-wB(�B#�B&�B#�B!�B%�B#B�B#B1vB1�B3�B/�B,"B./B&�B'mB0;B2B0oB/iB7fB;BB�B@4B>BB9�B9�BC�BMBPBQBR BS&BS&BRTBRoBW?BYKBWsBV�BS�BQ�B^OB_VB^jB^jB\xBY�BW�B[�B`�Ba�Bb�Bd�Bd�Be�Bh�Bh�Bh�Bi�Bf�Be�Bj�Bn�Bn�Bn�Bm�Bk�BkBk6Bp!BvBx�Bx�BxBwBt9BwBy>ByXB~BB~]B~]B~]Bz�B��B��B��B��B��B��B��B�
B�	B�'B�&B�
B�$B�zB�B�oB��B��B��B��BªB��B��B�'B�B��B� B�,B�,B�@B�@B�aB�yBؓB�jB�vB�HB�vB�|B�B�B�B��B�B��B�B�ZB	;B		RB	:B	�B	B	kB	VB	)yB	+kB	49B	4TB	5ZB	:DB	A;B	@�B	=�B	<�B	?�B	F�B	NB	P.B	TB	W
B	XB	Y1B	ZB	ZQB	]dB	abB	`�B	g�B	lqB	lqB	k�B	i�B	lB	oB	v�B	}"B	� B	�-B	�MB	�YB	�rB	�vB	��B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�DB	�B	�B	�!B	�5B	�!B	�B	�5B	�IB	�cB	�[B	�TB	�ZB	�tB	�fB	��B	��B	�xB	�^B	��B	��B	��B	��B	��B	ªB	ªB	ªB	B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	�	B	�0B	�B	�4B	�2B	�+B	�B	�B	�B	�B	�#B	�7B	�7B	�7B	�EB	�yB	یB	�pB	�|B	�B	�B	�tB	�B	�B	��B	�B	�B	�B	�B	�B	��B	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�2B	�B	��B	�B	�B	�B	�"B	�VB
B
B
B
�B
-B
-B
B
3B
3B
3B
?B
?B
EB
_B
	lB
JB
dB
jB
pB
�B
vB
}B
}B
�B
oB
oB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
#�B
#�B
#�B
"B
!�B
%�B
&�B
&�B
&�B
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
'�B
(
B
(
B
(
B
*B
*B
+B
-)B
./B
,=B
+QB
,"B
.B
./B
.B
/B
/ B
/5B
-CB
,WB
/5B
1'B
1'B
2B
2-B
2-B
2GB
1AB
2aB
6FB
7LB
8RB
8RB
8RB
8RB
9XB
9>B
9XB
9rB
8�B
7�B
6zB
8RB
9XB
7�B
8RB
8RB
9XB
9rB
9XB
9rB
9>B
9XB
9XB
:DB
;B
<PB
=qB
=qB
=qB
=qB
=qB
=qB
=�B
>�B
>�B
>wB
>wB
?�B
=�B
=�B
;�B
:�B
;�B
<�B
=�B
<�B
<�B
<�B
?}B
@�B
A�B
AoB
A�B
B�B
C�B
C�B
B�B
C�B
B�B
A�B
B�B
D�B
E�B
D�B
C�B
D�B
F�B
G�B
H�B
G�B
H�B
I�B
H�B
J�B
K�B
K�B
K�B
L�B
L�B
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
N�B
N�B
O�B
Q B
P�B
O�B
O�B
N�B
K�B
H1B
M6B
TB
W
B
VB
W
B
W
B
VB
W
B
W
B
V�B
W
B
V�B
W
B
V�B
W
B
W
B
V�B
W
B
V�B
W$B
W$B
VB
W$B
XB
W$B
XB
YB
YB
YB
YB
YB
X+B
YB
X+B
X+B
Y1B
X+B
ZB
Z7B
[#B
[	B
Z7B
[=B
Z7B
Z7B
\)B
\CB
[WB
ZQB
[WB
\CB
\]B
^OB
_;B
_;B
^OB
_;B
^OB
_VB
^5B
_VB
_VB
`\B
abB
`\B
`\B
aHB
aHB
aHB
cTB
cTB
cTB
cTB
cTB
cTB
bNB
bhB
b4B
cTB
bNB
abB
abB
bhB
cTB
cTB
cnB
cTB
bhB
b�B
dtB
dtB
e`B
e`B
eFB
dZB
e`B
dtB
e`B
e`B
e`B
e`B
e`B
ezB
d�B
f�B
ffB
fLB
gmB
ffB
ezB
d�B
e�B
g�B
hsB
hsB
h�B
h�B
h�B
h�B
ffB
e�B
f�B
i�B
k�B
k�B
j�B
l�B
l�B
m�B
m�B
m�B
m�B
n�B
n}B
n}B
n�B
o�B
n�B
n}B
n�B
l�B
m�B
m�B
n�B
n�B
o�B
p�B
q�B
p�B
q�B
q�B
q�B
r�B
r�B
q�B
q�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
q�B
r�B
r�B
r�B
s�B
s�B
s�B
r�B
s�B
s�B
s�B
t�B
t�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
v�B
w�B
w�B
w�B
w�B
w�B
v�B
v�B
v�B
u�B
x�B
y�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=-0.02(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201811110035352018111100353520181111003535201811110200192018111102001920181111020019201811120025482018111200254820181112002548  JA  ARFMdecpA19c                                                                20181107093514  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20181107003516  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20181107003519  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20181107003519  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20181107003520  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20181107003520  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20181107003520  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20181107003520  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20181107003520  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20181107003520                      G�O�G�O�G�O�                JA  ARUP                                                                        20181107005650                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20181107153049  CV  JULD            G�O�G�O�F�u�                JM  ARGQJMQC2.0                                                                 20181107153049  CV  JULD_LOCATION   G�O�G�O�F�u�                JM  ARGQJMQC2.0                                                                 20181107153049  CV  LATITUDE        G�O�G�O�A���                JM  ARGQJMQC2.0                                                                 20181107153049  CV  LONGITUDE       G�O�G�O��!�                JM  ARCAJMQC2.0                                                                 20181110153535  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20181110153535  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20181110170019  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20181111152548  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115131516                      G�O�G�O�G�O�                