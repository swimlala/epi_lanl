CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2016-06-26T00:35:26Z creation;2016-06-26T00:35:28Z conversion to V3.1;2019-12-19T08:33:26Z update;     
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
_FillValue                 �  I$   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `t   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �h   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �L   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �@   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �$   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �4   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ܼ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �L   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �L   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �L   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �L   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �,   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �0   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �@   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �D   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �H   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �LArgo profile    3.1 1.2 19500101000000  20160626003526  20200116201515  5905048                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               A   JA  I2_0577_011                     2C  D   NAVIS_A                         0577                            ARGO 102115                     863 @׶�R���1   @׶��[ @2��u���d�V�Ϫ�1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�ff@�  A   A   A@  Aa��A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B?��BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�|�D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ D�|�D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�C3Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�3D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�
=@���A Q�A Q�A@Q�Aa�A�(�A�(�A�(�A�(�A�(�A�(�A�(�A�(�B {B{B{B{B {B({B0{B8{B?�BH{BP{BX{B`{Bh{Bp{Bx{B�
=B�
=B�
=B�=qB�
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
CCCCCCCCCCC C"C$C&C(C*C,C.C0C2C4C6C8C:C<C>C@CBCDCFCHCJCLCNCPCRCTCVCXCZC\C^C`CbCdCfChCjClCnCpCrCtCvCxCzC|C~C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C���C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C���C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D HD �HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HD	HD	�HD
HD
�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HD HD �HD!HD!�HD"HD"�HD#HD#�HD$HD$�HD%HD%�HD&HD&�HD'HD'�HD(HD(�HD)HD)�HD*HD*�HD+HD+�HD,HD,�HD-HD-�HD.HD.�HD/HD/�HD0HD0�HD1HD1�HD2HD2�HD3HD3�HD4HD4�HD5HD5�HD6HD6�HD7HD7�HD8HD8�HD9HD9�HD:HD:�HD;HD;�HD<HD<�HD=HD=�HD>HD>�HD?HD?�HD@HD@�HDAHDA�HDBHDB�HDCHDC�HDDHDD�HDEHDE�HDFHDF�HDGHDG�HDHHDH�HDIHDI�HDJHDJ�HDKHDK�HDLHDL�HDMHDM�HDNHDN�HDOHDO�HDPHDP�HDQHDQ�HDRHDR�HDSHDS�HDTHDT�HDUHDU�HDVHDV�HDWHDW�HDXHDX�HDYHDY�HDZHDZ�HD[HD[�HD\HD\�HD]HD]�HD^HD^�HD_HD_�HD`HD`�HDaHDa�HDbHDb�HDcHDc�HDdHDd�HDeHDe�HDfHDf�HDgHDg�HDhHDh�HDiHDi�HDjHDj�HDkHDk�HDlHDl�HDmHDm�HDnHDn�HDoHDo�HDpHDp�HDqHDq�HDrHDr�HDsHDs�HDtHDt�HDuHDu�HDvHDv�HDwHDw�HDxHDx�HDyHDy�HDzHDz�HD{HD{�HD|HD|�HD}HD}�HD~HD~�HDHD�HD� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D��qD�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D��qD� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�=qD�}qD��qD� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D�D���D� �D�@�DÀ�D���D� �D�@�DĀ�D���D� �D�@�Dŀ�D���D� �D�@�Dƀ�D���D� �D�@�Dǀ�D���D� �D�@�DȀ�D���D� �D�@�Dɀ�D���D� �D�@�Dʀ�D���D� �D�@�D�}qD���D� �D�@�D̀�D���D� �D�@�D̀�D���D� �D�@�D΀�D���D� �D�@�Dπ�D���D� �D�@�DЀ�D���D� �D�@�Dр�D���D� �D�@�DҀ�D���D� �D�@�DӀ�D���D� �D�@�DԀ�D���D� �D�@�DՀ�D���D� �D�@�Dր�D���D� �D�@�D׀�D���D� �D�@�D؀�D���D� �D�@�Dـ�D���D� �D�C�Dڀ�D���D� �D�@�Dۀ�D���D� �D�@�D܀�D���D� �D�@�D݀�D���D� �D�@�Dހ�D���D� �D�@�D߀�D���D� �D�@�D���D���D� �D�@�DဤD���D� �D�@�D․D���D� �D�@�D〤D���D� �D�@�D䀤D���D� �D�@�D値D���D� �D�@�D怤D���D� �D�@�D瀤D���D� �D�@�D耤D���D� �D�@�D逤D���D� �D�@�DꀤD���D� �D�@�D뀤D���D��D�@�D쀤D���D� �D�@�D퀤D���D� �D�@�DD���D� �D�@�DD���D� �D�@�D���D���D� �D�@�D�D���D� �D�@�D�D���D� �D�@�D�D���D� �D�@�D�D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D��=1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��A��A��A��A��A�{A�bA�oA�{A�oA�bA�bA�bA�oA�bA�oA�oA�VA�
=A�
=A�%A���A��A�z�A�9XA��A�~�A���A���A҇+A��A���A�JAЮAЅAϙ�A�9XA�bA��yA�+AɶFA�(�A�t�Aǲ-A�bNA��AƗ�Aź^A���Aħ�A�XA�bNA���A�?}A�C�A���A���A�{A��`A�oA�l�A���A���A�ZA�XA��A�G�A��9A�33A���A� �A��FA��TA�
=A���A��A�M�A�jA�1A�^5A�A�33A��A�ƨA��7A�1A�VA��-A���A��\A��A��A���A��A�C�A�;dA���A�l�A��A�C�A�1A�hsA���A��DA���A��A��A�7LA�;dA�K�A���A��A�$�Az�Ay�PAyhsAydZAx��AxbNAxffAq\)Am%AiƨAg33Ad��AdjAb��A`�DA^�A[�AWt�AV-AT��ATAS7LAR�`AQ�
AP�RAO�AM��AKAHȴAF�DAD�HAB~�A?�#A<�`A:�A9�A9%A8A�A5�PA2�A1��A0��A01A-�A-7LA-oA,~�A+��A*�`A)��A)��A(ZA'?}A%�A$ȴA#�7A"ffA!�wA!�A�TA�RA\)A��AdZA�wA��A  A`BAM�Av�A�A5?Al�A��AXA�!A1A�A��A��At�A\)AĜA
�A��AI�A�PA�Ap�A�mAoA5?Ap�A�7A(�A��A�A�A�+A|�AG�A �HA I�@���@��mA �DA7LA �@�V@��@���@��T@��`@��F@��@��\@��T@��T@���@�%@��@�p�@�@�@�+@��@�V@���@��@�^5@��@��/@��^@�`B@�R@�9X@�bN@�7@�@���@�O�@ڰ!@�hs@ٙ�@և+@Ձ@Ԭ@ҸR@���@�z�@�9X@�  @�ƨ@�j@�
=@�x�@˶F@�@ʟ�@�E�@��@�X@��`@Ȭ@ȃ@�Q�@��H@�p�@ċD@ě�@�b@�ƨ@��@��^@��@���@���@��@��\@���@�1@�7L@�M�@��@��@�@��y@�V@��^@��T@�^5@��w@���@�o@�t�@���@���@���@�x�@�&�@�X@�%@�/@��@���@��@���@��D@�bN@�9X@��D@��@��m@���@��@���@��9@��@�l�@���@�ff@�7L@��`@�/@�%@�z�@��@���@�V@�o@�~�@�G�@���@�ȴ@�33@�t�@���@�K�@�@�
=@��H@���@�~�@��@��h@�hs@�Ĝ@�I�@��;@�dZ@�j@�ȴ@��@��H@��\@�ff@�o@��F@�9X@�r�@�Ĝ@�7L@�&�@��`@��@���@�Ĝ@��@�33@�J@���@�hs@��`@���@�1'@�1@�C�@�33@��y@���@�"�@�|�@�ff@���@���@��@���@��`@���@�Q�@�A�@�b@�b@��@� �@� �@��m@���@�o@��+@�J@�/@��@�/@�hs@��7@��^@���@��7@�hs@�%@�z�@�1@��
@���@��@�"�@��@�-@���@���@�`B@�O�@�/@�z�@�K�@�;d@�
=@�"�@�S�@���@��@�33@���@���@��@�%@��@���@��u@�A�@���@�S�@�;d@�+@�t�@�l�@�S�@��@�o@�@���@�v�@�5?@�{@��-@�x�@�G�@�7L@�V@���@�bN@�(�@� �@��
@��@�|�@�K�@�;d@�l�@�+@�ȴ@��@���@���@�~�@�=q@�=q@�=q@��@��u@���@��F@�S�@�"�@���@��@�ȴ@�^5@�J@�@���@��h@��7@�O�@���@��@���@��D@�z�@�j@�Q�@�(�@�  @��
@���@�S�@�@��@��@��@��@��R@���@�n�@���@�?}@��u@�bN@�Q�@�9X@�9X@�(�@���@��m@�ƨ@���@�l�@�@��\@���@�@���@���@�O�@�&�@�%@���@��`@��j@���@��@�1'@;d@}�@~��@+@;d@l�@+@~{@}`B@}�@|��@|�D@|1@{��@z�@z~�@x��@xb@w|�@w�@v��@u�@uO�@u/@t�@t9X@t(�@s33@q��@q��@qhs@q7L@q&�@p�u@pb@o��@o\)@n�@m@m?}@lz�@l1@k33@j�\@j��@j�!@j~�@i�7@iX@i7L@h��@h�`@h�@g�w@gl�@g;d@fȴ@fV@e�-@d��@d�@d�@d�D@dz�@dI�@c�F@b~�@a�@`��@`Ĝ@`��@`�@`b@_;d@_+@_�@_�@^ȴ@^��@^ff@^5?@^{@]�T@]�@^{@]�@]�@\I�@[�
@[��@[dZ@Z��@Z=q@Yx�@X�@XQ�@XA�@XQ�@XQ�@X1'@W�P@V�@Vff@V{@U��@U`B@UV@TI�@Sƨ@SdZ@SC�@S33@So@R�@R��@R=q@Q��@Q�#@Q��@Q�7@Q�7@Qx�@P��@PA�@P �@O�@O�;@O|�@Nv�@N{@M��@M�@M`B@M�@MV@MV@MV@L�@L�j@LZ@K�
@Kt�@KC�@K"�@J�H@J�\@J^5@I�#@H��@HbN@HQ�@HA�@G�w@F�y@F�+@F{@E�T@E��@Ep�@EO�@D��@D�@C�
@C�@B��@B�@A��@A&�@@r�@?|�@?;d@?
=@>5?@=@=��@=�@=?}@=�@<�@<�@<9X@;�
@;��@;t�@;C�@;"�@;@:��@:-@9��@9X@8��@8�@8Q�@7�@7\)@7�@6��@6�@6�R@6�+@6$�@5p�@5?}@5�@4��@4�j@4�D@49X@3��@3"�@2��@2�!@2n�@2�@1��@1��@1G�@1%@0��@0�u@0�@0�@0bN@0b@/K�@/;d@/;d@/+@/
=@/
=@.�y@.�y@.�@.ȴ@.��@.$�@-�@-�T@-�T@-�-@-p�@-O�@-?}@-�@-V@-V@,��@,�@,�j@,I�@+ƨ@+33@*�H@*��@*�\@*=q@)�#@)��@)x�@)G�@)&�@)%@(Q�@'��@'�@'|�@'l�@'K�@'�@&�R@&�+@&E�@%�@%�@%O�@$��@$z�@$(�@$�@#�
@#�@#dZ@#S�@#"�@"��@"~�@"-@!�#@!�7@!hs@!G�@!%@ �`@ �`@ ��@ ��@ bN@   @��@K�@�@�y@ȴ@�R@��@v�@V@{@��@�@O�@?}@��@��@�j@�@��@z�@I�@9X@1@�m@�F@��@t�@dZ@"�@��@�!@�\@~�@M�@=q@-@J@�@��@��@�7@hs@G�@��@Ĝ@�@Q�@ �@  @  @�@�;@��@�w@�@��@|�@K�@�@��@�y@�y@�@�R@�+@�+@{@�T@@�-@��@�h@p�@`B@O�@O�@O�@?}@�@V@��@�/@j@I�@�@�F@dZ@S�@C�@"�@o@@��@~�@n�@^5@M�@=q@-@-@J@�#@�^@��@x�@hs@X@&�@%@%@��@�`@�9@�@A�@ �@b@  @�@�;@�;@�w@�@�@�@��@|�@l�@K�@;d@�@��@��@��@�@��@��@�+@ff1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��A��A��A��A��A�{A�bA�oA�{A�oA�bA�bA�bA�oA�bA�oA�oA�VA�
=A�
=A�%A���A��A�z�A�9XA��A�~�A���A���A҇+A��A���A�JAЮAЅAϙ�A�9XA�bA��yA�+AɶFA�(�A�t�Aǲ-A�bNA��AƗ�Aź^A���Aħ�A�XA�bNA���A�?}A�C�A���A���A�{A��`A�oA�l�A���A���A�ZA�XA��A�G�A��9A�33A���A� �A��FA��TA�
=A���A��A�M�A�jA�1A�^5A�A�33A��A�ƨA��7A�1A�VA��-A���A��\A��A��A���A��A�C�A�;dA���A�l�A��A�C�A�1A�hsA���A��DA���A��A��A�7LA�;dA�K�A���A��A�$�Az�Ay�PAyhsAydZAx��AxbNAxffAq\)Am%AiƨAg33Ad��AdjAb��A`�DA^�A[�AWt�AV-AT��ATAS7LAR�`AQ�
AP�RAO�AM��AKAHȴAF�DAD�HAB~�A?�#A<�`A:�A9�A9%A8A�A5�PA2�A1��A0��A01A-�A-7LA-oA,~�A+��A*�`A)��A)��A(ZA'?}A%�A$ȴA#�7A"ffA!�wA!�A�TA�RA\)A��AdZA�wA��A  A`BAM�Av�A�A5?Al�A��AXA�!A1A�A��A��At�A\)AĜA
�A��AI�A�PA�Ap�A�mAoA5?Ap�A�7A(�A��A�A�A�+A|�AG�A �HA I�@���@��mA �DA7LA �@�V@��@���@��T@��`@��F@��@��\@��T@��T@���@�%@��@�p�@�@�@�+@��@�V@���@��@�^5@��@��/@��^@�`B@�R@�9X@�bN@�7@�@���@�O�@ڰ!@�hs@ٙ�@և+@Ձ@Ԭ@ҸR@���@�z�@�9X@�  @�ƨ@�j@�
=@�x�@˶F@�@ʟ�@�E�@��@�X@��`@Ȭ@ȃ@�Q�@��H@�p�@ċD@ě�@�b@�ƨ@��@��^@��@���@���@��@��\@���@�1@�7L@�M�@��@��@�@��y@�V@��^@��T@�^5@��w@���@�o@�t�@���@���@���@�x�@�&�@�X@�%@�/@��@���@��@���@��D@�bN@�9X@��D@��@��m@���@��@���@��9@��@�l�@���@�ff@�7L@��`@�/@�%@�z�@��@���@�V@�o@�~�@�G�@���@�ȴ@�33@�t�@���@�K�@�@�
=@��H@���@�~�@��@��h@�hs@�Ĝ@�I�@��;@�dZ@�j@�ȴ@��@��H@��\@�ff@�o@��F@�9X@�r�@�Ĝ@�7L@�&�@��`@��@���@�Ĝ@��@�33@�J@���@�hs@��`@���@�1'@�1@�C�@�33@��y@���@�"�@�|�@�ff@���@���@��@���@��`@���@�Q�@�A�@�b@�b@��@� �@� �@��m@���@�o@��+@�J@�/@��@�/@�hs@��7@��^@���@��7@�hs@�%@�z�@�1@��
@���@��@�"�@��@�-@���@���@�`B@�O�@�/@�z�@�K�@�;d@�
=@�"�@�S�@���@��@�33@���@���@��@�%@��@���@��u@�A�@���@�S�@�;d@�+@�t�@�l�@�S�@��@�o@�@���@�v�@�5?@�{@��-@�x�@�G�@�7L@�V@���@�bN@�(�@� �@��
@��@�|�@�K�@�;d@�l�@�+@�ȴ@��@���@���@�~�@�=q@�=q@�=q@��@��u@���@��F@�S�@�"�@���@��@�ȴ@�^5@�J@�@���@��h@��7@�O�@���@��@���@��D@�z�@�j@�Q�@�(�@�  @��
@���@�S�@�@��@��@��@��@��R@���@�n�@���@�?}@��u@�bN@�Q�@�9X@�9X@�(�@���@��m@�ƨ@���@�l�@�@��\@���@�@���@���@�O�@�&�@�%@���@��`@��j@���@��@�1'@;d@}�@~��@+@;d@l�@+@~{@}`B@}�@|��@|�D@|1@{��@z�@z~�@x��@xb@w|�@w�@v��@u�@uO�@u/@t�@t9X@t(�@s33@q��@q��@qhs@q7L@q&�@p�u@pb@o��@o\)@n�@m@m?}@lz�@l1@k33@j�\@j��@j�!@j~�@i�7@iX@i7L@h��@h�`@h�@g�w@gl�@g;d@fȴ@fV@e�-@d��@d�@d�@d�D@dz�@dI�@c�F@b~�@a�@`��@`Ĝ@`��@`�@`b@_;d@_+@_�@_�@^ȴ@^��@^ff@^5?@^{@]�T@]�@^{@]�@]�@\I�@[�
@[��@[dZ@Z��@Z=q@Yx�@X�@XQ�@XA�@XQ�@XQ�@X1'@W�P@V�@Vff@V{@U��@U`B@UV@TI�@Sƨ@SdZ@SC�@S33@So@R�@R��@R=q@Q��@Q�#@Q��@Q�7@Q�7@Qx�@P��@PA�@P �@O�@O�;@O|�@Nv�@N{@M��@M�@M`B@M�@MV@MV@MV@L�@L�j@LZ@K�
@Kt�@KC�@K"�@J�H@J�\@J^5@I�#@H��@HbN@HQ�@HA�@G�w@F�y@F�+@F{@E�T@E��@Ep�@EO�@D��@D�@C�
@C�@B��@B�@A��@A&�@@r�@?|�@?;d@?
=@>5?@=@=��@=�@=?}@=�@<�@<�@<9X@;�
@;��@;t�@;C�@;"�@;@:��@:-@9��@9X@8��@8�@8Q�@7�@7\)@7�@6��@6�@6�R@6�+@6$�@5p�@5?}@5�@4��@4�j@4�D@49X@3��@3"�@2��@2�!@2n�@2�@1��@1��@1G�@1%@0��@0�u@0�@0�@0bN@0b@/K�@/;d@/;d@/+@/
=@/
=@.�y@.�y@.�@.ȴ@.��@.$�@-�@-�T@-�T@-�-@-p�@-O�@-?}@-�@-V@-V@,��@,�@,�j@,I�@+ƨ@+33@*�H@*��@*�\@*=q@)�#@)��@)x�@)G�@)&�@)%@(Q�@'��@'�@'|�@'l�@'K�@'�@&�R@&�+@&E�@%�@%�@%O�@$��@$z�@$(�@$�@#�
@#�@#dZ@#S�@#"�@"��@"~�@"-@!�#@!�7@!hs@!G�@!%@ �`@ �`@ ��@ ��@ bN@   @��@K�@�@�y@ȴ@�R@��@v�@V@{@��@�@O�@?}@��@��@�j@�@��@z�@I�@9X@1@�m@�F@��@t�@dZ@"�@��@�!@�\@~�@M�@=q@-@J@�@��@��@�7@hs@G�@��@Ĝ@�@Q�@ �@  @  @�@�;@��@�w@�@��@|�@K�@�@��@�y@�y@�@�R@�+@�+@{@�T@@�-@��@�h@p�@`B@O�@O�@O�@?}@�@V@��@�/@j@I�@�@�F@dZ@S�@C�@"�@o@@��@~�@n�@^5@M�@=q@-@-@J@�#@�^@��@x�@hs@X@&�@%@%@��@�`@�9@�@A�@ �@b@  @�@�;@�;@�w@�@�@�@��@|�@l�@K�@;d@�@��@��@��@�@��@��@�+@ff1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B  B  B
��B  B�B� B�B�+B�VB��B��B��B�B�-B�^B�qB��B�HB��BB��B  BDBoB2-BP�BS�B`BBz�B��B��B��B��B�B�B�?B�qB��B�HB�yB�B��B  B  B��B�`B��B�B&�B)�B'�B�BbBDB��B�BŢB��B�JB�bBx�Bp�B\)Bx�Bz�Bv�BdZBVBL�BC�B5?B!�B�B\B�B��BÖB�?B��B�7B�B}�Bo�BcTBJ�B2-B'�BbB
��B
�B
��B
��B
q�B
ffB
_;B
=qB
,B
+B
+B
+B
(�B
.B
  B	�#B	ŢB	�B	��B	��B	�{B	�%B	y�B	o�B	YB	O�B	I�B	E�B	C�B	A�B	=qB	7LB	2-B	-B	"�B	�B	VB		7B	  B��B�B�B�B�sB�`B�NB�
B��B��B��BȴBŢBĜBÖB��B�wB�^B�XB�RB�}B�wB�LB�'B�'B�B��B��B��B�VB�7B�B�B�B�=B�%B�Bq�Bn�BjBiyBgmBhsBq�Br�Bs�B�B�B�3B�?B�jB�3B��B�bB�7Bt�BffBbNBdZB|�B�bB�B�LB��BÖBÖB�RB��B��B��B��B��B�!B�wB�
B�B��BƨB��B�#B�5B�/B�NB�`B�TB�`B�mB�`B�fB�NB�B�B�B�TB�ZB�`B�NB�/B�TB�`B�B�B�sB�;B�NB�sB�B�;B��BƨB��B��B��B��B��B�)B�B�B�#B�)B�)B�TB�yB�sB�mB�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B��B��B��B		7B	uB	�B	'�B	+B	,B	33B	5?B	8RB	;dB	G�B	A�B	>wB	A�B	E�B	G�B	J�B	K�B	H�B	K�B	Q�B	VB	`BB	aHB	aHB	bNB	dZB	ffB	ffB	hsB	n�B	|�B	~�B	� B	�B	�1B	�1B	�7B	�\B	�bB	�hB	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�'B	�LB	�^B	�dB	�jB	�wB	�wB	��B	��B	��B	ÖB	ÖB	ÖB	B	��B	�RB	�B	�B	�!B	�'B	�?B	�dB	�wB	ÖB	ĜB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	ȴB	ƨB	ŢB	B	�qB	�?B	�?B	�?B	�XB	�jB	�qB	�}B	ƨB	ŢB	ÖB	ƨB	ȴB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�)B	�5B	�;B	�NB	�ZB	�`B	�fB	�fB	�fB	�`B	�`B	�`B	�fB	�`B	�ZB	�NB	�HB	�BB	�BB	�BB	�BB	�BB	�;B	�;B	�BB	�TB	�fB	�B	�B	�B	�B	�B	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
%B
%B
%B
	7B
DB
DB
DB
DB
DB
DB

=B

=B
1B
	7B
	7B
	7B
	7B

=B

=B
JB
JB
PB
PB
PB
PB
VB
VB
VB
\B
bB
bB
bB
bB
bB
bB
bB
bB
hB
hB
hB
hB
hB
hB
hB
hB
hB
hB
bB
hB
oB
oB
oB
oB
uB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
!�B
#�B
#�B
$�B
%�B
&�B
&�B
&�B
&�B
&�B
'�B
(�B
)�B
)�B
)�B
(�B
(�B
'�B
'�B
)�B
)�B
)�B
+B
+B
+B
+B
+B
+B
,B
-B
-B
-B
.B
.B
.B
/B
0!B
0!B
0!B
0!B
0!B
/B
0!B
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
49B
49B
49B
49B
49B
5?B
5?B
7LB
8RB
8RB
8RB
8RB
8RB
8RB
8RB
8RB
7LB
6FB
6FB
6FB
7LB
8RB
8RB
9XB
:^B
;dB
;dB
;dB
;dB
;dB
;dB
<jB
<jB
<jB
<jB
<jB
<jB
=qB
=qB
=qB
=qB
=qB
=qB
<jB
<jB
<jB
=qB
=qB
=qB
=qB
=qB
>wB
?}B
@�B
@�B
@�B
@�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
B�B
B�B
A�B
A�B
A�B
B�B
B�B
C�B
D�B
D�B
D�B
D�B
D�B
E�B
E�B
F�B
G�B
G�B
G�B
G�B
H�B
I�B
I�B
I�B
J�B
J�B
J�B
K�B
K�B
L�B
L�B
L�B
M�B
M�B
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
P�B
O�B
P�B
P�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
S�B
S�B
S�B
S�B
T�B
VB
VB
VB
VB
VB
VB
VB
W
B
W
B
XB
YB
YB
YB
YB
YB
YB
YB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
[#B
[#B
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
^5B
^5B
^5B
_;B
_;B
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
`BB
aHB
aHB
aHB
aHB
aHB
bNB
bNB
bNB
bNB
cTB
cTB
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
ffB
ffB
ffB
ffB
ffB
ffB
ffB
ffB
gmB
gmB
hsB
hsB
hsB
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
u�B
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
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
y�B
y�B
y�B
y�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B B iB 4B�B%�B� B��B�KB��B�B��B��B��B��B�0B�wB�B�@B��B�B��B B�B,B3�BQhBT�Ba�B|�B��B�~B��B��B�5B��B�2B��B�B�B��B�|B�xBB�B��B�mB��B$B(>B+�B*B�B[B�B��B��B��B�5B��B�Bz�Bs�B]~By>B|By$Be�BX_BP�BGEB8B#TBB�B�B�FB�YB��B�5B��B��B��Bq�Bf�BM�B4�B+�B�B
�B
�B
�B
��B
sMB
h�B
d@B
>�B
,qB
+QB
+�B
,=B
+kB
5B
B	��B	ȴB	��B	�'B	�B	�$B	��B	~B	s�B	Z�B	Q�B	J�B	F�B	DgB	B�B	?B	9>B	4�B	0UB	%�B	xB	 B	~B	�B�VB�TB��B�B�0B��B�,B��B�aB�4B� BɠB�%BňBĜB��B��B�JB�B�B�UB��B��B��B�-B�;B��B�|B�=B��B�B�B�'B�{B�^B��B�MBs�Bo�Bk�Bk6BhsBiyBr�Bs�Br�B��B��B��B��B��B�?B�mB��B��Bw�Bh$Bb�BcB{B�VB�]B�B��B�B�YB��B�B��B��B�8B��B��B�]B�B��B� B��B�B��B��B�dB�B��B�B��B�
B�fB�B�NB�!B�B�QB�nB��B��B�:B�IB�&B�FB�5B�;B�B�;B�4B��B��B�B̈́BǔB�DBΥB͟B̳B�HB�B�yB�QB�WB�CB�CB�ZB�B�yB��B�B��B��B�'B�B��B��B�%B��B��B�XB��B�VB�]B��B�B�RB�UB�B�wB�ZB�xB�BB	fB	�B	~B	'�B	+6B	,"B	3�B	5�B	8RB	;�B	H�B	BB	>]B	AoB	E�B	G�B	K)B	L�B	H�B	K�B	RB	VB	`vB	abB	abB	bhB	dZB	f�B	f�B	hXB	n�B	}�B	�B	�iB	�{B	��B	��B	��B	��B	��B	� B	��B	��B	��B	�)B	�=B	�B	��B	�eB	�4B	�NB	�]B	�;B	��B	��B	�'B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�aB	�-B	�$B	�B	�B	�UB	�'B	��B	��B	�(B	�aB	�gB	�zB	��B	�B	��B	��B	�B	ӏB	уB	ϑB	��B	�B	�?B	�B	��B	��B	��B	�tB	��B	�jB	�qB	��B	�EB	��B	��B	��B	ȴB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�4B	�NB	�TB	�gB	ևB	�$B	��B	�B	�B	�;B	�hB	�tB	�B	�B	��B	�B	�B	�B	�B	�B	�B	�B	�hB	�B	�vB	�\B	��B	��B	��B	�VB	�VB	�BB	�:B	�fB	��B	�B	��B	�IB	��B	�B	�B	�B	�B	��B	��B	��B	�B	�B	�oB	��B	��B	�B	��B	��B	��B	�B	�B	�	B	�B	�B	�B	�B	�B	�B	�<B	�"B	�B	�.B
 B
;B
 B
B
B
YB
?B
%B
	7B
^B
xB
^B
^B
DB
�B
B

�B
fB
	�B
	lB
	lB
	RB

XB

�B
~B
~B
jB
jB
jB
jB
�B
pB
pB
vB
}B
bB
}B
}B
}B
}B
�B
�B
�B
�B
hB
hB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
oB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
_B
�B
�B
�B
�B
B
�B
�B
�B
�B
�B
�B
B
B
5B
 B
�B
 �B
 �B
!B
!�B
!�B
!�B
$B
#�B
%FB
&LB
'B
'B
'B
'B
'B
($B
)B
)�B
*KB
*KB
)*B
)DB
($B
(>B
*0B
)�B
*B
+B
+6B
+B
+B
+B
+B
,"B
-CB
-)B
-)B
./B
.IB
.IB
/5B
0;B
0;B
0;B
0;B
0!B
/iB
0oB
1[B
1vB
2-B
2-B
2GB
2aB
3hB
33B
3B
3MB
4TB
4TB
4TB
4TB
4TB
5?B
5%B
72B
8lB
8�B
8�B
8�B
8lB
8lB
8�B
8lB
7�B
6�B
6`B
6FB
7LB
8RB
8lB
9�B
:xB
;B
;B
;B
;B
;B
;�B
<�B
<�B
<�B
<�B
<�B
<�B
=�B
=�B
=�B
=�B
=�B
=�B
<jB
<�B
<�B
=�B
=�B
=�B
=�B
=�B
>�B
?}B
@�B
@�B
@�B
@�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
B�B
B�B
A�B
A�B
A�B
B�B
B�B
C�B
D�B
D�B
D�B
D�B
D�B
E�B
E�B
F�B
G�B
G�B
G�B
G�B
H�B
I�B
I�B
J	B
J�B
J�B
KB
K�B
LB
L�B
L�B
MB
M�B
M�B
M�B
M�B
M�B
M�B
N�B
OB
O�B
O�B
O�B
O�B
O�B
O�B
O�B
PB
PB
P�B
PB
Q B
Q B
QB
Q B
Q B
RB
RB
RB
Q�B
RB
SB
TB
TB
S�B
TB
UB
VB
VB
V9B
VB
VB
VB
VB
W$B
W$B
X+B
Y1B
Y1B
Y1B
YB
YB
Y1B
YKB
ZQB
ZB
ZB
ZB
Z7B
ZB
Z7B
ZB
Z7B
[#B
[=B
[WB
\CB
]/B
]/B
]IB
]dB
]IB
]/B
^5B
^5B
^B
^5B
^OB
^OB
^jB
^jB
_pB
_VB
^OB
^OB
^OB
^OB
_VB
_VB
_VB
_VB
_VB
_VB
`vB
abB
abB
abB
aHB
abB
bhB
bhB
bhB
bhB
cnB
cnB
dtB
d�B
dtB
dZB
dtB
dtB
ezB
ezB
ezB
ezB
ezB
ezB
f�B
ffB
f�B
f�B
f�B
ffB
ffB
fLB
g�B
g�B
h�B
h�B
h�B
i�B
i�B
i�B
i_B
j�B
j�B
j�B
j�B
jB
j�B
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
l�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
mwB
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
o�B
o�B
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
u�B
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
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
y�B
y�B
y�B
y�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=-0.02(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201606300036472016063000364720160630003647201806221258282018062212582820180622125828201804050656582018040506565820180405065658  JA  ARFMdecpA19c                                                                20160626093505  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20160626003526  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20160626003526  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20160626003527  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20160626003527  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20160626003527  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20160626003527  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20160626003527  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20160626003528  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20160626003528                      G�O�G�O�G�O�                JA  ARUP                                                                        20160626011734                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20160626153358  CV  JULD            G�O�G�O�F���                JM  ARCAJMQC2.0                                                                 20160629153647  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20160629153647  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404215658  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622035828  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200116201515                      G�O�G�O�G�O�                