CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-24T14:15:37Z creation      
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
resolution        =���   axis      Z           9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���        C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   K�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���        M�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        U�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ^    TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        `   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   h(   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        j0   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        rP   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   zp   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        |x   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20181024141537  20181024141537  5904970 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6785                            2B  A   APEX                            7726                            111215                          846 @��$�L�t1   @��%b���@7�z�H�c�r� Ĝ1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �B   B   B   @�  @�33A   AffA@  A`  A�  A�  A�  A�33A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C�fC�fC  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  D   D � D  D� D  D� D  D� D��Dy�D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:�fD;fD;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA�fDBfDB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQy�DQ��DR� DSfDS� DT  DT� DU  DU� DV  DV� DV��DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\fD\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dw�fDy��D�MqD��H1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@���@��
A Q�A�RA@Q�A`Q�A�(�A�(�A�(�A�\)A�(�A�(�A�(�A�(�B {B{B{B{B {B({B0{B8{B@{BH{BP{BX{B`{Bh{Bp{Bx{B�
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
=C CCCCC
CCCCCCCC�C�CC C"C$C&C(C*C,C.C0C2C4C6C8C:C<C>C@CBCDCFCHCJCLCNCPCRCTCVCXCZC\C^C`CbCdCfChCjClCnCpCrCtCvCxCzC|C~C��C��C��C�\C��C��C��C��C��C��C��C��C��C��C��C��CtCvCxCzC|C~C��C��C��C�\C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�\C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C���C���C���C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C���C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C���C��D HD �HDHD�HDHD�HDHD�HD��Dz�DHD�HDHD�HDHD�HDHD�HD	HD	�HD
HD
�HDHD�HDHD�HDHDz�DHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HD HD �HD!HD!�HD"HD"�HD#HD#�HD$HD$�HD%HD%�HD&HD&�HD'HD'�HD(HD(�HD)HD)�HD*HD*�HD+HD+�HD,HD,�HD-HD-�HD.HD.�HD/HD/�HD0HD0�HD1HD1�HD2HD2�HD3HD3�HD4HD4�HD5HD5�HD6HD6�HD7HD7�HD8HD8�HD9HD9�HD:HD:��D;�D;�HD<HD<�HD=HD=�HD>HD>�HD?HD?�HD@HD@�HDAHDA��DB�DB�HDCHDC�HDDHDD�HDEHDE�HDFHDF�HDGHDG�HDHHDH�HDIHDI�HDJHDJ�HDKHDK�HDLHDL�HDMHDM�HDNHDN�HDOHDO�HDPHDP�HDQHDQz�DQ��DR�HDS�DS�HDTHDT�HDUHDU�HDVHDV�HDV��DW�HDXHDX�HDYHDY�HDZHDZ�HD[HD[�HD\�D\�HD]HD]�HD^HD^�HD_HD_�HD`HD`�HDaHDa�HDbHDb�HDcHDc�HDdHDd�HDeHDe�HDfHDf�HDgHDg�HDhHDh�HDiHDi�HDjHDj�HDkHDk�HDlHDl�HDmHDm�HDnHDn�HDoHDo�HDpHDp�HDqHDq�HDrHDr�HDsHDs�HDtHDt�HDuHDu�HDvHDv�HDwHDw�HDw�Dy��D�ND���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�&�A�"�A��A��A�"�A�$�A�(�A�-A�-A�=qA�=qA�A�A�9XA�9XA�A�A�E�A�C�A�E�A�C�A�A�A�?}A�?}A�A�A�?}A���AËDA�S�A��A�
=A�  A��A��A��yA��`A��;A���A¶FA�A�r�A���A�bNA�l�A�ĜA�bNA�1A��RA�{A���A���A�/A�bNA��DA��A�VA��A�oA�bA��A�ĜA��A�O�A���A���A�ƨA�E�A��RA�ffA�S�A��yA���A�I�A�O�A�  A�bNA��+A�M�A��A�A��A���A���A��`A�ĜA�{A�1'A�Q�A���A���A�G�A�C�A��#A�z�A�x�A� �A�oA��yA��A���A���A�"�A�Q�A~{A{�wA{K�Az�yAy��Ax�9Ax^5Ax �Aw�mAwl�At��Aq��Ap$�Ao`BAmO�AlbNAjVAi�PAh��Ag�;AgoAe�Ac�7AbbNAaG�A_�A^=qA\�A[�#AZ��AX��AWhsAU�AT�yATE�AT1AS+AQ�^AP1AN{AM�PAg�;AgoAe�Ac�7AbbNAaG�A_�A^=qA\�A[�#AZ��AX��AWhsAU�AT�yATE�AT1AS+AQ�^AP1AN{AM�PAMx�AM|�AMO�ALz�AK&�AJ�!AI��AG�TAF�AF�+AF5?AE�;AE��AD�jAC"�AB$�AA�A@�A@�A?��A?�-A?\)A>��A<�`A<ffA<=qA<$�A;��A:�A9�-A9/A8VA7?}A6�HA61A3�mA21'A1��A0�A0^5A/��A-�
A+G�A*bA* �A)�A)&�A(bNA'�FA&�`A#��A!��A ��A�A�A
=A��A�A��A�PA�^A�HA�DAI�A�-A �AA�TA33A�9A�DA�mAoA��A��A�A�jA��A/A
�A
=qA�AM�A��A��A �A�A��A��A�7A=qAO�A Z@���@���@�O�@�A�@�&�@�9X@�"�@���@@�&�@�I�@�@땁@��@�^@��@�&�@���@�@�p�@㕁@�=q@�J@��H@�"�@��/@��@�z�@���@ݙ�@��@ݩ�@�"�@�E�@�V@ם�@�x�@��#@�&�@�O�@�O�@�G�@���@ύP@�ȴ@�^5@��#@��@̛�@�A�@ʟ�@ʟ�@Ɂ@��H@��T@�p�@���@��@ǥ�@ǅ@���@��@�^5@���@���@��@�bN@��;@�"�@�ff@��@�@�?}@��@���@�x�@��/@��w@�@��R@�v�@�M�@�J@��-@��@�dZ@��y@���@�~�@�M�@�-@��@�X@�&�@��j@�bN@���@�S�@��\@��T@��h@��@�x�@�9X@�33@��P@��@��H@��@�=q@�{@��T@��-@��`@��m@��F@��@�C�@�$�@�%@���@�Z@� �@���@�l�@�@��R@���@��R@�-@���@��@��/@��@��@���@�|�@�C�@��\@�{@��T@�O�@�&�@�V@���@���@��@�bN@�Z@���@�l�@�33@�+@��@��!@��\@��!@���@�V@�{@��^@�O�@��/@���@�bN@��@�Z@�O�@�X@�`B@���@�ff@���@��@�9X@��w@�1@��w@�;d@�@��y@��!@��@�"�@���@�o@�S�@��w@�  @���@�J@��@���@�5?@��@���@�  @��P@��@��@��@�hs@�p�@�X@���@��^@��h@��m@��@���@�ff@���@�`B@�%@���@��u@�Q�@�(�@��@���@��;@���@��;@��@���@�I�@���@���@��h@�`B@�x�@�O�@�V@���@�Q�@��@��@��@��F@�t�@��R@��!@��R@��\@�v�@�~�@��+@�v�@��@�`B@�7L@�V@��@��/@�Ĝ@���@��@�r�@��@��;@��f@{�k@g��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�&�A�"�A��A��A�"�A�$�A�(�A�-A�-A�=qA�=qA�A�A�9XA�9XA�A�A�E�A�C�A�E�A�C�A�A�A�?}A�?}A�A�A�?}A���AËDA�S�A��A�
=A�  A��A��A��yA��`A��;A���A¶FA�A�r�A���A�bNA�l�A�ĜA�bNA�1A��RA�{A���A���A�/A�bNA��DA��A�VA��A�oA�bA��A�ĜA��A�O�A���A���A�ƨA�E�A��RA�ffA�S�A��yA���A�I�A�O�A�  A�bNA��+A�M�A��A�A��A���A���A��`A�ĜA�{A�1'A�Q�A���A���A�G�A�C�A��#A�z�A�x�A� �A�oA��yA��A���A���A�"�A�Q�A~{A{�wA{K�Az�yAy��Ax�9Ax^5Ax �Aw�mAwl�At��Aq��Ap$�Ao`BAmO�AlbNAjVAi�PAh��Ag�;AgoAe�Ac�7AbbNAaG�A_�A^=qA\�A[�#AZ��AX��AWhsAU�AT�yATE�AT1AS+AQ�^AP1AN{AM�PAg�;AgoAe�Ac�7AbbNAaG�A_�A^=qA\�A[�#AZ��AX��AWhsAU�AT�yATE�AT1AS+AQ�^AP1AN{AM�PAMx�AM|�AMO�ALz�AK&�AJ�!AI��AG�TAF�AF�+AF5?AE�;AE��AD�jAC"�AB$�AA�A@�A@�A?��A?�-A?\)A>��A<�`A<ffA<=qA<$�A;��A:�A9�-A9/A8VA7?}A6�HA61A3�mA21'A1��A0�A0^5A/��A-�
A+G�A*bA* �A)�A)&�A(bNA'�FA&�`A#��A!��A ��A�A�A
=A��A�A��A�PA�^A�HA�DAI�A�-A �AA�TA33A�9A�DA�mAoA��A��A�A�jA��A/A
�A
=qA�AM�A��A��A �A�A��A��A�7A=qAO�A Z@���@���@�O�@�A�@�&�@�9X@�"�@���@@�&�@�I�@�@땁@��@�^@��@�&�@���@�@�p�@㕁@�=q@�J@��H@�"�@��/@��@�z�@���@ݙ�@��@ݩ�@�"�@�E�@�V@ם�@�x�@��#@�&�@�O�@�O�@�G�@���@ύP@�ȴ@�^5@��#@��@̛�@�A�@ʟ�@ʟ�@Ɂ@��H@��T@�p�@���@��@ǥ�@ǅ@���@��@�^5@���@���@��@�bN@��;@�"�@�ff@��@�@�?}@��@���@�x�@��/@��w@�@��R@�v�@�M�@�J@��-@��@�dZ@��y@���@�~�@�M�@�-@��@�X@�&�@��j@�bN@���@�S�@��\@��T@��h@��@�x�@�9X@�33@��P@��@��H@��@�=q@�{@��T@��-@��`@��m@��F@��@�C�@�$�@�%@���@�Z@� �@���@�l�@�@��R@���@��R@�-@���@��@��/@��@��@���@�|�@�C�@��\@�{@��T@�O�@�&�@�V@���@���@��@�bN@�Z@���@�l�@�33@�+@��@��!@��\@��!@���@�V@�{@��^@�O�@��/@���@�bN@��@�Z@�O�@�X@�`B@���@�ff@���@��@�9X@��w@�1@��w@�;d@�@��y@��!@��@�"�@���@�o@�S�@��w@�  @���@�J@��@���@�5?@��@���@�  @��P@��@��@��@�hs@�p�@�X@���@��^@��h@��m@��@���@�ff@���@�`B@�%@���@��u@�Q�@�(�@��@���@��;@���@��;@��@���@�I�@���@���@��h@�`B@�x�@�O�@�V@���@�Q�@��@��@��@��F@�t�@��R@��!@��R@��\@�v�@�~�@��+@�v�@��@�`B@�7L@�V@��@��/@�Ĝ@���@��@�r�@��@��;@��f@{�k@g��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Bo�Bp�Bp�Bo�Bo�Bo�Bo�Bo�Bo�Bo�Bo�Bp�Bo�Bo�Bp�Bq�Br�Br�Br�Bq�Bq�Bt�B{�B~�B��B��B��B�/B�;B�NB�ZB�`B�`B�fB�fB�mB�sB�yB�B�fB�BB�fB�B�B�B��BDB+B(�B8RBE�BE�B6FB.B�BB�B�TB��B�^B�B��B��B�=B�B� B{�Bu�Be`B\)BJ�B<jB7LB0!B�B��B�B�)B��B��B�B��B��B�{B�%Bk�BP�B,BB
��B
�B
�B
�TB
�)B
�#B
�B
��B
ɺB
�wB
�9B
��B
��B
�%B
�B
� B
x�B
o�B
l�B
iyB
ffB
bNB
R�B
>wB
2-B
)�B
�B
oB
B	��B	��B	�B	�`B	�5B	��B	�}B	�RB	�B	��B	��B	�hB	�=B	}�B	t�B	k�B	jB	e`B	cTB	`BB	VB	N�B	A�B	>wB	�B	�`B	�5B	��B	�}B	�RB	�B	��B	��B	�hB	�=B	}�B	t�B	k�B	jB	e`B	cTB	`BB	VB	N�B	A�B	?}B	?}B	?}B	@�B	@�B	=qB	;dB	6FB	/B	'�B	$�B	"�B	 �B	�B	�B	�B	{B	oB	\B	PB	
=B	1B	%B	B��B��B��B��B��B��B�B�mB�HB�B�B��BB�9B�!B�B�B��B��B�oB�=B�{B�uB�bB�VB�JB�1B�B|�Bu�Bq�Bp�Bp�Br�Bp�BjBe`BbNB[#BYBYB[#BVBP�BK�BJ�BI�BH�BG�BF�BG�BG�BG�BD�BA�B>wB<jB9XB6FB5?B49B2-B2-B6FB8RB33B1'B5?B2-B.B)�B$�B"�B#�B"�B!�B!�B �B"�B!�B!�B �B �B"�B"�B$�B%�B%�B"�B#�B"�B%�B%�B2-B9XB:^B2-B/B49B;dB>wBD�B>wB>wB?}B>wB<jB8RB5?B5?B5?B5?B49B9XB?}B?}B?}B>wB=qB>wBA�BC�BE�BC�BD�BF�BH�BZBcTBbNB`BB^5BbNBe`BhsBgmBe`BdZBcTBhsBjBk�Bl�Bo�Bq�Br�Br�Bq�Bq�Br�Bs�Bs�Bs�Bs�Bu�By�B{�B}�B�B�%B�+B�=B�bB�hB�{B��B��B��B��B��B��B��B��B�B�B�B�LB�FB�FB�XB�wB�qB�wB��B�wB��BŢBǮBɺB��B��B��B��B��B�
B�B�
B�B�#B�#B�#B�/B�5B�NB�mB�B�B�B��B��B	  B	%B	1B		7B		7B	DB	PB	VB	\B	uB	{B	�B	�B	!�B	#�B	'�B	-B	0!B	1'B	49B	9XB	<jB	>wB	C�B	E�B	K�B	Q�B	\)B	^5B	_;B	dZB	hsB	k�B	k�B	ffB	e`B	iyB	iyB	hsB	jB	jB	l�B	n�B	t�B	w�B	{�B	�B	�B	�PB	�=B	�1B	�7B	�DB	�VB	�oB	�hB	�PB	�DB	�VB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�9B	�RB	�}B	ĜB	ŢB	ĜB	ŢB	ŢB	ĜB	ŢB	ŢB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�#B	�#B	�#B	�#B	�)B	�)B	�/B	�)B	�5B	ߤB	�%B
�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111Bo�Bp�Bp�Bo�Bo�Bo�Bo�Bo�Bo�Bo�Bo�Bp�Bo�Bo�Bp�Bq�Br�Br�Br�Bq�Bq�Bt�B{�B~�B��B��B��B�/B�;B�NB�ZB�`B�`B�fB�fB�mB�sB�yB�B�fB�BB�fB�B�B�B��BDB+B(�B8RBE�BE�B6FB.B�BB�B�TB��B�^B�B��B��B�=B�B� B{�Bu�Be`B\)BJ�B<jB7LB0!B�B��B�B�)B��B��B�B��B��B�{B�%Bk�BP�B,BB
��B
�B
�B
�TB
�)B
�#B
�B
��B
ɺB
�wB
�9B
��B
��B
�%B
�B
� B
x�B
o�B
l�B
iyB
ffB
bNB
R�B
>wB
2-B
)�B
�B
oB
B	��B	��B	�B	�`B	�5B	��B	�}B	�RB	�B	��B	��B	�hB	�=B	}�B	t�B	k�B	jB	e`B	cTB	`BB	VB	N�B	A�B	>wB	�B	�`B	�5B	��B	�}B	�RB	�B	��B	��B	�hB	�=B	}�B	t�B	k�B	jB	e`B	cTB	`BB	VB	N�B	A�B	?}B	?}B	?}B	@�B	@�B	=qB	;dB	6FB	/B	'�B	$�B	"�B	 �B	�B	�B	�B	{B	oB	\B	PB	
=B	1B	%B	B��B��B��B��B��B��B�B�mB�HB�B�B��BB�9B�!B�B�B��B��B�oB�=B�{B�uB�bB�VB�JB�1B�B|�Bu�Bq�Bp�Bp�Br�Bp�BjBe`BbNB[#BYBYB[#BVBP�BK�BJ�BI�BH�BG�BF�BG�BG�BG�BD�BA�B>wB<jB9XB6FB5?B49B2-B2-B6FB8RB33B1'B5?B2-B.B)�B$�B"�B#�B"�B!�B!�B �B"�B!�B!�B �B �B"�B"�B$�B%�B%�B"�B#�B"�B%�B%�B2-B9XB:^B2-B/B49B;dB>wBD�B>wB>wB?}B>wB<jB8RB5?B5?B5?B5?B49B9XB?}B?}B?}B>wB=qB>wBA�BC�BE�BC�BD�BF�BH�BZBcTBbNB`BB^5BbNBe`BhsBgmBe`BdZBcTBhsBjBk�Bl�Bo�Bq�Br�Br�Bq�Bq�Br�Bs�Bs�Bs�Bs�Bu�By�B{�B}�B�B�%B�+B�=B�bB�hB�{B��B��B��B��B��B��B��B��B�B�B�B�LB�FB�FB�XB�wB�qB�wB��B�wB��BŢBǮBɺB��B��B��B��B��B�
B�B�
B�B�#B�#B�#B�/B�5B�NB�mB�B�B�B��B��B	  B	%B	1B		7B		7B	DB	PB	VB	\B	uB	{B	�B	�B	!�B	#�B	'�B	-B	0!B	1'B	49B	9XB	<jB	>wB	C�B	E�B	K�B	Q�B	\)B	^5B	_;B	dZB	hsB	k�B	k�B	ffB	e`B	iyB	iyB	hsB	jB	jB	l�B	n�B	t�B	w�B	{�B	�B	�B	�PB	�=B	�1B	�7B	�DB	�VB	�oB	�hB	�PB	�DB	�VB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�9B	�RB	�}B	ĜB	ŢB	ĜB	ŢB	ŢB	ĜB	ŢB	ŢB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�#B	�#B	�#B	�#B	�)B	�)B	�/B	�)B	�5B	ߤB	�%B
�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.02 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181024141537                              AO  ARCAADJP                                                                    20181024141537    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181024141537  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181024141537  QCF$                G�O�G�O�G�O�4000            