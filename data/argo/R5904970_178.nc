CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-24T14:15:35Z creation      
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
resolution        =���   axis      Z        �  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  A8   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C,   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  T�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \x   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ^l   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  f4   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  h(   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  o�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  w�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  y�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �t   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �h   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �0   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �`   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �`   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �`   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �`   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �    HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20181024141535  20181024141535  5904970 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6785                            2B  A   APEX                            7726                            111215                          846 @���?�]1   @��Q��b@85?|�h�dƧ1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �B   B   B   @,��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B��B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@�CB  CD  CF  CG�fCJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`�Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cy�fC|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  D   C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#�fD$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?�fD@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DKfDK�fDL  DL� DM  DM� DNfDN� DO  D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� DwٚDy�)D�@�D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @.{@���@���A Q�A Q�A@Q�A`Q�A�(�A�(�A�(�A�(�A�(�A�(�A�(�A�(�B {B{B{B�B {B({B0{B8{B@{BH{BP{BX{B`{Bh{Bp{Bx{B�
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
=B�=pB�
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
CCCCCCCCCCC C"C$C&C(C*C,C.C0C2C4C6C8C:C<C>C@�CBCDCFCG�CJCLCNCPCRCTCVCXCZC\C^C`�CbCdCfChCjClCnCpCrCtCvCxCy�C|C~C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C���C��C��C��C��C��D HC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C���C��C��C��C��C��D HD �HDHD�HDHD�HDHD�HDHD�HDHD�HDHD��DHD�HDHD�HD	HD	�HD
HD
�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HD HD �HD!HD!�HD"HD"�HD#HD#��D$HD$�HD%HD%�HD&HD&�HD'HD'�HD(HD(�HD)HD)�HD*HD*�HD+HD+�HD,HD,�HD-HD-�HD.HD.�HD/HD/�HD0HD0�HD1HD1�HD2HD2�HD3HD3�HD4HD4�HD5HD5�HD6HD6�HD7HD7�HD8HD8�HD9HD9�HD:HD:�HD;HD;�HD<HD<�HD=HD=�HD>HD>�HD?HD?��D@HD@�HDAHDA�HDBHDB�HDCHDC�HDDHDD�HDEHDE�HDFHDF�HDGHDG�HDHHDH�HDIHDI�HDJHDJ�HDK�DK��DLHDL�HDMHDM�HDN�DN�HDOHD[HD[�HD\HD\�HD]HD]�HD^HD^�HD_HD_�HD`HD`�HDaHDa�HDbHDb�HDcHDc�HDdHDd�HDeHDe�HDfHDf�HDgHDg�HDhHDh�HDiHDi�HDjHDj�HDkHDk�HDlHDl�HDmHDm�HDnHDn�HDoHDo�HDpHDp�HDqHDq�HDrHDr�HDsHDs�HDtHDt�HDuHDu�HDvHDv�HDwHDw�HDw��Dy�qD�AHD���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�-A�+A�/A�/A�1'A�-A�-A�+A�+A�/A�-A�-A�1'A�33A�-A�+A�"�A���A�\)A���AǾwAǲ-Aǧ�Aǝ�AǛ�AǙ�AǙ�AǙ�Aǟ�Aǡ�Aǟ�Aǥ�Aǰ!AǸRAǸRAǶFAǰ!Aǩ�Aǣ�Aǡ�Aǡ�Aǣ�Aǟ�AǅA�r�A�XA��A�  A�VAĉ7A�ZA�JA�ƨA�bNA��A���A��A���A��\A���A�ffA�C�A�t�A��mA�=qA�`BA��`A�dZA���A���A�A�A�E�A���A���A���A�A�A���A���A�dZA���A���A�|�A�t�A�n�A�hsA�bNA�`BA�^5A�A�A��`A���A���A��mA�^5A�VA���A�"�A���A�C�A��A��\A��A�1A�ffA��#A�p�A�(�A��A�A��A�dZA�VA���A��A��A��TA���A�1'A�XA�1A���A�?}A�v�A�1A�`BA�7LA��A��A�hsA�+A��A�r�A�hA~��A~$�A}��Az-Ax��Av�ArZAp9XAo�AnAlE�Ajn�AhffAe��Ac�Ac?}Aa��A^��AZ~�AY��AY?}AXĜAW�AS/AQ�wAP9XAO?}AL��AL��AL�RALAJ��AI/AF5?AE`BAD�9AC;dAB�AA?}A?t�A>��A=��A=S�A<v�A;��A;��A;�hA:Q�A9"�A8ȴA8~�A8=qA7hsA6^5A5%A4-A3K�A2^5A1��A1C�A/��A-l�A,Q�A*�A*9XA)��A(~�A'\)A&��A&r�A&1A%�FA$�\A#XA#33A"bNA!G�A  �A�DA33AhsA�A�\AE�A9XA33A�yA�A�A�AA�A1A�AƨA�!A+A5?A|�A�RAA�A�A/A
jA	�AE�A&�A�\A5?A�A�HA�mA��A�A��A��Ap�AdZAO�A Ĝ@�n�@��T@��A�A/A
jA	�AE�A&�A�\A5?A�A�HA�mA��A�A��A��Ap�AdZAO�A Ĝ@�n�@��T@��@�Q�@�S�@��R@��#@�(�@�@�?}@��`@���@���@�9X@�@�X@���@�u@�E�@�ƨ@���@�u@�Ĝ@�z�@޸R@��@�p�@���@��m@�X@���@�33@֧�@Ցh@�Ĝ@�=q@���@���@�v�@�{@�`B@�z�@��@��@�v�@�V@���@���@�  @�l�@�ff@���@�M�@��H@�t�@�A�@�b@� �@�b@ǥ�@Ə\@�(�@���@�33@�@���@���@���@��H@��#@��/@�(�@��@���@���@�r�@���@�O�@�Z@�ff@���@�5?@��@�Ĝ@��@��u@��`@��j@� �@�dZ@�{@�bN@�b@��m@���@�l�@�o@���@���@���@�=q@��@�?}@�G�@�7L@��@���@�K�@��!@��@��@��T@���@���@��7@�X@�7L@��j@�Q�@�9X@��
@�
=@��H@���@�n�@���@�&�@��`@��j@�Ĝ@��u@�1'@�+@�@�"�@�l�@�33@�33@�o@�;d@�K�@�o@���@���@��+@�^5@�E�@��T@�@���@�?}@���@��D@� �@�  @��;@���@���@�|�@���@�
=@��P@��
@�j@�bN@�I�@��w@��P@�33@�5?@�hs@��F@���@��@�hs@�X@�p�@���@���@�o@���@���@���@��@��#@���@�S�@��@���@��7@�x�@��^@��@�=q@�{@���@�I�@�bN@�(�@�b@��P@��H@���@�~�@�V@���@��^@��h@�O�@��j@��w@�C�@�dZ@�|�@��F@�ƨ@�t�@�
=@��@�o@��@w��@bZ�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�-A�+A�/A�/A�1'A�-A�-A�+A�+A�/A�-A�-A�1'A�33A�-A�+A�"�A���A�\)A���AǾwAǲ-Aǧ�Aǝ�AǛ�AǙ�AǙ�AǙ�Aǟ�Aǡ�Aǟ�Aǥ�Aǰ!AǸRAǸRAǶFAǰ!Aǩ�Aǣ�Aǡ�Aǡ�Aǣ�Aǟ�AǅA�r�A�XA��A�  A�VAĉ7A�ZA�JA�ƨA�bNA��A���A��A���A��\A���A�ffA�C�A�t�A��mA�=qA�`BA��`A�dZA���A���A�A�A�E�A���A���A���A�A�A���A���A�dZA���A���A�|�A�t�A�n�A�hsA�bNA�`BA�^5A�A�A��`A���A���A��mA�^5A�VA���A�"�A���A�C�A��A��\A��A�1A�ffA��#A�p�A�(�A��A�A��A�dZA�VA���A��A��A��TA���A�1'A�XA�1A���A�?}A�v�A�1A�`BA�7LA��A��A�hsA�+A��A�r�A�hA~��A~$�A}��Az-Ax��Av�ArZAp9XAo�AnAlE�Ajn�AhffAe��Ac�Ac?}Aa��A^��AZ~�AY��AY?}AXĜAW�AS/AQ�wAP9XAO?}AL��AL��AL�RALAJ��AI/AF5?AE`BAD�9AC;dAB�AA?}A?t�A>��A=��A=S�A<v�A;��A;��A;�hA:Q�A9"�A8ȴA8~�A8=qA7hsA6^5A5%A4-A3K�A2^5A1��A1C�A/��A-l�A,Q�A*�A*9XA)��A(~�A'\)A&��A&r�A&1A%�FA$�\A#XA#33A"bNA!G�A  �A�DA33AhsA�A�\AE�A9XA33A�yA�A�A�AA�A1A�AƨA�!A+A5?A|�A�RAA�A�A/A
jA	�AE�A&�A�\A5?A�A�HA�mA��A�A��A��Ap�AdZAO�A Ĝ@�n�@��T@��A�A/A
jA	�AE�A&�A�\A5?A�A�HA�mA��A�A��A��Ap�AdZAO�A Ĝ@�n�@��T@��@�Q�@�S�@��R@��#@�(�@�@�?}@��`@���@���@�9X@�@�X@���@�u@�E�@�ƨ@���@�u@�Ĝ@�z�@޸R@��@�p�@���@��m@�X@���@�33@֧�@Ցh@�Ĝ@�=q@���@���@�v�@�{@�`B@�z�@��@��@�v�@�V@���@���@�  @�l�@�ff@���@�M�@��H@�t�@�A�@�b@� �@�b@ǥ�@Ə\@�(�@���@�33@�@���@���@���@��H@��#@��/@�(�@��@���@���@�r�@���@�O�@�Z@�ff@���@�5?@��@�Ĝ@��@��u@��`@��j@� �@�dZ@�{@�bN@�b@��m@���@�l�@�o@���@���@���@�=q@��@�?}@�G�@�7L@��@���@�K�@��!@��@��@��T@���@���@��7@�X@�7L@��j@�Q�@�9X@��
@�
=@��H@���@�n�@���@�&�@��`@��j@�Ĝ@��u@�1'@�+@�@�"�@�l�@�33@�33@�o@�;d@�K�@�o@���@���@��+@�^5@�E�@��T@�@���@�?}@���@��D@� �@�  @��;@���@���@�|�@���@�
=@��P@��
@�j@�bN@�I�@��w@��P@�33@�5?@�hs@��F@���@��@�hs@�X@�p�@���@���@�o@���@���@���@��@��#@���@�S�@��@���@��7@�x�@��^@��@�=q@�{@���@�I�@�bN@�(�@�b@��P@��H@���@�~�@�V@���@��^@��h@�O�@��j@��w@�C�@�dZ@�|�@��F@�ƨ@�t�@�
=@��@�o@��@w��@bZ�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B  BJB/BM�BO�BQ�BR�BR�BR�BR�BR�BR�BVBXBXBZB\)B^5B_;B^5B^5B\)B[#B[#B[#B\)B[#BW
BS�BP�BJ�BQ�B\)Bn�B|�B�B��B�B�FB�FB�LB�LB�XB�}B��B��B��B��B�/B�B�B��B��B��B��BƨBB�?B�B��B��B��B��B�DBs�BiyBhsBhsBhsBgmBffBcTB[#B=qB33B/B!�B�B\BPB	7B��B��B�#B�}B�wB��B�{B�DB�By�Bt�Bm�BcTBW
BK�BF�BC�B?}B9XB5?B1'B/B(�B%�B�B
��B
�B
�`B
�B
��B
�jB
�!B
��B
��B
��B
��B
�hB
�1B
�B
{�B
bNB
M�B
8RB
{B
  B	��B	�B	�B	��B	�XB	��B	�uB	�PB	�B	q�B	M�B	H�B	C�B	>wB	8RB	�B	hB	%B	B��B��B��B��B��B�B�5B�`B�fB�NB�B�sB�5B�B��B��B��B��B��B�
B�B��B��B��B��B��B��B�wB�FB�B��B��B��B��B��B��B�\B�=B�7B�B�B�%B�+B�7B�1B�=B�=B�1B�1B�B�B� B|�Bz�Bw�Bv�Bu�Bs�Bm�Bk�BjBgmBffBe`BdZBdZBdZBcTB`BB\)BZBXBVBT�BN�BL�BI�BD�B@�B=qB;dB9XB5?B49B49B49B49B6FB6FB6FB6FB9XB9XB6FB5?BT�BN�BL�BI�BD�B@�B=qB;dB9XB5?B49B49B49B49B6FB6FB6FB6FB9XB9XB6FB5?B2-B0!B/B.B-B%�B#�B!�B!�B �B!�B!�B(�B/B0!B,B!�B�B�B%�B!�B�B�B�B�B�B$�B&�B$�B$�B&�B&�B+B)�B)�B)�B)�B)�B+B+B,B-B-B.B0!B49B7LB=qBD�BL�B`BBjBy�B|�B� B�B�B�1B�1B�7B�bB��B��B��B��B��B��B��B��B��B��B��B��B�{B�\B�\B�PB�%B�B�1B�VB�hB��B��B��B��B��B��B�B�B�B�B�'B�RB�wBÖBBƨBǮBȴB��B�
B�B�)B�B�B�B�#B�#B�#B�)B�/B�5B�;B�NB�ZB�`B�mB�B�B�B�B�B�B��B	  B	B	B	B	%B		7B	PB	uB	�B	 �B	&�B	'�B	)�B	,B	-B	.B	/B	0!B	0!B	33B	33B	49B	6FB	7LB	9XB	;dB	=qB	>wB	B�B	C�B	H�B	T�B	e`B	�B	�!B	�^B	�qB	�wB	�qB	�qB	�jB	�^B	�^B	�LB	�9B	�'B	�'B	�3B	�9B	�FB	�jB	��B	ɺB	��B	��B	��B	��B	��B	��B	ȴB	ƨB	ƨB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	ȴB	ǮB	ȴB	ȴB	ɺB	��B	��B	ɺB	ɺB	��B	�oB	��B
�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B  BJB/BM�BO�BQ�BR�BR�BR�BR�BR�BR�BVBXBXBZB\)B^5B_;B^5B^5B\)B[#B[#B[#B\)B[#BW
BS�BP�BJ�BQ�B\)Bn�B|�B�B��B�B�FB�FB�LB�LB�XB�}B��B��B��B��B�/B�B�B��B��B��B��BƨBB�?B�B��B��B��B��B�DBs�BiyBhsBhsBhsBgmBffBcTB[#B=qB33B/B!�B�B\BPB	7B��B��B�#B�}B�wB��B�{B�DB�By�Bt�Bm�BcTBW
BK�BF�BC�B?}B9XB5?B1'B/B(�B%�B�B
��B
�B
�`B
�B
��B
�jB
�!B
��B
��B
��B
��B
�hB
�1B
�B
{�B
bNB
M�B
8RB
{B
  B	��B	�B	�B	��B	�XB	��B	�uB	�PB	�B	q�B	M�B	H�B	C�B	>wB	8RB	�B	hB	%B	B��B��B��B��B��B�B�5B�`B�fB�NB�B�sB�5B�B��B��B��B��B��B�
B�B��B��B��B��B��B��B�wB�FB�B��B��B��B��B��B��B�\B�=B�7B�B�B�%B�+B�7B�1B�=B�=B�1B�1B�B�B� B|�Bz�Bw�Bv�Bu�Bs�Bm�Bk�BjBgmBffBe`BdZBdZBdZBcTB`BB\)BZBXBVBT�BN�BL�BI�BD�B@�B=qB;dB9XB5?B49B49B49B49B6FB6FB6FB6FB9XB9XB6FB5?BT�BN�BL�BI�BD�B@�B=qB;dB9XB5?B49B49B49B49B6FB6FB6FB6FB9XB9XB6FB5?B2-B0!B/B.B-B%�B#�B!�B!�B �B!�B!�B(�B/B0!B,B!�B�B�B%�B!�B�B�B�B�B�B$�B&�B$�B$�B&�B&�B+B)�B)�B)�B)�B)�B+B+B,B-B-B.B0!B49B7LB=qBD�BL�B`BBjBy�B|�B� B�B�B�1B�1B�7B�bB��B��B��B��B��B��B��B��B��B��B��B��B�{B�\B�\B�PB�%B�B�1B�VB�hB��B��B��B��B��B��B�B�B�B�B�'B�RB�wBÖBBƨBǮBȴB��B�
B�B�)B�B�B�B�#B�#B�#B�)B�/B�5B�;B�NB�ZB�`B�mB�B�B�B�B�B�B��B	  B	B	B	B	%B		7B	PB	uB	�B	 �B	&�B	'�B	)�B	,B	-B	.B	/B	0!B	0!B	33B	33B	49B	6FB	7LB	9XB	;dB	=qB	>wB	B�B	C�B	H�B	T�B	e`B	�B	�!B	�^B	�qB	�wB	�qB	�qB	�jB	�^B	�^B	�LB	�9B	�'B	�'B	�3B	�9B	�FB	�jB	��B	ɺB	��B	��B	��B	��B	��B	��B	ȴB	ƨB	ƨB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	ȴB	ǮB	ȴB	ȴB	ɺB	��B	��B	ɺB	ɺB	��B	�oB	��B
�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.02 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181024141535                              AO  ARCAADJP                                                                    20181024141535    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181024141535  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181024141535  QCF$                G�O�G�O�G�O�4000            