CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-24T14:14:10Z creation      
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
_FillValue                 �  A4   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C(   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  T�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \h   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ^\   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  f    TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  h   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  o�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  w�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  y�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �T   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �H   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �<   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �<   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �<   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �<   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �h   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �l   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �p   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �t   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �x   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20181024141410  20181024141410  5904969 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               4A   AO  6784                            2B  A   APEX                            7725                            111215                          846 @��$ua�H1   @��$��\6@29������c��x���1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      4A   A   A   @�  @�  A   A   A@  A`  A�  A�  A�  A�  A���A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C�C  C   C"  C$  C&  C(  C*  C,  C-�fC0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1�fD2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DN��DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\fD\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dpy�Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt��Du� Dv  Dv� Dw  Dwl�Dy�\D�N�D�z�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @���@���A Q�A Q�A@Q�A`Q�A�(�A�(�A�(�A�(�A���A�(�A�(�A�(�B {B{B{B{B {B({B0{B8{B@{BH{BP{BX{B`{Bh{Bp{Bx{B�
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
CCCCCCCCC�CC C"C$C&C(C*C,C-�C0C2C4C6C8C:C<C>C@CBCDCFCHCJCLCNCPCRCTCVCXCZC\C^C`CbCdCfChCjClCnCpCrCtCvCxCzC|C~C��C���C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C���C��C��C��C��C��C��C�\C��C��C��C��C��C��C��C��C��C��C��C��C��C��C���C��C��C��C���C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C���C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D HD �HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HD	HD	�HD
HD
�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HD HD �HD!HD!�HD"HD"�HD#HD#�HD$HD$�HD%HD%�HD&HD&�HD'HD'�HD(HD(�HD)HD)�HD*HD*�HD+HD+�HD,HD,�HD-HD-�HD.HD.�HD/HD/�HD0HD0�HD1HD1��D2HD2�HD3HD3�HD4HD4�HD5HD5�HD6HD6�HD7HD7�HD8HD8�HD9HD9�HD:HD:�HD;HD;�HD<HD<�HD=HD=�HD>HD>�HD?HD?�HD@HD@�HDAHDA�HDBHDB�HDCHDC�HDDHDD�HDEHDE�HDFHDF�HDGHDG�HDHHDH�HDIHDI�HDJHDJ�HDKHDK�HDLHDL�HDMHDM�HDNHDN�HDN��DO�HDPHDP�HDQHDQ�HDRHDR�HDSHDS�HDTHDT�HDUHDU�HDVHDV�HDWHDW�HDXHDX�HDYHDY�HDZHDZ�HD[HD[�HD\�D\�HD]HD]�HD^HD^�HD_HD_�HD`HD`�HDaHDa�HDbHDb�HDcHDc�HDdHDd�HDeHDe�HDfHDf�HDgHDg�HDhHDh�HDiHDi�HDjHDj�HDkHDk�HDlHDl�HDmHDm�HDnHDn�HDoHDo�HDpHDpz�DqHDq�HDrHDr�HDsHDs�HDtHDt�HDt��Du�HDvHDv�HDwHDwnDy��D�O\D�{311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A߅A߇+A߅A߅A߅A߅A߇+A߇+A߅A߇+A߇+A߉7AߋDA�z�A�-Aޛ�AޅA�t�A�\)A�=qA��A���A���Aݗ�A�|�A�G�Aܡ�A�33A�~�Aڧ�A�jA�^5A���A�K�AԅA�bNA���A�n�A�E�A��;A΁A�ZA��mA�|�Ả7A��
A��A�?}A�&�A�`BAˉ7Aʡ�A��HA���A���AȬA��A��TA���A�5?A�?}AþwA�I�A��/A�dZA���A�p�A���A�C�A�p�A�G�A�bNA���A�n�A�\)A�ƨA���A��A��FA�hsA�Q�A�bNA���A�33A�(�A�oA��A��A�A�A�ȴA��
A��A���A�A�\)A�1'A�oA��PA�K�A��!A��PA�hsA�ZA���A�
=A��`A�p�A��A�5?A��;A��\A�XA�+A��A�1'A�dZA}`BAz�AwS�Au�At1Aq�wAmXAg�Ad  Ab�uAa�;A_�wA^�A\r�AW;dAVAT�uASp�AR�+AR�AQ&�AP��AP{AN�AL��ALbAK?}AI�hAI7LAH�\ADZA?�wA=/A:�`A9�mA7ƨA5��A4�uA2�A2�!A2A�A1��A0�A-XA,I�A+�hA+33A)��A(1'A&�HA&JA$I�A"�HA!��A ��A�A�
A��A�RAM�A$�A7LA�A��AA�A��Al�AbNA��A�9A
=A�HA��An�A��A�wA"�A��A�Az�AC�A
-A	XA;dA��A�mA�AA ��A ffA7LA;dA�A\)A v�@��@�;d@�@���@��@�~�@�j@�&�@�E�@�Ĝ@��`@��@��@�7L@�^@�1@�S�@��@�7L@��/@���@���@�@�/@�&�@�!@�-@�I�@�P@�n�@�=q@�h@�@�r�@�Q�@ߕ�@��@���@�7L@�?}@۝�@�S�@�\)@�ff@�Ĝ@�+@�`B@�A�@�9X@�dZ@��y@��@��H@�n�@�@��`@�  @�dZ@�;d@���@�^5@���@̃@�r�@�z�@̃@̋D@�Q�@��m@�5?@�@�%@�+@�j@� �@Ý�@��@�b@���@���@��@���@�bN@�1'@��@��w@��;@�bN@��/@�hs@��R@��@��P@��P@��@���@�1@�Q�@���@��/@��D@���@��@��@�V@�{@�E�@�-@��!@��^@��@���@�M�@�=q@�E�@�E�@�-@��7@�Ĝ@��w@�  @���@�%@��@�/@���@���@��T@��-@�hs@���@���@��D@� �@��@���@�  @��F@�|�@�"�@�ȴ@��@��^@�@���@��h@��@���@�%@��^@��@�J@�E�@��h@�p�@�@��@���@���@��7@�hs@��@��D@��m@�1@��m@��w@���@�t�@�K�@�+@�
=@�o@���@���@�@�Ĝ@��@�Z@��@�1@�  @��;@�l�@�+@�~�@�{@�`B@���@�b@���@��;@�l�@�X@��@��D@��j@�%@�G�@�X@��^@���@���@�\)@�33@��@��H@��@�5?@���@�@��^@���@�{@��@���@�r�@���@��@��H@���@��H@�v�@�ff@��R@�ff@��@���@���@��
@�K�@�=q@�`B@���@�I�@�b@�|�@���@���@��@�+@�;d@��@�v�@�-@��@�`B@��u@�Z@�Q�@�9X@��@��@��+@���@��^@��h@�`B@�7L@��`@��@�j@�I�@�(�@�1@�ƨ@�S�@���@��\@�~�@�~�@�~�@�v�@��\@��\@��+@��\@���@�V@�J@��#@���@��-@���@�$�@h(�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A߅A߇+A߅A߅A߅A߅A߇+A߇+A߅A߇+A߇+A߉7AߋDA�z�A�-Aޛ�AޅA�t�A�\)A�=qA��A���A���Aݗ�A�|�A�G�Aܡ�A�33A�~�Aڧ�A�jA�^5A���A�K�AԅA�bNA���A�n�A�E�A��;A΁A�ZA��mA�|�Ả7A��
A��A�?}A�&�A�`BAˉ7Aʡ�A��HA���A���AȬA��A��TA���A�5?A�?}AþwA�I�A��/A�dZA���A�p�A���A�C�A�p�A�G�A�bNA���A�n�A�\)A�ƨA���A��A��FA�hsA�Q�A�bNA���A�33A�(�A�oA��A��A�A�A�ȴA��
A��A���A�A�\)A�1'A�oA��PA�K�A��!A��PA�hsA�ZA���A�
=A��`A�p�A��A�5?A��;A��\A�XA�+A��A�1'A�dZA}`BAz�AwS�Au�At1Aq�wAmXAg�Ad  Ab�uAa�;A_�wA^�A\r�AW;dAVAT�uASp�AR�+AR�AQ&�AP��AP{AN�AL��ALbAK?}AI�hAI7LAH�\ADZA?�wA=/A:�`A9�mA7ƨA5��A4�uA2�A2�!A2A�A1��A0�A-XA,I�A+�hA+33A)��A(1'A&�HA&JA$I�A"�HA!��A ��A�A�
A��A�RAM�A$�A7LA�A��AA�A��Al�AbNA��A�9A
=A�HA��An�A��A�wA"�A��A�Az�AC�A
-A	XA;dA��A�mA�AA ��A ffA7LA;dA�A\)A v�@��@�;d@�@���@��@�~�@�j@�&�@�E�@�Ĝ@��`@��@��@�7L@�^@�1@�S�@��@�7L@��/@���@���@�@�/@�&�@�!@�-@�I�@�P@�n�@�=q@�h@�@�r�@�Q�@ߕ�@��@���@�7L@�?}@۝�@�S�@�\)@�ff@�Ĝ@�+@�`B@�A�@�9X@�dZ@��y@��@��H@�n�@�@��`@�  @�dZ@�;d@���@�^5@���@̃@�r�@�z�@̃@̋D@�Q�@��m@�5?@�@�%@�+@�j@� �@Ý�@��@�b@���@���@��@���@�bN@�1'@��@��w@��;@�bN@��/@�hs@��R@��@��P@��P@��@���@�1@�Q�@���@��/@��D@���@��@��@�V@�{@�E�@�-@��!@��^@��@���@�M�@�=q@�E�@�E�@�-@��7@�Ĝ@��w@�  @���@�%@��@�/@���@���@��T@��-@�hs@���@���@��D@� �@��@���@�  @��F@�|�@�"�@�ȴ@��@��^@�@���@��h@��@���@�%@��^@��@�J@�E�@��h@�p�@�@��@���@���@��7@�hs@��@��D@��m@�1@��m@��w@���@�t�@�K�@�+@�
=@�o@���@���@�@�Ĝ@��@�Z@��@�1@�  @��;@�l�@�+@�~�@�{@�`B@���@�b@���@��;@�l�@�X@��@��D@��j@�%@�G�@�X@��^@���@���@�\)@�33@��@��H@��@�5?@���@�@��^@���@�{@��@���@�r�@���@��@��H@���@��H@�v�@�ff@��R@�ff@��@���@���@��
@�K�@�=q@�`B@���@�I�@�b@�|�@���@���@��@�+@�;d@��@�v�@�-@��@�`B@��u@�Z@�Q�@�9X@��@��@��+@���@��^@��h@�`B@�7L@��`@��@�j@�I�@�(�@�1@�ƨ@�S�@���@��\@�~�@�~�@�~�@�v�@��\@��\@��+@��\@���@�V@�J@��#@���@��-@���@�$�@h(�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
|�B
|�B
� B
�B
�B
�B
�+B
�JB
�{B
�uB
��B
��B
��B
�-B
ȴB
��B
��B
��B
ƨB
��B
��B
�-B
�-B
�uB
�B
z�B
�+B
�PB
�{B
��B
�9B
�5B
�B�B7LBR�Bn�Bz�Bq�Bl�Bn�Bx�B~�B�B�B�`B��B��BBB�B'�B9XBG�BO�BcTBz�Bz�Bo�Br�Bu�Bv�Bz�Bx�By�Bx�Bx�Bw�Bl�BdZBbNBdZB`BB`BBw�B�B��B��B��B��B�JBz�BZB%B�LB��B�7Bz�Bw�Bq�B\)B@�B$�B�BB
�B
�`B
��B
�B
}�B
q�B
l�B
;dB
�B	��B	��B
B	��B	��B	�B	��B	�FB	��B	�VB	�=B	|�B	m�B	aHB	;dB	9XB	1'B	(�B	#�B	!�B	�B	�B	{B	oB		7B	B	B��B��B��B�B�B��BŢB��B�jB�FB�?B�B�B�B��B�B��B��B��B��B��B��B�hB�\B�JB�+B�B}�Bx�Bt�Bm�Bl�BiyBjBcTBaHBaHB`BB^5B^5B[#BYBW
BVBT�BT�BVBT�BR�BM�BL�BJ�BJ�BF�BD�BB�BC�BC�BD�BF�BK�BR�B^5Bp�Bu�B�B��B��B��B��B��B�bB�bB�+B�B�PB�B� B�+B�B� B~�B�B�B� B|�B~�B��B�^B�B�B�NB�#B��B��B��B�B�
B�B�)B�)B�#B�5B�/B�#B�5B�5B�HB�BB�NB�mB�yB�mB�sB�sB�sB�sB�yB�sB�yB�B�B�B��B��B��B��B��B	B	B	%B	+B	1B		7B	PB	bB	{B	�B	�B	�B	oB	DB	JB	VB	1B	B��B��B��B��B��B��B��B��B��B	B	B	JB	+B	1B	uB	 �B	+B	,B	/B	5?B	;dB	;dB	:^B	<jB	>wB	?}B	B�B	F�B	K�B	M�B	T�B	[#B	\)B	`BB	m�B	m�B	l�B	l�B	o�B	p�B	n�B	n�B	p�B	x�B	z�B	}�B	�B	�B	|�B	y�B	x�B	z�B	|�B	{�B	~�B	� B	� B	�B	�B	�B	�B	�B	�B	�B	�B	�+B	�7B	�1B	�1B	�JB	�bB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�-B	�3B	�9B	�?B	�?B	�?B	�?B	�?B	�?B	�RB	�XB	�^B	�XB	�^B	�^B	�jB	�qB	��B	��B	B	B	B	��B	�}B	�}B	�jB	�^B	�XB	�RB	�XB	��B	�}B	�qB	�wB	��B	ÖB	ŢB	ƨB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�5B	�BB	�HB	�NB	�TB	�TB	�HB	�5B	�;B	�BB	�NB	�TB	�`B	�mB	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B	��B	��B	��B	��B
  B
  B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
+B
	7B

=B

=B

=B

=B
DB
JB
VB
VB
VB
VB
VB
VB
VB
VB
PB
B
xB
-11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
|�B
|�B
� B
�B
�B
�B
�+B
�JB
�{B
�uB
��B
��B
��B
�-B
ȴB
��B
��B
��B
ƨB
��B
��B
�-B
�-B
�uB
�B
z�B
�+B
�PB
�{B
��B
�9B
�5B
�B�B7LBR�Bn�Bz�Bq�Bl�Bn�Bx�B~�B�B�B�`B��B��BBB�B'�B9XBG�BO�BcTBz�Bz�Bo�Br�Bu�Bv�Bz�Bx�By�Bx�Bx�Bw�Bl�BdZBbNBdZB`BB`BBw�B�B��B��B��B��B�JBz�BZB%B�LB��B�7Bz�Bw�Bq�B\)B@�B$�B�BB
�B
�`B
��B
�B
}�B
q�B
l�B
;dB
�B	��B	��B
B	��B	��B	�B	��B	�FB	��B	�VB	�=B	|�B	m�B	aHB	;dB	9XB	1'B	(�B	#�B	!�B	�B	�B	{B	oB		7B	B	B��B��B��B�B�B��BŢB��B�jB�FB�?B�B�B�B��B�B��B��B��B��B��B��B�hB�\B�JB�+B�B}�Bx�Bt�Bm�Bl�BiyBjBcTBaHBaHB`BB^5B^5B[#BYBW
BVBT�BT�BVBT�BR�BM�BL�BJ�BJ�BF�BD�BB�BC�BC�BD�BF�BK�BR�B^5Bp�Bu�B�B��B��B��B��B��B�bB�bB�+B�B�PB�B� B�+B�B� B~�B�B�B� B|�B~�B��B�^B�B�B�NB�#B��B��B��B�B�
B�B�)B�)B�#B�5B�/B�#B�5B�5B�HB�BB�NB�mB�yB�mB�sB�sB�sB�sB�yB�sB�yB�B�B�B��B��B��B��B��B	B	B	%B	+B	1B		7B	PB	bB	{B	�B	�B	�B	oB	DB	JB	VB	1B	B��B��B��B��B��B��B��B��B��B	B	B	JB	+B	1B	uB	 �B	+B	,B	/B	5?B	;dB	;dB	:^B	<jB	>wB	?}B	B�B	F�B	K�B	M�B	T�B	[#B	\)B	`BB	m�B	m�B	l�B	l�B	o�B	p�B	n�B	n�B	p�B	x�B	z�B	}�B	�B	�B	|�B	y�B	x�B	z�B	|�B	{�B	~�B	� B	� B	�B	�B	�B	�B	�B	�B	�B	�B	�+B	�7B	�1B	�1B	�JB	�bB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�-B	�3B	�9B	�?B	�?B	�?B	�?B	�?B	�?B	�RB	�XB	�^B	�XB	�^B	�^B	�jB	�qB	��B	��B	B	B	B	��B	�}B	�}B	�jB	�^B	�XB	�RB	�XB	��B	�}B	�qB	�wB	��B	ÖB	ŢB	ƨB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�5B	�BB	�HB	�NB	�TB	�TB	�HB	�5B	�;B	�BB	�NB	�TB	�`B	�mB	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B	��B	��B	��B	��B
  B
  B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
+B
	7B

=B

=B

=B

=B
DB
JB
VB
VB
VB
VB
VB
VB
VB
VB
PB
B
xB
-11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.02 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181024141410                              AO  ARCAADJP                                                                    20181024141410    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181024141410  QCP$                G�O�G�O�G�O�F03E            AO  ARGQQCPL                                                                    20181024141410  QCF$                G�O�G�O�G�O�0               