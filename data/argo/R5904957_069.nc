CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-24T14:08:17Z creation      
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
resolution        =���   axis      Z          9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  @�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���       BD   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  IT   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���       K   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       R(   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Y8   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       Z�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  b   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       c�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       j�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  q�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       s�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  z�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       |�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �    HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �D   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �T   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �X   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �h   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �l   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �p   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �tArgo profile    3.1 1.2 19500101000000  20181024140817  20181024140817  5904957 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               EA   AO  6560                            2B  A   APEX                            7471                            062512                          846 @���'@��1   @��忆��@2$���S��c�n��O�1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      EA   A   A   @�ff@�  A   A   A@  A`  A~ffA�33A�  A�  A�  A�  A�  A�  B ffB  B  B  B   B(  B0  B8  B@  BG��BO��BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C�C�C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ�CL  CN  CP  CR�CT�CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Cg�fCi�fCk�fCn  Cp  Cr  Ct  Cv  Cx  Cz�C|  C~  C�  C��C��C��C�  C��3C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C��3C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C��C��C��C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C��C��D fD � D  Dy�D��Dy�D  D� D  D� D  D� D  D� D  D� DfD� D	  D	� D
  D
� D  D� D  D� D  D� DfD� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D�fD  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D1  D1� D2  D2� D3fD3�fD4  D4� D4��D5y�D6  D6� D7  D7� D8  D8�fD9fD9� D9��D:� D:��D;y�D<  D<� D=  D=�fD>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DKfDK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DSfDS�fDT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D[��D\y�D\��D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dgy�Dg��Dhy�Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dpy�Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dvy�Dw  Dw� Dw�fDy� D�/\D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��
@�p�A�RA"�RAB�RAb�RA��\A��\A�\)A�\)A�\)A�\)A�\)A�\)BzB�B�B�B �B(�B0�B8�B@�BHG�BPG�BX�B`�Bh�Bp�Bx�B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
C +�CECEC+�C+�C
+�C+�C+�C+�C+�C+�C+�C+�C+�C+�C+�C +�C"+�C$+�C&+�C(+�C*+�C,+�C.+�C0+�C2+�C4+�C6+�C8+�C:+�C<+�C>+�C@+�CB+�CD+�CF+�CH+�CJECL+�CN+�CP+�CRECTECV+�CX+�CZ+�C\+�C^+�C`+�Cb+�Cd+�Cf+�Ch�Cj�Cl�Cn+�Cp+�Cr+�Ct+�Cv+�Cx+�CzEC|+�C~+�C��C�"�C�"�C�"�C��C��C��C��C��C�"�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�"�C��C��C��C�"�C��C��C��C��C��C��C��C��C��C��C��C�"�C��C��C��C��C�"�C�"�C�"�C��C��C��C��C��C��C�"�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�"�C�"�C�"�D GD ��D
�D�{D{D�{D
�D��D
�D��D
�D��D
�D��D
�D��DGD��D	
�D	��D

�D
��D
�D��D
�D��D
�D��DGD��D
�D��D
�D��D
�D��D
�D��D
�D�GD
�D��D
�D��D
�D��D
�D�GD
�D��D
�D�GD
�D��D
�D��D
�D��D
�D��D
�D��D
�D��D 
�D ��D!
�D!��D"
�D"��D#
�D#��D$
�D$��D%
�D1
�D1��D2
�D2��D3GD3�GD4
�D4��D5{D5�{D6
�D6��D7
�D7��D8
�D8�GD9GD9��D:{D:��D;{D;�{D<
�D<��D=
�D=�GD>
�D>��D?
�D?��D@
�D@��DA
�DA��DB
�DB��DC
�DC��DD
�DD��DE
�DE��DF
�DF��DG
�DG��DH
�DH��DI
�DI��DJ
�DJ��DKGDK��DL
�DL��DM
�DM��DN
�DN��DO
�DO��DP
�DP��DQ
�DQ��DR
�DR��DSGDS�GDT
�DT��DU
�DU��DV
�DV��DW
�DW��DX
�DX��DY
�DY��DZ
�DZ��D[
�D[��D\{D\�{D]{D]��D^
�D^��D_
�D_��D`
�D`��Da
�Da��Db
�Db��Dc
�Dc��Dd
�Dd��De
�De��Df
�Df��Dg
�Dg�{Dh{Dh�{Di
�Di��Dj
�Dj��Dk
�Dk��Dl
�Dl��Dm
�Dm��Dn
�Dn��Do
�Do��Dp
�Dp�{Dq
�Dq��Dr
�Dr��Ds
�Ds��Dt
�Dt��Du
�Du��Dv
�Dv�{Dw
�Dw��Dw�GDy��D�4�D��H11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�^5A�\)A�^5A�`BA�`BA�bNA�XA�ZA�\)A�\)A�ZA�ZA�^5A�bNA�bNA�bNA�`BA�bNA�bNA�bNA�dZA�bNA�dZA�`BA�XA�O�A�K�A�;dA���AܶFA܍PA�Q�A���A���A�ZA�l�A��HAя\Aя\Aљ�Aї�A�33A�/Aϩ�A�AͶFA�r�AʾwA�?}A�ĜAɃA��A�"�A�{AŋDA�VAº^A��DA�ȴA��^A���A��A��`A���A�$�A�9XA�JA��A�A�A��yA���A�I�A��A��A��A��HA��A�E�A���A���A�/A��wA���A�bNA��A���A�=qA�+A�VA�Q�A�x�A�O�A�~�A�{A��TA���A��!A��!A���A�VA���A� �A��\A�$�A��TA�1A�-A���A��;A���A�z�A���A"�A}C�Au��Ap�Al��Aj�jAj~�Ai|�Ag�Ad��Ac��A`�A]`BAZ�/AYhsAXr�AWVAT�HAS�AR�/AQ�TAO�
AMAI"�AF��AE��AE;dABv�AA33A?�A>��A<ĜA:bNA9x�A8��A7XA�\A|�A��A�/A�#A�A;dAȴA�AVA�AO�A�9A?}A�A�A�mA9XA��A��A;dA�uA�A�;A��A7LA
=A�AQ�A��Av�A�
A~�A��A��A�A+A�RA�A	ƨA�!A�At�A&�A �yA ��A 1'@��y@��`@�ff@��-@���@�o@�l�@�{@�hs@�&�@��@�p�@���@�l�@�ȴ@���@�E�@�n�@�V@�+@ꟾ@��#@�p�@�V@�ȴ@�@��@�|�@�O�@�D@�|�@�R@�@�K�@� �@ڰ!@���@��@ְ!@�=q@��@�G�@�9X@���@Ӆ@���@���@��@�5?@͑h@�Z@�dZ@ʏ\@�X@���@ȓu@�A�@�  @��m@�33@�J@���@��@Å@�33@�o@���@�{@�hs@��@�A�@�dZ@�@�M�@��^@���@��u@���@��@�ȴ@���@��+@��#@�&�@��D@� �@�K�@�-@��-@��7@��h@��h@�A�@�~�@���@���@��@�O�@�7L@�&�@��u@�bN@�I�@�1'@�33@�@�X@�O�@��@�(�@���@��9@�  @��
@��w@�dZ@�;d@���@�=q@��@�J@���@��@��#@��^@�p�@�7L@���@��@��@�r�@�(�@�1@��;@��
@��P@���@��y@���@���@�~�@�-@���@��@��T@��@�J@�$�@�-@�$�@�-@�-@�$�@�{@��#@��@�hs@�X@�O�@�7L@�%@��`@��9@�I�@��@���@��;@��P@�
=@���@���@�^5@���@�`B@���@���@�Z@�  @���@�\)@�33@�@���@�v�@�{@�@�x�@�/@���@��@���@��D@�I�@�9X@�  @�l�@��@�@���@���@���@��\@�{@�@���@���@��7@�`B@�7L@�V@��D@�I�@��P@���@��@��@�@�O�@���@��j@�Ĝ@��9@���@�r�@�I�@�A�@�A�@�1'@�ƨ@�o@���@�ff@��#@�&�@���@�Ĝ@��9@���@��u@�r�@��@���@�dZ@�K�@�33@���@��H@���@��R@�~�@��@���@�`B@�7L@��q@s�]@e11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�^5A�\)A�^5A�`BA�`BA�bNA�XA�ZA�\)A�\)A�ZA�ZA�^5A�bNA�bNA�bNA�`BA�bNA�bNA�bNA�dZA�bNA�dZA�`BA�XA�O�A�K�A�;dA���AܶFA܍PA�Q�A���A���A�ZA�l�A��HAя\Aя\Aљ�Aї�A�33A�/Aϩ�A�AͶFA�r�AʾwA�?}A�ĜAɃA��A�"�A�{AŋDA�VAº^A��DA�ȴA��^A���A��A��`A���A�$�A�9XA�JA��A�A�A��yA���A�I�A��A��A��A��HA��A�E�A���A���A�/A��wA���A�bNA��A���A�=qA�+A�VA�Q�A�x�A�O�A�~�A�{A��TA���A��!A��!A���A�VA���A� �A��\A�$�A��TA�1A�-A���A��;A���A�z�A���A"�A}C�Au��Ap�Al��Aj�jAj~�Ai|�Ag�Ad��Ac��A`�A]`BAZ�/AYhsAXr�AWVAT�HAS�AR�/AQ�TAO�
AMAI"�AF��AE��AE;dABv�AA33A?�A>��A<ĜA:bNA9x�A8��A7XA�\A|�A��A�/A�#A�A;dAȴA�AVA�AO�A�9A?}A�A�A�mA9XA��A��A;dA�uA�A�;A��A7LA
=A�AQ�A��Av�A�
A~�A��A��A�A+A�RA�A	ƨA�!A�At�A&�A �yA ��A 1'@��y@��`@�ff@��-@���@�o@�l�@�{@�hs@�&�@��@�p�@���@�l�@�ȴ@���@�E�@�n�@�V@�+@ꟾ@��#@�p�@�V@�ȴ@�@��@�|�@�O�@�D@�|�@�R@�@�K�@� �@ڰ!@���@��@ְ!@�=q@��@�G�@�9X@���@Ӆ@���@���@��@�5?@͑h@�Z@�dZ@ʏ\@�X@���@ȓu@�A�@�  @��m@�33@�J@���@��@Å@�33@�o@���@�{@�hs@��@�A�@�dZ@�@�M�@��^@���@��u@���@��@�ȴ@���@��+@��#@�&�@��D@� �@�K�@�-@��-@��7@��h@��h@�A�@�~�@���@���@��@�O�@�7L@�&�@��u@�bN@�I�@�1'@�33@�@�X@�O�@��@�(�@���@��9@�  @��
@��w@�dZ@�;d@���@�=q@��@�J@���@��@��#@��^@�p�@�7L@���@��@��@�r�@�(�@�1@��;@��
@��P@���@��y@���@���@�~�@�-@���@��@��T@��@�J@�$�@�-@�$�@�-@�-@�$�@�{@��#@��@�hs@�X@�O�@�7L@�%@��`@��9@�I�@��@���@��;@��P@�
=@���@���@�^5@���@�`B@���@���@�Z@�  @���@�\)@�33@�@���@�v�@�{@�@�x�@�/@���@��@���@��D@�I�@�9X@�  @�l�@��@�@���@���@���@��\@�{@�@���@���@��7@�`B@�7L@�V@��D@�I�@��P@���@��@��@�@�O�@���@��j@�Ĝ@��9@���@�r�@�I�@�A�@�A�@�1'@�ƨ@�o@���@�ff@��#@�&�@���@�Ĝ@��9@���@��u@�r�@��@���@�dZ@�K�@�33@���@��H@���@��R@�~�@��@���@�`B@�7L@��q@s�]@e11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
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
B
ÖB
ɺB
��B
�ZB
��BhBS�B{�Bs�Bu�B� B�PB��B��B�FBB��B�`BBJBoB�B'�B8RBL�BjBs�Bp�Bq�Bv�B�B� B�B�DB��B�^B��BɺB�}B��B��B��B��B�BB�`B�B�B�B�fB�B��B�9B��B�7B�B� B� B~�Bm�BZB@�B,B��B�fB�
B��B��BiyB?}B.B�BhB
=BB
�ZB
�wB
�B
��B
��B
�PB
t�B
r�B
`BB
I�B
:^B
)�B	��B	�/B	ƨB	�XB	�FB	�B	��B	�B	�B	x�B	iyB	\)B	T�B	M�B	D�B	7LB	2-B	-B	%�B	�B	+B�B�NB�TB�B�/B�B��B��BƨBB�}B�qB�^By�By�Bt�Bn�BhsBe`BdZBcTBcTBdZBffBk�Bo�Bw�B�\B�{B��B�B�-B�FB�XB�FB�LB�RB�RB�wB��B��B�mB�B�B�B�sB�TB�/B�/B�/B�B��B��B�XB�XB��B�\B�PB�PB�DB�+B�Bx�Bu�Bq�Bl�Be`BbNBaHB_;B]/B^5B_;B_;B`BBaHBbNBdZBffBp�Bx�By�B�{B��B��B�B��B�B�'B�'B�3B�3B�3B�FB�jB�wB�}B��BŢBǮBǮBȴB��B��B��B��B�B�B�#B�#B�;B�NB�`B�B�B�B�B�B�B�B��B��B��B	%B	+B	1B	1B	JB	�B	�B	�B	�B	�B	!�B	$�B	&�B	,B	33B	5?B	6FB	6FB	6FB	7LB	8RB	9XB	:^B	=qB	@�B	B�B	D�B	E�B	I�B	I�B	J�B	N�B	P�B	Q�B	S�B	S�B	S�B	XB	ZB	[#B	[#B	^5B	aHB	e`B	gmB	l�B	n�B	l�B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�!B	�'B	�'B	�-B	�?B	�RB	�^B	�^B	�jB	�qB	�}B	��B	B	B	ĜB	ƨB	ƨB	ƨB	ŢB	ŢB	ƨB	ȴB	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�)B	�;B	�BB	�HB	�HB	�NB	�TB	�ZB	�ZB	�fB	�fB	�mB	�sB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
B
B
  B
B
B
B
B
B
B
B
%B
%B
%B
%B
+B
+B
+B
1B
+B
1B
1B
	7B

=B
DB
JB
JB
JB
JB
JB
JB
PB
\B
\B
\B
VB
\B
bB
bB
oB
�B
%�B
6�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B
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
B
ÖB
ɺB
��B
�ZB
��BhBS�B{�Bs�Bu�B� B�PB��B��B�FBB��B�`BBJBoB�B'�B8RBL�BjBs�Bp�Bq�Bv�B�B� B�B�DB��B�^B��BɺB�}B��B��B��B��B�BB�`B�B�B�B�fB�B��B�9B��B�7B�B� B� B~�Bm�BZB@�B,B��B�fB�
B��B��BiyB?}B.B�BhB
=BB
�ZB
�wB
�B
��B
��B
�PB
t�B
r�B
`BB
I�B
:^B
)�B	��B	�/B	ƨB	�XB	�FB	�B	��B	�B	�B	x�B	iyB	\)B	T�B	M�B	D�B	7LB	2-B	-B	%�B	�B	+B�B�NB�TB�B�/B�B��B��BƨBB�}B�qB�^By�By�Bt�Bn�BhsBe`BdZBcTBcTBdZBffBk�Bo�Bw�B�\B�{B��B�B�-B�FB�XB�FB�LB�RB�RB�wB��B��B�mB�B�B�B�sB�TB�/B�/B�/B�B��B��B�XB�XB��B�\B�PB�PB�DB�+B�Bx�Bu�Bq�Bl�Be`BbNBaHB_;B]/B^5B_;B_;B`BBaHBbNBdZBffBp�Bx�By�B�{B��B��B�B��B�B�'B�'B�3B�3B�3B�FB�jB�wB�}B��BŢBǮBǮBȴB��B��B��B��B�B�B�#B�#B�;B�NB�`B�B�B�B�B�B�B�B��B��B��B	%B	+B	1B	1B	JB	�B	�B	�B	�B	�B	!�B	$�B	&�B	,B	33B	5?B	6FB	6FB	6FB	7LB	8RB	9XB	:^B	=qB	@�B	B�B	D�B	E�B	I�B	I�B	J�B	N�B	P�B	Q�B	S�B	S�B	S�B	XB	ZB	[#B	[#B	^5B	aHB	e`B	gmB	l�B	n�B	l�B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�!B	�'B	�'B	�-B	�?B	�RB	�^B	�^B	�jB	�qB	�}B	��B	B	B	ĜB	ƨB	ƨB	ƨB	ŢB	ŢB	ƨB	ȴB	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�)B	�;B	�BB	�HB	�HB	�NB	�TB	�ZB	�ZB	�fB	�fB	�mB	�sB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
B
B
  B
B
B
B
B
B
B
B
%B
%B
%B
%B
+B
+B
+B
1B
+B
1B
1B
	7B

=B
DB
JB
JB
JB
JB
JB
JB
PB
\B
\B
\B
VB
\B
bB
bB
oB
�B
%�B
6�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.17 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181024140817                              AO  ARCAADJP                                                                    20181024140817    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181024140817  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181024140817  QCF$                G�O�G�O�G�O�0               