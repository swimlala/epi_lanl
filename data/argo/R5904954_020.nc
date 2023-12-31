CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:16:53Z creation      
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
_FillValue                    �tArgo profile    3.1 1.2 19500101000000  20181005191653  20181005191653  5904954 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               A   AO  6557                            2B  A   APEX                            7468                            062512                          846 @׭���1   @׭�F)�R@46E�����c���"��1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      A   A   B   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C�C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C;�fC>  C@  CB�CD  CE�fCH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C_�fCa�fCd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv�Cx  Cz  C|  C~  C�  C��3C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C��3C�  C�  C�  C��3C�  C�  C��C�  C�  C�  C�  C��3C��3C��3C��3C�  C�  C�  C��3C�  C��C�  C�  C�  C��3C�  C��3C��3C��C�  C�  C��C��C�  C��3C��3C��3C�  C�  C��3C��C��C��C��C��C�  C�  C�  C��3C��3C�  C��3C��3C�  C�  C�  C��C�  C�  C��3C��3C��3C�  C�  C�  C�  C��C��C��3C��3C��C�  C��3C�  C��3C��C�  C��3C��3C��3C��3C��3C��3C�  C�  C�  C��3C��C��C�  C��3C��3C�  C��3C�  C�  C��C�  C�  C�  D� D	fD	� D
  D
�fD  Dy�D�D� D  D�fDfD� D  D� D  D� D  D� DfD� D  D� D  D� D  D� D  D�fD��Dy�D  D�fDfD� D  D�fD  D�fD  Ds3D��D�fD  Dy�D��Dy�D   D � D ��D!�fD"  D"� D"��D#� D#��D$�fD%  D%� D%��D&�fD'  D'� D(fD(� D)  D)� D*  D*� D+  D+y�D+��D,�fD,��D-� D-��D.y�D/fD/� D0fD0� D1  D1�fD1��D2� D3�D3� D4fD4y�D4��D5�fD6  D6�fD7  D7�fD7��D8� D8��D9�fD:fD:y�D;fD;y�D<  D<y�D=fD=� D=��D>�fD?fD?y�D@  D@�fD@��DA�fDB  DB� DC  DC� DD  DD� DE  DEy�DF  DFy�DGfDG� DH  DH� DI  DI� DI��DJ� DKfDK� DK��DL�fDMfDMy�DN  DN�fDN��DOy�DO�3DP� DQ  DQs3DR  DR�fDS  DS� DT  DTy�DU  DU�fDU��DV� DWfDcfDc� Dd  Dd� De  Dey�Df  Df�fDg  Dg� Dh  Dh� Di  Di� Di��Dj� Dk  Dk� Dl  Dl� Dm  Dm�fDn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Dr��Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dw�fDy}qD�AHD��311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�G�@�{A
=A#
=AC
=Ac
=A��A��A��A��A��AхA�A�B BBBB B(B0B8B@BHBPBXB`BhBpBxB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�.B�.B�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHC 0�C0�C0�C0�C0�C
0�C0�C0�C0�CJ>C0�C0�C0�C0�C0�C0�C 0�C"0�C$0�C&0�C(0�C*0�C,0�C.0�C00�C20�C40�C60�C80�C:0�C<
C>0�C@0�CBJ>CD0�CF
CH0�CJ0�CL0�CN0�CP0�CR0�CT0�CV0�CX0�CZ0�C\0�C^0�C`
Cb
Cd0�Cf0�Ch0�Cj0�Cl0�Cn0�Cp0�Cr0�Ct0�CvJ>Cx0�Cz0�C|0�C~0�C�RC��C�RC�RC��C�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�%C�RC��C�RC�RC�RC��C�RC�RC�%C�RC�RC�RC�RC��C��C��C��C�RC�RC�RC��C�RC�%C�RC�RC�RC��C�RC��C��C�%C�RC�RC�%C�%C�RC��C��C��C�RC�RC��C�%C�%C�%C�%C�%C�RC�RC�RC��C��C�RC��C��C�RC�RC�RC�%C�RC�RC��C��C��C�RC�RC�RC�RC�%C�%C��C��C�%C�RC��C�RC��C�%C�RC��C��C��C��C��C��C�RC�RC�RC��C�%C�%C�RC��C��C�RC��C�RC�RC�%C�RC�RC�RD�)D	�D	�)D
)D
��D)D��D�D�)D)D��D�D�)D)D�)D)D�)D)D�)D�D�)D)D�)D)D�)D)D�)D)D��D�D��D)D��D�D�)D)D��D)D��D)D\D�D��D)D��D�D��D )D �)D!�D!��D")D"�)D#�D#�)D$�D$��D%)D%�)D&�D&��D')D'�)D(�D(�)D))D)�)D*)D*�)D+)D+��D,�D,��D-�D-�)D.�D.��D/�D/�)D0�D0�)D1)D1��D2�D2�)D3�D3�)D4�D4��D5�D5��D6)D6��D7)D7��D8�D8�)D9�D9��D:�D:��D;�D;��D<)D<��D=�D=�)D>�D>��D?�D?��D@)D@��DA�DA��DB)DB�)DC)DC�)DD)DD�)DE)DE��DF)DF��DG�DG�)DH)DH�)DI)DI�)DJ�DJ�)DK�DK�)DL�DL��DM�DM��DN)DN��DO�DO��DO�\DP�)DQ)DQ\DR)DR��DS)DS�)DT)DT��DU)DU��DV�DV�)DW�Dc�Dc�)Dd)Dd�)De)De��Df)Df��Dg)Dg�)Dh)Dh�)Di)Di�)Dj�Dj�)Dk)Dk�)Dl)Dl�)Dm)Dm��Dn)Dn�)Do)Do�)Dp)Dp�)Dq)Dq�)Dr)Dr�)Ds�Ds�)Dt)Dt�)Du)Du�)Dv)Dv�)Dw)Dw�)DwҏDy��D�G\D��G11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�AЍPAГuAЗ�AБhAБhAЏ\AЍPA�|�A�Q�A�&�A�oA�
=A���A��#A�Aϛ�A�ZA�/A�%A���A��A���A�AΧ�AΓuA�v�A�"�A��#ÁA�
=A�ĜA̟�A̝�A̙�A̓uA̓uA̕�A̕�A̕�A̕�A̗�A̗�Ȧ+A�$�A�
=A�A�A�JA�VA�oA�1A��A���AˮA˰!A˰!A���A�%A�z�A�  A��/A�ffA���A��hA��uA���A��HA�%A���A� �A��A���A�ƨA��A�bNA��A��TA�A�A�x�A���A���A���A�1A��DA��mA�33A�t�A�E�A�`BA��7A���A��A��A���A�ffA�A�A��wA���A�ĜA���A�r�A�dZA�~�A��A���A��yA��HA�|�A�$�A�ZA�ĜA�`BA���A��A� �A�`BA�v�A��AhsA}"�Azz�Aw��As�wAr��Ar9XAnĜAhJAgx�Agx�AgS�AgAe��Acl�AbffAa33A_��A]�AZ�AYVAX�\AW�AU`BAQ;dANI�AL�AJI�AH��AG+AE�ABA�A?��A>$�A<^5A;33A:(�A7�FA5�hA3��A45?A2��A0��A0=qA/��A.ZA,�uA+%A)�#A)/A(ĜA&Q�A&9XA&Q�A&ZA%�A%��A%33A$$�A#hsA#VA"�A"-A!oA ^5A (�A��A�A33A��A��A��AA��A��A  A33AjA�TA��A?}AoAA�9A��A�A`BA
ZA�yAp�A��AE�A �A��A�A�AdZA�9A�mA �yA I�A   @�n�@�r�@��w@�o@�ff@���@��@�r�@�
=@�x�@�S�@��@��@�A�@��
@��@�5?@��-@���@��H@��@��y@���@�dZ@�
=@���@�%@ߍP@Ο�@�E�@���@�hs@�7L@̬@�A�@˝�@�l�@�S�@�o@��@�=q@���@���@�Q�@��@ǍP@�"�@ũ�@���@Õ�@¸R@�@�/@��`@��9@�ƨ@�K�@�=q@���@�/@���@��w@�o@��\@�J@�/@���@��;@�\)@�~�@�G�@���@��D@�A�@��F@�+@���@��@���@��@���@�9X@���@��@���@�;d@��@��H@��H@���@���@�7L@��@��#@��^@���@��7@�p�@�X@�?}@���@���@��@�z�@��@�r�@�9X@� �@��@��@���@���@��+@�~�@�E�@��^@��^@���@��7@�p�@�O�@�7L@�&�@��@���@�(�@�A�@�I�@�j@�j@� �@��@�K�@�=q@��@�ff@��y@�$�@��@���@�\)@��@�G�@�Z@�l�@��@�1'@���@��@��y@�@�ff@�5?@���@�ȴ@��\@�G�@���@�"�@���@�v�@�M�@�$�@��#@���@�@�p�@�G�@���@��9@��j@��@�  @�ƨ@���@�l�@�"�@�~�@�=q@�J@��7@�/@��@�1@���@��@�z�@�bN@�1'@��@�1@��@��;@���@�ƨ@�|�@��H@���@�^5@���@��T@���@��^@���@��h@�hs@�G�@��@�V@���@���@��@�Q�@�A�@�1'@�1@��;@��@���@���@���@�\)@�;d@��y@���@��+@�n�@�=q@�@��|@|oi@k�$11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111AЍPAГuAЗ�AБhAБhAЏ\AЍPA�|�A�Q�A�&�A�oA�
=A���A��#A�Aϛ�A�ZA�/A�%A���A��A���A�AΧ�AΓuA�v�A�"�A��#ÁA�
=A�ĜA̟�A̝�A̙�A̓uA̓uA̕�A̕�A̕�A̕�A̗�A̗�Ȧ+A�$�A�
=A�A�A�JA�VA�oA�1A��A���AˮA˰!A˰!A���A�%A�z�A�  A��/A�ffA���A��hA��uA���A��HA�%A���A� �A��A���A�ƨA��A�bNA��A��TA�A�A�x�A���A���A���A�1A��DA��mA�33A�t�A�E�A�`BA��7A���A��A��A���A�ffA�A�A��wA���A�ĜA���A�r�A�dZA�~�A��A���A��yA��HA�|�A�$�A�ZA�ĜA�`BA���A��A� �A�`BA�v�A��AhsA}"�Azz�Aw��As�wAr��Ar9XAnĜAhJAgx�Agx�AgS�AgAe��Acl�AbffAa33A_��A]�AZ�AYVAX�\AW�AU`BAQ;dANI�AL�AJI�AH��AG+AE�ABA�A?��A>$�A<^5A;33A:(�A7�FA5�hA3��A45?A2��A0��A0=qA/��A.ZA,�uA+%A)�#A)/A(ĜA&Q�A&9XA&Q�A&ZA%�A%��A%33A$$�A#hsA#VA"�A"-A!oA ^5A (�A��A�A33A��A��A��AA��A��A  A33AjA�TA��A?}AoAA�9A��A�A`BA
ZA�yAp�A��AE�A �A��A�A�AdZA�9A�mA �yA I�A   @�n�@�r�@��w@�o@�ff@���@��@�r�@�
=@�x�@�S�@��@��@�A�@��
@��@�5?@��-@���@��H@��@��y@���@�dZ@�
=@���@�%@ߍP@Ο�@�E�@���@�hs@�7L@̬@�A�@˝�@�l�@�S�@�o@��@�=q@���@���@�Q�@��@ǍP@�"�@ũ�@���@Õ�@¸R@�@�/@��`@��9@�ƨ@�K�@�=q@���@�/@���@��w@�o@��\@�J@�/@���@��;@�\)@�~�@�G�@���@��D@�A�@��F@�+@���@��@���@��@���@�9X@���@��@���@�;d@��@��H@��H@���@���@�7L@��@��#@��^@���@��7@�p�@�X@�?}@���@���@��@�z�@��@�r�@�9X@� �@��@��@���@���@��+@�~�@�E�@��^@��^@���@��7@�p�@�O�@�7L@�&�@��@���@�(�@�A�@�I�@�j@�j@� �@��@�K�@�=q@��@�ff@��y@�$�@��@���@�\)@��@�G�@�Z@�l�@��@�1'@���@��@��y@�@�ff@�5?@���@�ȴ@��\@�G�@���@�"�@���@�v�@�M�@�$�@��#@���@�@�p�@�G�@���@��9@��j@��@�  @�ƨ@���@�l�@�"�@�~�@�=q@�J@��7@�/@��@�1@���@��@�z�@�bN@�1'@��@�1@��@��;@���@�ƨ@�|�@��H@���@�^5@���@��T@���@��^@���@��h@�hs@�G�@��@�V@���@���@��@�Q�@�A�@�1'@�1@��;@��@���@���@���@�\)@�;d@��y@���@��+@�n�@�=q@�@��|@|oi@k�$11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
l�B
m�B
m�B
m�B
o�B
s�B
�B
�RB
�HB
�NB
�`B
�fB
�sB
�B
�B
�B
��BBDBhBoB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B!�B%�B'�B)�B,B-B.B.B-B-B-B.B0!B9XBs�B~�B��B�RB�BDB�B:^B<jBM�B]/BcTBl�Bq�Bu�Bw�B�=B�uB�VB�DB�%Bk�BbNB^5B[#BZBjBjBffBcTBZBK�B6FB)�B�B�BBBB��B�B�qB�BW
B@�B.B2-B�B+B
�sB
��B
�qB
��B
��B
��B
�DB
~�B
u�B
jB
Q�B
?}B
+B
�B
	7B	��B	�5B	�
B	��B	�^B	��B	�uB	�uB	�oB	�\B	�1B	y�B	s�B	l�B	bNB	VB	G�B	=qB	9XB	1'B	$�B	oB	B��B�B�B�NB�
B��BŢB��B�wB�dB�XB�-B�B�?BɺB��B��B��BɺBɺBɺBɺBȴBȴB��B�B�5B�HB�`B�B�B�B��B	  B	B	B	B	B	B	1B	bB	oB	oB	oB	{B	{B	oB	oB	hB	VB��B��B��B�B�sB�mB�fB�`B�TB�;B�#B��BȴB��B�wB�wB�qB�qB�qB�jB�^B�FB�-B�B�B�B�B�B�B��B��B��B��B��B��B��B�B��B��B��B�B��B��B��B��B��B��B��B��B��B��B��B��B�B�5B�5B�5B�;B�;B�;B�HB�ZB�`B�`B�fB�fB�sB�sB�B�B�B�B�B��B��B��B��B��B	B	+B	+B	
=B	DB	\B	hB	uB	{B	�B	�B	"�B	$�B	'�B	'�B	)�B	+B	.B	5?B	9XB	<jB	?}B	B�B	D�B	F�B	H�B	J�B	L�B	N�B	Q�B	S�B	T�B	VB	ZB	]/B	^5B	_;B	bNB	ffB	k�B	o�B	r�B	r�B	s�B	t�B	w�B	y�B	z�B	|�B	|�B	}�B	~�B	�B	�B	�B	�B	�B	�B	�B	�JB	�JB	�JB	�DB	�PB	�PB	�PB	�VB	�VB	�\B	�\B	�\B	�\B	�\B	�oB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�3B	�LB	�RB	�9B	�?B	�FB	�LB	�jB	�jB	�dB	�FB	�-B	�B	�B	�B	�'B	�'B	�-B	�3B	�9B	�FB	�FB	�RB	�dB	�dB	�dB	�wB	�}B	��B	��B	ÖB	ȴB	��B	��B	��B	��B	��B	��B	��A���B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B

=B
�B
�B
'22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222242222222222222222222222222222222222222222222222B
l�B
m�B
m�B
m�B
o�B
s�B
�B
�RB
�HB
�NB
�`B
�fB
�sB
�B
�B
�B
��BBDBhBoB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B!�B%�B'�B)�B,B-B.B.B-B-B-B.B0!B9XBs�B~�B��B�RB�BDB�B:^B<jBM�B]/BcTBl�Bq�Bu�Bw�B�=B�uB�VB�DB�%Bk�BbNB^5B[#BZBjBjBffBcTBZBK�B6FB)�B�B�BBBB��B�B�qB�BW
B@�B.B2-B�B+B
�sB
��B
�qB
��B
��B
��B
�DB
~�B
u�B
jB
Q�B
?}B
+B
�B
	7B	��B	�5B	�
B	��B	�^B	��B	�uB	�uB	�oB	�\B	�1B	y�B	s�B	l�B	bNB	VB	G�B	=qB	9XB	1'B	$�B	oB	B��B�B�B�NB�
B��BŢB��B�wB�dB�XB�-B�B�?BɺB��B��B��BɺBɺBɺBɺBȴBȴB��B�B�5B�HB�`B�B�B�B��B	  B	B	B	B	B	B	1B	bB	oB	oB	oB	{B	{B	oB	oB	hB	VB��B��B��B�B�sB�mB�fB�`B�TB�;B�#B��BȴB��B�wB�wB�qB�qB�qB�jB�^B�FB�-B�B�B�B�B�B�B��B��B��B��B��B��B��B�B��B��B��B�B��B��B��B��B��B��B��B��B��B��B��B��B�B�5B�5B�5B�;B�;B�;B�HB�ZB�`B�`B�fB�fB�sB�sB�B�B�B�B�B��B��B��B��B��B	B	+B	+B	
=B	DB	\B	hB	uB	{B	�B	�B	"�B	$�B	'�B	'�B	)�B	+B	.B	5?B	9XB	<jB	?}B	B�B	D�B	F�B	H�B	J�B	L�B	N�B	Q�B	S�B	T�B	VB	ZB	]/B	^5B	_;B	bNB	ffB	k�B	o�B	r�B	r�B	s�B	t�B	w�B	y�B	z�B	|�B	|�B	}�B	~�B	�B	�B	�B	�B	�B	�B	�B	�JB	�JB	�JB	�DB	�PB	�PB	�PB	�VB	�VB	�\B	�\B	�\B	�\B	�\B	�oB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�3B	�LB	�RB	�9B	�?B	�FB	�LB	�jB	�jB	�dB	�FB	�-B	�B	�B	�B	�'B	�'B	�-B	�3B	�9B	�FB	�FB	�RB	�dB	�dB	�dB	�wB	�}B	��B	��B	ÖB	ȴB	��B	��B	��B	��B	��B	��B	��A���B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B

=B
�B
�B
'22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222242222222222222222222222222222222222222222222222G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.19 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005191653                              AO  ARCAADJP                                                                    20181005191653    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005191653  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005191653  QCF$                G�O�G�O�G�O�8000            