CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-24T14:08:49Z creation      
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20181024140849  20181024140849  5904957 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6560                            2B  A   APEX                            7471                            062512                          846 @��$�B�)1   @��%�s�@59�"��`�d���S�1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   A   @�  @�  A   A   AA��A`  A�  A�  A�  A�  A���A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�33B�33B�33B�33B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C�fC"  C$  C%�fC(  C*  C,  C.  C/�fC2  C4  C6  C8  C:  C<�C>  C@  CB  CD  CE�fCG�fCJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C]�fC`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr�Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C��C�  C��3C�  C�  C�  C��C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C��C��C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C��3C��3C��3C��3C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3D y�D  D� D��D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D  Dy�D  D� D  D� D  D�fD  D� D  D� D  D� D��Dy�D��D� D   D y�D!  D!�fD"  D"� D#  D#� D$  D$� D%  D%� D&  D&�fD'  D'� D(  D(� D)  D)� D*  D*�fD+fD+� D,  D,� D-  D-� D.  D.� D/  D/� D/��D0� D1  D1y�D2  D2� D3  D3� D3��D4y�D5  D5� D6  D6� D7  D7� D8  D8� D9  D9�fD:fD:�fD;  D;� D<  D<� D=  D=� D>  D>� D?fD?� D@  D@� D@��DA� DB  DB� DB��DCy�DC��DDy�DD��DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL�fDM  DM�fDNfDN�fDOfDO�fDPfDP�fDQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Dby�Dc  Dc� Dd  Dd� De  De� Df  Df� Df��Dg� Dh  Dh� Di  Diy�Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp�fDq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dws3Dyn�D�>�D��R11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�Q�@�Q�A(�A$(�AEAd(�A�{A�{A�{A�{A��HA�{A�{A�{B
=B	
=B
=B
=B!
=B)
=B1
=B9
=BA
=BI
=BQ
=BY
=Ba
=Bi
=Bq
=By
=B��B��B��B��B��B��B��RB��RB��RB��RB��B��B��B��B��B��B��B�Q�BȅB̅BЅB�Q�B؅B܅B��B�B�B�B��B�B��B��C B�CB�CB�CB�CB�C
B�CB�CB�CB�CB�CB�CB�CB�CB�CB�CB�C (�C"B�C$B�C&(�C(B�C*B�C,B�C.B�C0(�C2B�C4B�C6B�C8B�C:B�C<\)C>B�C@B�CBB�CDB�CF(�CH(�CJB�CLB�CNB�CPB�CRB�CTB�CVB�CXB�CZB�C\B�C^(�C`B�CbB�CdB�CfB�ChB�CjB�ClB�CnB�CpB�Cr\)CtB�CvB�CxB�CzB�C|B�C~B�C�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�.C�.C�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�{C�{C�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�.C�!HC�!HC�!HC�!HC�!HC�!HC�!HC�.C�!HC�{C�!HC�!HC�!HC�.C�!HC�!HC�!HC�{C�!HC�!HC�!HC�!HC�!HC�!HC�!HC�.C�!HC�!HC�!HC�.C�.C�!HC�!HC�!HC�.C�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�{C�{C�{C�{C�{C�{C�{C�{C�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�.C�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�{D 
=D �>D�D��D
>D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
�
D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D�>D�D��D�D�>D�D��D�D��D�D�
D�D��D�D��D�D��D
>D�>D
>D��D �D �>D!�D!�
D"�D"��D#�D#��D$�D$��D%�D%��D&�D&�
D'�D'��D(�D(��D)�D)��D*�D*�
D+
D+��D,�D,��D-�D-��D.�D.��D/�D/��D0
>D0��D1�D1�>D2�D2��D3�D3��D4
>D4�>D5�D5��D6�D6��D7�D7��D8�D8��D9�D9�
D:
D:�
D;�D;��D<�D<��D=�D=��D>�D>��D?
D?��D@�D@��DA
>DA��DB�DB��DC
>DC�>DD
>DD�>DE
>DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL�
DM�DM�
DN
DN�
DO
DO�
DP
DP�
DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db�>Dc�Dc��Dd�Dd��De�De��Df�Df��Dg
>Dg��Dh�Dh��Di�Di�>Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp�
Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Du�Du��Dv�Dv��Dw�Dw��Dy\D�G
D�Ф11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���AɶFAɼjA���A�ȴA�A���AɾwAɼjA���A�ƨA�ƨA���A��;A��TA��HA��yA��A���A���A���A��`A�ĜAɉ7A�
=A�33AǃA�x�A�dZA�^5AǕ�A�t�A�  AƩ�AƁA�^5A�G�A��A��
A�x�A�G�Aĝ�A��Aå�A�M�A�^5A��A�-A���A�9XA�G�A���A��hA��+A�JA���A��A�=qA��hA�;dA���A�Q�A�p�A�x�A�;dA���A�%A��RA�r�A�7LA�G�A��-A���A���A��A�JA�=qA���A��A�XA���A�n�A�VA�&�A���A���A�~�A�ĜA���A��A�XA��A�  A���A�=qA�1'A��A��\A��A��9A�E�A�At�A}+Ax�!AvA�Ast�Ap�ApbNAoK�Akp�Ab�9AaK�A`��A`ffA_XA[t�AY7LAWt�AVn�AU�7ATffAP1'ANn�AN  AM�;AM��AMXAK�AI|�AG�AF^5AF1'AF{AE��AD�jAC�7AAS�A@-A?�FA>ȴA>ffA<�yA;G�A9XA7�A6~�A4�\A3;dA0~�A.�yA.z�A. �A.{A-�A+�A*ȴA)��A(�A(�A'ƨA&��A%�-A$��A#�^A!��A �A�;A�/AƨA`BAoA~�A�
A1'A/A�A�mA7LA%A�9A;dA�HA��AM�AS�A�!A9XA��A�7A?}A��A�Al�A�A�#A
=A�/A�Al�A
n�A
bA	ƨA	�A�mA �A�AffAl�A �A ȴA =qA 5?A  �A �A v�A ��A bN@��@���@��T@��7@�G�@��;@�33@�ff@���@���@��@���@��#@���@�@�?}@���@�
=@���@�\)@�E�@�u@�ȴ@��@��#@���@�9X@�^5@�V@�o@��@���@�G�@�r�@Ь@�z�@��@�@͙�@�V@���@�+@ʧ�@�v�@���@��
@Ƈ+@�J@���@��@��@���@�hs@�/@���@��@�hs@�%@�I�@��w@�33@�ȴ@���@��`@�bN@�ƨ@�E�@��^@���@�+@�ff@��@�l�@�+@��H@��H@��+@�J@��@���@�`B@��@��
@�t�@�@��!@�ff@�p�@�j@��;@��P@�C�@��@���@�Ĝ@�z�@�I�@���@��@��@��;@��;@���@�ƨ@���@�t�@�\)@�33@�33@�
=@��y@���@���@�$�@���@�X@��/@���@��@�1'@���@��@�ȴ@�ff@�J@��^@��h@�X@��D@�(�@�1@�1'@��
@��@���@�E�@�V@���@�I�@�1'@�ƨ@���@��@�C�@�ȴ@�$�@�=q@�M�@�=q@�E�@�@��T@�p�@��`@�  @���@���@�S�@��@��!@�5?@���@��@�/@��@��j@��@�A�@�33@��!@�~�@�J@��^@��h@��@�?}@���@��u@�(�@��m@�ƨ@�C�@�"�@�
=@��@��@���@��+@�^5@�E�@�=q@�$�@�J@���@���@��7@�x�@�x�@�`B@��@��@��D@�1@��@�l�@�C�@�+@�@��R@��+@�M�@���@���@��-@���@��7@�x�@�?}@���@��u@�z�@�Q�@�(�@�  @��w@�l�@�+@��@���@��\@�M�@�$�@�@���@���@���@��h@�p�@�?}@�/@��@�%@��@�Ĝ@��j@��D@�j@�A�@�1@���@�l�@�K�@�+@�"�@��@�o@���@��@���@���@���@���@�n�@�M�@�@��T@��@��T@���@��@��@�X@���@���@�A�@� �@�!-@r�@b��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A���AɶFAɼjA���A�ȴA�A���AɾwAɼjA���A�ƨA�ƨA���A��;A��TA��HA��yA��A���A���A���A��`A�ĜAɉ7A�
=A�33AǃA�x�A�dZA�^5AǕ�A�t�A�  AƩ�AƁA�^5A�G�A��A��
A�x�A�G�Aĝ�A��Aå�A�M�A�^5A��A�-A���A�9XA�G�A���A��hA��+A�JA���A��A�=qA��hA�;dA���A�Q�A�p�A�x�A�;dA���A�%A��RA�r�A�7LA�G�A��-A���A���A��A�JA�=qA���A��A�XA���A�n�A�VA�&�A���A���A�~�A�ĜA���A��A�XA��A�  A���A�=qA�1'A��A��\A��A��9A�E�A�At�A}+Ax�!AvA�Ast�Ap�ApbNAoK�Akp�Ab�9AaK�A`��A`ffA_XA[t�AY7LAWt�AVn�AU�7ATffAP1'ANn�AN  AM�;AM��AMXAK�AI|�AG�AF^5AF1'AF{AE��AD�jAC�7AAS�A@-A?�FA>ȴA>ffA<�yA;G�A9XA7�A6~�A4�\A3;dA0~�A.�yA.z�A. �A.{A-�A+�A*ȴA)��A(�A(�A'ƨA&��A%�-A$��A#�^A!��A �A�;A�/AƨA`BAoA~�A�
A1'A/A�A�mA7LA%A�9A;dA�HA��AM�AS�A�!A9XA��A�7A?}A��A�Al�A�A�#A
=A�/A�Al�A
n�A
bA	ƨA	�A�mA �A�AffAl�A �A ȴA =qA 5?A  �A �A v�A ��A bN@��@���@��T@��7@�G�@��;@�33@�ff@���@���@��@���@��#@���@�@�?}@���@�
=@���@�\)@�E�@�u@�ȴ@��@��#@���@�9X@�^5@�V@�o@��@���@�G�@�r�@Ь@�z�@��@�@͙�@�V@���@�+@ʧ�@�v�@���@��
@Ƈ+@�J@���@��@��@���@�hs@�/@���@��@�hs@�%@�I�@��w@�33@�ȴ@���@��`@�bN@�ƨ@�E�@��^@���@�+@�ff@��@�l�@�+@��H@��H@��+@�J@��@���@�`B@��@��
@�t�@�@��!@�ff@�p�@�j@��;@��P@�C�@��@���@�Ĝ@�z�@�I�@���@��@��@��;@��;@���@�ƨ@���@�t�@�\)@�33@�33@�
=@��y@���@���@�$�@���@�X@��/@���@��@�1'@���@��@�ȴ@�ff@�J@��^@��h@�X@��D@�(�@�1@�1'@��
@��@���@�E�@�V@���@�I�@�1'@�ƨ@���@��@�C�@�ȴ@�$�@�=q@�M�@�=q@�E�@�@��T@�p�@��`@�  @���@���@�S�@��@��!@�5?@���@��@�/@��@��j@��@�A�@�33@��!@�~�@�J@��^@��h@��@�?}@���@��u@�(�@��m@�ƨ@�C�@�"�@�
=@��@��@���@��+@�^5@�E�@�=q@�$�@�J@���@���@��7@�x�@�x�@�`B@��@��@��D@�1@��@�l�@�C�@�+@�@��R@��+@�M�@���@���@��-@���@��7@�x�@�?}@���@��u@�z�@�Q�@�(�@�  @��w@�l�@�+@��@���@��\@�M�@�$�@�@���@���@���@��h@�p�@�?}@�/@��@�%@��@�Ĝ@��j@��D@�j@�A�@�1@���@�l�@�K�@�+@�"�@��@�o@���@��@���@���@���@���@�n�@�M�@�@��T@��@��T@���@��@��@�X@���@���@�A�@� �@�!-@r�@b��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BdZBcTBdZBcTBcTBbNBbNBbNBbNBbNBbNBbNBdZBhsBjBiyBl�Bv�Bw�B�B�uB��B��B��B��B�+B�B�JB�VB��B�?B��B�XB�?B��B�B�NB�ZB�mB�B�BJB�B�B#�B2-BZBk�Bv�B� B�7B�7B�\B�uB�1B}�B�B�B�B�%B�=B�PB�Bu�BgmBT�BE�B>wB:^B8RBJ�BXB=qB�B �B#�B�B�BB�`B�#B�B�B��B��B�}B�B��B�1Bu�BW
B>wB49B�BB
�ZB
��B
�LB
�hB
q�B
jB
`BB
O�B
<jB
�B
B	�B	�/B	�B	��B	�-B	�JB	�B	�B	}�B	u�B	bNB	VB	M�B	F�B	@�B	7LB	#�B	�B	�B	�B	�B	�B	hB	1B	  B	B	B	  B��B��B��B�B�B�sB�fB�mB�`B�;B�B��B��BƨB��B�XB�3B�-B�'B�!B�B��B��B��B��B��B��B��B�uB�hB�PB�=B�1B�%B�B�B�B� B}�B}�B{�B}�B~�B� B~�B~�B|�B|�B� B�B�B�B� B�B�B�B�B}�B}�B~�B~�B� B�B�B�B�%B�B�B�B�B|�Bu�Bl�Be`BaHB`BB`BBcTBffBgmBhsBs�B�B�B~�B{�B~�B�B�B�B�B�B�B�B~�B}�B~�B�DB�VB�VB�VB�bB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B��B��B��B��B��B��B��B��B�!B�^B�jB�qB�wB��BBBBBBBBBǮB��B�/B�TB�fB�fB�`B�TB�HB�;B�/B�B�
B��B��B��B�BB�sB�B�B�B��B��B��B��B	B	1B	DB	JB	VB	\B	VB	uB	�B	�B	�B	�B	�B	#�B	(�B	)�B	,B	.B	/B	/B	2-B	8RB	?}B	B�B	D�B	F�B	G�B	I�B	J�B	L�B	O�B	R�B	S�B	XB	ZB	^5B	aHB	aHB	aHB	bNB	dZB	e`B	hsB	hsB	k�B	n�B	p�B	s�B	s�B	t�B	v�B	z�B	~�B	~�B	�B	~�B	y�B	x�B	x�B	~�B	�B	�B	�B	�B	�B	�B	�B	�%B	�%B	�+B	�7B	�=B	�7B	�7B	�DB	�JB	�VB	�bB	�hB	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�9B	�?B	�LB	�XB	�^B	�dB	�qB	��B	ÖB	ŢB	ƨB	ƨB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�#B	�/B	�/B	�5B	�5B	�5B	�5B	�;B	�NB	�NB	�TB	�TB	�ZB	�`B	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
B
B
B
%B
?B
B
($11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   BdZBcTBdZBcTBcTBbNBbNBbNBbNBbNBbNBbNBdZBhsBjBiyBl�Bv�Bw�B�B�uB��B��B��B��B�+B�B�JB�VB��B�?B��B�XB�?B��B�B�NB�ZB�mB�B�BJB�B�B#�B2-BZBk�Bv�B� B�7B�7B�\B�uB�1B}�B�B�B�B�%B�=B�PB�Bu�BgmBT�BE�B>wB:^B8RBJ�BXB=qB�B �B#�B�B�BB�`B�#B�B�B��B��B�}B�B��B�1Bu�BW
B>wB49B�BB
�ZB
��B
�LB
�hB
q�B
jB
`BB
O�B
<jB
�B
B	�B	�/B	�B	��B	�-B	�JB	�B	�B	}�B	u�B	bNB	VB	M�B	F�B	@�B	7LB	#�B	�B	�B	�B	�B	�B	hB	1B	  B	B	B	  B��B��B��B�B�B�sB�fB�mB�`B�;B�B��B��BƨB��B�XB�3B�-B�'B�!B�B��B��B��B��B��B��B��B�uB�hB�PB�=B�1B�%B�B�B�B� B}�B}�B{�B}�B~�B� B~�B~�B|�B|�B� B�B�B�B� B�B�B�B�B}�B}�B~�B~�B� B�B�B�B�%B�B�B�B�B|�Bu�Bl�Be`BaHB`BB`BBcTBffBgmBhsBs�B�B�B~�B{�B~�B�B�B�B�B�B�B�B~�B}�B~�B�DB�VB�VB�VB�bB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B��B��B��B��B��B��B��B��B�!B�^B�jB�qB�wB��BBBBBBBBBǮB��B�/B�TB�fB�fB�`B�TB�HB�;B�/B�B�
B��B��B��B�BB�sB�B�B�B��B��B��B��B	B	1B	DB	JB	VB	\B	VB	uB	�B	�B	�B	�B	�B	#�B	(�B	)�B	,B	.B	/B	/B	2-B	8RB	?}B	B�B	D�B	F�B	G�B	I�B	J�B	L�B	O�B	R�B	S�B	XB	ZB	^5B	aHB	aHB	aHB	bNB	dZB	e`B	hsB	hsB	k�B	n�B	p�B	s�B	s�B	t�B	v�B	z�B	~�B	~�B	�B	~�B	y�B	x�B	x�B	~�B	�B	�B	�B	�B	�B	�B	�B	�%B	�%B	�+B	�7B	�=B	�7B	�7B	�DB	�JB	�VB	�bB	�hB	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�9B	�?B	�LB	�XB	�^B	�dB	�qB	��B	ÖB	ŢB	ƨB	ƨB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�#B	�/B	�/B	�5B	�5B	�5B	�5B	�;B	�NB	�NB	�TB	�TB	�ZB	�`B	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
B
B
B
%B
?B
B
($11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.26 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181024140849                              AO  ARCAADJP                                                                    20181024140849    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181024140849  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181024140849  QCF$                G�O�G�O�G�O�0               