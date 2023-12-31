CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:05:39Z creation      
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20181005190539  20181005190539  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6431                            2B  A   APEX                            7466                            062512                          846 @��i΁ɡ1   @��j`� V@0�1&�y�c��G�{1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   A   @�  @�33A   A   A@  A`  A�  A�  A�33A�33A�33A�  A�  A�  B   B  B  B��B   B(  B/��B8  B@ffBH  BP  BX  B`  Bh  Bp  Bx  B�  B�33B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  C   C  C  C�fC  C
  C  C  C�C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0�C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CG�fCJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C��3C�  C�  C��3C�  C��C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C��3C��C��C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C��3C�  C�  C��C�  C��3C�  C��3C�  C�  C��3C�  C�  C��3C��3C��3C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C��C��C��C��C�  C��3C��3C�  C�  C�  C�  C�  C��3C�  C�  C�  C��3C��3C��3C�  C��D fD �fDfD� D��Dy�D  D� D  D�fDfD� D��Dy�D  D� D  Dy�D��D	y�D	��D
� D  D� D  D�fD  D� DfD�fD  D� D��Dy�D  D�fDfD� DfD�fDfD�fD  D� DfD� D��D� DfD�fD  D� DfD� D  D�fD  Dy�D  D� D  D� D  D�fD   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&fD&� D'  D'� D(  D(� D)  D)� D*  D*�fD+fD+�fD,  D,� D-  D-y�D.  D.�fD/  D/y�D0  D0� D1  D1� D2  D2� D3  D3y�D3��D4� D5  D5� D6  D6� D6��D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DAfDA�fDB  DB� DC  DC� DD  DD� DE  DEy�DF  DF�fDG  DG� DH  DH� DIfDI� DJ  DJ� DK  DK� DK��DL� DM  DMy�DN  DN� DO  DO�fDP  DP� DQ  DQ� DR  DR� DR��DS� DTfDT� DT��DUy�DV  DV� DW  DWy�DW��DX� DYfDY�fDZ  DZ� DZ��D[y�D\  D\� D]  D]� D^  D^� D_fD_� D`  D`� Da  Da�fDbfDb� Dc  Dc� DdfDd�fDe  De� Df  Df� Dg  Dg� Dh  Dh� DifDi� Dj  Dj�fDk  Dky�Dl  Dl�fDm  Dm� Dn  Dn� Dn��Doy�Dp  Dp� Dp��Dqy�Dq��Dry�Dr��Ds� Dt  Dt� Du  Du� DvfDv� Dw  Dw� Dy{�D�A�D�
11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�Q�@˅A(�A$(�AD(�Ad(�A�{A�{A�G�A�G�A�G�A�{A�{A�{B
=B	
=B
=B��B!
=B)
=B0��B9
=BAp�BI
=BQ
=BY
=Ba
=Bi
=Bq
=By
=B��B��RB��B��B��RB��RB��B��B��B��B��B��B��B��B��B��B��B�Q�BȅB̅BЅBԅB؅B܅B��B�B�RB�RB��B�B��B��C B�CB�CB�C(�CB�C
B�CB�CB�C\)CB�CB�CB�CB�CB�CB�CB�C B�C"B�C$B�C&B�C(B�C*B�C,B�C.B�C0\)C2B�C4B�C6B�C8B�C:B�C<B�C>B�C@B�CBB�CDB�CFB�CH(�CJB�CLB�CNB�CPB�CRB�CTB�CVB�CXB�CZB�C\B�C^B�C`B�CbB�CdB�CfB�ChB�CjB�ClB�CnB�CpB�CrB�CtB�CvB�CxB�CzB�C|B�C~B�C�!HC�!HC�!HC�!HC�!HC�!HC�{C�!HC�!HC�!HC�!HC�.C�.C�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�.C�.C�!HC�!HC�!HC�!HC�{C�!HC�!HC�{C�!HC�.C�!HC�!HC�!HC�!HC�!HC�!HC�!HC�.C�!HC�!HC�!HC�!HC�!HC�{C�.C�.C�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�.C�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�.C�.C�!HC�!HC�!HC�!HC�!HC�{C�!HC�!HC�.C�!HC�{C�!HC�{C�!HC�!HC�{C�!HC�!HC�{C�{C�{C�!HC�.C�.C�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�.C�.C�.C�.C�!HC�{C�{C�!HC�!HC�!HC�!HC�!HC�{C�!HC�!HC�!HC�{C�{C�{C�!HC�.D 
D �
D
D��D
>D�>D�D��D�D�
D
D��D
>D�>D�D��D�D�>D	
>D	�>D

>D
��D�D��D�D�
D�D��D
D�
D�D��D
>D�>D�D�
D
D��D
D�
D
D�
D�D��D
D��D
>D��D
D�
D�D��D
D��D�D�
D�D�>D�D��D�D��D�D�
D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&
D&��D'�D'��D(�D(��D)�D)��D*�D*�
D+
D+�
D,�D,��D-�D-�>D.�D.�
D/�D/�>D0�D0��D1�D1��D2�D2��D3�D3�>D4
>D4��D5�D5��D6�D6��D7
>D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA
DA�
DB�DB��DC�DC��DD�DD��DE�DE�>DF�DF�
DG�DG��DH�DH��DI
DI��DJ�DJ��DK�DK��DL
>DL��DM�DM�>DN�DN��DO�DO�
DP�DP��DQ�DQ��DR�DR��DS
>DS��DT
DT��DU
>DU�>DV�DV��DW�DW�>DX
>DX��DY
DY�
DZ�DZ��D[
>D[�>D\�D\��D]�D]��D^�D^��D_
D_��D`�D`��Da�Da�
Db
Db��Dc�Dc��Dd
Dd�
De�De��Df�Df��Dg�Dg��Dh�Dh��Di
Di��Dj�Dj�
Dk�Dk�>Dl�Dl�
Dm�Dm��Dn�Dn��Do
>Do�>Dp�Dp��Dq
>Dq�>Dr
>Dr�>Ds
>Ds��Dt�Dt��Du�Du��Dv
Dv��Dw�Dw��Dy�)D�J>D��\11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�&�A�7LA�;dA�=qA�?}A�?}A�?}A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�C�A�C�A�A�A�?}A�?}A�;dA�+A�"�A��A�
=A���A��A��A��`A��;A���Aͺ^AͮA͋DA�t�A�O�A�1'A�"�A�A��TA���A�ĜA̲-A̋DA�-A��A���AˍPA�bNA�K�A��HAʗ�A�G�A�A�O�A�^5AǴ9A�l�A�n�A�{A��A���A���A��A�z�A�I�A���A��/A���A�=qA���A��jA��A��!A���A�`BA�C�A�A��;A��`A�p�A�S�A�M�A�{A�x�A���A��HA��A�A�bA�1'A�A��9A�%A��A�JA�$�A���A��A��9A�v�A���A��DA�
=A���A�JA�-A}��A{hsAx�HAs�Ao\)Akp�Ah5?Af �Ad��AdQ�Ab�A^9XAY�PAW�AU��AT�AR�RAO/AK/AG�mAFM�AD-AC�A@�`A?%A=��A<��A9�A8 �A7��A6�\A5�A3�A2ȴA2�A1��A1t�A1+A0JA.^5A,JA)%A(ȴA(�jA'�A&�\A$�9A"�`A!��A��A��AC�A9XA|�A��A=qA�A�A��A��AG�A\)AI�A��A�yAr�A-A��AbA
��A�`A=qA�mA��A�;A/A��A9XA�
A33A��AZA1A�mAA ��A v�A =q@��m@��y@�~�@�-@��7@�\)@��y@��!@���@��D@���@��@��m@��;@��F@�K�@���@�+@�J@�`B@�@�@�9X@��@�5?@ꟾ@�J@��@�bN@�7L@�ƨ@�w@�@�+@�V@��T@�h@�/@��D@�|�@�K�@��@�~�@��@܋D@�^5@��`@׾w@�33@Ցh@���@��@�ff@щ7@�&�@��@�X@��@Л�@��`@У�@�I�@��;@�"�@�ȴ@·+@�G�@��;@�+@�ȴ@ʧ�@ʗ�@�v�@�V@�J@���@�O�@��@�33@���@�{@�?}@�r�@���@Õ�@�33@�V@���@�x�@�hs@��9@�(�@���@���@�-@���@�^5@�@�X@��@��9@��@�O�@�j@�C�@��@�ȴ@���@�5?@�@���@�O�@���@���@��@�1@�t�@�+@��@��w@��w@�C�@��y@��R@���@�$�@���@�Q�@�A�@�(�@�b@�1@�  @�b@��P@�|�@�\)@�@��@��@���@�V@��T@��@�?}@��@��u@�9X@�1@���@���@�dZ@�K�@�"�@�ȴ@�5?@��@���@��@���@��u@�A�@���@���@���@��@�v�@�J@���@�`B@���@��j@��D@�bN@�I�@�b@��@�l�@�;d@�+@�@��@���@�~�@�J@��@��^@�/@���@�A�@�1'@�(�@���@��F@���@�dZ@���@���@���@�v�@�ff@�M�@��#@�?}@��j@�A�@���@��w@��@�"�@���@���@�n�@�{@���@��7@�&�@��D@�z�@�z�@�Z@�C�@���@�V@�J@�hs@�?}@�/@�&�@�V@���@�z�@� �@�  @���@�S�@�33@��@�v�@�{@���@��@�7L@��@�V@���@�bN@�9X@��@�|�@�C�@�+@��@�@�ȴ@�V@�=q@���@���@�7L@�V@�%@��`@��u@� �@�b@���@��P@�"�@���@���@��\@�ff@�M�@��@��^@���@��7@�?}@��@�V@���@���@���@��9@���@��u@��D@�Q�@�  @��@��P@��P@��P@��@�t�@�+@���@���@|w�@d111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�&�A�7LA�;dA�=qA�?}A�?}A�?}A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�C�A�C�A�A�A�?}A�?}A�;dA�+A�"�A��A�
=A���A��A��A��`A��;A���Aͺ^AͮA͋DA�t�A�O�A�1'A�"�A�A��TA���A�ĜA̲-A̋DA�-A��A���AˍPA�bNA�K�A��HAʗ�A�G�A�A�O�A�^5AǴ9A�l�A�n�A�{A��A���A���A��A�z�A�I�A���A��/A���A�=qA���A��jA��A��!A���A�`BA�C�A�A��;A��`A�p�A�S�A�M�A�{A�x�A���A��HA��A�A�bA�1'A�A��9A�%A��A�JA�$�A���A��A��9A�v�A���A��DA�
=A���A�JA�-A}��A{hsAx�HAs�Ao\)Akp�Ah5?Af �Ad��AdQ�Ab�A^9XAY�PAW�AU��AT�AR�RAO/AK/AG�mAFM�AD-AC�A@�`A?%A=��A<��A9�A8 �A7��A6�\A5�A3�A2ȴA2�A1��A1t�A1+A0JA.^5A,JA)%A(ȴA(�jA'�A&�\A$�9A"�`A!��A��A��AC�A9XA|�A��A=qA�A�A��A��AG�A\)AI�A��A�yAr�A-A��AbA
��A�`A=qA�mA��A�;A/A��A9XA�
A33A��AZA1A�mAA ��A v�A =q@��m@��y@�~�@�-@��7@�\)@��y@��!@���@��D@���@��@��m@��;@��F@�K�@���@�+@�J@�`B@�@�@�9X@��@�5?@ꟾ@�J@��@�bN@�7L@�ƨ@�w@�@�+@�V@��T@�h@�/@��D@�|�@�K�@��@�~�@��@܋D@�^5@��`@׾w@�33@Ցh@���@��@�ff@щ7@�&�@��@�X@��@Л�@��`@У�@�I�@��;@�"�@�ȴ@·+@�G�@��;@�+@�ȴ@ʧ�@ʗ�@�v�@�V@�J@���@�O�@��@�33@���@�{@�?}@�r�@���@Õ�@�33@�V@���@�x�@�hs@��9@�(�@���@���@�-@���@�^5@�@�X@��@��9@��@�O�@�j@�C�@��@�ȴ@���@�5?@�@���@�O�@���@���@��@�1@�t�@�+@��@��w@��w@�C�@��y@��R@���@�$�@���@�Q�@�A�@�(�@�b@�1@�  @�b@��P@�|�@�\)@�@��@��@���@�V@��T@��@�?}@��@��u@�9X@�1@���@���@�dZ@�K�@�"�@�ȴ@�5?@��@���@��@���@��u@�A�@���@���@���@��@�v�@�J@���@�`B@���@��j@��D@�bN@�I�@�b@��@�l�@�;d@�+@�@��@���@�~�@�J@��@��^@�/@���@�A�@�1'@�(�@���@��F@���@�dZ@���@���@���@�v�@�ff@�M�@��#@�?}@��j@�A�@���@��w@��@�"�@���@���@�n�@�{@���@��7@�&�@��D@�z�@�z�@�Z@�C�@���@�V@�J@�hs@�?}@�/@�&�@�V@���@�z�@� �@�  @���@�S�@�33@��@�v�@�{@���@��@�7L@��@�V@���@�bN@�9X@��@�|�@�C�@�+@��@�@�ȴ@�V@�=q@���@���@�7L@�V@�%@��`@��u@� �@�b@���@��P@�"�@���@���@��\@�ff@�M�@��@��^@���@��7@�?}@��@�V@���@���@���@��9@���@��u@��D@�Q�@�  @��@��P@��P@��P@��@�t�@�+@���@���@|w�@d111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�=B�=B�=B�=B�=B�=B�=B�=B�=B�=B�=B�=B�=B�=B�=B�DB�DB�DB�DB�DB�DB�DB�JB�JB�JB�PB�\B�\B�\B�hB�oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�'B�?B�RBÖB��B��B�B{B&�B7LBF�BI�BG�BVB]/B_;BhsBn�Bo�Bq�By�Bv�Bp�Bn�BjB`BB]/BZBO�BN�BD�B1'B"�B�BhB��B�B�BB��B�jB��B�\B�Bx�BhsBI�B)�BB
�)B
ŢB
�9B
��B
aHB
8RB
PB	�B	�#B	��B	�^B	��B	�B	n�B	ZB	M�B	E�B	@�B	49B	#�B	uB	PB	1B	  B��B�B�;B�
B��B��B��BȴBŢBÖB�}B�jB�^B�RB�FB�?B�3B�3B�-B�-B�'B�!B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B��B��B�B��B�B�B�B�!B�LB�XB�^B�jBƨBȴBǮB��B��B��B�B�HB�NB�NB�HB�HB�HB�HB�NB�`B�sB�mB�mB�mB�sB�sB�mB�sB�B�B�B�B�sB�fB�ZB�ZB�TB�fB�B�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B	  B	B��B��B��B	B	B	B	%B	%B		7B	bB	�B	�B	�B	%�B	,B	.B	0!B	/B	/B	/B	.B	/B	33B	5?B	6FB	7LB	9XB	:^B	?}B	@�B	D�B	H�B	H�B	J�B	M�B	O�B	Q�B	Q�B	S�B	T�B	XB	YB	ZB	\)B	ZB	ZB	_;B	hsB	gmB	^5B	XB	VB	T�B	S�B	R�B	T�B	\)B	\)B	^5B	`BB	aHB	cTB	bNB	ffB	hsB	n�B	s�B	t�B	t�B	v�B	x�B	|�B	�B	�7B	�DB	�hB	�uB	�{B	�uB	�uB	�hB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�'B	�-B	�-B	�-B	�-B	�3B	�9B	�?B	�?B	�FB	�RB	�^B	�dB	�jB	�jB	�wB	�}B	��B	��B	B	B	ÖB	ĜB	ǮB	ǮB	ȴB	ȴB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�#B	�/B	�;B	�;B	�BB	�BB	�;B	�;B	�;B	�;B	�NB	�TB	�TB	�NB	�ZB	�ZB	�ZB	�ZB	�TB	�ZB	�`B	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
B
B
B
B
%B
+B
+B
+B
+B
+B
1B
1B
	7B
	7B
	7B

=B

=B

=B

=B

=B
DB
DB
JB
JB
JB
JB
JB
VB
\B
\B
\B
\B
\B
\B
bB
HB
'8B
2a22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222   B�=B�=B�=B�=B�=B�=B�=B�=B�=B�=B�=B�=B�=B�=B�=B�DB�DB�DB�DB�DB�DB�DB�JB�JB�JB�PB�\B�\B�\B�hB�oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�'B�?B�RBÖB��B��B�B{B&�B7LBF�BI�BG�BVB]/B_;BhsBn�Bo�Bq�By�Bv�Bp�Bn�BjB`BB]/BZBO�BN�BD�B1'B"�B�BhB��B�B�BB��B�jB��B�\B�Bx�BhsBI�B)�BB
�)B
ŢB
�9B
��B
aHB
8RB
PB	�B	�#B	��B	�^B	��B	�B	n�B	ZB	M�B	E�B	@�B	49B	#�B	uB	PB	1B	  B��B�B�;B�
B��B��B��BȴBŢBÖB�}B�jB�^B�RB�FB�?B�3B�3B�-B�-B�'B�!B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B��B��B�B��B�B�B�B�!B�LB�XB�^B�jBƨBȴBǮB��B��B��B�B�HB�NB�NB�HB�HB�HB�HB�NB�`B�sB�mB�mB�mB�sB�sB�mB�sB�B�B�B�B�sB�fB�ZB�ZB�TB�fB�B�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B	  B	B��B��B��B	B	B	B	%B	%B		7B	bB	�B	�B	�B	%�B	,B	.B	0!B	/B	/B	/B	.B	/B	33B	5?B	6FB	7LB	9XB	:^B	?}B	@�B	D�B	H�B	H�B	J�B	M�B	O�B	Q�B	Q�B	S�B	T�B	XB	YB	ZB	\)B	ZB	ZB	_;B	hsB	gmB	^5B	XB	VB	T�B	S�B	R�B	T�B	\)B	\)B	^5B	`BB	aHB	cTB	bNB	ffB	hsB	n�B	s�B	t�B	t�B	v�B	x�B	|�B	�B	�7B	�DB	�hB	�uB	�{B	�uB	�uB	�hB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�'B	�-B	�-B	�-B	�-B	�3B	�9B	�?B	�?B	�FB	�RB	�^B	�dB	�jB	�jB	�wB	�}B	��B	��B	B	B	ÖB	ĜB	ǮB	ǮB	ȴB	ȴB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�#B	�/B	�;B	�;B	�BB	�BB	�;B	�;B	�;B	�;B	�NB	�TB	�TB	�NB	�ZB	�ZB	�ZB	�ZB	�TB	�ZB	�`B	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
B
B
B
B
%B
+B
+B
+B
+B
+B
1B
1B
	7B
	7B
	7B

=B

=B

=B

=B

=B
DB
DB
JB
JB
JB
JB
JB
VB
\B
\B
\B
\B
\B
\B
bB
HB
'8B
2a22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.26 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190539                              AO  ARCAADJP                                                                    20181005190539    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190539  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190539  QCF$                G�O�G�O�G�O�8000            