CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:06:14Z creation      
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20181005190614  20181005190614  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL              -A   AO  6431                            2B  A   APEX                            7466                            062512                          846 @�jR�S�1   @�j�	��@0X�t�j�cy/��w1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                     -A   A   A   @�  @���A   A   A@  Aa��A�  A�  A�  A�  A�33A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BHffBP��BW��B`  Bh  Bp  Bx  B�  B�  B�  B���B���B���B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C�fC  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C_�fCb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C��C��C��C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C��C��C�  C�  C�  C��C��C��C��C�  C�  C�  C�  C�  C��C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C��C�  C�  C�  C�  C�  C�  C��C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C��3C��3C�  C��C��C�  C�  C��3C��3C�  C��C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D fD �fD  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D��D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� D  D� D  D� DfD�fD  Dy�D  D� D   D � D!  D!� D!��D"� D#  D#� D$fD$�fD$��D%y�D&  D&y�D'  D'�fD(  D(� D)fD)�fD*  D*� D*��D+� D,  D,�fD-fD-� D.  D.� D/fD/� D0  D0� D1  D1y�D2  D2� D3  D3y�D4  D4� D5  D5� D5��D6y�D7  D7� D8fD8� D9  D9� D9��D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DDy�DE  DE�fDFfDF�fDGfDG� DH  DH� DI  DI� DJ  DJ� DK  DKy�DL  DL� DMfDM�fDN  DN� DO  DO� DO��DPy�DP��DQy�DQ��DR� DS  DS� DTfDT�fDU  DU� DV  DVy�DV��DW� DXfDX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D]��D^� D_fD_� D`fD`�fDafDa� Db  Db� Dc  Dc� Dd  Dd� DefDe� Df  Df� Dg  Dg�fDhfDh� Dh��Di� Dj  Dj� Dk  Dk� Dl  Dl� DmfDm� Dn  Dn�fDo  Doy�Dp  Dp� Dq  Dq� Dq��Dry�Ds  Ds�fDt  Dt� DufDu� Dv  Dv� Dv��Dw� Dx  DxffD�-�D��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�Q�@��A(�A$(�AD(�AeA�{A�{A�{A�{A�G�A�{A�{A�{B
=B	
=B
=B
=B!
=B)
=B1
=B9
=BA
=BIp�BQ�
BX��Ba
=Bi
=Bq
=By
=B��B��B��B�Q�B�Q�B�Q�B��B��RB��RB��B��B��B��B��B��B��B��BąBȸRB̅BЅBԅB؅B܅B��B�B�B�B��B�B��B��C B�CB�CB�CB�CB�C
B�CB�CB�CB�CB�CB�CB�C(�CB�CB�CB�C B�C"B�C$B�C&B�C(B�C*B�C,B�C.B�C0B�C2B�C4B�C6B�C8B�C:B�C<B�C>B�C@B�CBB�CDB�CFB�CHB�CJB�CLB�CNB�CPB�CRB�CTB�CVB�CXB�CZB�C\B�C^B�C`(�CbB�CdB�CfB�ChB�CjB�ClB�CnB�CpB�CrB�CtB�CvB�CxB�CzB�C|B�C~B�C�!HC�!HC�.C�.C�.C�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�.C�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�{C�!HC�!HC�.C�.C�!HC�!HC�!HC�.C�.C�.C�.C�!HC�!HC�!HC�!HC�!HC�.C�!HC�{C�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�.C�!HC�!HC�!HC�!HC�!HC�!HC�!HC�.C�!HC�!HC�.C�!HC�!HC�!HC�!HC�!HC�!HC�.C�!HC�.C�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�{C�!HC�!HC�!HC�!HC�{C�{C�!HC�.C�.C�!HC�!HC�{C�{C�!HC�.C�!HC�!HC�!HC�{C�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HD 
D �
D�D�>D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D
>D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D�>D�D��D�D��D�D��D�D��D
D�
D�D�>D�D��D �D ��D!�D!��D"
>D"��D#�D#��D$
D$�
D%
>D%�>D&�D&�>D'�D'�
D(�D(��D)
D)�
D*�D*��D+
>D+��D,�D,�
D-
D-��D.�D.��D/
D/��D0�D0��D1�D1�>D2�D2��D3�D3�>D4�D4��D5�D5��D6
>D6�>D7�D7��D8
D8��D9�D9��D:
>D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD�>DE�DE�
DF
DF�
DG
DG��DH�DH��DI�DI��DJ�DJ��DK�DK�>DL�DL��DM
DM�
DN�DN��DO�DO��DP
>DP�>DQ
>DQ�>DR
>DR��DS�DS��DT
DT�
DU�DU��DV�DV�>DW
>DW��DX
DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^
>D^��D_
D_��D`
D`�
Da
Da��Db�Db��Dc�Dc��Dd�Dd��De
De��Df�Df��Dg�Dg�
Dh
Dh��Di
>Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm
Dm��Dn�Dn�
Do�Do�>Dp�Dp��Dq�Dq��Dr
>Dr�>Ds�Ds�
Dt�Dt��Du
Du��Dv�Dv��Dw
>Dw��Dx�Dxw
D�6D��f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�VA��A��A��A��A��A��A��A��A��A��A��A��A� �A��A� �A��A��A��A��A��A�"�A�&�A��#A�C�A� �A���A�-A�t�A�  A��A��A�z�A���A�l�A�9XA��-A�p�A�A�A� �A��`A�|�A�S�A�"�A�{A�A��\A��TA�v�A�7LA��A��\A�-A���A�E�A�VA��A���A�1'A���A�-A�K�A��FA��A���A� �A���A�A�x�A��-A�JA�I�A���A�S�A���A��A���A��9A��A�z�A�p�A���A��DA��A�1'A�(�A�&�A��A�A�M�A��#A�ffA��HA���A��A��A��uA~�Ax��Au��AtffAs�wArĜAo��Ai�#Ad1A^v�AV��AQ�AM��AJ  AE�mAC��AA�A?&�A=��A;�#A;;dA:(�A9A7�hA6�yA5ƨA5VA4��A4I�A49XA2�A2{A2Q�A2�uA2��A3oA1��A1�A1��A/?}A)�A%VA$bNA$�`A$��A$(�A!?}Ax�AG�A9XA�A��A�wAr�A�^AjA�7A�A��AffA+A&�A�9A�#A��A�uAn�A�TAI�A�AVA�A�7A�A	��A��A�yA9XA�^AdZA"�A�\A��AA~�A�-AffAƨA�A �@�@�M�@�G�@��D@��@�l�@�@��9@�  @�|�@���@���@��7@�A�@�V@���@��@�@�l�@�K�@�M�@���@�9X@�@�D@��H@�O�@�j@��@�
=@���@�~�@ݺ^@��@�r�@�z�@�A�@��m@�(�@ݑh@�-@��@�v�@�V@�V@�?}@��
@���@�~�@�hs@�@���@ՙ�@Լj@�bN@� �@Ӿw@�ȴ@��@���@�@љ�@���@�  @�|�@�\)@�C�@�+@�@Η�@�@�?}@���@� �@���@�G�@���@��@���@��@ēu@�-@��@��9@�z�@�bN@�bN@��;@���@��@�7L@��#@�@�?}@�V@���@���@��j@���@��D@�Z@��@��m@��;@��w@��!@�5?@��#@�p�@�Q�@���@��
@���@��P@�~�@�~�@�~�@�n�@�V@��@��^@���@�`B@��j@�b@��@���@�  @�ƨ@��@��@��R@�;d@��;@���@�l�@��w@��w@�^5@�E�@�^5@�?}@�9X@�+@�@���@�o@��@���@��@�x�@�&�@��@�b@�K�@��@���@�{@���@���@�@�M�@�`B@��j@�1@�ƨ@�S�@�ȴ@�J@�x�@��/@�z�@�9X@���@��
@�@�n�@�7L@��u@�/@�`B@��@���@�bN@��@�S�@�;d@�\)@��@�$�@���@��D@�Z@���@�ƨ@��@�K�@���@���@��@���@�J@���@���@�hs@��@�r�@�I�@� �@�1@�1@��@��
@���@�\)@�
=@��+@��@��#@���@���@��h@��@�X@�&�@��@���@�Ĝ@�A�@�j@��D@�1@�ƨ@�|�@�\)@��@���@�^5@�$�@��@��7@��@���@�z�@� �@�1@�ƨ@�ƨ@���@�ƨ@���@�l�@���@�hs@�`B@�p�@��@�Ĝ@��@� �@���@�b@�Z@�Z@�j@�I�@�1'@�(�@��@��
@���@�K�@�ȴ@�~�@�V@�=q@���@��@�  @���@��@�C�@��+@��^@�/@��@���@�bN@���@��9@��@��@�Z@�9X@�b@�1@�1@��;@���@��F@��F@��P@�t�@�t�@�\)@�o@���@��R@���@�=�@t9X111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�VA��A��A��A��A��A��A��A��A��A��A��A��A� �A��A� �A��A��A��A��A��A�"�A�&�A��#A�C�A� �A���A�-A�t�A�  A��A��A�z�A���A�l�A�9XA��-A�p�A�A�A� �A��`A�|�A�S�A�"�A�{A�A��\A��TA�v�A�7LA��A��\A�-A���A�E�A�VA��A���A�1'A���A�-A�K�A��FA��A���A� �A���A�A�x�A��-A�JA�I�A���A�S�A���A��A���A��9A��A�z�A�p�A���A��DA��A�1'A�(�A�&�A��A�A�M�A��#A�ffA��HA���A��A��A��uA~�Ax��Au��AtffAs�wArĜAo��Ai�#Ad1A^v�AV��AQ�AM��AJ  AE�mAC��AA�A?&�A=��A;�#A;;dA:(�A9A7�hA6�yA5ƨA5VA4��A4I�A49XA2�A2{A2Q�A2�uA2��A3oA1��A1�A1��A/?}A)�A%VA$bNA$�`A$��A$(�A!?}Ax�AG�A9XA�A��A�wAr�A�^AjA�7A�A��AffA+A&�A�9A�#A��A�uAn�A�TAI�A�AVA�A�7A�A	��A��A�yA9XA�^AdZA"�A�\A��AA~�A�-AffAƨA�A �@�@�M�@�G�@��D@��@�l�@�@��9@�  @�|�@���@���@��7@�A�@�V@���@��@�@�l�@�K�@�M�@���@�9X@�@�D@��H@�O�@�j@��@�
=@���@�~�@ݺ^@��@�r�@�z�@�A�@��m@�(�@ݑh@�-@��@�v�@�V@�V@�?}@��
@���@�~�@�hs@�@���@ՙ�@Լj@�bN@� �@Ӿw@�ȴ@��@���@�@љ�@���@�  @�|�@�\)@�C�@�+@�@Η�@�@�?}@���@� �@���@�G�@���@��@���@��@ēu@�-@��@��9@�z�@�bN@�bN@��;@���@��@�7L@��#@�@�?}@�V@���@���@��j@���@��D@�Z@��@��m@��;@��w@��!@�5?@��#@�p�@�Q�@���@��
@���@��P@�~�@�~�@�~�@�n�@�V@��@��^@���@�`B@��j@�b@��@���@�  @�ƨ@��@��@��R@�;d@��;@���@�l�@��w@��w@�^5@�E�@�^5@�?}@�9X@�+@�@���@�o@��@���@��@�x�@�&�@��@�b@�K�@��@���@�{@���@���@�@�M�@�`B@��j@�1@�ƨ@�S�@�ȴ@�J@�x�@��/@�z�@�9X@���@��
@�@�n�@�7L@��u@�/@�`B@��@���@�bN@��@�S�@�;d@�\)@��@�$�@���@��D@�Z@���@�ƨ@��@�K�@���@���@��@���@�J@���@���@�hs@��@�r�@�I�@� �@�1@�1@��@��
@���@�\)@�
=@��+@��@��#@���@���@��h@��@�X@�&�@��@���@�Ĝ@�A�@�j@��D@�1@�ƨ@�|�@�\)@��@���@�^5@�$�@��@��7@��@���@�z�@� �@�1@�ƨ@�ƨ@���@�ƨ@���@�l�@���@�hs@�`B@�p�@��@�Ĝ@��@� �@���@�b@�Z@�Z@�j@�I�@�1'@�(�@��@��
@���@�K�@�ȴ@�~�@�V@�=q@���@��@�  @���@��@�C�@��+@��^@�/@��@���@�bN@���@��9@��@��@�Z@�9X@�b@�1@�1@��;@���@��F@��F@��P@�t�@�t�@�\)@�o@���@��R@���@�=�@t9X111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�'B�'B�'B�'B�'B�'B�'B�'B�-B�'B�'B�'B�'B�'B�'B�'B�'B�'B�'B�-B�-B�'B�'B�dB��B�B	:^B	T�B	m�B	�B	�fB
5?B
\)B
��B
��B
�^B
�B+B\B�B�B/B;dBB�BG�BO�BZB]/Be`Bn�B}�B�hB��BƨB��B�;B�`B�B��BJBoB �B�B�B�BJBB�yB��B��B�B�bBz�BZB[#Bm�Bo�Bs�BL�BG�B@�B7LB,B�BoBB
�B
B
��B
�hB
}�B
jB
Q�B
@�B
!�B	��B	�B	�{B	v�B	gmB	_;B	XB	K�B	-B	PB�B�B��B�B��B��B�B�!B�RB�qBBɺB��B��B��B�B�)B�HB�sB��B	B	
=B	\B	�B	 �B	+B	49B	M�B	Q�B	Q�B	R�B	=qB	�B��B		7B	{B	uB	VB��B�B�5B�B�B�BƨB�B��B��B��B��B��B�'B�wBĜB��B�B��B��B��BǮB�XB�9B�3B�?B�qB��BƨBĜB��B��B��B��B�B��B�B�B�#B�B��B��B�B��B�
B�)B�#B�5B�;B�NB�NB�HB�BB�NB�ZB�mB��B��B��B��B��B��B��B��B��B��B��B��B	  B	+B	%B	B	B	B	
=B	JB	DB	
=B	DB	bB	hB	uB	�B	)�B	0!B	1'B	6FB	8RB	9XB	6FB	5?B	5?B	8RB	:^B	=qB	=qB	>wB	C�B	F�B	F�B	G�B	K�B	M�B	N�B	N�B	M�B	Q�B	VB	XB	YB	YB	YB	ZB	\)B	^5B	bNB	dZB	bNB	^5B	ZB	[#B	\)B	[#B	XB	P�B	O�B	R�B	T�B	YB	\)B	\)B	_;B	`BB	dZB	n�B	v�B	|�B	~�B	~�B	� B	� B	� B	�B	�B	�B	�B	�B	�B	�B	�1B	�\B	�oB	�hB	�uB	�{B	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�9B	�9B	�?B	�RB	�XB	�?B	�?B	�^B	�RB	�?B	�3B	�?B	�XB	�^B	�^B	�dB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�)B	�/B	�)B	�#B	�B	�B	�B	�B	�B	�B	�#B	�)B	�/B	�5B	�/B	�5B	�5B	�5B	�;B	�BB	�NB	�TB	�ZB	�`B	�`B	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
%B
+B
+B
%B
%B
%B
%B
%B
%B
%B
%B
+B
1B

=B

=B

=B

=B

=B
1B
+B
+B
1B
+B
%B
%B
%B
+B
DB
\B
hB
oB
oB
oB
oB
oB
oB
oB
oB
oB
oB
hB
bB
\B
VB
PB
PB
PB
PB
PB
VB
\B
bB
bB
oB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
 �B
!�B
1'B
;J222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  B�'B�'B�'B�'B�'B�'B�'B�'B�-B�'B�'B�'B�'B�'B�'B�'B�'B�'B�'B�-B�-B�'B�'B�dB��B�B	:^B	T�B	m�B	�B	�fB
5?B
\)B
��B
��B
�^B
�B+B\B�B�B/B;dBB�BG�BO�BZB]/Be`Bn�B}�B�hB��BƨB��B�;B�`B�B��BJBoB �B�B�B�BJBB�yB��B��B�B�bBz�BZB[#Bm�Bo�Bs�BL�BG�B@�B7LB,B�BoBB
�B
B
��B
�hB
}�B
jB
Q�B
@�B
!�B	��B	�B	�{B	v�B	gmB	_;B	XB	K�B	-B	PB�B�B��B�B��B��B�B�!B�RB�qBBɺB��B��B��B�B�)B�HB�sB��B	B	
=B	\B	�B	 �B	+B	49B	M�B	Q�B	Q�B	R�B	=qB	�B��B		7B	{B	uB	VB��B�B�5B�B�B�BƨB�B��B��B��B��B��B�'B�wBĜB��B�B��B��B��BǮB�XB�9B�3B�?B�qB��BƨBĜB��B��B��B��B�B��B�B�B�#B�B��B��B�B��B�
B�)B�#B�5B�;B�NB�NB�HB�BB�NB�ZB�mB��B��B��B��B��B��B��B��B��B��B��B��B	  B	+B	%B	B	B	B	
=B	JB	DB	
=B	DB	bB	hB	uB	�B	)�B	0!B	1'B	6FB	8RB	9XB	6FB	5?B	5?B	8RB	:^B	=qB	=qB	>wB	C�B	F�B	F�B	G�B	K�B	M�B	N�B	N�B	M�B	Q�B	VB	XB	YB	YB	YB	ZB	\)B	^5B	bNB	dZB	bNB	^5B	ZB	[#B	\)B	[#B	XB	P�B	O�B	R�B	T�B	YB	\)B	\)B	_;B	`BB	dZB	n�B	v�B	|�B	~�B	~�B	� B	� B	� B	�B	�B	�B	�B	�B	�B	�B	�1B	�\B	�oB	�hB	�uB	�{B	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�9B	�9B	�?B	�RB	�XB	�?B	�?B	�^B	�RB	�?B	�3B	�?B	�XB	�^B	�^B	�dB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�)B	�/B	�)B	�#B	�B	�B	�B	�B	�B	�B	�#B	�)B	�/B	�5B	�/B	�5B	�5B	�5B	�;B	�BB	�NB	�TB	�ZB	�`B	�`B	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
%B
+B
+B
%B
%B
%B
%B
%B
%B
%B
%B
+B
1B

=B

=B

=B

=B

=B
1B
+B
+B
1B
+B
%B
%B
%B
+B
DB
\B
hB
oB
oB
oB
oB
oB
oB
oB
oB
oB
oB
hB
bB
\B
VB
PB
PB
PB
PB
PB
VB
\B
bB
bB
oB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
 �B
!�B
1'B
;J222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.26 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190614                              AO  ARCAADJP                                                                    20181005190614    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190614  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190614  QCF$                G�O�G�O�G�O�8000            