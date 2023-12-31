CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:17:57Z creation      
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20181005191757  20181005191757  5904954 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL              +A   AO  6557                            2B  A   APEX                            7468                            062512                          846 @��$�LMb1   @��%q�/&@5;"��`B�d�-1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                     +A   A   A   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C�fC!�fC$  C&  C(�C*  C,  C.  C0  C2  C4�C6  C7�fC:  C<  C>  C@  CB  CD  CE�fCG�fCI�fCL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd�Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~�C�  C��C��C�  C��C��C�  C�  C�  C��C�  C��3C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C�  C��C��C�  C��C�  C�  C�  C�  C��3C��3C��3C�  C�  C��3C��3C��3C�  C��3C�  C��C�  C�  C��3C��C��C�  C�  C��C�  C�  C��C�  C��C��C�  C��3C��3C��3C��3C�  C��C�  C�  C��C��C�  C�  C��C��C��3C�  C��3C�  C�  C�  C�  C��3C��3C��C��3C�  C��C��3C��C��C�  C�  C��3D   D �fD  D� D  Ds3D��D� D  D�fD  D�fD��D� D  Dy�D  D� D	  D	� D
  D
� D  Dy�D��D� D  D�fDfDy�DfD�fD  D� D��D�fD  D�fD  Dy�D��D�fDfD� D��D� DfDy�DfDy�D  D� D  D� D��D� D  D�fD  D� DfD�fD  D� D fD � D!fD!� D!��D"s3D"��D#�fD$fD$y�D%  D%� D%��D&� D'  D'� D(  D(� D)  D)y�D)��D*� D+fD+�fD,  D,y�D-  D-� D.  D.� D/  D/s3D/�3D0y�D0��D1� D2  D2� D3  D3�fD4fD4� D5  D5y�D6  D6�fD6��D7� D8  D8s3D8��D9� D:  D:�fD;  D;� D<fD<y�D=  D=� D>fD>� D?fD?y�D?��D@y�DA  DA�fDB  DB� DCfDCy�DD  DDy�DE  DE�fDF  DF�fDF��DGy�DH  DH� DIfDI�fDJ  DJ� DKfDK� DLfDL� DM  DM� DN  DN� DN��DO� DPfDP� DQ  DQ�fDR  DR� DS  DS� DT  DT� DU  DU� DV  DV�fDW  DW� DX  DX� DY  DY� DZ  DZy�D[fD[�fD[��D\y�D]  D]� D^  D^� D_fD_� D_��D`� Da  Da� Db  Dby�Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg�fDh  Dh�fDifDi� DjfDj�fDk  Dk� Dl  Dl� DmfDm� Dn  Dn� Do  Doy�Dp  Dp� Dq  Dq�fDr  Dr� Ds  Ds� Dt  Dt�fDu  Du� Dv  Dv� Dw  Dwy�Dw�fDy�RD�K�D��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��R@�Q�A(�A$(�AD(�Ad(�A�{A�{A�{A�{A�{A�{A�{A�{B
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
=B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��BąBȅB̅BЅBԅB؅B܅B��B�B�B�B��B�B��B��C B�CB�CB�CB�CB�C
B�CB�CB�CB�CB�CB�CB�CB�CB�CB�CB�C (�C"(�C$B�C&B�C(\)C*B�C,B�C.B�C0B�C2B�C4\)C6B�C8(�C:B�C<B�C>B�C@B�CBB�CDB�CF(�CH(�CJ(�CLB�CNB�CPB�CRB�CTB�CVB�CXB�CZB�C\B�C^B�C`B�CbB�Cd\)CfB�ChB�CjB�ClB�CnB�CpB�CrB�CtB�CvB�CxB�CzB�C|B�C~\)C�!HC�.C�.C�!HC�.C�.C�!HC�!HC�!HC�.C�!HC�{C�!HC�.C�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�{C�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�.C�.C�.C�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�{C�!HC�.C�!HC�.C�.C�!HC�.C�!HC�!HC�!HC�!HC�{C�{C�{C�!HC�!HC�{C�{C�{C�!HC�{C�!HC�.C�!HC�!HC�{C�.C�.C�!HC�!HC�.C�!HC�!HC�.C�!HC�.C�.C�!HC�{C�{C�{C�{C�!HC�.C�!HC�!HC�.C�.C�!HC�!HC�.C�.C�{C�!HC�{C�!HC�!HC�!HC�!HC�{C�{C�.C�{C�!HC�.C�{C�.C�.C�!HC�!HC�{D �D �
D�D��D�D��D
>D��D�D�
D�D�
D
>D��D�D�>D�D��D	�D	��D
�D
��D�D�>D
>D��D�D�
D
D�>D
D�
D�D��D
>D�
D�D�
D�D�>D
>D�
D
D��D
>D��D
D�>D
D�>D�D��D�D��D
>D��D�D�
D�D��D
D�
D�D��D 
D ��D!
D!��D"
>D"��D#
>D#�
D$
D$�>D%�D%��D&
>D&��D'�D'��D(�D(��D)�D)�>D*
>D*��D+
D+�
D,�D,�>D-�D-��D.�D.��D/�D/��D0�D0�>D1
>D1��D2�D2��D3�D3�
D4
D4��D5�D5�>D6�D6�
D7
>D7��D8�D8��D9
>D9��D:�D:�
D;�D;��D<
D<�>D=�D=��D>
D>��D?
D?�>D@
>D@�>DA�DA�
DB�DB��DC
DC�>DD�DD�>DE�DE�
DF�DF�
DG
>DG�>DH�DH��DI
DI�
DJ�DJ��DK
DK��DL
DL��DM�DM��DN�DN��DO
>DO��DP
DP��DQ�DQ�
DR�DR��DS�DS��DT�DT��DU�DU��DV�DV�
DW�DW��DX�DX��DY�DY��DZ�DZ�>D[
D[�
D\
>D\�>D]�D]��D^�D^��D_
D_��D`
>D`��Da�Da��Db�Db�>Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg�
Dh�Dh�
Di
Di��Dj
Dj�
Dk�Dk��Dl�Dl��Dm
Dm��Dn�Dn��Do�Do�>Dp�Dp��Dq�Dq�
Dr�Dr��Ds�Ds��Dt�Dt�
Du�Du��Dv�Dv��Dw�Dw�>Dw�
Dy��D�S�D�� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�~�AăAāA�jA�p�A�Aã�A�/A���A���A���A��A��RA�ffA�A�dZA�&�A��A��hA�t�A�v�A�x�A��A�M�A���A�n�A�1'A�33A�=qA�?}A���A��
A��A�dZA�bA��yA��
A��
A��A��A��
A�ȴA��9A���A��A�^5A�&�A��mA�v�A�E�A� �A��A��RA�^5A��A��FA�|�A�l�A�E�A��A��
A��A�=qA��hA�E�A��A�O�A��HA��9A�
=A�&�A�&�A�{A��A���A�;dA��
A�v�A��^A�bNA��A��
A�dZA�;dA��A��HA���A��A���A��9A�ĜA�ffA���A��A���A��A��wA�E�A�VA�;dA�-A���A�O�A��!A��A��^A�Q�A�JA��jA�hsA�x�A��RA�ZA�VA���A�/A��A�I�A��\A��A�;dA���A��A�=qA�A�A��A�\)A��HA�;dA���A�|�A33Azr�Aw��Ar=qAq�-Ap�`Ao�wAk��Ajz�Ah�jAh�Agx�Af�jAf  Ad{Ab��Aa��Aa�A`�\A^��A]�^A\VAZ�AW�FAV��AV~�AV1'ATAR��AQ�;AQ+APE�AN�AM��AK�
AJ��AH�AF��AF��AF�AFJAEG�AA�FA@ �A>ĜA>�A=;dA;�A9�A6��A5�A4�A3ƨA1�TA0�A-�hA,E�A+
=A)l�A(�yA(��A(-A'7LA%�A$VA"�HA!�A $�A=qA&�AdZAXAAVAZAƨA(�A��AI�AK�A�9AZA9XA{A�hA&�A�`A?}AZA��A`BA/A
�A
$�A^5AK�A��Al�A�A��A �@��@�n�@�=q@��@�{@���@�%@��@�ȴ@��@��7@�r�@�
=@�@�hs@��/@�9X@��;@�"�@�ff@�@���@��@�7@��@�@߾w@�p�@��@��@�5?@��@�z�@� �@��m@�S�@֟�@��@�ȴ@�-@���@�{@̬@�ƨ@˕�@�t�@ʇ+@�O�@��#@ɺ^@�x�@�G�@��;@Ə\@�M�@�J@�G�@� �@��m@��m@�@�O�@�|�@�5?@�Ĝ@���@�+@��@�=q@��/@��/@�ƨ@�t�@��@�@��h@�hs@��/@�r�@��;@��P@�C�@���@�M�@�x�@�%@���@�bN@�9X@��@�t�@���@�;d@�o@���@��H@���@���@��!@���@���@���@���@�(�@���@�-@��@��-@��@���@�Z@�b@��w@�t�@�o@��R@���@��+@�ff@�V@�V@�=q@�@��@��@���@�dZ@�@�E�@���@��h@�O�@�/@�&�@��@�V@��@��@��@�  @��m@��
@��w@��@��P@�|�@�S�@��@���@�J@���@�hs@�`B@��@�hs@�hs@��@�hs@��`@���@��@�r�@�1'@�ƨ@�\)@�K�@�33@�
=@���@�ff@��T@��7@�G�@�7L@�/@���@��@�j@�bN@�Q�@�  @�ƨ@���@�t�@�+@���@�V@���@�@�%@��j@���@�z�@�Q�@�A�@�  @��F@���@�  @��@���@�l�@�|�@�t�@�;d@��y@���@�n�@�-@�=q@�ff@��@���@��/@�G�@��@���@���@�A�@�bN@�j@�1'@���@�t�@�t�@�t�@�+@��@��H@���@���@�ff@�$�@���@�O�@�G�@���@��j@��D@�Z@�(�@�b@��;@�|�@�o@�
=@�
=@�@��R@���@�V@�J@��#@���@���@��h@��@��@��j@�Ĝ@��u@�9X@���@�x@zl�@jq�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�~�AăAāA�jA�p�A�Aã�A�/A���A���A���A��A��RA�ffA�A�dZA�&�A��A��hA�t�A�v�A�x�A��A�M�A���A�n�A�1'A�33A�=qA�?}A���A��
A��A�dZA�bA��yA��
A��
A��A��A��
A�ȴA��9A���A��A�^5A�&�A��mA�v�A�E�A� �A��A��RA�^5A��A��FA�|�A�l�A�E�A��A��
A��A�=qA��hA�E�A��A�O�A��HA��9A�
=A�&�A�&�A�{A��A���A�;dA��
A�v�A��^A�bNA��A��
A�dZA�;dA��A��HA���A��A���A��9A�ĜA�ffA���A��A���A��A��wA�E�A�VA�;dA�-A���A�O�A��!A��A��^A�Q�A�JA��jA�hsA�x�A��RA�ZA�VA���A�/A��A�I�A��\A��A�;dA���A��A�=qA�A�A��A�\)A��HA�;dA���A�|�A33Azr�Aw��Ar=qAq�-Ap�`Ao�wAk��Ajz�Ah�jAh�Agx�Af�jAf  Ad{Ab��Aa��Aa�A`�\A^��A]�^A\VAZ�AW�FAV��AV~�AV1'ATAR��AQ�;AQ+APE�AN�AM��AK�
AJ��AH�AF��AF��AF�AFJAEG�AA�FA@ �A>ĜA>�A=;dA;�A9�A6��A5�A4�A3ƨA1�TA0�A-�hA,E�A+
=A)l�A(�yA(��A(-A'7LA%�A$VA"�HA!�A $�A=qA&�AdZAXAAVAZAƨA(�A��AI�AK�A�9AZA9XA{A�hA&�A�`A?}AZA��A`BA/A
�A
$�A^5AK�A��Al�A�A��A �@��@�n�@�=q@��@�{@���@�%@��@�ȴ@��@��7@�r�@�
=@�@�hs@��/@�9X@��;@�"�@�ff@�@���@��@�7@��@�@߾w@�p�@��@��@�5?@��@�z�@� �@��m@�S�@֟�@��@�ȴ@�-@���@�{@̬@�ƨ@˕�@�t�@ʇ+@�O�@��#@ɺ^@�x�@�G�@��;@Ə\@�M�@�J@�G�@� �@��m@��m@�@�O�@�|�@�5?@�Ĝ@���@�+@��@�=q@��/@��/@�ƨ@�t�@��@�@��h@�hs@��/@�r�@��;@��P@�C�@���@�M�@�x�@�%@���@�bN@�9X@��@�t�@���@�;d@�o@���@��H@���@���@��!@���@���@���@���@�(�@���@�-@��@��-@��@���@�Z@�b@��w@�t�@�o@��R@���@��+@�ff@�V@�V@�=q@�@��@��@���@�dZ@�@�E�@���@��h@�O�@�/@�&�@��@�V@��@��@��@�  @��m@��
@��w@��@��P@�|�@�S�@��@���@�J@���@�hs@�`B@��@�hs@�hs@��@�hs@��`@���@��@�r�@�1'@�ƨ@�\)@�K�@�33@�
=@���@�ff@��T@��7@�G�@�7L@�/@���@��@�j@�bN@�Q�@�  @�ƨ@���@�t�@�+@���@�V@���@�@�%@��j@���@�z�@�Q�@�A�@�  @��F@���@�  @��@���@�l�@�|�@�t�@�;d@��y@���@�n�@�-@�=q@�ff@��@���@��/@�G�@��@���@���@�A�@�bN@�j@�1'@���@�t�@�t�@�t�@�+@��@��H@���@���@�ff@�$�@���@�O�@�G�@���@��j@��D@�Z@�(�@�b@��;@�|�@�o@�
=@�
=@�@��R@���@�V@�J@��#@���@���@��h@��@��@��j@�Ĝ@��u@�9X@���@�x@zl�@jq�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B��B��B��B��B��BBVB�B�B:^BL�BM�BQ�BVB_;Bs�Bu�Bp�Bs�B}�B}�B~�B�B� B{�Bv�Bv�B�B�%B�1B�1B�DB�VB�DB�B}�B~�B�VB��B��B��B��B�B�B�'B�?B�RB�dB�wB�wB�qB�qB�jB�wB��BÖBŢBƨBȴB��B��B��B�B�/B�;B�HB�B�fB�HBBbB�B�B�B!�B!�B�B�BbB�B�BoBJBDB{B�B�BPB��B�sB�B�qB�LB��Bm�BL�B?}B:^B/B �BuBB�BȴB��B��B�1B�B|�Bv�BiyBW
BQ�BL�BF�B@�B<jB33B(�B�B1B
��B
�B
�B
ɺB
ÖB
�wB
�RB
�!B
��B
��B
�1B
gmB
VB
;dB
6FB
.B
%�B
JB	��B	�yB	�NB	�)B	�B	��B	��B	�jB	��B	�jB	�LB	�B	��B	��B	�JB	z�B	s�B	p�B	m�B	aHB	XB	Q�B	K�B	C�B	7LB	.B	"�B	�B	\B	
=B		7B	+B	B��B�B�mB�NB�HB�;B�B��B��B�^B�FB�!B��B��B��B�uB�bB�PB�DB�=B�1B�B�B� B�Bz�Bp�BjBgmBe`BcTBaHBaHB`BB]/B\)BZB\)B[#B[#B[#BZBZBYB]/B`BB]/BZB[#B\)B\)B\)B[#BZBXBZBYBW
BT�BQ�BO�BM�BM�BM�BP�BZB^5B]/BZB]/B]/B]/B_;BaHBbNBcTBffBgmBiyBk�Bm�Bn�Bn�Bn�Bl�Bl�Be`BaHBaHBaHBbNBdZBdZBe`BffBm�Bn�Bn�Bn�Bn�Bx�B~�B�B�B�B�B�B�DB��B��B��B��B��B��B��B��B��B��B�B�-B�^B��BƨB��B��B��B��B��B�B��B�B�
B�B�B�B�)B�;B�HB�HB�TB�TB�TB�ZB�ZB�`B�fB�mB�sB�B�B��B��B		7B	PB	\B	hB	hB	oB	oB	�B	�B	#�B	'�B	)�B	0!B	2-B	33B	49B	8RB	:^B	>wB	A�B	C�B	E�B	I�B	M�B	N�B	N�B	N�B	O�B	P�B	S�B	W
B	XB	YB	YB	YB	ZB	_;B	dZB	gmB	iyB	k�B	k�B	l�B	m�B	n�B	{�B	� B	�B	�B	�B	�%B	�+B	�+B	�%B	�%B	�%B	�1B	�DB	�JB	�VB	�\B	�hB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�!B	�-B	�-B	�-B	�?B	�RB	�RB	�XB	�^B	�dB	�wB	B	ĜB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�5B	�/B	�5B	�BB	�HB	�;B	�5B	�NB	�`B	�`B	�`B	�mB	�fB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
�B
�B
,�222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  B��B��B��B��B��BBVB�B�B:^BL�BM�BQ�BVB_;Bs�Bu�Bp�Bs�B}�B}�B~�B�B� B{�Bv�Bv�B�B�%B�1B�1B�DB�VB�DB�B}�B~�B�VB��B��B��B��B�B�B�'B�?B�RB�dB�wB�wB�qB�qB�jB�wB��BÖBŢBƨBȴB��B��B��B�B�/B�;B�HB�B�fB�HBBbB�B�B�B!�B!�B�B�BbB�B�BoBJBDB{B�B�BPB��B�sB�B�qB�LB��Bm�BL�B?}B:^B/B �BuBB�BȴB��B��B�1B�B|�Bv�BiyBW
BQ�BL�BF�B@�B<jB33B(�B�B1B
��B
�B
�B
ɺB
ÖB
�wB
�RB
�!B
��B
��B
�1B
gmB
VB
;dB
6FB
.B
%�B
JB	��B	�yB	�NB	�)B	�B	��B	��B	�jB	��B	�jB	�LB	�B	��B	��B	�JB	z�B	s�B	p�B	m�B	aHB	XB	Q�B	K�B	C�B	7LB	.B	"�B	�B	\B	
=B		7B	+B	B��B�B�mB�NB�HB�;B�B��B��B�^B�FB�!B��B��B��B�uB�bB�PB�DB�=B�1B�B�B� B�Bz�Bp�BjBgmBe`BcTBaHBaHB`BB]/B\)BZB\)B[#B[#B[#BZBZBYB]/B`BB]/BZB[#B\)B\)B\)B[#BZBXBZBYBW
BT�BQ�BO�BM�BM�BM�BP�BZB^5B]/BZB]/B]/B]/B_;BaHBbNBcTBffBgmBiyBk�Bm�Bn�Bn�Bn�Bl�Bl�Be`BaHBaHBaHBbNBdZBdZBe`BffBm�Bn�Bn�Bn�Bn�Bx�B~�B�B�B�B�B�B�DB��B��B��B��B��B��B��B��B��B��B�B�-B�^B��BƨB��B��B��B��B��B�B��B�B�
B�B�B�B�)B�;B�HB�HB�TB�TB�TB�ZB�ZB�`B�fB�mB�sB�B�B��B��B		7B	PB	\B	hB	hB	oB	oB	�B	�B	#�B	'�B	)�B	0!B	2-B	33B	49B	8RB	:^B	>wB	A�B	C�B	E�B	I�B	M�B	N�B	N�B	N�B	O�B	P�B	S�B	W
B	XB	YB	YB	YB	ZB	_;B	dZB	gmB	iyB	k�B	k�B	l�B	m�B	n�B	{�B	� B	�B	�B	�B	�%B	�+B	�+B	�%B	�%B	�%B	�1B	�DB	�JB	�VB	�\B	�hB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�!B	�-B	�-B	�-B	�?B	�RB	�RB	�XB	�^B	�dB	�wB	B	ĜB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�5B	�/B	�5B	�BB	�HB	�;B	�5B	�NB	�`B	�`B	�`B	�mB	�fB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
�B
�B
,�222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.26 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005191757                              AO  ARCAADJP                                                                    20181005191757    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005191757  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005191757  QCF$                G�O�G�O�G�O�8000            