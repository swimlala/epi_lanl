CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-24T14:08:15Z creation      
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20181024140815  20181024140815  5904957 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               ;A   AO  6560                            2B  A   APEX                            7471                            062512                          846 @׾e��1   @׾e���@2����m�c�;dZ�1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      ;A   A   A   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   BffBffB  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C�fC  C  C  C�fC  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@�CB�CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Ce�fCg�fCi�fCl  Cn  Cp  Cr  Ct  Cv  Cx  Cz�C|  C~  C�  C��3C�  C�  C�  C�  C��C�  C�  C�  C��C�  C��3C��3C�  C�  C�  C�  C��C��C�  C�  C�  C��3C�  C��C��C�  C�  C�  C��3C��3C�  C�  C�  C��3C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C��C�  C�  C��C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��D   D � D  D� D  D� D  D� D  D� D  D�fDfD� D  D�fD  D� D	  D	� D
  D
� D  D� D��D� D  D� D  Dy�D��D� DfD� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� DfD� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$y�D%  D%� D&fD&�fD'  D'� D(  D(� D)  D)� D*  D*� D*��D+y�D,  D,� D-  D-� D.  D.�fD/  D/� D0  D0y�D0��D1� D2  D2� D3  D3� D4  D4� D5  D5� D5��D6y�D7  D7� D7��D8y�D9  D9� D:  D:� D;  D;� D<fD<� D<��D=� D>  D>� D?  D?� D@  D@� DAfDA�fDB  DBy�DB��DCy�DC��DD� DE  DEy�DE��DF� DGfDG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DMy�DN  DN� DN��DOy�DP  DP� DQ  DQ� DR  DR� DS  DSy�DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� D`��Da� Db  Db� Dc  Dc� Dd  Dd� De  De�fDf  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Dr��Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dw�fDy��D�=qD��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@�Q�A(�A$(�AD(�Ad(�A�{A�{A�{A�{A�{A�{A�{A�{B
=B	p�Bp�B
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
=B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��RBąBȅB̅B�Q�BԅB؅B܅B��B�B�B�B��B�B��B��C B�CB�CB�CB�CB�C
B�CB�CB�C(�CB�CB�CB�C(�CB�CB�CB�C B�C"B�C$B�C&B�C(B�C*B�C,B�C.B�C0B�C2B�C4B�C6B�C8B�C:B�C<B�C>B�C@\)CB\)CDB�CFB�CHB�CJB�CLB�CNB�CPB�CRB�CTB�CVB�CXB�CZB�C\B�C^B�C`B�CbB�CdB�Cf(�Ch(�Cj(�ClB�CnB�CpB�CrB�CtB�CvB�CxB�Cz\)C|B�C~B�C�!HC�{C�!HC�!HC�!HC�!HC�.C�!HC�!HC�!HC�.C�!HC�{C�{C�!HC�!HC�!HC�!HC�.C�.C�!HC�!HC�!HC�{C�!HC�.C�.C�!HC�!HC�!HC�{C�{C�!HC�!HC�!HC�{C�!HC�!HC�.C�.C�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�{C�{C�!HC�.C�!HC�!HC�.C�!HC�{C�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�{C�!HC�!HC�!HC�{C�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�.C�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�.C�.D �D ��D�D��D�D��D�D��D�D��D�D�
D
D��D�D�
D�D��D	�D	��D
�D
��D�D��D
>D��D�D��D�D�>D
>D��D
D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D
>D��D
D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$�>D%�D%��D&
D&�
D'�D'��D(�D(��D)�D)��D*�D*��D+
>D+�>D,�D,��D-�D-��D.�D.�
D/�D/��D0�D0�>D1
>D1��D2�D2��D3�D3��D4�D4��D5�D5��D6
>D6�>D7�D7��D8
>D8�>D9�D9��D:�D:��D;�D;��D<
D<��D=
>D=��D>�D>��D?�D?��D@�D@��DA
DA�
DB�DB�>DC
>DC�>DD
>DD��DE�DE�>DF
>DF��DG
DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM�>DN�DN��DO
>DO�>DP�DP��DQ�DQ��DR�DR��DS�DS�>DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da
>Da��Db�Db��Dc�Dc��Dd�Dd��De�De�
Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds
>Ds��Dt�Dt��Du�Du��Dv�Dv��Dw�Dw��Dw�
Dy�)D�E�D��f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��A��A�A�A�bA�bA�bA�oA�oA�bA�oA�oA�oA��A��A�oA�JA�1A�JA��A��#A���A�ȴAܺ^Aܥ�A�p�A�E�A��A�ĜA�r�A�O�A�1'A�A���A��A�Aڙ�A�A�A���A�A֕�A�v�AӺ^A�33A�`BA��mAљ�A�=qA�S�A�ȴA��#A�$�Aɕ�Aǥ�A�p�A�t�A�O�A�G�A�=qA�;dAĬA�p�A�+AÑhA���A�/A��RA��A�n�A��A��DA��A�=qA��A���A�E�A��A�ZA��A���A�oA�  A���A�%A��A�A�;dA��jA��yA�jA��A��PA�S�A�ĜA��A�S�A�{A�%A��uA�^5A��FA��A��A���A��!A�n�A�ȴA��A�r�A���A��
A���A}+A|��AzĜAw|�Atn�As��Ar�`An�Am7LAmVAl1'Ak�hAkXAj�HAjA�AhȴAf�AcƨAa��A`ffA_;dA]`BA[�mAY;dAV�`ASx�AP  AL$�AI�-AIXAG��AEp�AD=qACƨAC&�ABr�AAhsA>��A=|�A=+A;�mA;�A:��A9�;A8�uA7�-A6{A4�RA2�\A/�hA-�PA+��A*bA)"�A(~�A'�A'hsA%�wA$��A#��A"��A!�^A �\AdZA��A=qA��A;dA��AbA��A
=AffA�/A��A�jA�A�A�A��An�A�wA�A�9A�RAz�A9XAA�A�A��A�A�;A�9A|�AA�-A�yA��A�-A��A
=A	O�A�9AĜAE�AĜA�A?}A�AVAdZ@��@�b@��@���@�@�@�@��@�1@���@���@◍@߅@���@�ff@�$�@ّh@Ӆ@�=q@���@̴9@� �@�`B@��
@�b@���@�1'@��`@�ȴ@�G�@��H@¸R@��#@�  @ȓu@��;@��/@��@��T@�Ĝ@�K�@ʗ�@ʗ�@�{@ɑh@�`B@�G�@���@ȋD@��
@�S�@��T@�7L@ě�@�1'@���@��m@�ƨ@Õ�@�l�@�@�v�@�{@���@�X@���@� �@�ƨ@���@���@���@�z�@�l�@�
=@���@�E�@�{@��#@��7@�X@���@��@�bN@�I�@�1'@��@���@��@�;d@�@���@�E�@��@��#@���@�`B@��@��D@�Z@�9X@���@���@��w@�C�@���@�@�@��-@��-@��-@��-@���@�x�@��/@�z�@�b@��
@���@���@�ƨ@��@�o@��R@���@�v�@�V@�=q@�{@��@���@�?}@��@�Z@�I�@�b@�33@���@�$�@��T@�x�@��@���@��D@�z�@�r�@�j@�j@�bN@�Q�@� �@�ƨ@��F@���@���@�K�@�ff@�%@���@���@��@�1'@��
@�\)@�@���@�M�@�@��@��T@�@��h@�X@��@��9@�r�@� �@��m@��w@��P@�+@��@�~�@���@��@�?}@���@��`@��/@�7L@�G�@�?}@��`@�|�@���@�V@�E�@��@�O�@��u@�1'@�  @���@��w@�t�@�C�@��@���@��+@�V@�$�@��@��#@��^@�x�@�?}@��/@��m@�@��@��\@�^5@�-@���@��@���@��#@���@���@���@���@�@��^@��^@�&�@��@�Z@�(�@�  @��;@�|�@�C�@�33@��@��@�ȴ@�ȴ@��R@��+@�n�@�M�@�5?@��@��7@�X@�&�@��j@��u@��@�r�@�I�@�9X@�1'@�1'@�(�@� �@���@���@�S�@��@��@��!@�ff@���@{�F@i#�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��A��A�A�A�bA�bA�bA�oA�oA�bA�oA�oA�oA��A��A�oA�JA�1A�JA��A��#A���A�ȴAܺ^Aܥ�A�p�A�E�A��A�ĜA�r�A�O�A�1'A�A���A��A�Aڙ�A�A�A���A�A֕�A�v�AӺ^A�33A�`BA��mAљ�A�=qA�S�A�ȴA��#A�$�Aɕ�Aǥ�A�p�A�t�A�O�A�G�A�=qA�;dAĬA�p�A�+AÑhA���A�/A��RA��A�n�A��A��DA��A�=qA��A���A�E�A��A�ZA��A���A�oA�  A���A�%A��A�A�;dA��jA��yA�jA��A��PA�S�A�ĜA��A�S�A�{A�%A��uA�^5A��FA��A��A���A��!A�n�A�ȴA��A�r�A���A��
A���A}+A|��AzĜAw|�Atn�As��Ar�`An�Am7LAmVAl1'Ak�hAkXAj�HAjA�AhȴAf�AcƨAa��A`ffA_;dA]`BA[�mAY;dAV�`ASx�AP  AL$�AI�-AIXAG��AEp�AD=qACƨAC&�ABr�AAhsA>��A=|�A=+A;�mA;�A:��A9�;A8�uA7�-A6{A4�RA2�\A/�hA-�PA+��A*bA)"�A(~�A'�A'hsA%�wA$��A#��A"��A!�^A �\AdZA��A=qA��A;dA��AbA��A
=AffA�/A��A�jA�A�A�A��An�A�wA�A�9A�RAz�A9XAA�A�A��A�A�;A�9A|�AA�-A�yA��A�-A��A
=A	O�A�9AĜAE�AĜA�A?}A�AVAdZ@��@�b@��@���@�@�@�@��@�1@���@���@◍@߅@���@�ff@�$�@ّh@Ӆ@�=q@���@̴9@� �@�`B@��
@�b@���@�1'@��`@�ȴ@�G�@��H@¸R@��#@�  @ȓu@��;@��/@��@��T@�Ĝ@�K�@ʗ�@ʗ�@�{@ɑh@�`B@�G�@���@ȋD@��
@�S�@��T@�7L@ě�@�1'@���@��m@�ƨ@Õ�@�l�@�@�v�@�{@���@�X@���@� �@�ƨ@���@���@���@�z�@�l�@�
=@���@�E�@�{@��#@��7@�X@���@��@�bN@�I�@�1'@��@���@��@�;d@�@���@�E�@��@��#@���@�`B@��@��D@�Z@�9X@���@���@��w@�C�@���@�@�@��-@��-@��-@��-@���@�x�@��/@�z�@�b@��
@���@���@�ƨ@��@�o@��R@���@�v�@�V@�=q@�{@��@���@�?}@��@�Z@�I�@�b@�33@���@�$�@��T@�x�@��@���@��D@�z�@�r�@�j@�j@�bN@�Q�@� �@�ƨ@��F@���@���@�K�@�ff@�%@���@���@��@�1'@��
@�\)@�@���@�M�@�@��@��T@�@��h@�X@��@��9@�r�@� �@��m@��w@��P@�+@��@�~�@���@��@�?}@���@��`@��/@�7L@�G�@�?}@��`@�|�@���@�V@�E�@��@�O�@��u@�1'@�  @���@��w@�t�@�C�@��@���@��+@�V@�$�@��@��#@��^@�x�@�?}@��/@��m@�@��@��\@�^5@�-@���@��@���@��#@���@���@���@���@�@��^@��^@�&�@��@�Z@�(�@�  @��;@�|�@�C�@�33@��@��@�ȴ@�ȴ@��R@��+@�n�@�M�@�5?@��@��7@�X@�&�@��j@��u@��@�r�@�I�@�9X@�1'@�1'@�(�@� �@���@���@�S�@��@��@��!@�ff@���@{�F@i#�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
�}B
�}B
�}B
�}B
�}B
�}B
�}B
�}B
�}B
�wB
�}B
�}B
�}B
ÖB
ĜB
ǮB
ɺB
��B
��B
��B
��B
�B
�B
�B
�B
�)B
�`B
�B
�B
��B
��B  B+B+B+B%B%B
=B49BS�BjB�uB��B�{B�{B��B��B��B��B��B��B��B�PBl�B{�B~�B~�B~�B|�B�+B��B�B�LBɺB�NB�B��BB+BPB �B/B8RB<jB=qB<jB;dB9XB5?B.B)�B%�B�B\B1BB��B�mB�dB�-BÖB�BɺBÖB�LB��BB�jB��B}�BgmBVB6FB�B
=B
��B
��B
�;B
�wB
��B
�7B
bNB
E�B
?}B
0!B
�B
DB
B	��B	�fB	�/B	�)B	�
B	��B	��B	��B	ȴB	�jB	�B	��B	�bB	�+B	� B	v�B	l�B	\)B	L�B	5?B	 �B	bB	B��B�B�BB�B�
B�B�B��BɺBŢBĜBBBÖBƨBŢBB�wB�RB�B��B��B��B��B��B��B�{B�oB�bB�PB�JB�=B�1B�%B�B�B~�B�B�+B�JB�DB�uB�{B�{B��B�B�-B�^B��BĜBŢB��B��B�B�B�#B�5B�HB�mB�sB�B�`B��B�LB�!B�wB�B�B�B�`B�)B�B��B��B��B��B��B��B��BǮBȴBŢB�LB�'B�B��B��B��B��B�DB�\B�JB�\B�JB�B�JB��B�bB}�BiyB_;B]/B\)B[#B\)BcTBgmBy�B��B�JB�%B�Bx�B{�B��B�B�RB��B�B�B��B��B��B��B��B��B��B��B	B	1B	JB	PB	VB	PB	PB	PB	PB	PB	PB	VB	VB	VB	\B	bB	uB	{B	�B	�B	�B	�B	�B	�B	�B	�B	 �B	"�B	$�B	'�B	)�B	+B	,B	-B	.B	2-B	33B	49B	49B	5?B	6FB	:^B	?}B	A�B	B�B	B�B	C�B	C�B	D�B	F�B	K�B	K�B	M�B	O�B	Q�B	Q�B	T�B	YB	[#B	]/B	^5B	_;B	_;B	_;B	`BB	bNB	dZB	iyB	k�B	n�B	o�B	p�B	p�B	p�B	q�B	t�B	w�B	x�B	y�B	z�B	z�B	|�B	~�B	�B	�B	�7B	�=B	�=B	�DB	�\B	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�-B	�3B	�9B	�?B	�RB	�dB	�jB	�qB	�qB	�qB	�wB	�}B	�}B	��B	��B	B	ÖB	ÖB	ĜB	ǮB	ǮB	��B	��B	��B	��B	��B	��B	��B	�B	�BB	�BB	�;B	�5B	�5B	�5B	�5B	�;B	�HB	�NB	�ZB	�ZB	�`B	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�yB	�sB	�sB	�sB	�sB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
9B
�B
0111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
�}B
�}B
�}B
�}B
�}B
�}B
�}B
�}B
�}B
�wB
�}B
�}B
�}B
ÖB
ĜB
ǮB
ɺB
��B
��B
��B
��B
�B
�B
�B
�B
�)B
�`B
�B
�B
��B
��B  B+B+B+B%B%B
=B49BS�BjB�uB��B�{B�{B��B��B��B��B��B��B��B�PBl�B{�B~�B~�B~�B|�B�+B��B�B�LBɺB�NB�B��BB+BPB �B/B8RB<jB=qB<jB;dB9XB5?B.B)�B%�B�B\B1BB��B�mB�dB�-BÖB�BɺBÖB�LB��BB�jB��B}�BgmBVB6FB�B
=B
��B
��B
�;B
�wB
��B
�7B
bNB
E�B
?}B
0!B
�B
DB
B	��B	�fB	�/B	�)B	�
B	��B	��B	��B	ȴB	�jB	�B	��B	�bB	�+B	� B	v�B	l�B	\)B	L�B	5?B	 �B	bB	B��B�B�BB�B�
B�B�B��BɺBŢBĜBBBÖBƨBŢBB�wB�RB�B��B��B��B��B��B��B�{B�oB�bB�PB�JB�=B�1B�%B�B�B~�B�B�+B�JB�DB�uB�{B�{B��B�B�-B�^B��BĜBŢB��B��B�B�B�#B�5B�HB�mB�sB�B�`B��B�LB�!B�wB�B�B�B�`B�)B�B��B��B��B��B��B��B��BǮBȴBŢB�LB�'B�B��B��B��B��B�DB�\B�JB�\B�JB�B�JB��B�bB}�BiyB_;B]/B\)B[#B\)BcTBgmBy�B��B�JB�%B�Bx�B{�B��B�B�RB��B�B�B��B��B��B��B��B��B��B��B	B	1B	JB	PB	VB	PB	PB	PB	PB	PB	PB	VB	VB	VB	\B	bB	uB	{B	�B	�B	�B	�B	�B	�B	�B	�B	 �B	"�B	$�B	'�B	)�B	+B	,B	-B	.B	2-B	33B	49B	49B	5?B	6FB	:^B	?}B	A�B	B�B	B�B	C�B	C�B	D�B	F�B	K�B	K�B	M�B	O�B	Q�B	Q�B	T�B	YB	[#B	]/B	^5B	_;B	_;B	_;B	`BB	bNB	dZB	iyB	k�B	n�B	o�B	p�B	p�B	p�B	q�B	t�B	w�B	x�B	y�B	z�B	z�B	|�B	~�B	�B	�B	�7B	�=B	�=B	�DB	�\B	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�-B	�3B	�9B	�?B	�RB	�dB	�jB	�qB	�qB	�qB	�wB	�}B	�}B	��B	��B	B	ÖB	ÖB	ĜB	ǮB	ǮB	��B	��B	��B	��B	��B	��B	��B	�B	�BB	�BB	�;B	�5B	�5B	�5B	�5B	�;B	�HB	�NB	�ZB	�ZB	�`B	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�yB	�sB	�sB	�sB	�sB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
9B
�B
0111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.26 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181024140815                              AO  ARCAADJP                                                                    20181024140815    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181024140815  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181024140815  QCF$                G�O�G�O�G�O�0               