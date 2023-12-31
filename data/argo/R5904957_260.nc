CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-24T14:08:54Z creation      
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20181024140854  20181024140854  5904957 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL              A   AO  6560                            2B  A   APEX                            7471                            062512                          846 @�����1   @�𥪪�v@5��^5?}�c웥�S�1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                     A   A   A   @�  @�  A   A   A@  A^ffA�  A���A�  A�33A�33A�  A�  A�  A�33B  B  B��B   B(  B0  B8  B@  BH  BP  BXffB`  Bh  Bp  Bw��B��B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C�C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>�C@  CB  CD�CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv�Cx  Cz  C|�C~  C�  C�  C�  C�  C��3C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C��3C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C��C��C�  C��3C�  C��C�  C��3C��3C�  D   D �fD  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D�fD	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� D  D� D��D� DfD�fD  D� D  D� D  D� D  D� D  D� D  D� D��D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%�fD&  D&� D'  D'� D(fD(�fD)fD)� D*fD*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D1��D2� D3  D3� D3��D4� D5  D5� D6  D6� D7  D7� D8fD8� D9fD9� D:  D:� D;  D;� D;��D<y�D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DF��DG� DHfDH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DPfDP� DQfDQ� DR  DRy�DS  DSy�DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D^��D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� Dd��De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dj��Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� DrfDr�fDs  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dw�fDy�\D�5q11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�Q�@�Q�A(�A$(�AD(�Ab�\A�{A��HA�{A�G�A�G�A�{A�{A�{B ��B	
=B
=B��B!
=B)
=B1
=B9
=BA
=BI
=BQ
=BYp�Ba
=Bi
=Bq
=Bx��B�Q�B�Q�B��B��B��B��B��B��B��B��B��B��B��RB��RB��B�Q�B��BąBȅB̅BЅBԅB؅B܅B��B�B�B�B��B��RB��B��C B�CB�CB�CB�CB�C
B�CB�CB�CB�CB�CB�CB�C\)CB�CB�CB�C B�C"B�C$B�C&B�C(B�C*B�C,B�C.B�C0B�C2B�C4B�C6B�C8B�C:B�C<B�C>\)C@B�CBB�CD\)CFB�CHB�CJB�CLB�CNB�CPB�CRB�CTB�CVB�CXB�CZB�C\B�C^B�C`B�CbB�CdB�CfB�ChB�CjB�ClB�CnB�CpB�CrB�CtB�Cv\)CxB�CzB�C|\)C~B�C�!HC�!HC�!HC�!HC�{C�!HC�!HC�{C�{C�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�.C�.C�!HC�!HC�!HC�!HC�!HC�!HC�!HC�{C�{C�!HC�!HC�{C�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�{C�!HC�!HC�{C�!HC�.C�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�.C�!HC�!HC�!HC�.C�.C�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�{C�!HC�!HC�!HC�!HC�.C�.C�!HC�{C�!HC�.C�!HC�{C�{C�!HD �D �
D�D��D�D��D�D��D�D��D�D��D�D��D�D�>D�D�
D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D�>D�D��D�D��D�D��D
>D��D
D�
D�D��D�D��D�D��D�D��D�D��D�D��D
>D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%�
D&�D&��D'�D'��D(
D(�
D)
D)��D*
D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2
>D2��D3�D3��D4
>D4��D5�D5��D6�D6��D7�D7��D8
D8��D9
D9��D:�D:��D;�D;��D<
>D<�>D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG
>DG��DH
DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP
DP��DQ
DQ��DR�DR�>DS�DS�>DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_
>D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De
>De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk
>Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr
Dr�
Ds�Ds��Dt�Dt��Du�Du��Dv�Dv��Dw�Dw��Dw�
Dy� D�=�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�oA��A�{A�oA�$�A�(�A�(�A�-A�/A�1'A�7LA�=qA�=qA�=qA�?}A�?}A�;dA�;dA�=qA�=qA�9XA�;dA�1'A��^A�n�A�9XA��A�A��/A���A��9A���A�~�A�r�A�S�A�{A���A��-A��+A��`A��;A�$�A�M�A�bA���A�/A�=qA���A�I�A���A��A�7LA�n�A�A�oA�|�A�^5A�A�A��#A��A�n�A�ĜA��HA��DA�5?A�v�A�Q�A���A��DA���A�G�A�ĜA��A��A�bA��A��`A� �A���A��A�\)A���A�%A��jA�
=A��A�?}A���A���A���A��hA��A��A�oA�=qA��A~v�A~JA}�
A}p�A{�#A{C�AwK�AtffAsC�Apz�Al�uAl=qAlAk�PAi7LAf��AfI�Af  Ac��Ab�A`�HA]�mAX��AWoAVn�AU�
AUK�AT�ASoAPȴAK�AI��AGƨAEx�AA��A>�RA>bA=A=��A=|�A=K�A=VA<r�A<$�A;��A:��A97LA7+A4�jA4$�A3�A3��A3t�A2r�A1��A0~�A/?}A.JA,�DA+��A*�A%p�A"{A!�#A!�-A!"�A  �AA�A�A�Ap�A��A�;AbAS�A��A��AZAp�A�yA��A��AA�A�7A/AVA�RAVA��AȴAz�A��A
�uA
�\A
bA��A�hA/A�HA?}A�!Az�AbAp�A�9A9XA�A��A��AO�A �HA bN@�l�@�M�@��u@�@���@���@�j@�1'@��
@�|�@�;d@�"�@�@�=q@��y@�F@��y@��@�^5@��#@�@�hs@�O�@�b@���@�A�@���@�l�@�{@� �@ڧ�@�7L@�\)@Ցh@ԋD@�+@���@�@�v�@�@ёh@щ7@�?}@��m@��@͉7@���@�r�@��@�+@�@�ȴ@ʸR@�~�@�{@���@ǍP@��@�Z@��
@�K�@���@�{@��@���@��@��!@�J@�x�@���@���@�z�@�bN@�1'@��@�t�@���@���@�-@�V@��@��+@��@�O�@��@��j@��9@�1'@�o@��!@�n�@���@���@�`B@�V@��j@��D@�"�@��7@�V@�bN@���@���@��@���@���@���@�ƨ@� �@�(�@��@��m@���@�dZ@�C�@�+@��@�v�@��@���@�?}@��@�r�@�1'@��@���@�5?@���@���@���@�Z@�bN@���@��@���@�"�@�
=@��H@���@��!@��!@���@���@���@��\@���@���@�hs@��@���@��@���@��
@�t�@�S�@���@�
=@��m@�dZ@�+@�~�@�G�@���@��@��u@��j@�z�@�1'@�9X@�(�@���@��;@���@��@��P@�+@���@�ff@�E�@���@���@���@���@���@��@���@�9X@�ƨ@�K�@��R@�ff@�E�@�-@��T@�O�@�&�@��@�V@��/@��/@���@�Ĝ@�bN@���@�C�@�5?@��@�@��7@�O�@�?}@���@��j@��
@���@�E�@���@���@��D@��@��F@�S�@�33@��@���@�ff@�$�@�J@��-@��7@�`B@�G�@�%@�Ĝ@��u@�Z@� �@��w@��F@�l�@�S�@�33@��@��@���@�V@�E�@�=q@��@���@��h@�G�@�%@��@�j@�b@���@��;@��@��
@���@�C�@���@���@���@�
=@��y@��R@���@�
=@�@��R@��!@���@�$�@���@���@���@���@��@���@�Q�@�@\)@~�y@~ff@~$�@|-�@p�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�oA��A�{A�oA�$�A�(�A�(�A�-A�/A�1'A�7LA�=qA�=qA�=qA�?}A�?}A�;dA�;dA�=qA�=qA�9XA�;dA�1'A��^A�n�A�9XA��A�A��/A���A��9A���A�~�A�r�A�S�A�{A���A��-A��+A��`A��;A�$�A�M�A�bA���A�/A�=qA���A�I�A���A��A�7LA�n�A�A�oA�|�A�^5A�A�A��#A��A�n�A�ĜA��HA��DA�5?A�v�A�Q�A���A��DA���A�G�A�ĜA��A��A�bA��A��`A� �A���A��A�\)A���A�%A��jA�
=A��A�?}A���A���A���A��hA��A��A�oA�=qA��A~v�A~JA}�
A}p�A{�#A{C�AwK�AtffAsC�Apz�Al�uAl=qAlAk�PAi7LAf��AfI�Af  Ac��Ab�A`�HA]�mAX��AWoAVn�AU�
AUK�AT�ASoAPȴAK�AI��AGƨAEx�AA��A>�RA>bA=A=��A=|�A=K�A=VA<r�A<$�A;��A:��A97LA7+A4�jA4$�A3�A3��A3t�A2r�A1��A0~�A/?}A.JA,�DA+��A*�A%p�A"{A!�#A!�-A!"�A  �AA�A�A�Ap�A��A�;AbAS�A��A��AZAp�A�yA��A��AA�A�7A/AVA�RAVA��AȴAz�A��A
�uA
�\A
bA��A�hA/A�HA?}A�!Az�AbAp�A�9A9XA�A��A��AO�A �HA bN@�l�@�M�@��u@�@���@���@�j@�1'@��
@�|�@�;d@�"�@�@�=q@��y@�F@��y@��@�^5@��#@�@�hs@�O�@�b@���@�A�@���@�l�@�{@� �@ڧ�@�7L@�\)@Ցh@ԋD@�+@���@�@�v�@�@ёh@щ7@�?}@��m@��@͉7@���@�r�@��@�+@�@�ȴ@ʸR@�~�@�{@���@ǍP@��@�Z@��
@�K�@���@�{@��@���@��@��!@�J@�x�@���@���@�z�@�bN@�1'@��@�t�@���@���@�-@�V@��@��+@��@�O�@��@��j@��9@�1'@�o@��!@�n�@���@���@�`B@�V@��j@��D@�"�@��7@�V@�bN@���@���@��@���@���@���@�ƨ@� �@�(�@��@��m@���@�dZ@�C�@�+@��@�v�@��@���@�?}@��@�r�@�1'@��@���@�5?@���@���@���@�Z@�bN@���@��@���@�"�@�
=@��H@���@��!@��!@���@���@���@��\@���@���@�hs@��@���@��@���@��
@�t�@�S�@���@�
=@��m@�dZ@�+@�~�@�G�@���@��@��u@��j@�z�@�1'@�9X@�(�@���@��;@���@��@��P@�+@���@�ff@�E�@���@���@���@���@���@��@���@�9X@�ƨ@�K�@��R@�ff@�E�@�-@��T@�O�@�&�@��@�V@��/@��/@���@�Ĝ@�bN@���@�C�@�5?@��@�@��7@�O�@�?}@���@��j@��
@���@�E�@���@���@��D@��@��F@�S�@�33@��@���@�ff@�$�@�J@��-@��7@�`B@�G�@�%@�Ĝ@��u@�Z@� �@��w@��F@�l�@�S�@�33@��@��@���@�V@�E�@�=q@��@���@��h@�G�@�%@��@�j@�b@���@��;@��@��
@���@�C�@���@���@���@�
=@��y@��R@���@�
=@�@��R@��!@���@�$�@���@���@���@���@��@���@�Q�@�@\)@~�y@~ff@~$�@|-�@p�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BoBoBoBoBoBoBoBoBoBuB{B�B�B�B�B�B�B�B�B�B"�B-B5?BQ�BZB^5BaHBcTBgmBiyBjBk�Bl�Bm�Bm�Bm�Bl�BjBgmBhsBjBq�B~�B~�B~�B� B�B}�Bz�By�Bw�Bt�Br�Bl�BgmBcTB]/BVBR�BK�BE�B>wB5?B0!B+B!�B�BbB+B��B�`B�/B��B�}B��B��B�B��B��B��B�+Bk�B_;BN�BD�B6FB�BB
�B
��B
�9B
��B
��B
�B
w�B
e`B
[#B
XB
W
B
S�B
J�B
E�B
-B
#�B
�B
oB	��B	��B	��B	�B	�NB	��B	��B	��B	��B	�LB	��B	��B	|�B	q�B	m�B	jB	ffB	`BB	XB	D�B	)�B	#�B	�B	\B	  B�B�B�B�B�B�B�B�yB�mB�ZB�BB�B��BɺBǮBǮBƨBĜBB�wB�^B�9B�!B�B�B��B��B�JB�DB�=B�1B�%B�B� B}�Bz�Bx�Bu�Bu�Br�Br�Bs�Bs�Bv�Bt�Bs�Br�Bp�Bn�Bm�Bn�Bl�BjBiyBiyBhsBiyBjBk�Bk�BjBjBjBiyBiyBgmBffBe`BcTBaHB_;B]/B\)B[#BZBYBXBZB[#BYBZBYBYBYBZBZBZBZBZBZBZB]/BffBhsBhsBhsBjBn�Bq�Bs�Bw�Bw�Bt�Bs�Br�Bs�Bs�Bs�Bu�Bz�B� B~�B�B�7B�PB�\B�VB�VB��B��B��B�B�'B�3B�9B�?B�FB�FB�FB�FB�FB�LB�^B�wBǮBɺB��B��B��B��B��B�#B�/B�5B�HB�TB�fB�fB�mB�mB�sB�yB�B�B�B�B��B��B��B��B	B	B	1B		7B	PB	oB	�B	�B	�B	�B	�B	 �B	"�B	"�B	'�B	,B	+B	,B	-B	-B	.B	0!B	0!B	1'B	6FB	@�B	B�B	H�B	J�B	K�B	M�B	N�B	P�B	S�B	VB	T�B	VB	VB	T�B	T�B	S�B	T�B	XB	XB	XB	XB	ZB	\)B	]/B	^5B	aHB	cTB	dZB	dZB	e`B	ffB	ffB	ffB	ffB	ffB	ffB	gmB	k�B	m�B	o�B	p�B	q�B	r�B	t�B	w�B	w�B	w�B	x�B	{�B	�+B	�1B	�1B	�%B	�B	�B	�B	�%B	�7B	�JB	�\B	�\B	�bB	�hB	�oB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�'B	�-B	�-B	�-B	�3B	�3B	�3B	�3B	�FB	�^B	�dB	�}B	��B	��B	��B	B	ÖB	ĜB	ĜB	ŢB	ŢB	ŢB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�#B	�#B	�)B	�/B	�5B	�;B	�BB	�NB	�ZB	�mB	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
@11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   BoBoBoBoBoBoBoBoBoBuB{B�B�B�B�B�B�B�B�B�B"�B-B5?BQ�BZB^5BaHBcTBgmBiyBjBk�Bl�Bm�Bm�Bm�Bl�BjBgmBhsBjBq�B~�B~�B~�B� B�B}�Bz�By�Bw�Bt�Br�Bl�BgmBcTB]/BVBR�BK�BE�B>wB5?B0!B+B!�B�BbB+B��B�`B�/B��B�}B��B��B�B��B��B��B�+Bk�B_;BN�BD�B6FB�BB
�B
��B
�9B
��B
��B
�B
w�B
e`B
[#B
XB
W
B
S�B
J�B
E�B
-B
#�B
�B
oB	��B	��B	��B	�B	�NB	��B	��B	��B	��B	�LB	��B	��B	|�B	q�B	m�B	jB	ffB	`BB	XB	D�B	)�B	#�B	�B	\B	  B�B�B�B�B�B�B�B�yB�mB�ZB�BB�B��BɺBǮBǮBƨBĜBB�wB�^B�9B�!B�B�B��B��B�JB�DB�=B�1B�%B�B� B}�Bz�Bx�Bu�Bu�Br�Br�Bs�Bs�Bv�Bt�Bs�Br�Bp�Bn�Bm�Bn�Bl�BjBiyBiyBhsBiyBjBk�Bk�BjBjBjBiyBiyBgmBffBe`BcTBaHB_;B]/B\)B[#BZBYBXBZB[#BYBZBYBYBYBZBZBZBZBZBZBZB]/BffBhsBhsBhsBjBn�Bq�Bs�Bw�Bw�Bt�Bs�Br�Bs�Bs�Bs�Bu�Bz�B� B~�B�B�7B�PB�\B�VB�VB��B��B��B�B�'B�3B�9B�?B�FB�FB�FB�FB�FB�LB�^B�wBǮBɺB��B��B��B��B��B�#B�/B�5B�HB�TB�fB�fB�mB�mB�sB�yB�B�B�B�B��B��B��B��B	B	B	1B		7B	PB	oB	�B	�B	�B	�B	�B	 �B	"�B	"�B	'�B	,B	+B	,B	-B	-B	.B	0!B	0!B	1'B	6FB	@�B	B�B	H�B	J�B	K�B	M�B	N�B	P�B	S�B	VB	T�B	VB	VB	T�B	T�B	S�B	T�B	XB	XB	XB	XB	ZB	\)B	]/B	^5B	aHB	cTB	dZB	dZB	e`B	ffB	ffB	ffB	ffB	ffB	ffB	gmB	k�B	m�B	o�B	p�B	q�B	r�B	t�B	w�B	w�B	w�B	x�B	{�B	�+B	�1B	�1B	�%B	�B	�B	�B	�%B	�7B	�JB	�\B	�\B	�bB	�hB	�oB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�'B	�-B	�-B	�-B	�3B	�3B	�3B	�3B	�FB	�^B	�dB	�}B	��B	��B	��B	B	ÖB	ĜB	ĜB	ŢB	ŢB	ŢB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�#B	�#B	�)B	�/B	�5B	�;B	�BB	�NB	�ZB	�mB	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
@11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.26 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181024140854                              AO  ARCAADJP                                                                    20181024140854    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181024140854  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181024140854  QCF$                G�O�G�O�G�O�0               