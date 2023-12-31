CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-24T14:08:25Z creation      
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
resolution        =���   axis      Z        p  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  @�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     p  B�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J,   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     p  L   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p  Sx   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Z�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p  \�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  d4   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p  f   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p  m�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  t�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p  v�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ~<   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �4   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �D   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �H   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �X   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �\   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �`   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �dArgo profile    3.1 1.2 19500101000000  20181024140825  20181024140825  5904957 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               rA   AO  6560                            2B  A   APEX                            7471                            062512                          846 @��$��>�1   @��%��@49�"��`�c�7KƧ�1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      rA   A   B   @9��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B ffBffBffB  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�33B�33B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C�fC   C"  C$  C&  C(  C*  C,  C.  C0  C1�fC4  C6�C8�C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\�C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~�C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C��C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C��C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D fD �fD  D� D  D� D��D� DfD� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D�fDfD  D� D  D�fD  Dy�D  D� D  D� D  D� D  D� D  D� D   D y�D!  D!� D"fD"�fD#fD#�fD$  D$� D%  D%� D&  D&� D'  D'� D'��D(� D)  D)� D)��D*y�D*��D+y�D+��D,y�D-  D-� D.  D.� D/  D/� D0  D0� D1fD1� D2  D2�fD3  D3� D3��D4y�D4��D5y�D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;y�D;��D<� D=  D=y�D=��D>y�D?  D?� D@  D@� DA  DA� DB  DB� DCfDC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DKfDK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZy�DZ��D[y�D[��D\y�D]  D]� D^  D^� D_  D_� D`  D`�fDa  Da� Da��Db� Dc  Dc� Dc��Dd� De  De� DffDf� Dg  Dg�fDh  Dh� Di  Di� Di��Dj� Dk  Dk� Dl  Dl� DmfDm� Dm��Dn� Do  Do� Do��Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dw�3Dy�qD�( D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@J=q@�Q�@�Q�A(�A$(�AD(�Ad(�A�{A�{A�{A�{A�{A�{A�{A�{Bp�B	p�Bp�B
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
=B��B��B��B��B�Q�B��B��B��B��B��B��B��B��B��B��B��B��BąBȅB̅BЅBԅB؅B܅B��B�B�Q�B�B��B��RB��RB��C B�CB�CB�CB�CB�C
B�CB�CB�CB�CB�CB�CB�CB�CB�CB�C(�C B�C"B�C$B�C&B�C(B�C*B�C,B�C.B�C0B�C2(�C4B�C6\)C8\)C:B�C<B�C>B�C@B�CBB�CDB�CFB�CHB�CJB�CLB�CNB�CPB�CRB�CTB�CVB�CXB�CZB�C\\)C^B�C`B�CbB�CdB�CfB�ChB�CjB�ClB�CnB�CpB�CrB�CtB�CvB�CxB�CzB�C|B�C~\)C�.C�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�{C�{C�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�{C�{C�!HC�.C�!HC�!HC�!HC�!HC�.C�!HC�!HC�!HC�!HC�!HC�!HC�!HC�.C�!HC�!HC�!HC�{C�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�{C�!HC�!HC�!HC�!HC�!HC�!HC�!HC�.C�.C�.C�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�.C�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�{C�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HD 
D �
D�D��D�D��D
>D��D
D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D�
D
D�D��D�D�
D�D�>D�D��D�D��D�D��D�D��D�D��D �D �>D!�D!��D"
D"�
D#
D#�
D$�D$��D%�D%��D&�D&��D'�D'��D(
>D(��D)�D)��D*
>D*�>D+
>D+�>D,
>D,�>D-�D-��D.�D.��D/�D/��D0�D0��D1
D1��D2�D2�
D3�D3��D4
>D4�>D5
>D5�>D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;�>D<
>D<��D=�D=�>D>
>D>�>D?�D?��D@�D@��DA�DA��DB�DB��DC
DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK
DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ�>D[
>D[�>D\
>D\�>D]�D]��D^�D^��D_�D_��D`�D`�
Da�Da��Db
>Db��Dc�Dc��Dd
>Dd��De�De��Df
Df��Dg�Dg�
Dh�Dh��Di�Di��Dj
>Dj��Dk�Dk��Dl�Dl��Dm
Dm��Dn
>Dn��Do�Do��Dp
>Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Du�Du��Dv�Dv��Dw�Dw��Dx�Dy�D�0RD���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�I�A�ZA�hsA�p�A�r�A�t�A�p�A�p�A�p�A�r�A�r�A�n�A�r�A�r�A�v�A�z�A�x�A�x�A�/A��A�  A���A�jA�=qA�Q�A֏\A�5?A�ƨA�XAоwA�^5A�+A���AΉ7A���A�hsA�bAɰ!A�bNA��AǮA��A�5?A�A�n�A�A��
A�v�A�A�+A���A�{A��A�S�A��`A�C�A��HA��A�C�A�JA�bNA�bNA�  A�
=A�x�A�XA��mA�|�A��\A�&�A�A��+A�~�A��A�  A���A��A�~�A���A��A�n�A�K�A��A�(�A�z�A�{A�ĜA�O�A���A�A�A�ZA��A��A�hsA�^5A�VA��-A��A��A�9XA��#A�n�A�XA�+A�r�A}��A|1A{&�Ay�
Av�HAv1AtZAr=qAp�HApbAn�!Am�PAjr�Ad��Ac��A`��A]�AZr�AX �AU��AT��AS\)APVAM�hAL�AK�-AG�7ADr�ACl�A@{A<��A9�;A9"�A8��A8ffA7�FA7%A6��A5�
A3��A3p�A2��A1�hA//A-XA,�9A+dZA*E�A)�
A(bA'dZA'�A%VA#��A#hsA"E�A!��A z�A��A?}A�;AdZA��Ax�A�A��A��A�DA��A�+A$�A�AXAM�Al�AC�At�A�AG�A�HA�\A��A�A��A�A	�wA	�A��A  AoA(�AhsAjAG�A$�A �jA ffA {@���@���@�V@�V@�Ĝ@�A�@�ƨ@�dZ@�33@�"�@���@��@�bN@���@�M�@�M�@��@��@��@�7L@�j@���@��@�V@�b@�C�@���@䛦@�;d@�E�@�%@�K�@ݲ-@��@�z�@܋D@�`B@�o@�~�@ա�@�/@�Ĝ@�(�@�l�@�K�@��y@��#@ѡ�@�X@��`@�9X@�dZ@��@�ȴ@�ff@��@��@���@ͺ^@��@�z�@�  @���@���@��;@��m@�  @˶F@�+@���@��`@�Ĝ@ȴ9@��@�S�@�;d@�X@�Ĝ@���@�t�@�C�@�K�@�;d@���@��#@��7@�&�@���@�b@�l�@��@�ff@�j@���@�1'@�`B@�%@��h@��#@��^@���@��@�/@���@�~�@�=q@���@�?}@���@�r�@� �@��@���@�Ĝ@�A�@�  @��@�  @�&�@��h@�x�@���@���@���@�+@�+@�+@�K�@��P@���@�/@�@���@��@��@���@��/@�"�@�=q@��@���@�n�@���@��@�O�@�G�@�Ĝ@�A�@��;@���@�;d@�K�@�+@�@��R@��-@�x�@��@�M�@�$�@���@��#@��@�p�@��@��/@��j@��@��@�l�@�\)@��@�  @�b@�  @�t�@���@�^5@��@�J@���@��T@���@���@�7L@���@��`@���@��@�z�@��@�r�@��m@��@�|�@�l�@�l�@�t�@��;@�  @��@��;@���@��@�ȴ@���@���@��+@�n�@�M�@�{@��#@�X@�G�@�7L@��j@�9X@�ƨ@�|�@�\)@�33@�"�@�33@�"�@���@�V@�-@���@�7L@�%@���@���@��/@�Ĝ@�z�@�Z@�1'@���@��;@���@��P@�t�@�K�@��@�ȴ@���@�ff@�M�@�E�@�$�@��T@�`B@��@��/@���@�Q�@�I�@�9X@�(�@�b@�1@��;@�l�@���@�ȴ@��R@�V@�{@��@��-@���@u��@dz�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�I�A�ZA�hsA�p�A�r�A�t�A�p�A�p�A�p�A�r�A�r�A�n�A�r�A�r�A�v�A�z�A�x�A�x�A�/A��A�  A���A�jA�=qA�Q�A֏\A�5?A�ƨA�XAоwA�^5A�+A���AΉ7A���A�hsA�bAɰ!A�bNA��AǮA��A�5?A�A�n�A�A��
A�v�A�A�+A���A�{A��A�S�A��`A�C�A��HA��A�C�A�JA�bNA�bNA�  A�
=A�x�A�XA��mA�|�A��\A�&�A�A��+A�~�A��A�  A���A��A�~�A���A��A�n�A�K�A��A�(�A�z�A�{A�ĜA�O�A���A�A�A�ZA��A��A�hsA�^5A�VA��-A��A��A�9XA��#A�n�A�XA�+A�r�A}��A|1A{&�Ay�
Av�HAv1AtZAr=qAp�HApbAn�!Am�PAjr�Ad��Ac��A`��A]�AZr�AX �AU��AT��AS\)APVAM�hAL�AK�-AG�7ADr�ACl�A@{A<��A9�;A9"�A8��A8ffA7�FA7%A6��A5�
A3��A3p�A2��A1�hA//A-XA,�9A+dZA*E�A)�
A(bA'dZA'�A%VA#��A#hsA"E�A!��A z�A��A?}A�;AdZA��Ax�A�A��A��A�DA��A�+A$�A�AXAM�Al�AC�At�A�AG�A�HA�\A��A�A��A�A	�wA	�A��A  AoA(�AhsAjAG�A$�A �jA ffA {@���@���@�V@�V@�Ĝ@�A�@�ƨ@�dZ@�33@�"�@���@��@�bN@���@�M�@�M�@��@��@��@�7L@�j@���@��@�V@�b@�C�@���@䛦@�;d@�E�@�%@�K�@ݲ-@��@�z�@܋D@�`B@�o@�~�@ա�@�/@�Ĝ@�(�@�l�@�K�@��y@��#@ѡ�@�X@��`@�9X@�dZ@��@�ȴ@�ff@��@��@���@ͺ^@��@�z�@�  @���@���@��;@��m@�  @˶F@�+@���@��`@�Ĝ@ȴ9@��@�S�@�;d@�X@�Ĝ@���@�t�@�C�@�K�@�;d@���@��#@��7@�&�@���@�b@�l�@��@�ff@�j@���@�1'@�`B@�%@��h@��#@��^@���@��@�/@���@�~�@�=q@���@�?}@���@�r�@� �@��@���@�Ĝ@�A�@�  @��@�  @�&�@��h@�x�@���@���@���@�+@�+@�+@�K�@��P@���@�/@�@���@��@��@���@��/@�"�@�=q@��@���@�n�@���@��@�O�@�G�@�Ĝ@�A�@��;@���@�;d@�K�@�+@�@��R@��-@�x�@��@�M�@�$�@���@��#@��@�p�@��@��/@��j@��@��@�l�@�\)@��@�  @�b@�  @�t�@���@�^5@��@�J@���@��T@���@���@�7L@���@��`@���@��@�z�@��@�r�@��m@��@�|�@�l�@�l�@�t�@��;@�  @��@��;@���@��@�ȴ@���@���@��+@�n�@�M�@�{@��#@�X@�G�@�7L@��j@�9X@�ƨ@�|�@�\)@�33@�"�@�33@�"�@���@�V@�-@���@�7L@�%@���@���@��/@�Ĝ@�z�@�Z@�1'@���@��;@���@��P@�t�@�K�@��@�ȴ@���@�ff@�M�@�E�@�$�@��T@�`B@��@��/@���@�Q�@�I�@�9X@�(�@�b@�1@��;@�l�@���@�ȴ@��R@�V@�{@��@��-@���@u��@dz�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�B�!B�!B�B�B�B�B�B�B�B�B�B�!B�!B�!B�!B�!B�!B�B�B�B�dB�}B��B��B��B�B�B��B�
B�#B�#B�B�`B+BJB\B'�B?}BD�BE�B^5BffBiyBm�Bo�Bt�B�B��B�!B��B��B�\Bz�BbNBW
BC�B;dB5?B;dBm�B�VB�uB��B��B��B�bB�Bs�BO�B2-B%�B�BbBVB
=B  B�TBBt�Bl�BcTBl�B�oB��B��B�bB~�By�Br�B]/B>wB"�BoBB
�B
�B
��B
�jB
�FB
�B
��B
�JB
o�B
T�B
@�B
49B
-B
!�B
VB
B	��B	�B	�ZB	�BB	�B	��B	�jB	��B	�DB	}�B	jB	XB	I�B	=qB	5?B	-B	�B	oB	JB	B��B�B�`B�B��B��BȴBǮBŢBĜBÖBÖB��B�qB�jB�XB�LB�dB�jB�qB�LB�-B�B��B��B��B��B��B��B�uB�oB�oB�oB�oB�hB�JB�+B�B�B� B~�B}�B|�Bz�Bx�Bv�Bu�Bv�Bz�B�B�1B�DB�=B�1B�+B�1B�oB��B��B�uB�oB�oB�uB�uB�uB�uB��B�uB�\B�VB�PB�JB�DB�=B�7B�PB�JB�JB�JB�JB�DB�DB�=B�=B�DB�JB�PB�JB�JB�PB�\B�\B�\B�\B�\B�\B�hB�hB�\B�\B�hB�oB�uB��B��B��B��B�B�B�-B�qBÖBĜBĜBƨBɺB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�
B�B�)B�5B�;B�BB�HB�ZB�mB�yB�mB�sB�mB�sB�yB�B07LB	M�B	Q�B	R�B	R�B	R�B	Q�B	T�B	YB	\)B	^5B	`BB	dZB	hsB	iyB	hsB	bNB	cTB	e`B	o�B	s�B	y�B	|�B	|�B	z�B	t�B	n�B	k�B	hsB	gmB	gmB	gmB	ffB	gmB	gmB	hsB	n�B	r�B	s�B	s�B	t�B	u�B	}�B	�B	�B	�B	�%B	�B	�B	�B	�B	�B	�B	�B	�B	�1B	�DB	�\B	�hB	�bB	�\B	�PB	�=B	�7B	�B	�B	�B	�+B	�1B	�1B	�JB	�\B	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�'B	�9B	�RB	��B	�}B	�}B	�}B	�}B	�}B	�wB	��B	��B	ĜB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�;B	�HB	�HB	�HB	�HB	�NB	�fB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
+B
1B
1B
1B
	7B
	7B
	7B
1B
	7B

=B
DB
DB
DB
JB
JB
PB
VB
�B
"�B
+611111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�B�!B�!B�B�B�B�B�B�B�B�B�B�!B�!B�!B�!B�!B�!B�B�B�B�dB�}B��B��B��B�B�B��B�
B�#B�#B�B�`B+BJB\B'�B?}BD�BE�B^5BffBiyBm�Bo�Bt�B�B��B�!B��B��B�\Bz�BbNBW
BC�B;dB5?B;dBm�B�VB�uB��B��B��B�bB�Bs�BO�B2-B%�B�BbBVB
=B  B�TBBt�Bl�BcTBl�B�oB��B��B�bB~�By�Br�B]/B>wB"�BoBB
�B
�B
��B
�jB
�FB
�B
��B
�JB
o�B
T�B
@�B
49B
-B
!�B
VB
B	��B	�B	�ZB	�BB	�B	��B	�jB	��B	�DB	}�B	jB	XB	I�B	=qB	5?B	-B	�B	oB	JB	B��B�B�`B�B��B��BȴBǮBŢBĜBÖBÖB��B�qB�jB�XB�LB�dB�jB�qB�LB�-B�B��B��B��B��B��B��B�uB�oB�oB�oB�oB�hB�JB�+B�B�B� B~�B}�B|�Bz�Bx�Bv�Bu�Bv�Bz�B�B�1B�DB�=B�1B�+B�1B�oB��B��B�uB�oB�oB�uB�uB�uB�uB��B�uB�\B�VB�PB�JB�DB�=B�7B�PB�JB�JB�JB�JB�DB�DB�=B�=B�DB�JB�PB�JB�JB�PB�\B�\B�\B�\B�\B�\B�hB�hB�\B�\B�hB�oB�uB��B��B��B��B�B�B�-B�qBÖBĜBĜBƨBɺB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�
B�B�)B�5B�;B�BB�HB�ZB�mB�yB�mB�sB�mB�sB�yB�B07LB	M�B	Q�B	R�B	R�B	R�B	Q�B	T�B	YB	\)B	^5B	`BB	dZB	hsB	iyB	hsB	bNB	cTB	e`B	o�B	s�B	y�B	|�B	|�B	z�B	t�B	n�B	k�B	hsB	gmB	gmB	gmB	ffB	gmB	gmB	hsB	n�B	r�B	s�B	s�B	t�B	u�B	}�B	�B	�B	�B	�%B	�B	�B	�B	�B	�B	�B	�B	�B	�1B	�DB	�\B	�hB	�bB	�\B	�PB	�=B	�7B	�B	�B	�B	�+B	�1B	�1B	�JB	�\B	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�'B	�9B	�RB	��B	�}B	�}B	�}B	�}B	�}B	�wB	��B	��B	ĜB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�;B	�HB	�HB	�HB	�HB	�NB	�fB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
+B
1B
1B
1B
	7B
	7B
	7B
1B
	7B

=B
DB
DB
DB
JB
JB
PB
VB
�B
"�B
+611111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.26 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181024140825                              AO  ARCAADJP                                                                    20181024140825    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181024140825  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181024140825  QCF$                G�O�G�O�G�O�0               