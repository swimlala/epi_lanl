CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-24T14:08:37Z creation      
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
resolution        =���   axis      Z        l  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  @�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     l  B�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J$   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     l  L    TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  Sl   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Z�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  \�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  d    TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  e�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  mh   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  t�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  v�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ~   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �d   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �    HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �$   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �4   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �8   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �<   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �@Argo profile    3.1 1.2 19500101000000  20181024140837  20181024140837  5904957 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6560                            2B  A   APEX                            7471                            062512                          846 @�ۤ�k��1   @�ۥb���@5���l�D�c�I�^51   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   B   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B?��BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B���B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C=�fC@  CB  CD  CF  CH  CJ  CL  CN  CP  CR�CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cy�fC{�fC~  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  D   D � DfD�fD  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D	  D	� D
  D
� D  D� DfD� D  D� D  D� D  D� D  D� DfD�fDfD� D  D� D  D� D  Dy�D��D� D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2�fD3fD3�fD4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:y�D:��D;y�D<  D<�fD=fD=�fD>  D>� D>��D?� D@  D@� DAfDA�fDBfDB�fDC  DC� DD  DDy�DD��DE� DFfDF�fDG  DGy�DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DPfDP� DQ  DQ� DR  DRy�DR��DS� DT  DT� DU  DU�fDV  DV� DW  DW�fDXfDX� DY  DY�fDZfDZ� D[  D[� D\  D\� D]  D]�fD^fD^�fD_fD_� D_��D`y�Da  Da� Db  Dby�Db��Dc� Dd  Dd� De  De� DffDf� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dw�3Dy��D�1�D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@�Q�A(�A$(�AD(�Ad(�A�{A�{A�{A�{A�{A�{A�{A�{B
=B	
=B
=B
=B!
=B)
=B1
=B9
=B@��BI
=BQ
=BY
=Ba
=Bi
=Bq
=By
=B��B��B��B��B��RB��B��B��B��B��B��B��B��B��B��B��B��BąBȅB̅BЅBԅBظRB܅B�Q�B�B�B�B��B�B��B��C B�CB�CB�CB�CB�C
B�CB�CB�CB�CB�CB�CB�CB�CB�CB�CB�C B�C"B�C$B�C&B�C(B�C*B�C,B�C.B�C0B�C2B�C4B�C6B�C8B�C:B�C<B�C>(�C@B�CBB�CDB�CFB�CHB�CJB�CLB�CNB�CPB�CR\)CTB�CVB�CXB�CZB�C\B�C^B�C`B�CbB�CdB�CfB�ChB�CjB�ClB�CnB�CpB�CrB�CtB�CvB�CxB�Cz(�C|(�C~B�C�!HC�!HC�!HC�!HC�!HC�!HC�!HC�{C�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�{C�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�.C�!HC�!HC�!HC�!HC�!HC�!HC�!HC�{C�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�{C�!HC�!HC�!HC�!HC�!HC�!HC�!HC�.C�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�.C�!HC�!HC�!HC�!HC�!HC�!HC�!HC�.C�.C�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�.C�!HC�!HD �D ��D
D�
D�D��D�D��D�D��D�D��D�D��D�D�>D�D��D	�D	��D
�D
��D�D��D
D��D�D��D�D��D�D��D�D��D
D�
D
D��D�D��D�D��D�D�>D
>D��D�D��D�D�>D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2�
D3
D3�
D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:�>D;
>D;�>D<�D<�
D=
D=�
D>�D>��D?
>D?��D@�D@��DA
DA�
DB
DB�
DC�DC��DD�DD�>DE
>DE��DF
DF�
DG�DG�>DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP
DP��DQ�DQ��DR�DR�>DS
>DS��DT�DT��DU�DU�
DV�DV��DW�DW�
DX
DX��DY�DY�
DZ
DZ��D[�D[��D\�D\��D]�D]�
D^
D^�
D_
D_��D`
>D`�>Da�Da��Db�Db�>Dc
>Dc��Dd�Dd��De�De��Df
Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Du�Du��Dv�Dv��Dw�Dw��Dw��Dy��D�:>D��H1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���A���A���A���A���A���A�A�%A�1A�A�  A�A�1A�
=A�VA�VA�bA�bA�oA�bA�bA�{A�oA���A���Aљ�AсA�jA�bNA�XA�K�A�7LA�{AЁA�Q�A�;dA�XA�Aȕ�A�$�Aŝ�Aě�A��A��A�Q�A��PA��A�ȴA���A��HA���A�ȴA�^5A�K�A�7LA� �A���A�VA��+A��9A���A�p�A�l�A�7LA�&�A���A�t�A�^5A�ZA�Q�A��TA��^A�(�A���A���A��A��9A��`A��jA�l�A��A���A�9XA��uA���A��A���A�ƨA�Q�A�A��HA���A��A���A�"�A��A�\)A�VA���A� �A��/A��A��A�E�A���A�p�A��#A�~�A�^5A�ƨA��A}7LA{33Ax�/AvZAt��As�PAoVAi��Ah�9AhE�Ah-Ah�Ah1Ag�Ae�mAd1AbA�A_�#A_�A\��AZ��AX�yAT�RARI�AR(�AQ�AQ"�AOoAM��AM+ALbNAKO�AI�AI&�AH�AGS�AFA�AD��AB�`AA��AAA?t�A<ĜA:�jA:9XA:�A9�mA9&�A6E�A5�hA4^5A2�A0JA/G�A/�A.�HA.^5A-�TA-C�A+33A*Q�A*A(��A&�DA%G�A$�uA#��A#"�A"�+A"-A!��A�A�;A�jAr�A1'A��A�A7LA"�A5?A�A&�A�An�A�
A;dA-AA
��A	`BA	oAz�A�#AXAM�A�^Ax�A��A�F@�1'@�O�@��@�{@���@�Q�@��@�dZ@��@�$�@�?}@�@�I�@� �@�1@畁@�^@�@�v�@��@��@�?}@���@�z�@޸R@ۅ@�;d@��@��y@��H@�ȴ@ڧ�@ڏ\@�-@�-@�{@�@�@�&�@׍P@ְ!@�ff@ղ-@���@�1'@�v�@�G�@��@Ѓ@� �@�  @Ϯ@ϥ�@�dZ@�~�@��@ͩ�@��@��@���@Ǖ�@�"�@Ə\@ă@���@�S�@��@+@�5?@���@�/@��j@�(�@�+@�n�@��^@���@���@��7@�X@��j@��
@���@�l�@�+@���@��!@��\@�n�@��T@��-@���@�&�@���@�Z@� �@���@���@�33@�~�@�E�@��T@�hs@�V@�C�@�%@��j@���@��D@�z�@�9X@��@��;@��w@�|�@�C�@�;d@�+@�o@�ȴ@�V@���@���@��D@�j@�I�@�(�@�  @��@�33@�
=@��y@�V@�?}@��@��@���@���@��@���@��@��@�j@��
@�ƨ@��F@�S�@���@��h@�p�@��`@���@�Q�@�A�@�(�@�  @��@��;@��P@�|�@�|�@�t�@�l�@�S�@�+@�@��y@���@�~�@�-@�{@���@��T@��-@���@�hs@��/@�9X@�1'@�(�@��@�  @��m@���@���@��+@�$�@���@���@�`B@��@�9X@��m@�|�@�"�@��y@�ȴ@��!@���@�V@�{@��@��7@�X@���@��`@���@��@��`@��@��@��@�bN@��@���@�"�@�ȴ@�^5@�J@���@��T@���@��-@�hs@���@�j@�(�@��@��m@���@��@���@��@�\)@���@��h@���@��@���@�ƨ@�1@�(�@�v�@��@���@��
@�  @�C�@�
=@�@���@��@���@���@��H@�n�@�5?@�=q@�-@���@�x�@�@vȴ@h$1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A���A���A���A���A���A���A�A�%A�1A�A�  A�A�1A�
=A�VA�VA�bA�bA�oA�bA�bA�{A�oA���A���Aљ�AсA�jA�bNA�XA�K�A�7LA�{AЁA�Q�A�;dA�XA�Aȕ�A�$�Aŝ�Aě�A��A��A�Q�A��PA��A�ȴA���A��HA���A�ȴA�^5A�K�A�7LA� �A���A�VA��+A��9A���A�p�A�l�A�7LA�&�A���A�t�A�^5A�ZA�Q�A��TA��^A�(�A���A���A��A��9A��`A��jA�l�A��A���A�9XA��uA���A��A���A�ƨA�Q�A�A��HA���A��A���A�"�A��A�\)A�VA���A� �A��/A��A��A�E�A���A�p�A��#A�~�A�^5A�ƨA��A}7LA{33Ax�/AvZAt��As�PAoVAi��Ah�9AhE�Ah-Ah�Ah1Ag�Ae�mAd1AbA�A_�#A_�A\��AZ��AX�yAT�RARI�AR(�AQ�AQ"�AOoAM��AM+ALbNAKO�AI�AI&�AH�AGS�AFA�AD��AB�`AA��AAA?t�A<ĜA:�jA:9XA:�A9�mA9&�A6E�A5�hA4^5A2�A0JA/G�A/�A.�HA.^5A-�TA-C�A+33A*Q�A*A(��A&�DA%G�A$�uA#��A#"�A"�+A"-A!��A�A�;A�jAr�A1'A��A�A7LA"�A5?A�A&�A�An�A�
A;dA-AA
��A	`BA	oAz�A�#AXAM�A�^Ax�A��A�F@�1'@�O�@��@�{@���@�Q�@��@�dZ@��@�$�@�?}@�@�I�@� �@�1@畁@�^@�@�v�@��@��@�?}@���@�z�@޸R@ۅ@�;d@��@��y@��H@�ȴ@ڧ�@ڏ\@�-@�-@�{@�@�@�&�@׍P@ְ!@�ff@ղ-@���@�1'@�v�@�G�@��@Ѓ@� �@�  @Ϯ@ϥ�@�dZ@�~�@��@ͩ�@��@��@���@Ǖ�@�"�@Ə\@ă@���@�S�@��@+@�5?@���@�/@��j@�(�@�+@�n�@��^@���@���@��7@�X@��j@��
@���@�l�@�+@���@��!@��\@�n�@��T@��-@���@�&�@���@�Z@� �@���@���@�33@�~�@�E�@��T@�hs@�V@�C�@�%@��j@���@��D@�z�@�9X@��@��;@��w@�|�@�C�@�;d@�+@�o@�ȴ@�V@���@���@��D@�j@�I�@�(�@�  @��@�33@�
=@��y@�V@�?}@��@��@���@���@��@���@��@��@�j@��
@�ƨ@��F@�S�@���@��h@�p�@��`@���@�Q�@�A�@�(�@�  @��@��;@��P@�|�@�|�@�t�@�l�@�S�@�+@�@��y@���@�~�@�-@�{@���@��T@��-@���@�hs@��/@�9X@�1'@�(�@��@�  @��m@���@���@��+@�$�@���@���@�`B@��@�9X@��m@�|�@�"�@��y@�ȴ@��!@���@�V@�{@��@��7@�X@���@��`@���@��@��`@��@��@��@�bN@��@���@�"�@�ȴ@�^5@�J@���@��T@���@��-@�hs@���@�j@�(�@��@��m@���@��@���@��@�\)@���@��h@���@��@���@�ƨ@�1@�(�@�v�@��@���@��
@�  @�C�@�
=@�@���@��@���@���@��H@�n�@�5?@�=q@�-@���@�x�@�@vȴ@h$1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BcTBcTBcTBcTBcTBcTBcTBcTBcTBcTBcTBcTBcTBcTBcTBcTBcTBbNBbNBcTBe`BcTBcTBn�B}�B�DB�bB�{B��B��B��B��B��B��B�`B��B{B �B&�B.B9XB@�BD�BO�BZB`BBo�Bo�Bo�Bo�Bn�Bm�Bm�Bm�Bm�Bl�Bl�Bk�Bk�BjBk�Bk�Bk�BjBhsBhsBhsBiyBk�Bn�Bm�BbNB_;BYBO�BG�B@�B:^B49B2-B/B)�B�B�BVBB��B�fB�;B��B�B��B�\BgmB^5BVBI�BD�B6FB�BB
�B
��B
�wB
�LB
�3B
�B
��B
x�B
K�B
=qB
-B
�B
\B
  B	��B	�yB	��B	�'B	��B	��B	��B	��B	��B	��B	��B	�DB	�B	v�B	p�B	ffB	XB	L�B	<jB	/B	-B	+B	%�B	�B	�B	{B	bB	DB	%B	B��B��B��B�B�sB�ZB�BB�B��B��B��B��BɺBƨB��B�qB�LB�'B�B�B�B��B��B��B��B��B��B��B��B�{B�hB�\B�VB�VB�VB�PB�DB�\B�{B��B�{B�{B�oB�VB�1B�1B�7B�7B�7B�=B�=B�=B�1B�B{�Bq�Bl�Bl�BjBgmBe`BcTBaHB_;B]/Bz�B�oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�3B�9B�9B�?B�?B�?B�?B�?B�FB�FB�FB�FB�?B�FB�XB�^B�^B�XB�jB��BŢBǮBȴBȴBɺB��B��B��B��B��B��B��B��B�#B�ZB�B�B�B��B��B	B	B	B	%B	+B	DB	PB	bB	{B	�B	�B	�B	�B	�B	�B	!�B	&�B	'�B	)�B	.B	0!B	1'B	1'B	2-B	49B	49B	49B	6FB	8RB	9XB	:^B	;dB	=qB	A�B	D�B	F�B	H�B	J�B	M�B	W
B	aHB	cTB	cTB	dZB	dZB	e`B	gmB	gmB	hsB	iyB	k�B	k�B	k�B	l�B	m�B	p�B	u�B	y�B	|�B	}�B	}�B	~�B	� B	�B	�B	�B	�B	�B	�B	�%B	�%B	�B	�B	�B	�B	�B	�B	�B	�%B	�%B	�%B	�%B	�=B	�JB	�JB	�VB	�\B	�hB	�hB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�!B	�?B	�XB	�^B	�^B	�^B	�XB	�XB	�^B	�wB	�}B	�}B	��B	��B	��B	��B	��B	ÖB	ÖB	ÖB	B	ÖB	ÖB	ĜB	ĜB	ŢB	ǮB	ɺB	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�/B	�;B	�HB	�ZB	�`B	�mB	�yB	�yB	�yB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�yB	�mB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B
0B
�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 BcTBcTBcTBcTBcTBcTBcTBcTBcTBcTBcTBcTBcTBcTBcTBcTBcTBbNBbNBcTBe`BcTBcTBn�B}�B�DB�bB�{B��B��B��B��B��B��B�`B��B{B �B&�B.B9XB@�BD�BO�BZB`BBo�Bo�Bo�Bo�Bn�Bm�Bm�Bm�Bm�Bl�Bl�Bk�Bk�BjBk�Bk�Bk�BjBhsBhsBhsBiyBk�Bn�Bm�BbNB_;BYBO�BG�B@�B:^B49B2-B/B)�B�B�BVBB��B�fB�;B��B�B��B�\BgmB^5BVBI�BD�B6FB�BB
�B
��B
�wB
�LB
�3B
�B
��B
x�B
K�B
=qB
-B
�B
\B
  B	��B	�yB	��B	�'B	��B	��B	��B	��B	��B	��B	��B	�DB	�B	v�B	p�B	ffB	XB	L�B	<jB	/B	-B	+B	%�B	�B	�B	{B	bB	DB	%B	B��B��B��B�B�sB�ZB�BB�B��B��B��B��BɺBƨB��B�qB�LB�'B�B�B�B��B��B��B��B��B��B��B��B�{B�hB�\B�VB�VB�VB�PB�DB�\B�{B��B�{B�{B�oB�VB�1B�1B�7B�7B�7B�=B�=B�=B�1B�B{�Bq�Bl�Bl�BjBgmBe`BcTBaHB_;B]/Bz�B�oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�3B�9B�9B�?B�?B�?B�?B�?B�FB�FB�FB�FB�?B�FB�XB�^B�^B�XB�jB��BŢBǮBȴBȴBɺB��B��B��B��B��B��B��B��B�#B�ZB�B�B�B��B��B	B	B	B	%B	+B	DB	PB	bB	{B	�B	�B	�B	�B	�B	�B	!�B	&�B	'�B	)�B	.B	0!B	1'B	1'B	2-B	49B	49B	49B	6FB	8RB	9XB	:^B	;dB	=qB	A�B	D�B	F�B	H�B	J�B	M�B	W
B	aHB	cTB	cTB	dZB	dZB	e`B	gmB	gmB	hsB	iyB	k�B	k�B	k�B	l�B	m�B	p�B	u�B	y�B	|�B	}�B	}�B	~�B	� B	�B	�B	�B	�B	�B	�B	�%B	�%B	�B	�B	�B	�B	�B	�B	�B	�%B	�%B	�%B	�%B	�=B	�JB	�JB	�VB	�\B	�hB	�hB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�!B	�?B	�XB	�^B	�^B	�^B	�XB	�XB	�^B	�wB	�}B	�}B	��B	��B	��B	��B	��B	ÖB	ÖB	ÖB	B	ÖB	ÖB	ĜB	ĜB	ŢB	ǮB	ɺB	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�/B	�;B	�HB	�ZB	�`B	�mB	�yB	�yB	�yB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�yB	�mB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B
0B
�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.26 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181024140837                              AO  ARCAADJP                                                                    20181024140837    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181024140837  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181024140837  QCF$                G�O�G�O�G�O�0               