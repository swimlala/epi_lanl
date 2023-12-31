CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:17:54Z creation      
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
_FillValue                 �  A<   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C0   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  T�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ^|   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  fH   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  h<   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  p   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  w�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  y�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �T   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �    HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �$   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �(   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �,   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �0Argo profile    3.1 1.2 19500101000000  20181005191754  20181005191754  5904954 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL              A   AO  6557                            2B  A   APEX                            7468                            062512                          846 @��$�z1:1   @��%8㠖@5S�����d~��n�1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                     A   A   A   @��@�  @�  A   A   A@  A`  A�  A�33A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�33B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B���B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C+�fC.  C0  C2  C4  C6�C8  C:�C<  C=�fC@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx�Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C��C�  C�  C�  C��3C�  C��C��C��C�  C��C��C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C��3C�  C��C�  C��3C�  C��C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C��3C��3C��3C��3C�  C��C��C�  C��C�  C�  C��C�  C��3C��3C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C��3C�  C�  C��3C�  C�  C�  C��3C�  C��C��C��C�  C��3C�  C��C�  C�  C�  C��3C��C�  C��3C��3C��3D �fDfD� DfD�fD��Dy�D��Dy�D  D�fDfD� D�3D� DfD�fD	  D	y�D
  D
� D
�3Dy�D  D� D  D�fDfD� D��D� DfD� D  D� D��Dy�D��D� D  Dy�DfD� D��Dy�D  D�fD��D� DfD�fD  Dy�D  D� D��D� DfD�fD  D�fD  D� D fD � D!  D!�fD"  D"� D#  D#� D$  D$� D%  D%y�D%��D&� D'  D'y�D'��D(� D)  D)� D*  D*�fD+  D+� D,fD,� D,��D-y�D.  D.�fD/  D/y�D0  D0� D1  D1� D2  D2�fD3  D3s3D3�3D4� D5  D5y�D6fD6�fD7fD7�fD8  D8� D9fD9� D:  D:� D;fD;� D;��D<� D=fD=�fD>  D>� D?fD?�fD@  D@� DA  DA� DBfDB� DC  DCy�DC��DD� DE  DE� DF  DF��DG  DGy�DH  DH�fDI  DI�fDJfDJ��DK  DKy�DLfDL�fDL��DM� DNfDN� DN��DO� DPfDPy�DQ  DQ�fDRfDR� DR��DSy�DTfDT�fDT��DU�fDVfDV� DV��DW� DXfDXy�DX��DYs3DZ  DZ�fD[  D[�fD\�D\�fD\��D]y�D]��D^y�D^��D_� D`fD`�fDa  Da�fDbfDb� Dc  Dcy�Dd  Dd� De  De� Df  Df� Dg  Dg� DhfDh�fDh��Diy�Dj  Dj�fDk  Dk� Dl  Dl� Dm  Dm�fDnfDn� Do  Do� Do��Dpy�Dp��Dqy�Dr  Dr�fDs�Ds�fDtfDt� Du  Duy�Du��Dvy�Dv��Dw� Dw� Dy�{D�'�D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @*=q@�Q�@�Q�A(�A$(�AD(�Ad(�A�{A�G�A�{A�{A�{A�{A�{A�{B
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
=B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��RBĸRBȸRB̅BЅBԸRB؅B܅B��B�B�B�B��B�B�Q�B��C B�CB�CB�CB�CB�C
B�CB�CB�CB�CB�CB�CB�CB�CB�CB�CB�C B�C"B�C$B�C&B�C(B�C*B�C,(�C.B�C0B�C2B�C4B�C6\)C8B�C:\)C<B�C>(�C@B�CBB�CDB�CFB�CHB�CJB�CLB�CNB�CPB�CRB�CTB�CVB�CXB�CZB�C\B�C^B�C`B�CbB�CdB�CfB�ChB�CjB�ClB�CnB�CpB�CrB�CtB�CvB�Cx\)CzB�C|B�C~B�C�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�{C�!HC�!HC�!HC�!HC�.C�!HC�!HC�!HC�{C�!HC�.C�.C�.C�!HC�.C�.C�!HC�!HC�{C�{C�!HC�!HC�!HC�!HC�!HC�!HC�!HC�{C�!HC�!HC�!HC�!HC�!HC�!HC�{C�!HC�.C�!HC�{C�!HC�.C�!HC�!HC�!HC�{C�{C�!HC�!HC�!HC�!HC�!HC�!HC�{C�{C�{C�{C�!HC�.C�.C�!HC�.C�!HC�!HC�.C�!HC�{C�{C�!HC�!HC�!HC�.C�!HC�!HC�!HC�!HC�!HC�!HC�{C�!HC�!HC�{C�!HC�!HC�!HC�{C�!HC�.C�.C�.C�!HC�{C�!HC�.C�!HC�!HC�!HC�{C�.C�!HC�{C�{D 
=D �
D
D��D
D�
D
>D�>D
>D�>D�D�
D
D��D�D��D
D�
D	�D	�>D
�D
��D�D�>D�D��D�D�
D
D��D
>D��D
D��D�D��D
>D�>D
>D��D�D�>D
D��D
>D�>D�D�
D
>D��D
D�
D�D�>D�D��D
>D��D
D�
D�D�
D�D��D 
D ��D!�D!�
D"�D"��D#�D#��D$�D$��D%�D%�>D&
>D&��D'�D'�>D(
>D(��D)�D)��D*�D*�
D+�D+��D,
D,��D-
>D-�>D.�D.�
D/�D/�>D0�D0��D1�D1��D2�D2�
D3�D3��D4�D4��D5�D5�>D6
D6�
D7
D7�
D8�D8��D9
D9��D:�D:��D;
D;��D<
>D<��D=
D=�
D>�D>��D?
D?�
D@�D@��DA�DA��DB
DB��DC�DC�>DD
>DD��DE�DE��DF�DF�qDG�DG�>DH�DH�
DI�DI�
DJ
DJ�qDK�DK�>DL
DL�
DM
>DM��DN
DN��DO
>DO��DP
DP�>DQ�DQ�
DR
DR��DS
>DS�>DT
DT�
DU
>DU�
DV
DV��DW
>DW��DX
DX�>DY
>DY��DZ�DZ�
D[�D[�
D\qD\�
D]
>D]�>D^
>D^�>D_
>D_��D`
D`�
Da�Da�
Db
Db��Dc�Dc�>Dd�Dd��De�De��Df�Df��Dg�Dg��Dh
Dh�
Di
>Di�>Dj�Dj�
Dk�Dk��Dl�Dl��Dm�Dm�
Dn
Dn��Do�Do��Dp
>Dp�>Dq
>Dq�>Dr�Dr�
DsqDs�
Dt
Dt��Du�Du�>Dv
>Dv�>Dw
>Dw��Dw�Dy�D�0 D��H1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���A���A��A���A�bA�5?A�VA�-A�M�A�S�A�hsA�ffA�bNA�dZA�dZA�\)A�ZA�S�A�?}A��A��A��wA�ƨA���A��A��A�VA�1A��TA�A���A��-A��9A��RA���A���A�ffA�&�A��A�ƨA��RA��-A���A�-A��/A��-A�|�A�p�A�\)A�?}A�/A�JA�5?A�ȴA��9A���A�^5A��yA�{A��A�bNA�|�A�hsA��A�I�A��A�x�A�VA�=qA��hA�7LA���A��A�VA���A�bA�A�-A�G�A�;dA��A���A���A�p�A�A�1A��FA�1'A�9XA��A���A���A�/A��\A�Q�A�E�A�A��;A�~�A���A���A�^5A�A��A�ffA�  A���A�-A��FA���A�x�A���A���A��A�^5A��A�S�A���A~ȴAxbNAv�\Au�7At�RAr�HAmS�Ak�Ai�PAh��Ag��Af��Af��Af(�Ae��Ae?}Ad^5Ab��Aa�PA`ffA^VA[�
AY��AT�RAS|�ARQ�AQ�-API�APJAP1'AP �AOVAN-AM�#AM�ALI�AI�#AIAG"�AEXAC�ACoAB�AA��AAhsA@�A?�A=+A;��A:�\A9��A8��A7��A6=qA5��A4��A4jA3�A2�A1hsA/��A.E�A,�HA,�DA+�A*��A)�-A)
=A'��A'"�A&�A$��A#;dA"��A"r�A"5?A!�;A ȴA��AĜA��A��A/A�A{Al�A�\A(�A�AĜA��A�jA��A��A �A�A�A��A+A��A��A
=A��A{AȴAA&�A
v�A	C�AbNA�#A+A�`A�A�!A1A�RAA&�A �\A 9X@�dZ@�Q�@�
=@�`B@��\@��@�ƨ@�@�7L@���@�u@�bN@�K�@�n�@��/@�  @�"�@�M�@��T@�^@�?}@���@�bN@� �@�^5@�|�@�\@�z�@�|�@�ȴ@�$�@�O�@�  @�33@�^5@�@�`B@��`@�9X@���@�hs@���@ӶF@ҧ�@ѡ�@���@Ѓ@Ͼw@��@�`B@�A�@�33@�p�@ȋD@�(�@Ǯ@�K�@�+@���@�M�@���@��y@�$�@���@�X@�I�@�n�@��@�  @��w@�S�@�^5@���@�&�@���@�bN@���@�|�@�"�@�V@��@�@���@�hs@��/@�I�@���@�\)@���@�$�@�x�@�7L@��`@��j@��@���@��7@���@��D@� �@��w@�;d@���@�E�@�p�@�O�@�?}@��@��/@���@��@�  @��m@���@�ƨ@��@���@�$�@���@�7L@��`@���@�Q�@�b@���@�=q@��@��@�G�@��@��j@� �@��
@�|�@�l�@��F@�dZ@�;d@���@���@���@�^5@��@�hs@�&�@�/@�7L@��/@�9X@�b@�  @� �@�b@�  @���@�ƨ@���@��@�\)@�K�@�C�@�C�@�K�@�C�@�;d@���@�=q@�@���@�x�@�V@��u@�1@��@�;d@���@�ȴ@��\@�^5@�M�@�=q@�J@��#@�x�@�?}@�7L@�/@�V@�%@���@�A�@�(�@�1@��
@��w@�|�@�\)@�S�@�33@��y@���@�V@�=q@�{@��@�hs@�7L@��@�%@��9@�j@�1'@���@��@���@�S�@��@��H@��@���@�v�@�V@�M�@�@��^@�`B@�&�@��@��/@��@��u@�bN@��@��;@���@�ƨ@��@�t�@�K�@�C�@�C�@�;d@��@�
=@��H@���@�v�@�ff@�M�@�$�@��^@�`B@�?}@�G�@��@�%@���@��N@|�@j�X1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A���A���A��A���A�bA�5?A�VA�-A�M�A�S�A�hsA�ffA�bNA�dZA�dZA�\)A�ZA�S�A�?}A��A��A��wA�ƨA���A��A��A�VA�1A��TA�A���A��-A��9A��RA���A���A�ffA�&�A��A�ƨA��RA��-A���A�-A��/A��-A�|�A�p�A�\)A�?}A�/A�JA�5?A�ȴA��9A���A�^5A��yA�{A��A�bNA�|�A�hsA��A�I�A��A�x�A�VA�=qA��hA�7LA���A��A�VA���A�bA�A�-A�G�A�;dA��A���A���A�p�A�A�1A��FA�1'A�9XA��A���A���A�/A��\A�Q�A�E�A�A��;A�~�A���A���A�^5A�A��A�ffA�  A���A�-A��FA���A�x�A���A���A��A�^5A��A�S�A���A~ȴAxbNAv�\Au�7At�RAr�HAmS�Ak�Ai�PAh��Ag��Af��Af��Af(�Ae��Ae?}Ad^5Ab��Aa�PA`ffA^VA[�
AY��AT�RAS|�ARQ�AQ�-API�APJAP1'AP �AOVAN-AM�#AM�ALI�AI�#AIAG"�AEXAC�ACoAB�AA��AAhsA@�A?�A=+A;��A:�\A9��A8��A7��A6=qA5��A4��A4jA3�A2�A1hsA/��A.E�A,�HA,�DA+�A*��A)�-A)
=A'��A'"�A&�A$��A#;dA"��A"r�A"5?A!�;A ȴA��AĜA��A��A/A�A{Al�A�\A(�A�AĜA��A�jA��A��A �A�A�A��A+A��A��A
=A��A{AȴAA&�A
v�A	C�AbNA�#A+A�`A�A�!A1A�RAA&�A �\A 9X@�dZ@�Q�@�
=@�`B@��\@��@�ƨ@�@�7L@���@�u@�bN@�K�@�n�@��/@�  @�"�@�M�@��T@�^@�?}@���@�bN@� �@�^5@�|�@�\@�z�@�|�@�ȴ@�$�@�O�@�  @�33@�^5@�@�`B@��`@�9X@���@�hs@���@ӶF@ҧ�@ѡ�@���@Ѓ@Ͼw@��@�`B@�A�@�33@�p�@ȋD@�(�@Ǯ@�K�@�+@���@�M�@���@��y@�$�@���@�X@�I�@�n�@��@�  @��w@�S�@�^5@���@�&�@���@�bN@���@�|�@�"�@�V@��@�@���@�hs@��/@�I�@���@�\)@���@�$�@�x�@�7L@��`@��j@��@���@��7@���@��D@� �@��w@�;d@���@�E�@�p�@�O�@�?}@��@��/@���@��@�  @��m@���@�ƨ@��@���@�$�@���@�7L@��`@���@�Q�@�b@���@�=q@��@��@�G�@��@��j@� �@��
@�|�@�l�@��F@�dZ@�;d@���@���@���@�^5@��@�hs@�&�@�/@�7L@��/@�9X@�b@�  @� �@�b@�  @���@�ƨ@���@��@�\)@�K�@�C�@�C�@�K�@�C�@�;d@���@�=q@�@���@�x�@�V@��u@�1@��@�;d@���@�ȴ@��\@�^5@�M�@�=q@�J@��#@�x�@�?}@�7L@�/@�V@�%@���@�A�@�(�@�1@��
@��w@�|�@�\)@�S�@�33@��y@���@�V@�=q@�{@��@�hs@�7L@��@�%@��9@�j@�1'@���@��@���@�S�@��@��H@��@���@�v�@�V@�M�@�@��^@�`B@�&�@��@��/@��@��u@�bN@��@��;@���@�ƨ@��@�t�@�K�@�C�@�C�@�;d@��@�
=@��H@���@�v�@�ff@�M�@�$�@��^@�`B@�?}@�G�@��@�%@���@��N@|�@j�X1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B1'B1'B1'B2-B6FB=qBC�B;dBA�BD�BK�BO�BQ�BS�BXB[#B[#BZBYBVBP�BL�BP�BT�BXB_;BhsBgmBdZB_;BdZBl�Bn�Bq�Bo�Bt�B�JB�hB��B��B�B�B�'B�9B�?B�?B�FB�LB�LB�LB�LB�FB�LB�XB�^B�^B�XB�dB�wBƨB��BȴB�XB�
B��B�!B��B�\B�B�B� Bz�Bq�BhsBYBVBF�B=qB:^B>wB;dB8RB.B�B%B��B��B�B�/B�qB��B�uB�%B� B}�B|�Bx�BjBdZB\)BYBT�BQ�BK�B7LB(�B �B
=BBB
��B
�B
�)B
��B
�B
��B
��B
�DB
u�B
H�B
9XB
0!B
'�B
�B	��B	�B	�)B	�B	��B	ɺB	ƨB	ÖB	�}B	�dB	�3B	��B	��B	��B	�B	o�B	`BB	D�B	;dB	49B	0!B	,B	7LB	B�B	H�B	G�B	B�B	@�B	>wB	9XB	,B	%�B	�B	uB	JB	1B	B	B	  B��B��B�B�B�fB�NB�)B�B��B��B��B��B��BǮBĜB��B�jB�RB�FB�3B�B�B��B��B��B��B��B��B��B��B��B��B��B�oB�bB�PB�JB�DB�7B�1B�B�B~�B|�Bz�By�Bw�Bu�Bs�Br�Bq�Bq�Bp�Bp�Bn�Bl�BjBjBiyBiyBl�Bm�Bm�Bl�Bo�Bu�Bw�Bv�Bu�Bs�Bs�Bs�Bq�Br�Bt�Bs�Bp�Bo�Bn�Bm�Bm�Bl�BjBk�Bk�Bk�Bk�Bk�Bk�Bk�Bo�Bo�Bq�Br�Br�Br�Br�Br�Br�Bv�By�Bz�Bz�Bz�By�Bz�B{�B~�B�B�B�+B�1B�7B�=B�JB�\B�hB�hB�oB�{B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�B�FB�XB�dB�jB�wBĜB��BǮB��B��B�B�)B�BB�TB�`B�B�B�B�B�B��B��B��B��B	B	B	
=B	JB	bB	hB	{B	�B	�B	�B	�B	�B	�B	�B	$�B	(�B	,B	/B	33B	6FB	8RB	8RB	;dB	?}B	?}B	@�B	E�B	F�B	G�B	K�B	N�B	Q�B	S�B	YB	]/B	_;B	bNB	dZB	ffB	k�B	n�B	o�B	q�B	s�B	u�B	u�B	u�B	v�B	y�B	|�B	� B	�B	�B	�B	�+B	�1B	�1B	�7B	�\B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�!B	�-B	�-B	�3B	�?B	�FB	�FB	�LB	�XB	�dB	�qB	�}B	��B	��B	��B	��B	B	ĜB	ŢB	ƨB	ǮB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�#B	�/B	�5B	�HB	�NB	�TB	�ZB	�`B	�fB	�fB	�mB	�sB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
%B
+B
1B
mB
�B
.I2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 B1'B1'B1'B2-B6FB=qBC�B;dBA�BD�BK�BO�BQ�BS�BXB[#B[#BZBYBVBP�BL�BP�BT�BXB_;BhsBgmBdZB_;BdZBl�Bn�Bq�Bo�Bt�B�JB�hB��B��B�B�B�'B�9B�?B�?B�FB�LB�LB�LB�LB�FB�LB�XB�^B�^B�XB�dB�wBƨB��BȴB�XB�
B��B�!B��B�\B�B�B� Bz�Bq�BhsBYBVBF�B=qB:^B>wB;dB8RB.B�B%B��B��B�B�/B�qB��B�uB�%B� B}�B|�Bx�BjBdZB\)BYBT�BQ�BK�B7LB(�B �B
=BBB
��B
�B
�)B
��B
�B
��B
��B
�DB
u�B
H�B
9XB
0!B
'�B
�B	��B	�B	�)B	�B	��B	ɺB	ƨB	ÖB	�}B	�dB	�3B	��B	��B	��B	�B	o�B	`BB	D�B	;dB	49B	0!B	,B	7LB	B�B	H�B	G�B	B�B	@�B	>wB	9XB	,B	%�B	�B	uB	JB	1B	B	B	  B��B��B�B�B�fB�NB�)B�B��B��B��B��B��BǮBĜB��B�jB�RB�FB�3B�B�B��B��B��B��B��B��B��B��B��B��B��B�oB�bB�PB�JB�DB�7B�1B�B�B~�B|�Bz�By�Bw�Bu�Bs�Br�Bq�Bq�Bp�Bp�Bn�Bl�BjBjBiyBiyBl�Bm�Bm�Bl�Bo�Bu�Bw�Bv�Bu�Bs�Bs�Bs�Bq�Br�Bt�Bs�Bp�Bo�Bn�Bm�Bm�Bl�BjBk�Bk�Bk�Bk�Bk�Bk�Bk�Bo�Bo�Bq�Br�Br�Br�Br�Br�Br�Bv�By�Bz�Bz�Bz�By�Bz�B{�B~�B�B�B�+B�1B�7B�=B�JB�\B�hB�hB�oB�{B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�B�FB�XB�dB�jB�wBĜB��BǮB��B��B�B�)B�BB�TB�`B�B�B�B�B�B��B��B��B��B	B	B	
=B	JB	bB	hB	{B	�B	�B	�B	�B	�B	�B	�B	$�B	(�B	,B	/B	33B	6FB	8RB	8RB	;dB	?}B	?}B	@�B	E�B	F�B	G�B	K�B	N�B	Q�B	S�B	YB	]/B	_;B	bNB	dZB	ffB	k�B	n�B	o�B	q�B	s�B	u�B	u�B	u�B	v�B	y�B	|�B	� B	�B	�B	�B	�+B	�1B	�1B	�7B	�\B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�!B	�-B	�-B	�3B	�?B	�FB	�FB	�LB	�XB	�dB	�qB	�}B	��B	��B	��B	��B	B	ĜB	ŢB	ƨB	ǮB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�#B	�/B	�5B	�HB	�NB	�TB	�ZB	�`B	�fB	�fB	�mB	�sB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
%B
+B
1B
mB
�B
.I2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.26 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005191754                              AO  ARCAADJP                                                                    20181005191754    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005191754  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005191754  QCF$                G�O�G�O�G�O�8000            