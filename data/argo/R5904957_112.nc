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
resolution        =���   axis      Z        t  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  @�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     t  B�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J8   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     t  L   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     t  S�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  [    TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     t  \�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  dT   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     t  f4   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     t  m�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  u   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     t  v�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ~p   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     t  �P   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �    HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �$   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �(   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �,   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �0   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �p   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20181024140825  20181024140825  5904957 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               pA   AO  6560                            2B  A   APEX                            7471                            062512                          846 @�ˤ�$�1   @�˥X�@4.��+�c֟�vȴ1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      pA   A   A   @@  @�  @�  A   A   A@  A^ffA~ffA�  A�  A�  A�  A�  A�  A�  B   BffB  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�33B�  B�  B�33B�33B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD�CF  CH  CJ  CL  CN  CP  CR�CT  CV  CX  CZ  C\  C^�C`  Cb  Cd�Cf�Ch  Cj  Cl  Cn  Cp  Cr�Ct  Cv  Cx  Cz  C|  C~�C��C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C�  C�  C�  C��C��C�  C�  C��3C��3C�  C��3C��3C��3C��3C�  C�  C�  C��C��C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C��3C��3C��3C�  C�  C�  C��C�  D   D �fD  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D%��D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+y�D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D1��D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8fD8�fD9  DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DJ��DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DSfDS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D^��D_� D`fD`�fDafDa�fDbfDb�fDcfDc�fDd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk�fDl  Dly�Dm  Dm� Dn  Dn� Do  Do� DpfDp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt��Du� Dv  Dv�fDwfDw� Dx  DxS3Dy��D�5�D��3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @R�\@�G�@�G�A��A$��AD��Ac
=A��A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�B(�B	�\B(�B(�B!(�B)(�B1(�B9(�BA(�BI(�BQ(�BY(�Ba(�Bi(�Bq(�By(�B��{B��{B��{B��{B��{B��{B��{B�aHB��{B��{B��{B��{B��{B��{B��{B��{B��{BĔ{BȔ{B̔{B�ǮBԔ{Bؔ{B�ǮB��{B�{B�ǮB�ǮB�{B��{B��{B��{C J=CJ=CJ=CJ=CJ=C
J=CJ=CJ=CJ=CJ=CJ=CJ=CJ=CJ=CJ=CJ=C J=C"J=C$J=C&J=C(J=C*J=C,J=C.J=C0J=C2J=C4J=C6J=C8J=C:J=C<J=C>J=C@J=CBJ=CDc�CFJ=CHJ=CJJ=CLJ=CNJ=CPJ=CRc�CTJ=CVJ=CXJ=CZJ=C\J=C^c�C`J=CbJ=Cdc�Cfc�ChJ=CjJ=ClJ=CnJ=CpJ=Crc�CtJ=CvJ=CxJ=CzJ=C|J=C~c�C�1�C�%C�%C�%C�%C�%C�%C�%C�%C�RC�RC�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�1�C�%C�%C�%C�%C�%C�%C�1�C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�RC�%C�%C�%C�1�C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�RC�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�RC�%C�1�C�%C�%C�%C�1�C�1�C�%C�%C�RC�RC�%C�RC�RC�RC�RC�%C�%C�%C�1�C�1�C�%C�%C�%C�%C�%C�1�C�1�C�%C�%C�%C�%C�RC�RC�RC�%C�%C�%C�1�C�%D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&)D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+�)D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2)D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK)DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_)D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl�)Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Du)Du��Dv�Dv��Dw�Dw��Dx�Dxe�Dy�3D�?D��{111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��A��yA��TA��`A��A��A��yA��A���A���A���A���A�=qA�A�p�A�&�A���A��#AᗍA�dZA�ƨAߋDA���A�?}Aٟ�A�^5A�JA�^5A���A�C�A�jA�z�A�/AͬA��`A̅Aˏ\A�&�A�jA�XAƙ�Aš�A���A�/A���A���A���A�/A��RA���A��A��9A�S�A��yA��FA��/A���A��hA��;A��^A��A�x�A�G�A�-A���A�A��A�S�A���A��`A��+A�33A��;A�%A��mA�-A��mA�O�A��A��;A�O�A�oA�K�A�z�A���A�/A��A��A�33A�{A��hA���A��
A�5?A�A�
=A�\)A��A���A�v�A�A���A��A��A�x�A~ZAz�Aw��Avv�As"�Ann�Al9XAk;dAj�Ai�Ai�Ah�Afz�Ab��A^(�A\�`A\ �A[`BAY�FAXJAU�AR^5ANE�AKx�AIl�AGl�AF�AE�
AD�AB��AA�7A@E�A?��A>�/A<E�A:�`A9�7A7�PA61'A5�A5��A5+A4��A4  A2��A1��A0��A/��A.��A.bNA-�A-A,1A(�A'�A&~�A&bA%�
A$$�A"ffA!dZA!;dA ĜA =qA��A|�A�Ax�A��A�A��A�A+A1A?}A��AO�A�HAA�AoA"�A��A
��A	7LA(�Az�AĜAA�A��A?}AdZA �@��y@���@��9@�I�@��
@��P@�@��@�b@�ƨ@��@�E�@�@�%@�I�@@�dZ@�hs@��H@���@��@�@�z�@��H@���@噚@���@��@�K�@��H@�J@���@�Z@ߝ�@���@�I�@�l�@�S�@�l�@��H@���@ڏ\@���@ٲ-@ٙ�@�7L@؋D@��@�7L@�Z@���@�v�@�=q@�J@с@��/@д9@д9@Ь@ύP@�C�@Ώ\@�-@ͩ�@��@̛�@� �@�(�@�;d@��T@�@ɡ�@Ɂ@��
@�M�@���@�p�@��@�Ĝ@�(�@�S�@�@��@���@��@�Z@��@�ƨ@�C�@�@��@���@�E�@�@���@�/@���@�1'@� �@��
@��w@�  @��@�b@���@�ƨ@�dZ@��H@�~�@�^5@�E�@�-@���@��/@���@��D@�j@��;@�C�@��y@���@�V@�5?@�{@�@��T@�X@���@���@�(�@��F@�K�@��+@��@�v�@��@��@��@�  @��@�\)@�@���@���@��@�Ĝ@�z�@��@��y@��+@�~�@�=q@��@���@��h@��D@���@�dZ@�@��R@��!@�n�@�V@�=q@��+@��y@��!@��@�hs@�&�@��j@��@��@���@���@���@���@��P@�\)@�C�@��\@��-@�x�@��@�%@��@�G�@�7L@��/@��j@���@�  @���@�@���@��@�"�@�
=@�V@���@��T@��#@�v�@���@���@���@�V@�@���@�@��@�Q�@��w@���@�1'@�I�@��m@�dZ@��F@���@�\)@�ȴ@���@�n�@�-@���@���@��@�7L@���@���@�Z@�dZ@�M�@��-@��^@��@��-@��`@��@�A�@��
@�t�@�dZ@�C�@�
=@��@�ȴ@���@��!@��!@���@�=q@���@��-@�X@��@��@�A�@��F@���@��w@�C�@�o@���@��R@���@�v�@�-@�M�@��@��^@��@�X@���@��@�q�@pXy@a��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��A��yA��TA��`A��A��A��yA��A���A���A���A���A�=qA�A�p�A�&�A���A��#AᗍA�dZA�ƨAߋDA���A�?}Aٟ�A�^5A�JA�^5A���A�C�A�jA�z�A�/AͬA��`A̅Aˏ\A�&�A�jA�XAƙ�Aš�A���A�/A���A���A���A�/A��RA���A��A��9A�S�A��yA��FA��/A���A��hA��;A��^A��A�x�A�G�A�-A���A�A��A�S�A���A��`A��+A�33A��;A�%A��mA�-A��mA�O�A��A��;A�O�A�oA�K�A�z�A���A�/A��A��A�33A�{A��hA���A��
A�5?A�A�
=A�\)A��A���A�v�A�A���A��A��A�x�A~ZAz�Aw��Avv�As"�Ann�Al9XAk;dAj�Ai�Ai�Ah�Afz�Ab��A^(�A\�`A\ �A[`BAY�FAXJAU�AR^5ANE�AKx�AIl�AGl�AF�AE�
AD�AB��AA�7A@E�A?��A>�/A<E�A:�`A9�7A7�PA61'A5�A5��A5+A4��A4  A2��A1��A0��A/��A.��A.bNA-�A-A,1A(�A'�A&~�A&bA%�
A$$�A"ffA!dZA!;dA ĜA =qA��A|�A�Ax�A��A�A��A�A+A1A?}A��AO�A�HAA�AoA"�A��A
��A	7LA(�Az�AĜAA�A��A?}AdZA �@��y@���@��9@�I�@��
@��P@�@��@�b@�ƨ@��@�E�@�@�%@�I�@@�dZ@�hs@��H@���@��@�@�z�@��H@���@噚@���@��@�K�@��H@�J@���@�Z@ߝ�@���@�I�@�l�@�S�@�l�@��H@���@ڏ\@���@ٲ-@ٙ�@�7L@؋D@��@�7L@�Z@���@�v�@�=q@�J@с@��/@д9@д9@Ь@ύP@�C�@Ώ\@�-@ͩ�@��@̛�@� �@�(�@�;d@��T@�@ɡ�@Ɂ@��
@�M�@���@�p�@��@�Ĝ@�(�@�S�@�@��@���@��@�Z@��@�ƨ@�C�@�@��@���@�E�@�@���@�/@���@�1'@� �@��
@��w@�  @��@�b@���@�ƨ@�dZ@��H@�~�@�^5@�E�@�-@���@��/@���@��D@�j@��;@�C�@��y@���@�V@�5?@�{@�@��T@�X@���@���@�(�@��F@�K�@��+@��@�v�@��@��@��@�  @��@�\)@�@���@���@��@�Ĝ@�z�@��@��y@��+@�~�@�=q@��@���@��h@��D@���@�dZ@�@��R@��!@�n�@�V@�=q@��+@��y@��!@��@�hs@�&�@��j@��@��@���@���@���@���@��P@�\)@�C�@��\@��-@�x�@��@�%@��@�G�@�7L@��/@��j@���@�  @���@�@���@��@�"�@�
=@�V@���@��T@��#@�v�@���@���@���@�V@�@���@�@��@�Q�@��w@���@�1'@�I�@��m@�dZ@��F@���@�\)@�ȴ@���@�n�@�-@���@���@��@�7L@���@���@�Z@�dZ@�M�@��-@��^@��@��-@��`@��@�A�@��
@�t�@�dZ@�C�@�
=@��@�ȴ@���@��!@��!@���@�=q@���@��-@�X@��@��@�A�@��F@���@��w@�C�@�o@���@��R@���@�v�@�-@�M�@��@��^@��@�X@���@��@�q�@pXy@a��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BBB  B  B  BB  BB	7B{B#�B/BT�Bx�B�uB�hB�JB�7B�=B�\B�\Bx�B^5B[#B}�B��BǮB��B��B�#B�BB�B��B�B0!B8RB/B,B0!BL�BL�BK�BM�BO�BD�BE�BJ�BQ�B`BBv�B�+B�7B�=B�%B�B|�Bt�BiyBbNBYBE�B0!B�B+B33B/B&�B-B)�B&�B!�B�B�B�B1B��B�TB��B�^B��B�uB�%B�7B�VB{�Br�BiyBR�BD�B5?B.B!�B
��B
�B
�B
�;B
ƨB
�wB
�RB
�?B
�B
��B
�B
n�B
T�B
A�B
+B
�B
1B	�B	��B	ĜB	�jB	�?B	�'B	�B	��B	��B	�B	l�B	e`B	_;B	ZB	O�B	C�B	5?B	#�B	bB	B��B�B�B�sB�BB�#B�
B��B��B��BĜB�qB�RB�9B�-B�'B�B�B�B�B��B��B��B��B��B��B��B��B��B��B�{B�uB�oB�hB�hB�bB�bB�bB�bB�bB�\B�bB�bB�bB�VB�PB�DB�=B�7B�%B�B}�B{�Bz�Bx�Bv�B}�Bq�Bs�Br�Bn�BgmBdZBffBffBgmBiyBk�Bo�Bq�Br�Bs�Bt�Bt�Bt�Bv�B}�B~�B}�B{�Bv�Bu�Bt�Bs�Bq�Bq�Bp�Bo�Bs�Bw�By�B{�B~�B�B�B�+B�=B�VB�oB��B��B��B��B��B��B��B��B�B�B�B�B�3B�9B�?B�?B�?B�LB�qB��BȴBɺBɺB��B��B��B��B��B��B�
B�BB�TB�HB�;B�;B�NB�`B�`B�yB�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B	  B	B	B	%B		7B	DB	bB	�B	�B	�B	�B	 �B	�B	 �B	 �B	!�B	$�B	(�B	,B	-B	/B	33B	7LB	>wB	B�B	C�B	C�B	D�B	D�B	F�B	G�B	F�B	G�B	K�B	P�B	S�B	W
B	YB	ZB	[#B	\)B	]/B	_;B	aHB	dZB	gmB	jB	k�B	k�B	hsB	]/B	S�B	W
B	`BB	cTB	iyB	l�B	p�B	s�B	{�B	�B	�B	�B	�+B	�=B	�PB	�VB	�hB	�hB	�hB	�oB	��B	��B	��B	�{B	�{B	�{B	�{B	�uB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�!B	�-B	�3B	�3B	�3B	�3B	�9B	�FB	�RB	�LB	�XB	�^B	�jB	��B	ŢB	ĜB	ĜB	ÖB	B	B	ÖB	ƨB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�
B	�
B	�B	�B	��B	��B	��B	��B	�B	�)B	�5B	�TB	�fB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
%B
+B
	7B
	7B
1B
1B
1B

=B
�B
 �B
-]111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   BBB  B  B  BB  BB	7B{B#�B/BT�Bx�B�uB�hB�JB�7B�=B�\B�\Bx�B^5B[#B}�B��BǮB��B��B�#B�BB�B��B�B0!B8RB/B,B0!BL�BL�BK�BM�BO�BD�BE�BJ�BQ�B`BBv�B�+B�7B�=B�%B�B|�Bt�BiyBbNBYBE�B0!B�B+B33B/B&�B-B)�B&�B!�B�B�B�B1B��B�TB��B�^B��B�uB�%B�7B�VB{�Br�BiyBR�BD�B5?B.B!�B
��B
�B
�B
�;B
ƨB
�wB
�RB
�?B
�B
��B
�B
n�B
T�B
A�B
+B
�B
1B	�B	��B	ĜB	�jB	�?B	�'B	�B	��B	��B	�B	l�B	e`B	_;B	ZB	O�B	C�B	5?B	#�B	bB	B��B�B�B�sB�BB�#B�
B��B��B��BĜB�qB�RB�9B�-B�'B�B�B�B�B��B��B��B��B��B��B��B��B��B��B�{B�uB�oB�hB�hB�bB�bB�bB�bB�bB�\B�bB�bB�bB�VB�PB�DB�=B�7B�%B�B}�B{�Bz�Bx�Bv�B}�Bq�Bs�Br�Bn�BgmBdZBffBffBgmBiyBk�Bo�Bq�Br�Bs�Bt�Bt�Bt�Bv�B}�B~�B}�B{�Bv�Bu�Bt�Bs�Bq�Bq�Bp�Bo�Bs�Bw�By�B{�B~�B�B�B�+B�=B�VB�oB��B��B��B��B��B��B��B��B�B�B�B�B�3B�9B�?B�?B�?B�LB�qB��BȴBɺBɺB��B��B��B��B��B��B�
B�BB�TB�HB�;B�;B�NB�`B�`B�yB�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B	  B	B	B	%B		7B	DB	bB	�B	�B	�B	�B	 �B	�B	 �B	 �B	!�B	$�B	(�B	,B	-B	/B	33B	7LB	>wB	B�B	C�B	C�B	D�B	D�B	F�B	G�B	F�B	G�B	K�B	P�B	S�B	W
B	YB	ZB	[#B	\)B	]/B	_;B	aHB	dZB	gmB	jB	k�B	k�B	hsB	]/B	S�B	W
B	`BB	cTB	iyB	l�B	p�B	s�B	{�B	�B	�B	�B	�+B	�=B	�PB	�VB	�hB	�hB	�hB	�oB	��B	��B	��B	�{B	�{B	�{B	�{B	�uB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�!B	�-B	�3B	�3B	�3B	�3B	�9B	�FB	�RB	�LB	�XB	�^B	�jB	��B	ŢB	ĜB	ĜB	ÖB	B	B	ÖB	ƨB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�
B	�
B	�B	�B	��B	��B	��B	��B	�B	�)B	�5B	�TB	�fB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
%B
+B
	7B
	7B
1B
1B
1B

=B
�B
 �B
-]111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.29 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181024140825                              AO  ARCAADJP                                                                    20181024140825    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181024140825  QCP$                G�O�G�O�G�O�F03E            AO  ARGQQCPL                                                                    20181024140825  QCF$                G�O�G�O�G�O�0               