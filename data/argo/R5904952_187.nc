CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:05:48Z creation      
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
_FillValue                 �  @�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  B�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  JP   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L0   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  S�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  [0   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ]   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  d�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  fp   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  m�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  up   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  wP   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ~�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20181005190548  20181005190548  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6431                            2B  A   APEX                            7466                            062512                          846 @��)�qz�1   @��*/h`�@1�
=p���c��;dZ1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   A   B  B   B'��B/��B8  B@  BG��BO��BX  B_��Bg��Bo��Bw��B��B�  B�33B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  C   C  C  C�fC  C
  C  C�C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0�C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Ci�fCl  Cn  Cp  Cr  Ct  Cv  Cx  Cz�C|  C~  C�  C�  C��C��C��C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C��C�  C�  C�  C��C��C�  C��3C��3C�  C�  C�  C��C�  C��3C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C��C�  C�  C�  C��3C��3C��3C�  C��C��C��C�  C��C��C�  C��3C�  C��3C�  C�  C�  C��3C��3C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C��C�  C�  C��C�  C��3C��3C�  C�  C�  C�  C��3D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D	  D	� D
  D
�fDfD� DfD�fDfD� D  D� D  Dy�D  D� D��Dy�D  D�fD  D� D  D�fDfD� D  D�fD  Dy�D  Dy�D  D� DfD� D  D� D  Dy�D  D� DfD�fD  D� D   D � D!  D!� D"  D"� D"��D#y�D#��D$� D%  D%� D%��D&y�D'  D'�fD(fD(�fD)fD)�fD*  D*y�D+  D+�fD,  D,y�D-  D-� D-��D.� D/fD/� D0  D0� D1  D1y�D2  D2� D2��D3� D4  D4� D5fD5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<fD<�fD=  D=y�D>  D>� D?fD?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DDy�DE  DE� DF  DF� DG  DG� DH  DH� DI  DI�fDJ  DJ�fDKfDK�fDL  DL� DMfDM� DM��DNy�DO  DO� DP  DP� DQfDQ� DQ��DR� DSfDS� DT  DT� DU  DU� DV  DV� DWfDW� DX  DX� DY  DY� DY��DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� DafDa�fDb  Db� Dc  Dc� DdfDd�fDefDe�fDf  Df� Dg  Dg�fDhfDh� Di  Di� Dj  Dj�fDk  Dk�fDl  Dl� Dm  Dm� Dn  Dny�Do  Do� Do��Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dty�Du  Du� Dv  Dv� Dw  Dwy�Dw�3Dy�3D�I�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B(�B!(�B(B0B9(�BA(�BHBPBY(�B`BhBpBxB�aHB��{B�ǮB��{B��{B�ǮB��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B�ǮBĔ{BȔ{B̔{B�aHBԔ{Bؔ{Bܔ{B��{B�{B�{B�aHB�{B��{B��{B��{C J=CJ=CJ=C0�CJ=C
J=CJ=Cc�CJ=CJ=CJ=CJ=CJ=CJ=CJ=CJ=C J=C"J=C$J=C&J=C(J=C*J=C,J=C.J=C0c�C2J=C4J=C6J=C8J=C:J=C<J=C>J=C@J=CBJ=CDJ=CFJ=CHJ=CJJ=CLJ=CNJ=CPJ=CRJ=CTJ=CVJ=CXJ=CZJ=C\J=C^J=C`J=CbJ=CdJ=CfJ=ChJ=Cj0�ClJ=CnJ=CpJ=CrJ=CtJ=CvJ=CxJ=Czc�C|J=C~J=C�%C�%C�1�C�1�C�1�C�%C�%C�%C�%C�%C�%C�%C�RC�%C�%C�%C�%C�RC�%C�%C�%C�%C�%C�1�C�1�C�%C�%C�%C�%C�%C�1�C�%C�%C�%C�1�C�1�C�%C�RC�RC�%C�%C�%C�1�C�%C�RC�%C�%C�%C�%C�%C�%C�1�C�%C�%C�%C�RC�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�1�C�%C�%C�%C�%C�%C�%C�%C�1�C�%C�%C�%C�%C�1�C�%C�%C�%C�RC�RC�RC�%C�1�C�1�C�1�C�%C�1�C�1�C�%C�RC�%C�RC�%C�%C�%C�RC�RC�%C�%C�%C�%C�1�C�%C�%C�%C�%C�%C�%C�RC�RC�%C�%C�1�C�%C�%C�1�C�%C�RC�RC�%C�%C�%C�%D )D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D)D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D�)D�D��D)D�)D�D��D�D��D�D��D�D��D�D��D�D�)D�D�)D�D��D�D��D�D��D�D�)D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#)D#�)D$)D$��D%�D%��D&)D&�)D'�D'��D(�D(��D)�D)��D*�D*�)D+�D+��D,�D,�)D-�D-��D.)D.��D/�D/��D0�D0��D1�D1�)D2�D2��D3)D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=�)D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD�)DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN)DN�)DO�DO��DP�DP��DQ�DQ��DR)DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ)DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn�)Do�Do��Dp)Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt�)Du�Du��Dv�Dv��Dw�Dw�)Dw��Dy��D�R�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�  A�9XA͗�A�v�A��Ạ�A�v�A��A���A�K�A�1A��A��AǙ�A�7LA��A��A�A�AǍPA�dZA�dZA�(�A�^5A�ZA� �A��A�/A���A��A�AŶFA��A�-AÏ\A���A���A�ffA��PA�G�A��-A��A���A���A���A�x�A��A��+A��-A��A�bNA��;A��A��hA�v�A�$�A���A��A���A���A�|�A��FA�JA��/A�+A�z�A�9XA�S�A�ZA�?}A�E�A�
=A��hA�t�A�jA�G�A�Q�A���A���A���A�|�A�jA�~�A��A��A���A��A���A�1A��A�ĜA��A|{Ay��AxZAtv�Aq\)Ao�Ak��Aix�AeAa��A^ZAZ(�AWx�AU��AT�`AT�RAR��AP��AO%AM�AKVAI�hAHA�AFz�AE�AC�A@�yA>�9A>1'A=S�A<�RA;��A:��A8bNA41A1�
A0JA/�
A/�A/
=A.�\A-K�A,-A*~�A(��A(9XA'K�A&n�A$�A${A#��A"�A!��A ��A   A"�A�uA=qA��A|�A�uAA�
A��A�A��AbA7LA1AA�wAz�A�wAp�A�yAt�AĜA�hA
�A	�
A	XAȴA�AE�A��A/A~�A�A��A�Ap�A��AȴA\)A%AVA�A"�@��@���@�9X@�C�@��\@�5?@�/@�(�@�l�@�n�@�=q@�?}@���@�C�@�@�|�@�|�@�v�@�j@�;d@ꟾ@�^@�Ĝ@�Z@�I�@��;@�@�D@���@�-@�7@�X@��@��D@�l�@�V@ݲ-@�`B@�Ĝ@ۮ@ڸR@١�@��/@�  @���@�@���@�;d@�~�@�v�@��T@�@Ь@��@�V@���@�`B@�V@���@�A�@�9X@��@˅@�33@���@�M�@���@Ɂ@�V@ț�@�Z@�1'@�  @�@���@ư!@Ƨ�@�E�@ũ�@�`B@��@�1@�+@�@�V@���@�G�@�%@�Ĝ@���@��D@��@��P@�;d@���@�$�@��7@�I�@�b@��
@�t�@�C�@�+@��H@��\@�@�O�@��/@�1'@��;@�ƨ@��y@��^@�~�@�E�@�-@���@�7L@��`@��@���@�dZ@�  @���@��@���@��h@�(�@�  @�ƨ@�dZ@��y@��@�x�@�`B@�7L@�V@��@�ƨ@�o@���@�v�@�J@�J@�J@�@���@��#@��-@��h@�`B@��@�bN@�  @�ƨ@��F@��@���@���@���@���@���@���@�l�@�33@��@�
=@���@��y@���@���@�ff@��7@��/@���@�A�@���@��@�l�@�S�@�;d@�+@��@�
=@��y@�=q@���@�X@�G�@�7L@�V@���@�(�@�b@���@�
=@��R@�M�@�$�@�@�hs@�r�@�|�@�+@���@�M�@�-@�-@�$�@�@��#@�&�@���@�9X@�ƨ@���@��P@��P@���@���@��P@�S�@��@�~�@�M�@�=q@�@�G�@��@�z�@��@��F@�t�@�"�@���@�=q@�{@��#@�x�@�V@���@��j@�Z@�I�@�A�@��@�  @���@���@�;d@��!@��\@��+@�J@�@�?}@��@���@��`@�r�@��@�K�@��@���@�$�@��^@���@���@��@�z�@�r�@�(�@���@���@��@�"�@���@���@�v�@�ff@��@���@��@�V@���@��`@��/@��@��D@���@v��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�  A�9XA͗�A�v�A��Ạ�A�v�A��A���A�K�A�1A��A��AǙ�A�7LA��A��A�A�AǍPA�dZA�dZA�(�A�^5A�ZA� �A��A�/A���A��A�AŶFA��A�-AÏ\A���A���A�ffA��PA�G�A��-A��A���A���A���A�x�A��A��+A��-A��A�bNA��;A��A��hA�v�A�$�A���A��A���A���A�|�A��FA�JA��/A�+A�z�A�9XA�S�A�ZA�?}A�E�A�
=A��hA�t�A�jA�G�A�Q�A���A���A���A�|�A�jA�~�A��A��A���A��A���A�1A��A�ĜA��A|{Ay��AxZAtv�Aq\)Ao�Ak��Aix�AeAa��A^ZAZ(�AWx�AU��AT�`AT�RAR��AP��AO%AM�AKVAI�hAHA�AFz�AE�AC�A@�yA>�9A>1'A=S�A<�RA;��A:��A8bNA41A1�
A0JA/�
A/�A/
=A.�\A-K�A,-A*~�A(��A(9XA'K�A&n�A$�A${A#��A"�A!��A ��A   A"�A�uA=qA��A|�A�uAA�
A��A�A��AbA7LA1AA�wAz�A�wAp�A�yAt�AĜA�hA
�A	�
A	XAȴA�AE�A��A/A~�A�A��A�Ap�A��AȴA\)A%AVA�A"�@��@���@�9X@�C�@��\@�5?@�/@�(�@�l�@�n�@�=q@�?}@���@�C�@�@�|�@�|�@�v�@�j@�;d@ꟾ@�^@�Ĝ@�Z@�I�@��;@�@�D@���@�-@�7@�X@��@��D@�l�@�V@ݲ-@�`B@�Ĝ@ۮ@ڸR@١�@��/@�  @���@�@���@�;d@�~�@�v�@��T@�@Ь@��@�V@���@�`B@�V@���@�A�@�9X@��@˅@�33@���@�M�@���@Ɂ@�V@ț�@�Z@�1'@�  @�@���@ư!@Ƨ�@�E�@ũ�@�`B@��@�1@�+@�@�V@���@�G�@�%@�Ĝ@���@��D@��@��P@�;d@���@�$�@��7@�I�@�b@��
@�t�@�C�@�+@��H@��\@�@�O�@��/@�1'@��;@�ƨ@��y@��^@�~�@�E�@�-@���@�7L@��`@��@���@�dZ@�  @���@��@���@��h@�(�@�  @�ƨ@�dZ@��y@��@�x�@�`B@�7L@�V@��@�ƨ@�o@���@�v�@�J@�J@�J@�@���@��#@��-@��h@�`B@��@�bN@�  @�ƨ@��F@��@���@���@���@���@���@���@�l�@�33@��@�
=@���@��y@���@���@�ff@��7@��/@���@�A�@���@��@�l�@�S�@�;d@�+@��@�
=@��y@�=q@���@�X@�G�@�7L@�V@���@�(�@�b@���@�
=@��R@�M�@�$�@�@�hs@�r�@�|�@�+@���@�M�@�-@�-@�$�@�@��#@�&�@���@�9X@�ƨ@���@��P@��P@���@���@��P@�S�@��@�~�@�M�@�=q@�@�G�@��@�z�@��@��F@�t�@�"�@���@�=q@�{@��#@�x�@�V@���@��j@�Z@�I�@�A�@��@�  @���@���@�;d@��!@��\@��+@�J@�@�?}@��@���@��`@�r�@��@�K�@��@���@�$�@��^@���@���@��@�z�@�r�@�(�@���@���@��@�"�@���@���@�v�@�ff@��@���@��@�V@���@��`@��/@��@��D@���@v��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
��B
�XB
�B
��BJB�B�B
�NB
��B
�=B
�PB
�uB
��B
ÖB
��B
��B
�NB
�BDB�B,B,BF�BS�BP�BZBm�Bl�BP�BXB{�B��B��BɺBȴBɺB�ZB��B\BuB�BbB�B%�B"�B!�B�B�BF�BS�BS�BM�BJ�BK�BI�BE�B=qB6FB/B'�B!�B�BoB
=BB�B�sB�#B��B�jB�LB��Bz�BL�B8RB%�B
��B
�NB
��B
��B
�B
��B
�B
v�B
l�B
>wB
,B
"�B
B	�B	�HB	��B	�dB	�'B	��B	�B	p�B	]/B	N�B	9XB	&�B	�B	  B�B�B�mB�B�mB�B��BǮBŢB��B�dB�?B�-B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�{B�uB��B��B�{B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�'B�!B�B�B�B��B��B��B�B�B�B�B�B�B�B�B�!B�'B�-B�^B�dBB�jB��B��BÖB��B�wB�wB�}B�}B�wB�}BBŢBǮB��B��B��B�B�B�#B�B�5B�BB�)B�#B�)B�;B�ZB�`B�fB�yB�fB�mB�B�B�B�B�B�B��B��B��B��B��B��B	B	B	%B	JB	\B	bB	oB	uB	uB	�B	�B	$�B	#�B	 �B	!�B	#�B	%�B	&�B	&�B	'�B	+B	-B	0!B	1'B	2-B	5?B	:^B	?}B	D�B	E�B	F�B	F�B	G�B	L�B	M�B	M�B	P�B	R�B	ZB	\)B	^5B	cTB	ffB	hsB	l�B	o�B	p�B	q�B	r�B	s�B	s�B	v�B	z�B	{�B	|�B	~�B	� B	}�B	�B	�B	�%B	�JB	�bB	�oB	�uB	�uB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	�B	�!B	�'B	�-B	�9B	�?B	�LB	�LB	�RB	�RB	�jB	�qB	�wB	�}B	�}B	�}B	�}B	�}B	�wB	�}B	��B	ĜB	ŢB	ƨB	ƨB	ƨB	ƨB	ƨB	ƨB	ƨB	ƨB	ȴB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�)B	�/B	�5B	�5B	�5B	�5B	�BB	�BB	�HB	�HB	�NB	�NB	�TB	�ZB	�TB	�ZB	�fB	�sB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
  B
  B
B
B
B
B
B
B
+B
1B
1B
	7B

=B
JB
JB
PB
PB
PB
VB
\B
bB
bB
bB
bB
\B
\B
\B
bB
hB
hB
oB
uB
uB
uB
uB
�B
2B
)�222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222B
��B
�XB
�B
��BJB�B�B
�NB
��B
�=B
�PB
�uB
��B
ÖB
��B
��B
�NB
�BDB�B,B,BF�BS�BP�BZBm�Bl�BP�BXB{�B��B��BɺBȴBɺB�ZB��B\BuB�BbB�B%�B"�B!�B�B�BF�BS�BS�BM�BJ�BK�BI�BE�B=qB6FB/B'�B!�B�BoB
=BB�B�sB�#B��B�jB�LB��Bz�BL�B8RB%�B
��B
�NB
��B
��B
�B
��B
�B
v�B
l�B
>wB
,B
"�B
B	�B	�HB	��B	�dB	�'B	��B	�B	p�B	]/B	N�B	9XB	&�B	�B	  B�B�B�mB�B�mB�B��BǮBŢB��B�dB�?B�-B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�{B�uB��B��B�{B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�'B�!B�B�B�B��B��B��B�B�B�B�B�B�B�B�B�!B�'B�-B�^B�dBB�jB��B��BÖB��B�wB�wB�}B�}B�wB�}BBŢBǮB��B��B��B�B�B�#B�B�5B�BB�)B�#B�)B�;B�ZB�`B�fB�yB�fB�mB�B�B�B�B�B�B��B��B��B��B��B��B	B	B	%B	JB	\B	bB	oB	uB	uB	�B	�B	$�B	#�B	 �B	!�B	#�B	%�B	&�B	&�B	'�B	+B	-B	0!B	1'B	2-B	5?B	:^B	?}B	D�B	E�B	F�B	F�B	G�B	L�B	M�B	M�B	P�B	R�B	ZB	\)B	^5B	cTB	ffB	hsB	l�B	o�B	p�B	q�B	r�B	s�B	s�B	v�B	z�B	{�B	|�B	~�B	� B	}�B	�B	�B	�%B	�JB	�bB	�oB	�uB	�uB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	�B	�!B	�'B	�-B	�9B	�?B	�LB	�LB	�RB	�RB	�jB	�qB	�wB	�}B	�}B	�}B	�}B	�}B	�wB	�}B	��B	ĜB	ŢB	ƨB	ƨB	ƨB	ƨB	ƨB	ƨB	ƨB	ƨB	ȴB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�)B	�/B	�5B	�5B	�5B	�5B	�BB	�BB	�HB	�HB	�NB	�NB	�TB	�ZB	�TB	�ZB	�fB	�sB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
  B
  B
B
B
B
B
B
B
+B
1B
1B
	7B

=B
JB
JB
PB
PB
PB
VB
\B
bB
bB
bB
bB
\B
\B
\B
bB
hB
hB
oB
uB
uB
uB
uB
�B
2B
)�222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.29 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190548                              AO  ARCAADJP                                                                    20181005190548    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190548  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190548  QCF$                G�O�G�O�G�O�8000            