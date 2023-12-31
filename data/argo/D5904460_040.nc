CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-03-25T09:16:36Z AOML 3.0 creation; 2016-08-07T21:17:35Z UW 3.1 conversion     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7    PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7X   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8<   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8\   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~       axis      T           8`   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8h   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~            8l   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8t   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8|   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  A|   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  Ct   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  KP   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  MH   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  U$   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ]    TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ^�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  f�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  h�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  p�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  x�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  z|   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �X   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �P   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �,   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �\   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �\   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �\   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �\   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �    HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20150325091636  20160807141735  5904460 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               (A   AO  5285_8895_040                   2C  D   APEX                            6487                            072314                          846 @�D�� 	1   @�D�_�@.�vȴ9�c�+I�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    (A   B   B   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B ffB(ffB0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�33B�33B�  B�  B�33B���B���B���B�  B�  B���B���B���B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C#�fC&  C(  C*  C,�C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�fDy� D��D�9�D�s3D���D��D�L�D�� D�� D�	�D�<�D�vfD�ɚD�f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�\)@�A�HA&�HAF�HAf�HA�p�A�p�A�p�A�p�A�p�A�p�A�p�A�p�B�RB	�RB�RB�RB"�B*�B1�RB9�RBA�RBI�RBQ�RBY�RBa�RBi�RBq�RBy�RB�\B�\B��)B��)B�\B�u�B���B���B��)B��)B�u�B���B���B��)B��)B��)B�\B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)C nCnCnCnCnC
nCnCnCnCnCnCnCnCnCnCnC nC"nC$TzC&nC(nC*nC,��C.nC0nC2nC4nC6nC8nC:nC<nC>nC@nCBnCDnCFnCHnCJnCLnCNnCPnCRnCTnCVnCXnCZnC\nC^nC`nCbnCdnCfnChnCjnClnCnnCpnCrnCtnCvnCxnCznC|nC~nC�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Dt��Dy��D�']D�G]D���D��]D�']D�Z�D���D���D�]D�J�D��)D��]D�$)11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��`A��#A���Aԟ�A�n�A�bA�%A���A��A��A��mA��;A��#A��
A��/A��;A���A�ƨA�AӾwAӾwAӾwAӺ^AӲ-AӲ-AӲ-Aӟ�Aӏ\AӓuAӝ�Aӣ�Aә�A���A�|�A·+Ȁ\A�VAɶFA��yA��Aò-A�|�A��A��yA�A���A��HA���A��A�  A�ƨA�E�A��A�l�A�=qA���A��!A�XA��A�33A�A�A�M�A�ZA���A��A��A�-A� �A��wA��/A���A�K�A�bNA�  A��!A���A� �A}�7Az  Av�Aq�Am`BAiK�Ael�AbM�A`ĜA^��AY
=AT�DAO/AL�/AI`BAGXAF�RAF(�AFbAF1AE%AC�FA@��A=��A;x�A:-A8�yA8~�A8-A8$�A7��A7t�A5��A533A4ZA1ƨA.z�A-x�A+7LA)?}A(z�A(VA((�A&�!A$1A"$�A ��AS�AȴA��AȴAM�A�PAjAoA��A`BAx�A;dA��AjA��A��A�A�FA7LAĜAVA�
Ap�A�jA�^A33A�A�A��A/A��A
�A
9XA	��A	�FA	?}A�AbAt�A�Ap�A��A"�A�A�hA v�A|�AVA�hA?}A ��@��+@���@�hs@�&�@�Q�@�(�@�&�@��@�5?@��PA {@�33@�{@�?}@�  @�l�@�"�@��R@�J@���@��y@���@�G�@���@��
@��y@�J@�-@��#@�&�@�j@�Z@���@�@���@���@�@���@�1'@�w@�R@��T@�1'@�n�@��@��@�  @�dZ@���@◍@�5?@�ff@�p�@���@�Q�@�l�@��y@�^5@��T@�$�@��@ܛ�@�(�@��;@�ƨ@�t�@�33@���@ڇ+@�V@ٺ^@ؼj@�1'@�1@��
@���@׶F@�|�@�
=@��@�hs@�V@Դ9@ԣ�@��m@�S�@��y@ҸR@�v�@�$�@�`B@��`@Ь@�I�@υ@�\)@�
=@�{@���@͙�@�`B@̣�@�  @˅@�@ʟ�@��@�O�@��@�Q�@Ǯ@�33@���@�^5@��@Ų-@��@Ĵ9@ēu@�z�@�1'@��;@�|�@��H@�n�@���@�G�@��`@���@��@�b@��P@�33@��R@�^5@�@��@��j@�Q�@�1@��@�@�@�@���@�$�@�@�7L@���@���@�Ĝ@���@�z�@�bN@�b@��@��!@�n�@�M�@�J@�@�hs@�&�@��/@��D@�Q�@�b@���@�l�@���@��+@�E�@��@�@�O�@��@�Ĝ@��u@�Z@��m@�|�@��@���@�-@��@��^@��7@�7L@���@��u@�Z@�A�@��m@���@�"�@��@���@�M�@�=q@�=q@���@���@�z�@�A�@�(�@�  @��w@�C�@���@�ff@�=q@�{@��T@�hs@�&�@���@���@���@��9@� �@��@���@�33@�@���@��+@��@��@��-@�hs@�&�@���@���@��9@�r�@�dZ@�C�@�"�@��H@��R@��\@�~�@�5?@��^@�x�@�X@��@��@��/@��D@�Q�@��
@�l�@�@���@��\@�5?@���@��#@���@�p�@��@��j@��@�1'@�b@��
@���@�+@�V@�-@�{@�J@��T@�7L@��@��@��`@�r�@��F@�l�@�dZ@�33@��@���@���@��\@�-@���@�7L@��`@�r�@�b@��F@�S�@�33@��@�E�@���@�`B@�/@���@���@�j@�9X@�b@��@���@��P@�o@��H@��R@��\@��h@�b@�E�@z�@t1@l1@c��@\�@T�@LZ@D��@?;d@8A�@2��@,Z11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111 A��`A��#A���Aԟ�A�n�A�bA�%A���A��A��A��mA��;A��#A��
A��/A��;A���A�ƨA�AӾwAӾwAӾwAӺ^AӲ-AӲ-AӲ-Aӟ�Aӏ\AӓuAӝ�Aӣ�Aә�A���A�|�A·+Ȁ\A�VAɶFA��yA��Aò-A�|�A��A��yA�A���A��HA���A��A�  A�ƨA�E�A��A�l�A�=qA���A��!A�XA��A�33A�A�A�M�A�ZA���A��A��A�-A� �A��wA��/A���A�K�A�bNA�  A��!A���A� �A}�7Az  Av�Aq�Am`BAiK�Ael�AbM�A`ĜA^��AY
=AT�DAO/AL�/AI`BAGXAF�RAF(�AFbAF1AE%AC�FA@��A=��A;x�A:-A8�yA8~�A8-A8$�A7��A7t�A5��A533A4ZA1ƨA.z�A-x�A+7LA)?}A(z�A(VA((�A&�!A$1A"$�A ��AS�AȴA��AȴAM�A�PAjAoA��A`BAx�A;dA��AjA��A��A�A�FA7LAĜAVA�
Ap�A�jA�^A33A�A�A��A/A��A
�A
9XA	��A	�FA	?}A�AbAt�A�Ap�A��A"�A�A�hA v�A|�AVA�hA?}A ��@��+@���@�hs@�&�@�Q�@�(�@�&�@��@�5?@��PA {@�33@�{@�?}@�  @�l�@�"�@��R@�J@���@��y@���@�G�@���@��
@��y@�J@�-@��#@�&�@�j@�Z@���@�@���@���@�@���@�1'@�w@�R@��T@�1'@�n�@��@��@�  @�dZ@���@◍@�5?@�ff@�p�@���@�Q�@�l�@��y@�^5@��T@�$�@��@ܛ�@�(�@��;@�ƨ@�t�@�33@���@ڇ+@�V@ٺ^@ؼj@�1'@�1@��
@���@׶F@�|�@�
=@��@�hs@�V@Դ9@ԣ�@��m@�S�@��y@ҸR@�v�@�$�@�`B@��`@Ь@�I�@υ@�\)@�
=@�{@���@͙�@�`B@̣�@�  @˅@�@ʟ�@��@�O�@��@�Q�@Ǯ@�33@���@�^5@��@Ų-@��@Ĵ9@ēu@�z�@�1'@��;@�|�@��H@�n�@���@�G�@��`@���@��@�b@��P@�33@��R@�^5@�@��@��j@�Q�@�1@��@�@�@�@���@�$�@�@�7L@���@���@�Ĝ@���@�z�@�bN@�b@��@��!@�n�@�M�@�J@�@�hs@�&�@��/@��D@�Q�@�b@���@�l�@���@��+@�E�@��@�@�O�@��@�Ĝ@��u@�Z@��m@�|�@��@���@�-@��@��^@��7@�7L@���@��u@�Z@�A�@��m@���@�"�@��@���@�M�@�=q@�=q@���@���@�z�@�A�@�(�@�  @��w@�C�@���@�ff@�=q@�{@��T@�hs@�&�@���@���@���@��9@� �@��@���@�33@�@���@��+@��@��@��-@�hs@�&�@���@���@��9@�r�@�dZ@�C�@�"�@��H@��R@��\@�~�@�5?@��^@�x�@�X@��@��@��/@��D@�Q�@��
@�l�@�@���@��\@�5?@���@��#@���@�p�@��@��j@��@�1'@�b@��
@���@�+@�V@�-@�{@�J@��T@�7L@��@��@��`@�r�@��F@�l�@�dZ@�33@��@���@���@��\@�-@���@�7L@��`@�r�@�b@��F@�S�@�33@��@�E�@���@�`B@�/@���@���@�j@�9X@�b@��@���@��P@�o@��H@��R@��\G�O�@�b@�E�@z�@t1@l1@c��@\�@T�@LZ@D��@?;d@8A�@2��@,Z11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;oBk�BjBjBhsBhsBffBffBffBffBffBffBffBffBffBhsBiyBffBdZBcTBcTBbNBbNBbNB`BBaHBbNB_;B_;BcTBhsBk�B�B	��B	�NB
t�B
�9B
��B
��B�B7LB;dB=qBN�BP�BZBhsBs�Bs�Bw�B|�BffB_;BcTBx�Bl�BD�B1'B�B  B
�`B
�/B
�TB
�B
�B
�#B
��B
��B
�^B
��B
�B
��B
�7B
_;B
8RB	��B	�B	�B	��B	�-B	��B	�1B	p�B	[#B	D�B	:^B	33B	&�B	hB��B�B�mB�TB�HB�BB�BB�;B�/B�)B�5B�TB�mB�B�B��B��B��B��B��B��B	B	B	B	DB	uB	hB	bB	{B	�B	!�B	!�B	�B	�B	#�B	'�B	(�B	'�B	+B	/B	/B	/B	0!B	,B	/B	49B	=qB	<jB	<jB	E�B	I�B	I�B	L�B	N�B	N�B	N�B	N�B	N�B	N�B	M�B	N�B	VB	cTB	e`B	e`B	dZB	bNB	^5B	\)B	\)B	`BB	^5B	\)B	ZB	ZB	ZB	]/B	n�B	q�B	hsB	\)B	ZB	q�B	q�B	|�B	|�B	x�B	q�B	o�B	p�B	t�B	u�B	� B	�JB	��B	��B	�'B	�XB	�XB	�XB	�qB	�^B	�jB	�qB	��B	��B	�}B	�wB	�jB	�qB	�}B	B	B	��B	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�#B	�#B	�#B	�#B	�B	�B	�B	�B	�/B	�/B	�)B	�)B	�)B	�)B	�5B	�BB	�HB	�HB	�BB	�BB	�;B	�;B	�BB	�NB	�NB	�NB	�ZB	�TB	�NB	�TB	�TB	�TB	�`B	�ZB	�`B	�fB	�mB	�mB	�mB	�sB	�sB	�sB	�sB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
+B
+B
+B
%B
+B
1B
	7B
	7B
	7B
	7B
	7B
	7B
	7B

=B

=B
DB

=B
DB
JB
JB
JB
JB
JB
JB
PB
PB
VB
VB
VB
VB
\B
\B
\B
\B
\B
\B
bB
\B
\B
hB
hB
hB
oB
uB
uB
uB
uB
uB
uB
uB
uB
oB
oB
oB
oB
uB
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
!�B
"�B
"�B
"�B
"�B
#�B
#�B
#�B
#�B
$�B
$�B
#�B
#�B
#�B
!�B
 �B
 �B
"�B
"�B
"�B
#�B
#�B
#�B
#�B
#�B
#�B
$�B
$�B
$�B
$�B
@�B
'�B
.B
49B
<jB
B�B
C�B
J�B
N�B
Q�B
VB
^5B
`BB
ffB
j11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111 Bk`Bj]Bj^BhPBhNBfABfABfBBfCBfBBfBBfEBfCBfCBhOBiWBfDBdOBc5Bc3Bb-Bb-Bb*B` Ba%Bb0B_B_Bc2BhMBkaB��B	��B	�4B
t�B
�B
κB
��BB7)B;@B=OBN�BP�BY�BhOBs�Bs�Bw�B|�Bf>B_Bc.Bx�BlfBDtB1B�B
��B
�<B
�B
�,B
�B
�wB
��B
��B
нB
�9B
��B
��B
�aB
�B
_B
8/B	��B	��B	��B	�jB	�B	��B	�B	p�B	[B	D~B	:AB	3B	&�B	KB��B��B�PB�7B�*B�$B�'B� B�B�B�B�7B�OB�lB��B��B��B��B��B��B��B	�B	 B	�B	#B	RB	GB	AB	XB	�B	!�B	!�B	�B	cB	#�B	'�B	(�B	'�B	*�B	.�B	.�B	.�B	/�B	+�B	.�B	4B	=LB	<FB	<DB	E|B	I�B	I�B	L�B	N�B	N�B	N�B	N�B	N�B	N�B	M�B	N�B	U�B	c-B	e6B	e:B	d3B	b&B	^B	[�B	\B	`B	^B	\B	Y�B	Y�B	Y�B	]B	nnB	q�B	hKB	\B	Y�B	qB	q�B	|�B	|�B	x�B	q�B	owB	pyB	t�B	u�B	�B	�B	�oB	��B	��B	�,B	�.B	�+B	�DB	�3B	�>B	�DB	�\B	�VB	�QB	�KB	�?B	�CB	�PB	�bB	�`B	�]B	ǂB	ɍB	ʖB	˗B	̠B	ΪB	зB	��B	��B	��B	��B	��B	��B	��B	ϰB	˚B	˘B	ͤB	ѿB	еB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	��B	��B	��B	�	B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�.B	�%B	� B	�&B	�%B	�&B	�0B	�,B	�0B	�6B	�?B	�>B	�?B	�CB	�CB	�DB	�FB	�KB	�HB	�JB	�OB	�VB	�WB	�OB	�UB	�YB	�[B	�[B	�\B	�cB	�iB	�aB	�bB	�\B	�aB	�aB	�cB	�hB	�hB	�nB	�tB	�vB	�tB	�tB	�uB	�yB	�B	�B	�~B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
	B
	B
	B
	B
	B
	B
	B

B

B
B

B
B
B
B
B
B
B
B
B
B
#B
"B
#B
%B
*B
+B
,B
*B
+B
,B
0B
+B
,B
5B
2B
6B
>B
DB
DB
DB
DB
DB
CB
CB
FB
>B
=B
;B
<B
DB
FB
HB
PB
NB
OB
OB
UB
OB
XB
TB
UB
TB
WB
VB
UB
\B
bB
nB
tB
sB
yB
yB
�B
�B
�B
�B
�B
�B
�B
�B
�B
!�B
"�B
"�B
"�B
"�B
#�B
#�B
#�B
#�B
$�B
$�B
#�B
#�B
#�B
!�B
 �B
 �B
"�B
"�B
"�B
#�B
#�B
#�B
#�B
#�B
#�B
$�B
$�B
$�B
$�G�O�B
'�B
-�B
4	B
<5B
BZB
CeB
J�B
N�B
Q�B
U�B
^ B
`B
f3B
jJ11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.43 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071417352016080714173520160807141735  AO  ARCAADJP                                                                    20150325091636    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150325091636  QCP$                G�O�G�O�G�O�DFB5E           AO  ARGQQCPL                                                                    20150325091636  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807141735  IP                  G�O�G�O�G�O�                