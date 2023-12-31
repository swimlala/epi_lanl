CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-04-29T02:15:36Z AOML 3.0 creation; 2016-08-07T21:51:17Z UW 3.1 conversion     
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
_FillValue                 �  A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  K|   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  Mx   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  Uh   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ]X   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _T   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  gD   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  i@   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  q0   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  y    PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �(   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �(   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �(   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �(   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �T   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �X   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �\   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �`   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �d   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20150429021536  20160807145117  5904462 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               1A   AO  5287_9017_049                   2C  D   APEX                            6529                            072314                          846 @�L�a��1   @�L�����@1��`A��d��O�;d1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    1A   B   B   @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   BffB  B  B   B(  B0  B8  B@  BH  BP  BX  B`ffBh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�33B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C�fC  C  C  C  C�C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dts3Dys3D��D�L�D�p D�� D�fD�P D�i�D�ɚD�fD�6fD�|�D��fD�fD�L�Dڀ D�ٚD�3D�9�D�|�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�@�A�HA&�HAF�HAf�HA�p�A�p�A�p�A�p�A�p�A�p�A�p�A�p�B�RB
�B�RB�RB!�RB)�RB1�RB9�RBA�RBI�RBQ�RBY�RBb�Bi�RBq�RBy�RB��)B��)B��)B��)B��)B��)B�\B��)B���B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)C nCnCnCnCnC
nCnCnCTzCnCnCnCnC��CnCnC nC"nC$nC&nC(nC*nC,nC.nC0nC2nC4nC6nC8nC:nC<nC>nC@nCBnCDnCFnCHnCJnCLnCNnCPnCRnCTnCVnCXnCZnC\nC^nC`nCbnCdnCfnChnCjnClnCnnCpnCrnCtnCvnCxnCznC|nC~nC�7
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
C�C�C�C�C�7
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
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��DD��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Dy��D�*�D�Z�D�}�D���D�$)D�]�D�w]D��]D�)D�D)D���D��)D�$)D�Z�Dڍ�D��]D� �D�G]D�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�bA�JA�A���A���A��A��A��yA��yA��yA��mA��mA��`A��TA��`A���A԰!A�jA�=qA��A�l�Aϴ9AσA�K�A�  AΧ�A�5?A��A�^5A���A���A�oAˉ7A�AʶFA�ĜAȏ\A��mAġ�AuA��/A�ffA��9A��TA�ĜA�C�A�  A�bA�dZA���A��A�^5A�=qA�ffA���A�z�A��wA�?}A�l�A�G�A��A�A�A���A�ffA�A���A�1'A�~�A��FA�oA��-A���A�5?A��A�E�A�9XA��A�z�A�ȴA���A���A�oA��A��TA��TA��A�|�A���A���A�ffA���A�ZA�A���A���A��^A~��A}�^A|�A|~�A{"�Aw�7AuhsAsoAqhsApJAn��AmC�Al~�Ak�Ai��AeƨA`jA]�hA[&�AY�AW�^AW"�AV�AU�TATE�AS�AR�APA�AN{AL��AI�AG&�ADjAB  AA�A?7LA<^5A;�A8�yA3�hA/\)A.A�A-|�A-&�A,�\A*��A'K�A&ȴA&r�A%��A$�`A#A �A A�A�7A
=A��A�
A��A��A��A �A�Az�AO�A �A�hAhsAG�AC�Ap�A��A(�A~�AƨA?}A��AȴAffA�#A��A|�A$�A��A
�+Av�AĜA�#A�DA�A��A�A��AM�A�A=qAl�A��A�/A��AhsA ZA 1@���@��y@��T@��@�|�@��@��;@�7L@�+@���@��@�@홚@��@�hs@�%@�@�o@�?}@��@�  @�;d@�ȴ@�E�@���@�I�@�M�@�!@�\@�ȴ@�X@�C�@�ȴ@��T@���@���@�ȴ@��@�{@�O�@ܛ�@�&�@�@ڟ�@�~�@��@ف@�V@�z�@׮@�=q@�bN@�+@�&�@��m@�S�@�=q@�@́@���@� �@˶F@�
=@�V@ɩ�@�p�@���@��`@ȴ9@ȃ@�9X@�S�@�
=@���@š�@�Ĝ@�(�@Ý�@�o@�ȴ@�~�@�@��7@�`B@���@��D@�z�@� �@���@�\)@�;d@��y@���@���@���@�n�@�v�@�~�@��@�x�@�X@�/@���@�%@���@�Z@��@�  @�1@��F@�"�@��@��@�M�@���@��@��^@���@��h@�hs@�%@�z�@�(�@��@�C�@�ȴ@���@��+@�ff@�=q@��@��^@�7L@��@���@���@�9X@� �@��m@��;@���@�|�@�S�@�\)@�l�@�33@���@�n�@�-@�J@��#@�p�@��@���@�Z@� �@��;@���@�ȴ@�E�@�J@���@��^@�/@��9@�r�@�Z@� �@��w@��y@��R@�V@���@�&�@���@�j@��m@�C�@�"�@�@��!@���@��\@�^5@�5?@��#@��@�hs@�&�@�V@��j@�1@���@��@���@���@���@���@�t�@�K�@�"�@��@��R@��@��-@���@�p�@�G�@�X@�hs@�x�@�7L@��@�r�@��@�\)@�+@�o@�ȴ@�M�@���@���@�G�@��@��/@�Ĝ@�bN@���@�ƨ@�dZ@��R@�5?@���@�J@���@�O�@�&�@�X@�%@��@��@��
@�o@���@��H@��@��H@�ff@��#@��T@��@��@��@��@�bN@�9X@�9X@� �@��m@���@���@��@�K�@�o@�
=@���@�~�@�^5@�E�@�-@�{@��@�%@��`@�Ĝ@� �@��@�ƨ@���@���@�dZ@��@�@��@���@��+@�ff@�V@�=q@���@�v�@��@y�@r-@j-@a&�@Y��@P�9@H �@A��@9�^@0bN@*��@%�@@7L@��@�7@�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111A�bA�JA�A���A���A��A��A��yA��yA��yA��mA��mA��`A��TA��`A���A԰!A�jA�=qA��A�l�Aϴ9AσA�K�A�  AΧ�A�5?A��A�^5A���A���A�oAˉ7A�AʶFA�ĜAȏ\A��mAġ�AuA��/A�ffA��9A��TA�ĜA�C�A�  A�bA�dZA���A��A�^5A�=qA�ffA���A�z�A��wA�?}A�l�A�G�A��A�A�A���A�ffA�A���A�1'A�~�A��FA�oA��-A���A�5?A��A�E�A�9XA��A�z�A�ȴA���A���A�oA��A��TA��TA��A�|�A���A���A�ffA���A�ZA�A���A���A��^A~��A}�^A|�A|~�A{"�Aw�7AuhsAsoAqhsApJAn��AmC�Al~�Ak�Ai��AeƨA`jA]�hA[&�AY�AW�^AW"�AV�AU�TATE�AS�AR�APA�AN{AL��AI�AG&�ADjAB  AA�A?7LA<^5A;�A8�yA3�hA/\)A.A�A-|�A-&�A,�\A*��A'K�A&ȴA&r�A%��A$�`A#A �A A�A�7A
=A��A�
A��A��A��A �A�Az�AO�A �A�hAhsAG�AC�Ap�A��A(�A~�AƨA?}A��AȴAffA�#A��A|�A$�A��A
�+Av�AĜA�#A�DA�A��A�A��AM�A�A=qAl�A��A�/A��AhsA ZA 1@���@��y@��T@��@�|�@��@��;@�7L@�+@���@��@�@홚@��@�hs@�%@�@�o@�?}@��@�  @�;d@�ȴ@�E�@���@�I�@�M�@�!@�\@�ȴ@�X@�C�@�ȴ@��T@���@���@�ȴ@��@�{@�O�@ܛ�@�&�@�@ڟ�@�~�@��@ف@�V@�z�@׮@�=q@�bN@�+@�&�@��m@�S�@�=q@�@́@���@� �@˶F@�
=@�V@ɩ�@�p�@���@��`@ȴ9@ȃ@�9X@�S�@�
=@���@š�@�Ĝ@�(�@Ý�@�o@�ȴ@�~�@�@��7@�`B@���@��D@�z�@� �@���@�\)@�;d@��y@���@���@���@�n�@�v�@�~�@��@�x�@�X@�/@���@�%@���@�Z@��@�  @�1@��F@�"�@��@��@�M�@���@��@��^@���@��h@�hs@�%@�z�@�(�@��@�C�@�ȴ@���@��+@�ff@�=q@��@��^@�7L@��@���@���@�9X@� �@��m@��;@���@�|�@�S�@�\)@�l�@�33@���@�n�@�-@�J@��#@�p�@��@���@�Z@� �@��;@���@�ȴ@�E�@�J@���@��^@�/@��9@�r�@�Z@� �@��w@��y@��R@�V@���@�&�@���@�j@��m@�C�@�"�@�@��!@���@��\@�^5@�5?@��#@��@�hs@�&�@�V@��j@�1@���@��@���@���@���@���@�t�@�K�@�"�@��@��R@��@��-@���@�p�@�G�@�X@�hs@�x�@�7L@��@�r�@��@�\)@�+@�o@�ȴ@�M�@���@���@�G�@��@��/@�Ĝ@�bN@���@�ƨ@�dZ@��R@�5?@���@�J@���@�O�@�&�@�X@�%@��@��@��
@�o@���@��H@��@��H@�ff@��#@��T@��@��@��@��@�bN@�9X@�9X@� �@��m@���@���@��@�K�@�o@�
=@���@�~�@�^5@�E�@�-@�{@��@�%@��`@�Ĝ@� �@��@�ƨ@���@���@�dZ@��@�@��@���@��+@�ff@�VG�O�@���@�v�@��@y�@r-@j-@a&�@Y��@P�9@H �@A��@9�^@0bN@*��@%�@@7L@��@�7@�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB	�FB	�FB	�FB	�FB	�FB	�FB	�FB	�FB	�FB	�FB	�FB	�FB	�FB	�FB	�FB	�?B	�9B	�-B	�B	��B	��B	��B	��B	�'B	��B	��B
T�B
�B
�FBBO�B��BÖB�B�)B�`B�B��B�B>wB5?B\B��B��B%B��B��B�B�B�sB�sB�B��B��B��B  B�B[#BcTBaHBYBI�B;dB-B!�B�BuBJB  B�B�B��B�LB�RBÖB�B��B�XB�B�BjBe`BS�B;dB�B�B�B
��B
�B
�`B
��B
�dB
�!B
��B
�B
q�B
`BB
YB
R�B
L�B
A�B
,B
�B
DB	��B	�B	�yB	�5B	�
B	��B	�wB	��B	�B	p�B	bNB	]/B	VB	Q�B	O�B	J�B	C�B	?}B	;dB	/B	%�B	�B	oB	%B��B��B��B�B�sB�5B��B�LB�!B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�hB�hB�\B�PB�VB�uB��B��B�'BǮB�
B�NB�sB�B�B�B�fB�B��B��B��BǮB�?B�'B�dB�fB��B	B	DB	
=B		7B	%B	  B��B��B��B��B�B�B�B�yB�sB�sB�sB�sB�ZB�B�B�B�yB�B�B�sB�yB�sB�yB�`B�`B�NB�B�/B�;B�BB�NB��B	JB	�B	#�B	)�B	2-B	.B	&�B	%�B	#�B	�B	�B	$�B	2-B	=qB	@�B	B�B	<jB	E�B	I�B	N�B	S�B	T�B	T�B	T�B	VB	VB	S�B	S�B	O�B	P�B	Q�B	S�B	W
B	YB	YB	_;B	hsB	k�B	k�B	p�B	p�B	p�B	p�B	p�B	p�B	p�B	q�B	r�B	q�B	q�B	r�B	q�B	q�B	r�B	s�B	t�B	z�B	}�B	�B	�B	�B	�B	�+B	�7B	�7B	�DB	�PB	�VB	�\B	�\B	�\B	�oB	�uB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�'B	�'B	�'B	�'B	�?B	�RB	�XB	�^B	�^B	�dB	�^B	�^B	�^B	�^B	�^B	�^B	�^B	�qB	�qB	�wB	�}B	��B	B	ÖB	ÖB	ÖB	ĜB	ŢB	ƨB	ƨB	ǮB	ȴB	ȴB	ȴB	ǮB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�
B	�
B	�B	�/B	�;B	�;B	�;B	�BB	�HB	�NB	�TB	�ZB	�fB	�fB	�mB	�sB	�sB	�sB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
1B
	7B
	7B
1B
	7B
	7B

=B
	7B
	7B
	7B
1B
1B
+B
+B
1B
1B
	7B
	7B
	7B
	7B
	7B
	7B

=B

=B
	7B

=B
DB
DB
JB
hB
�B
�B
%�B
-B
2-B
6FB
>wB
E�B
J�B
Q�B
W
B
^5B
bNB
gmB
k�B
p�B
t�B
w�B
|�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111B	�0B	�2B	�.B	�.B	�-B	�.B	�-B	�/B	�2B	�2B	�-B	�.B	�.B	�.B	�/B	�)B	�$B	�B	��B	��B	��B	��B	��B	�B	̸B	��B
T�B
��B
�*B�BO�B��B�sB��B�B�=B�{B��B�B>SB5B5B��B��BB��B��B�B�eB�MB�OB�B��B��B��B��B�BZ�Bc*Ba BX�BI�B;=B,�B!�BkBNBB��B�B�vB��B� B�&B�nB��B�^B�-B��B��BjVBe8BS�B;:B�BWB^B
��B
�oB
�7B
оB
�:B
��B
��B
��B
q�B
`B
X�B
R�B
L�B
AdB
+�B
yB
!B	��B	�B	�VB	�B	��B	̪B	�UB	��B	��B	p�B	b.B	]B	U�B	Q�B	O�B	J�B	CuB	?]B	;DB	.�B	%�B	�B	OB	B��B��B��B�B�TB�B̰B�/B�B��B��B��B��B��B��B��B��B��B��B��B�B�xB�{B��B�vB�}B�}B�{B�uB�pB�^B�MB�LB�?B�4B�9B�VB�pB��B�BǌB��B�,B�SB�nB�nB�vB�CB��B��BͲBθBǋB� B�B�DB�CB��B	�B	B	
B		B	B��B��B��B��B��B�B�sB�dB�TB�NB�PB�OB�LB�8B�gB�fB�YB�WB�bB�ZB�PB�TB�MB�UB�7B�:B�*B��B�	B�B�B�)B��B	#B	�B	#�B	)�B	2B	-�B	&�B	%�B	#�B	eB	sB	$�B	2B	=HB	@[B	BfB	<@B	EzB	I�B	N�B	S�B	T�B	T�B	T�B	U�B	U�B	S�B	S�B	O�B	P�B	Q�B	S�B	V�B	X�B	X�B	_B	hFB	kZB	k[B	pwB	pxB	pxB	pxB	pyB	pyB	pyB	qB	r�B	qB	q|B	r�B	q{B	q~B	r�B	s�B	t�B	z�B	}�B	��B	��B	��B	��B	��B	�	B	�
B	�B	�$B	�(B	�.B	�.B	�/B	�?B	�FB	�AB	�QB	�XB	�YB	�mB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�"B	�*B	�0B	�0B	�6B	�0B	�0B	�/B	�1B	�.B	�/B	�/B	�AB	�BB	�IB	�PB	�[B	�aB	�iB	�gB	�hB	�lB	�sB	�zB	�wB	ǁB	ȇB	ȅB	ȅB	ǁB	�B	ȆB	ʓB	˘B	ͤB	͢B	ͣB	ΫB	еB	еB	ѺB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�B	�B	�B	�$B	�)B	�5B	�7B	�<B	�BB	�CB	�CB	�BB	�AB	�KB	�NB	�MB	�UB	�UB	�\B	�hB	�mB	�mB	�mB	�kB	�sB	�nB	�kB	�uB	�zB	�wB	�~B	�wB	�zB	�xB	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 B
	B
	B
B
	B
	B

B
	B
	B
	B
B
 B
�B
�B
B
 B
	B
	B
	B
	B
	B
	B


B

B
	B

B
B
G�O�B
5B
iB
�B
%�B
,�B
1�B
6B
>DB
EoB
J�B
Q�B
V�B
^B
bB
g6B
kQB
pqB
t�B
w�B
|�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.43 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071451172016080714511720160807145117  AO  ARCAADJP                                                                    20150429021536    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150429021536  QCP$                G�O�G�O�G�O�0               AO  ARGQQCPL                                                                    20150429021536  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807145117  IP                  G�O�G�O�G�O�                