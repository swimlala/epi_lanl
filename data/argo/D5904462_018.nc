CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-02-26T22:14:19Z AOML 3.0 creation; 2016-08-07T21:51:12Z UW 3.1 conversion     
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
_FillValue                    A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    K�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  U�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ]�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    g�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  i�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  q�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    y�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �|   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �(   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �8   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �<   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �L   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �P   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �T   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �XArgo profile    3.1 1.2 19500101000000  20150226221419  20160807145112  5904462 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               A   AO  5287_9017_018                   2C  D   APEX                            6529                            072314                          846 @�#�˩�	1   @�#�ff?�@2��\(���d A�7K�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   B   B   @@  @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B̙�B���B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ�C\�C^�C`  Cb  Cd  Cf  Ch  Cj  Ck�fCn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR�fDS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�fDy��D�3D�L�D�l�D���D�fD�<�D�� D���D���D�0 D��fD�ٚD�fD�C3Dڌ�D�� D��fD�33D�|�D��f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @[�@�@�A�HA&�HAF�HAf�HA�p�A�p�A�p�A�p�A�p�A�p�A�p�A�p�B�RB	�RB�RB�RB!�RB)�RB1�RB9�RBA�RBI�RBQ�RBY�RBa�RBi�RBq�RBy�RB��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B�\B��)B��)B��)B��)B��)B��)B��)B�u�BШ�B�\B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)C nCnCnCnCnC
nCnCnCnCnCnCnCnCnCnCnC nC"nC$nC&nC(nC*nC,nC.nC0nC2nC4nC6nC8nC:nC<nC>nC@nCBnCDnCFnCHnCJnCLnCNnCPnCRnCTnCVnCXnCZ��C\��C^��C`nCbnCdnCfnChnCjnClTzCnnCpnCrnCtnCvnCxnCznC|nC~nC�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�C�C�7
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
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Du�Dy�D��D�Z�D�z�D��D�)D�J�D���D��]D���D�=�D��)D��]D�)D�P�Dښ�D���D�)D�@�D�D��)1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�%A�A�A�%A�%A�A�  A�%A�
=A�
=A�JA�JA�JA�A�  A���A���A��A��A��A��A��#A�AؾwAضFAء�A؟�A؝�A؛�A؛�A؛�Aؗ�Aؕ�AؑhA؍PA؉7A؉7A؇+A؅A�~�A�~�A؁A�+A�bNA���A�?}A�v�A�ZA�ffAѓuA�(�A�  AȼjAÁA�bNA��A�C�A��A�Q�A���A�jA���A��DA��^A�A�l�A�E�A��A�9XA�t�A�x�A��;A���A��-A�7LA�&�A���A��A�&�A��FA��A�oA��#A��!A�jA��A��A���A�S�A��A���A�"�A�oA��wA��7A��uA���A���A��mA���A���A�v�A���A��RA�$�A���A�"�A��yA}�mAwC�AuK�Ao��Ah��Ad�DAb��A\��A[+AX�AR�`AO��AN��AM\)AJ��AF��ADȴAB�!AA/A@�9A@�A=33A<��A<9XA;�
A:�uA8�\A7S�A3%A/��A-/A,�DA+�A+�-A)�-A&��A%l�A%oA ��AĜA�A$�AA�AS�A��A��AffA��AS�A�A�A�PAjA�A�7A&�A+AȴA�A{AƨA��A�A(�A��A�A��A�A��A5?A�Ax�A\)A��A��AffA �A��A�hA
=A
  A	�A	��A	G�Az�A�PAVA��A1A�^A7LAQ�AbA��A��A��A�A��AjA��A%A �/A z�A (�@���@���@��@��@�%@���@�Z@�b@��
@��@�r�@��y@�~�@�-@��@�7@�&�@��`@��D@�F@�x�@�/@�9@�ƨ@��@�R@�=q@�G�@�u@�j@��;@�@�+@��H@�~�@�~�@�M�@旍@��;@�C�@�V@��@�G�@��#@�@��@�/@� �@�
=@�v�@�^5@��@ᙚ@�O�@�`B@�?}@�1@ߕ�@ߍP@߅@��H@�@�hs@���@�-@�ff@���@�G�@ܼj@ۮ@�o@�~�@�E�@��@٩�@��`@��/@ؼj@�Z@ץ�@��@�$�@�V@ԓu@�z�@ӝ�@Ӆ@�33@ҏ\@�5?@љ�@���@�j@Ͼw@�l�@Χ�@���@�@�hs@̬@�b@˕�@�"�@ʟ�@�V@�^5@��@�hs@�V@��`@���@Ȭ@�r�@��m@Ǯ@ǥ�@�l�@Ƈ+@��@�@��@ź^@�p�@�O�@��/@�r�@�I�@�  @Õ�@�K�@�33@�@�n�@��@�?}@��@��@���@�;d@��+@�ff@�E�@�{@��-@�%@���@��@�b@��@�dZ@��y@�ff@��-@�x�@�G�@��@���@�Ĝ@���@��@��y@��\@�V@�M�@�@��@�`B@�7L@��@��9@�bN@���@�;d@���@��@���@���@�j@�I�@�  @���@�dZ@�l�@�dZ@���@��@���@��u@���@��F@�l�@�+@�
=@��H@�J@�?}@���@�Ĝ@��u@�j@�I�@��w@��@���@�V@�{@���@�`B@�%@�z�@�(�@��@���@��@���@�33@�@���@��\@�=q@��-@�7L@���@��9@�Z@� �@���@��F@�S�@�o@���@�M�@��T@��-@�V@��9@�Z@��
@�C�@��H@��!@���@���@��\@�~�@�^5@��#@���@�hs@�G�@�7L@�V@���@��@��@�33@�
=@���@�v�@�V@���@�@�hs@��`@��9@��9@���@�bN@���@���@�|�@�
=@���@��R@���@���@���@��@��@��-@���@{@so@j�@a�^@Xb@QX@K�F@C�m@<Z@3��@-�T@)x�@$�/@   @I�@�u@�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111 A�%A�A�A�%A�%A�A�  A�%A�
=A�
=A�JA�JA�JA�A�  A���A���A��A��A��A��A��#A�AؾwAضFAء�A؟�A؝�A؛�A؛�A؛�Aؗ�Aؕ�AؑhA؍PA؉7A؉7A؇+A؅A�~�A�~�A؁A�+A�bNA���A�?}A�v�A�ZA�ffAѓuA�(�A�  AȼjAÁA�bNA��A�C�A��A�Q�A���A�jA���A��DA��^A�A�l�A�E�A��A�9XA�t�A�x�A��;A���A��-A�7LA�&�A���A��A�&�A��FA��A�oA��#A��!A�jA��A��A���A�S�A��A���A�"�A�oA��wA��7A��uA���A���A��mA���A���A�v�A���A��RA�$�A���A�"�A��yA}�mAwC�AuK�Ao��Ah��Ad�DAb��A\��A[+AX�AR�`AO��AN��AM\)AJ��AF��ADȴAB�!AA/A@�9A@�A=33A<��A<9XA;�
A:�uA8�\A7S�A3%A/��A-/A,�DA+�A+�-A)�-A&��A%l�A%oA ��AĜA�A$�AA�AS�A��A��AffA��AS�A�A�A�PAjA�A�7A&�A+AȴA�A{AƨA��A�A(�A��A�A��A�A��A5?A�Ax�A\)A��A��AffA �A��A�hA
=A
  A	�A	��A	G�Az�A�PAVA��A1A�^A7LAQ�AbA��A��A��A�A��AjA��A%A �/A z�A (�@���@���@��@��@�%@���@�Z@�b@��
@��@�r�@��y@�~�@�-@��@�7@�&�@��`@��D@�F@�x�@�/@�9@�ƨ@��@�R@�=q@�G�@�u@�j@��;@�@�+@��H@�~�@�~�@�M�@旍@��;@�C�@�V@��@�G�@��#@�@��@�/@� �@�
=@�v�@�^5@��@ᙚ@�O�@�`B@�?}@�1@ߕ�@ߍP@߅@��H@�@�hs@���@�-@�ff@���@�G�@ܼj@ۮ@�o@�~�@�E�@��@٩�@��`@��/@ؼj@�Z@ץ�@��@�$�@�V@ԓu@�z�@ӝ�@Ӆ@�33@ҏ\@�5?@љ�@���@�j@Ͼw@�l�@Χ�@���@�@�hs@̬@�b@˕�@�"�@ʟ�@�V@�^5@��@�hs@�V@��`@���@Ȭ@�r�@��m@Ǯ@ǥ�@�l�@Ƈ+@��@�@��@ź^@�p�@�O�@��/@�r�@�I�@�  @Õ�@�K�@�33@�@�n�@��@�?}@��@��@���@�;d@��+@�ff@�E�@�{@��-@�%@���@��@�b@��@�dZ@��y@�ff@��-@�x�@�G�@��@���@�Ĝ@���@��@��y@��\@�V@�M�@�@��@�`B@�7L@��@��9@�bN@���@�;d@���@��@���@���@�j@�I�@�  @���@�dZ@�l�@�dZ@���@��@���@��u@���@��F@�l�@�+@�
=@��H@�J@�?}@���@�Ĝ@��u@�j@�I�@��w@��@���@�V@�{@���@�`B@�%@�z�@�(�@��@���@��@���@�33@�@���@��\@�=q@��-@�7L@���@��9@�Z@� �@���@��F@�S�@�o@���@�M�@��T@��-@�V@��9@�Z@��
@�C�@��H@��!@���@���@��\@�~�@�^5@��#@���@�hs@�G�@�7L@�V@���@��@��@�33@�
=@���@�v�@�V@���@�@�hs@��`@��9@��9@���@�bN@���@���@�|�@�
=@���@��R@���@���G�O�@��@��@��-@���@{@so@j�@a�^@Xb@QX@K�F@C�m@<Z@3��@-�T@)x�@$�/@   @I�@�u@�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBiyBiyBiyBiyBiyBiyBiyBiyBiyBiyBiyBiyBiyBiyBhsBhsBhsBhsBgmBgmBgmBffBe`Be`BffBbNBbNBbNBaHBaHBbNBaHBaHBaHBaHB`BB`BB`BB`BB`BBaHBgmBz�B��B��B��B��B��B��B��B�BiyBp�B��BJB�B-B?}BA�BR�B�1B�B�B�%B�JB�=B�1B�7B~�Bw�Bl�Bo�B� B�7B�PB�oB��B�Bk�BVBJ�BZBQ�B+B	7B�#BÖB��B�B��B�B�+BffB2-B
��B
��B
�}B
��B
��B
�hB
�DB
z�B
w�B
�B
s�B
n�B
e`B
O�B
{B	�B	��B	�JB	VB	N�B	N�B	 �B	{B	B��B�B�B�sB�fB�fB�NB�TB�HB�BB�5B�5B�)B�#B�B�B�5B�)B�#BɺBB��B�
B��B��BƨBǮB��B�LB�!B�FB�B�5B�ZB�B��B��B��B��B��B	  B	DB	VB	uB	bB	\B	PB	\B	hB	hB	hB	hB	\B	VB	PB	uB	�B	�B	�B	�B	�B	�B	�B	�B	 �B	"�B	%�B	)�B	/B	6FB	7LB	2-B	:^B	>wB	<jB	9XB	8RB	6FB	8RB	8RB	6FB	6FB	49B	5?B	;dB	=qB	@�B	?}B	A�B	I�B	L�B	N�B	O�B	Q�B	Q�B	R�B	T�B	VB	YB	[#B	[#B	[#B	[#B	ZB	ZB	_;B	cTB	dZB	e`B	e`B	ffB	gmB	hsB	hsB	iyB	m�B	n�B	o�B	r�B	s�B	s�B	v�B	{�B	� B	�B	�B	�B	�B	�B	�%B	�=B	�PB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�3B	�RB	�RB	�LB	�FB	�9B	�?B	�XB	�dB	�qB	��B	B	ÖB	ÖB	ĜB	ÖB	B	B	��B	ÖB	ŢB	ŢB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�B	�B	�#B	�)B	�)B	�)B	�)B	�/B	�/B	�/B	�/B	�)B	�B	�B	�5B	�5B	�;B	�;B	�;B	�;B	�BB	�BB	�;B	�HB	�TB	�TB	�TB	�ZB	�TB	�NB	�HB	�HB	�HB	�NB	�NB	�NB	�TB	�TB	�TB	�TB	�TB	�NB	�NB	�NB	�NB	�TB	�ZB	�`B	�`B	�`B	�`B	�ZB	�ZB	�TB	�TB	�TB	�TB	�ZB	�fB	�`B	�`B	�`B	�`B	�`B	�`B	�`B	�`B	�`B	�`B	�`B	�fB	�fB	�fB	�mB	�mB	�sB	�mB	�sB	�B	�yB	�sB	�sB	�sB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
  B
B
B
B
B
B
B
B
%B
	7B
	7B
bB
hB
�B
�B
$�B
-B
49B
;dB
?}B
E�B
I�B
M�B
W
B
\)B
`BB
cTB
gmB
jB
o�B
s�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111 BiYBiTBiTBiVBiVBiTBiTBiTBiVBiVBiYBiTBiYBiYBhTBhSBhSBhSBgMBgMBgMBfFBe@Be@BfDBb,Bb0Bb-Ba)Ba'Bb,Ba&Ba&Ba&Ba'B`#B`!B`#B`#B`#Ba#BgNBz�B��B��B��B��B��B��B��B��BiUBp�B��B%B�B,�B?\BAeBR�B�B��B��B�B�'B�B�B�B~�Bw�BldBo{B�B�B�+B�HB�tB��BkaBU�BJ�BY�BQ�B*�B	B��B�nB��B�WB�cB��B��Bf>B2B
��B
ͬB
�TB
��B
�fB
�EB
�B
z�B
w�B
��B
s�B
nqB
e=B
O�B
UB	��B	�hB	�*B	U�B	N�B	N�B	 �B	]B	 B��B�B�lB�XB�JB�HB�2B�8B�)B�!B�B�B�B�B�B� B�B�B�BɞB�qB��B��B��BκBƈBǍBκB�-B�B�(B��B�B�:B�}B��B��B��B��B��B��B	B	3B	OB	>B	8B	,B	7B	DB	EB	FB	EB	7B	1B	,B	QB	{B	�B	�B	oB	{B	�B	B	�B	 �B	"�B	%�B	)�B	.�B	6B	7(B	2B	:6B	>QB	<DB	92B	8,B	6B	8*B	8+B	6B	6B	4B	5B	;<B	=KB	@]B	?XB	AbB	I�B	L�B	N�B	O�B	Q�B	Q�B	R�B	T�B	U�B	X�B	Z�B	Z�B	Z�B	Z�B	Y�B	Y�B	_B	c*B	d3B	e7B	e6B	f<B	gEB	hLB	hJB	iNB	mgB	nnB	otB	r�B	s�B	s�B	v�B	{�B	�B	��B	��B	��B	��B	��B	��B	�B	�&B	�PB	��B	��B	��B	�gB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�%B	�%B	�B	�B	�
B	�B	�(B	�7B	�BB	�WB	�eB	�iB	�iB	�lB	�hB	�bB	�aB	�\B	�gB	�tB	�uB	ǁB	ɍB	̞B	̟B	ΫB	ϯB	ϲB	ѽB	ѾB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�
B	�B	�B	�B	�B	�"B	�%B	�&B	�*B	�'B	�B	�B	�B	�B	�B	�B	�B	�&B	�%B	�$B	�#B	�&B	�B	�B	�B	�B	�$B	�*B	�0B	�/B	�0B	�1B	�+B	�*B	�#B	�!B	�%B	�#B	�*B	�7B	�1B	�/B	�/B	�.B	�1B	�/B	�/B	�1B	�/B	�0B	�1B	�5B	�6B	�7B	�<B	�>B	�BB	�<B	�CB	�OB	�JB	�EB	�BB	�DB	�IB	�HB	�IB	�PB	�NB	�OB	�[B	�aB	�bB	�aB	�`B	�_B	�aB	�cB	�fB	�iB	�hB	�oB	�lB	�mB	�mB	�mB	�oB	�sB	�sB	�tB	�tB	�tB	�sB	�{B	�zB	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
 �B
�B
�B
�B
�B
 �B
 �B
 �B
 �B	��B
 �B
 �B
�B
�B
�B
�B
�B
�G�O�B
	B
1B
3B
[B
�B
$�B
,�B
4B
;0B
?JB
EpB
I�B
M�B
V�B
[�B
`B
c B
g;B
jLB
olB
s�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.43 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071451122016080714511220160807145112  AO  ARCAADJP                                                                    20150226221419    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150226221419  QCP$                G�O�G�O�G�O�DFB5E           AO  ARGQQCPL                                                                    20150226221419  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807145112  IP                  G�O�G�O�G�O�                