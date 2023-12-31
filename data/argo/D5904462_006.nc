CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-02-26T22:14:04Z AOML 3.0 creation; 2016-08-07T21:51:10Z UW 3.1 conversion     
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
_FillValue                    gx   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ix   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  qp   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    yh   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {h   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �`   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �`   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �X   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �(   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �,   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �0   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20150226221404  20160807145110  5904462 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               A   AO  5287_9017_006                   2C  D   APEX                            6529                            072314                          846 @�F-RP1   @�F�Q��@2Ƨ�c�����1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   B   B   @�  @�  A   A   A>ffA`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B���B�  B�  B�  B�  B�  B�  B�  B�33B���B���B�  B���B�  B�  C   C  C  C  C�C
  C  C  C  C  C  C  C  C  C�C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP�CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C��C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt��Dy��D�  D�<�D���D���D��D�I�D�p D��fD���D�@ D��fD��fD�	�D�S3Dډ�D�� D���D�<�D�s3D�� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�@�A�HA&�HAEG�Af�HA�p�A�p�A�p�A�p�A�p�A�p�A�p�A�p�B�RB	�RB�RB�RB!�RB)�RB1�RB9�RBA�RBI�RBQ�RBY�RBa�RBi�RBq�RBy�RB��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B�\B�\BĨ�B��)B��)B��)B��)B��)B��)B��)B�\B��B��B��)B���B��)B��)C nCnCnCnC��C
nCnCnCnCnCnCnCnCnC��CnC nC"nC$nC&nC(nC*nC,nC.nC0nC2nC4nC6nC8nC:nC<nC>nC@nCBnCDnCFnCHnCJnCLnCNnCP��CRnCTnCVnCXnCZnC\nC^nC`nCbnCdnCfnChnCjnClnCnnCpnCrnCtnCvnCxnCznC|nC~nC�7
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
C�C�C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�C�C�7
C�*=C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
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
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Dt�RDy�D�-�D�J�D���D��]D��D�W]D�}�D��)D�]D�M�D��)D��)D�]D�`�Dڗ]D���D�
�D�J�D��D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�1'A�$�A�$�A��A��A��HAܮAܕ�A�x�A�r�A�l�A�ffA�`BA�\)A�XA�S�A�Q�A�M�A�M�A�M�A�K�A�K�A�I�A�E�A�C�A�+A�^5A�=qA��A�  A�ƨAٕ�A�~�A�=qA؝�A׸RA�\)AՇ+Aԟ�A�S�A���A�z�A�G�AґhA�p�A��yA�t�A���A��;AͼjA��TA˾wA�A�A�7LAöFA�Q�A�"�A�ƨA���A�bNA�9XA���A�\)A��/A��#A��DA��A�=qA��wA��A��RA�7LA�r�A��RA��DA���A�v�A�O�A���A�ƨA�K�A��+A��wA��yA�$�A���A��hA��DA��A��7A��mA~ffAxbAvffAu�-Au%At(�AoG�Am+Ah��Ae�^Ab��A\�\A[/AXA�ATr�AO��AL��AH�\AF1AE7LAC��AA��A@-A=��A:��A:(�A8�+A7�A6�uA533A4v�A4�+A3;dA1\)A.�HA,  A+O�A*ĜA)hsA(9XA&�A$r�A#dZA"��A!ƨA ZAȴA��AdZA�A7LAƨAO�A
=AZA��AAA�A�7AoA�RAz�AZA�A��A�\A(�A��A��AƨA��A��AffA�AG�AA	S�A~�A�TA�AK�A�A�A��Ap�AA��AZA=qAM�AffA�/Al�Ar�A�uA�A�AZA(�A�DA��A�Az�A�A\)A�A7LA (�@�v�@�&�@�9X@�33@���@�~�@�$�@��-@��@���@���@�V@�5?@�@��-@�p�@�/@�%@��`@��/@��j@�1'@�|�@�^5@�^@�X@���@�I�@���@��
@�R@�@���@�9@웦@���@�@�@��y@��@�
=@��@�ȴ@ꟾ@��@�~�@�J@�-@�7@�O�@��@�Ĝ@�|�@��T@��@���@�"�@�|�@��m@◍@�j@�j@�Q�@ߥ�@��@އ+@�5?@���@ݩ�@ݺ^@ݲ-@݁@���@�ƨ@ۅ@�l�@�S�@�o@�$�@�/@�;d@�=q@�O�@ԛ�@��
@ӝ�@�\)@ҟ�@���@Ѻ^@��@�Z@�1@��@��;@���@�ƨ@ϥ�@ϝ�@�+@�ff@���@���@�1'@˝�@ʇ+@���@ə�@�x�@�hs@�G�@�V@ȋD@�Q�@�b@��m@ǍP@�33@��@�@Ɨ�@��@��@�&�@�Ĝ@�bN@Ý�@�+@���@\@���@�G�@��@��@�Ĝ@�j@��@��@���@�@�V@��-@���@�J@��7@�z�@�\)@��H@��R@��\@�v�@�^5@���@�x�@�O�@�G�@�?}@��@��u@��F@�"�@�
=@���@���@�v�@��@�@�/@�z�@�I�@���@�l�@�o@�@��y@�v�@�@��h@��`@��@�9X@���@�V@��@���@���@��`@���@���@��9@��@��u@��@��@��@��@��@�bN@�Z@���@�@���@�n�@�-@���@���@�G�@��`@��@� �@�ƨ@�t�@�
=@��@��@���@���@�@�p�@�%@���@�j@��w@�\)@��@���@�^5@���@���@�x�@�7L@�%@��@��D@�Z@�I�@�9X@��@��w@���@�dZ@�33@��y@�=q@���@�`B@�&�@��@���@�Z@���@�K�@��@���@���@�^5@��@���@���@�1'@�ƨ@�l�@��@���@�-@��T@��-@�p�@�V@��`@��u@�A�@��@��F@���@�|�@�@���@�-@��T@�X@��@�A�@�b@���@���@�S�@�"�@��@�M�@�(�@�dZ@�@zJ@sC�@h��@`�9@V�@P��@HbN@@�`@81'@0��@,1@#�F@�h@X@{@S�@A�@O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A�1'A�$�A�$�A��A��A��HAܮAܕ�A�x�A�r�A�l�A�ffA�`BA�\)A�XA�S�A�Q�A�M�A�M�A�M�A�K�A�K�A�I�A�E�A�C�A�+A�^5A�=qA��A�  A�ƨAٕ�A�~�A�=qA؝�A׸RA�\)AՇ+Aԟ�A�S�A���A�z�A�G�AґhA�p�A��yA�t�A���A��;AͼjA��TA˾wA�A�A�7LAöFA�Q�A�"�A�ƨA���A�bNA�9XA���A�\)A��/A��#A��DA��A�=qA��wA��A��RA�7LA�r�A��RA��DA���A�v�A�O�A���A�ƨA�K�A��+A��wA��yA�$�A���A��hA��DA��A��7A��mA~ffAxbAvffAu�-Au%At(�AoG�Am+Ah��Ae�^Ab��A\�\A[/AXA�ATr�AO��AL��AH�\AF1AE7LAC��AA��A@-A=��A:��A:(�A8�+A7�A6�uA533A4v�A4�+A3;dA1\)A.�HA,  A+O�A*ĜA)hsA(9XA&�A$r�A#dZA"��A!ƨA ZAȴA��AdZA�A7LAƨAO�A
=AZA��AAA�A�7AoA�RAz�AZA�A��A�\A(�A��A��AƨA��A��AffA�AG�AA	S�A~�A�TA�AK�A�A�A��Ap�AA��AZA=qAM�AffA�/Al�Ar�A�uA�A�AZA(�A�DA��A�Az�A�A\)A�A7LA (�@�v�@�&�@�9X@�33@���@�~�@�$�@��-@��@���@���@�V@�5?@�@��-@�p�@�/@�%@��`@��/@��j@�1'@�|�@�^5@�^@�X@���@�I�@���@��
@�R@�@���@�9@웦@���@�@�@��y@��@�
=@��@�ȴ@ꟾ@��@�~�@�J@�-@�7@�O�@��@�Ĝ@�|�@��T@��@���@�"�@�|�@��m@◍@�j@�j@�Q�@ߥ�@��@އ+@�5?@���@ݩ�@ݺ^@ݲ-@݁@���@�ƨ@ۅ@�l�@�S�@�o@�$�@�/@�;d@�=q@�O�@ԛ�@��
@ӝ�@�\)@ҟ�@���@Ѻ^@��@�Z@�1@��@��;@���@�ƨ@ϥ�@ϝ�@�+@�ff@���@���@�1'@˝�@ʇ+@���@ə�@�x�@�hs@�G�@�V@ȋD@�Q�@�b@��m@ǍP@�33@��@�@Ɨ�@��@��@�&�@�Ĝ@�bN@Ý�@�+@���@\@���@�G�@��@��@�Ĝ@�j@��@��@���@�@�V@��-@���@�J@��7@�z�@�\)@��H@��R@��\@�v�@�^5@���@�x�@�O�@�G�@�?}@��@��u@��F@�"�@�
=@���@���@�v�@��@�@�/@�z�@�I�@���@�l�@�o@�@��y@�v�@�@��h@��`@��@�9X@���@�V@��@���@���@��`@���@���@��9@��@��u@��@��@��@��@��@�bN@�Z@���@�@���@�n�@�-@���@���@�G�@��`@��@� �@�ƨ@�t�@�
=@��@��@���@���@�@�p�@�%@���@�j@��w@�\)@��@���@�^5@���@���@�x�@�7L@�%@��@��D@�Z@�I�@�9X@��@��w@���@�dZ@�33@��y@�=q@���@�`B@�&�@��@���@�Z@���@�K�@��@���@���@�^5@��@���@���@�1'@�ƨ@�l�@��@���@�-@��T@��-@�p�@�V@��`@��u@�A�@��@��F@���@�|�@�@���@�-@��T@�X@��@�A�@�b@���@���@�S�@�"�@��G�O�@�(�@�dZ@�@zJ@sC�@h��@`�9@V�@P��@HbN@@�`@81'@0��@,1@#�F@�h@X@{@S�@A�@O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B!�B�B�B�BoBB
��B
�fB
�B�B�B$�B)�B'�B(�B,B+B+B/BB�BL�BS�Bu�Bl�B!�BB,BhB
�B
�fB
�fB
�BB
�`B
�B
�B
��B
��B
�jB
�?B
��B
�uB
�1B
m�B
YB
R�B
T�B
K�B
A�B
;dB
1'B
"�B
{B
bB
JB
B	��B	��B	��B	��B	��B	�B	�fB	ɺB	��B	��B	�uB	�VB	�%B	l�B	\)B	F�B	5?B	$�B	JB	B��B�B�NB�#B�B�B�/B�`B�yB�B�B�B�B�NB�/B�#B�#B�TB�fB�HB�/B�BɺBɺBȴBȴBǮB��B��B��B��BȴB��B��B��B�
B�B�B�
B�B�B�B�/B�HB�ZB�`B�fB�fB�fB�`B�TB�NB�NB�HB�5B�B��B��B��B��BȴBŢBƨBĜBBBBB��BBÖBÖBĜBƨBȴBɺB��B�
B��B	�B	,B	.B	6FB	:^B	>wB	C�B	Q�B	YB	`BB	`BB	e`B	aHB	ZB	VB	O�B	Q�B	VB	W
B	VB	VB	T�B	T�B	VB	ZB	\)B	gmB	jB	k�B	l�B	n�B	o�B	p�B	q�B	r�B	r�B	s�B	u�B	w�B	{�B	|�B	}�B	}�B	}�B	}�B	|�B	{�B	{�B	|�B	}�B	~�B	�B	�%B	�+B	�=B	�VB	�bB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�{B	�{B	��B	��B	��B	��B	�uB	��B	��B	�{B	�oB	�oB	�oB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�'B	�3B	�9B	�9B	�?B	�?B	�FB	�RB	�RB	�XB	�^B	�dB	�qB	�qB	�qB	�wB	�}B	�}B	��B	��B	��B	��B	B	ÖB	ĜB	ĜB	ĜB	ĜB	ĜB	ĜB	ĜB	ĜB	ĜB	ŢB	ŢB	ƨB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�B	�B	�B	�B	�#B	�B	�B	�B	�B	�#B	�#B	�#B	�/B	�/B	�)B	�5B	�;B	�;B	�5B	�5B	�5B	�/B	�)B	�)B	�#B	�#B	�B	�#B	�5B	�BB	�HB	�HB	�HB	�HB	�HB	�HB	�NB	�NB	�NB	�HB	�HB	�NB	�HB	�TB	�`B	�fB	�mB	�mB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
%B
%B
+B
+B
%B
+B
+B
+B
%B
bB
+B
\B
�B
�B
(�B
33B
9XB
?}B
A�B
F�B
I�B
O�B
VB
[#B
aHB
gmB
l�B
o�B
q�B
t�B
w�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B�B�B�B�B�B�B�B�B�B�B�B�B�BwByB{BtBsBsBoB{BxBwBsB{BoB_B�B�B!�B�BgBaBOB�B
��B
�GB
�BfByB$�B)�B'�B(�B+�B*�B*�B.�BBqBL�BS�Bu�BliB!�B�B+�BHB
�B
�FB
�EB
�#B
�;B
��B
��B
��B
�fB
�EB
�B
��B
�RB
�B
mqB
X�B
R�B
T�B
K�B
AeB
;DB
1B
"�B
YB
BB
*B
�B	��B	��B	��B	��B	��B	�B	�CB	ɗB	��B	�rB	�VB	�6B	�B	lkB	\B	F�B	5!B	$�B	.B	�B��B�tB�2B�B��B�B�B�BB�ZB�hB�aB�eB�gB�2B�B�B�B�3B�IB�(B�B��BɜBɚBȕBȗBǎBʤBͳBθB˧BȕB̮BιB��B��B��B��B��B��B��B��B�B�'B�9B�>B�EB�CB�BB�@B�3B�-B�,B�(B�B��B��BϿBζBζBȑBłBƇB�{B�nB�oB�oB�nB�bB�kB�uB�vB�{BƄBȐBɗBͰB��B��B	cB	+�B	-�B	6B	:5B	>PB	CpB	Q�B	X�B	`B	`B	e8B	aB	Y�B	U�B	O�B	Q�B	U�B	V�B	U�B	U�B	T�B	T�B	U�B	Y�B	[�B	gDB	jVB	k]B	laB	nnB	osB	p{B	qB	r�B	r�B	s�B	u�B	w�B	{�B	|�B	}�B	}�B	}�B	}�B	|�B	{�B	{�B	|�B	}�B	~�B	��B	��B	��B	�B	�+B	�6B	�EB	�PB	�\B	�YB	�gB	�nB	�nB	�oB	�zB	��B	��B	�tB	�]B	�PB	�OB	�UB	�mB	��B	�gB	�IB	�WB	�UB	�PB	�@B	�BB	�CB	�CB	�HB	�^B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�$B	�"B	�*B	�/B	�8B	�CB	�DB	�DB	�KB	�NB	�PB	�UB	�UB	�RB	�[B	�`B	�iB	�oB	�pB	�nB	�mB	�nB	�nB	�nB	�mB	�mB	�rB	�sB	�|B	ɊB	ͤB	ͣB	ΪB	ͤB	ͣB	ͥB	зB	ѻB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	� B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�&B	�0B	�5B	�>B	�<B	�CB	�DB	�JB	�LB	�TB	�[B	�aB	�bB	�hB	�oB	�mB	�lB	�mB	�mB	�mB	�mB	�fB	�fB	�aB	�aB	�aB	�bB	�[B	�\B	�^B	�aB	�lB	�yB	�}B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�G�O�B
�B
)B
[B
�B
(�B
3B
9#B
?HB
AVB
FuB
I�B
O�B
U�B
Z�B
aB
g8B
lWB
ogB
qvB
t�B
w�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.43 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071451102016080714511020160807145110  AO  ARCAADJP                                                                    20150226221404    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150226221404  QCP$                G�O�G�O�G�O�DFB5E           AO  ARGQQCPL                                                                    20150226221404  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807145110  IP                  G�O�G�O�G�O�                