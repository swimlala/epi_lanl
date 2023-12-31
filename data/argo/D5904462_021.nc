CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-02-26T22:14:22Z AOML 3.0 creation; 2016-08-07T21:51:12Z UW 3.1 conversion     
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
resolution        :�o     �  U|   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ]p   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _p   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    gd   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  id   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  qX   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    yL   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {L   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �@   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �@   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �4   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �d   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �d   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �d   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �d   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20150226221422  20160807145112  5904462 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               A   AO  5287_9017_021                   2C  D   APEX                            6529                            072314                          846 @�'ؽ��1   @�'�hK�@2G�z�H�d(�\1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   B   B   @�  @���A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   BffB  B  B��B(  B0  B8  B@  BG��BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B���B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  C   C�C�C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D'��D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D/��D0y�D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� DgfDg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dts3DyY�D�fD�P D���D���D� D�6fD�� D��3D��D�I�D�� D�ٚD� D�P D�l�D��3D��fD�6fD�s3D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�@ʏ\A�HA&�HAF�HAf�HA�p�A�p�A�p�A�p�A�p�A�p�A�p�A�p�B�RB
�B�RB�RB!Q�B)�RB1�RB9�RBA�RBIQ�BQ�RBY�RBa�RBi�RBq�RBy�RB��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B�\BШ�B��)B��)B��)B��)B��B��)B��)B��)B��)B��)B��)C nC��C��CnCnC
nCnCnCnCnCnCnCnCnCnCnC nC"nC$nC&nC(nC*nC,nC.nC0nC2nC4nC6nC8nC:nC<nC>nC@nCBnCDnCFnCHnCJnCLnCNnCPnCRnCTnCVnCXnCZnC\nC^nC`nCbnCdnCfnChnCjnClnCnnCpnCrnCtnCvnCxnCznC|nC~nC�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
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
D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0D0�D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg!�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��DyuD�$)D�]�D��]D���D��D�D)D���D���D�*�D�W]D���D��]D��D�]�D�z�D���D�)D�D)D��D��]11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�"�A�(�A�33A�/A�7LA�A�A�?}A�?}A�E�A�9XA�A�A�I�A�Q�A�VA�XA�O�A�O�A�S�A�S�A�S�A�M�A�K�A�S�A�Q�A�S�A�\)A�`BA�`BA�\)A�\)A�\)A�^5A�^5A�`BA�`BA�bNA�bNA�bNA�dZA�dZA�ffA�ffA�hsA�hsA�bNA�`BA�\)A�^5A�`BA�O�A���A��yA��yA��yA�Q�A�p�A¾wA�9XA��jA�JA�A�+A�
=A��yA���A���A��7A�A�A�G�A���A�%A�|�A�/A��A���A��#A���A�M�A���A�x�A��DA�M�A��wA��A��uA���A�ffA�VA���A��yA��uA��A�r�A�E�A���A���A�Q�A��A�A�A���A��A��A���A��A�z�A�&�A���A�p�A�ffA�/A�;dA���A�Az��At��Ap1AlbNAk�Ae�PAa�A`��A_/A\�uAZ�+AW�hAU�FAT��AT�AR��AO|�AM�AKO�AIl�AG�AF9XADz�AC��ACl�AB�A@��A?A:bA5XA4�+A/XA-�
A,�/A*�\A(��A'�TA%�A$(�A#�A"�\A"A�A!�hA ��A 1At�AA�A�7Al�A?}AJA|�A^5A�;A�^A��A�^A�An�AZA�^A33AbNAA��A1'A\)A�A%A�A��AffAbA|�A
�/A
�A	
=A�!Ar�A=qA��A�HAz�A�A��A|�A`BA�!AZAJA�A
=A�`A��AjAZA$�A  A��A =q@�l�@���@��@��9@�  @�o@��@��9@�C�@���@�~�@�@��9@�ƨ@�C�@�$�@���@�O�@�9@�b@�w@�C�@@�$�@��@���@��@�r�@���@�w@�@��H@ꗍ@陚@�G�@�j@�r�@�Z@�b@�!@�1'@�E�@��@���@��m@ߝ�@�;d@�"�@�o@��@���@�~�@ݑh@��`@��
@��@��m@�ƨ@ۥ�@��@�M�@�E�@�E�@�M�@��T@١�@��@ؓu@�bN@��@��@ם�@׮@�\)@���@��T@պ^@ՙ�@թ�@�hs@�&�@���@���@���@�1'@ӥ�@�t�@�l�@�l�@�o@�V@Ѻ^@�p�@�7L@Гu@�Z@� �@���@�t�@ΰ!@��T@��#@�@ͩ�@́@�O�@���@̴9@̛�@̃@�bN@�b@˾w@˥�@ˍP@�v�@�{@ɺ^@�hs@��@���@�1'@�  @���@���@���@���@ǅ@�dZ@�o@Ɨ�@�{@š�@�G�@ě�@þw@�;d@���@�ff@�E�@�5?@��^@�7L@�b@�S�@�;d@��H@�ff@��h@���@�r�@��@��@�"�@�@�=q@�@��/@��u@���@�"�@��+@�@�`B@���@��@���@�S�@�
=@���@�v�@�=q@���@��@��@���@�x�@�?}@�V@���@��@���@��D@�Q�@��m@�K�@��y@�^5@�E�@���@�?}@��9@�1@��@�\)@�o@��@���@�{@���@�&�@��@���@��@��D@�r�@�I�@��;@���@�t�@��@��!@��\@��+@��+@�n�@�J@�&�@���@��u@�bN@�I�@��@���@���@��@�33@�
=@��@��@��@���@��@���@���@�5?@��-@�G�@���@��u@�I�@��@�K�@�33@��@�@�ȴ@�ff@��T@�?}@�/@�%@��/@� �@��P@�\)@�C�@���@�~�@��@�@�@���@�p�@�O�@��@���@��D@�9X@�  @���@�S�@�S�@�33@�r�@��w@�E�@�Z@}@r~�@iX@_��@Vȴ@K��@BJ@;33@5p�@-�T@'�@"��@��@�@ff@��@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   A�"�A�(�A�33A�/A�7LA�A�A�?}A�?}A�E�A�9XA�A�A�I�A�Q�A�VA�XA�O�A�O�A�S�A�S�A�S�A�M�A�K�A�S�A�Q�A�S�A�\)A�`BA�`BA�\)A�\)A�\)A�^5A�^5A�`BA�`BA�bNA�bNA�bNA�dZA�dZA�ffA�ffA�hsA�hsA�bNA�`BA�\)A�^5A�`BA�O�A���A��yA��yA��yA�Q�A�p�A¾wA�9XA��jA�JA�A�+A�
=A��yA���A���A��7A�A�A�G�A���A�%A�|�A�/A��A���A��#A���A�M�A���A�x�A��DA�M�A��wA��A��uA���A�ffA�VA���A��yA��uA��A�r�A�E�A���A���A�Q�A��A�A�A���A��A��A���A��A�z�A�&�A���A�p�A�ffA�/A�;dA���A�Az��At��Ap1AlbNAk�Ae�PAa�A`��A_/A\�uAZ�+AW�hAU�FAT��AT�AR��AO|�AM�AKO�AIl�AG�AF9XADz�AC��ACl�AB�A@��A?A:bA5XA4�+A/XA-�
A,�/A*�\A(��A'�TA%�A$(�A#�A"�\A"A�A!�hA ��A 1At�AA�A�7Al�A?}AJA|�A^5A�;A�^A��A�^A�An�AZA�^A33AbNAA��A1'A\)A�A%A�A��AffAbA|�A
�/A
�A	
=A�!Ar�A=qA��A�HAz�A�A��A|�A`BA�!AZAJA�A
=A�`A��AjAZA$�A  A��A =q@�l�@���@��@��9@�  @�o@��@��9@�C�@���@�~�@�@��9@�ƨ@�C�@�$�@���@�O�@�9@�b@�w@�C�@@�$�@��@���@��@�r�@���@�w@�@��H@ꗍ@陚@�G�@�j@�r�@�Z@�b@�!@�1'@�E�@��@���@��m@ߝ�@�;d@�"�@�o@��@���@�~�@ݑh@��`@��
@��@��m@�ƨ@ۥ�@��@�M�@�E�@�E�@�M�@��T@١�@��@ؓu@�bN@��@��@ם�@׮@�\)@���@��T@պ^@ՙ�@թ�@�hs@�&�@���@���@���@�1'@ӥ�@�t�@�l�@�l�@�o@�V@Ѻ^@�p�@�7L@Гu@�Z@� �@���@�t�@ΰ!@��T@��#@�@ͩ�@́@�O�@���@̴9@̛�@̃@�bN@�b@˾w@˥�@ˍP@�v�@�{@ɺ^@�hs@��@���@�1'@�  @���@���@���@���@ǅ@�dZ@�o@Ɨ�@�{@š�@�G�@ě�@þw@�;d@���@�ff@�E�@�5?@��^@�7L@�b@�S�@�;d@��H@�ff@��h@���@�r�@��@��@�"�@�@�=q@�@��/@��u@���@�"�@��+@�@�`B@���@��@���@�S�@�
=@���@�v�@�=q@���@��@��@���@�x�@�?}@�V@���@��@���@��D@�Q�@��m@�K�@��y@�^5@�E�@���@�?}@��9@�1@��@�\)@�o@��@���@�{@���@�&�@��@���@��@��D@�r�@�I�@��;@���@�t�@��@��!@��\@��+@��+@�n�@�J@�&�@���@��u@�bN@�I�@��@���@���@��@�33@�
=@��@��@��@���@��@���@���@�5?@��-@�G�@���@��u@�I�@��@�K�@�33@��@�@�ȴ@�ff@��T@�?}@�/@�%@��/@� �@��P@�\)@�C�@���@�~�@��@�@�@���@�p�@�O�@��@���@��D@�9X@�  @���@�S�@�S�G�O�@�r�@��w@�E�@�Z@}@r~�@iX@_��@Vȴ@K��@BJ@;33@5p�@-�T@'�@"��@��@�@ff@��@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�RB�LB�LB�RB�LB�LB�LB�LB�LB�LB�LB�LB�LB�LB�LB�LB�LB�LB�LB�LB�LB�LB�LB�LB�LB�LB�LB�LB�LB�LB�LB�LB�?B�LB�LB�LB�LB�LB�LB�LB�LB�LB�LB�LB�LB�RB�RB�RB�RB�XB�XB��BhB-B1'B/B33B@�BH�BN�B^5BbNBcTBe`BdZBcTBbNBbNBdZBjBu�B|�B�7B��B�B�B�\Bz�BffBN�BJ�BQ�B^5BcTB\)BM�B>wB5?B,B�BDB��B�B�)BȴB�dB��B�1Bs�BVB;dB(�B�BVB
��B
��B
��B
�+B
gmB
Q�B
A�B
7LB
�B	��B	ɺB	��B	�\B	�B	gmB	VB	N�B	F�B	9XB	/B	$�B	�B	�B	�B	{B	1B	  B��B�B�B�sB�ZB�HB�BB�)B�
B��B��B��BǮBɺBƨBB�}B�dB�RB�3B�B��B��B��B��B��B��B��B��B��B��B��B�{B��B��B��B��B��B�B�-BƨB�#B�B��B��B��B��B��B	B	%B	JB	VB	VB	\B	bB	\B	JB		7B	+B	uB	%�B	.B	8RB	8RB	9XB	?}B	B�B	B�B	K�B	[#B	]/B	\)B	]/B	]/B	]/B	]/B	]/B	\)B	\)B	[#B	[#B	ZB	YB	W
B	VB	VB	T�B	T�B	T�B	T�B	VB	YB	ZB	YB	YB	YB	YB	YB	ZB	[#B	]/B	^5B	aHB	e`B	jB	n�B	n�B	m�B	l�B	l�B	o�B	w�B	�B	�B	�%B	�7B	�1B	�VB	�bB	�oB	�uB	�oB	�\B	�bB	�{B	��B	��B	��B	�B	�B	�B	�B	�B	�'B	�'B	�!B	�'B	�-B	�?B	�LB	�RB	�XB	�jB	�dB	�dB	�dB	�dB	�qB	�qB	��B	ŢB	ƨB	ǮB	ȴB	ǮB	ɺB	ɺB	ƨB	ĜB	ĜB	ÖB	ƨB	ŢB	ŢB	ƨB	ƨB	ǮB	ɺB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�B	�B	�)B	�)B	�)B	�)B	�)B	�)B	�/B	�/B	�/B	�5B	�;B	�BB	�HB	�NB	�TB	�TB	�ZB	�ZB	�ZB	�TB	�TB	�TB	�ZB	�`B	�`B	�`B	�`B	�`B	�fB	�`B	�`B	�`B	�fB	�`B	�`B	�`B	�ZB	�ZB	�TB	�TB	�ZB	�`B	�fB	�fB	�fB	�fB	�`B	�`B	�`B	�`B	�`B	�`B	�`B	�`B	�`B	�`B	�`B	�`B	�`B	�ZB	�`B	�`B	�`B	�ZB	�fB	�fB	�fB	�mB	�sB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
+B

=B
\B
{B
�B
$�B
+B
2-B
<jB
C�B
G�B
O�B
VB
\)B
aHB
e`B
hsB
m�B
p�B
t�B
w�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   B�0B�+B�)B�0B�+B�+B�)B�,B�+B�,B�+B�)B�)B�,B�+B�,B�+B�)B�)B�)B�)B�)B�)B�)B�)B�*B�*B�(B�(B�*B�+B�(B�B�(B�'B�(B�(B�(B�(B�(B�'B�'B�'B�'B�(B�.B�.B�0B�1B�6B�6BʠBBB,�B1B.�B3B@]BH�BN�B^Bb*Bc0Be9Bd7Bc2Bb)Bb)Bd5BjYBu�B|�B�B��B��B��B�;Bz�BfBBN�BJ�BQ�B^Bc-B\BM�B>RB5B+�B�BB��B�iB� BȋB�=B��B�
Bs�BU�B;:B(�B�B0B
��B
пB
��B
�B
gIB
Q�B
AgB
7&B
uB	��B	ɔB	��B	�;B	��B	gLB	U�B	N�B	F�B	99B	.�B	$�B	�B	�B	�B	^B	B��B��B�B�uB�UB�=B�+B�&B�B��B��BλB˩BǎBɜBƉB�rB�_B�DB�2B�B��B��B��B��B��B��B��B��B�kB�eB�gB�dB�^B�bB��B��B��B��B��B�BƅB�B�bB��B��B��B��B��B	 �B	B	$B	3B	4B	9B	=B	7B	&B		B	B	PB	%�B	-�B	8*B	8+B	92B	?UB	BkB	BhB	K�B	Z�B	]B	\B	]B	]B	]B	]B	]B	\B	\B	Z�B	Z�B	Y�B	X�B	V�B	U�B	U�B	T�B	T�B	T�B	T�B	U�B	X�B	Y�B	X�B	X�B	X�B	X�B	X�B	Y�B	Z�B	]B	^B	aB	e6B	jUB	noB	nlB	miB	lcB	laB	osB	w�B	��B	��B	��B	�B	�B	�*B	�8B	�EB	�IB	�DB	�.B	�8B	�RB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B	�B	�B	�'B	�+B	�>B	�7B	�7B	�7B	�7B	�EB	�@B	�\B	�tB	�yB	ǀB	ȆB	ǁB	ɌB	ɍB	�zB	�mB	�nB	�iB	�zB	�sB	�tB	�yB	�xB	ǀB	ɋB	ɍB	ɎB	ʓB	ʓB	˙B	ͧB	ͧB	ΫB	άB	ϱB	ϰB	ΫB	ΩB	ϰB	ѾB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B	� B	��B	�	B	�B	�B	�B	�!B	�&B	�$B	�,B	�+B	�+B	�%B	�&B	�#B	�)B	�1B	�2B	�/B	�1B	�2B	�7B	�4B	�0B	�1B	�7B	�1B	�2B	�1B	�*B	�*B	�$B	�!B	�*B	�1B	�7B	�8B	�6B	�8B	�1B	�2B	�/B	�0B	�0B	�/B	�0B	�0B	�0B	�1B	�0B	�1B	�1B	�*B	�/B	�0B	�1B	�+B	�5B	�7B	�6B	�=B	�CB	�CB	�KB	�HB	�LB	�WB	�VB	�WB	�UB	�WB	�]B	�\B	�[B	�ZB	�\B	�[B	�\B	�ZB	�YB	�\B	�ZB	�aB	�`B	�bB	�`B	�aB	�`B	�`B	�cB	�_B	�`B	�aB	�aB	�ZB	�[B	�ZB	�ZB	�[B	�aB	�aB	�hB	�mB	�mB	�tB	�xB	�B	�~B	�~B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
 �B
 �B
�B
�B
�B
�B
�B
�B
�G�O�B
�B

B
*B
HB
vB
$�B
*�B
1�B
<8B
CcB
G|B
O�B
U�B
[�B
aB
e,B
h>B
m[B
pqB
t�B
w�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.43 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071451122016080714511220160807145112  AO  ARCAADJP                                                                    20150226221422    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150226221422  QCP$                G�O�G�O�G�O�DFB5E           AO  ARGQQCPL                                                                    20150226221422  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807145112  IP                  G�O�G�O�G�O�                