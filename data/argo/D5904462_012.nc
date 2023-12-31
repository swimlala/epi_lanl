CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-02-26T22:14:11Z AOML 3.0 creation; 2016-08-07T21:51:11Z UW 3.1 conversion     
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
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20150226221411  20160807145111  5904462 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               A   AO  5287_9017_012                   2C  D   APEX                            6529                            072314                          846 @����1   @���/?�@2dZ�1�c�"��`B1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   B   B   @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*�C,�C.  C0  C1�fC4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX�CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� DfD� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�fDy� D�3D�9�D��fD��fD�3D�I�D�l�D�ٚD�3D�\�D���D�ɚD� D�6fDڌ�D�ɚD�3D�33D�l�D��3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�@�A�HA&�HAF�HAf�HA�p�A�p�A�p�A�p�A�p�A�p�A�p�A�p�B�RB	�RB�RB�RB!�RB)�RB1�RB9�RBA�RBI�RBQ�RBY�RBa�RBi�RBq�RBy�RB��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B�\B��)B��)BԨ�B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)C nCnCnCnCnC
nCnCnCnCnCnCnCnCnCnCnC nC"nC$nC&nC(nC*��C,��C.nC0nC2TzC4nC6nC8nC:nC<nC>nC@nCBnCDnCFnCHnCJnCLnCNnCPnCRnCTnCVnCX��CZnC\nC^nC`nCbnCdnCfnChnCjnClnCnnCpnCrnCtnCvnCxnCznC|nC~nC�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
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
D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D!�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Dt��Dy��D��D�G]D��)D��)D��D�W]D�z�D��]D��D�j�D��]D��]D��D�D)Dښ�D��]D� �D�@�D�z�D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�ȴA�ƨAܾwAܺ^Aܴ9Aܴ9Aܴ9Aܴ9Aܲ-Aܲ-Aܲ-Aܲ-Aܴ9Aܴ9AܶFAܴ9Aܴ9Aܴ9Aܴ9Aܴ9AܶFAܴ9AܶFAܶFAܸRAܸRAܺ^Aܺ^AܼjAܼjAܼjAܾwAܾwAܼjAܼjAܾwAܾwAܼjAܼjAܸRAܰ!Aܛ�A�ȴA�x�A��A֍PA�t�A�z�A��TA�%A�A�A�%A�C�A� �A�/A�bNAǍPA�?}A��TA���A�A�C�A��A��uA���A�9XA��A�bA�ZA�7LA���A�jA�%A�G�A�n�A��A�-A�C�A���A��A�oA��A�n�A���A��PA��
A�9XA���A�x�A��#A�Q�A� �A��A�  A��A�v�A�=qA�ƨA��hA�bA���A�K�A��hAK�A~Q�A|��Au�-At$�Ap�AhffAct�A\^5AVbNAUhsATbAR�jAQ\)ANQ�AL��AKƨAI�AG�AG%ADv�A@�A>r�A>bA=|�A<��A:��A9�A8��A8M�A6�A5��A5�A4r�A2VA/�A,��A,bA+��A*�+A'�hA&z�A&9XA$��A"��A!VA��A5?AE�A�A|�A�A��A�yA�\A��AA9XAz�A�A��A��A��A�
A�7Al�Al�Ap�Al�A7LA(�A
�A�+A��AbA33AK�A�A�hA  A�;A V@�@�@�X@�z�@�ƨ@��@��^@�@�@���@�p�@��9@�(�@��F@���@��!@�o@�{@�?}@�7L@�"�@�j@�E�@�b@�n�@�?}@���@�
=@��@�l�@�~�@�E�@�$�@�=q@���@�33@�X@�33@�R@�\@�J@��@�j@�j@��@��@���@�@�t�@��#@���@�E�@�~�@�~�@�n�@�@�h@���@��@��@�E�@��@�7@��@߾w@��y@�{@ݲ-@܃@ܼj@�X@�7L@��@��
@�l�@�;d@��@�E�@�@���@�/@��/@ش9@�A�@�+@ְ!@�5?@ՙ�@�&�@��/@ԋD@� �@�ƨ@���@��@ѩ�@�`B@�Ĝ@�  @�@�n�@��#@�X@���@̛�@�z�@�bN@�I�@�  @˝�@���@��@��@���@ȃ@��m@���@�n�@�&�@�r�@�  @�|�@�S�@°!@��T@�p�@��@��@��@�=q@�-@�@��T@���@�G�@�%@�(�@���@�|�@�S�@��@�v�@��-@��u@�(�@��;@���@��@���@���@�~�@�M�@�{@��@�@�`B@��@�I�@���@��
@��w@���@��@��@��@�S�@��@�n�@�-@���@��@���@�V@��j@��j@��j@��@�bN@�b@��F@�+@��!@��-@�G�@�A�@�ƨ@���@��@�K�@���@�~�@�{@�p�@��@��/@��u@�z�@�A�@���@�t�@��y@���@�v�@�=q@���@�G�@�/@���@���@���@�bN@�r�@�z�@�z�@�bN@�I�@�1'@�1@���@��@���@�V@�@���@�?}@���@�Q�@�  @��@��F@�dZ@��@���@��@���@���@���@��\@��+@�E�@��h@��`@�bN@��@�\)@�"�@�@�=q@���@�`B@��@��@�r�@�A�@��@��@���@�\)@���@���@��\@�ff@�{@���@�x�@���@���@�j@� �@�b@��w@�;d@���@��H@�ȴ@���@��\@�v�@�$�@��T@��#@���@�p�@��/@�bN@��@��@���@�ff@��@�p�@��@��/@�Ĝ@��@���@��D@��@�z�@�r�@�bN@���@��P@�"�@�5?@��D@�$�@}`B@u/@ko@b�H@Z��@TZ@L�D@D��@;"�@2��@,�@&�y@#"�@ b@��@|�@�@b111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A�ȴA�ƨAܾwAܺ^Aܴ9Aܴ9Aܴ9Aܴ9Aܲ-Aܲ-Aܲ-Aܲ-Aܴ9Aܴ9AܶFAܴ9Aܴ9Aܴ9Aܴ9Aܴ9AܶFAܴ9AܶFAܶFAܸRAܸRAܺ^Aܺ^AܼjAܼjAܼjAܾwAܾwAܼjAܼjAܾwAܾwAܼjAܼjAܸRAܰ!Aܛ�A�ȴA�x�A��A֍PA�t�A�z�A��TA�%A�A�A�%A�C�A� �A�/A�bNAǍPA�?}A��TA���A�A�C�A��A��uA���A�9XA��A�bA�ZA�7LA���A�jA�%A�G�A�n�A��A�-A�C�A���A��A�oA��A�n�A���A��PA��
A�9XA���A�x�A��#A�Q�A� �A��A�  A��A�v�A�=qA�ƨA��hA�bA���A�K�A��hAK�A~Q�A|��Au�-At$�Ap�AhffAct�A\^5AVbNAUhsATbAR�jAQ\)ANQ�AL��AKƨAI�AG�AG%ADv�A@�A>r�A>bA=|�A<��A:��A9�A8��A8M�A6�A5��A5�A4r�A2VA/�A,��A,bA+��A*�+A'�hA&z�A&9XA$��A"��A!VA��A5?AE�A�A|�A�A��A�yA�\A��AA9XAz�A�A��A��A��A�
A�7Al�Al�Ap�Al�A7LA(�A
�A�+A��AbA33AK�A�A�hA  A�;A V@�@�@�X@�z�@�ƨ@��@��^@�@�@���@�p�@��9@�(�@��F@���@��!@�o@�{@�?}@�7L@�"�@�j@�E�@�b@�n�@�?}@���@�
=@��@�l�@�~�@�E�@�$�@�=q@���@�33@�X@�33@�R@�\@�J@��@�j@�j@��@��@���@�@�t�@��#@���@�E�@�~�@�~�@�n�@�@�h@���@��@��@�E�@��@�7@��@߾w@��y@�{@ݲ-@܃@ܼj@�X@�7L@��@��
@�l�@�;d@��@�E�@�@���@�/@��/@ش9@�A�@�+@ְ!@�5?@ՙ�@�&�@��/@ԋD@� �@�ƨ@���@��@ѩ�@�`B@�Ĝ@�  @�@�n�@��#@�X@���@̛�@�z�@�bN@�I�@�  @˝�@���@��@��@���@ȃ@��m@���@�n�@�&�@�r�@�  @�|�@�S�@°!@��T@�p�@��@��@��@�=q@�-@�@��T@���@�G�@�%@�(�@���@�|�@�S�@��@�v�@��-@��u@�(�@��;@���@��@���@���@�~�@�M�@�{@��@�@�`B@��@�I�@���@��
@��w@���@��@��@��@�S�@��@�n�@�-@���@��@���@�V@��j@��j@��j@��@�bN@�b@��F@�+@��!@��-@�G�@�A�@�ƨ@���@��@�K�@���@�~�@�{@�p�@��@��/@��u@�z�@�A�@���@�t�@��y@���@�v�@�=q@���@�G�@�/@���@���@���@�bN@�r�@�z�@�z�@�bN@�I�@�1'@�1@���@��@���@�V@�@���@�?}@���@�Q�@�  @��@��F@�dZ@��@���@��@���@���@���@��\@��+@�E�@��h@��`@�bN@��@�\)@�"�@�@�=q@���@�`B@��@��@�r�@�A�@��@��@���@�\)@���@���@��\@�ff@�{@���@�x�@���@���@�j@� �@�b@��w@�;d@���@��H@�ȴ@���@��\@�v�@�$�@��T@��#@���@�p�@��/@�bN@��@��@���@�ff@��@�p�@��@��/@�Ĝ@��@���@��D@��@�z�@�r�@�bN@���@��PG�O�@�5?@��D@�$�@}`B@u/@ko@b�H@Z��@TZ@L�D@D��@;"�@2��@,�@&�y@#"�@ b@��@|�@�@b111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBDBDBDBDB
=BDBDBDBDBDBDBDBDBDBDBDBDB
=BDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDB
=BDB	7B	7BBB	7BJB	7B1B�B7LBL�BffBv�Bk�BaHBgmBp�By�B�+B��B��B�9B�RBŢB�
B�mB��BoB�B#�B(�B+B#�B1B�/BB�XB�B��B��B��B��B�B��B�bB��B��BW
BE�B6FB.B(�B �BDB
�B
�
B
��B
��B
�%B
m�B
E�B
(�B
+B	�B	�ZB	��B	�B	��B	�B	M�B	0!B	1B�sB�ZB�;B�B��BȴBĜBȴBǮBĜB��B�jB�wB��B�}B�}B�}BBBBBĜBŢBĜBÖB��B��B�qB�}B��BÖBÖBȴBȴBB��B��B��B��B��B�B�B�B�B�B�
B��B��B�B�;B�;B�BB�NB�`B�mB�sB�yB�B�B�yB�yB�B�yB�5B�B��B��B�/B�B�B�`B�B��B�
B�B�/B�5B�;B�BB�fB�mB�mB�sB�yB�B�B��B��B	B	
=B	$�B	@�B	F�B	?}B	L�B	Q�B	F�B	D�B	J�B	E�B	D�B	Q�B	R�B	Q�B	R�B	S�B	XB	aHB	hsB	e`B	^5B	_;B	cTB	e`B	gmB	gmB	gmB	iyB	jB	k�B	l�B	k�B	ffB	gmB	p�B	u�B	v�B	v�B	v�B	u�B	u�B	s�B	v�B	u�B	v�B	x�B	|�B	}�B	}�B	~�B	}�B	�B	�+B	�VB	�bB	�bB	�hB	�oB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�!B	�-B	�3B	�9B	�?B	�?B	�?B	�?B	�?B	�FB	�FB	�LB	�LB	�XB	�^B	�^B	�qB	�wB	��B	B	ÖB	ĜB	ĜB	ƨB	ǮB	ȴB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�#B	�#B	�)B	�/B	�/B	�/B	�5B	�BB	�BB	�BB	�;B	�BB	�BB	�BB	�HB	�HB	�NB	�NB	�NB	�NB	�NB	�NB	�TB	�TB	�ZB	�ZB	�fB	�fB	�fB	�`B	�`B	�`B	�`B	�`B	�ZB	�ZB	�TB	�NB	�TB	�TB	�TB	�TB	�ZB	�mB	�mB	�mB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
%B
+B
1B
1B
1B
	7B

=B

=B

=B

=B

=B

=B
DB
DB
DB

=B

=B
DB
DB
hB
PB
�B
�B
%�B
-B
-B
/B
33B
=qB
E�B
I�B
O�B
VB
\)B
aHB
dZB
ffB
k�B
o�B
t�B
v�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B#B$B$B"B
B$B#B%B$B'B$B'B'B'B%B#B%B
B$B#B$B#B$B$B%B$B#B'B%B%B$B#B#B#B%B$B$B$B
B#B	B	B B�B	B-B	BBnB7*BL�BfCBv�BkbBa'BgJBp�By�B�B�fB��B�B�.B�|B��B�GB��BKBuB#�B(�B*�B#�B
B�B�kB�2B��B��B��B�eB��B��B��B�:B��B�sBV�BEzB6B-�B(�B �BB
�lB
��B
�]B
��B
�B
mkB
EB
(�B
	B	�nB	�8B	��B	��B	��B	��B	M�B	0B	B�WB�AB�!B��B��BȚBĂBșBǑBăB�mB�NB�\B�fB�aB�bB�bB�sB�rB�sB�rB�BŅBĀB�wB��B̮B�PB�aB�mB�yB�wBȕBȗB�qB˨B̮B��BͰB��B��B��B��B��B��B��B��B��B��B�B�B�!B�,B�:B�KB�RB�VB�]B�]B�VB�VB�ZB�WB�B��B��B��B�B�B�vB�;B��B��B��B��B�B�B�B�B�BB�GB�GB�MB�VB�jB�B��B��B	�B	
B	$�B	@]B	F�B	?VB	L�B	Q�B	F�B	DtB	J�B	E|B	DuB	Q�B	R�B	Q�B	R�B	S�B	W�B	a B	hLB	e6B	^B	_B	c+B	e5B	gEB	gDB	gDB	iPB	jVB	k]B	lbB	kZB	f=B	gBB	pyB	u�B	v�B	v�B	v�B	u�B	u�B	s�B	v�B	u�B	v�B	x�B	|�B	}�B	}�B	~�B	}�B	��B	�B	�*B	�6B	�8B	�;B	�AB	�CB	�OB	�\B	�aB	�iB	�sB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�B	�B	�B	�B	�!B	�)B	�1B	�/B	�BB	�JB	�\B	�`B	�hB	�lB	�mB	�zB	ǂB	ȅB	ȄB	ɌB	˘B	ͣB	ͣB	͢B	ͤB	ΪB	άB	άB	жB	ѻB	ѼB	ѾB	ѼB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�
B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�'B	�#B	�)B	�+B	�6B	�4B	�5B	�/B	�.B	�.B	�/B	�1B	�*B	�+B	�%B	�B	�#B	�$B	�"B	�"B	�)B	�<B	�<B	�;B	�CB	�BB	�IB	�VB	�TB	�\B	�ZB	�[B	�YB	�bB	�`B	�cB	�aB	�cB	�gB	�hB	�hB	�fB	�nB	�sB	�tB	�|B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 B
 B
	B

B

	B


B

B

	B

B
B
B
B

B

B
B
G�O�B
B
uB
}B
%�B
,�B
,�B
.�B
3B
==B
EoB
I�B
O�B
U�B
[�B
aB
d&B
f2B
kQB
ohB
t�B
v�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.43 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071451112016080714511120160807145111  AO  ARCAADJP                                                                    20150226221411    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150226221411  QCP$                G�O�G�O�G�O�DFB5E           AO  ARGQQCPL                                                                    20150226221411  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807145111  IP                  G�O�G�O�G�O�                