CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2016-01-28T20:16:06Z AOML 3.0 creation; 2016-06-01T00:08:28Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20160128201606  20160531170829  5903740 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  4055_7112_140                   2C  D   APEX                            5374                            041511                          846 @בe(d2>1   @בeǮ'a@:Qhr� ��c�$�/�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   A   A   @�ff@���A   A   AA��A`  A�  A�  A�  A�33A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dty�Dy�fD��D�L�D���D��3D�fD�VfD�|�D�� D�3D�VfD�� D��3D�3D�6fD�i�D���D�  D�6fD�l�D��311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�(�@ʏ\A�HA&�HAHz�Af�HA�p�A�p�A�p�A���A�p�A�p�A�p�A�p�B�RB	�RB�RB�RB!�RB)�RB1�RB9�RBA�RBI�RBQ�RBY�RBa�RBi�RBq�RBy�RB��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)C nCnCnCnCnC
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
D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt�Dy��D�*�D�Z�D���D���D�$)D�d)D���D���D��D�d)D���D���D� �D�D)D�w]D�ڐD��D�D)D�z�D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��TA��mA��yA��yA��A��A��A��A��mA��;A��HA��`A��mA��mA��`A��mA��#A��^A��9A��!A��!A��A�9XA�VA�JA�
=A�1A�1A�1A�%A�  A���A���A���A��A��A��HA��
A�ƨA��FA���A��7A�|�A�ffA�S�A�K�A�G�A�=qA�7LA�1'A� �A��hA���A�-A��;A�5?A��PA���A��A��A��yA���A��A��/A�dZA��A�
=A�x�A�VA��wA��PA�?}A���A��A��A��A�VA�JA���A�bA�ZA���A�/A��RA���A�Q�A���A�oA�E�A�+A��PA���A�|�A�&�A�33A�oA��DA��^A��-A��wA�ȴA��A���A�M�A�/A�K�A���A�9XA��
A��PA�^5A�?}A�VAoA}
=A{��Az�HAy�Ax�/Aw�Au�TAt��AsO�Ap�Ao&�AmhsAlbAj��Ah�Ag�
Ag%Ae�7AdĜAc��Ac��Aa�A`z�A^�jA]��A[dZAY�TAY�hAY`BAX��AW�#AW?}AVr�AU��AUXAU�ATȴAT9XAS|�AR�HAR-AO�#AO%AN��AM|�AL�/AKO�AI��AIC�AH�jAG�AGO�AF�DAD��AC��AB-A@�!A@{A?;dA>��A>-A=�A<I�A;?}A:9XA9|�A8r�A733A5S�A3��A2�!A21A0��A/33A-x�A,�A+|�A*�A*�A(��A(VA&��A%%A$ �A#�;A#t�A"�+A A�AVAffA�mA
=A=qAp�AoAv�A��A^5A��A�AVA$�A�wA�A��AK�A�yAZA��AJA�AjA�Ar�A"�A	�A��A�;AC�A(�A��A\)A��A=qA�A�PA�AffA�AoA bN@��@��-@��j@�A�@�;d@��!@��h@��/@�I�@���@�/@�\)@�?}@�I�@�t�@��@�E�@��@�D@��`@��@��
@���@�J@�%@�Z@߶F@�V@�1'@�5?@�I�@�@�G�@�Ĝ@��@��@��`@���@�r�@υ@���@͙�@�&�@̋D@ˍP@�{@�bN@���@�^5@�5?@�@���@�%@�l�@�n�@���@��\@�O�@�dZ@�V@�(�@�S�@�@���@��T@�hs@�/@���@��@���@�t�@���@��@���@�~�@��-@�Ĝ@�l�@��R@���@�v�@�5?@���@�%@���@��@�"�@�v�@��@���@��;@�\)@��H@�~�@��@���@���@�j@�A�@���@��@�l�@�"�@��H@�V@�J@��@���@�p�@��@��9@�Z@�1@���@��y@�n�@�{@��h@�hs@�?}@�/@�V@���@���@�r�@�b@���@�33@�@���@��H@�ȴ@��R@��\@�^5@�@��-@�?}@��D@�b@��w@��@�t�@�l�@�\)@�C�@��@��y@��R@��+@�$�@��@��@��@�(�@�9X@���@���@��P@�|�@�dZ@�C�@�"�@���@�J@�hs@���@�I�@�A�@��m@��@�;d@�|�@�\)@��@�V@�G�@�%@���@��j@���@���@���@�%@�x�@�hs@��@�z�@�r�@�Q�@�z�@�/@�`B@���@��@�j@�9X@�9X@�9X@��
@�S�@�+@�$�@��7@��@��`@��/@�@�p�@�X@�V@��/@��@�Q�@�A�@�(�@��@��;@��F@���@�|�@�t�@�S�@�33@�;d@�
=@�V@�J@���@�p�@�%@���@��D@� �@;d@~�+@~��@~��@}�@}O�@}V@}/@}p�@|�@st�@nV@hb@^v�@W��@Q%@L��@D��@9�#@2��@+S�@#��@ ��@�\@��@x�@ff@
-@{@�\11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��TA��mA��yA��yA��A��A��A��A��mA��;A��HA��`A��mA��mA��`A��mA��#A��^A��9A��!A��!A��A�9XA�VA�JA�
=A�1A�1A�1A�%A�  A���A���A���A��A��A��HA��
A�ƨA��FA���A��7A�|�A�ffA�S�A�K�A�G�A�=qA�7LA�1'A� �A��hA���A�-A��;A�5?A��PA���A��A��A��yA���A��A��/A�dZA��A�
=A�x�A�VA��wA��PA�?}A���A��A��A��A�VA�JA���A�bA�ZA���A�/A��RA���A�Q�A���A�oA�E�A�+A��PA���A�|�A�&�A�33A�oA��DA��^A��-A��wA�ȴA��A���A�M�A�/A�K�A���A�9XA��
A��PA�^5A�?}A�VAoA}
=A{��Az�HAy�Ax�/Aw�Au�TAt��AsO�Ap�Ao&�AmhsAlbAj��Ah�Ag�
Ag%Ae�7AdĜAc��Ac��Aa�A`z�A^�jA]��A[dZAY�TAY�hAY`BAX��AW�#AW?}AVr�AU��AUXAU�ATȴAT9XAS|�AR�HAR-AO�#AO%AN��AM|�AL�/AKO�AI��AIC�AH�jAG�AGO�AF�DAD��AC��AB-A@�!A@{A?;dA>��A>-A=�A<I�A;?}A:9XA9|�A8r�A733A5S�A3��A2�!A21A0��A/33A-x�A,�A+|�A*�A*�A(��A(VA&��A%%A$ �A#�;A#t�A"�+A A�AVAffA�mA
=A=qAp�AoAv�A��A^5A��A�AVA$�A�wA�A��AK�A�yAZA��AJA�AjA�Ar�A"�A	�A��A�;AC�A(�A��A\)A��A=qA�A�PA�AffA�AoA bN@��@��-@��j@�A�@�;d@��!@��h@��/@�I�@���@�/@�\)@�?}@�I�@�t�@��@�E�@��@�D@��`@��@��
@���@�J@�%@�Z@߶F@�V@�1'@�5?@�I�@�@�G�@�Ĝ@��@��@��`@���@�r�@υ@���@͙�@�&�@̋D@ˍP@�{@�bN@���@�^5@�5?@�@���@�%@�l�@�n�@���@��\@�O�@�dZ@�V@�(�@�S�@�@���@��T@�hs@�/@���@��@���@�t�@���@��@���@�~�@��-@�Ĝ@�l�@��R@���@�v�@�5?@���@�%@���@��@�"�@�v�@��@���@��;@�\)@��H@�~�@��@���@���@�j@�A�@���@��@�l�@�"�@��H@�V@�J@��@���@�p�@��@��9@�Z@�1@���@��y@�n�@�{@��h@�hs@�?}@�/@�V@���@���@�r�@�b@���@�33@�@���@��H@�ȴ@��R@��\@�^5@�@��-@�?}@��D@�b@��w@��@�t�@�l�@�\)@�C�@��@��y@��R@��+@�$�@��@��@��@�(�@�9X@���@���@��P@�|�@�dZ@�C�@�"�@���@�J@�hs@���@�I�@�A�@��m@��@�;d@�|�@�\)@��@�V@�G�@�%@���@��j@���@���@���@�%@�x�@�hs@��@�z�@�r�@�Q�@�z�@�/@�`B@���@��@�j@�9X@�9X@�9X@��
@�S�@�+@�$�@��7@��@��`@��/@�@�p�@�X@�V@��/@��@�Q�@�A�@�(�@��@��;@��F@���@�|�@�t�@�S�@�33@�;d@�
=@�V@�J@���@�p�@�%@���@��D@� �@;d@~�+@~��@~��@}�@}O�@}V@}/@}p�@|�@st�@nV@hb@^v�@W��@Q%@L��@D��@9�#@2��@+S�@#��@ ��@�\@��@x�@ff@
-@{@�\11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBZBZBZBZBZBZBZBZBZBYBZBYBZBZBZBYBYBYBYBXBXBW
BVBT�BT�BT�BT�BT�BT�BT�BS�BS�BS�BS�BS�BR�BR�BQ�BQ�BQ�BP�BM�BM�BL�BK�BJ�BJ�BI�BI�BH�BF�BB�B?}B>wB;dB33B$�B�B�BJBB��B��B��B�B�B�yB�/B�B�
B��BɺBÖB�XB��B��B�\B�7Bz�Bp�Be`BP�B>wB6FB)�B\B�;B�wB��B�bB�+B�Bw�Bp�BaHBQ�B?}B33B-B{B
��B
�B
�B
�mB
�ZB
�B
ÖB
�9B
�B
�B
��B
��B
��B
��B
�1B
~�B
w�B
o�B
ffB
ZB
K�B
@�B
2-B
�B
hB
B	��B	�B	�HB	�/B	�B	��B	��B	�
B	��B	ǮB	�qB	�!B	��B	��B	�VB	�JB	�DB	�+B	~�B	z�B	t�B	o�B	m�B	k�B	hsB	dZB	]/B	YB	R�B	G�B	C�B	@�B	9XB	49B	-B	(�B	$�B	!�B	�B	�B	�B	bB	DB	B	  B��B��B��B��B�B�B�yB�ZB�;B�B��B��BƨBB�}B�^B�?B�!B�B�B��B��B��B��B��B��B�{B�uB�hB�JB�+B�B�B� B|�Bz�By�Bw�Bv�Bs�Bp�Bn�Bl�Bk�BiyBgmBdZBbNB`BB_;B\)BYBS�BQ�BO�BK�BH�BE�BB�B@�B>wB<jB;dB:^B9XB7LB7LB6FB6FB6FB5?B5?B49B33B1'B0!B0!B/B/B.B.B-B,B+B(�B'�B(�B(�B(�B'�B&�B%�B#�B �B!�B �B �B �B �B�B�B�B�B�B�B�B�B�B�B�B �B"�B&�B'�B'�B'�B'�B&�B&�B'�B&�B&�B&�B&�B&�B%�B%�B(�B(�B)�B.B1'B49B:^B<jB>wB?}B?}BA�BC�BC�BE�BF�BG�BG�BH�BM�BP�BS�BVBXB\)B_;B_;B_;B`BBaHBcTBdZBffBjBo�Bt�Bu�Bw�Bx�Bz�B|�B~�B�B�%B�+B�+B�7B�=B�DB�PB�PB�\B�bB�hB�oB�{B��B��B��B��B��B��B��B��B�B�B�B�B�B�!B�'B�?B�RB�RB�dB�qB�wB��B��B��BBÖBŢBǮB��B��B��B�B�
B�B�B�B�B�#B�)B�5B�;B�HB�NB�ZB�fB�B�B�B�B�B�B�B�B�B�B�B��B��B��B��B��B��B	B	+B	+B	+B	
=B	bB	oB	uB	�B	�B	"�B	$�B	'�B	-B	0!B	0!B	/B	1'B	1'B	33B	6FB	7LB	9XB	<jB	=qB	>wB	?}B	?}B	?}B	?}B	?}B	?}B	=qB	=qB	>wB	@�B	K�B	R�B	VB	YB	]/B	aHB	bNB	cTB	e`B	ffB	gmB	hsB	iyB	jB	k�B	l�B	m�B	o�B	s�B	t�B	u�B	u�B	u�B	u�B	v�B	x�B	y�B	{�B	{�B	}�B	� B	� B	�B	�%B	�7B	�=B	��B	�B	ŢB	�#B	�`B	��B
B
bB
 �B
,B
8RB
A�B
K�B
O�B
XB
^5B
cTB
gmB
k�B
q�B
u�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   BY�BY�BY�BY�BY�BY�BY�BY�BY�BX�BY�BX�BY�BY�BY�BX�BX�BX�BX�BW�BW�BV�BU�BT�BT�BT�BT�BT�BT�BT�BS�BS�BS�BS�BS�BR�BR�BQ�BQ�BQ�BP�BM�BM�BL�BK�BJ�BJ�BI�BI�BH�BF�BBkB?YB>SB;@B3B$�B�BbB'B �B��B��B��B�xB�hB�VB�	B��B��BͭBɘB�lB�2B��B�eB�6B�Bz�BpwBe8BP�B>QB6B)�B0B�B�MB��B�:B��B��Bw�Bp|Ba#BQ�B?SB3B,�BTB
��B
�B
�]B
�EB
�0B
��B
�kB
�B
��B
��B
��B
��B
��B
�cB
�B
~�B
w�B
oxB
f@B
Y�B
K�B
@\B
2B
�B
CB
�B	��B	�sB	�$B	�
B	��B	��B	��B	��B	��B	ǉB	�MB	��B	��B	�cB	�5B	�'B	�!B	�
B	~�B	z�B	t�B	o{B	mrB	kcB	hTB	d7B	]B	X�B	R�B	G�B	CsB	@cB	98B	4B	,�B	(�B	$�B	!�B	�B	�B	iB	DB	%B	B��B��B��B��B��B�B�rB�\B�<B�B� B��B̰BƋB�tB�_B�AB�#B�B��B��B��B��B��B��B��B�jB�^B�WB�MB�.B�B��B��B�B|�Bz�By�Bw�Bv�Bs�Bp�Bn}BloBkiBi_BgSBd=Bb3B`(B_B\BX�BS�BQ�BO�BK�BH�BE�BBsB@jB>]B<PB;JB:CB9@B72B70B6(B6,B6-B5$B5$B4B3B1B0B0B/B/B-�B-�B,�B+�B*�B(�B'�B(�B(�B(�B'�B&�B%�B#�B �B!�B �B �B �B �B�B�B�B�B�B�B�B�B�B�B�B �B"�B&�B'�B'�B'�B'�B&�B&�B'�B&�B&�B&�B&�B&�B%�B%�B(�B(�B)�B-�B1	B4B:@B<HB>XB?^B?^BAkBCwBCuBE�BF�BG�BG�BH�BM�BP�BS�BU�BW�B\B_B_B_B`"Ba'Bc3Bd7BfFBj[Bo|Bt�Bu�Bw�Bx�Bz�B|�B~�B��B�B�B�B�B�B�B�-B�/B�8B�=B�DB�KB�XB�hB�tB��B��B��B��B��B��B��B��B��B��B��B��B��B�B�*B�+B�<B�JB�RB�ZB�[B�aB�hB�nB�zBǆBʜBϸB��B��B��B��B��B��B��B��B�B�B�B�B�'B�2B�<B�XB�\B�WB�bB�~B�~B�~B�}B�B�B�B��B��B��B��B��B��B	�B	�B	B	�B	
B	7B	BB	JB	cB	�B	"�B	$�B	'�B	,�B	/�B	/�B	.�B	0�B	0�B	3B	6B	7 B	9+B	<;B	=DB	>JB	?SB	?QB	?RB	?PB	?QB	?OB	=DB	=DB	>HB	@VB	K�B	R�B	U�B	X�B	] B	aB	b B	c(B	e1B	f9B	g?B	hFB	iKB	jRB	kXB	l]B	mdB	oqB	s�B	t�B	u�B	u�B	u�B	u�B	v�B	x�B	y�B	{�B	{�B	}�B	�B	�B	��B	��B	�B	�B	�fB	��B	�qB	��B	�0B	��B
�B
.B
 �B
+�B
8 B
AUB
K�B
O�B
W�B
^B
c B
g9B
kQB
qxB
u�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.43 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311708292016053117082920160531170829  AO  ARCAADJP                                                                    20160128201606    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20160128201606  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20160128201606  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531170829  IP                  G�O�G�O�G�O�                