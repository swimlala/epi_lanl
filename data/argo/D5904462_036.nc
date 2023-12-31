CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-02-26T22:14:40Z AOML 3.0 creation; 2016-08-07T21:51:15Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20150226221440  20160807145115  5904462 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               $A   AO  5287_9017_036                   2C  D   APEX                            6529                            072314                          846 @�;�s| 1   @�;��`@1���R�d��C��1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    $A   B   B   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:�C<  C>  C@  CB  CD  CF  CH  CJ  CL  CM�fCP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dts3Dy��D�	�D�L�D�s3D��fD�	�D�)�D�l�D��fD���D�0 D��3DǶfD��D�I�Dڀ D�ɚD���D�0 D�i�D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�(�@�A�HA&�HAF�HAf�HA�p�A�p�A�p�A�p�A�p�A�p�A�p�A�p�B�RB	�RB�RB�RB!�RB)�RB1�RB9�RBA�RBI�RBQ�RBY�RBa�RBi�RBq�RBy�RB��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B�\B�\C nCnCnCnCnC
nCnCnCnCnCnCnCnCnCnCnC nC"nC$nC&nC(nC*nC,nC.nC0nC2nC4nC6nC8nC:��C<nC>nC@nCBnCDnCFnCHnCJnCLnCNTzCPnCRnCTnCVnCXnCZnC\nC^nC`nCbnCdnCfnChnCjnClnCnnCpnCrnCtnCvnCxnCznC|nC~nC�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
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
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Dy�D�]D�Z�D���D��)D�]D�7]D�z�D��)D�]D�=�D���D��)D�']D�W]Dڍ�D��]D�
�D�=�D�w]D��]11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�jA�A��#A���Aȉ7A�33A�ȴA�I�A��A���A��A��TA��/A��
A���A���Aƥ�AƟ�AƍPA�ZA��A�l�Aŏ\Aŏ\A�hsAēuA��AìAÃA�E�A��mA�ffA�`BA�ĜA�l�A��A���A�5?A���A��jA���A��\A��A�VA��9A�9XA��wA���A�&�A��!A�1A�A��^A���A��A���A�VA��A�VA���A�t�A�+A�$�A�t�A�K�A��A�G�A���A�E�A���A�5?A�I�A��!A��A��hA��TA� �A�
=A�{A���A�;dA��A�K�A���A��TA��DA�1'A�G�A�`BA�&�A��A���A�bA�VA�5?A~ĜA{O�Au?}ArI�Ap�`AnffAl{Ah�Ab��A`��A_�hA]��A]|�A\�uAZ�AZ�AY��AY�7AY�AX�AXbATĜAQ��AO�PAM\)ALbAK�AJE�AG�AD��AB�uA>�!A;��A:��A9��A9&�A7�wA6��A5�FA5C�A3�A3&�A0v�A/A-�7A*��A(��A'\)A&jA%��A$v�A#�
A"(�A!�A �\AjAVA��A9XA�A�/A%AQ�A�FA+A9XAt�AS�A�HA��A�A��AAVA5?A��A��AbA��A��A��A�wA�AC�A~�A �A�TA
��A	A	x�A�A��AZAJA��A�;AdZAȴA�#A��A�A��A�uAZAJA��Ap�AS�A/A�A(�A�;A�
A��A�^AK�A �\A 1@�ƨ@�
=@��R@���@�n�@�=q@��^@��;@�-@�p�@��D@�Q�@�o@�^5@�=q@�G�@�@��y@��#@��@�@�;d@���@�n�@���@�`B@웦@���@���@�@���@陚@�p�@�`B@�G�@�7L@��@���@��`@�j@�F@�+@�E�@�Ĝ@�b@��@�t�@��@�h@�hs@���@��@�1@߶F@�|�@�\)@���@�5?@�@�x�@���@���@۾w@��H@�E�@���@�O�@���@؃@�Z@�1'@�1@׾w@�
=@֧�@��T@Ձ@�?}@��@Դ9@�I�@��
@ӥ�@Ӆ@��@���@�$�@ёh@щ7@�`B@��@�Ĝ@�r�@�bN@�1'@� �@��@��m@�ƨ@�l�@�;d@��y@�@��@�(�@˅@�^5@�@�O�@���@ț�@�1@��m@Ǿw@�K�@�ff@�?}@���@��`@�(�@�|�@��H@�J@��@��-@��h@�X@��@���@��D@� �@��@�ƨ@�|�@�S�@�33@�
=@�^5@��7@�7L@���@��/@�r�@��F@��@��!@�~�@�5?@�@�{@�5?@�E�@�V@�$�@�J@���@�x�@���@�Q�@��@�K�@���@�-@��#@���@�G�@���@�I�@���@�\)@��@�M�@��^@�O�@�7L@��@���@�  @���@�;d@�ȴ@�ff@�=q@��h@���@��9@���@��@���@�|�@�C�@�ȴ@�~�@��#@��@�&�@�V@�%@��@�Ĝ@��@��D@�1'@��w@�;d@�E�@�{@�/@�Ĝ@��u@���@��@�dZ@��@�ff@�E�@�J@��@��T@���@���@�9X@�K�@��T@�x�@��@���@���@��j@��j@��9@��@��@�1'@���@���@��@��@��P@��@�33@���@�^5@�{@��@�x�@���@�z�@�A�@��m@���@�dZ@�@���@�5?@�=q@��@�J@��@���@��^@��h@�O�@���@�I�@���@���@�+@��@��R@���@���@�~�@�M�@�J@�b@���@�G�@�O�@v��@m��@f{@]?}@W�;@N�@G�@@Ĝ@:�!@1��@-V@)�7@$z�@�y@�@+@�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   A�jA�A��#A���Aȉ7A�33A�ȴA�I�A��A���A��A��TA��/A��
A���A���Aƥ�AƟ�AƍPA�ZA��A�l�Aŏ\Aŏ\A�hsAēuA��AìAÃA�E�A��mA�ffA�`BA�ĜA�l�A��A���A�5?A���A��jA���A��\A��A�VA��9A�9XA��wA���A�&�A��!A�1A�A��^A���A��A���A�VA��A�VA���A�t�A�+A�$�A�t�A�K�A��A�G�A���A�E�A���A�5?A�I�A��!A��A��hA��TA� �A�
=A�{A���A�;dA��A�K�A���A��TA��DA�1'A�G�A�`BA�&�A��A���A�bA�VA�5?A~ĜA{O�Au?}ArI�Ap�`AnffAl{Ah�Ab��A`��A_�hA]��A]|�A\�uAZ�AZ�AY��AY�7AY�AX�AXbATĜAQ��AO�PAM\)ALbAK�AJE�AG�AD��AB�uA>�!A;��A:��A9��A9&�A7�wA6��A5�FA5C�A3�A3&�A0v�A/A-�7A*��A(��A'\)A&jA%��A$v�A#�
A"(�A!�A �\AjAVA��A9XA�A�/A%AQ�A�FA+A9XAt�AS�A�HA��A�A��AAVA5?A��A��AbA��A��A��A�wA�AC�A~�A �A�TA
��A	A	x�A�A��AZAJA��A�;AdZAȴA�#A��A�A��A�uAZAJA��Ap�AS�A/A�A(�A�;A�
A��A�^AK�A �\A 1@�ƨ@�
=@��R@���@�n�@�=q@��^@��;@�-@�p�@��D@�Q�@�o@�^5@�=q@�G�@�@��y@��#@��@�@�;d@���@�n�@���@�`B@웦@���@���@�@���@陚@�p�@�`B@�G�@�7L@��@���@��`@�j@�F@�+@�E�@�Ĝ@�b@��@�t�@��@�h@�hs@���@��@�1@߶F@�|�@�\)@���@�5?@�@�x�@���@���@۾w@��H@�E�@���@�O�@���@؃@�Z@�1'@�1@׾w@�
=@֧�@��T@Ձ@�?}@��@Դ9@�I�@��
@ӥ�@Ӆ@��@���@�$�@ёh@щ7@�`B@��@�Ĝ@�r�@�bN@�1'@� �@��@��m@�ƨ@�l�@�;d@��y@�@��@�(�@˅@�^5@�@�O�@���@ț�@�1@��m@Ǿw@�K�@�ff@�?}@���@��`@�(�@�|�@��H@�J@��@��-@��h@�X@��@���@��D@� �@��@�ƨ@�|�@�S�@�33@�
=@�^5@��7@�7L@���@��/@�r�@��F@��@��!@�~�@�5?@�@�{@�5?@�E�@�V@�$�@�J@���@�x�@���@�Q�@��@�K�@���@�-@��#@���@�G�@���@�I�@���@�\)@��@�M�@��^@�O�@�7L@��@���@�  @���@�;d@�ȴ@�ff@�=q@��h@���@��9@���@��@���@�|�@�C�@�ȴ@�~�@��#@��@�&�@�V@�%@��@�Ĝ@��@��D@�1'@��w@�;d@�E�@�{@�/@�Ĝ@��u@���@��@�dZ@��@�ff@�E�@�J@��@��T@���@���@�9X@�K�@��T@�x�@��@���@���@��j@��j@��9@��@��@�1'@���@���@��@��@��P@��@�33@���@�^5@�{@��@�x�@���@�z�@�A�@��m@���@�dZ@�@���@�5?@�=q@��@�J@��@���@��^@��h@�O�@���@�I�@���@���@�+@��@��R@���@���@�~�@�M�G�O�@�b@���@�G�@�O�@v��@m��@f{@]?}@W�;@N�@G�@@Ĝ@:�!@1��@-V@)�7@$z�@�y@�@+@�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB��B��B��B��B��B�B�'B�^B�wB��B��BBBĜBŢBƨB�B�/B�B�B#�B6FBL�B]/B\)BK�B7LB.B,B(�B#�B+B,B-B/B5?B:^B=qBA�BE�BH�BL�BN�BL�B@�B?}B>wB2-B%�B�B �B!�B�B\B��B�TB��B��BɺB��B��B�PBo�BXBE�B1'B&�B�BoB1B�B�5B��B�qB�B�B��B�B�B�B��B�DB�Bw�Bm�Be`BQ�B9XB,B�B\B  B
�yB
ȴB
��B
~�B
aHB
+B
hB
B	�B	�B	��B	��B	��B	�\B	�DB	�1B	�B	|�B	w�B	v�B	t�B	q�B	n�B	hsB	XB	G�B	<jB	0!B	)�B	"�B	�B	\B	B��B�yB�BB�/B�B�
B��B��B��B��BǮBŢB��B�jB�RB�3B�!B�B��B��B��B��B��B��B��B��B��B��B��B��B��B�3B�}BĜBĜBƨBȴBȴBǮBǮB��B��B�5B�ZB�TB�NB�`B�yB�B�B�B��B��B��B	  B	B	  B	B	%B	+B		7B		7B	
=B	
=B	
=B	DB	{B	�B	�B	�B	�B	�B	#�B	)�B	+B	49B	9XB	:^B	<jB	?}B	E�B	E�B	E�B	D�B	D�B	E�B	G�B	J�B	K�B	N�B	P�B	P�B	P�B	P�B	O�B	R�B	W
B	XB	W
B	W
B	W
B	W
B	W
B	XB	YB	\)B	]/B	`BB	cTB	gmB	iyB	jB	m�B	n�B	q�B	q�B	t�B	x�B	z�B	z�B	{�B	{�B	|�B	|�B	|�B	}�B	}�B	~�B	� B	�B	�B	�1B	�=B	�JB	�JB	�hB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�!B	�'B	�-B	�-B	�-B	�-B	�9B	�9B	�9B	�?B	�FB	�RB	�XB	�XB	�^B	�^B	�jB	�wB	�wB	�wB	�}B	�}B	�}B	��B	��B	��B	��B	ĜB	ƨB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�
B	�B	�B	�B	�B	�B	�B	�#B	�#B	�)B	�/B	�/B	�5B	�;B	�;B	�5B	�;B	�5B	�5B	�5B	�;B	�;B	�5B	�5B	�/B	�/B	�5B	�5B	�;B	�BB	�HB	�TB	�ZB	�ZB	�ZB	�`B	�fB	�fB	�mB	�mB	�sB	�sB	�sB	�sB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
%B
+B
+B
1B
1B
	7B
1B
	7B
	7B
DB
JB
oB
�B
�B
%�B
+B
1'B
;dB
?}B
B�B
G�B
N�B
P�B
YB
_;B
e`B
iyB
l�B
p�B
s�B
w�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   B��B��B��B��B��B��B� B�;B�WB�dB�gB�kB�mB�xB�~BƈB��B�B�B{B#�B6%BL�B]B\	BK�B7.B-�B+�B(�B#�B*�B+�B,�B.�B5B:<B=OBAjBE�BH�BL�BN�BL�B@`B?XB>UB2B%�BB �B!�BwB8B��B�.B��BζBɓB�]B��B�'BotBW�BEsB0�B&�BxBCBB�xB�BϴB�IB��B��B��B��B��B��B��B�B��Bw�BmfBe:BQ�B9.B+�BqB2B
��B
�PB
ȋB
��B
~�B
a"B
*�B
EB
�B	�mB	��B	�fB	��B	�jB	�9B	�#B	�B	��B	|�B	w�B	v�B	t�B	q�B	nwB	hRB	W�B	G�B	<HB	0B	)�B	"�B	�B	?B	�B��B�]B�&B�B� B��B��B��BͶBʥBǍBńB�fB�KB�4B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�]B�|B�|BƆBȔBȓBǍBǌB˨B��B�B�9B�3B�,B�=B�VB�lB�tB�B��B��B��B��B	 �B��B	�B	 B	B		B		B	
B	
B	
B	B	VB	tB	_B	mB	�B	�B	#�B	)�B	*�B	4B	90B	:8B	<AB	?WB	EzB	E}B	E{B	DuB	DwB	EzB	G�B	J�B	K�B	N�B	P�B	P�B	P�B	P�B	O�B	R�B	V�B	W�B	V�B	V�B	V�B	V�B	V�B	W�B	X�B	[�B	]B	`B	c+B	gDB	iOB	jVB	miB	nmB	q�B	q�B	t�B	x�B	z�B	z�B	{�B	{�B	|�B	|�B	|�B	}�B	}�B	~�B	�B	��B	��B	�B	�B	�B	�B	�;B	�KB	�QB	�UB	�YB	�fB	�sB	�wB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B	��B	� B	�B	�B	�B	�B	�B	�B	�&B	�+B	�*B	�1B	�2B	�>B	�LB	�MB	�MB	�MB	�LB	�NB	�VB	�ZB	�[B	�\B	�oB	�{B	ȅB	ʔB	̝B	ͦB	ΫB	жB	жB	еB	ѿB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	�B	�B	�B	�B	�B	�$B	�+B	�,B	�)B	�1B	�5B	�6B	�=B	�>B	�CB	�DB	�CB	�DB	�BB	�HB	�JB	�NB	�MB	�NB	�VB	�UB	�PB	�NB	�OB	�PB	�WB	�VB	�TB	�\B	�ZB	�[B	�^B	�[B	�\B	�[B	�_B	�hB	�gB	�hB	�iB	�hB	�lB	�tB	�rB	�uB	�vB	�qB	�|B	�xB	�xB	�zB	�tB	�jB	�UB	�aB	�cB	�aB	�xB	�~B	�~B	�B	�}B	�zB	�vB	�tB	�yB	�yB	�zB	�sB	�oB	�aB	�ZB	�TB	�aB	�nB	�nB	�kB	�nB	�kB	�nB	�lB	�hB	�iB	�hB	�]B	�fB	�yB	�B	�B	�pB	�mB	�sB	�B	��B	�B	��B	��B	��B	��B	��B	��B	��B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
B
	B
�B
	B
	G�O�B
B
=B
NB
xB
%�B
*�B
0�B
;2B
?IB
B\B
G|B
N�B
P�B
X�B
_B
e-B
iFB
lVB
ppB
s�B
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.43 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071451152016080714511520160807145115  AO  ARCAADJP                                                                    20150226221440    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150226221440  QCP$                G�O�G�O�G�O�DFB5E           AO  ARGQQCPL                                                                    20150226221440  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807145115  IP                  G�O�G�O�G�O�                